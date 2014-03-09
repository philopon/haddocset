{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception

import qualified Filesystem                        as P
import qualified Filesystem.Path.CurrentOS         as P

import           System.IO
import           System.IO.Error
import           System.Process

import qualified Database.SQLite.Simple            as Sql

import           Data.Maybe
import qualified Data.Text                         as T
import           Text.HTML.TagSoup                 as Ts
import           Text.HTML.TagSoup.Match           as Ts

import           Distribution.InstalledPackageInfo
import           Distribution.Package
import           Distribution.Compat.ReadP
import           Distribution.Text                 (display, parse)
import           Documentation.Haddock
import qualified Module                            as Ghc
import qualified Name                              as Ghc

import           Data.Conduit
import qualified Data.Conduit.Filesystem           as P
import qualified Data.Conduit.List                 as CL

import           Options.Applicative

docsetDir :: P.FilePath -> P.FilePath
docsetDir d =
    if P.extension d == Just "docset"
    then d
    else d P.<.> "docset"

globalPackageDirectory :: FilePath -> IO P.FilePath
globalPackageDirectory hcPkg =
    P.decodeString . init . head . lines <$> readProcess hcPkg ["list"] ""

packageConfs :: P.FilePath -> IO [P.FilePath]
packageConfs dir =
    filter ((== Just "conf") . P.extension) <$> P.listDirectory dir

data DocInfo = DocInfo 
    { diPackageId  :: PackageId
    , diInterfaces :: [P.FilePath]
    , diHTMLs      :: [P.FilePath]
    , diExposed    :: Bool
    } deriving Show

readDocInfoFile :: P.FilePath -> IO (Maybe DocInfo)
readDocInfoFile pifile = P.isDirectory pifile >>= \isDir ->
    if isDir
    then filter ((== Just "haddock") . P.extension) <$> P.listDirectory pifile >>= \hdc -> case hdc of
        []       -> return Nothing
        hs@(h:_) -> readInterfaceFile freshNameCache (P.encodeString h) >>= \ei -> case ei of
            Left _     -> return Nothing
            Right (InterfaceFile _ (intf:_)) -> do
                let rPkg = readP_to_S parse . Ghc.packageIdString . Ghc.modulePackageId $ instMod intf :: [(PackageId, String)]
                case rPkg of
                    []  -> return Nothing
                    pkg -> do
                        return . Just $ DocInfo (fst $ last pkg) hs [P.collapse $ pifile P.</> P.decodeString "./" ] True
            Right _ -> return Nothing
    else do
        result <- parseInstalledPackageInfo <$> readFile (P.encodeString pifile)
        return $ case result of
            ParseFailed _ -> Nothing
            ParseOk [] a
                | null (haddockHTMLs a)      -> Nothing
                | null (haddockInterfaces a) -> Nothing
                | otherwise -> Just $
                    DocInfo (sourcePackageId a) (map P.decodeString $ haddockInterfaces a) 
                            (map (P.decodeString . (++"/")) $ haddockHTMLs a) (exposed a)

            ParseOk _  _  -> Nothing

data Plist = Plist
    { cfBundleIdentifier   :: String
    , cfBundleName         :: String
    , docSetPlatformFamily :: String
    } deriving Show

showPlist :: Plist -> String
showPlist Plist{..} = unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"
        , "<plist version=\"1.0\">"
        , "<dict>"
        , "<key>CFBundleIdentifier</key>"
        , "<string>" ++ cfBundleIdentifier ++ "</string>"
        , "<key>CFBundleName</key>"
        , "<string>" ++ cfBundleName ++ "</string>"
        , "<key>DocSetPlatformFamily</key>"
        ,	"<string>" ++ docSetPlatformFamily ++ "</string>"
        , "<key>isDashDocset</key>"
        , "<true/>"
        , "<key>dashIndexFilePath</key>"
        , "<string>index.html</string>"
        , "</dict>"
        , "</plist>"
        ]

copyHtml :: DocFile -> P.FilePath -> IO ()
copyHtml doc dst = do
    tags <- Ts.parseTags <$> P.readTextFile (docAbsolute doc)
    P.writeTextFile dst . Ts.renderTags $ map mapFunc tags
  where
    mapFunc tag
        | Ts.tagOpenLit "a" (Ts.anyAttrNameLit "href") tag =
            let url    = Ts.fromAttrib "href" tag
                absp p = P.collapse $ docBaseDir doc P.</> P.fromText p
                attr   = filter (\(n,_) -> n /= "href") (getAttr tag)
            in case url of
                _ | "http://"  `T.isPrefixOf` url -> tag
                  | "https://" `T.isPrefixOf` url -> tag
                  | "file:///" `T.isPrefixOf` url -> Ts.TagOpen "a" (toAttr "href" (rebase . P.fromText $ T.drop 7 url) attr)
                  | otherwise                     -> Ts.TagOpen "a" (toAttr "href" (rebase . absp       $          url) attr)
        | otherwise = tag

    getAttr (TagOpen _ a) = a
    getAttr _             = error "copyHtml: call attr to !TagOpen."

    toAttr attr url = case P.toText url of
        Right r -> ((attr, r):)
        Left  _ -> id

    both a = case a of
        Left l -> l
        Right r -> r

    rebase p = let fil  = P.filename p
                   pkgs = filter packageLike . reverse $ P.splitDirectories (P.parent p)
               in case pkgs of
                   []    -> fil
                   pkg:_ -> relativize (P.decodeString . display . packageName $ docPackage doc) $ pkg P.</> fil

    packageLike p = let t = both $ P.toText p
                        in T.any (== '-') t && (T.all (`elem` "0123456789.") . T.takeWhile (/= '-') $ T.reverse t)

relativize :: P.FilePath -> P.FilePath -> P.FilePath
relativize base path = up P.</> p
    where pfx = P.commonPrefix [base, path]
          up  = P.concat . flip replicate ".." . length . P.splitDirectories . fromMaybe (error "relativize") $ P.stripPrefix pfx base
          p   = fromMaybe (error "relativize") $ P.stripPrefix pfx path

data DocFile = DocFile
    { docPackage     :: PackageId
    , docBaseDir     :: P.FilePath
    , docRationalDir :: P.FilePath
    } deriving Show


docAbsolute :: DocFile -> P.FilePath
docAbsolute doc = docBaseDir doc P.</> docRationalDir doc

copyDocument :: (MonadThrow m, MonadIO m) => P.FilePath
             -> Consumer DocFile m ()
copyDocument docdir = awaitForever $ \doc -> do
    let full = docAbsolute doc
        dst = docdir
              P.</> (P.decodeString . display) (docPackage doc)
              P.</> docRationalDir doc
    liftIO $ P.createTree (P.directory dst)
    ex <- liftIO $ (||) <$> P.isFile dst <*> P.isDirectory dst
    when ex $ monadThrow $ mkIOError alreadyExistsErrorType "copyDocument" Nothing (Just $ P.encodeString dst)
    case P.extension $ docRationalDir doc of
        Just "html"    -> liftIO $ copyHtml doc dst
        Just "haddock" -> return ()
        _              -> liftIO $ P.copyFile full dst

docFiles :: MonadIO m => PackageId -> [P.FilePath] -> Producer m DocFile
docFiles sourcePackageId haddockHTMLs =
    forM_ haddockHTMLs $ \dir ->
        P.traverse False dir
            =$= awaitForever (\f -> yield $ DocFile sourcePackageId dir $ fromMaybe (error $ "Prefix missmatch: " ++ show (dir ,f)) $ P.stripPrefix dir f)

data Provider
    = Haddock  PackageId P.FilePath
    | Package  PackageId
    | Module   PackageId Ghc.Module
    | Function PackageId Ghc.Module Ghc.Name

moduleProvider :: MonadIO m => DocInfo -> Producer m Provider
moduleProvider iFile =
    mapM_ sub $ diInterfaces iFile
  where
    sub file = do
        rd <- liftIO $ readInterfaceFile freshNameCache (P.encodeString file)
        case rd of
            Left _ -> return ()
            Right (ifInstalledIfaces -> iIntrf) -> do
                let pkg = diPackageId iFile
                yield $ Haddock pkg file
                yield $ Package pkg
                forM_ iIntrf $ \i -> do
                    let modn = instMod i
                        fs   = instVisibleExports i
                    when (OptHide `notElem` instOptions i) $ do
                        yield $ Module pkg modn
                        mapM_ (yield . Function pkg modn) fs

populatePackage :: Sql.Connection -> PackageId -> IO ()
populatePackage conn pkg =
    Sql.execute conn "INSERT OR IGNORE INTO searchIndex(name, type, path,package) VALUES (?,?,?,?);"
        (display pkg, "Package" :: String, url,display pkg)
  where
    url = display pkg ++ "/index.html"

moduleNmaeUrl :: String -> String
moduleNmaeUrl = map dot2Dash
  where dot2Dash '.'  = '-'
        dot2Dash c    = c

populateModule :: Sql.Connection -> PackageId -> Ghc.Module -> IO ()
populateModule conn pkg modn =
    Sql.execute conn "INSERT OR IGNORE INTO searchIndex(name, type, path, package) VALUES (?,?,?,?);"
        (Ghc.moduleNameString $ Ghc.moduleName modn, "Module" :: String, url,display pkg)
  where
    url = display pkg ++ '/':
          (moduleNmaeUrl . Ghc.moduleNameString . Ghc.moduleName) modn ++ ".html"

populateFunction :: Sql.Connection -> PackageId -> Ghc.Module -> Ghc.Name -> IO ()
populateFunction conn pkg modn name =
    Sql.execute conn "INSERT OR IGNORE INTO searchIndex(name, type, path, package) VALUES (?,?,?,?);"
        (Ghc.getOccString name, dataType :: String, url, display pkg)
  where
    url = display pkg ++ '/':
          (moduleNmaeUrl . Ghc.moduleNameString . Ghc.moduleName) modn ++ ".html#" ++
          prefix : ':' :
          escapeSpecial (Ghc.getOccString name)
    specialChars  = "!&|+$%(,)*<>-/=#^\\?"
    escapeSpecial = concatMap (\c -> if c `elem` specialChars then '-': show (fromEnum c) ++ "-" else [c])
    prefix        = case name of
        _ | Ghc.isTyConName name -> 't'
          | otherwise            -> 'v'
    dataType      = case name of
        _ | Ghc.isTyConName   name -> "Type"
          | Ghc.isDataConName name -> "Constructor"
          | otherwise              -> "Function"

progress :: MonadIO m => Bool -> Int -> Char -> ConduitM o o m ()
progress cr u c = sub 1
  where
    sub n = await >>= \mbi -> case mbi of
        Nothing -> when cr $ liftIO (hPutChar stderr '\n' >> hFlush stderr)
        Just i  -> do
            when (n `mod` u == 0) $ liftIO (hPutChar stderr c >> hFlush stderr)
            yield i
            sub (succ n)

dispatchProvider :: Sql.Connection -> P.FilePath -> Provider -> IO ()
dispatchProvider _ hdir (Haddock p h) =
    let dst = hdir  P.</> P.decodeString (display p) P.<.> "haddock"
    in P.copyFile h dst
dispatchProvider conn _ (Package p)      = populatePackage  conn p
dispatchProvider conn _ (Module p m)     = populateModule   conn p m
dispatchProvider conn _ (Function p m n) = populateFunction conn p m n


createCommand :: Options -> IO ()
createCommand o = do
    unless (optQuiet o) $ putStrLn "[1/5] Create Directory."
    P.createDirectory False (optTarget o) -- for fail when directory already exists.
    P.createTree           (optDocumentsDir o)
    P.createDirectory True (optHaddockDir o)

    unless (optQuiet o) $ putStrLn "[2/5] Writing plist."
    writeFile (P.encodeString $ optTarget o P.</> "Contents/Info.plist") $ showPlist (createPlist $ optCommand o)

    unless (optQuiet o) $ putStrLn "[3/5] Migrate Database."
    conn <- Sql.open . P.encodeString $ optTarget o P.</> "Contents/Resources/docSet.dsidx"
    Sql.execute_ conn "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT, package TEXT);"
    Sql.execute_ conn "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path, package);"

    globalDir <- globalPackageDirectory (optHcPkg o)
    unless (optQuiet o) $ putStr "    Global package directory: " >> putStrLn (P.encodeString globalDir)
    globals <- map (globalDir P.</>) <$> packageConfs globalDir
    let locals = toAddFiles $ optCommand o
    iFiles <- filter diExposed . catMaybes <$> mapM readDocInfoFile (globals ++ locals)
    unless (optQuiet o) $ putStr "    Global package count:     " >> print (length globals)

    unless (optQuiet o) $ putStrLn "[4/5] Copy and populate Documents."
    forM_ iFiles $ \iFile -> addSinglePackage o conn iFile

    unless (optQuiet o) $ putStrLn "[5/5] Create index."
    haddockIndex o


haddockIndex :: Options -> IO ()
haddockIndex o = do
    argIs <- map (\h -> "--read-interface="
               ++   P.encodeString (P.dropExtension $ P.filename h) ++
               ',': P.encodeString h) <$> P.listDirectory (optHaddockDir o)

    haddock $ "--gen-index": "--gen-contents": ("--odir=" ++ P.encodeString (optDocumentsDir o)): argIs

addSinglePackage :: Options -> Sql.Connection -> DocInfo -> IO ()
addSinglePackage o conn iFile = go `catchIOError` handler
  where 
    go = do
        putStr "    " >> putStr (display $ diPackageId iFile) >> putChar ' ' >> hFlush stdout
        docFiles (diPackageId iFile) (diHTMLs iFile)
            $$ (if optQuiet o then id else (progress False  10 '.' =$)) (copyDocument $ optDocumentsDir o)
        Sql.execute_ conn "BEGIN;"
        ( moduleProvider iFile
            $$ (if optQuiet o then id else (progress True  100 '*' =$)) (CL.mapM_ (liftIO . dispatchProvider conn (optHaddockDir o))))
            `onException` (Sql.execute_ conn "ROLLBACK;")
        Sql.execute_ conn "COMMIT;"
    handler ioe
        | isDoesNotExistError ioe = putStr "Error: " >> print   ioe
        | otherwise               = ioError ioe


addCommand :: Options -> IO ()
addCommand o = do
    conn <- Sql.open . P.encodeString $ optTarget o P.</> "Contents/Resources/docSet.dsidx"
    forM_ (toAddFiles $ optCommand o) $ \i -> go conn i
        `catchIOError` handler
    haddockIndex o
  where
    go conn p = readDocInfoFile p >>= \mbIFile -> case mbIFile of
        Nothing    -> return ()
        Just iFile -> addSinglePackage o conn iFile
    handler ioe
            | isDoesNotExistError ioe = print ioe
            | otherwise               = ioError ioe

listCommand :: Options -> IO ()
listCommand o =
    mapM_ (putStrLn . P.encodeString . P.dropExtension . P.filename) =<< P.listDirectory (optHaddockDir o)

data Options
    = Options { optHcPkg   :: String
              , optTarget  :: P.FilePath
              , optQuiet   :: Bool
              , optCommand :: Command
              }
    deriving Show

optHaddockDir, optDocumentsDir :: Options -> P.FilePath
optHaddockDir   opt = optTarget opt P.</> "Contents/Resources/Haddock/"
optDocumentsDir opt = optTarget opt P.</> "Contents/Resources/Documents/"

data Command
    = Create { createPlist :: Plist, toAddFiles :: [P.FilePath] }
    | List
    | Add    { toAddFiles :: [P.FilePath] }
    deriving Show

main :: IO ()
main = do
    opts <- execParser optRule
    case opts of
        Options{optCommand = Create{}} -> createCommand opts
        Options{optCommand = List}     -> listCommand   opts
        Options{optCommand = Add{}}    -> addCommand    opts
  where
    optRule = info (helper <*> options) fullDesc
    options = Options
              <$> (strOption (long "hc-pkg" <> metavar "CMD" <> help "hc-pkg command (default: ghc-pkg)") <|> pure "ghc-pkg")
              <*> fmap (docsetDir . P.decodeString)
                  (strOption (long "target" <> short 't' <> metavar "DOCSET" <> help "output directory (default: haskell.docset)") <|> pure "haskell")
              <*> switch (long "quiet" <> short 'q' <> help "suppress output.")
              <*> subparser (command "create" (info createOpts  $ progDesc "crate new docset.")
                          <> command "list"   (info (pure List) $ progDesc "list package of docset.")
                          <> command "add"    (info addOpts $ progDesc "add package to docset."))

    createOpts = Create
                 <$> ( Plist
                         <$> (strOption (long "CFBundleIdentifier")   <|> pure "haskell")
                         <*> (strOption (long "CFBundleName")         <|> pure "Haskell")
                         <*> (strOption (long "DocSetPlatformFamily") <|> pure "haskell"))
                 <*> arguments (Just . P.decodeString) (metavar "CONFS" <> help "path to installed package configuration.")

    addOpts    = Add <$> arguments1 (Just . P.decodeString) (metavar "CONFS" <> help "path to installed package configuration.")

