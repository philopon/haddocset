{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}

module Documentation.Haddocset where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class

import qualified Filesystem                        as P
import qualified Filesystem.Path.CurrentOS         as P

import           System.IO
import           System.IO.Error(mkIOError, alreadyExistsErrorType, isDoesNotExistError)
import           System.Process

import qualified Database.SQLite.Simple            as Sql

import           Data.Maybe
import qualified Data.Text                         as T
import           Text.HTML.TagSoup                 as Ts
import           Text.HTML.TagSoup.Match           as Ts

import           Distribution.Compat.ReadP
import           Distribution.InstalledPackageInfo
import           Distribution.Package
import           Distribution.Text                 (display, parse)
import           Documentation.Haddock
import qualified Module                            as Ghc
import qualified Name                              as Ghc

import           Data.Conduit
import qualified Data.Conduit.Filesystem           as P
import qualified Data.Conduit.List                 as CL

docsetDir :: P.FilePath -> P.FilePath
docsetDir d =
    if P.extension d == Just "docset"
    then d
    else d P.<.> "docset"

globalPackageDirectories :: FilePath -> IO [P.FilePath]
globalPackageDirectories hcPkg = do
    ds <- map (P.decodeString . init) . filter isPkgDBLine . lines <$>
        readProcess hcPkg ["list", "--global"] ""
    forM ds $ \d -> P.isDirectory d >>= \isDir ->
        if isDir
        then return d
        else return (P.directory d)
  where
    isPkgDBLine ""      = False
    isPkgDBLine (' ':_) = False
    isPkgDBLine _       = True

packageConfs :: P.FilePath -> IO [P.FilePath]
packageConfs dir =
    filter (("package.cache" /=) . P.filename) <$> P.listDirectory dir

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
            let absp p = P.collapse $ docBaseDir doc P.</> P.fromText p
                attr   = filter (\(n,_) -> n /= "href") (getAttr tag)
            in case Ts.fromAttrib "href" tag of
                url | "http://"  `T.isPrefixOf` url -> tag
                    | "https://" `T.isPrefixOf` url -> tag
                    | "file:///" `T.isPrefixOf` url -> Ts.TagOpen "a" (toAttr "href" (rebase . P.fromText $ T.drop 7 url) attr)
                    | "#"        `T.isPrefixOf` url -> Ts.TagOpen "a" (toAttr "href" (rebase $ addHash url dst) attr)
                    | otherwise                     -> Ts.TagOpen "a" (toAttr "href" (rebase . absp       $          url) attr)
        | Ts.tagOpenLit "a" (Ts.anyAttrNameLit "name") tag =
            let Ts.TagOpen _ attr = tag
                hash = '#' `T.cons` Ts.fromAttrib "name" tag
            in Ts.TagOpen "a" (toAttr "href" (rebase $ addHash hash dst) attr)
        | otherwise = tag

    addHash h file = P.dirname file P.</> case P.toText $ P.filename file of
        Right r -> P.fromText $ r `T.append` h
        Left  _ -> ""

    getAttr (TagOpen _ a) = a
    getAttr _             = error "copyHtml: call attr to !TagOpen."

    toAttr attr url = case P.toText url of
        Right r -> ((attr, r):)
        Left  _ -> id

    both a = case a of
        Left  l -> l
        Right r -> r

    rebase p =
        let file    = P.filename p
            isSrc   = "src" `elem` P.splitDirectories (P.parent p)
            srcNize = if isSrc then ("src" P.</>) else id
            pkgs    = filter packageLike . reverse $ P.splitDirectories (P.parent p)
        in case pkgs of
            []    -> file
            pkg:_ -> ".." P.</> pkg P.</> srcNize file

    packageLike p = let t = both $ P.toText p
                        in T.any (== '-') t && (T.all (`elem` "0123456789.") . T.takeWhile (/= '-') $ T.reverse t)

commonPrefix :: P.FilePath -> P.FilePath -> P.FilePath
commonPrefix a0 b0 = P.concat $ loop id (P.splitDirectories a0) (P.splitDirectories b0) where
  loop f [] _  = f []
  loop f _  [] = f []
  loop f (a:as) (b:bs) | a == b    = loop (f . (a:)) as bs
                       | otherwise = f []

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
    when ex $ throwM $ mkIOError alreadyExistsErrorType "copyDocument" Nothing (Just $ P.encodeString dst)
    case P.extension $ docRationalDir doc of
        Just "html"    -> liftIO $ copyHtml doc dst
        Just "haddock" -> return ()
        _              -> liftIO $ P.copyFile full dst

docFiles :: (MonadIO m, MonadResource m) => PackageId -> [P.FilePath] -> Producer m DocFile
docFiles sourcePackageId haddockHTMLs =
    forM_ haddockHTMLs $ \dir ->
        P.sourceDirectoryDeep False (P.encodeString dir)
            =$= awaitForever (\f -> yield $ DocFile sourcePackageId dir $ fromMaybe (error $ "Prefix missmatch: " ++ show (dir ,f)) $ P.stripPrefix dir (P.decodeString f))

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


haddockIndex :: P.FilePath -> P.FilePath -> IO ()
haddockIndex haddockdir documentdir = do
    argIs <- map (\h -> "--read-interface="
               ++   P.encodeString (P.dropExtension $ P.filename h) ++
               ',': P.encodeString h) <$> P.listDirectory haddockdir

    haddock $ "--gen-index": "--gen-contents": ("--odir=" ++ P.encodeString documentdir): argIs

--        dst = docdir
--              P.</> (P.decodeString . display) (docPackage doc)
addSinglePackage :: Bool -> Bool -> P.FilePath -> P.FilePath -> Sql.Connection -> DocInfo -> IO ()
addSinglePackage quiet force docDir haddockDir conn iFile = go `catchIOError` handler
  where
    go = do
        putStr "    " >> putStr (display $ diPackageId iFile) >> putChar ' ' >> hFlush stdout
        when force $ P.removeTree $ docDir P.</> (P.decodeString . display) (diPackageId iFile)
        runResourceT $ docFiles (diPackageId iFile) (diHTMLs iFile)
            $$ (if quiet then id else (progress False  10 '.' =$)) (copyDocument docDir)
        Sql.execute_ conn "BEGIN;"
        ( moduleProvider iFile
            $$ (if quiet then id else (progress True  100 '*' =$)) (CL.mapM_ (liftIO . dispatchProvider conn haddockDir)))
            `onException` (Sql.execute_ conn "ROLLBACK;")
        Sql.execute_ conn "COMMIT;"
    handler ioe
        | isDoesNotExistError ioe = putStr "Error: " >> print   ioe
        | otherwise               = ioError ioe

