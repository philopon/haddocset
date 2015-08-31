{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE CPP                #-}

module Documentation.Haddocset where

#if __GLASGOW_HASKELL__ < 709
import           Control.Applicative
#endif

import           Control.Monad.Catch
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class

import qualified Filesystem                        as P
import qualified Filesystem.Path.CurrentOS         as P

import           System.IO
import           System.IO.Error(mkIOError, alreadyExistsErrorType, isDoesNotExistError)
import           System.Process

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

import Documentation.Haddocset.Index

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
#if __GLASGOW_HASKELL__ >= 710
                let rPkg = readP_to_S parse . Ghc.packageKeyString . Ghc.modulePackageKey $ instMod intf :: [(PackageId, String)]
#else
                let rPkg = readP_to_S parse . Ghc.packageIdString . Ghc.modulePackageId $ instMod intf :: [(PackageId, String)]
#endif
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
            isSrc   = (`elem` P.splitDirectories (P.parent p)) `any` ["src", "src/"]
            srcNize = if isSrc then ("src" P.</>) else id
            pkgs    = filter packageLike . reverse $ P.splitDirectories (P.parent p)
        in case pkgs of
            []    -> file
            pkg:_ -> ".." P.</> pkg P.</> srcNize file

    packageLike p =
        let t' = both (P.toText p)
            t = fromMaybe t' (T.stripSuffix "/" t')
            (pkg, version) = T.breakOnEnd "-" t
        in not (T.null pkg) && T.all (`elem` ("0123456789." :: String)) version

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
        =$= awaitForever
            (\f -> yield
                $ DocFile sourcePackageId dir
                $ fromMaybe (error $ "Prefix missmatch: " ++ show (dir, f))
                $ P.stripPrefix dir (P.decodeString f))

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

moduleNmaeUrl :: String -> String
moduleNmaeUrl = map dot2Dash
  where dot2Dash '.'  = '-'
        dot2Dash c    = c

progress :: MonadIO m => Bool -> Int -> Char -> ConduitM o o m ()
progress cr u c = sub 1
  where
    sub n = await >>= \mbi -> case mbi of
        Nothing -> when cr $ liftIO (hPutChar stderr '\n' >> hFlush stderr)
        Just i  -> do
            when (n `mod` u == 0) $ liftIO (hPutChar stderr c >> hFlush stderr)
            yield i
            sub (succ n)

providerToEntry :: P.FilePath -> Conduit Provider IO IndexEntry
providerToEntry hdir = awaitForever $ \provider -> case provider of
    Haddock p h -> liftIO $
        P.copyFile
            h (hdir  P.</> P.decodeString (display p) P.<.> "haddock")
    Package pkg ->
        yield $! IndexEntry
            { entryName = T.pack (display pkg)
            , entryType = PackageEntry
            , entryPath = display pkg ++ "/index.html"
            , entryPackage = pkg
            }
    Module pkg modn ->
        yield $! IndexEntry
            { entryName = T.pack $ Ghc.moduleNameString $ Ghc.moduleName modn
            , entryType = ModuleEntry
            , entryPath = display pkg ++ '/' : moduleNameURL modn ++ ".html"
            , entryPackage = pkg
            }
    Function pkg modn name ->
        let typ = case () of
                    _ | Ghc.isTyConName   name -> TypeEntry
                      | Ghc.isDataConName name -> ConstructorEntry
                      | otherwise              -> FunctionEntry
            prefix = case typ of
                        TypeEntry -> 't'
                        _         -> 'v'
        in yield $! IndexEntry
            { entryName = T.pack $ Ghc.getOccString name
            , entryType = typ
            , entryPath
                = display pkg ++ '/'
                : moduleNameURL modn ++ ".html#"
               ++ prefix : ':' : escapeSpecial (Ghc.getOccString name)
            , entryPackage = pkg
            }

  where
    moduleNameURL = moduleNmaeUrl . Ghc.moduleNameString . Ghc.moduleName

    specialChars  = "!&|+$%(,)*<>-/=#^\\?" :: String
    escapeSpecial = concatMap (\c -> if c `elem` specialChars then '-': show (fromEnum c) ++ "-" else [c])


haddockIndex :: P.FilePath -> P.FilePath -> IO ()
haddockIndex haddockdir documentdir = do
    argIs <- map (\h -> "--read-interface="
               ++   P.encodeString (P.dropExtension $ P.filename h) ++
               ',': P.encodeString h) <$> P.listDirectory haddockdir

    haddock $ "--gen-index": "--gen-contents": ("--odir=" ++ P.encodeString documentdir): argIs


addSinglePackage
    :: Bool -> Bool -> P.FilePath -> P.FilePath
    -> SearchIndex ReadOnly
    -> DocInfo
    -> IO ()
addSinglePackage quiet force docDir haddockDir idx iFile = go `catchIOError` handler
  where
    go = do
        putStr "    "
        putStr (display $ diPackageId iFile)
        putChar ' '
        hFlush stdout

        when force $
            P.removeTree $
                docDir P.</> (P.decodeString . display) (diPackageId iFile)

        runResourceT $ docFiles (diPackageId iFile) (diHTMLs iFile)
            $$ (if quiet then id else (progress False  10 '.' =$)) (copyDocument docDir)
        withReadWrite idx $ \idx' ->
            moduleProvider iFile
                $= providerToEntry haddockDir
                $$ (if quiet then id else (progress True  100 '*' =$))
                   (sinkEntries idx')

    handler ioe
        | isDoesNotExistError ioe = putStr "Error: " >> print   ioe
        | otherwise               = ioError ioe
