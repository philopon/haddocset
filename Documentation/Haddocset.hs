{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Documentation.Haddocset
  ( globalPackageDirectories
  , packageConfs

  , DocInfo(..)
  , readDocInfoFile

  , ResolutionStrategy(..)
  , addSinglePackage

  , docsetDir
  , haddockIndex
  ) where

#if __GLASGOW_HASKELL__ < 709
import           Control.Applicative
#endif

import           Control.Monad.Catch
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Data.Typeable                     (Typeable)

import           System.FilePath
import           System.Directory

import           System.IO
import           System.IO.Error(mkIOError, alreadyExistsErrorType, isDoesNotExistError)
import           System.Process

import           Network.HTTP.Types.URI (urlEncode)

import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import qualified Data.Text.Encoding                as T
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
import           Data.Conduit.Filesystem (sourceDirectoryDeep)

import Documentation.Haddocset.Index

collapse :: FilePath -> FilePath
collapse = joinPath . reverse . go [] . splitDirectories
  where
    go cs []        = cs
    go cs (".": ps) = go cs ps
    go cs ("..":ps) = go (tail cs) ps
    go cs (p:   ps) = go (p:cs) ps

parent :: FilePath -> FilePath
parent = takeDirectory . dropTrailingPathSeparator

stripPrefixPath :: FilePath -> FilePath -> Maybe FilePath
stripPrefixPath a0 b0 = joinPath <$> go (splitPath a0) (splitPath b0)
  where
    go _      []  = Nothing
    go []     bs  = Just bs
    go (a:as) (b:bs)
      | a == b    = go as bs
      | otherwise = Nothing

docsetDir :: FilePath -> FilePath
docsetDir = flip replaceExtension "docset"

listDirectory :: FilePath -> IO [FilePath]
listDirectory p = map (p </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents p

globalPackageDirectories :: FilePath -> IO [FilePath]
globalPackageDirectories hcPkg = do
    ds <- map init . filter isPkgDBLine . lines <$>
        readProcess hcPkg ["list", "--global"] ""
    forM ds $ \d -> doesDirectoryExist d >>= \isDir -> return $
        if isDir
        then d
        else (takeDirectory d)
  where
    isPkgDBLine ""      = False
    isPkgDBLine (' ':_) = False
    isPkgDBLine _       = True

packageConfs :: FilePath -> IO [FilePath]
packageConfs dir =
    filter (("package.cache" /=) . takeFileName) <$> listDirectory dir

data DocInfo = DocInfo
    { diPackageId  :: PackageId
    , diInterfaces :: [FilePath]
    , diHTMLs      :: [FilePath]
    , diExposed    :: Bool
    } deriving Show

readDocInfoFile :: FilePath -> IO (Maybe DocInfo)
readDocInfoFile pifile = doesDirectoryExist pifile >>= \isDir ->
    if isDir
    then filter ((== ".haddock") . takeExtension) <$> listDirectory pifile >>= \hdc -> case hdc of
        []       -> return Nothing
        hs@(h:_) -> readInterfaceFile freshNameCache h >>= \ei -> case ei of
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
                        return . Just $ DocInfo (fst $ last pkg) hs [collapse pifile] True
            Right _ -> return Nothing
    else do
        result <- parseInstalledPackageInfo <$> readFile pifile
        return $ case result of
            ParseFailed _ -> Nothing
            ParseOk [] a
                | null (haddockHTMLs a)      -> Nothing
                | null (haddockInterfaces a) -> Nothing
                | otherwise -> Just $
                    DocInfo
                        (sourcePackageId a)
                        (haddockInterfaces a)
                        (haddockHTMLs a)
                        (exposed a)

            ParseOk _  _  -> Nothing

copyHtml :: DocFile -> FilePath -> IO ()
copyHtml doc dst = do
    tags <- Ts.parseTags <$> T.readFile (docAbsolute doc)
    T.writeFile dst . Ts.renderTags $ concatMap (addAnchor . modifyUrl) tags
  where
    modifyUrl tag
        | Ts.tagOpenLit "a" (Ts.anyAttrNameLit "href") tag =
            let absp p = collapse $ docBaseDir doc </> p
                attr   = filter (\(n,_) -> n /= "href") (getAttr tag)
            in case T.unpack $ Ts.fromAttrib "href" tag of
                url | "http://"  `isPrefixOf` url -> tag
                    | "https://" `isPrefixOf` url -> tag
                    | "file:///" `isPrefixOf` url -> Ts.TagOpen "a" $ ("href", T.pack . rebase $ drop 7 url) : attr
                    | "#"        `isPrefixOf` url -> Ts.TagOpen "a" $ ("href", T.pack . rebase $ dst ++ url) : attr
                    | otherwise                   -> Ts.TagOpen "a" $ ("href", T.pack . rebase $ absp  url) : attr
        | Ts.tagOpenLit "a" (Ts.anyAttrNameLit "name") tag =
            let Ts.TagOpen _ attr = tag
                hash = '#' : T.unpack (Ts.fromAttrib "name" tag)
            in Ts.TagOpen "a" $ ("href", T.pack . rebase $ dst ++ hash) : attr
        | otherwise = tag

    addAnchor tag@(Ts.TagOpen "a" as)
        | Ts.anyAttrNameLit "name" as && hasClass "def" as =
            maybe id (\(t, n) -> (++)
                [ Ts.TagOpen  "a"
                    [ ("class", "dashAnchor")
                    , ("name", T.concat ["//apple_ref/cpp/", t, "/", T.decodeUtf8 . urlEncode True $ T.encodeUtf8 n])
                    ]
                , Ts.TagClose "a"
                ]) (anchorName =<< lookup "name" as) $
            [tag]
      | otherwise = [tag]
    addAnchor tag = [tag]

    unescape [] = Just []
    unescape ('-':n) = case reads n of
        [(c, '-':o)] -> (toEnum c :) <$> unescape o
        _            -> Nothing
    unescape (c:n) = (c:) <$> unescape n

    anchorName s = fmap T.pack <$> case T.unpack s of
      ('t':_:t)     -> ("Type",) <$> unescape t
      ('v':_:':':c) -> ("Constructor",) <$> unescape (':':c)
      ('v':_:a:as)
        | isUpper a -> ("Constructor",) <$> unescape (a:as)
        | otherwise -> ("Function",) <$> unescape (a:as)
      _ -> Nothing

    hasClass c = maybe False (any (c ==) . T.words) . lookup "class"

    getAttr (TagOpen _ a) = a
    getAttr _             = error "copyHtml: call attr to !TagOpen."

    rebase :: FilePath -> FilePath
    rebase p =
        let file    = takeFileName p
            isSrc   = "src" `elem` splitDirectories (parent p)
            srcNize = if isSrc then ("src" </>) else id
            pkgs    = filter packageLike . reverse $ splitDirectories (parent p)
        in case pkgs of
            []    -> file
            pkg:_ -> ".." </> pkg </> srcNize file

    packageLike :: FilePath -> Bool
    packageLike p =
        let t = T.pack $ dropTrailingPathSeparator p
            (pkg, version) = T.breakOnEnd "-" t
        in not (T.null pkg) && T.all (`elem` ("0123456789." :: String)) version

data DocFile = DocFile
    { docPackage     :: PackageId
    , docBaseDir     :: FilePath
    , docRationalDir :: FilePath
    } deriving Show

docAbsolute :: DocFile -> FilePath
docAbsolute doc = docBaseDir doc </> docRationalDir doc

copyDocument :: (MonadThrow m, MonadIO m) => FilePath
             -> Consumer DocFile m ()
copyDocument docdir = awaitForever $ \doc -> do
    let full = docAbsolute doc
        dst = docdir
              </> display (docPackage doc)
              </> docRationalDir doc
    liftIO $ createDirectoryIfMissing True (takeDirectory dst)
    ex <- liftIO $ (||) <$> doesFileExist dst <*> doesDirectoryExist dst
    when ex $ throwM $ mkIOError alreadyExistsErrorType "copyDocument" Nothing (Just dst)
    case takeExtension $ docRationalDir doc of
        ".html"    -> liftIO $ copyHtml doc dst
        ".haddock" -> return ()
        _          -> liftIO $ copyFile full dst

docFiles :: (MonadIO m, MonadResource m) => PackageId -> [FilePath] -> Producer m DocFile
docFiles sourcePackageId haddockHTMLs =
    forM_ haddockHTMLs $ \dir ->
        sourceDirectoryDeep False dir
        =$= awaitForever
            (\f -> yield
                $ DocFile sourcePackageId dir
                $ fromMaybe (error $ "Prefix mismatch: " ++ show (dir, f))
                $ stripPrefixPath (addTrailingPathSeparator dir) f)

data Provider
    = Haddock  PackageId FilePath
    | Package  PackageId
    | Module   PackageId Ghc.Module
    | Function PackageId Ghc.Module Ghc.Name

moduleProvider :: MonadIO m => DocInfo -> Producer m Provider
moduleProvider iFile =
    mapM_ sub $ diInterfaces iFile
  where
    sub file = do
        rd <- liftIO $ readInterfaceFile freshNameCache file
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

providerToEntry :: FilePath -> Conduit Provider IO IndexEntry
providerToEntry hdir = awaitForever $ \provider -> case provider of
    Haddock p h -> liftIO $
        copyFile h (hdir </> display p <.> "haddock")
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


haddockIndex :: FilePath -> FilePath -> IO ()
haddockIndex haddockdir documentdir = do
    argIs <- map (\h -> "--read-interface="
               ++   (dropExtension $ takeFileName h) ++
               ',': h) <$> listDirectory haddockdir
    haddock $ "--gen-index": "--gen-contents": ("--odir=" ++ documentdir): argIs

-- | What to do if documentation for a package already exists?
data ResolutionStrategy
    = Skip
    | Overwrite
    | Fail
    deriving (Show, Ord, Eq)

addSinglePackage
    :: Bool
    -> ResolutionStrategy
    -> FilePath
    -> FilePath
    -> SearchIndex ReadOnly
    -> DocInfo
    -> IO ()
addSinglePackage quiet resolution docDir haddockDir idx iFile =
    go `catchIOError` handler
  where
    -- Directory to which documentation for this package will be written.
    pkgDocDir = docDir </> display (diPackageId iFile)

    resolveConflict :: PackageId -> IO () -> IO ()
    resolveConflict pkgId io = do
        hasConflict <- doesDirectoryExist pkgDocDir
        if not hasConflict
          then io
          else case resolution of
                 Fail      -> throwM $ AlreadyExists pkgId
                 Overwrite -> do
                    unless quiet $
                        putStrLn $ "    " ++ display pkgId ++
                                   ": Found existing documentation. Deleting."
                    removeDirectoryRecursive pkgDocDir >> io
                 _         ->
                    unless quiet $
                        putStrLn $ "    " ++ display pkgId ++
                                   ": Found existing documentation. Skipping."

    go = resolveConflict (diPackageId iFile) $ do
        -- These operations are run only after resolving a conflict--if
        -- one existed.

        putStr "    "
        putStr (display $ diPackageId iFile)
        putChar ' '
        hFlush stdout

        runResourceT $
          docFiles (diPackageId iFile) (diHTMLs iFile)
            $$ (if quiet then id else (progress False  10 '.' =$))
               (copyDocument docDir)

        withReadWrite idx $ \idx' ->
            moduleProvider iFile
                $= providerToEntry haddockDir
                $$ (if quiet then id else (progress True  100 '*' =$))
                   (sinkEntries idx')

    handler ioe
        | isDoesNotExistError ioe = putStr "Error: " >> print   ioe
        | otherwise               = ioError ioe


data AddException
    = AlreadyExists PackageId
  deriving (Typeable)

instance Exception AddException

instance Show AddException where
    show (AlreadyExists pkg) =
        "Failed to write documentation for " ++ display pkg ++ ". " ++
        "Documentation for it already exists. " ++
        "Use -f/--force to overwrite it or -s/--skip to skip it."
