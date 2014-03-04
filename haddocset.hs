{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, OverloadedStrings, StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards, TupleSections, NamedFieldPuns, Rank2Types #-}

module Haddocset where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import qualified Filesystem as P
import qualified Filesystem.Path.CurrentOS as P

import System.Process
import qualified Database.SQLite.Simple as Sql

import Data.Maybe

import Distribution.InstalledPackageInfo
import Distribution.Text(display)
import Distribution.Package
import Documentation.Haddock

import Text.HTML.TagSoup as Ts
import Text.HTML.TagSoup.Match as Ts
import qualified Data.Set as S

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Filesystem as P

import qualified Module as Ghc
import qualified Name as Ghc

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


parseConf :: P.FilePath -> IO (Maybe InstalledPackageInfo)
parseConf pifile = do
    result <- parseInstalledPackageInfo <$> readFile (P.encodeString pifile)
    return $ case result of
        ParseFailed _ -> Nothing
        ParseOk [] a
            | null (haddockHTMLs a)      -> Nothing
            | null (haddockInterfaces a) -> Nothing
            | otherwise -> Just a

        ParseOk _  _  -> Nothing

data Plist = Plist 
    { cfBundleIdentifier   :: String
    , cfBundleName         :: String
    , docSetPlatformFamily :: String
    } 

instance Show Plist where
    show Plist{..} = unlines
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

copyHtml :: S.Set String -> P.FilePath -> P.FilePath -> IO ()
copyHtml pkgs src dst = do
    tags <- Ts.parseTags <$> P.readTextFile src
    P.writeTextFile dst (Ts.renderTags . map mapFunc $ tags)
  where
    mapFunc tag
        | Ts.tagOpenLit "a" (Ts.anyAttrNameLit "href") tag =
            let url  = Ts.fromAttrib "href" tag
                attr = ("href", rebase url) :
                       filter (\(n,_) -> n /= "href") (getAttr tag)
            in Ts.TagOpen "a" attr
        | otherwise = tag

    getAttr (TagOpen _ a) = a
    getAttr _             = error "copyHtml: call attr to !TagOpen."

    rebase url = url

data DocFile = DocFile
    { docPackage     :: PackageId
    , docBaseDir     :: P.FilePath
    , docRationalDir :: P.FilePath
    }

copyDocument :: MonadIO m => S.Set String -> P.FilePath
             -> Consumer DocFile m ()
copyDocument pkgs docset = awaitForever $ \doc -> do
    liftIO $ print (docPackage doc)
    let full = docBaseDir doc P.</> docRationalDir doc
        dst = docset
              P.</> "Contents/Resources/Documents"
              P.</> (P.decodeString . display) (docPackage doc)
              P.</> docRationalDir doc
    liftIO $ P.createTree (P.directory dst)
    case P.extension $ docRationalDir doc of
        Just "html"    -> liftIO $ copyHtml   pkgs full dst
        Just "haddock" -> return ()
        _              -> liftIO $ P.copyFile full dst

docFiles :: MonadIO m => PackageId -> [FilePath] -> Producer m DocFile
docFiles sourcePackageId haddockHTMLs =
    forM_ haddockHTMLs $ \d ->
        let dir = P.decodeString $ d ++ "/"
        in P.traverse False dir
            =$= awaitForever (\f -> yield $ DocFile sourcePackageId dir $ fromMaybe (error $ show (dir ,f)) $ P.stripPrefix dir f)

data Provider
    = Haddock  PackageId P.FilePath
    | Package  PackageId
    | Module   PackageId Ghc.Module
    | Function PackageId Ghc.Module Ghc.Name

moduleProvider :: MonadIO m => InstalledPackageInfo -> Producer m Provider
moduleProvider iFile = 
    mapM_ sub $ haddockInterfaces iFile
  where 
    sub file = do
        rd <- liftIO $ readInterfaceFile freshNameCache file
        case rd of
            Left _ -> return ()
            Right (ifInstalledIfaces -> iIntrf) -> do 
                let pkg = sourcePackageId iFile
                yield $ Haddock pkg (P.decodeString file)
                yield $ Package pkg
                liftIO $ print pkg
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

create :: String -> Plist -> P.FilePath -> IO ()
create hcPkg plist (docsetDir -> dir) = do
    P.createDirectory False dir

    let haddocDir = dir P.</> "Contents/Resources/Haddock"
    P.createTree $ dir P.</> "Contents/Resources/Documents"
    P.createDirectory True haddocDir

    writeFile (P.encodeString $ dir P.</> "Contents/Info.plist") $ show plist

    conn <- Sql.open . P.encodeString $ dir P.</> "Contents/Resources/docSet.dsidx"
    Sql.execute_ conn "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT, package TEXT);"
    Sql.execute_ conn "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path, package);"

    global <- globalPackageDirectory hcPkg
    iFiles <- fmap (filter exposed . catMaybes) $ mapM (parseConf . (global P.</>)) =<< packageConfs global

    let pkgs   = S.fromList $ map (display . sourcePackageId) iFiles
    mapM_ (\i -> docFiles (sourcePackageId i) (haddockHTMLs i)) iFiles $$ copyDocument pkgs dir

    Sql.execute_ conn "BEGIN;"
    mapM_ moduleProvider iFiles $$ CL.mapM_ (\i -> case i of
        Haddock  p h   -> do
            let dst = haddocDir P.</> P.decodeString (display p) P.<.> "haddock"
            liftIO $ P.copyFile h dst
        Package  p     -> liftIO (populatePackage  conn p)
        Module   p m   -> liftIO (populateModule   conn p m)
        Function p m n -> liftIO (populateFunction conn p m n)
        )
    Sql.execute_ conn "COMMIT;"

    argIs <- map (\h -> "--read-interface="
               ++   P.encodeString (P.dropExtension $ P.filename h) ++
               ',': P.encodeString h) <$> P.listDirectory haddocDir

    haddock $  "--gen-index": "--gen-contents": ("--odir=" ++ P.encodeString (dir P.</> "Contents/Resources/Documents/")): argIs

    return ()

main :: IO ()
main = create "ghc-pkg" (Plist "haskell" "Haskell" "haskell") "haskell"

{-
instance Show Ghc.Module where
    show m = Ghc.moduleNameString (Ghc.moduleName m) ++ '(': Ghc.packageIdString (Ghc.modulePackageId m) ++ ")"

instance Show Ghc.ModuleName where
    show = Ghc.moduleNameString

deriving instance Show a => Show (Doc a)
instance Show Ghc.OccName where
    show _ = "[occname]"

instance Show Ghc.Name where
    show n = ((Ghc.getOccString n ++ "[") ++)
             $ (if Ghc.isSystemName   n then ('S':) else id)
             $ (if Ghc.isInternalName n then ('I':) else id)
             $ (if Ghc.isExternalName n then ('E':) else id)
             $ (if Ghc.isTyVarName n then ('V':) else id)
             $ (if Ghc.isTyConName n then ('C':) else id)
             $ (if Ghc.isDataConName n then ('D':) else id)
             $ (if Ghc.isValName n then ('l':) else id)
             $ (if Ghc.isVarName n then ('v':) else id)
             $ (if Ghc.isWiredInName n then ('w':) else id)
             $ (if Ghc.isBuiltInSyntax n then ('B':) else id) "]"
             -}
