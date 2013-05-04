{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TupleSections #-}

import Control.Applicative
import Control.Monad
import Control.Exception

import System.Exit
import System.Environment
import System.Console.GetOpt
import System.FilePath
import System.Directory
import System.Posix.Files

import Data.Char
import Data.Maybe
import qualified Data.Map as M
import Text.XML.Plist

import Database.SQLite.Simple

import qualified Module as GHC
import qualified Name   as GHC
import Documentation.Haddock

data RawOptions =
  RawOptions { output_               :: FilePath
             , inputs_               :: [FilePath]
             , link_                 :: Bool
             , force_                :: Bool
             , full_                 :: Bool
             , cfBundleIdentifier_   :: String
             , cfBundleName_         :: String
             , docSetPlatformFamily_ :: String
             }
  | Help

defaultOptions :: RawOptions
defaultOptions = RawOptions "haskell.docset" [] False False False "haskell" "haskell" "haskell"

set :: (RawOptions -> RawOptions) -> RawOptions -> RawOptions
set _ Help = Help
set f a    = f a

options :: [OptDescr (RawOptions -> RawOptions)]
options =
  [ Option "f" ["force"]     (NoArg $       set (\a -> a{force_  = True}))                "force"
  , Option []  ["full-name"] (NoArg $       set (\a -> a{full_   = True}))                "full name index"
  , Option "l" ["link"]      (NoArg $       set (\a -> a{link_   = True}))                "use symbolic link"
  , Option "o" ["output"]    (ReqArg (\o -> set (\a -> a{output_ = o}))           "FILE") "output file"
  , Option "i" ["input"]     (ReqArg (\o -> set (\a -> a{inputs_ = o:inputs_ a})) "FILE") "input *.haddock file"
  , Option "h" ["help"]      (NoArg $ const Help) "show this message"
  , Option [] ["cf-bundle-identifier"]   (ReqArg (\o -> set (\a -> a{cfBundleIdentifier_   = o}))   "ID") "plist"
  , Option [] ["cf-bundle-name"]         (ReqArg (\o -> set (\a -> a{cfBundleName_         = o})) "NAME") "plist"
  , Option [] ["docset-platform-family"] (ReqArg (\o -> set (\a -> a{docSetPlatformFamily_ = o}))  "FAM") "plist"
  ]

data Options =
  Options { output               :: FilePath
          , inputs               :: [FilePath]
          , link                 :: Bool
          , full                 :: Bool
          , cfBundleIdentifier   :: String
          , cfBundleName         :: String
          , docSetPlatformFamily :: String
          }
  deriving Show

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb f = mb >>= \b -> when b f

parseOptions :: [String] -> IO Options
parseOptions args = do
  header <- getProgName >>= \pn -> return $ "Usage: " ++ pn ++ " [OPTIONS]\nOPTIONS:"
  case getOpt Permute options args of
    (ofs, _, []) -> case foldl (flip id) defaultOptions ofs of
      Help                                  -> putStrLn (usageInfo header options) >> exitSuccess
      RawOptions{output_ = o, inputs_, force_, link_, full_,
                 cfBundleIdentifier_, cfBundleName_, docSetPlatformFamily_} -> do
        whenM (doesDirectoryExist o) $
          if force_
          then removeDirectoryRecursive o
          else throwIO (userError "output file already exists.")
        return $ Options{ output = o
                        , inputs = inputs_
                        , link   = link_
                        , full   = full_
                        , cfBundleIdentifier   = cfBundleIdentifier_
                        , cfBundleName         = cfBundleName_
                        , docSetPlatformFamily = docSetPlatformFamily_
                        }
    (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo header options

--------------------------------------------------------------------------------

writePList :: FilePath -> String -> String -> String -> IO ()
writePList file ident name family =
  writePlistToFile file $ PlDict
  [ ("CFBundleIdentifier",   PlString ident)
  , ("CFBundleName",         PlString name)
  , ("DocSetPlatformFamily", PlString family)
  , ("isDashDocset",         PlBool   True)
  , ("dashIndexFilePath",    PlString "index.html")
  ]

createDirectoryP :: FilePath -> IO ()
createDirectoryP dir = let a:as = splitPath dir
                       in sub a as
  where sub p    []  =    whenM (not <$> doesDirectoryExist p) $ createDirectory p
        sub p (a:as) = do whenM (not <$> doesDirectoryExist p) $ createDirectory p
                          sub (p </> a) as

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory from to = do
  conts <- filter (`notElem` [".", ".."]) <$> getDirectoryContents from
  forM_ conts $ \target -> doesDirectoryExist (from </> target) >>= \isDir ->
    if isDir
    then createDirectory (to </> target) >> copyDirectory (from </> target) (to </> target)
    else copyFile (from </> target) (to </> target)

integration :: Connection -> IO ()
integration conn = do
  execute_ conn "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT)"
  execute_ conn "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path)"

insertIndex :: Connection -> String -> String -> String -> IO ()
insertIndex conn name typ path =
  execute conn "INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES (?, ?, ?)" (name, typ, path)

inTransaction :: String -> (Connection -> IO a) -> IO a
inTransaction connString =
  bracket
  (open connString >>= \conn -> execute_ conn "BEGIN" >> return conn)
  (\conn -> execute_ conn "END" >> close conn)

main :: IO ()
main = do
  opts@Options{output = out, inputs,
               cfBundleIdentifier, cfBundleName, docSetPlatformFamily} <- parseOptions =<< getArgs
  createDirectoryP $ out </> "Contents/Resources/Documents"
  writePList (out </> "Contents" </> "Info.plist") cfBundleIdentifier cfBundleName docSetPlatformFamily
  mbModinp <- inTransaction (out </> "Contents/Resources/docSet.dsidx") $ \conn -> do
    integration conn
    forM inputs $ \file -> do
      eitherInterfaceFile <- readInterfaceFile freshNameCache file
      mbpid <- case eitherInterfaceFile of
        Left err -> putStrLn err >> return Nothing
        Right interfaceFile -> do
          let le = ifLinkEnv interfaceFile
          packageProcess conn opts file le
      return $ (,file) <$> mbpid

  haddock $ [ "--gen-index", "--gen-contents"
            , "--odir=" ++ out </> "Contents/Resources/Documents"
            , "--title=Haskell modules on this system"
            ] ++ map (\(dir,hdck) -> "--read-interface=" ++ dir ++ ',': hdck) (catMaybes mbModinp)

packageProcess :: Connection -> Options -> String -> M.Map GHC.Name GHC.Module -> IO (Maybe String)
packageProcess conn Options{output = out, link, full} file le = case M.minView le of
  Nothing   -> return Nothing
  Just view -> do
    let pid = GHC.packageIdString $ GHC.modulePackageId (fst view :: GHC.Module)
        from = fst $ splitFileName file
        to   = out </> "Contents/Resources/Documents" </> pid
    if link
      then createSymbolicLink from to
      else createDirectory to >> copyDirectory from to
    forM_ (M.toList le) $ uncurry (moduleProcess full conn pid)
    insertIndex conn pid "Library" (pid </> "index.html")
    return $ Just pid

moduleProcess :: Bool -> Connection -> String -> GHC.Name -> GHC.Module -> IO ()
moduleProcess full conn pid name mdl = do
  let nameStr  = GHC.getOccString name
      mdlStr   = GHC.moduleNameString $ GHC.moduleName mdl

      typ      = case ()
                 of _ | GHC.isTyConName   name -> "Class"
                      | GHC.isDataConName name -> "Constructor"
                      | otherwise               -> "Function"
      path     = pid </> map (\c -> if c == '.' then '-' else c) mdlStr <.> "html"
      hash     = '#': (if GHC.isValName name then 'v' else 't'): ':': makeAnchorId nameStr
  insertIndex conn mdlStr  "Module" path
  insertIndex conn ((if full then (mdlStr <.>) else id) nameStr) typ (path ++ hash)

makeAnchorId :: String -> String
makeAnchorId [] = []
makeAnchorId (f:r) = escape isLegal f ++ concatMap (escape isLegal) r
  where
    escape p c | p c = [c]
               | otherwise = '-' : show (ord c) ++ "-"
    isLegal ':' = True
    isLegal '_' = True
    isLegal '.' = True
    isLegal c = isAscii c && isAlphaNum c
