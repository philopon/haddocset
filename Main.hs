{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

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
import qualified Data.Map as M
import Text.XML.Plist

import Database.SQLite.Simple

import qualified Module as GHC
import qualified Name   as GHC
import Documentation.Haddock

data RawOptions =
  RawOptions { output_ :: FilePath
             , inputs_ :: [FilePath]
             , link_   :: Bool
             , force_  :: Bool
             }
  | Help

defaultOptions :: RawOptions
defaultOptions = RawOptions "haskell.docset" [] False False

set :: (RawOptions -> RawOptions) -> RawOptions -> RawOptions
set _ Help = Help
set f a    = f a

options :: [OptDescr (RawOptions -> RawOptions)]
options =
  [ Option "f" ["force"]  (NoArg $       set (\a -> a{force_ = True}))                 "force"
  , Option "l" ["link"]   (NoArg $       set (\a -> a{link_  = True}))                 "use symbolic link"
  , Option "o" ["output"] (ReqArg (\o -> set (\a -> a{output_ = o}))           "FILE") "output file"
  , Option "i" ["input"]  (ReqArg (\o -> set (\a -> a{inputs_ = o:inputs_ a})) "FILE") "input *.haddock file"
  , Option "h" ["help"]   (NoArg $ const Help) "show this message"
  ]

data Options =
  Options { output :: FilePath
          , inputs :: [FilePath]
          , link   :: Bool
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
      RawOptions{output_ = o, inputs_ = is, force_ = f, link_ = l} -> do
        whenM (doesDirectoryExist o) $
          if f
          then removeDirectoryRecursive o
          else throwIO (userError "output file already exists.")
        return $ Options o is l
    (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo header options

--------------------------------------------------------------------------------

writePList :: FilePath -> String -> String -> String -> IO ()
writePList file ident name family =
  writePlistToFile file $ PlDict
  [ ("CFBundleIdentifier",   PlString ident)
  , ("CFBundleName",         PlString name)
  , ("DocSetPlatformFamily", PlString family)
  , ("isDashDocset",         PlBool   True)
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
  forM_ conts $ \target -> doesDirectoryExist (from </> target) >>= \isDirectory ->
    if isDirectory
    then createDirectory (to </> target) >> copyDirectory (from </> target) (to </> target)
    else copyFile (from </> target) (to </> target)

main :: IO ()
main = do
  Options{output = out, inputs, link} <- parseOptions =<< getArgs
  createDirectoryP $ out </> "Contents/Resources/Documents"
  writePList (out </> "Contents" </> "Info.plist") "haskell" "haskell" "haskell"  
  withConnection (out </> "Contents/Resources/docSet.dsidx") $ \conn -> do
    execute_ conn "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT)"
    execute_ conn "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path)"
    forM_ inputs $ \f -> do
      eitherInterfaceFile <- readInterfaceFile freshNameCache f
      case eitherInterfaceFile of
        Left err -> putStrLn err
        Right interfaceFile -> do
          let le   = ifLinkEnv interfaceFile
              -- pid  = GHC.packageIdString. GHC.modulePackageId. snd $ M.findMin le
          case M.minView le of
            Nothing -> return ()
            Just view -> do
              let pid = GHC.packageIdString. GHC.modulePackageId. fst $ view
                  from = fst $ splitFileName f
                  to   = out </> "Contents/Resources/Documents" </> pid
              if link 
                then createSymbolicLink from to
                else createDirectory to >> copyDirectory from to
              forM_ (M.toList le) $ \(name_, mod_) -> do
                let name     = GHC.getOccString name_
                    typ      = case () of
                      _ | GHC.isTyConName   name_ -> "Class"
                        | GHC.isDataConName name_ -> "Constructor"
                        | otherwise               -> "Function"
                    modle    = GHC.moduleNameString $ GHC.moduleName mod_
                    path     = pid </> map (\c -> if c == '.' then '-' else c) modle <.> "html"
                    hash     = '#': (if GHC.isValName name_ then 'v' else 't'): ':': makeAnchorId name
                execute conn "INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES (?, ?, ?)"
                  (name, typ :: String, path ++ hash)

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
