{-#LANGUAGE TemplateHaskell, NamedFieldPuns, OverloadedStrings, TupleSections#-}

import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Lens hiding ((<.>))

import System.Exit
import System.Environment
import System.Console.GetOpt
import System.FilePath
import System.Directory
import System.Posix.Files

import Database.SQLite.Simple

import Text.XML
import Text.XML.Plist

import Data.Maybe
import Data.Char
import Data.String (fromString)
import qualified Data.Text     as T
import qualified Data.Map.Lazy as ML

import qualified Text.HTML.DOM as HTML
import Text.XML.Cursor

import qualified Module as GHC
import Documentation.Haddock hiding (Interface)

--------------------------------------------------------------------------------

data Options a =
  Options { _overwrite            :: Bool
          , _fullName             :: Bool
          , _output               :: FilePath
          , _inputs               :: [a]
          , _cfBundleIdentifier   :: String
          , _cfBundleName         :: String
          , _docSetPlatformFamily :: String
          }
  | Help
  deriving Show

type Interface = (Maybe FilePath, FilePath)

makeLenses ''Options

defaultOptions :: Options FilePath
defaultOptions = Options { _overwrite            = False
                         , _fullName             = False
                         , _output               = "haskell.docset"
                         , _inputs               = []
                         , _cfBundleIdentifier   = "haskell"
                         , _cfBundleName         = "haskell"
                         , _docSetPlatformFamily = "haskell"
                         }

options :: [OptDescr (Options FilePath -> Options FilePath)]
options =
  [ Option "f" ["force"]                 (NoArg  $ overwrite .~ True) "over write"
  , Option []  ["full-name"]             (NoArg  $ fullName  .~ True) "full name"
  , Option "h" ["help"]                  (NoArg  $ const Help) "show this message"
  , Option "o" ["output"]                (ReqArg (output .~)              "FILE") "output"
  , Option "i" ["input"]                 (ReqArg ((inputs %~) . cons)     "FILE") "input"
  , Option [] ["cf-bundle-identifier"]   (ReqArg (cfBundleIdentifier .~)    "ID") "plist"
  , Option [] ["cf-bundle-name"]         (ReqArg (cfBundleName .~)        "NAME") "plist"
  , Option [] ["docset-platform-family"] (ReqArg (docSetPlatformFamily .~) "FAM") "plist"
  ]

parseOptions :: [String] -> IO (Options Interface)
parseOptions args = do
  header <- getProgName >>= \pn -> return $ "Usage: " ++ pn ++ " [OPTIONS]\nOPTIONS:"
  case getOpt Permute options args of
    (ofs, _, []) -> case foldl (flip id) defaultOptions ofs of
      Help -> putStrLn (usageInfo header options) >> exitSuccess
      opts -> do
        oe <- doesFileOrDirExist (opts ^. output)
        when (not (opts ^?! overwrite) && oe) $ putStrLn "output already exists." >> exitFailure
        inps <- opts ^!! inputs . folded . act toInterface
        return $ opts & inputs .~ inps
    (_, _, errs) -> ioError $ userError $ concat errs ++ usageInfo header options

doesFileOrDirExist :: FilePath -> IO Bool
doesFileOrDirExist fd = (||) <$> doesFileExist fd <*> doesDirectoryExist fd

--------------------------------------------------------------------------------

createDirectoryP :: FilePath -> IO ()
createDirectoryP dir = foldM_ sub "" (splitPath dir)
  where sub p a = do e <- doesDirectoryExist (p </> a)
                     unless e $ createDirectory (p </> a)
                     return $ p </> a

writePList :: FilePath -> String -> String -> String -> IO ()
writePList file ident name family =
  writePlistToFile file $ PlDict
  [ ("CFBundleIdentifier",   PlString ident)
  , ("CFBundleName",         PlString name)
  , ("DocSetPlatformFamily", PlString family)
  , ("isDashDocset",         PlBool   True)
  , ("dashIndexFilePath",    PlString "index.html")
  ]

integration :: Connection -> IO ()
integration conn = do
  execute_ conn "CREATE TABLE IF NOT EXISTS searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT);"
  execute_ conn "CREATE UNIQUE INDEX IF NOT EXISTS anchor ON searchIndex (name, type, path);"
  execute_ conn "DELETE FROM searchIndex;"

scaffolding :: Options Interface -> IO ()
scaffolding opts = do
  createDirectoryP $ opts ^. output </> "Contents/Resources/Documents"
  linkRoot $ opts ^. output </> "Contents/Resources/Documents/root"
  writePList (opts ^. output </> "Contents/Info.plist")
    (opts ^. cfBundleIdentifier) (opts ^. cfBundleName) (opts ^. docSetPlatformFamily)

linkRoot :: FilePath -> IO ()
linkRoot root = fileExist root >>= \e ->
  if e
  then do stat <- getSymbolicLinkStatus root
          if isSymbolicLink stat
            then (== "/") <$> readSymbolicLink root >>=
                 \toRoot -> unless toRoot $ removeLink root >> createSymbolicLink "/" root
            else throwIO $ userError "cannot create root directory link. maybe already exists."
  else createSymbolicLink "/" root

--------------------------------------------------------------------------------

inTransaction :: String -> (Connection -> IO a) -> IO a
inTransaction connString =
  bracket
  (open connString >>= \conn -> execute_ conn "BEGIN" >> return conn)
  (\conn -> execute_ conn "END" >> close conn)

insertIndex :: Connection -> T.Text -> T.Text -> FilePath -> IO ()
insertIndex conn name typ path =
  execute conn "INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES (?, ?, ?);" (name, typ, path)

--------------------------------------------------------------------------------

type Reference = (Maybe T.Text, T.Text, T.Text)
parseDocument :: FilePath -> IO [Reference]
parseDocument path = do
  cur  <- fromDocument <$> HTML.readFile (fromString path)
  let defs    = cur $// hasClass "def"
  let refs    = catMaybes $ flip map defs $ \node -> do
        let typ = listToMaybe $
                  parent node >>= (child >=> hasClass "keyword" >=> child >=> content)
        fun  <- listToMaybe $ node $| child >=> content
        name <- listToMaybe $ attribute "name" node
        return (typ, funEsc fun, name)
  return refs
  where funEsc a = if let c = T.head a in isAlpha c || (c `elem` "_(")
                   then a
                   else '(' `T.cons` a `T.snoc` ')'

hasClass :: T.Text -> Axis
hasClass c = checkElement $ \e -> case ML.lookup "class" (elementAttributes e) of
  Nothing  -> False
  Just cls -> c `T.isInfixOf` cls

toEntryType :: Reference -> T.Text
toEntryType (Just "type",    _, _) = "Type"
toEntryType (Just "data",    _, _) = "Class"
toEntryType (Just "newtype", _, _) = "Class"
toEntryType (Just "class",   _, _) = "Mixin"
toEntryType (_, f, _) = if isUpper $ T.head f
                        then "Constructor"
                        else "Function"

--------------------------------------------------------------------------------

toInterface :: FilePath -> IO Interface
toInterface path = let (a, b) = span (/= ',') path
                   in if null b
                      then (Nothing,) <$> canonicalizePath a
                      else (,) <$> (Just <$> canonicalizePath a) <*> canonicalizePath (tail b)

insertModule :: Connection -> Options Interface -> FilePath -> String -> IO ()
insertModule conn opts dir mdl = do
  let path = dir </> map (\c -> if c == '.' then '-' else c) mdl <.> "html"
  e <- doesFileExist path
  when e $ do
    insertIndex conn (T.pack mdl) "Module" ("root" ++ path)
    mapM_ (insert $ "root" ++ path) =<< parseDocument path
  where insert path ref = insertIndex conn
                          ((if opts ^?! fullName
                            then ((T.pack mdl `T.snoc` '.') `T.append`)
                            else id) $ ref^._2)
                          (toEntryType ref)
                          (path ++ '#': ref^._3.to T.unpack)

main :: IO ()
main = do
  opts <- parseOptions =<< getArgs
  scaffolding opts 
  inTransaction (opts ^. output </> "Contents/Resources/docSet.dsidx") $ \conn -> do
    integration conn
    forM_ (opts ^. inputs) $ \(mbdir, hdck) -> do
      eif <- readInterfaceFile freshNameCache hdck
      case ifInstalledIfaces <$> eif of
        Left  err   -> putStrLn err
        Right ifces -> do
          let mods = map (GHC.moduleNameString . GHC.moduleName . instMod) .
                     filter ((OptHide `notElem`) . instOptions) $ ifces
              pkg  = GHC.packageIdString . GHC.modulePackageId . instMod <$> listToMaybe ifces
          let dir = fromMaybe (takeDirectory hdck) mbdir
          when (isJust pkg) $
            insertIndex conn (T.pack $ fromJust pkg) "Package" ("root" ++ dir </> "index.html")
          mapM_ (insertModule conn opts dir) mods
  haddock $ [ "--gen-index", "--gen-contents"
            , "--odir=" ++ opts ^. output </> "Contents/Resources/Documents"
            , "--title=Haskell modules on this system"
            ] ++ map toHaddockInterface (opts ^. inputs)

toHaddockInterface :: Interface -> String
toHaddockInterface (Nothing, f) = "--read-interface=" ++ f
toHaddockInterface (Just d,  f) = "--read-interface=" ++ d ++ ',': f
