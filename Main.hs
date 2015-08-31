{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative
import           Control.Monad

import qualified Filesystem                as P
import qualified Filesystem.Path.CurrentOS as P

import           System.IO.Error

import qualified Data.Text                 as T

import           Data.Maybe


import           Options.Applicative

import           Documentation.Haddocset
import           Documentation.Haddocset.Index
import           Documentation.Haddocset.Plist

createCommand :: Options -> IO ()
createCommand o = do
  unless (optQuiet o) $ putStrLn "[1/5] Create Directory."
  P.createDirectory False (optTarget o) -- for fail when directory already exists.
  P.createTree           (optDocumentsDir o)
  P.createDirectory True (optHaddockDir o)

  unless (optQuiet o) $ putStrLn "[2/5] Writing plist."
  P.writeTextFile (optTarget o P.</> "Contents/Info.plist") $
        showPlist (createPlist $ optCommand o)

  unless (optQuiet o) $ putStrLn "[3/5] Migrate Database."
  withSearchIndex (optTarget o P.</> "Contents/Resources/docSet.dsidx") $ \idx -> do

    globalDirs <- globalPackageDirectories (optHcPkg o)
    unless (optQuiet o) $ do
        putStr "    Global package directory: "
        putStr (P.encodeString $ head globalDirs)
        if length globalDirs > 1
            then putStr " and " >> putStr (show . pred $ length globalDirs) >> putStrLn "directories."
            else putStrLn ""
    globals <- concat <$> mapM (\d -> map (d P.</>) <$> packageConfs d) globalDirs
    let locals = toAddFiles $ optCommand o
    iFiles <- filter diExposed . catMaybes <$> mapM readDocInfoFile (globals ++ locals)
    unless (optQuiet o) $ putStr "    Global package count:     " >> print (length globals)

    unless (optQuiet o) $ putStrLn "[4/5] Copy and populate Documents."
    forM_ iFiles $ \iFile ->
        addSinglePackage (optQuiet o) False (optDocumentsDir o) (optHaddockDir o) idx iFile

    unless (optQuiet o) $ putStrLn "[5/5] Create index."
    haddockIndex (optHaddockDir o) (optDocumentsDir o)

addCommand :: Options -> Bool -> IO ()
addCommand o force =
  withSearchIndex (optTarget o P.</> "Contents/Resources/docSet.dsidx") $ \idx -> do
    forM_ (toAddFiles $ optCommand o) $ \i ->
        go idx i `catchIOError` handler
    haddockIndex (optHaddockDir o) (optDocumentsDir o)
  where
    go idx p = readDocInfoFile p >>= \mbIFile -> case mbIFile of
        Nothing    -> return ()
        Just iFile -> addSinglePackage (optQuiet o) force (optDocumentsDir o) (optHaddockDir o) idx iFile
    handler ioe
            | isDoesNotExistError ioe = print   ioe
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
    | Add    { toAddFiles :: [P.FilePath], forceAdd :: Bool }
    deriving Show

main :: IO ()
main = do
    opts <- execParser optRule
    case opts of
        Options{optCommand = Create{}}      -> createCommand opts
        Options{optCommand = List}          -> listCommand   opts
        Options{optCommand = Add{forceAdd}} -> addCommand    opts forceAdd
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
        <$> ( Plist <$> (textOption (long "CFBundleIdentifier")   <|> pure "haskell")
                    <*> (textOption (long "CFBundleName")         <|> pure "Haskell")
                    <*> (textOption (long "DocSetPlatformFamily") <|> pure "haskell"))
        <*> many (argument (P.decodeString <$> str) (metavar "CONFS" <> help "path to installed package configuration."))

    addOpts = Add
        <$> some (argument (P.decodeString <$> str) (metavar "CONFS" <> help "path to installed package configuration."))
        <*> switch (long "force" <> short 'f' <> help "overwrite exist package.")

    textOption = fmap T.pack . strOption
