{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TupleSections     #-}

module Main ( main ) where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable             (asum)

import           System.FilePath
import           System.Directory

import           System.IO.Error

import qualified Data.Text                 as T
import qualified Data.Text.IO              as T

import           Data.Maybe


import           Options.Applicative

import           Documentation.Haddocset
import           Documentation.Haddocset.Index
import           Documentation.Haddocset.Plist

createCommand :: Options -> IO ()
createCommand o = do
  unless (optQuiet o) $ putStrLn "[1/5] Create Directory."
  createDirectory (optTarget o) -- for fail when directory already exists.
  createDirectoryIfMissing True (optDocumentsDir o)
  createDirectoryIfMissing False (optHaddockDir o)

  unless (optQuiet o) $ putStrLn "[2/5] Writing plist."
  T.writeFile (optTarget o </> "Contents/Info.plist") $
        showPlist (createPlist $ optCommand o)

  unless (optQuiet o) $ putStrLn "[3/5] Migrate Database."
  withSearchIndex (optTarget o </> "Contents/Resources/docSet.dsidx") $ \idx -> do

    globalDirs <- globalPackageDirectories (optHcPkg o)
    unless (optQuiet o) $ do
        putStr "    Global package directory: "
        putStr (head globalDirs)
        if length globalDirs > 1
            then putStr " and " >> putStr (show . pred $ length globalDirs) >> putStrLn "directories."
            else putStrLn ""
    globals <- concat <$> mapM (\d -> map (d </>) <$> packageConfs d) globalDirs
    let locals = toAddFiles $ optCommand o
    iFiles <- filter diExposed . catMaybes <$> mapM readDocInfoFile (globals ++ locals)
    unless (optQuiet o) $ putStr "    Global package count:     " >> print (length globals)

    unless (optQuiet o) $ putStrLn "[4/5] Copy and populate Documents."
    forM_ iFiles $ \iFile ->
        addSinglePackage (optQuiet o) Fail (optDocumentsDir o) (optHaddockDir o) idx iFile

    unless (optQuiet o) $ putStrLn "[5/5] Create index."
    haddockIndex (optHaddockDir o) (optDocumentsDir o)

addCommand :: Options -> ResolutionStrategy -> IO ()
addCommand o resolution =
  withSearchIndex (optTarget o </> "Contents/Resources/docSet.dsidx") $ \idx -> do
    forM_ (toAddFiles $ optCommand o) $ \i ->
        go idx i `catchIOError` handler
    haddockIndex (optHaddockDir o) (optDocumentsDir o)
  where
    go idx p = readDocInfoFile p >>= \mbIFile -> case mbIFile of
        Nothing    -> return ()
        Just iFile -> addSinglePackage (optQuiet o) resolution (optDocumentsDir o) (optHaddockDir o) idx iFile
    handler ioe
            | isDoesNotExistError ioe = print   ioe
            | otherwise               = ioError ioe

listCommand :: Options -> IO ()
listCommand o =
    mapM_ (putStrLn . dropExtension . takeFileName) =<< getDirectoryContents (optHaddockDir o)

data Options
    = Options { optHcPkg   :: String
              , optTarget  :: FilePath
              , optQuiet   :: Bool
              , optCommand :: Command
              }
    deriving Show

optHaddockDir, optDocumentsDir :: Options -> FilePath
optHaddockDir   opt = optTarget opt </> "Contents/Resources/Haddock/"
optDocumentsDir opt = optTarget opt </> "Contents/Resources/Documents/"

data Command
    = Create { createPlist :: Plist, toAddFiles :: [FilePath] }
    | List
    | Add    { toAddFiles :: [FilePath]
             , resolution :: ResolutionStrategy
             }
    deriving Show

main :: IO ()
main = do
    opts <- execParser optRule
    case opts of
        Options{optCommand = Create{}}        -> createCommand opts
        Options{optCommand = List}            -> listCommand   opts
        Options{optCommand = Add{resolution}} -> addCommand    opts resolution
  where
    optRule = info (helper <*> options) fullDesc
    options = Options
        <$> (strOption (long "hc-pkg" <> metavar "CMD" <> help "hc-pkg command (default: ghc-pkg)") <|> pure "ghc-pkg")
        <*> fmap docsetDir
            (strOption (long "target" <> short 't' <> metavar "DOCSET" <> help "output directory (default: haskell.docset)") <|> pure "haskell")
        <*> switch (long "quiet" <> short 'q' <> help "suppress output.")
        <*> subparser (command "create" (info createOpts  $ progDesc "crate new docset.")
                    <> command "list"   (info (pure List) $ progDesc "list package of docset.")
                    <> command "add"    (info addOpts $ progDesc "add package to docset."))

    createOpts = Create
        <$> ( Plist <$> (textOption (long "CFBundleIdentifier")   <|> pure "haskell")
                    <*> (textOption (long "CFBundleName")         <|> pure "Haskell")
                    <*> (textOption (long "DocSetPlatformFamily") <|> pure "haskell"))
        <*> many (argument str (metavar "CONFS" <> help "path to installed package configuration."))

    addOpts = Add
        <$> some (argument str (metavar "CONFS" <> help "path to installed package configuration."))
        <*> asum
            [ flag' Overwrite (long "force" <> short 'f' <> help "overwrite exist package.")
            , flag' Skip (long "skip" <> short 's' <> help "skip existing packages")
            , pure Fail
            ]

    textOption = fmap T.pack . strOption
