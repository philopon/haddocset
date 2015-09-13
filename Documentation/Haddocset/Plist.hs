{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Documentation.Haddocset.Plist
    ( Plist(..)
    , showPlist
    ) where

import Data.Monoid
import Data.Text   (Text)

import qualified Data.Text as T

data Plist = Plist
    { cfBundleIdentifier   :: Text
    , cfBundleName         :: Text
    , docSetPlatformFamily :: Text
    } deriving Show

showPlist :: Plist -> Text
showPlist Plist{..} = T.unlines
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">"
    , "<plist version=\"1.0\">"
    , "<dict>"
    , "<key>CFBundleIdentifier</key>"
    , "<string>" <> cfBundleIdentifier <> "</string>"
    , "<key>CFBundleName</key>"
    , "<string>" <> cfBundleName <> "</string>"
    , "<key>DocSetPlatformFamily</key>"
    , "<string>" <> docSetPlatformFamily <> "</string>"
    , "<key>isDashDocset</key>"
    , "<true/>"
    , "<key>dashIndexFilePath</key>"
    , "<string>index.html</string>"
    , "</dict>"
    , "</plist>"
    ]
