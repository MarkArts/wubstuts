{-# LANGUAGE OverloadedStrings #-}

module Settings
(
    getSettings,
    Settings,
    website_path,
    ignore_folders

) where

import Data.Aeson
import Data.Maybe
import Control.Applicative
import qualified Data.ByteString.Lazy as B

settingsFile :: FilePath
settingsFile = "config/settings-example.json"

data Settings = Settings
    {
        website_path :: FilePath,
        ignore_folders :: [FilePath]
    } deriving (Show)

instance FromJSON Settings where
  parseJSON (Object v) =
    Settings <$>
    (v .: "website_path")    <*>
    (v .: "ignore_folders")


getSettings :: IO Settings
getSettings = do
    settingsContent <- B.readFile settingsFile
    let settings = decode settingsContent :: Maybe Settings
    if isJust settings then do
        return $ fromMaybe (Settings "This won't happen anyway" []) settings
    else
        error "Couldn't parse the settings file"

