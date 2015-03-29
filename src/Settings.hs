{-# LANGUAGE OverloadedStrings #-}

module Settings
(
    getSettings,
    getSetting,
    Settings,
    website_path,
    search_depth,
    ignore_folders

) where

import Data.Aeson
import Data.Maybe
import Control.Applicative
import qualified Data.ByteString.Lazy as B

settingsFile :: FilePath
settingsFile = "config/settings.json"

data Settings = Settings
    {
        website_path :: FilePath,
        search_depth :: Int,
        ignore_folders :: [FilePath]
    } deriving (Show)

instance FromJSON Settings where
  parseJSON (Object v) =
    Settings <$>
    (v .: "website_path")    <*>
    (v .: "search_depth")    <*>
    (v .: "ignore_folders")

getSettings :: IO Settings
getSettings = do
    settingsContent <- B.readFile settingsFile
    let settings = decode settingsContent :: Maybe Settings
    if isJust settings then do
        return $ fromMaybe (Settings "This won't happen anyway" 0 []) settings
    else
        error "Couldn't parse the settings file"

getSetting :: (Settings -> a) -> IO a
getSetting f = do
    settings <- getSettings
    return $ f settings