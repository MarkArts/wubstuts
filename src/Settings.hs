{-# LANGUAGE OverloadedStrings #-}

module Settings
(
    getSettings,
    getSetting,
    Settings,
    website_path,
    search_depth,
    ignore_folders,
    wordpress_site,
    drupal_site

) where

import Data.Aeson
import Control.Applicative
import qualified Data.ByteString.Lazy as B

settingsFile :: FilePath
settingsFile = "config/settings.json"

data Settings = Settings
    {
        website_path :: FilePath,
        search_depth :: Int,
        ignore_folders :: [FilePath],
        wordpress_site :: [[FilePath]],
        drupal_site :: [[FilePath]]
    } deriving (Show)

instance FromJSON Settings where
  parseJSON (Object v) =
    Settings <$>
    (v .: "website_path")       <*>
    (v .: "search_depth")       <*>
    (v .: "ignore_folders")     <*>
    (v .: "wordpress_site")     <*>
    (v .: "drupal_site")

getSettings :: IO Settings
getSettings = do
    settingsContent <- B.readFile settingsFile
    let settings = decode settingsContent :: Maybe Settings
    case settings of
        Just s  -> return s
        Nothing -> error "Couldn't parse settings file"

getSetting :: (Settings -> a) -> IO a
getSetting f = getSettings >>= return . f

