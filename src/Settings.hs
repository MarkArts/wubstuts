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
import Control.Monad.State
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

-- A stateful Settings monad to avoid parsing the
-- entire file on each getSetting call
type SettingsT = StateT (Maybe Settings) IO

instance FromJSON Settings where
  parseJSON (Object v) =
    Settings <$>
    (v .: "website_path")       <*>
    (v .: "search_depth")       <*>
    (v .: "ignore_folders")     <*>
    (v .: "wordpress_site")     <*>
    (v .: "drupal_site")

-- Parse the Settings file
parseSettings :: IO Settings
parseSettings = do
    settingsContent <- B.readFile settingsFile
    let settings = decode settingsContent :: Maybe Settings
    case settings of
        Just s  -> return s
        Nothing -> error "Couldn't parse settings file"

-- Put the Settings into the SettingsT state monad
getSettings :: SettingsT ()
getSettings = do
    settings <- liftIO parseSettings
    put $ Just settings

-- Get Settings from the SettingsT
-- Calling getSetting if the Settings are missing in SettingsT
getSetting :: (Settings -> a) -> SettingsT a
getSetting f = get >>= maybe (getSettings >> getSetting f) (return . f)
