{-# LANGUAGE OverloadedStrings #-}

module Settings where

import           Control.Applicative
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Types

settingsFile :: FilePath
settingsFile = "config/settings.json"

--todo: rename to somthing better
data WebsiteFilter = WebsiteFilter {
    getFilterType :: WebsiteType,
    getConditions :: Conditions
    } deriving (Show)

data Settings = Settings {
    getWebsitePath      :: FilePath,
    getSearchDepth      :: Int,
    getAdditionalDepth  :: Int,
    getIgnoreFolders    :: [FilePath],
    getFilters          :: [WebsiteFilter]
    } deriving (Show)

-- A stateful Settings monad to avoid parsing the
-- entire file on each getSetting call
type SettingsT = StateT (Maybe Settings) IO

instance FromJSON Settings where
    parseJSON (Object v) = do
        Settings                        <$>
            (v .: "website_path")       <*>
            (v .: "additional_depth")   <*>
            (v .: "search_depth")       <*>
            (v .: "ignore_folders")     <*>
            (v .: "filters")

instance FromJSON WebsiteFilter where
    parseJSON (Object v) = do
        WebsiteFilter           <$>
            (v .: "type")       <*>
            (v .: "conditions")

-- Parse the Settings file
parseSettings :: IO Settings
parseSettings = do
    settingsContent <- B.readFile settingsFile
    case eitherDecode settingsContent of
        Left x  -> error $ show x
        Right s -> return s

-- Put the Settings into the SettingsT state monad
getSettings :: SettingsT ()
getSettings = liftIO parseSettings >>= put . Just

-- Get Settings from the SettingsT
-- Calling getSetting if the Settings are missing in SettingsT
getSetting :: (Settings -> a) -> SettingsT a
getSetting f = get >>= maybe (getSettings >> getSetting f) (return . f)
