{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Data.Aeson
import Data.Tree
import System.FilePath.Posix
import Control.Applicative
import Control.Monad.State
import qualified Data.ByteString.Lazy as B

settingsFile :: FilePath
settingsFile = "config/settings.json"

type Name = String
type Version = String

data Website = Website WebsiteType Version (Tree FilePath)
instance Show Website where
    show (Website websiteType version (Node path _ )) = show (websiteType, version, path)

type Conditions = [[FilePath]]

data WebsiteType = Wordpress | Drupal | Unknown
    deriving (Show)

instance FromJSON WebsiteType where
  parseJSON "wordpress" = return Wordpress
  parseJSON "drupal" = return Drupal
  parseJSON _ = return Unknown

--todo: rename to somthing better
data WebsiteFilter = WebsiteFilter {
    getFilterType :: WebsiteType,
    getConditions :: Conditions
} deriving (Show)

data Settings = Settings {
        getWebsitePath :: FilePath,
        getSearchDepth :: Int,
        getIgnoreFolders :: [FilePath],
        getFilters :: [WebsiteFilter]
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
    (v .: "filters")

instance FromJSON WebsiteFilter where
  parseJSON (Object v) =
    WebsiteFilter <$>
    (v .: "type")             <*>
    (v .: "conditions")

-- Parse the Settings file
parseSettings :: IO Settings
parseSettings = do
    settingsContent <- B.readFile settingsFile
    let settings = eitherDecode settingsContent :: Either String Settings
    case settings of
        Left x -> error $ show x
        Right s -> return s

-- Put the Settings into the SettingsT state monad
getSettings :: SettingsT ()
getSettings = do
    settings <- liftIO parseSettings
    put $ Just settings

-- Get Settings from the SettingsT
-- Calling getSetting if the Settings are missing in SettingsT
getSetting :: (Settings -> a) -> SettingsT a
getSetting f = get >>= maybe (getSettings >> getSetting f) (return . f)


cd :: Tree FilePath -> [FilePath] -> Maybe (Tree FilePath)
cd (Node _ []) _ = Nothing
cd t@(Node a _) [p] = getChild t (a </> p)
cd t@(Node a _) ps = case getChild t ( a </> (head ps) ) of
                        Just child -> cd child (tail ps)
                        Nothing -> Nothing

getChild :: (Eq a) => Tree a -> a -> Maybe (Tree a)
getChild (Node _ []) _ = Nothing
getChild (Node _ xs) p = foldl (\acc child ->
                                    case acc of
                                        Nothing -> if rootLabel child == p then Just child else Nothing
                                        Just _ -> acc) Nothing xs