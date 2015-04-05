{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Tree
import Data.Aeson

--todo: move this to DirTree or someplace else
type DirTree = Tree FilePath

type Name = String
type Version = String

data WebsiteVersion = Version String | UnknownVersion
    deriving(Show)

data Plugin = Plugin Name Version
    deriving(Show)

data Website = Website WebsiteType WebsiteVersion [Plugin] (DirTree)
instance Show Website where
    show (Website websiteType version plugins (Node path _ )) = show (websiteType, version, plugins, path)

type Conditions = [[FilePath]]

data WebsiteType = Wordpress | Drupal | UnknownType
    deriving (Show)

-- todo: should this code be under settings?
instance FromJSON WebsiteType where
  parseJSON "wordpress" = return Wordpress
  parseJSON "drupal" = return Drupal
  parseJSON _ = return UnknownType