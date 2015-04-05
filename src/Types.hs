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

data Website = Website WebsiteType WebsiteVersion (DirTree)
instance Show Website where
    show (Website websiteType version (Node path _ )) = show (websiteType, version, path)

type Conditions = [[FilePath]]

data WebsiteType = Wordpress | Drupal | UnknownType
    deriving (Show)

-- todo: should this code be under settings?
instance FromJSON WebsiteType where
  parseJSON "wordpress" = return Wordpress
  parseJSON "drupal" = return Drupal
  parseJSON _ = return UnknownType