module Main where

import System.Directory
import Data.List

testPath :: String
testPath = "/home/mark/projects"

fileFilter :: [String]
fileFilter = ["."];


main :: IO()
main = do
	putStrLn "This is the wubstuts program"

data WebsiteType = Wordpress | Drupal | Unknown
type WebsitePath = String

getWebsiteList :: FilePath -> IO [WebsitePath]
getWebsiteList = getFilteredDirectoryContents

filterDirectoryContents :: [FilePath] -> [FilePath]
filterDirectoryContents paths = filter (not . flip matchStringAgaints fileFilter) paths

getFilteredDirectoryContents :: FilePath -> IO [FilePath]
getFilteredDirectoryContents path = do
    unfiltered <- getDirectoryContents path
    return (filterDirectoryContents unfiltered)


matchStringAgaints :: String -> [String] -> Bool
matchStringAgaints strings = or . map (flip isInfixOf strings )

getWebsiteType :: WebsitePath -> IO WebsiteType
getWebsiteType _ = return Wordpress