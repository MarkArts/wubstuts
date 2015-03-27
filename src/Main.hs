module Main where

import System.Directory
import Data.List
import Data.Maybe

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

type Name = String
data PathTree = Folder Name [PathTree] | File String
     deriving (Show)

getContentsTill :: FilePath -> Int -> Int -> IO PathTree
getContentsTill path depth maxDepth 
    | depth >= maxDepth = return (Folder path [] )
    | otherwise = do
        paths <- getFilteredDirectoryContents path
        subFolders <- mapM f ( map ((++) $ path ++ "/" ) paths )
        return $ Folder path subFolders
            where f p = do
                        isFolder <- doesDirectoryExist $ p
                        if isFolder then do
                            subFolders <- getContentsTill p (depth+1) maxDepth
                            return $ Folder p $ fromMaybe [] $ getContentFrom subFolders 
                        else
                            return $ File p

getContentFrom :: PathTree -> Maybe [PathTree]
getContentFrom (Folder _ ps)  = Just ps
getContentFrom  _ = Nothing