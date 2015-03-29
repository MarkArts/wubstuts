module Main where

import System.Directory
import Data.List
import Data.Maybe

import Settings

type Name = String
data PathTree = Folder Name [PathTree] | File String
     deriving (Show)

data Website = Website Name WebsiteType PathTree
instance Show Website where
    show (Website name websiteType _) = show (name, websiteType)

data WebsiteType = Wordpress | Drupal | Unknown
    deriving (Show)

type WebsitePath = String

fileFilter :: [String]
fileFilter = [".", ".."]

main :: IO()
main = do
    settings <- getSettings
    testPrint (website_path settings)

testPrint :: String -> IO ()
testPrint websiteFolder = do
    websites <- getWebsiteList websiteFolder
    putStrLn $ show (findTypes websites)

findTypes :: [Website] -> [Website]
findTypes webs = map f webs
    where f w@(Website name _ pathThree) = Website name (getWebsiteType w) pathThree

getWebsiteType :: Website -> WebsiteType
getWebsiteType (Website _ _  pathThree)
    | isWordPress pathThree = Wordpress
    | isDrupal pathThree = Drupal
    | otherwise = Unknown

-- TODO: Make this break early
isWordPress :: PathTree -> Bool
isWordPress (File f)
    | isInfixOf "wp-config.php" f = True
    | otherwise = False
isWordPress (Folder _ []) = False
isWordPress (Folder _ ps) = or $ map isWordPress ps

isDrupal :: PathTree -> Bool
isDrupal _ = False

getWebsiteList :: FilePath -> IO [Website]
getWebsiteList path = do
                        folders <- getFilteredDirectoryContents path
                        mapM f folders
                        where f p = do
                                   pathThree <- getContentsTill (path ++ "/" ++ p) 20
                                   return $ Website p Unknown pathThree

filterDirectoryContents :: [FilePath] -> [FilePath]
filterDirectoryContents paths = filter (not . flip matchStringAgaints fileFilter) paths

getFilteredDirectoryContents :: FilePath -> IO [FilePath]
getFilteredDirectoryContents filePath = do
    unfiltered <- getDirectoryContents filePath
    return (filterDirectoryContents unfiltered)

matchStringAgaints :: String -> [String] -> Bool
matchStringAgaints strings = or . map (== strings )

getContentsTill :: FilePath ->  Int -> IO PathTree
getContentsTill path 0 = return (Folder path [] )
getContentsTill path depth = do
    paths <- getFilteredDirectoryContents path
    subFolders <- mapM f ( map ((++) $ path ++ "/" ) paths )
    return $ Folder path subFolders
        where f p = do
                    isFolder <- doesDirectoryExist $ p
                    if isFolder then do
                        subFolders <- getContentsTill p (depth-1)
                        return $ Folder p $ fromMaybe [] $ getContentFrom subFolders
                    else
                        return $ File p

getContentFrom :: PathTree -> Maybe [PathTree]
getContentFrom (Folder _ ps)  = Just ps
getContentFrom  _ = Nothing