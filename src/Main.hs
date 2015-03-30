module Main where

import System.Directory
import Data.List
import Data.Maybe

import Settings

type Name = String
data Path = Folder Name [Path] | File String
     deriving (Show)

data Website = Website Name WebsiteType Path
instance Show Website where
    show (Website name websiteType _) = show (name, websiteType)

data WebsiteType = Wordpress | Drupal | Unknown
    deriving (Show)

type WebsitePath = String

fileFilter :: [String]
fileFilter = [".", ".."]

main :: IO()
main = do
    targetFolder <- getSetting website_path
    testPrint targetFolder

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
isWordPress :: Path -> Bool
isWordPress (File f)
    | isInfixOf "wp-config.php" f = True
    | otherwise = False
isWordPress (Folder _ []) = False
isWordPress (Folder _ ps) = or $ map isWordPress ps

isDrupal :: Path -> Bool
isDrupal _ = False

getWebsiteList :: FilePath -> IO [Website]
getWebsiteList path = do
                        folders <- getFilteredDirectoryContents path
                        mapM f folders
                        where f p = do
                                   depth <- getSetting search_depth
                                   pathThree <- getContentsTill (path ++ "/" ++ p) depth
                                   return $ Website p Unknown pathThree

filterDirectoryContents :: [FilePath] -> [FilePath] -> [FilePath]
filterDirectoryContents paths filters = filter (not . flip matchStringAgaints filters) paths

getFilteredDirectoryContents :: FilePath -> IO [FilePath]
getFilteredDirectoryContents filePath = do
    dirFilter <- getSetting ignore_folders
    unfiltered <- getDirectoryContents filePath
    return $ filterDirectoryContents unfiltered dirFilter

matchStringAgaints :: String -> [String] -> Bool
matchStringAgaints strings = or . map (== strings )

getContentsTill :: FilePath -> Int -> IO Path
getContentsTill path 0 = return (Folder path [] )
getContentsTill path depth = do
    paths <- getFilteredDirectoryContents path
    subFolders <- mapM f ( map ((++) $ path ++ "/" ) paths )
    return $ Folder path subFolders
        where f p = do
                    isFolder <- doesDirectoryExist $ p
                    if isFolder then do
                        permissions <- getPermissions p
                        if readable permissions then do
                            subFolders <- getContentsTill p (pred depth)
                            return $ Folder p $ fromMaybe [] $ getContentFrom subFolders
                        else
                            return $ Folder p []
                    else
                        return $ File p

getContentFrom :: Path -> Maybe [Path]
getContentFrom (Folder _ ps)  = Just ps
getContentFrom  _ = Nothing
