module Main where

import System.Directory
import System.FilePath.Posix
import Data.List
import Data.Maybe
import Data.List.Split

import Settings

type Name = String

--todo: rename to somthing tat accuratly described iether a folder or file item
data Path = Folder FilePath [Path] | File Name
     deriving (Show)

getPathName :: Path -> Name
getPathName (Folder n _) = n
getPathName (File n ) = n

data Website = Website Name WebsiteType Path
instance Show Website where
    show (Website name websiteType _) = show (name, websiteType)

data WebsiteType = Wordpress | Drupal | Unknown
    deriving (Show)

type WebsitePath = String

main :: IO()
main = do
    targetFolder <- getSetting website_path
    testPrint targetFolder

testPrint :: String -> IO ()
testPrint websiteFolder = do
   -- depth <- getSetting search_depth 
   -- rootPath <- getContentsTill websiteFolder 
    putStrLn "ENd of thingy"
    --websites <- getWebsiteList websiteFolder depth
    --websitesWithType <- findTypes websites
    --putStrLn $ show websitesWithType

findWebsites :: Path -> IO (Maybe [Website])
findWebsites (File _) = return Nothing
findWebsites (Folder p ps) = return Nothing


findTypes :: [Website] -> IO [Website]
findTypes ws = do
    mapM( 
        \w@(Website name _ pathThree) -> do 
            t <- getWebsiteType w 
            return $ Website name t pathThree 
        ) ws

--todo: replace this with a more dynamic system
getWebsiteType :: Website -> IO WebsiteType
getWebsiteType (Website _ _  pathThree) = do
    wp <- isWordPress pathThree
    if wp then do
        return Wordpress
    else do
        dp <- isDrupal pathThree
        if dp then do
            return Drupal
        else
            return Unknown

isWordPress :: Path -> IO Bool
isWordPress p = do 
    filter <- getSetting wordpress_site
    return $ or ( map (pathMatches p) filter )

isDrupal :: Path -> IO Bool
isDrupal p = do 
    filter <- getSetting drupal_site
    return $ or ( map (pathMatches p) filter )

-- todo: rewrite so that a pathItem has a path and name property so we don't need to use getFileName
pathMatches :: Path -> [FilePath] -> Bool
pathMatches f@(File _) m = matchStringAgaints (takeFileName $ getPathName f) m
pathMatches f@(Folder _ []) m = matchStringAgaints (takeFileName $ getPathName f) m
pathMatches f@(Folder _ ps) m = or $ (matchStringAgaints (takeFileName $ getPathName f) m) : (map (flip pathMatches m) ps)

getWebsiteList :: FilePath -> Int -> IO [Website]
getWebsiteList path depth = do
                        folders <- getFilteredDirectoryContents path
                        mapM f folders
                        where f p = do
                                   pathThree <- getContentsTill (path </> p) depth
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
    subFolders <- mapM f ( map (path </>) paths )
    return $ Folder path subFolders
        where f p = do
                    isFolder <- doesDirectoryExist $ p
                    if isFolder then do
                        permissions <- getPermissions p
                        if readable permissions then do
                            subFolders <- getContentsTill p (pred depth)
                            return $ case getContentFrom subFolders of
                                Just a -> Folder p a
                                Nothing -> Folder p []
                        else
                            return $ Folder p []
                    else
                        return $ File p

getContentFrom :: Path -> Maybe [Path]
getContentFrom (Folder _ ps)  = Just ps
getContentFrom  _ = Nothing
