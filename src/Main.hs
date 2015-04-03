module Main where

import System.Directory
import System.FilePath.Posix
import qualified Settings as Shit -- Refactor the settings to somthing les syntaxtual anoying
import Control.Exception
import Control.Monad.State   (evalStateT) -- For extracting Settings from StateT

type Name = String

--todo: rename to somthing tat accuratly described iether a folder or file item
data Path = Folder FilePath [Path] | File Name
     deriving (Show)

getPathName :: Path -> Name
getPathName (Folder n _) = n
getPathName (File n ) = n

data Website = Website WebsiteType Path
instance Show Website where
    show (Website websiteType p) = show (websiteType, getPathName p)

data WebsiteType = Wordpress | Drupal | Unknown
    deriving (Show)

type WebsitePath = String

main :: IO()
main = do
    targetFolder <- evalStateT (Shit.getSetting Shit.website_path) Nothing
    testPrint targetFolder

testPrint :: String -> IO ()
testPrint websiteFolder = do
    depth <- evalStateT (Shit.getSetting Shit.search_depth) Nothing
    rootPath <- getContentsTill websiteFolder depth
    websites <- findWebsites rootPath
    putStrLn $ show websites
    --websites <- getWebsiteList websiteFolder depth
    --websitesWithType <- findTypes websites
    --putStrLn $ show websitesWithType

findWebsites :: Path -> IO [Website]
findWebsites (File _) = return []
findWebsites p@(Folder _ ps) = do
    wT <- getWebsiteType p
    case wT of
        Unknown -> do
            childs <- mapM findWebsites ps
            return $ concat childs
            --F.foldlM (\a x -> (a ++ (findWebsites x) ) ) [] ps
            --Unknown ->  $ mapM (\x -> findWebsites x) ps
        _ -> return $ [(Website wT p)]


--todo: refactor to something less ridiculous
getWebsiteType :: Path -> IO WebsiteType
getWebsiteType (File _ ) = return Unknown
getWebsiteType p =  do
    wp <- isWordPress p
    if wp then do
        return Wordpress
    else do
        dp <- isDrupal p
        if dp then do
            return Drupal
        else
            return Unknown

isWordPress :: Path -> IO Bool
isWordPress p = do
    wpFitler <- evalStateT (Shit.getSetting Shit.wordpress_site) Nothing
    return $ or ( map (pathMatches p) wpFitler )

isDrupal :: Path -> IO Bool
isDrupal p = do
    dpFilter <- evalStateT (Shit.getSetting Shit.drupal_site) Nothing
    return $ or ( map (pathMatches p) dpFilter )

pathMatches :: Path -> [FilePath] -> Bool
pathMatches (File _) _ = False
pathMatches (Folder _ []) _ = False
pathMatches (Folder _ ps) m = and $ map (\x -> matchStringAgaints x (map (takeFileName . getPathName) ps)) m

getWebsiteList :: FilePath -> Int -> IO [Website]
getWebsiteList path depth = do
                        folders <- getFilteredDirectoryContents path
                        mapM f folders
                        where f p = do
                                   pathThree <- getContentsTill (path </> p) depth
                                   return $ Website Unknown pathThree

filterDirectoryContents :: [FilePath] -> [FilePath] -> [FilePath]
filterDirectoryContents paths filters = let normalFitlered = filter (not . flip matchStringAgaints filters) paths 
                                        in  filter (\x -> not ( '.' == head x)) normalFitlered

getFilteredDirectoryContents :: FilePath -> IO [FilePath]
getFilteredDirectoryContents filePath = do
    dirFilter <- evalStateT (Shit.getSetting Shit.ignore_folders) Nothing
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
        where
            f :: FilePath -> IO Path
            f p = do
                isFolder <- doesDirectoryExist $ p
                if isFolder then do
                    result <- try( do
                                subFolders <- getContentsTill p (pred depth)
                                case getContentFrom subFolders of
                                    Just a -> return $ Folder p a
                                    Nothing -> return $  Folder p []
                             ) :: IO (Either SomeException Path)
                    case result of
                        Left ex -> do
                            putStrLn $ "Exception when trying to read dir: " ++ show ex
                            return $ Folder p []
                        Right val -> return val
                else
                    return $ File p

getContentFrom :: Path -> Maybe [Path]
getContentFrom (Folder _ ps)  = Just ps
getContentFrom  _ = Nothing
