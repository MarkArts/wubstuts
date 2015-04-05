module Main where

import System.Directory
import Data.Tree
import System.FilePath.Posix
import Settings
import Types.Wordpress
import Control.Exception
import Control.Monad.State   (evalStateT) -- For extracting Settings from StateT

main :: IO()
main = do
    targetFolder <- evalStateT (getSetting getWebsitePath) Nothing
    testPrint targetFolder

testPrint :: String -> IO ()
testPrint websiteFolder = do
    depth <- evalStateT (getSetting getSearchDepth) Nothing
    rootPath <- buildDirTree websiteFolder depth
    websites <- findWebsites rootPath
    websiteWithTypes <- mapM findWebsiteVersion websites
    putStrLn $ show websiteWithTypes

findWebsites :: Tree FilePath -> IO [Website]
findWebsites (Node _ []) = return []
findWebsites t@(Node _ ps) = do
    wT <- getWebsiteType t
    case wT of
        Unknown -> do
            -- todo: refactor to 1 line
            childs <- mapM findWebsites ps
            return $ concat childs
        _ -> return $ [(Website wT "" t)]

-- todo: Refactor to not use foldl?
getWebsiteType :: Tree FilePath -> IO WebsiteType
getWebsiteType (Node _ []) = return Unknown
getWebsiteType t =  do
    filters <- evalStateT (getSetting getFilters) Nothing
    return $ foldl (\acc x -> do
            case acc of
                Unknown ->  if or ( map (treeMatches t) (getConditions x) )  then
                                getFilterType x
                            else
                                Unknown
                _ -> acc
        ) Unknown filters

-- Maybe do something like this instead of the fold
   {- return $ foldUntil
                (not . (==) Unknown)
                (\x ->  if or ( map (treeMatches t) (getConditions x)) then
                            getFilterType x
                        else
                            Unknown
                )
                filters

foldUntil :: (b->Bool) -> (a->b) -> [a] -> b
foldUntil _ f [l] = f l
foldUntil check f val = do
                        let next = f $ head val
                        if check next then
                            next
                        else
                            foldUntil check f (tail val)-}

findWebsiteVersion :: Website -> IO Website
findWebsiteVersion w@(Website Wordpress _ td) = do
                                            t <- wpVersion w
                                            case t of
                                                Left v -> return $ Website Wordpress v td
                                                Right v -> return $ Website Wordpress v td
findWebsiteVersion w@(Website Drupal _ _) = return $ w

-- todo: rewrite to matches :: [FilePath] -> [FilePath] -> Bool?
treeMatches :: Tree FilePath -> [FilePath] -> Bool
treeMatches (Node _ []) _ = False
treeMatches (Node _ ts) m = and $ map (\x -> matchStringAgaints x (map (\(Node p _) -> takeFileName p) ts)) m

filterDirectoryContents :: [FilePath] -> [FilePath] -> [FilePath]
filterDirectoryContents paths filters = let normalFitlered = filter (not . flip matchStringAgaints filters) paths
                                        in  filter (\x -> not ( '.' == head x)) normalFitlered

getFilteredDirectoryContents :: FilePath -> IO [FilePath]
getFilteredDirectoryContents filePath = do
    dirFilter <- evalStateT (getSetting getIgnoreFolders) Nothing
    result <- try( getDirectoryContents filePath ) :: IO (Either SomeException [FilePath])
    case result of
        Left ex -> do
            putStrLn $ "Exception when trying to read dir: " ++ show ex
            return $ []
        Right childs -> return $ filterDirectoryContents childs dirFilter

matchStringAgaints :: String -> [String] -> Bool
matchStringAgaints strings = or . map (== strings )

buildDirTree :: FilePath -> Int -> IO (Tree FilePath)
buildDirTree p 0 = return $ Node p []
buildDirTree p depth = do
    isDir <- doesDirectoryExist p
    if isDir then do
        children <- getFilteredDirectoryContents p
        let subFolders = map (p </>) children
        subTrees <- mapM (\x -> buildDirTree x (pred depth)) subFolders
        return $ Node p subTrees
    else
        return $ Node p []