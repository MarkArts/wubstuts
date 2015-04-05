module Main where

import System.Directory
import Data.Tree
import System.FilePath.Posix
import Settings
import Control.Exception
import Control.Monad.State   (evalStateT) -- For extracting Settings from StateT

type Name = String

data Website = Website WebsiteType (Tree String)
instance Show Website where
    show (Website websiteType (Node path _ )) = show (websiteType, path)

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

main :: IO()
main = do
    targetFolder <- evalStateT (getSetting getWebsitePath) Nothing
    testPrint targetFolder

testPrint :: String -> IO ()
testPrint websiteFolder = do
    depth <- evalStateT (getSetting getSearchDepth) Nothing
    rootPath <- buildDirTree websiteFolder depth
    websites <- findWebsites rootPath
    putStrLn $ show websites

findWebsites :: Tree FilePath -> IO [Website]
findWebsites (Node _ []) = return []
findWebsites t@(Node _ ps) = do
    wT <- getWebsiteType t
    case wT of
        Unknown -> do
            -- todo: refactor to 1 line
            childs <- mapM findWebsites ps
            return $ concat childs
        _ -> return $ [(Website wT t)]

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