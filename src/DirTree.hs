module DirTree where

import Data.Tree
import System.FilePath.Posix
import System.Directory
import Control.Monad.State
import Settings
import Control.Exception

cd :: DirTree -> [FilePath] -> Maybe (DirTree)
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

buildDirTree :: FilePath -> Int -> IO (DirTree)
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

-- todo: rewrite to matches :: [FilePath] -> [FilePath] -> Bool?
treeMatches :: DirTree -> [FilePath] -> Bool
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