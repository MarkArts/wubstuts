module DirTree where

import Data.Tree
import System.FilePath.Posix
import System.Directory
import Control.Monad.State
import Settings
import Control.Applicative
import Types
import Control.Exception
import Prelude hiding (traverse)

traverse :: DirTree -> [FilePath] -> Maybe (DirTree)
traverse (Node _ []) _ = Nothing
traverse t@(Node a _) [p] = getChild t ((==) (a </> p) . rootLabel)
traverse t@(Node a _) ps = case getChild t ((==) (a </> (head ps) ) . rootLabel) of
                        Just child -> traverse child (tail ps)
                        Nothing -> Nothing

getChild :: (Eq a) => Tree a -> (Tree a -> Bool) -> Maybe (Tree a)
getChild (Node _ []) _ = Nothing
getChild n f = case getChilds n f of
                            [] -> Nothing
                            (x:_) -> Just x

getChilds :: (Eq a) => Tree a -> (Tree a -> Bool) -> [Tree a]
getChilds (Node _ []) _ = []
getChilds (Node _ xs) f = filter f xs

getChildFile :: Tree FilePath -> FilePath -> Maybe (Tree FilePath)
getChildFile (Node _ []) _ = Nothing
getChildFile t@(Node n _) p = getChild t ((==) (n </> p) . rootLabel)

findChilds :: (Eq a) => Tree a -> (Tree a -> Bool) -> [Tree a]
findChilds n@(Node _ xs) f
                | f n = n : childs
                | otherwise = childs
                where childs = concat $ map (flip findChilds f) xs

findTopChilds :: (Eq a) => Tree a -> (Tree a -> Bool) -> [Tree a]
findTopChilds n@(Node _ xs) f = case getChild n f of
                                    Just t -> [t]
                                    Nothing ->  concat $ map (flip findTopChilds f) xs

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

expandDirTree :: DirTree -> Int-> IO DirTree
expandDirTree t 0 = return t
expandDirTree (Node p []) d = buildDirTree p d
expandDirTree (Node x xs) d = Node x <$> mapM (flip expandDirTree d) xs

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