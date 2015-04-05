module Main where

import Data.Tree
import Settings
import Types.Wordpress
import DirTree
import Types
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

findWebsites :: DirTree -> IO [Website]
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
getWebsiteType :: DirTree -> IO WebsiteType
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
