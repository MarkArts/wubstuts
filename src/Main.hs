module Main where

import Data.Tree
import Settings
import Types.Wordpress
import Types.Drupal
import DirTree
import Control.Applicative
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
    websiteExpanded <- mapM (flip expandWebsiteDirTree 4) websites
    websiteWithTypes <- mapM findWebsiteVersion websiteExpanded
    websiteWithPlugins <- mapM findWebsitePlugins websiteWithTypes
    putStrLn $ show websiteWithPlugins

expandWebsiteDirTree :: Website -> Int -> IO Website
expandWebsiteDirTree (Website a b c xs) d = Website a b c <$> expandDirTree xs d

findWebsites :: DirTree -> IO [Website]
findWebsites (Node _ []) = return []
findWebsites t@(Node _ ps) = do
    wT <- findWebsiteType t
    case wT of
        UnknownType -> do
            -- todo: refactor to 1 line
            childs <- mapM findWebsites ps
            return $ concat childs
        _ -> return $ [(Website wT UnknownVersion [] t)]

-- todo: Refactor to not use foldl?
-- todo: should this function except a webiste instead?
findWebsiteType :: DirTree -> IO WebsiteType
findWebsiteType (Node _ []) = return UnknownType
findWebsiteType t =  do
    filters <- evalStateT (getSetting getFilters) Nothing
    return $ foldl (\acc x -> do
            case acc of
                UnknownType ->  if or ( map (treeMatches t) (getConditions x) )  then
                                    getFilterType x
                                else
                                    UnknownType
                _ -> acc
        ) UnknownType filters

-- Maybe do something like this instead of the fold
   {- return $ foldUntil
                (not . (==) UnknownType)
                (\x ->  if or ( map (treeMatches t) (getConditions x)) then
                            getFilterType x
                        else
                            UnknownType
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
findWebsiteVersion w@(Website Wordpress _ ps td) = do
                                            t <- wpVersion w
                                            return $ Website Wordpress t ps td
findWebsiteVersion w@(Website Drupal _ ps td) =  do
                                            t <- dpVersion w
                                            return $ Website Drupal t ps td

findWebsitePlugins :: Website -> IO Website
findWebsitePlugins (Website Wordpress n _ td) = return $ Website Wordpress n [] td
findWebsitePlugins w@(Website Drupal n _ td) =  do
                                            ps <- dpModules w
                                            return $ Website Drupal n ps td