module Main where

import Data.Tree
import Data.Aeson
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
    additionalDepth <- evalStateT (getSetting getAdditionalDepth) Nothing
    rootPath <- buildDirTree websiteFolder depth
    websites <- findWebsites rootPath
    websiteExpanded <- mapM (flip expandWebsiteDirTree additionalDepth) websites
    websiteWithTypes <- mapM findWebsiteVersion websiteExpanded
    websiteWithPlugins <- mapM findWebsitePlugins websiteWithTypes
    putStrLn $ show $ encode websiteWithPlugins

expandWebsiteDirTree :: Website -> Int -> IO Website
expandWebsiteDirTree (Website a b c xs) d = Website a b c <$> expandDirTree xs d

findWebsites :: DirTree -> IO [Website]
findWebsites (Node _ []) = return []
findWebsites t@(Node _ ps) = do
    wT <- findWebsiteType t
    case wT of
        UnknownType -> mapM findWebsites ps >>= return . concat
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
findWebsiteVersion w = appVersion w >>= return . setWebsiteVersion w

findWebsitePlugins :: Website -> IO Website
findWebsitePlugins w = appPlugins w >>= return . setWebsitePlugins w

setWebsiteVersion :: Website -> Version -> Website
setWebsiteVersion w version = w { getVersion = version }

setWebsitePlugins :: Website -> [Plugin] -> Website
setWebsitePlugins w plugins = w { getPlugins = plugins }

-- TODO; (Mats Rietdijk) this should be made into an instance
appVersion :: Website -> IO Version
appVersion w = case getWebsiteType w of
    Wordpress -> wpVersion w
    Drupal    -> dpVersion w
    _         -> return UnknownVersion

-- TODO; (Mats Rietdijk) this should be made into an instance
appPlugins :: Website -> IO [Plugin]
appPlugins w = case getWebsiteType w of
    Wordpress -> wpPlugins w
    Drupal    -> dpModules w
    _         -> return []
