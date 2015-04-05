module Types.Drupal where

import Text.Parsec
import System.FilePath.Posix
import Data.Tree
import DirTree
import Types

pluginsFolder :: [FilePath]
pluginsFolder = ["modules"]
versionFileLocation :: [FilePath]
versionFileLocation = ["includes", "bootstrap.inc"]

-- todo: add error reporting
dpVersion :: Website -> IO Version
dpVersion (Website _ _ _ t) = do
    case cd t versionFileLocation of
        Nothing -> return UnknownVersion
        Just f -> do
            versionFile <- readFile (rootLabel f)
            case parse dpParseVersionFile "??" versionFile of
                Left _ -> return UnknownVersion
                Right v -> return $ Version v

dpParseVersionFile :: Parsec String () String
dpParseVersionFile = do
    manyTill anyChar (try $ string "define('VERSION'")
    manyTill anyChar (char '\'' <|> char '"')
    version <- manyTill anyChar (char '\'' <|> char '"')
    return  version

dpModules :: Website -> IO [Plugin]
dpModules (Website _ _ _ t) = do
    case cd t pluginsFolder of
        Nothing -> return []
        Just (Node _ f) -> mapM (\n@(Node p _) -> do
                                            case getChild n (p </> takeFileName p <.> ".info") of
                                                Nothing -> return $ Plugin "oops" UnknownVersion
                                                Just (Node inf _) -> do
                                                    file <- readFile inf -- .info must be the same as the plugin folder. Maybe write a find .info function
                                                    return $ dpParseModuleInfo file
                                        ) f

-- this only finds the standard drupal modules not the actual included ones
dpParseModuleInfo :: String -> Plugin
dpParseModuleInfo s = do
                        let name = parse dpFindModuleName "??" s
                        case name of
                            Left e -> Plugin (show e) UnknownVersion
                            Right n -> Plugin n UnknownVersion

dpFindModuleName :: Parsec String () String
dpFindModuleName = do
    manyTill anyChar (try $ string "name")
    manyTill anyChar (lookAhead $ oneOf (['a'..'z'] ++ ['A'..'Z']) )
    name <- manyTill anyChar (char '\n')
    return name

{-
    /**
        * The current system version.
    */
    define('VERSION', '7.34');
-}