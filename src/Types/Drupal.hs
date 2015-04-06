module Types.Drupal where

import Text.Parsec
import Text.Parsec.ByteString (parseFromFile)
import System.FilePath.Posix
import Data.Tree
import DirTree
import Types

versionFileLocation :: [FilePath]
versionFileLocation = ["includes", "bootstrap.inc"]

dpPluginsFolders :: Website -> [[FilePath]]
dpPluginsFolders (Website _ _ _ t) = [map rootLabel (findChilds t ((==) "modules" . takeFileName . rootLabel ) ) ]

-- todo: add error reporting
dpVersion :: Website -> IO Version
dpVersion (Website _ _ _ t) = do
    case cd t versionFileLocation of
        Nothing -> return UnknownVersion
        Just f -> do
            result <- parseFromFile dpParseVersionFile (rootLabel f)
            case result of
                Left _ -> return UnknownVersion
                Right v -> return $ Version v

dpParseVersionFile = do
    manyTill anyChar (try $ string "define('VERSION'")
    char ','
    version <- between (char '\'') (char '\'') (many1 anyChar)
    string ");"
    return version

-- todo: Map over all dpPluginFolders
dpModules :: Website -> IO [Plugin]
dpModules w@(Website _ _ _ t) = do
    case cd t (head $ dpPluginsFolders w) of
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
