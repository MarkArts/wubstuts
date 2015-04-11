module Types.Drupal where

import Text.Parsec
import Text.Parsec.ByteString (parseFromFile)
import System.FilePath.Posix
import Data.Tree
import Data.ByteString (ByteString)
import System.Directory
import DirTree
import Types

versionFileLocation :: [FilePath]
versionFileLocation = ["includes", "bootstrap.inc"]

dpModuleFolders :: Website -> [DirTree]
dpModuleFolders (Website _ _ _ t) = findTopChilds t ((==) "modules" . takeFileName . rootLabel )

-- todo: add error reporting
dpVersion :: Website -> IO Version
dpVersion (Website Drupal _ _ t) = do
    case traverse t versionFileLocation of
        Nothing -> return UnknownVersion
        Just f -> do
            result <- parseFromFile dpParseVersionFile (rootLabel f)
            case result of
                Left _ -> return UnknownVersion
                Right v -> return $ Version v
dpVersion _ = error "Can't lookup Drupal version for a non Drupal website"

dpParseVersionFile :: Parsec ByteString () String
dpParseVersionFile = do
    manyTill anyChar (try $ string "define('VERSION'")
    char ','
    version <- between (char '\'') (char '\'') (many1 anyChar)
    string ");"
    return version

-- todo: Map over all dpPluginFolders
dpModules :: Website -> IO [Plugin]
dpModules w = mapM dpFindModuleInfo $ map rootLabel $ concat $ map (flip findTopChilds infos) $ dpModuleFolders w
                              where infos (Node n _) = takeExtension n == ".info"

dpFindModulesIn :: DirTree -> [FilePath]
dpFindModulesIn t = [ rootLabel f | f@(Node _ (_:_)) <- subForest t]

dpFindModuleInfo :: FilePath -> IO Plugin
dpFindModuleInfo p = do
                        fileExist <- doesFileExist p
                        case fileExist of
                            False -> return $ Plugin ("No .info file found for " ++ p) UnknownVersion
                            True -> do
                                plugin <- parseFromFile dpParseModuleInfo p
                                case plugin of
                                    Left e -> error $ show e
                                    Right n -> return n

dpParseModuleInfo :: Parsec ByteString () Plugin
dpParseModuleInfo = do
    n <- dpParseModuleName
    v <- dpParseModuleVersion
    return $ Plugin n (Version v)

dpParseModuleName :: Parsec ByteString () String
dpParseModuleName = do
    manyTill anyChar (try $ string "name")
    manyTill anyChar (lookAhead $ oneOf (['a'..'z'] ++ ['A'..'Z']) )
    name <- manyTill anyChar (char '\n')
    return name

dpParseModuleVersion :: Parsec ByteString () String
dpParseModuleVersion = do
    manyTill anyChar (try $ string "version")
    manyTill anyChar (lookAhead $ oneOf (['a'..'z'] ++ ['A'..'Z']) )
    name <- manyTill anyChar (char '\n')
    return name

{-
    /**
        * The current system version.
    */
    define('VERSION', '7.34');
-}
