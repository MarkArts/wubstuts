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
dpModuleFolders (Website _ _ _ t) = findChilds t ((==) "modules" . takeFileName . rootLabel )


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
                                    Left e -> return $ Plugin (show e) UnknownVersion
                                    Right n -> return n

dpParseVersionFile :: Parsec ByteString () String
dpParseVersionFile = do
    manyTill anyChar (try $ string "define('VERSION'")
    char ','
    version <- between (char '\'') (char '\'') (many1 anyChar)
    string ");"
    return version

-- we use lookAhead because the version and name may not be in that order
dpParseModuleInfo :: Parsec ByteString () Plugin
dpParseModuleInfo = do
    n <- lookAhead dpParseModuleName
    version <- lookAhead dpParseModuleVersion
    case version of
        Just v -> return $ Plugin n (Version v)
        Nothing -> return $ Plugin n UnknownVersion

dpParseModuleName :: Parsec ByteString () String
dpParseModuleName = dpInfoVariable "name"

dpParseModuleVersion :: Parsec ByteString () (Maybe String)
dpParseModuleVersion = optionMaybe (try $ dpInfoVariable "version")

dpInfoVariable :: String -> Parsec ByteString () String
dpInfoVariable v = do
    manyTill anyChar (try $ varDecleration)
    spaces >> char '=' >> spaces
    var <- between stringLike stringLike contents
    endOfLine
    return var
  where
    varDecleration = string v >> (space <|> char '=')
    stringLike = optional (char '\'' <|> char '"')
    contents = many1 $ noneOf ['"', '\'', '\r', '\n']
