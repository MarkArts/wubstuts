module Types.Wordpress where

import Text.Parsec
import Text.Parsec.ByteString (parseFromFile)
import Data.Tree
import Data.Maybe
import System.FilePath.Posix
import System.Directory
import Data.ByteString (ByteString)
import DirTree
import Types

pluginsFolder :: [FilePath]
pluginsFolder = ["wp-content", "plugins"]

versionFileLocation :: [FilePath]
versionFileLocation = ["wp-includes", "version.php"]

wpVersion :: Website -> IO Version
wpVersion (Website Wordpress _ _ t) = do
    case traverse t versionFileLocation of
        Nothing -> error "Couldn't find the versions file for"
        Just f -> do
            result <- parseFromFile wpParseVersionFile (rootLabel f)
            case result of
                Left e -> error $ show e
                Right v -> return $ Version v
wpVersion _ = error "Can't lookup Wordpress version for a non Wordpress website"

wpParseVersionFile :: Parsec ByteString () String
wpParseVersionFile = do
    manyTill anyChar . try $ versionVar
    spaces >> char '=' >> spaces
    anyQuote
    manyTillAnyQuote >>= return
  where
    versionVar = endOfLine >> spaces >> string "$wp_version"
    anyQuote = char '\'' <|> char '"'
    manyTillAnyQuote = manyTill anyChar anyQuote

wpPlugins :: Website -> IO [Plugin]
wpPlugins w@(Website Wordpress _ _ t) =
    case traverse t pluginsFolder of
        Nothing -> error $ "Couldn't find the modules folder for: " ++ (show w)
        Just d -> do
            let folders = [p | p@(Node _ (_:_)) <- subForest d]
            mapM wpParsePluginFolder folders
wpPlugins _ = error "Can't lookup Wordpress plugins for a non Wordpress website"

-- we have to check all .php files because the plugin name can differ from the folder
wpParsePluginFolder :: Tree FilePath -> IO Plugin
wpParsePluginFolder t = do
    possibles <- mapM wpFindPluginInfo (phpFiles t)
    case catMaybes possibles of
        [] -> error $ "couldn't find a wordpress plugin file in: " ++ (rootLabel t)
        (x:_) -> return x
    where phpFiles d = [ rootLabel p | p <- getChilds d ((==) ".php" . takeExtension . rootLabel) ]

wpFindPluginInfo :: FilePath -> IO (Maybe Plugin)
wpFindPluginInfo p = do
                        fileExist <- doesFileExist p
                        case fileExist of
                            False -> error $ "Trying to parse non-existent file" ++ p
                            True -> do
                                plugin <- parseFromFile wpParsePluginInfo p
                                case plugin of
                                    Left e -> error $ show e
                                    Right x -> return x

-- we use lookAhead because the version and name may not be in that order
wpParsePluginInfo :: Parsec ByteString () (Maybe Plugin)
wpParsePluginInfo = do
    name <- optionMaybe $ try $ lookAhead wpParsePluginName
    case name of
        Nothing -> return Nothing
        Just n -> do
            version <- optionMaybe $ try $ lookAhead wpParsePluginVersion
            case version of
                Just v -> return $ Just $ Plugin n (Version v)
                Nothing -> return $ Just $ Plugin n UnknownVersion

wpParsePluginName :: Parsec ByteString () String
wpParsePluginName = wpInfoVariable "Plugin Name"

wpParsePluginVersion :: Parsec ByteString () String
wpParsePluginVersion = wpInfoVariable "Version"

wpInfoVariable :: String -> Parsec ByteString () String
wpInfoVariable v = do
    manyTill anyChar (try $ string (v ++ ":"))
    spaces
    var <- manyTill anyChar endOfLine
    return var