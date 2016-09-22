module Types.Wordpress where

import Text.Parsec
import Text.Parsec.ByteString (parseFromFile)
import System.FilePath.Posix
import Control.Monad
import Data.Tree
import System.Directory
import Data.ByteString (ByteString)
import qualified DirTree as DT
import Types
import Data.Maybe 

wpPluginsFolders :: Website -> [DirTree]
wpPluginsFolders (Website _ _ _ t) = DT.findChilds t ((==) "plugins" . folderName)
                                              where folderName = takeFileName . rootLabel

wpVersionFileLocation :: Website -> Maybe (FilePath)
wpVersionFileLocation (Website _ _ _ t) = liftM rootLabel $ DT.findChild t ((==) "version.php" . filename)
                                          where filename = takeFileName . rootLabel                                           

-- todo: add error reporting
wpVersion :: Website -> IO Version
wpVersion w@(Website Wordpress _ _ _) = do
    case wpVersionFileLocation w of
        Nothing -> return UnknownVersion
        Just file -> do
            result <- parseFromFile wpParseVersionFile file
            case result of
                Left e -> error $ show e
                Right v -> return $ Version v
wpVersion _ = error "Can't lookup Wordpress version for a non Wordpress website"

wpPlugins :: Website -> IO [Plugin]
wpPlugins w = catMaybes `liftM` mapM wpParsePluginFile (concatMap pluginFilesFromFolder (wpPluginsFolders w))

wpParsePluginFile :: FilePath -> IO (Maybe (Plugin))    
wpParsePluginFile p = do 
                        fileExist <- doesFileExist p
                        case fileExist of
                            False -> return Nothing
                            True -> do 
                                      plugin <- parseFromFile wpPluginFile p
                                      case plugin of
                                          Left e -> return Nothing
                                          Right n -> return $ Just n                                           

pluginFilesFromFolder :: DirTree -> [FilePath]
pluginFilesFromFolder (Node _ xs) = filter ((==) ".php" . takeExtension) $ map rootLabel $ concatMap subForest xs
                                    -- for only plugins that follow the wordpress plugin naming conventions
                                    --map (\(Node p _) -> p </> takeFileName p <.> "php") plugins
                                    --where plugins = filter (not . hasExtension . rootLabel) xs

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

-- we use lookAhead because the version and name may not be in that order
wpPluginFile :: Parsec ByteString () Plugin
wpPluginFile = do
    n <- lookAhead $ wpInfoVariable "Plugin Name:"
    version <- optionMaybe (try $ lookAhead $ wpInfoVariable "Version:")
    case version of
        Just v -> return $ Plugin n (Version v)
        Nothing -> return $ Plugin n UnknownVersion

wpInfoVariable :: String -> Parsec ByteString () String
wpInfoVariable s = do
    manyTill anyChar . try $ string s
    manyTill anyChar space
    manyTill anyChar endOfLine