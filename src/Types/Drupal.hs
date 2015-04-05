module Types.Drupal where

import Text.Parsec
import Data.Tree
import DirTree
import Types

pluginsFolder :: [FilePath]
pluginsFolder = ["modules"]
versionFileLocation :: [FilePath]
versionFileLocation = ["includes", "bootstrap.inc"]

-- todo: add error reporting
dpVersion :: Website -> IO WebsiteVersion
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

{-
    /**
        * The current system version.
    */
    define('VERSION', '7.34');
-}