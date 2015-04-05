module Types.Wordpress where

import Text.Parsec
import Data.Tree
import DirTree
import Types

pluginsFolder :: [FilePath]
pluginsFolder = ["wp-content", "plugins"]
versionFileLocation :: [FilePath]
versionFileLocation = ["wp-includes", "version.php"]

-- todo: add error reporting
wpVersion :: Website -> IO WebsiteVersion
wpVersion (Website _ _ t) = do
    case cd t versionFileLocation of
        Nothing -> return UnknownVersion
        Just f -> do
            versionFile <- readFile (rootLabel f)
            case parse wpParseVersionFile "??" versionFile of
                Left _ -> return UnknownVersion
                Right v -> return $ Version v

wpParseVersionFile :: Parsec String () String
wpParseVersionFile = do
    manyTill anyChar (string "$wp_version")
    manyTill anyChar (char '\'' <|> char '"')
    version <- manyTill anyChar (char '\'' <|> char '"')
    return  version

{-wpParseVersion :: Parser String
wpParseVersion = do
    string "$wp_version"-}



{-

    consume until "Plugin Name:"
    Consume until letter
    read until next line

    consume until "Version: "
    consume until char
    read until line break

    Plugin Name: WordPress SEO
    Version: 1.7.4
-}

{-

    consume until "$wp_version"
    maybe consume space
    maybe consume =
    maybe consume space
    consume ' or ""
    read until ' or ""

    $wp_version = '4.1.1';

-}