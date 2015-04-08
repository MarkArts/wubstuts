module Types.Wordpress where

import Text.Parsec
import Text.Parsec.ByteString (parseFromFile)
import Data.Tree
import DirTree
import Types

pluginsFolder :: [FilePath]
pluginsFolder = ["wp-content", "plugins"]

versionFileLocation :: [FilePath]
versionFileLocation = ["wp-includes", "version.php"]

-- todo: add error reporting
wpVersion :: Website -> IO Version
wpVersion (Website _ _ _ t) = do
    case cd t versionFileLocation of
        Nothing -> return UnknownVersion
        Just f -> do
            result <- parseFromFile wpParseVersionFile (rootLabel f)
            case result of
                Left _ -> return UnknownVersion
                Right v -> return $ Version v

wpParseVersionFile = do
    manyTill anyChar (try $ string "$wp_version") -- Find $wp_version
    space >> char '=' >> space -- Match the equals sign after $wp_version
    version <- between (char '\'' ) (char '\'') (many anyChar) -- Parse the version between two apostrophes
    return version



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
