module Types.Wordpress where

import Text.Parsec
import Text.Parsec.ByteString (parseFromFile)
import Data.Tree
import Data.ByteString (ByteString)
import DirTree
import Types

pluginsFolder :: [FilePath]
pluginsFolder = ["wp-content", "plugins"]

versionFileLocation :: [FilePath]
versionFileLocation = ["wp-includes", "version.php"]

-- todo: add error reporting
wpVersion :: Website -> IO Version
wpVersion (Website Wordpress _ _ t) = do
    case traverse t versionFileLocation of
        Nothing -> return UnknownVersion
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
