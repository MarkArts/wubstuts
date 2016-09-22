module Types.Wordpress where

import Text.Parsec
import Text.Parsec.ByteString (parseFromFile)
import System.FilePath.Posix
import Data.Tree
import Data.ByteString (ByteString)
import qualified DirTree as DT
import Types

pluginsFolder :: [FilePath]
pluginsFolder = ["wp-content", "plugins"]

wpVersionFileLocation :: Website -> Maybe (FilePath)
wpVersionFileLocation (Website _ _ _ t) = DT.findChild t ((==) "version.php" . filename) >>= return . rootLabel
                                          where filename = takeFileName . rootLabel                                           

-- todo: add error reporting
wpVersion :: Website -> IO Version
wpVersion w@(Website Wordpress _ _ _) = do
    case wpVersionFileLocation w of
        Nothing -> return UnknownVersion
        Just f -> do
            result <- parseFromFile wpParseVersionFile f
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