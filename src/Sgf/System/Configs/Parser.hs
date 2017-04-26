{-# LANGUAGE OverloadedStrings  #-}

module Sgf.System.Configs.Parser
    ( parseMd5
    , parseLoaded
    , parseComputed
    , parseStatus
    , parseConffile
    , parsePackage
    , byField
    , parseLine
    , inprocParse
    )
  where

import           Prelude                hiding (FilePath)
import           Data.String
import           Data.Char
import           Data.Attoparsec.Text
import qualified Data.Text              as T
import           Filesystem.Path.CurrentOS
import           Control.Applicative
import           Control.Monad.Except
import           Turtle.Line
import           Turtle.Shell
import           Turtle.Prelude

import Sgf.System.Configs.Types
import Sgf.System.Configs.Common


-- | Parse 'Md5' hash string.
parseMd5 :: Parser Md5
parseMd5            = fmap Md5 $ skipSpace *> do
    xs <- T.take 33 <$> takeWhile1 (inClass "1234567890abcdef")
    if T.length xs == 32
      then return xs
      else fail $ if T.length xs > 32
        then "md5 hash is too long: " ++ T.unpack xs
        else "md5 hash is too short or contains incorrect characters: "
                ++ T.unpack xs

-- | Parse @${Conffiles}@ hash values.
parseLoaded :: Parser (Hash Loaded)
parseLoaded         =
    (   Stored   <$> parseMd5 <* endOfInput
    <|> Obsolete <$> parseMd5 <* parseString "obsolete" <* endOfInput
    <|> const Newconffile <$> parseString "newconffile" <* endOfInput
    ) <|> fail "Can't parse '${Conffiles}' hash value."

-- | Parse computed hash (e.g. from @md5sum@ utility).
parseComputed :: Parser (Hash Computed)
parseComputed       = Computed <$> parseMd5

-- | Parse string into 'FilePath'.
parseFilePath :: Parser FilePath
parseFilePath       = fromText <$> parseWord

-- | Parser '${Status}' line. May be preceded by spaces.
parseStatus :: Parser (Maybe T.Text)
parseStatus         = Just . T.unwords
                        <$> count 3 (skipSpace *> parseWord)

-- | Parse @${Conffiles}@'s line. Requires exactly one space at the beginning.
parseConffile :: Parser (FilePath, Hash Loaded)
parseConffile       = (,) <$> (space *> parseFilePath) <*> parseLoaded
                      <|> fail "Can't parse '${Conffiles}'."

-- | Parse @${Package} ${Status}@ line. @${Status}@ part is optional.
parsePackage :: Parser Package
parsePackage        =
    Package <$> (Just <$> parseWord)
            <*> option Nothing parseStatus
        <|> fail "Can't parse '${Package} ${Status}'."

-- | Parse non-empty string (may /not/ be preceded by spaces).
parseWord :: Parser T.Text
parseWord           = takeWhile1 (not . isSpace)

-- | Parse a string (may be) preceeded by spaces.
parseString :: T.Text -> Parser T.Text
parseString xs      = skipSpace *> string xs

-- | Predicate working on a specified (using 'LensA') field of a value and
-- using an 'Parser' to match field with (parser must match a field
-- _completely_).
byField ::    (a -> T.Text)     -- ^ Lens to field.
           -> Parser T.Text   -- ^ Parser to try.
           -> a                 -- ^ Value to work on.
           -> Bool
byField f p         = either (const False) (const True)
                        . parseOnly p . f

parseLine :: (IsString e, MonadError e m) => Parser a -> Line -> m a
parseLine p         = liftEither . parseOnly p . lineToText

-- | Parse using attoparsec 'Parser' in 'MonadError' .
-- | Read process and 'parse' its 'stdout'.
inprocParse :: Parser a -> T.Text -> [T.Text] -> Shell Line -> P a
inprocParse p cmd args inp  =
    ExceptT (inprocWithErr cmd args inp) >>= parseLine p

