{-# LANGUAGE OverloadedStrings  #-}

module Sgf.System.Configs.Parser
    ( parseMd5
    , parseLoaded
    , parseComputed
    , parseStatus
    , parseConffile
    , parsePackage
    , byField
    , parse
    , inprocParse
    )
  where

import Prelude hiding (FilePath)
import Data.String
import Data.Char
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Filesystem.Path.CurrentOS
import Control.Applicative
import Control.Monad.Except
import Turtle.Line
import Turtle.Shell
import Turtle.Prelude

import Sgf.System.Configs.Types
import Sgf.System.Configs.Common


-- | Parse 'Md5' hash string.
parseMd5 :: A.Parser Md5
parseMd5            = fmap Md5 $ A.skipSpace *> do
    xs <- T.take 33 <$> A.takeWhile1 (A.inClass "1234567890abcdef")
    if T.length xs == 32
      then return xs
      else fail $ if T.length xs > 32
        then "md5 hash is too long: " ++ T.unpack xs
        else "md5 hash is too short or contains incorrect characters: "
                ++ T.unpack xs

-- | Parse @${Conffiles}@ hash values.
parseLoaded :: A.Parser (Hash Loaded)
parseLoaded         =
    (   Stored   <$> parseMd5 <* A.endOfInput
    <|> Obsolete <$> parseMd5 <* parseString "obsolete"     <* A.endOfInput
    <|> (const Newconffile)  <$> parseString "newconffile"  <* A.endOfInput
    ) <|> fail "Can't parse '${Conffiles}' hash value."

-- | Parse computed hash (e.g. from @md5sum@ utility).
parseComputed :: A.Parser (Hash Computed)
parseComputed       = Computed <$> parseMd5

-- | Parse string into 'FilePath'.
parseFilePath :: A.Parser FilePath
parseFilePath       = fromText <$> parseWord

-- | Parser '${Status}' line. May be preceded by spaces.
parseStatus :: A.Parser (Maybe T.Text)
parseStatus         = Just . T.unwords
                        <$> A.count 3 (A.skipSpace *> parseWord)

-- | Parse @${Conffiles}@'s line. Requires exactly one space at the beginning.
parseConffile :: A.Parser (FilePath, Hash Loaded)
parseConffile       = (,) <$> (A.space *> parseFilePath) <*> parseLoaded
                      <|> fail "Can't parse '${Conffiles}'."

-- | Parse @${Package} ${Status}@ line. @${Status}@ part is optional.
parsePackage :: A.Parser Package
parsePackage        =
    Package <$> (Just <$> parseWord)
            <*> A.option Nothing parseStatus
        <|> fail "Can't parse '${Package} ${Status}'."

-- | Parse non-empty string (may /not/ be preceded by spaces).
parseWord :: A.Parser T.Text
parseWord           = A.takeWhile1 (not . isSpace)

-- | Parse a string (may be) preceeded by spaces.
parseString :: T.Text -> A.Parser T.Text
parseString xs      = A.skipSpace *> A.string xs

-- | Predicate working on a specified (using 'LensA') field of a value and
-- using an 'A.Parser' to match field with (parser must match a field
-- _completely_).
byField ::    (a -> T.Text)     -- ^ Lens to field.
           -> A.Parser T.Text   -- ^ Parser to try.
           -> a                 -- ^ Value to work on.
           -> Bool
byField f p         = either (const False) (const True)
                        . A.parseOnly p . f

parse :: (IsString e, MonadError e m) => A.Parser a -> Line -> m a
parse p             = liftEither . A.parseOnly p . lineToText

-- | Parse using attoparsec 'Parser' in 'MonadError' .
-- | Read process and 'parse' its 'stdout'.
inprocParse :: A.Parser a -> T.Text -> [T.Text] -> Shell Line -> P a
inprocParse p cmd args inp  = ExceptT (inprocWithErr cmd args inp) >>= parse p

