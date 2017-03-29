{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text as T
import Turtle
import qualified Data.Attoparsec.Text as A
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Either
import Control.Monad
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Turtle.Bytes as B

-- | Parse non-empty string may be preceeded by spaces.
parseWord :: A.Parser Text
parseWord           = A.skipSpace *> A.peekChar' *> A.takeWhile (not . isSpace)

-- | Either..
fromEither :: Show a => Either a b -> b
fromEither          = either (error . show) id

-- | Convert between different string-like error types.
liftEither :: (Show c, IsString e, MonadError e m) => Either c a -> m a
liftEither          = either (throwError . fromString . show) return

-- | Return 'mempty' instead of error.
ignoreError :: (MonadError e m, Monoid a) => m a -> m a
ignoreError         = flip catchError (return . const mempty)

-- | Print utf8 error message correctly.
stderrUtf8 :: MonadIO m => Line -> m ()
stderrUtf8          = B.stderr . return . encodeUtf8 . linesToText . (: [])

-- | Monad for running everything.
type P a            = ExceptT Line Shell a

runP :: (MonadIO m, Monoid a) => P a -> m a
runP mx             = flip fold mempty $ do
    x <- runExceptT mx
    either (\e -> stderrUtf8 e >> die "Terminating") return x

-- | Parse using attoparsec 'Parser' in 'MonadError' .
parse :: (IsString e, MonadError e m) => A.Parser a -> Line -> m a
parse p             = liftEither . A.parseOnly p . lineToText

-- | Read process and 'parse' its 'stdout'.
inprocParse :: A.Parser a -> Text -> [Text] -> Shell Line -> P a
inprocParse p cmd args inp  = ExceptT (inprocWithErr cmd args inp) >>= parse p

-- | Config filename and md5 hash.
data Conf       = Conf {file :: Text, hash :: Text}
  deriving (Show)

-- | Parse @dpkg-query@ output line to 'Conf'.
parseConf :: A.Parser Conf
parseConf           = Conf <$> parseWord <*> parseWord
--parseConf           = Conf <$> parseWord <*> parseWord <|> A.endOfInput *> pure (Conf "" "")

-- FIXME: print errors to stderr from md5sum instead of ignoring them.
main_ :: ExceptT Line Shell ()
main_               = do
    l <- ExceptT $
      inprocWithErr "dpkg-query" ["-W", "-f=${Conffiles}\\n"] empty
    when (l /= "") $ do
      Conf {file = f, hash = h} <- parse parseConf l
      h' <- ignoreError $ inprocParse parseWord "md5sum" [f] empty
      --h' <- inprocParse parseWord "md5sum" [f] empty
      when (h' /= "" && h /= h') (liftIO $ print f)
      --when (h /= h') (liftIO $ print f)

