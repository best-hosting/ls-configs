{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts       #-}

module Sgf.System.Configs.Common
    ( (<:>)
    , (<&&>)
    , maybeList
    , maybeUpdate
    , liftEither
    , runIO
    , runP
    , whenDef
    , liftMaybe
    )
  where

import Prelude hiding (FilePath)
import Data.String
import qualified Data.Map.Strict            as M
import Control.Applicative
import Turtle.Prelude
import Turtle.Shell
import Turtle.Line
import qualified Turtle.Bytes as B
import Filesystem.Path.CurrentOS (FilePath)
import Control.Monad.Except
import Data.Text.Encoding
import Control.Foldl (Fold(..))

import Sgf.Control.Lens
import Sgf.System.Configs.Types


-- | List cons operator lifted to 'Applicative'.
infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>)               = liftA2 (:)

infixr 3 <&&>
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>)              = liftA2 (&&)

-- | Convert an empty list to 'Nothing' and non-empty to 'Just [a]'.
maybeList :: [a] -> Maybe [a]
maybeList xs
  | null xs         = Nothing
  | otherwise       = Just xs

-- | If field is `Nothing` try to evaluate supplied monadic value to get a new
-- value.
maybeUpdate :: (IsString e, MonadError e m, Alternative m) =>
               LensA a (Maybe b) -> m b -> a -> m a
maybeUpdate l mh    = modifyAA l (\w -> Just <$> (liftMaybe w <|> mh))


-- | Print utf8 error message correctly.
stderrUtf8 :: MonadIO m => Line -> m ()
stderrUtf8          = B.stderr . return . encodeUtf8 . linesToText . (: [])



-- * Utils.
-- $utils

-- | Convert between different string-like error types.
liftEither :: (Show c, IsString e, MonadError e m) => Either c a -> m a
liftEither          = either (throwError . fromString . show) return

-- | Lift 'Maybe' into 'MonadError'.
liftMaybe :: (IsString e, MonadError e m) => Maybe a -> m a
liftMaybe           = maybe (throwError "Error: Nothing. ") return

-- | Return 'mempty' instead of error.
ignoreError :: (MonadError Line m, Monoid a, MonadIO m) => m a -> m a
ignoreError         = ignoreErrorDef mempty

ignoreErrorDef :: (MonadError Line m, MonadIO m) => a -> m a -> m a
ignoreErrorDef def  = flip catchError (\e -> stderrUtf8 e >> return def)

whenDef :: Monad m => Bool -> a -> m a -> m a
whenDef b y mx
  | b               = mx
  | otherwise       = return y

-- | Lift 'Shell a' into 'P' monad.
liftShell :: Shell a -> P a
liftShell           = ExceptT . fmap Right

-- | Run 'ExceptT' in 'MonadIO'.
runIO :: MonadIO m => ExceptT Line m a -> m a
runIO mx            = do
    x <- runExceptT mx
    either (\e -> stderrUtf8 e >> die "Terminating") return x

-- | Run 'P' monad.
runP :: (MonadIO m, Monoid a) => P a -> m a
runP mx             = fold (runIO mx) (Fold (flip mappend) mempty id)

