{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Lib where

import Prelude hiding (FilePath)
import qualified Data.Text as T
import Turtle
import qualified Data.Attoparsec.Text as A
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Maybe
import qualified Data.List as L
import Data.Either
import Control.Monad
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Turtle.Bytes as B
import Control.Monad.Catch
import Data.Maybe
import Data.Monoid
import Data.Default

import Sgf.Control.Lens


-- * Types.
-- $types

-- | MD5 hash.
newtype Md5         = Md5 {fromMd5 :: Text}
  deriving (Show, Eq)
-- | Lens from 'Md5' to 'Text'.
md5 :: LensA Md5 Text
md5 f z@(Md5 {fromMd5 = x})
                    = fmap (\x' -> z{fromMd5 = x'}) (f x)

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

-- | Type for computed by us hash values (e.g. using @md5sum@).
data Computed
-- | Type for hash values computed by others (e.g. read from @dpkg-query@).
data Loaded

-- | Hash value.
data Hash a where
    Computed    :: {_computedH  :: Md5} -> Hash Computed
    Stored      :: {_storedH    :: Md5} -> Hash Loaded
    Obsolete    :: {_obsoleteH  :: Md5} -> Hash Loaded
    Newconffile :: Hash Loaded
deriving instance Show (Hash a)
deriving instance Eq (Hash a)

-- | Lens to hash value computed by us.
computedH :: LensA (Hash Computed) Md5
computedH f z@(Computed {_computedH = x})
                    = fmap (\x' -> z{_computedH = x'}) (f x)
-- | Lens to hash value read from @dpkg-query@.
storedH :: LensA (Hash Loaded) Md5
storedH f z@(Stored {_storedH = x})
                    = fmap (\x' -> z{_storedH = x'}) (f x)
storedH _ z         = pure z
-- | Lens to hash value marked as @obsolete@ in @dpkg-query@ output.
obsoleteH :: LensA (Hash Loaded) Md5
obsoleteH f z@(Obsolete {_obsoleteH = x})
                    = fmap (\x' -> z{_obsoleteH = x'}) (f x)
obsoleteH _ z       = pure z
-- | Lens to @newconffile@ string in @dpkg-query@ output.
newconffileH :: LensA (Hash Loaded) ()
newconffileH f Newconffile
                    = pure Newconffile
newconffileH _ z    = pure z

-- | Config info.
data CInfo          = FileInfo
                        { _fileHash     :: Maybe (Hash Computed)
                        , _loadedHashes :: [Hash Loaded]
                        }
  deriving (Show, Eq)
-- | Default 'FileInfo' value.
defFileInfo :: CInfo
defFileInfo         = FileInfo {_fileHash = Nothing, _loadedHashes = []}

instance Monoid CInfo where
    mempty          = defFileInfo
    x `mappend` y   = modifyA loadedHashes (++ viewA loadedHashes y) x

-- | Lens from 'CInfo' to 'Computed' file hash.
fileHash :: LensA CInfo (Maybe (Hash Computed))
fileHash f z@(FileInfo {_fileHash = x})
                    = fmap (\x' -> z{_fileHash = x'}) (f x)
-- | Lens from 'CInfo' to 'Loaded' file hashes.
loadedHashes :: LensA CInfo [Hash Loaded]
loadedHashes f z@(FileInfo {_loadedHashes = x})
                    = fmap (\x' -> z{_loadedHashes = x'}) (f x)

-- | Lens from 'CInfo' to computed 'Md5' hash.
computed :: LensA CInfo Md5
computed           = fileHash . maybeL . computedH
-- | Lens from 'CInfo' to stored 'Md5' hash.
stored :: LensA CInfo Md5
stored             = loadedHashes . listL . storedH
-- | Lens from 'CInfo' to obsolete 'Md5' hash.
obsolete :: LensA CInfo Md5
obsolete           = loadedHashes . listL . obsoleteH
-- | Lens from 'CInfo' to 'Newconffile' hash.
newconffile :: LensA CInfo ()
newconffile        = loadedHashes . listL . newconffileH

{-viewInside :: LensA b c -> LensA a [b] -> a -> [c]
viewInside = undefined-}

-- | All stored 'Md5' hashes in 'CInfo'.
allStored :: CInfo -> [Md5]
allStored x         = fromMaybe [] $ do
    ts <- viewAmaybe loadedHashes x
    return . catMaybes . map (viewAmaybe storedH) $ ts

-- | All obsolete 'Md5' hashes in 'CInfo'.
allObsolete :: CInfo -> [Md5]
allObsolete x       = fromMaybe [] $ do
    ts <- viewAmaybe loadedHashes x
    return . catMaybes . map (viewAmaybe obsoleteH) $ ts

-- | Compute hash for all files in 'ConfMap'.
compute :: forall e m. (IsString e, MonadError e m, Alternative m) =>
           (FilePath -> m (Hash Computed)) -> ConfMap -> m ConfMap
compute hash z0     = M.foldrWithKey go (return z0) z0
  where
    go :: (IsString e, MonadError e m, Alternative m) =>
          FilePath -> CInfo -> m ConfMap -> m ConfMap
    go k x mz
      | otherwise       = do
          z  <- mz
          x' <- upd (hash k) x
          return (M.insert k x' z)
    upd :: (IsString e, MonadError e m, Alternative m) =>
           m (Hash Computed) -> CInfo -> m CInfo
    upd mh          = modifyAA fileHash (\w -> Just <$> (liftMaybe w <|> mh) <|> return Nothing)

-- | Map for storing information about configs.
type ConfMap        = M.Map FilePath CInfo


-- * Filters.
-- $filters
--
-- Filters for 'ConfMap' working on 'CInfo' values.

-- Changed configs.
changed3 :: CInfo -> Bool
changed3 x   = fromMaybe False $ do
    let ys = allStored x <|> allObsolete x
    c <- viewAmaybe computed x
    return (not (null ys) && c `notElem` ys)

-- | Computed hash matches to one of stored hashes.
ok3 :: CInfo -> Bool
ok3 x   = fromMaybe False $ do
    let ys = allStored x
    c <- viewAmaybe computed x
    return (c `elem` ys)

-- | Computed hash matches to obsolete stored hash.
obsoleter :: CInfo -> Bool
obsoleter x    = fromMaybe False $ do
    let ys = allObsolete x
    c <- viewAmaybe computed x
    return (c `elem` ys)

-- | No loaded hashes.
missed3 :: CInfo -> Bool
missed3 x      = let ys = allStored x <|> allObsolete x
                 in  null ys

cantCompute :: CInfo -> Bool
cantCompute     = isNothing . viewAmaybe computed

-- * System.
-- $system

-- | @dpkg-query@ call listing all known config files with their hashes.
storedConfs :: Shell (Either Line Line)
storedConfs         = inprocWithErr "dpkg-query" ["-W", "-f=${Conffiles}\\n"] empty

-- | Parse @dpkg-query@ hash values into 'CInfo'.
parseCInfo :: A.Parser CInfo
parseCInfo          =
    (   addFileInfo Stored   <$> parseMd5 <* A.endOfInput
    <|> addFileInfo Obsolete <$> parseMd5 <* parseString "obsolete"
            <* A.endOfInput
    <|> addFileInfo (const Newconffile)  <$> parseString "newconffile"
            <* A.endOfInput
    ) <|> fail "Can't parse `dpkg` hash value."
  where
    addFileInfo :: (a -> Hash Loaded) -> a -> CInfo
    addFileInfo f x = modifyA loadedHashes ([f x] ++) defFileInfo

parseFilePath :: A.Parser FilePath
parseFilePath       = fromText <$> parseWord

-- | Parse @dpkg-query@ output into ('FilePath', 'CInfo') pair for loading
-- into 'ConfMap'.
parseConf :: A.Parser (FilePath, CInfo)
parseConf           = (,) <$> parseFilePath <*> parseCInfo

-- | Calculate md5 hash of a file.
md5sum :: FilePath -> P (Hash Computed)
md5sum xf           = do
    x <- liftEither (toText xf)
    inprocParse (Computed <$> parseMd5) "md5sum" [x] empty


-- * Main.
-- $main

-- | Monad for running everything.
type P a            = ExceptT Line Shell a

-- | Run 'P' monad.
runP :: (MonadIO m, Monoid a) => P a -> m a
runP mx             = fold (runIO mx) (Fold (flip mappend) mempty id)

-- | Parse using attoparsec 'Parser' in 'MonadError' .
parse :: (IsString e, MonadError e m) => A.Parser a -> Line -> m a
parse p             = liftEither . A.parseOnly p . lineToText

-- | Read process and 'parse' its 'stdout'.
inprocParse :: A.Parser a -> Text -> [Text] -> Shell Line -> P a
inprocParse p cmd args inp  = ExceptT (inprocWithErr cmd args inp) >>= parse p

-- | Load 'ConfMap' from a 'Shell' parsing output using specified parser.
loadConfMap :: MonadIO m =>
               Shell (Either Line Line)     -- ^ Input.
            -> ConfMap                      -- ^ Add parsed values here.
            -> A.Parser (FilePath, CInfo)   -- ^ Parser for input.
            -> m ConfMap
loadConfMap x z p   = foldIO x $
                        FoldM (\z mx -> runIO (liftEither mx >>= go z))
                              (return z)
                              return
  where
    go :: (IsString e, MonadIO m, MonadError e m) =>
          ConfMap -> Line -> m ConfMap
    go zm y
      | y == ""     = return zm
      | otherwise   = parse ((\(k, w) -> M.insertWith mappend k w zm) <$> p) y

main_3 :: P ()
main_3  = do
    xm <- loadConfMap storedConfs M.empty parseConf
    let --obs = any ((== Just Obsolete) . viewAmaybe hashSource)
        --noh = any (not . isJust . viewAmaybe hash2)
        --com = any ((== Just Computed) . viewAmaybe hashSource)
        --scom l
    ym <- compute md5sum xm
    liftIO $ do
      mapM_ print (M.toList ym)
      --print "\nComputed:"
      --mapM_ print (M.keys (M.filter com ym))
      print "Changed:"
      mapM_ print (M.keys (M.filter changed3 ym))
      print "Ok:"
      mapM_ print (M.keys (M.filter ok3 ym))
      print "Obsolete:"
      mapM_ print (M.keys (M.filter obsoleter ym))
      print "Missed4:"
      mapM_ print (M.keys (M.filter missed3 ym))
      print "Cant compute:"
      mapM_ print (M.keys (M.filter cantCompute ym))
      {-print "\nObsolete:"
      mapM_ print (M.keys (M.filter obs xm))
      print "\nNo hash:"
      mapM_ print (M.keys (M.filter noh xm))-}


-- * Utils.
-- $utils

-- | Parse non-empty string (may be) preceeded by spaces.
parseWord :: A.Parser Text
parseWord           = A.skipSpace *> A.takeWhile1 (not . isSpace)

-- | Parse a string (may be) preceeded by spaces.
parseString :: Text -> A.Parser Text
parseString xs      = A.skipSpace *> A.string xs

-- | Convert between different string-like error types.
liftEither :: (Show c, IsString e, MonadError e m) => Either c a -> m a
liftEither          = either (throwError . fromString . show) return

-- | Lift 'Maybe' into 'MonadError'.
liftMaybe :: (IsString e, MonadError e m) => Maybe a -> m a
liftMaybe           = maybe (throwError "Nothing") return

-- | Return 'mempty' instead of error.
ignoreError :: (MonadError Line m, Monoid a, MonadIO m) => m a -> m a
ignoreError         = flip catchError (\e -> stderrUtf8 e >> return mempty)

-- | Print utf8 error message correctly.
stderrUtf8 :: MonadIO m => Line -> m ()
stderrUtf8          = B.stderr . return . encodeUtf8 . linesToText . (: [])

-- | Run 'ExceptT' in 'MonadIO'.
runIO :: MonadIO m => ExceptT Line m a -> m a
runIO mx            = do
    x <- runExceptT mx
    either (\e -> stderrUtf8 e >> die "Terminating") return x

