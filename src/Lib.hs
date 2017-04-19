{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

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
import qualified System.Posix.Files as F
import qualified Filesystem.Path.CurrentOS as F
import qualified Options.Applicative as Opt
import qualified Control.Foldl as F
import Data.Function
import Control.Monad.Reader
import Data.Functor.Const

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

-- | Config file info.
data CInfo          = FileInfo
                        { _fileHash         :: Maybe (Hash Computed)
                            -- ^ Hash of file (or file pointed by symlink).
                        , _loadedHashes     :: [Hash Loaded]
                            -- ^ Loaded hashes for file name.
                        , _symLinkTargets   :: M.Map Int FilePath
                            -- ^ Symlink targets, if file is symlink, numbered
                            -- by depth.
                        , _package          :: Package
                        }
  deriving (Show, Eq)
-- | Default 'FileInfo' value.
defFileInfo :: CInfo
defFileInfo         = FileInfo
                        { _fileHash         = Nothing
                        , _loadedHashes     = []
                        , _symLinkTargets   = M.empty
                        , _package          = defPackage
                        }

-- | Lens from 'CInfo' to 'Computed' file hash.
fileHash :: LensA CInfo (Maybe (Hash Computed))
fileHash f z@(FileInfo {_fileHash = x})
                    = fmap (\x' -> z{_fileHash = x'}) (f x)
-- | Lens from 'CInfo' to 'Loaded' file hashes.
loadedHashes :: LensA CInfo [Hash Loaded]
loadedHashes f z@(FileInfo {_loadedHashes = x})
                    = fmap (\x' -> z{_loadedHashes = x'}) (f x)
symLinkTargets :: LensA CInfo (M.Map Int FilePath)
symLinkTargets f z@(FileInfo {_symLinkTargets = x})
                    = fmap (\x' -> z{_symLinkTargets = x'}) (f x)
package :: LensA CInfo Package
package f z@(FileInfo {_package = x})
                    = fmap (\x' -> z{_package = x'}) (f x)

isFileInfo :: CInfo -> Bool
isFileInfo          = M.null . viewA symLinkTargets
isSymLinkInfo :: CInfo -> Bool
isSymLinkInfo       = not . isFileInfo

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
    go k x mz       = do
          z  <- mz
          x' <- maybeUpdate fileHash (hash k) x <|> return x
          return (M.insert k x' z)

-- | If field is `Nothing` try to evaluate supplied monadic value to get a new
-- value.
maybeUpdate :: (IsString e, MonadError e m, Alternative m) =>
               LensA a (Maybe b) -> m b -> a -> m a
maybeUpdate l mh    = modifyAA l (\w -> Just <$> (liftMaybe w <|> mh))

-- | Map for storing information about configs.
type ConfMap        = M.Map FilePath CInfo

-- | Program own config.
data Config         = Config
                        { dpkgOutput        :: Shell (Either Line Line)
                        , etcPath           :: FilePath
                        , hashesList        :: CInfo -> [Md5]
                        , hashFilter        :: (CInfo -> [Md5]) -> CInfo -> Bool
                        , fileTypeFilter    :: CInfo -> Bool
                        , packageFilter     :: Package -> Bool
                        }

data Package        = Package {_pkgName :: Text, _pkgStatus :: Text}
  deriving (Show, Eq)

pkgName :: LensA Package Text
pkgName f z@(Package {_pkgName = x})
                    = fmap (\x' -> z{_pkgName = x'}) (f x)
pkgStatus :: LensA Package Text
pkgStatus f z@(Package {_pkgStatus = x})
                    = fmap (\x' -> z{_pkgStatus = x'}) (f x)
defPackage :: Package
defPackage          = Package {_pkgName = "", _pkgStatus = ""}


-- * Filters.
-- $filters
--
-- Filters for 'ConfMap' working on 'CInfo' values.

-- | Options for generating list of hashes to filter from.
hashesListOpts :: Opt.Parser (CInfo -> [Md5])
hashesListOpts      = fromMaybe ((++) <$> allStored <*> allObsolete)
    . foldr mappend mempty
        <$> (Opt.flag Nothing (Just allStored)
                (   Opt.long "stored"
                <>  Opt.help
                      ("Search among files with not an obsolete hashes"
                    ++ " (may be combined with '--obsolete')."))
        <:> Opt.flag Nothing (Just allObsolete)
                (   Opt.long "obsolete"
                <>  Opt.help
                       ("Search among files with obsolete hashes"
                     ++ " (may be combined with '--stored')."))
        <:> pure [])

-- | Options for filtering list generated by 'hashesListOpts'.
hashFilterOpts :: Opt.Parser ((CInfo -> [Md5]) -> CInfo -> Bool)
hashFilterOpts      =
        Opt.flag' (viewAmaybe computed `neq`)
            (  Opt.long "changed"
            <> Opt.help
                      ("Show changed configs instead"
                    ++ " (not matching to selected hashes). "
                    ++ " By default show matched configs."))
    <|> Opt.flag' (\f -> null . f)
            (   Opt.long "other"
            <>  Opt.help
                    "Show files, which does not have hashes in 'dpkg' db.")
    <|> Opt.flag' (const (isNothing . viewAmaybe computed))
            (   Opt.long "errors-read"
            <>  Opt.help
                    "Show files, which can't be read.")
    <|> Opt.flag' (const (     (== defPackage) . viewA package
                          <&&> not . null . viewA loadedHashes))
            (   Opt.long "errors-orphaned"
            <>  Opt.help
                    "Show files does not belonging to any package.")
    <|> pure (viewAmaybe computed `eq`)
  where
    -- | Generalize an `elem`. Empty list is a failure (default value is
    -- returned). Default value is 'False'.
    eq :: Eq a =>
             (CInfo -> Maybe a) -- ^ Obtain an element.
          -> (CInfo -> [a])     -- ^ Obtain a list to match with.
          -> CInfo -> Bool
    eq mh mhs x    = fromMaybe False . flip runReaderT x $ do
        h  <- ReaderT mh
        hs <- ReaderT (maybeList <$> mhs)
        return (h `elem` hs)
    -- | Generalize a `notElem`. Empty list is a failure (default value is
    -- returned). Default value is 'False'.
    neq :: Eq a =>
             (CInfo -> Maybe a) -- ^ Obtain an element.
          -> (CInfo -> [a])     -- ^ Obtain a list to match with.
          -> CInfo -> Bool
    neq mh mhs x    = fromMaybe False . flip runReaderT x $ do
        h  <- ReaderT mh
        hs <- ReaderT (maybeList <$> mhs)
        return (h `notElem` hs)

-- | Options for filtering by filetype.
fileTypeOpts :: Opt.Parser (CInfo -> Bool)
fileTypeOpts        = maybe (const True) (fmap getAny) . foldr mappend mempty
    <$> Opt.flag Nothing (Just $ Any <$> isFileInfo)
            (  Opt.long "files"
            <> Opt.help
                "Only search among files (may be combined '--symlinks').")
    <:> Opt.flag Nothing (Just $ Any <$> isSymLinkInfo)
            (  Opt.long "symlinks"
            <> Opt.help
                  ("Only search among symbolic links"
                ++ " (may be combined '--files')."))
    <:> pure []

packageOpts :: Opt.Parser (Package -> Bool)
packageOpts         = liftA2 (&&)
    <$> (pkgF    <$> ((some $ Opt.option readParser
            (  Opt.long "package"
            <> Opt.metavar "PACKAGE"
            <> Opt.help ("Filter `dpkg-query` output by package name "
                         ++ " (exact match only).")))
        <|> pure [A.takeText]))   -- This 'pure' is at 'Opt.Parser' level.
    <*> (statusF <$> ((some $ Opt.option readParser
            (  Opt.long "status"
            <> Opt.metavar "STATUS"
            <> Opt.help ("Filter `dpkg-query` output by package install status"
                         ++ " (exact match only).")))
        <|> pure [A.takeText]))    -- This 'pure' is at 'Opt.Parser' level.
  where
    readParser :: Opt.ReadM (A.Parser Text)
    readParser      = Opt.eitherReader (return . A.string . T.pack)
    pkgF :: [A.Parser Text] -> Package -> Bool
    pkgF ps         = byField pkgName   (A.choice ps <* A.endOfInput)
    statusF :: [A.Parser Text] -> Package -> Bool
    statusF ps      = byField pkgStatus (A.choice ps <* A.endOfInput)


-- * System.
-- $system

-- | @dpkg-query@ call listing all known config files with their hashes.
systemConfs :: Shell (Either Line Line)
systemConfs         = inprocWithErr "dpkg-query" ["-W", "-f=${Conffiles}\\n"] empty

systemConfsWithPkg :: Shell (Either Line Line)
systemConfsWithPkg  = inprocWithErr "dpkg-query"
                        [ "-W"
                        , "-f=${Package} ${Status}\n${Conffiles}\\n"
                        ] empty

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
parseStatus :: A.Parser Text
parseStatus         = T.unwords <$> A.count 3 (A.skipSpace *> parseWord)

-- | Parse @${Conffiles}@'s line. Requires exactly one space at the beginning.
parseConffile :: A.Parser (FilePath, Hash Loaded)
parseConffile       = (,) <$> (A.space *> parseFilePath) <*> parseLoaded
                      <|> fail "Can't parse '${Conffiles}'."

-- | Parse @${Package} ${Status}@ line. @${Status}@ part is optional.
parsePackage :: A.Parser Package
parsePackage        = Package <$> (T.cons <$> A.satisfy (not . isSpace)
                                          <*> parseWord)
                              <*> A.option "" parseStatus
                      <|> fail "Can't parse '${Package} ${Status}'."

-- | Calculate md5 hash of a file.
md5sum :: FilePath -> P (Hash Computed)
md5sum xf           = do
    x <- liftEither (toText xf)
    inprocParse parseComputed "md5sum" [x] empty

-- | Read file path referenced by symbolic link.
readSymbolicLink :: (IsString e, MonadError e m, MonadIO m) =>
                    FilePath -> m FilePath
readSymbolicLink xf = do
    x <- liftEither (toText xf)
    y <- liftIO $ F.readSymbolicLink (T.unpack x)
    return (fromText (T.pack y))


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

-- | Load @dpkg-query@ output into 'ConfMap' for files /already/ present in
-- 'ConfMap'.
loadDpkg :: MonadIO m =>
               Shell (Either Line Line)         -- ^ Input.
            -> ConfMap                          -- ^ Add parsed values here.
            -> m ConfMap
loadDpkg s z    = foldIO s $ FoldM (\z ml -> runIO (liftEither ml >>= go z))
                                   (return (defPackage, z))
                                   (return . snd)
  where
    go :: (IsString e, MonadIO m, MonadError e m, Alternative m) =>
          (Package, ConfMap) -> Line -> m (Package, ConfMap)
    go (pkg, zm) y
      | y == ""     = return (pkg, zm)
      | otherwise   =     parse ((pkg, ) . flip adj zm  <$> parseConffile) y
                      <|> parse (                (, zm) <$> parsePackage)  y
      where
        adj :: (FilePath, Hash Loaded) -> ConfMap -> ConfMap
        adj (k, w') = flip M.adjust k
                        $ setA package pkg . modifyA loadedHashes (w' :)

lstreeNoDeref :: FilePath -> Shell FilePath
lstreeNoDeref p     = do
    x <- ls p
    xs <- lstat x
    if not (isSymbolicLink xs) && isDirectory xs
      then return x <|> lstreeNoDeref x
      else return x

-- | Add all files from `/etc` to db.
readEtc :: MonadIO m => Shell FilePath -> ConfMap -> m ConfMap
readEtc x z         = foldIO x (FoldM go (return z) return)
  where
    go :: MonadIO m => ConfMap -> FilePath -> m ConfMap
    go z xf         = do
        xt <- lstat xf
        case xt of
          _
            | isDirectory xt      -> return z
            | isSymbolicLink xt   -> runIO $ do
                y <- readSymbolicLink xf
                let c = setA symLinkTargets (M.fromList [(0, y)]) defFileInfo
                return $ M.insertWith (flip const) xf c z
            | otherwise           -> return $
                M.insertWith (flip const) xf defFileInfo z

opts :: Opt.Parser Config
opts                = Config
    <$> (   Opt.flag' (Right <$> stdin)
                (  Opt.long "stdin"
                <> Opt.help "Read `dpkg-query` output from stdin.")
            <|> fmap Right . input . fromString <$> Opt.strOption
                (  Opt.long "file"
                <> Opt.metavar "FILE"
                <> Opt.help "Read `dpkg-query` output from file.")
            <|> pure systemConfsWithPkg
        )
    <*> pure "/etc"
    <*> hashesListOpts
    <*> hashFilterOpts
    <*> fileTypeOpts
    <*> packageOpts

work :: Config -> P ()
work Config { dpkgOutput        = dpkg
            , etcPath           = etc
            , hashesList        = hs
            , hashFilter        = eq
            , fileTypeFilter    = ft
            , packageFilter     = pkf
            }       = do
    xm <- readEtc (lstreeNoDeref etc) M.empty >>= loadDpkg dpkg
    ym0 <- compute md5sum xm
    let ym = M.filter (eq hs <&&> ft <&&> pf) ym0
    liftIO $ mapM_ print (M.keys ym)
  where
    -- | Convert 'Package -> Bool' to 'CInfo -> Bool'.
    pf :: CInfo -> Bool
    pf          = getAny .  getConst . modifyAA package (Const . Any . pkf)
    {-
     - liftIO $ do
      mapM_ print (M.toList ym)
      --print "\nComputed:"
      --mapM_ print (M.keys (M.filter com ym))
      print "Changed:"
      --mapM_ print (M.keys (M.filter changed3 ym))
      mapM_ print (M.keys (M.filter changed41 ym))
      print "Ok:"
      --mapM_ print (M.keys (M.filter ok3 ym))
      mapM_ print (M.keys (M.filter ok41 ym))
      print "Obsolete:"
      --mapM_ print (M.keys (M.filter obsoleter ym))
      mapM_ print (M.keys (M.filter obsolete41 ym))
      print "Missed4:"
      --mapM_ print (M.keys (M.filter missed3 ym))
      mapM_ print (M.keys (M.filter (\x -> isFileInfo x && missed41 x) ym))
      print "Cant compute:"
      --mapM_ print (M.keys (M.filter cantCompute ym))
      mapM_ print (M.keys (M.filter (\x -> isFileInfo x && cantCompute41 x) ym))-}

      {-let c = print "Only pkgs:"
      seq c c
      mapM_ print (M.keys (M.filter (byField (package . pkgName) (pn <* A.endOfInput)) ym))-}
      {-print "\nObsolete:"
      mapM_ print (M.keys (M.filter obs xm))
      print "\nNo hash:"
      mapM_ print (M.keys (M.filter noh xm))-}

main_3 :: P ()
main_3              =
    join . liftIO . Opt.execParser
        $ Opt.info (work <$> (Opt.helper <*> opts))
        (  Opt.fullDesc
        <> Opt.header "List changed config files."
        <> Opt.progDesc "List changed or added config files." )

-- * Utils.
-- $utils

-- | Parse non-empty string (may be) preceeded by spaces.
parseWord :: A.Parser Text
parseWord           = A.takeWhile1 (not . isSpace)

-- | Parse a string (may be) preceeded by spaces.
parseString :: Text -> A.Parser Text
parseString xs      = A.skipSpace *> A.string xs

-- | Convert between different string-like error types.
liftEither :: (Show c, IsString e, MonadError e m) => Either c a -> m a
liftEither          = either (throwError . fromString . show) return

-- | Lift 'Maybe' into 'MonadError'.
liftMaybe :: (IsString e, MonadError e m) => Maybe a -> m a
liftMaybe           = maybe (throwError "Error: Nothing. ") return

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

-- | Predicate working on a specified (using 'LensA') field of a value and
-- using an 'A.Parser' to match field with (parser must match a field
-- _completely_).
byField ::    LensA a Text  -- ^ Lens to field.
           -> A.Parser Text -- ^ Parser to try.
           -> a             -- ^ Value to work on.
           -> Bool
byField l p         = either (const False) (const True)
                        . A.parseOnly p . viewA l

