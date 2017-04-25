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
import qualified Data.ByteString.Lazy as B
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
import qualified System.Posix.Files as F
import qualified Filesystem.Path.CurrentOS as F
import qualified Options.Applicative as Opt
import qualified Control.Foldl as F
import Data.Function
import Control.Monad.Reader
import Data.Functor.Const
import Data.Aeson

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

instance FromJSON Md5 where
    parseJSON       = withObject "Md5" $ \v -> Md5 <$> v .: "md5"
instance ToJSON Md5 where
    toJSON x        = object [ "md5" .= viewA md5 x]

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

instance FromJSON (Hash Loaded) where
    parseJSON       = withObject "Hash Loaded" $ \v ->
            Stored   <$> v .: "Stored"
        <|> Obsolete <$> v .: "Obsolete"
        <|> (v .: "tag" >>= \t -> case (t :: Text) of
              "Newconffile" -> return Newconffile
              _             -> fail "Huh")
instance ToJSON (Hash Loaded) where
    toJSON x        = object
        [ case x of
            Stored   m  -> "Stored"     .= m
            Obsolete m  -> "Obsolete"   .= m
            Newconffile -> "tag"        .= T.pack (show Newconffile)
        ]

instance FromJSON (Hash Computed) where
    parseJSON       = withObject "Hash Computed" $ \v ->
                        Computed <$> v .: "Computed"
instance ToJSON (Hash Computed) where
    toJSON x        = object ["Computed" .= viewA computedH x]

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
                        { _filePath         :: FilePath
                        , _fileHash         :: Maybe (Hash Computed)
                            -- ^ Hash of file (or file pointed by symlink).
                        , _targetFileHash   :: Maybe (Hash Computed)
                        , _loadedHashes     :: [Hash Loaded]
                            -- ^ Loaded hashes for file name.
                        , _symLinkTargets   :: M.Map Int FilePath
                            -- ^ Symlink targets, if file is symlink, numbered
                            -- by depth.
                        , _package          :: Package
                        }
  deriving (Show, Eq)

instance Monoid CInfo where
    mempty          = defFileInfo
    x `mappend` y   =
          modifyA fileHash       (<|> viewA fileHash y)
        . modifyA loadedHashes   (viewA loadedHashes y ++)
        . modifyA symLinkTargets (`mappend` viewA symLinkTargets y)
        . modifyA package        (`mappend` viewA package y)
        $ x

instance FromJSON CInfo where
    parseJSON       = withObject "FileInfo" $ \v -> FileInfo
                        <$> (F.decodeString <$> v .: "filePath")
                        <*> v .: "fileHash"
                        <*> v .: "targetFileHash"
                        <*> v .: "loadedHashes"
                        <*> (M.map F.decodeString <$> v .: "symLinkTargets")
                        <*> v .: "package"
instance ToJSON CInfo where
    toJSON x        = object
        [ "filePath"       .= F.encodeString (viewA filePath x)
        , "fileHash"       .= viewA fileHash x
        , "targetFileHash" .= viewA targetFileHash x
        , "loadedHashes"   .= viewA loadedHashes x
        , "symLinkTargets" .= M.map F.encodeString (viewA symLinkTargets x)
        , "package"        .= viewA package x
        ]

-- | Default 'FileInfo' value.
defFileInfo :: CInfo
defFileInfo         = FileInfo
                        { _filePath         = ""
                        , _fileHash         = Nothing
                        , _targetFileHash   = Nothing
                        , _loadedHashes     = []
                        , _symLinkTargets   = M.empty
                        , _package          = defPackage
                        }

filePath :: LensA CInfo FilePath
filePath f z@(FileInfo {_filePath = x})
                    = fmap (\x' -> z{_filePath = x'}) (f x)
-- | Lens from 'CInfo' to 'Computed' file hash.
fileHash :: LensA CInfo (Maybe (Hash Computed))
fileHash f z@(FileInfo {_fileHash = x})
                    = fmap (\x' -> z{_fileHash = x'}) (f x)
targetFileHash :: LensA CInfo (Maybe (Hash Computed))
targetFileHash f z@(FileInfo {_targetFileHash = x})
                    = fmap (\x' -> z{_targetFileHash = x'}) (f x)
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
              (CInfo -> FilePath)                 -- ^ Function to obtain file path.
           -> LensA CInfo (Maybe (Hash Computed)) -- ^ Lens to field to compute.
           -> (FilePath -> m (Hash Computed))     -- ^ Function for computing.
           -> ConfMap -> m ConfMap
compute f l hash z0 = foldr go (return z0) z0
  where
    go :: (IsString e, MonadError e m, Alternative m) =>
          CInfo -> m ConfMap -> m ConfMap
    go x mz         = do
          z  <- mz
          let k = f x
          x' <- maybeUpdate l (hash k) x <|> return x
          return (M.adjust (const x') (getKey x) z)
          -- !!!

-- | If field is `Nothing` try to evaluate supplied monadic value to get a new
-- value.
maybeUpdate :: (IsString e, MonadError e m, Alternative m) =>
               LensA a (Maybe b) -> m b -> a -> m a
maybeUpdate l mh    = modifyAA l (\w -> Just <$> (liftMaybe w <|> mh))

-- | Map for storing information about configs.
type ConfMap        = M.Map Key CInfo

newtype Key         = Key FilePath
  deriving (Eq, Ord)

getKey :: CInfo -> Key
getKey              = Key . viewA filePath

-- | Program own config.
data Config         = Config
                        { dpkgOutput        :: Shell (Either Line Line)
                        , dbFile            :: Maybe FilePath
                        , etcPath           :: FilePath
                        , targetPath        :: Maybe FilePath
                        , hashesList        :: CInfo -> [Md5]
                        , hashFilter        :: (CInfo -> [Md5]) -> CInfo -> Bool
                        , fileTypeFilter    :: CInfo -> Bool
                        , fileFilter        :: Maybe FilePath
                        , packageFilter     :: Package -> Bool
                        , targetFilter      :: CInfo -> Bool
                        }

data Package        = Package { _pkgName    :: Maybe Text
                              , _pkgStatus  :: Maybe Text
                              }
  deriving (Show, Eq)

defPackage :: Package
defPackage          = Package {_pkgName = Nothing, _pkgStatus = Nothing}

instance Monoid Package where
    mempty          = defPackage
    x `mappend` y   =     modifyA pkgName   (<|> viewA pkgName y)
                        . modifyA pkgStatus (<|> viewA pkgStatus y)
                        $ x
instance FromJSON Package where
    parseJSON       = withObject "Package" $ \v -> Package
                        <$> v .: "pkgName"
                        <*> v .: "pkgStatus"
instance ToJSON Package where
    toJSON x        = object [ "pkgName"    .= viewA pkgName x
                             , "pkgStatus"  .= viewA pkgStatus x
                             ]

pkgName :: LensA Package (Maybe Text)
pkgName f z@(Package {_pkgName = x})
                    = fmap (\x' -> z{_pkgName = x'}) (f x)
pkgStatus :: LensA Package (Maybe Text)
pkgStatus f z@(Package {_pkgStatus = x})
                    = fmap (\x' -> z{_pkgStatus = x'}) (f x)

-- * Filters.
-- $filters
--
-- Filters for 'ConfMap' working on 'CInfo' values.

-- | Options related to target directory.
targetFilterOpts :: Opt.Parser (CInfo -> Bool)
targetFilterOpts    =
        Opt.flag' (cmpBy (==))
            (  Opt.long "target-equal"
            <> Opt.help
                    ("Show source files, which are the same in target."))
    <|> Opt.flag' (fromMaybe False <$> cmpBy (liftA2 (/=)))
            (  Opt.long "target-differ"
            <> Opt.help
                    ("Show source files differing from target ones."))
    <|> Opt.flag' (isNothing . viewA targetFileHash)
            (  Opt.long "target-missed"
            <> Opt.help
                    ("Show source files missed in target."))
    <|> pure (fromMaybe True <$> cmpBy (liftA2 (/=)))
  where
    cmpBy :: (Maybe (Hash Computed) -> Maybe (Hash Computed) -> b)
           -> CInfo -> b
    cmpBy eq        = eq <$> viewA fileHash <*> viewA targetFileHash

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
    <|> Opt.flag' (const (isJust . viewAmaybe computed))
            (   Opt.long "all"
            <>  Opt.help
                    ("Do not filter files by computed hash,"
                    ++ " but still require, that i _can_ compute hash."))
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
    viewM :: Monoid b => LensA a (Maybe b) -> a -> b
    viewM l         = fromMaybe mempty . viewA l
    pkgF :: [A.Parser Text] -> Package -> Bool
    pkgF ps         = byField (viewM pkgName)   (A.choice ps <* A.endOfInput)
    statusF :: [A.Parser Text] -> Package -> Bool
    statusF ps      = byField (viewM pkgStatus) (A.choice ps <* A.endOfInput)

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
parseStatus :: A.Parser (Maybe Text)
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

-- | Make a predicate from a list of excluded files read from a file.
excludeFiles :: Maybe FilePath -> P (CInfo -> Bool)
excludeFiles mf     = flip catchError def $ do
    f <- liftMaybe mf
    b <- testfile f
    if (not b)
      then throwError "Exclude file does not exist."
      else fold (readExcludes f) (Fold mappend mempty (fmap getAll))
  where
    def :: MonadError e m => e -> m (CInfo -> Bool)
    def _           = return (const True)
    -- | Read file with excludes and build predicates from read filenames.
    readExcludes :: FilePath -> Shell (CInfo -> All)
    readExcludes f  = do
        x <- fromText . lineToText <$> input f
        return (All <$> (/= x) . viewA filePath)


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
               FilePath                 -- ^ Path prefix (`/etc` usually).
            -> Shell (Either Line Line) -- ^ Input.
            -> ConfMap                  -- ^ Add parsed values here.
            -> m ConfMap
loadDpkg etc s z    = foldIO s $ FoldM (\z ml -> runIO (liftEither ml >>= go z))
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
        -- | `dpkg` output should always contain configs in `/etc`, so
        -- hardcode it here.
        adj :: (FilePath, Hash Loaded) -> ConfMap -> ConfMap
        adj (k, w') = let x = setA filePath (replacePrefix etc "" k)
                                . setA package pkg
                                . modifyA loadedHashes (w' :)
                                $ mempty
                      in  M.adjust (`mappend` x) (getKey x)
-- !!!

lstreeNoDeref :: FilePath -> Shell FilePath
lstreeNoDeref p     = do
    x <- ls p
    xs <- lstat x
    if not (isSymbolicLink xs) && isDirectory xs
      then return x <|> lstreeNoDeref x
      else return x

-- | Add all files from `/etc` to db.
readEtc :: MonadIO m => FilePath -> Shell FilePath -> ConfMap -> m ConfMap
readEtc etc x z     = foldIO x (FoldM go (return z) return)
  where
    go :: MonadIO m => ConfMap -> FilePath -> m ConfMap
    go z xf         = do
        let v = setA filePath (replacePrefix etc "" xf) mempty
        xt <- lstat xf
        if isDirectory xt
        -- !!!
          then return z
          else do
            v' <- whenDef (isSymbolicLink xt) v $ runIO $ do
                    y <- readSymbolicLink xf
                    return (setA symLinkTargets (M.fromList [(0, y)]) v)
            return $ M.insert (getKey v') v' z

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
    <*> (   Opt.option (Opt.eitherReader readFilePath)
                (  Opt.long "db"
                <> Opt.value Nothing
                <> Opt.metavar "FILE"
                <> Opt.help "Store db to file.")
        )
    <*> (fromMaybe "/etc/"
            <$> Opt.option (Opt.eitherReader readFilePath)
                (  Opt.long "source"
                <> Opt.value Nothing
                <> Opt.metavar "DIR"
                <> Opt.help "Store db to file.")
        )
    <*> (Opt.option (Opt.eitherReader readFilePath)
            (  Opt.long "target"
            <> Opt.value Nothing
            <> Opt.metavar "DIR"
            <> Opt.help ("Directory to compare source files with.")))
    <*> hashesListOpts
    <*> hashFilterOpts
    <*> fileTypeOpts
    <*> (Opt.option (Opt.eitherReader readFilePath)
            (  Opt.long "filter"
            <> Opt.value Nothing
            <> Opt.metavar "FILE"
            <> Opt.help
                (   "Exclude filter file."
                ++  " Pathes must match *exactly* "
                ++  " and must be relative to source path.")))
    <*> packageOpts
    <*> targetFilterOpts
  where
    readFilePath :: String -> Either String (Maybe FilePath)
    readFilePath          = Right . Just . fromText . fromString

-- | Convert db to json and write to file.
saveDb :: Maybe FilePath -> ConfMap -> P ()
saveDb mf xm        = flip catchError (const (return ())) $ do
    db <- liftMaybe mf
    liftIO . B.writeFile (F.encodeString db) . encode . M.elems $ xm

-- | Load db from json.
loadDb :: (MonadError e m, IsString e, MonadIO m) =>
          FilePath -> Maybe FilePath -> m ConfMap
loadDb etc mf       = flip catchError def $ do
    db <- liftMaybe mf
    b  <- testfile db
    if (not b)
      then throwError "Db file does not exist."
      else do
        xs <- liftIO (B.readFile (F.encodeString db)) >>= liftMaybe . decode
        return $ M.fromList . map (\x -> (getKey x, x)) $ xs
  where
    def :: (MonadError e m, IsString e, MonadIO m) => e -> m ConfMap
    def _           = readEtc etc (lstreeNoDeref etc) M.empty

work :: Config -> P ()
work Config { dpkgOutput        = dpkg
            , dbFile            = db
            , etcPath           = etc
            , targetPath        = mtrg
            , hashesList        = hs
            , hashFilter        = eq
            , fileTypeFilter    = ft
            , fileFilter        = mf
            , packageFilter     = pkf
            , targetFilter      = trf
            }       = do
    xm0 <- loadDb etc db
    xm1 <- loadDpkg "/etc/" dpkg xm0 >>=
           compute ((etc </>) . viewA filePath) fileHash md5sum
    saveDb db xm1
    ym0 <- maybe (return xm1)
                 (\trg -> compute ((trg </>) . viewA filePath)
                            targetFileHash
                            md5sum xm1)
                 mtrg
    excl <- excludeFiles mf
    let ym = M.filter (excl <&&> trf <&&> eq hs <&&> ft <&&> pf) ym0
    liftIO $ mapM_ print (M.elems . M.map ((etc </>) . viewA filePath) $ ym)
    -- !!!
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

-- | Parse non-empty string (may /not/ be preceded by spaces).
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
ignoreError         = ignoreErrorDef mempty

ignoreErrorDef :: (MonadError Line m, MonadIO m) => a -> m a -> m a
ignoreErrorDef def  = flip catchError (\e -> stderrUtf8 e >> return def)

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
byField ::    (a -> Text)   -- ^ Lens to field.
           -> A.Parser Text -- ^ Parser to try.
           -> a             -- ^ Value to work on.
           -> Bool
byField f p         = either (const False) (const True)
                        . A.parseOnly p . f

whenDef :: Monad m => Bool -> a -> m a -> m a
whenDef b y mx
  | b               = mx
  | otherwise       = return y

-- | Replace path prefix @old@ (starting and ending at path component
-- boundaries) with @new@, if matched:
--
-- >    replacePrefix old new path
replacePrefix ::   FilePath     -- ^ @Old@ path prefix to replace with.
                -> FilePath     -- ^ @New@ path prefix to substitute to.
                -> FilePath     -- ^ Path.
                -> FilePath     -- ^ Resulting path.
replacePrefix old new x  = maybe x (new </>) (stripPrefix old x)

-- | Lift 'Shell a' into 'P' monad.
liftShell :: Shell a -> P a
liftShell           = ExceptT . fmap Right

