{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}

module Sgf.System.Configs.Types
    (

      Md5 (..)
    , md5

    , Computed
    , Loaded
    , Hash (..)
    , computedH
    , storedH
    , obsoleteH
    , newconffileH

    , Package (..)
    , pkgName
    , pkgStatus

    , CInfo
    , filePath
    , fileHash
    , targetFileHash
    , loadedHashes
    , symLinkTargets
    , package

    , isFileInfo
    , isSymLinkInfo

    , computed
    , stored
    , obsolete
    , newconffile

    , allStored
    , allObsolete

    , ConfMap
    , Key
    , getKey

    , P
    )
  where

import Prelude      hiding (FilePath)
import Data.Text    hiding (map)
import Data.Text.Encoding
import Data.Aeson
import Data.Maybe
import qualified Data.Map.Strict            as M
import Filesystem.Path.CurrentOS
import Control.Applicative
import Control.Foldl (Fold(..))
import Control.Monad.Except

import Turtle.Shell
import Turtle.Line
import Turtle.Prelude

import Sgf.Control.Lens


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
            Newconffile -> "tag"        .= pack (show Newconffile)
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
newconffileH _ Newconffile
                    = pure Newconffile
newconffileH _ z    = pure z

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
                        <$> (decodeString <$> v .: "filePath")
                        <*> v .: "fileHash"
                        <*> v .: "targetFileHash"
                        <*> v .: "loadedHashes"
                        <*> (M.map decodeString <$> v .: "symLinkTargets")
                        <*> v .: "package"
instance ToJSON CInfo where
    toJSON x        = object
        [ "filePath"       .= encodeString (viewA filePath x)
        , "fileHash"       .= viewA fileHash x
        , "targetFileHash" .= viewA targetFileHash x
        , "loadedHashes"   .= viewA loadedHashes x
        , "symLinkTargets" .= M.map encodeString (viewA symLinkTargets x)
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

-- | Map for storing information about configs.
type ConfMap        = M.Map Key CInfo

newtype Key         = Key FilePath
  deriving (Eq, Ord)

getKey :: CInfo -> Key
getKey              = Key . viewA filePath

-- | Monad for running everything.
type P a            = ExceptT Line Shell a

