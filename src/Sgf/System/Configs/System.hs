{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}

module Sgf.System.Configs.System
    ( systemConfs
    , systemConfsWithPkg
    , loadDpkg
    , md5sum
    , compute
    , lstreeNoDeref
    , readEtc
    )
  where

import           Prelude                    hiding (FilePath)
import           Data.String
import qualified Data.Map.Strict            as M
import           Turtle.Shell
import           Turtle.Line
import           Control.Applicative
import           Control.Monad.Except
import qualified Data.Text                  as T
import qualified System.Posix.Files         as F
import           Filesystem.Path.CurrentOS  hiding (empty)
import           Turtle.Prelude
import           Control.Foldl

import Sgf.Control.Lens
import Sgf.System.Configs.Types
import Sgf.System.Configs.Common
import Sgf.System.Configs.Parser

-- | Load @dpkg-query@ output into 'ConfMap' for files /already/ present in
-- 'ConfMap'.
loadDpkg :: MonadIO m =>
               FilePath                 -- ^ Path prefix (`/etc` usually).
            -> Shell (Either Line Line) -- ^ Input.
            -> ConfMap                  -- ^ Add parsed values here.
            -> m ConfMap
loadDpkg etc s z0   = foldIO s $ FoldM (\z ml -> runIO (liftEither ml >>= go z))
                                   (return (mempty, z0))
                                   (return . snd)
  where
    go :: (IsString e, MonadIO m, MonadError e m, Alternative m) =>
          (Package, ConfMap) -> Line -> m (Package, ConfMap)
    go (pkg, zm) y
      | y == ""     = return (pkg, zm)
      | otherwise   =
                parseLine ((pkg, ) . flip adj zm  <$> parseConffile) y
            <|> parseLine (                (, zm) <$> parsePackage)  y
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

-- | Add all files from `/etc` to db.
readEtc :: MonadIO m => FilePath -> Shell FilePath -> ConfMap -> m ConfMap
readEtc etc x z0    = foldIO x (FoldM go (return z0) return)
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

-- | @dpkg-query@ call listing all known config files with their hashes.
systemConfs :: Shell (Either Line Line)
systemConfs         = inprocWithErr "dpkg-query" ["-W", "-f=${Conffiles}\\n"] empty

systemConfsWithPkg :: Shell (Either Line Line)
systemConfsWithPkg  = inprocWithErr "dpkg-query"
                        [ "-W"
                        , "-f=${Package} ${Status}\n${Conffiles}\n"
                        ] empty

-- | Replace path prefix @old@ (starting and ending at path component
-- boundaries) with @new@, if matched:
--
-- >    replacePrefix old new path
replacePrefix ::   FilePath     -- ^ @Old@ path prefix to replace with.
                -> FilePath     -- ^ @New@ path prefix to substitute to.
                -> FilePath     -- ^ Path.
                -> FilePath     -- ^ Resulting path.
replacePrefix old new x  = maybe x (new </>) (stripPrefix old x)

