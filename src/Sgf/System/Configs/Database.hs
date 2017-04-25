{-# LANGUAGE OverloadedStrings      #-}

module Sgf.System.Configs.Database
    ( saveDb
    , loadDb
    )
  where

import           Data.Aeson
import           Prelude                    hiding (FilePath)
import           Data.String
import qualified Data.Map.Strict            as M
import           Control.Monad.Except
import qualified Data.ByteString.Lazy       as B
import qualified Filesystem.Path.CurrentOS  as F
import           Turtle.Prelude

import Sgf.System.Configs.Types
import Sgf.System.Configs.Common
import Sgf.System.Configs.System


-- | Convert db to json and write to file.
saveDb :: Maybe F.FilePath -> ConfMap -> P ()
saveDb mf xm        = flip catchError (const (return ())) $ do
    db <- liftMaybe mf
    liftIO . B.writeFile (F.encodeString db) . encode . M.elems $ xm

-- | Load db from json.
loadDb :: (MonadError e m, IsString e, MonadIO m) =>
          F.FilePath -> Maybe F.FilePath -> m ConfMap
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

