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
import Sgf.System.Configs.Types
import Sgf.System.Configs.Common
import Sgf.System.Configs.Parser
import Sgf.System.Configs.Options
import Sgf.System.Configs.System



-- * Types.
-- $types

{-viewInside :: LensA b c -> LensA a [b] -> a -> [c]
viewInside = undefined-}

-- * Filters.
-- $filters
--
-- Filters for 'ConfMap' working on 'CInfo' values.

-- * System.
-- $system


-- * Main.
-- $main

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

main_3 :: IO ()
main_3              = runP $ do
    join . liftIO . Opt.execParser
        $ Opt.info (work <$> (Opt.helper <*> opts))
        (  Opt.fullDesc
        <> Opt.header "List changed config files."
        <> Opt.progDesc "List changed or added config files." )

