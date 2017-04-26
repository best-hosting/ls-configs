{-# LANGUAGE OverloadedStrings      #-}

module Lib where

import           Prelude                hiding (FilePath)
import           Data.Monoid
import qualified Data.Map.Strict        as M
import           Control.Monad.Except
import           Filesystem.Path.CurrentOS
import           Options.Applicative

import Sgf.Control.Lens
import Sgf.System.Configs.Types
import Sgf.System.Configs.Common
import Sgf.System.Configs.Options
import Sgf.System.Configs.System
import Sgf.System.Configs.Database



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
  where
    -- | Convert 'Package -> Bool' to 'CInfo -> Bool'.
    pf :: CInfo -> Bool
    pf          = getAny .  getConst . modifyAA package (Const . Any . pkf)

main_3 :: IO ()
main_3              = runP . join . liftIO
    . execParser
        $ info (work <$> (helper <*> opts))
        (  fullDesc
        <> header "List changed config files."
        <> progDesc "List changed or added config files." )

