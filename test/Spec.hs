{-# LANGUAGE OverloadedStrings #-}

import System.Info
import qualified Data.Text as T
import Turtle.Prelude

main :: IO ()
main                = do
    r <- proc "shelltest"
            [ "--diff"
            , "-w"
            , T.pack $ "./.stack-work/dist/"
                ++ arch
                ++ "-linux/Cabal-1.24.2.0/build/list-changed-configs-exe/list-changed-configs-exe"
            , "test/list-changed-configs.test"
            ]
            mempty
    exit r
