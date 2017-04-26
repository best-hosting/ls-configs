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
                ++ "-linux/Cabal-1.24.2.0/build/ls-configs-exe/ls-configs-exe"
            , "test/ls-configs.test"
            ]
            mempty
    exit r
