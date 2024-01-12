{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals #-}
module Main where

import qualified Target.HomeLab.HL2 as HomeLab.HL2

main :: IO ()
main = do
    HomeLab.HL2.main
