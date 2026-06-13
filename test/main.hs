module Main (main) where

import qualified ComposeT
import qualified WriterTStrictness

main :: IO ()
main = do
  ComposeT.test
  WriterTStrictness.test
