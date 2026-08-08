module Main (main) where

import qualified ComposeT
import qualified WriterStrictness
import qualified StateStrictness
import Test.Tasty

main :: IO ()
main = do
  ComposeT.test
  defaultMain $ testGroup "Transformers" [
    WriterStrictness.test,
    StateStrictness.test
   ]
