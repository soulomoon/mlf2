module Main (main) where

import Test.Hspec

import qualified ConstraintGenSpec
import qualified NormalizeSpec

main :: IO ()
main = hspec $ do
    ConstraintGenSpec.spec
    NormalizeSpec.spec
