module Main (main) where

import Test.Hspec

import qualified ConstraintGenSpec

main :: IO ()
main = hspec ConstraintGenSpec.spec
