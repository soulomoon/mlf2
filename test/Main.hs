module Main (main) where

import Test.Hspec

import qualified ConstraintGenSpec
import qualified NormalizeSpec
import qualified AcyclicitySpec
import qualified PresolutionSpec
import qualified SolveSpec
import qualified PipelineSpec

main :: IO ()
main = hspec $ do
    ConstraintGenSpec.spec
    NormalizeSpec.spec
    AcyclicitySpec.spec
    PresolutionSpec.spec
    SolveSpec.spec
    PipelineSpec.spec
