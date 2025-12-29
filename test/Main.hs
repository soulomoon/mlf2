module Main (main) where

import Test.Hspec

import qualified ConstraintGenSpec
import qualified NormalizeSpec
import qualified AcyclicitySpec
import qualified PresolutionSpec
import qualified SolveSpec
import qualified PipelineSpec
import qualified TypeCheckSpec
import qualified ReduceSpec
import qualified ElaborationSpec
import qualified BindingSpec
import qualified GraphOpsSpec

main :: IO ()
main = hspec $ do
    ConstraintGenSpec.spec
    NormalizeSpec.spec
    AcyclicitySpec.spec
    PresolutionSpec.spec
    SolveSpec.spec
    PipelineSpec.spec
    TypeCheckSpec.spec
    ReduceSpec.spec
    ElaborationSpec.spec
    BindingSpec.spec
    GraphOpsSpec.spec
