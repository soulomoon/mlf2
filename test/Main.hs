module Main (main) where

import Test.Hspec

import qualified ConstraintGenSpec
import qualified NormalizeSpec
import qualified AcyclicitySpec
import qualified Presolution.EdgeTraceSpec
import qualified Presolution.EnforcementSpec
import qualified Presolution.ExpansionSpec
import qualified Presolution.InstantiateSpec
import qualified Presolution.MergeEmissionSpec
import qualified Presolution.RaiseSpec
import qualified Presolution.WitnessSpec
import qualified SolveSpec
import qualified PipelineSpec
import qualified TypeCheckSpec
import qualified ReduceSpec
import qualified ElaborationSpec
import qualified BindingSpec
import qualified GraphOpsSpec
import qualified CanonicalizerSpec
import qualified FrontendParseSpec
import qualified FrontendPrettySpec
import qualified XMLFParseSpec
import qualified XMLFPrettySpec

main :: IO ()
main = hspec $ do
    ConstraintGenSpec.spec
    NormalizeSpec.spec
    AcyclicitySpec.spec
    Presolution.EnforcementSpec.spec
    Presolution.InstantiateSpec.spec
    Presolution.EdgeTraceSpec.spec
    Presolution.MergeEmissionSpec.spec
    Presolution.WitnessSpec.spec
    Presolution.ExpansionSpec.spec
    Presolution.RaiseSpec.spec
    SolveSpec.spec
    PipelineSpec.spec
    TypeCheckSpec.spec
    ReduceSpec.spec
    ElaborationSpec.spec
    BindingSpec.spec
    GraphOpsSpec.spec
    CanonicalizerSpec.spec
    FrontendParseSpec.spec
    FrontendPrettySpec.spec
    XMLFParseSpec.spec
    XMLFPrettySpec.spec
