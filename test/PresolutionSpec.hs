module PresolutionSpec (spec) where

import Test.Hspec

import qualified Presolution.EdgeInterpreterSpec
import qualified Presolution.EdgePlannerSpec
import qualified Presolution.EdgeTraceSpec
import qualified Presolution.EnforcementSpec
import qualified Presolution.ExpansionSpec
import qualified Presolution.InstantiateSpec
import qualified Presolution.MergeEmissionSpec
import qualified Presolution.RaiseSpec
import qualified Presolution.WitnessSpec

spec :: Spec
spec = describe "Phase 4 — Principal Presolution" $ do
    Presolution.EnforcementSpec.spec
    Presolution.InstantiateSpec.spec
    Presolution.EdgeTraceSpec.spec
    Presolution.MergeEmissionSpec.spec
    Presolution.EdgePlannerSpec.spec
    Presolution.EdgeInterpreterSpec.spec
    Presolution.WitnessSpec.spec
    Presolution.ExpansionSpec.spec
    Presolution.RaiseSpec.spec
