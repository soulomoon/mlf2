module PresolutionSpec (spec) where

import Data.List (isInfixOf)
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
    describe "Migration guards" $ do
        it "PresolutionPlanBuilder closes over PresolutionView, not Solved" $ do
            src <- readFile "src/MLF/Constraint/Presolution/Base.hs"
            src `shouldSatisfy` (isInfixOf ":: PresolutionView")
            src `shouldSatisfy` (not . isInfixOf ":: Solved")

        it "GeneralizeEnv stores canonical maps, not solved handles" $ do
            planSrc <- readFile "src/MLF/Constraint/Presolution/Plan.hs"
            ctxSrc <- readFile "src/MLF/Constraint/Presolution/Plan/Context.hs"
            ctxSrc `shouldSatisfy` (isInfixOf "geCanonicalMap :: IntMap.IntMap NodeId")
            ctxSrc `shouldSatisfy` (not . isInfixOf "geRes :: Solved")
            planSrc `shouldSatisfy` (not . isInfixOf "buildSolvedFromPresolutionView ::")

    Presolution.EnforcementSpec.spec
    Presolution.InstantiateSpec.spec
    Presolution.EdgeTraceSpec.spec
    Presolution.MergeEmissionSpec.spec
    Presolution.EdgePlannerSpec.spec
    Presolution.EdgeInterpreterSpec.spec
    Presolution.WitnessSpec.spec
    Presolution.ExpansionSpec.spec
    Presolution.RaiseSpec.spec
