module Main (main) where

import Control.Monad (unless)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Exit (die)
import Test.Hspec

import qualified AcyclicitySpec
import qualified BindingSharedAbstractionSpec
import qualified BindingSpec
import qualified CanonicalizerSpec
import qualified ConstraintGenSpec
import qualified ElaborationSpec
import qualified FrontendNormalizeSpec
import qualified FrontendDesugarSpec
import qualified FrontendParseSpec
import qualified FrontendPrettySpec
import qualified FrozenParitySpec
import qualified GeneralizeSpec
import qualified GraphOpsSpec
import qualified InertSpec
import qualified NormalizeSpec
import qualified PipelineSpec
import qualified PresolutionFacadeSpec
import qualified Phi.AlignmentSpec
import qualified Phi.WitnessDomainSpec
import qualified PublicSurfaceSpec
import qualified RepoGuardSpec
import qualified Research.C1AuthoritativeSurfaceSpec
import qualified Research.P5ClearBoundarySpec
import qualified Research.UriR2C1PrototypeP1Spec
import qualified PresolutionSpec
import qualified Presolution.UnificationClosureSpec
import qualified ReduceSpec
import qualified SolveSpec
import qualified ScopeSpec
import qualified ThesisFixDirectionSpec
import qualified TypeCheckSpec
import qualified TypeSoundnessSpec
import qualified XMLFParseSpec
import qualified XMLFPrettySpec
import qualified TranslatablePresolutionSpec
import qualified PhiSoundnessSpec
import qualified AlignmentInvariantSpec
import qualified ExpansionMinimalitySpec
import qualified Constraint.SolvedSpec

main :: IO ()
main = do
    presolutionMarker <- newIORef False
    hspec $ do
        ConstraintGenSpec.spec
        NormalizeSpec.spec
        AcyclicitySpec.spec
        runIO (writeIORef presolutionMarker True)
        PresolutionSpec.spec
        Presolution.UnificationClosureSpec.spec
        runIO $ do
            wasPresolutionWired <- readIORef presolutionMarker
            unless wasPresolutionWired $
                die "PresolutionSpec was not wired into the test harness."
        SolveSpec.spec
        ScopeSpec.spec
        PipelineSpec.spec
        PublicSurfaceSpec.spec
        RepoGuardSpec.spec
        Research.C1AuthoritativeSurfaceSpec.spec
        Research.P5ClearBoundarySpec.spec
        Research.UriR2C1PrototypeP1Spec.spec
        PresolutionFacadeSpec.spec
        ThesisFixDirectionSpec.spec
        TypeCheckSpec.spec
        TypeSoundnessSpec.spec
        ReduceSpec.spec
        ElaborationSpec.spec
        GeneralizeSpec.spec
        BindingSpec.spec
        BindingSharedAbstractionSpec.spec
        GraphOpsSpec.spec
        InertSpec.spec
        CanonicalizerSpec.spec
        FrontendParseSpec.spec
        FrontendPrettySpec.spec
        FrontendNormalizeSpec.spec
        FrontendDesugarSpec.spec
        XMLFParseSpec.spec
        XMLFPrettySpec.spec
        FrozenParitySpec.spec
        Phi.WitnessDomainSpec.spec
        Phi.AlignmentSpec.spec
        TranslatablePresolutionSpec.spec
        PhiSoundnessSpec.spec
        AlignmentInvariantSpec.spec
        ExpansionMinimalitySpec.spec
        Constraint.SolvedSpec.spec
