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
import qualified FrontendParseSpec
import qualified FrontendPrettySpec
import qualified GeneralizeSpec
import qualified GraphOpsSpec
import qualified InertSpec
import qualified NormalizeSpec
import qualified PipelineSpec
import qualified Phi.IdentityBridgeSpec
import qualified PresolutionSpec
import qualified ReduceSpec
import qualified SolveSpec
import qualified ThesisFixDirectionSpec
import qualified TypeCheckSpec
import qualified TypeSoundnessSpec
import qualified XMLFParseSpec
import qualified XMLFPrettySpec
import qualified TranslatablePresolutionSpec
import qualified PhiSoundnessSpec
import qualified ExpansionMinimalitySpec

main :: IO ()
main = do
    presolutionMarker <- newIORef False
    hspec $ do
        ConstraintGenSpec.spec
        NormalizeSpec.spec
        AcyclicitySpec.spec
        runIO (writeIORef presolutionMarker True)
        PresolutionSpec.spec
        runIO $ do
            wasPresolutionWired <- readIORef presolutionMarker
            unless wasPresolutionWired $
                die "PresolutionSpec was not wired into the test harness."
        SolveSpec.spec
        PipelineSpec.spec
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
        XMLFParseSpec.spec
        XMLFPrettySpec.spec
        Phi.IdentityBridgeSpec.spec
        TranslatablePresolutionSpec.spec
        PhiSoundnessSpec.spec
        ExpansionMinimalitySpec.spec
