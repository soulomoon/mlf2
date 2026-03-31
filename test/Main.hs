module Main (main) where

import AcyclicitySpec qualified
import AlignmentInvariantSpec qualified
import BindingSharedAbstractionSpec qualified
import BindingSpec qualified
import CanonicalizerSpec qualified
import Constraint.SolvedSpec qualified
import ConstraintGenSpec qualified
import Control.Monad (unless)
import Data.IORef (newIORef, readIORef, writeIORef)
import ElaborationSpec qualified
import ExpansionMinimalitySpec qualified
import FrontendDesugarSpec qualified
import FrontendNormalizeSpec qualified
import FrontendParseSpec qualified
import FrontendPrettySpec qualified
import FrozenParitySpec qualified
import GeneralizeSpec qualified
import GoldenSpec qualified
import GraphOpsSpec qualified
import InertSpec qualified
import NormalizeSpec qualified
import Phi.AlignmentSpec qualified
import Phi.WitnessDomainSpec qualified
import PhiSoundnessSpec qualified
import PipelineSpec qualified
import Presolution.UnificationClosureSpec qualified
import PresolutionFacadeSpec qualified
import PresolutionSpec qualified
import Property.QuickCheckPropertySpec qualified
import PublicSurfaceSpec qualified
import ReduceSpec qualified
import Reify.CoreSpec qualified
import Reify.NamedSpec qualified
import Reify.TypeOpsSpec qualified
import Reify.TypeSpec qualified
import RepoGuardSpec qualified
import Research.C1AuthoritativeSurfaceSpec qualified
import Research.P5ClearBoundarySpec qualified
import Research.SameLaneRetainedChildRepresentativeGapSpec qualified
import Research.UriR2C1PrototypeP1Spec qualified
import ScopeSpec qualified
import SolveSpec qualified
import System.Exit (die)
import Test.Hspec
import ThesisFixDirectionSpec qualified
import TranslatablePresolutionSpec qualified
import TypeCheckSpec qualified
import TypeSoundnessSpec qualified
import Util.GraphSpec qualified
import Util.UnionFindSpec qualified
import XMLFParseSpec qualified
import XMLFPrettySpec qualified

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
    Research.SameLaneRetainedChildRepresentativeGapSpec.spec
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
    Util.UnionFindSpec.spec
    Util.GraphSpec.spec
    Reify.TypeOpsSpec.spec
    Reify.NamedSpec.spec
    Reify.TypeSpec.spec
    Reify.CoreSpec.spec
    Property.QuickCheckPropertySpec.spec
    GoldenSpec.spec
