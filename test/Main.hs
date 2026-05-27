{-# LANGUAGE DataKinds #-}
module Main (main) where

import AcyclicitySpec qualified
import AlignmentInvariantSpec qualified
import BackendEmissionPrepareSpec qualified
import BackendIRSpec qualified
import BackendConvertSpec qualified
import BackendLLVMSpec qualified
import BackendStructuralRecursiveDataSpec qualified
import BindingSharedAbstractionSpec qualified
import BindingSpec qualified
import CanonicalizerSpec qualified
import Constraint.SolvedSpec qualified
import ConstraintGenSpec qualified
import Control.Exception (finally)
import Control.Monad (unless)
import Data.IORef (newIORef, readIORef, writeIORef)
import ElaborationSpec qualified
import ExpansionMinimalitySpec qualified
import FrontendDesugarSpec qualified
import FrontendNormalizeSpec qualified
import FrontendParseSpec qualified
import FrontendPrettySpec qualified
import FrontendTypeLevelSpec qualified
import FrozenParitySpec qualified
import GeneralizeSpec qualified
import GoldenSpec qualified
import GraphOpsSpec qualified
import GHC.Clock (getMonotonicTimeNSec)
import InertSpec qualified
import NormalizeSpec qualified
import PrimitiveInventorySpec qualified
import Phi.AlignmentSpec qualified
import Phi.WitnessDomainSpec qualified
import PhiSoundnessSpec qualified
import PipelineSpec qualified
import ProgramConformanceCorpusSpec qualified
import ProgramCompilerSeedSpec qualified
import ProgramPackageBuildGraphSpec qualified
import ProgramCliPackageSpec qualified
import ProgramFixturePackageSpec qualified
import ProgramInterfaceSpec qualified
import ProgramPackageSpec qualified
import ProgramPackageDiscoverySpec qualified
import ProgramParserParitySpec qualified
import ProgramSpec qualified
import Presolution.EdgeInterpreterSpec qualified
import Presolution.EdgePlannerSpec qualified
import Presolution.EdgeTraceSpec qualified
import Presolution.EnforcementSpec qualified
import Presolution.ExpansionSpec qualified
import Presolution.InstantiateSpec qualified
import Presolution.MergeEmissionSpec qualified
import Presolution.RaiseSpec qualified
import Presolution.UnificationClosureSpec qualified
import Presolution.WitnessSpec qualified
import PresolutionSpec qualified
import Property.QuickCheckPropertySpec qualified
import PublicSurfaceSpec qualified
import PhaseSingletonsSpec qualified
import ReduceSpec qualified
import Reify.CoreSpec qualified
import Reify.NamedSpec qualified
import Reify.TypeOpsSpec qualified
import Reify.TypeSpec qualified
import ResolvedSymbolSpec qualified
import RepoGuardSpec qualified
import Research.C1AuthoritativeSurfaceSpec qualified
import Research.P2RepresentativeSupportSpec qualified
import Research.P5ClearBoundarySpec qualified
import Research.SameLaneRetainedChildRepresentativeGapSpec qualified
import ScopeSpec qualified
import SolveSpec qualified
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Test.Hspec
import ThesisFixDirectionSpec qualified
import Thesis.ObligationPropertySpec qualified
import Text.Printf (printf)
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
    timedSpec "PhaseSingletonsSpec" PhaseSingletonsSpec.spec
    timedSpec "ConstraintGenSpec" ConstraintGenSpec.spec
    timedSpec "NormalizeSpec" NormalizeSpec.spec
    timedSpec "AcyclicitySpec" AcyclicitySpec.spec
    runIO (writeIORef presolutionMarker True)
    timedSpec "PresolutionSpec" PresolutionSpec.spec
    timedSpec "Presolution.UnificationClosureSpec" Presolution.UnificationClosureSpec.spec
    timedSpec "Presolution.EdgeInterpreterSpec" Presolution.EdgeInterpreterSpec.spec
    timedSpec "Presolution.EdgePlannerSpec" Presolution.EdgePlannerSpec.spec
    timedSpec "Presolution.EdgeTraceSpec" Presolution.EdgeTraceSpec.spec
    timedSpec "Presolution.EnforcementSpec" Presolution.EnforcementSpec.spec
    timedSpec "Presolution.ExpansionSpec" Presolution.ExpansionSpec.spec
    timedSpec "Presolution.InstantiateSpec" Presolution.InstantiateSpec.spec
    timedSpec "Presolution.MergeEmissionSpec" Presolution.MergeEmissionSpec.spec
    timedSpec "Presolution.RaiseSpec" Presolution.RaiseSpec.spec
    timedSpec "Presolution.WitnessSpec" Presolution.WitnessSpec.spec
    runIO $ do
      wasPresolutionWired <- readIORef presolutionMarker
      unless wasPresolutionWired $
        die "PresolutionSpec was not wired into the test harness."
    timedSpec "SolveSpec" SolveSpec.spec
    timedSpec "ScopeSpec" ScopeSpec.spec
    timedSpec "PrimitiveInventorySpec" PrimitiveInventorySpec.spec
    timedSpec "BackendEmissionPrepareSpec" BackendEmissionPrepareSpec.spec
    timedSpec "BackendConvertSpec" BackendConvertSpec.spec
    timedSpec "BackendIRSpec" BackendIRSpec.spec
    timedSpec "BackendStructuralRecursiveDataSpec" BackendStructuralRecursiveDataSpec.spec
    timedSpec "BackendLLVMSpec" BackendLLVMSpec.spec
    timedSpec "ProgramInterfaceSpec" ProgramInterfaceSpec.spec
    timedSpec "ProgramPackageBuildGraphSpec" ProgramPackageBuildGraphSpec.spec
    timedSpec "ProgramConformanceCorpusSpec" ProgramConformanceCorpusSpec.spec
    timedSpec "ProgramCliPackageSpec" ProgramCliPackageSpec.spec
    timedSpec "ProgramCompilerSeedSpec" ProgramCompilerSeedSpec.spec
    timedSpec "ProgramFixturePackageSpec" ProgramFixturePackageSpec.spec
    timedSpec "ProgramPackageSpec" ProgramPackageSpec.spec
    timedSpec "ProgramPackageDiscoverySpec" ProgramPackageDiscoverySpec.spec
    timedSpec "ProgramParserParitySpec" ProgramParserParitySpec.spec
    timedSpec "ProgramSpec" ProgramSpec.spec
    timedSpec "ResolvedSymbolSpec" ResolvedSymbolSpec.spec
    timedSpec "PipelineSpec" PipelineSpec.spec
    timedSpec "PublicSurfaceSpec" PublicSurfaceSpec.spec
    timedSpec "RepoGuardSpec" RepoGuardSpec.spec
    timedSpec "Research.C1AuthoritativeSurfaceSpec" Research.C1AuthoritativeSurfaceSpec.spec
    timedSpec "Research.P2RepresentativeSupportSpec" Research.P2RepresentativeSupportSpec.spec
    timedSpec "Research.P5ClearBoundarySpec" Research.P5ClearBoundarySpec.spec
    timedSpec "Research.SameLaneRetainedChildRepresentativeGapSpec" Research.SameLaneRetainedChildRepresentativeGapSpec.spec
    timedSpec "ThesisFixDirectionSpec" ThesisFixDirectionSpec.spec
    timedSpec "TypeCheckSpec" TypeCheckSpec.spec
    timedSpec "TypeSoundnessSpec" TypeSoundnessSpec.spec
    timedSpec "ReduceSpec" ReduceSpec.spec
    timedSpec "ElaborationSpec" ElaborationSpec.spec
    timedSpec "GeneralizeSpec" GeneralizeSpec.spec
    timedSpec "BindingSpec" BindingSpec.spec
    timedSpec "BindingSharedAbstractionSpec" BindingSharedAbstractionSpec.spec
    timedSpec "GraphOpsSpec" GraphOpsSpec.spec
    timedSpec "InertSpec" InertSpec.spec
    timedSpec "CanonicalizerSpec" CanonicalizerSpec.spec
    timedSpec "FrontendParseSpec" FrontendParseSpec.spec
    timedSpec "FrontendPrettySpec" FrontendPrettySpec.spec
    timedSpec "FrontendNormalizeSpec" FrontendNormalizeSpec.spec
    timedSpec "FrontendTypeLevelSpec" FrontendTypeLevelSpec.spec
    timedSpec "FrontendDesugarSpec" FrontendDesugarSpec.spec
    timedSpec "XMLFParseSpec" XMLFParseSpec.spec
    timedSpec "XMLFPrettySpec" XMLFPrettySpec.spec
    timedSpec "FrozenParitySpec" FrozenParitySpec.spec
    timedSpec "Phi.WitnessDomainSpec" Phi.WitnessDomainSpec.spec
    timedSpec "Phi.AlignmentSpec" Phi.AlignmentSpec.spec
    timedSpec "TranslatablePresolutionSpec" TranslatablePresolutionSpec.spec
    timedSpec "PhiSoundnessSpec" PhiSoundnessSpec.spec
    timedSpec "AlignmentInvariantSpec" AlignmentInvariantSpec.spec
    timedSpec "ExpansionMinimalitySpec" ExpansionMinimalitySpec.spec
    timedSpec "Constraint.SolvedSpec" Constraint.SolvedSpec.spec
    timedSpec "Util.UnionFindSpec" Util.UnionFindSpec.spec
    timedSpec "Util.GraphSpec" Util.GraphSpec.spec
    timedSpec "Reify.TypeOpsSpec" Reify.TypeOpsSpec.spec
    timedSpec "Reify.NamedSpec" Reify.NamedSpec.spec
    timedSpec "Reify.TypeSpec" Reify.TypeSpec.spec
    timedSpec "Reify.CoreSpec" Reify.CoreSpec.spec
    timedSpec "Property.QuickCheckPropertySpec" Property.QuickCheckPropertySpec.spec
    timedSpec "Thesis.ObligationPropertySpec" Thesis.ObligationPropertySpec.spec
    timedSpec "GoldenSpec" GoldenSpec.spec

timedSpec :: String -> Spec -> Spec
timedSpec label =
  aroundAll_ $ \runSpec -> do
    settings <- testTimingSettings
    case settings of
      Nothing ->
        runSpec
      Just mbPath -> do
        start <- getMonotonicTimeNSec
        runSpec `finally` do
          end <- getMonotonicTimeNSec
          let seconds = fromIntegral (end - start) / 1000000000 :: Double
              line = "TEST-TIMING\t" ++ label ++ "\t" ++ printf "%.3f" seconds
          hPutStrLn stderr line
          case mbPath of
            Nothing -> pure ()
            Just path -> appendFile path (line ++ "\n")

testTimingSettings :: IO (Maybe (Maybe FilePath))
testTimingSettings = do
  mbEnabled <- lookupEnv "MLF_TEST_TIMING"
  mbPath <- lookupEnv "MLF_TEST_TIMING_FILE"
  let enabled =
        case mbEnabled of
          Just value -> value `notElem` ["", "0", "false", "False"]
          Nothing -> False
      mbNonEmptyPath =
        case mbPath of
          Just path | not (null path) -> Just path
          _ -> Nothing
  pure $
    if enabled || maybe False (const True) mbNonEmptyPath
      then Just mbNonEmptyPath
      else Nothing
