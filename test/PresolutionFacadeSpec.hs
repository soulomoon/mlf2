module PresolutionFacadeSpec (spec) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import Test.Hspec

spec :: Spec
spec = describe "MLF.Constraint.Presolution facade" $ do
    it "test-only helpers are absent from the production facade" $ do
        src <- readFile "src/MLF/Constraint/Presolution.hs"
        forM_ bannedMarkers $ \marker ->
            src `shouldSatisfy` (not . isInfixOf marker)

    it "test-only helpers live behind the explicit test-support seam" $ do
        src <- readFile "src/MLF/Constraint/Presolution/TestSupport.hs"
        forM_ supportMarkers $ \marker ->
            src `shouldSatisfy` isInfixOf marker


    it "EdgeUnify stays a thin façade over State/Omega/Unify owners" $ do
        src <- readFile "src/MLF/Constraint/Presolution/EdgeUnify.hs"
        forM_
            [ "import MLF.Constraint.Presolution.EdgeUnify.State"
            , "import qualified MLF.Constraint.Presolution.EdgeUnify.Omega as Omega"
            , "import MLF.Constraint.Presolution.EdgeUnify.Unify"
            , "executeEdgeLocalOmegaOps omegaEnv baseOps action = do"
            ] $ \marker ->
                src `shouldSatisfy` isInfixOf marker
        forM_
            [ "data EdgeUnifyState = EdgeUnifyState"
            , "pendingWeakenOwners = do"
            , "flushPendingWeakensAtOwnerBoundary owner ="
            , "unifyAcyclicEdge n1 n2 = do"
            , "unifyStructureEdge n1 n2 = do"
            , "mkOmegaExecEnv copyMap ="
            ] $ \marker ->
                src `shouldSatisfy` (not . isInfixOf marker)

    it "pendingWeakenOwners bypasses the EdgeUnify façade" $ do
        edgeUnifySrc <- readFile "src/MLF/Constraint/Presolution/EdgeUnify.hs"
        driverSrc <- readFile "src/MLF/Constraint/Presolution/Driver.hs"
        edgeProcessingSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing.hs"
        edgeUnifySrc `shouldSatisfy` (not . isInfixOf "pendingWeakenOwners")
        driverSrc `shouldSatisfy` isInfixOf "MLF.Constraint.Presolution.EdgeUnify.Omega (pendingWeakenOwners)"
        edgeProcessingSrc `shouldSatisfy` isInfixOf "MLF.Constraint.Presolution.EdgeUnify.Omega"
        edgeProcessingSrc `shouldSatisfy` isInfixOf "pendingWeakenOwners"

bannedMarkers :: [String]
bannedMarkers =
    [ "PresolutionState(..)"
    , "CopyMapping(..)"
    , "CopyMap"
    , "lookupCopy"
    , "insertCopy"
    , "copiedNodes"
    , "originalNodes"
    , "InteriorNodes(..)"
    , "fromListInterior"
    , "toListInterior"
    , "runPresolutionM"
    , "fromPresolutionResult"
    , "defaultPlanBuilder"
    , "GaBindParents(..)"
    , "validateCrossGenMapping"
    , "decideMinimalExpansion"
    , "processInstEdge"
    , "validateReplayMapTraceContract"
    , "unifyAcyclicRawWithRaiseTrace"
    , "runEdgeUnifyForTest"
    , "instantiateScheme"
    , "instantiateSchemeWithTrace"
    , "mergeExpansions"
    , "applyExpansion"
    , "normalizeEdgeWitnessesM"
    , "validateTranslatablePresolution"
    , "translatableWeakenedNodes"
    ]

supportMarkers :: [String]
supportMarkers =
    [ "module MLF.Constraint.Presolution.TestSupport"
    , "PresolutionState(..)"
    , "CopyMapping(..)"
    , "runPresolutionM"
    , "processInstEdge"
    , "validateTranslatablePresolution"
    ]
