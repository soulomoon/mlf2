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
