{- |
Module      : MLF.Elab.Phi.TestOnly
Description : Test-only Φ translation entrypoints

This module exposes Φ translation helpers for tests and debugging.
The no-trace entrypoint is strict fail-fast; use the auto-trace helper for
trace-backed positive fixtures.
-}
module MLF.Elab.Phi.TestOnly (
    phiFromEdgeWitnessNoTrace,
    phiFromEdgeWitnessAutoTrace,
    phiFromEdgeWitness,
    shadowCompareTypesTestOnly,
    selectSolvedOrderWithShadowTestOnly,
    normalizeInstTestOnly
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Presolution (EdgeTrace(..), fromListInterior)
import MLF.Constraint.Solved (Solved)
import MLF.Constraint.Types.Graph (NodeId, NodeRef)
import MLF.Constraint.Types.Witness (EdgeWitness(..), InstanceOp(..), getInstanceOps)
import MLF.Elab.Generalize (GaBindParents, shadowCompareTypes, selectSolvedOrderWithShadow)
import MLF.Elab.Phi.Translate (phiFromEdgeWitness, phiFromEdgeWitnessNoTrace, phiFromEdgeWitnessWithTrace)
import MLF.Elab.Phi.Omega (normalizeInst)
import MLF.Elab.Types (ElabError, ElabType, ElabScheme, Instantiation, SchemeInfo)
import MLF.Util.Trace (TraceConfig)

shadowCompareTypesTestOnly :: String -> ElabType -> ElabType -> Either ElabError ()
shadowCompareTypesTestOnly = shadowCompareTypes

selectSolvedOrderWithShadowTestOnly
    :: String
    -> ElabType
    -> Maybe ElabType
    -> Either ElabError ElabType
selectSolvedOrderWithShadowTestOnly = selectSolvedOrderWithShadow

-- | Trace-backed helper for tests that previously used the no-trace entrypoint.
-- The synthesized trace keeps root + witness op targets in I(r) and uses an
-- empty replay map when the fixture does not model binder-source provenance.
phiFromEdgeWitnessAutoTrace
    :: TraceConfig
    -> (Maybe GaBindParents -> Solved -> NodeRef -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String))
    -> Solved
    -> Maybe SchemeInfo
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessAutoTrace traceCfg generalizeAtWith solved mSchemeInfo ew =
    phiFromEdgeWitnessWithTrace
        traceCfg
        generalizeAtWith
        solved
        Nothing
        mSchemeInfo
        (Just syntheticTrace)
        ew
  where
    opTargets op =
        case op of
            OpGraft arg n -> [arg, n]
            OpMerge n m -> [n, m]
            OpRaise n -> [n]
            OpWeaken n -> [n]
            OpRaiseMerge n m -> [n, m]

    syntheticTrace =
        EdgeTrace
            { etRoot = ewRoot ew
            , etBinderArgs = []
            , etInterior = fromListInterior (ewRoot ew : concatMap opTargets (getInstanceOps (ewWitness ew)))
            , etBinderReplayMap = IntMap.empty
            , etCopyMap = mempty
            }

normalizeInstTestOnly :: Instantiation -> Instantiation
normalizeInstTestOnly = normalizeInst
