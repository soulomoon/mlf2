{- |
Module      : MLF.Elab.Phi.TestOnly
Description : Test-only Φ translation entrypoints

This module exposes Φ translation helpers for tests and debugging.
The no-trace entrypoint is strict fail-fast; positive fixtures should
call the trace-backed pipeline entrypoint with explicit traces.
-}
module MLF.Elab.Phi.TestOnly (
    phiFromEdgeWitnessNoTrace,
    phiFromEdgeWitness,
    shadowCompareTypesTestOnly,
    selectSolvedOrderWithShadowTestOnly,
    normalizeInstTestOnly
) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types.Graph (NodeId, NodeRef)
import MLF.Constraint.Types.Witness (EdgeWitness(..))
import MLF.Elab.Generalize (GaBindParents, shadowCompareTypes, selectSolvedOrderWithShadow)
import MLF.Elab.Phi.Omega (normalizeInst)
import MLF.Elab.Types (ElabError(..), ElabType, ElabScheme, Instantiation, SchemeInfo)
import MLF.Util.Trace (TraceConfig)

shadowCompareTypesTestOnly :: String -> ElabType -> ElabType -> Either ElabError ()
shadowCompareTypesTestOnly = shadowCompareTypes

selectSolvedOrderWithShadowTestOnly
    :: String
    -> ElabType
    -> Maybe ElabType
    -> Either ElabError ElabType
selectSolvedOrderWithShadowTestOnly = selectSolvedOrderWithShadow

type GeneralizeAtWith =
    Maybe GaBindParents
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)

-- | Strict no-trace path for tests/debugging:
-- fails fast because production Φ requires edge traces.
phiFromEdgeWitnessNoTrace
    :: TraceConfig
    -> GeneralizeAtWith
    -> Maybe SchemeInfo
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessNoTrace _traceCfg _generalizeAtWith _mSchemeInfo ew =
    Left (MissingEdgeTrace (ewEdgeId ew))

-- | Legacy alias for the fail-fast no-trace helper.
phiFromEdgeWitness
    :: TraceConfig
    -> GeneralizeAtWith
    -> Maybe SchemeInfo
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitness = phiFromEdgeWitnessNoTrace

normalizeInstTestOnly :: Instantiation -> Instantiation
normalizeInstTestOnly = normalizeInst
