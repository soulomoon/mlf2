{- |
Module      : MLF.Elab.Phi.TestOnly
Description : Test-only Φ translation entrypoints

This module exposes Φ translation helpers that bypass trace requirements.
These entrypoints are intended only for tests and debugging.
-}
module MLF.Elab.Phi.TestOnly (
    phiFromEdgeWitnessNoTrace,
    phiFromEdgeWitness,
    shadowCompareTypesTestOnly,
    selectSolvedOrderWithShadowTestOnly
) where

import MLF.Elab.Generalize (shadowCompareTypes, selectSolvedOrderWithShadow)
import MLF.Elab.Phi.Translate (phiFromEdgeWitness, phiFromEdgeWitnessNoTrace)
import MLF.Elab.Types (ElabError, ElabType)

shadowCompareTypesTestOnly :: String -> ElabType -> ElabType -> Either ElabError ()
shadowCompareTypesTestOnly = shadowCompareTypes

selectSolvedOrderWithShadowTestOnly
    :: String
    -> ElabType
    -> Maybe ElabType
    -> Either ElabError ElabType
selectSolvedOrderWithShadowTestOnly = selectSolvedOrderWithShadow
