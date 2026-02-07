{- |
Module      : MLF.Elab.Phi.TestOnly
Description : Test-only Φ translation entrypoints

This module exposes Φ translation helpers that bypass trace requirements.
These entrypoints are intended only for tests and debugging.
-}
module MLF.Elab.Phi.TestOnly (
    phiFromEdgeWitnessNoTrace,
    phiFromEdgeWitness,
    shadowCompareTypesTestOnly
) where

import MLF.Elab.Generalize (shadowCompareTypes)
import MLF.Elab.Phi.Translate (phiFromEdgeWitness, phiFromEdgeWitnessNoTrace)
import MLF.Elab.Types (ElabError, ElabType)

shadowCompareTypesTestOnly :: String -> ElabType -> ElabType -> Either ElabError ()
shadowCompareTypesTestOnly = shadowCompareTypes
