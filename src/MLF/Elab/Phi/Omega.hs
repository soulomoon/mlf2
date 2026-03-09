{- |
Module      : MLF.Elab.Phi.Omega
Description : Omega/Step interpretation for witness translation
Copyright   : (c) 2024
License     : BSD-3-Clause

This module hosts the Omega/Step interpretation helpers used by the
Phi translation pipeline, keeping the main Phi facade focused on orchestration.
-}
module MLF.Elab.Phi.Omega (
    OmegaContext(..),
    phiWithSchemeOmega,
    normalizeInst,
    collapseAdjacentPairs
) where

import MLF.Elab.Phi.Omega.Domain (OmegaContext(..))
import MLF.Elab.Phi.Omega.Interpret (phiWithSchemeOmega)
import MLF.Elab.Phi.Omega.Normalize (collapseAdjacentPairs, normalizeInst)
