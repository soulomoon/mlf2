{- |
Module      : MLF.Elab.Phi
Description : Facade for witness translation
Copyright   : (c) 2024
License     : BSD-3-Clause

This module re-exports the live Phi translation helpers from the
split submodules under "MLF.Elab.Phi.*".
-}
module MLF.Elab.Phi (
    -- * Re-exports from Context
    contextToNodeBound,
    -- * Checked occurrence endpoint authority
    PhiOccurrenceRole(..),
    PhiEndpointShapeAuthority(..),
    -- * Construction-closed replay authority
    PhiReplayCertificate,
    mkPhiReplayCertificate,
    -- * Main entry point (chi-native callback: no solved-typed arg)
    phiFromEdgeWitnessWithTrace,
    phiFromEdgeWitnessWithTraceReadModel,
    phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints,
    phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpointsFor,
    phiOccurrenceFromEdgeWitnessWithTrace,
    phiOccurrenceFromEdgeWitnessWithTraceReadModel
) where

import MLF.Elab.Phi.Context (contextToNodeBound)
import MLF.Elab.Phi.Translate
    ( PhiEndpointShapeAuthority(..)
    , PhiOccurrenceRole(..)
    , PhiReplayCertificate
    , mkPhiReplayCertificate
    , phiFromEdgeWitnessWithTrace
    , phiFromEdgeWitnessWithTraceReadModel
    , phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpoints
    , phiFromEdgeWitnessWithTraceReadModelAtFrozenEndpointsFor
    , phiOccurrenceFromEdgeWitnessWithTrace
    , phiOccurrenceFromEdgeWitnessWithTraceReadModel
    )
