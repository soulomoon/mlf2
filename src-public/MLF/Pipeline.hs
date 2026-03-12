{-# LANGUAGE PatternSynonyms #-}
{- |
Module      : MLF.Pipeline
Description : Focused normalized-pipeline API for downstream callers

`MLF.Pipeline` exposes the normalized eMLF pipeline surface without the wider
raw-syntax, parser, or pretty-printing conveniences re-exported by `MLF.API`.

Prefer this module for consumers that already operate on normalized surface
terms and want the smallest supported entrypoint for inference/elaboration.
-}
module MLF.Pipeline
    ( -- * Staged frontend types
      NormSurfaceExpr
    , NormSrcType
    , StructBound
    , NormalizationError (..)
    , normalizeExpr
    , normalizeType
    -- * Constraint generation
    , ConstraintResult (..)
    , ConstraintError (..)
    , BaseTy (..)
    , PolySyms
    , inferConstraintGraph
    -- * Elaboration/runtime types
    , ElabType
    , Ty (..)
    , ElabScheme
    , pattern Forall
    , ElabTerm
    , Instantiation (..)
    , ElabError (..)
    , TypeCheckError
    , Pretty (..)
    , schemeFromType
    -- * Pipeline configuration
    , PipelineConfig(..)
    , defaultPipelineConfig
    , TraceConfig(..)
    , defaultTraceConfig
    -- * Pipeline entrypoints (normalized-only)
    , PipelineError(..)
    , renderPipelineError
    , runPipelineElab
    , runPipelineElabChecked
    , runPipelineElabWithConfig
    , runPipelineElabCheckedWithConfig
    -- * Phase 7 helpers
    , typeCheck
    , step
    , normalize
    , isValue
    ) where

import MLF.Frontend.Syntax (NormSurfaceExpr, NormSrcType, StructBound)
import MLF.Frontend.Normalize (NormalizationError(..), normalizeExpr, normalizeType)
import MLF.Frontend.ConstraintGen (ConstraintError(..), ConstraintResult(..), generateConstraints)
import MLF.Constraint.Types.Graph (BaseTy(..), PolySyms)
-- Keep legacy elaboration conversion helpers quarantined in MLF.Elab.Legacy.
import MLF.Elab.Pipeline
    ( ElabError (..)
    , ElabScheme
    , pattern Forall
    , ElabTerm
    , ElabType
    , Ty (..)
    , Instantiation (..)
    , TypeCheckError
    , PipelineConfig(..)
    , PipelineError(..)
    , Pretty (..)
    , defaultPipelineConfig
    , TraceConfig(..)
    , defaultTraceConfig
    , isValue
    , normalize
    , renderPipelineError
    , runPipelineElab
    , runPipelineElabChecked
    , runPipelineElabWithConfig
    , runPipelineElabCheckedWithConfig
    , schemeFromType
    , step
    , typeCheck
    )

inferConstraintGraph :: PolySyms -> NormSurfaceExpr -> Either ConstraintError ConstraintResult
inferConstraintGraph = generateConstraints
