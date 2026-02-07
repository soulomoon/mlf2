module MLF.Pipeline
    ( -- * Staged frontend types
      NormSurfaceExpr
    , NormSrcType (..)
    , StructBound (..)
    , NormalizationError (..)
    , normalizeExpr
    , normalizeType
    -- * Constraint generation
    , BaseTy (..)
    , PolySyms
    , inferConstraintGraph
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

import MLF.Frontend.Syntax (NormSurfaceExpr, NormSrcType(..), StructBound(..))
import MLF.Frontend.Normalize (NormalizationError(..), normalizeExpr, normalizeType)
import MLF.Frontend.ConstraintGen (ConstraintError, ConstraintResult, generateConstraints)
import MLF.Constraint.Types.Graph (BaseTy(..), PolySyms)
import MLF.Elab.Pipeline
    ( PipelineConfig(..)
    , PipelineError(..)
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
    , step
    , typeCheck
    )

inferConstraintGraph :: PolySyms -> NormSurfaceExpr -> Either ConstraintError ConstraintResult
inferConstraintGraph = generateConstraints
