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
    , CycleError(..)
    , renderPipelineError
    , formatPipelineError
    , pipelineErrorPhase
    , pipelineErrorPhaseName
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

import Data.Text (Text)
import qualified Data.Text as T
import MLF.Constraint.Acyclicity (CycleError(..))
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

-- | Extract the numeric pipeline phase where the error occurred.
--
-- Phase mapping:
--
--   * 1 \u2014 Constraint generation
--   * 3 \u2014 Acyclicity check
--   * 4 \u2014 Presolution
--   * 5 \u2014 Solve (unification)
--   * 6 \u2014 Elaboration
--   * 7 \u2014 Type checking
pipelineErrorPhase :: PipelineError -> Int
pipelineErrorPhase err = case err of
    PipelineConstraintError {}  -> 1
    PipelineCycleError {}       -> 3
    PipelinePresolutionError {} -> 4
    PipelineSolveError {}       -> 5
    PipelineElabError {}        -> 6
    PipelineTypeCheckError {}   -> 7

-- | Human-readable name of the pipeline phase where the error occurred.
pipelineErrorPhaseName :: PipelineError -> String
pipelineErrorPhaseName err = case err of
    PipelineConstraintError {}  -> "constraint generation"
    PipelineCycleError {}       -> "acyclicity check"
    PipelinePresolutionError {} -> "presolution"
    PipelineSolveError {}       -> "solve"
    PipelineElabError {}        -> "elaboration"
    PipelineTypeCheckError {}   -> "type checking"

-- | Structured, multi-line 'Text' rendering of a 'PipelineError'.
--
-- Returns output in the format:
--
-- @
-- [Phase N] phase-name error:
--   \<detail from Show instance\>
-- @
--
-- Use 'renderPipelineError' for a single-line 'String' alternative.
formatPipelineError :: PipelineError -> Text
formatPipelineError err =
    T.pack $
        "[Phase "
            ++ show (pipelineErrorPhase err)
            ++ "] "
            ++ pipelineErrorPhaseName err
            ++ " error:\n  "
            ++ detail err
  where
    detail e = case e of
        PipelineConstraintError ce  -> show ce
        PipelineCycleError ce       -> show ce
        PipelinePresolutionError pe -> show pe
        PipelineSolveError se       -> show se
        PipelineElabError ee        -> show ee
        PipelineTypeCheckError te   -> show te
