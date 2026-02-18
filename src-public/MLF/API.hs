{-# LANGUAGE PatternSynonyms #-}
module MLF.API
    ( -- * Frontend syntax (raw and staged types)
      module MLF.Frontend.Syntax
    -- * Constraint generation
    , ConstraintResult (..)
    , ConstraintError (..)
    -- * Parse error types
    , EmlfParseError
    , NormParseError (..)
    , renderEmlfParseError
    , renderNormParseError
    -- * Raw parser entrypoints
    , parseRawEmlfExpr
    , parseRawEmlfType
    -- * Normalized parser entrypoints
    , parseNormEmlfExpr
    , parseNormEmlfType
    -- * Normalization (raw → normalized)
    , NormalizationError (..)
    , normalizeType
    , normalizeExpr
    -- * Pretty-printing
    , prettyEmlfExpr
    , prettyEmlfType
    -- * Constraint graph
    , BaseTy (..)
    , PolySyms
    , inferConstraintGraph
    -- * Elaboration types
    , ElabType
    , Ty (..)
    , ElabScheme
    , pattern Forall
    , ElabTerm
    , Instantiation (..)
    , ElabError (..)
    , TypeCheckError (..)
    -- * Pipeline configuration
    , PipelineConfig(..)
    , defaultPipelineConfig
    , TraceConfig(..)
    , defaultTraceConfig
    -- * Pipeline entrypoints (normalized-only)
    , PipelineError(..)
    , renderPipelineError
    , Pretty (..)
    , schemeFromType
    , runPipelineElab
    , runPipelineElabChecked
    , runPipelineElabWithConfig
    , runPipelineElabCheckedWithConfig
    , typeCheck
    , step
    , normalize
    , isValue
    ) where

import MLF.Frontend.Syntax
import MLF.Frontend.Parse
    ( EmlfParseError
    , NormParseError (..)
    , parseRawEmlfExpr
    , parseRawEmlfType
    , parseNormEmlfExpr
    , parseNormEmlfType
    , renderEmlfParseError
    , renderNormParseError
    )
import MLF.Frontend.Normalize
    ( NormalizationError (..)
    , normalizeType
    , normalizeExpr
    )
import MLF.Frontend.Pretty
    ( prettyEmlfExpr
    , prettyEmlfType
    )
import MLF.Constraint.Types.Graph (BaseTy (..), PolySyms)
import MLF.Frontend.ConstraintGen (ConstraintError (..), ConstraintResult (..))
-- Keep legacy elaboration conversion helpers quarantined in MLF.Elab.Legacy.
import MLF.Elab.Pipeline
    ( ElabError (..)
    , ElabScheme
    , pattern Forall
    , ElabTerm
    , ElabType
    , Ty (..)
    , Instantiation (..)
    , TypeCheckError (..)
    , PipelineConfig(..)
    , defaultPipelineConfig
    , TraceConfig(..)
    , defaultTraceConfig
    , PipelineError(..)
    , Pretty (..)
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
import MLF.Pipeline (inferConstraintGraph)
