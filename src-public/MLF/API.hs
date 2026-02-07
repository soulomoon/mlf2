{-# LANGUAGE PatternSynonyms #-}
module MLF.API
    ( module MLF.Frontend.Syntax
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
    -- * Legacy parser aliases (backward-compatible, same as raw)
    , parseEmlfExpr
    , parseEmlfType
    -- * Normalization
    , NormalizationError (..)
    , normalizeType
    , normalizeExpr
    -- * Pretty-printing
    , prettyEmlfExpr
    , prettyEmlfType
    , BaseTy (..)
    , PolySyms
    , inferConstraintGraph
    , ElabType
    , Ty (..)
    , ElabScheme
    , pattern Forall
    , ElabTerm
    , Instantiation (..)
    , ElabError (..)
    , TypeCheckError (..)
    , PipelineConfig(..)
    , defaultPipelineConfig
    , TraceConfig(..)
    , defaultTraceConfig
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
    , parseEmlfExpr
    , parseEmlfType
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
