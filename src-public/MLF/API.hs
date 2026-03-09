{- |
Module      : MLF.API
Description : Stable umbrella API for downstream eMLF/xMLF users

`MLF.API` is the frontend-oriented umbrella API for downstream callers that
want surface syntax, parsing/pretty-printing, and normalization helpers.

Choose `MLF.Pipeline` for constraint generation, elaboration, execution,
typechecking, and runtime-facing xMLF helpers. Choose `MLF.XMLF` when you only
need explicit xMLF syntax tooling.
-}
{-# LANGUAGE PatternSynonyms #-}
module MLF.API
    ( -- * Frontend syntax (raw and staged types)
      module MLF.Frontend.Syntax
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
