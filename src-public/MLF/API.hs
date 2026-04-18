{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      : MLF.API
-- Description : Stable umbrella API for downstream eMLF/xMLF users
--
-- `MLF.API` is the frontend-oriented umbrella API for downstream callers that
-- want surface syntax, eMLF / `.mlfp` parsing and pretty-printing,
-- normalization helpers, and constraint graph introspection.
--
-- Choose `MLF.Pipeline` for constraint generation, elaboration, execution,
-- typechecking, and runtime-facing xMLF helpers. Choose `MLF.XMLF` when you only
-- need explicit xMLF syntax tooling.
module MLF.API
  ( -- * Frontend syntax (raw and staged types)
    module MLF.Frontend.Syntax,

    -- * Parse error types
    EmlfParseError,
    NormParseError (..),
    ProgramParseError,
    renderEmlfParseError,
    renderNormParseError,
    renderProgramParseError,

    -- * Raw parser entrypoints
    parseRawEmlfExpr,
    parseRawEmlfType,
    parseRawProgram,

    -- * Normalized parser entrypoints
    parseNormEmlfExpr,
    parseNormEmlfType,

    -- * Normalization (raw → normalized)
    NormalizationError (..),
    normalizeType,
    normalizeExpr,

    -- * Pretty-printing
    prettyEmlfExpr,
    prettyEmlfType,
    Program,
    prettyProgram,

    -- * Constraint graph introspection
    Constraint (..),
    NodeId (..),
    TyNode (..),
    InstEdge (..),
    UnifyEdge (..),
    GenNode (..),
    GenNodeId (..),
    BindFlag (..),
    BindParents,
    NodeMap,
    GenNodeMap,
    lookupNode,
    constraintNodeCount,
    constraintEdgeCount,
  )
where

import MLF.Constraint.Types.Graph
  ( BindFlag (..),
    BindParents,
    Constraint (..),
    GenNode (..),
    GenNodeId (..),
    GenNodeMap,
    InstEdge (..),
    NodeId (..),
    NodeMap,
    TyNode (..),
    UnifyEdge (..),
    lookupNode,
    toListNode,
  )
import MLF.Frontend.Normalize
  ( NormalizationError (..),
    normalizeExpr,
    normalizeType,
  )
import MLF.Frontend.Parse
  ( EmlfParseError,
    NormParseError (..),
    parseNormEmlfExpr,
    parseNormEmlfType,
    parseRawEmlfExpr,
    parseRawEmlfType,
    renderEmlfParseError,
    renderNormParseError,
  )
import MLF.Frontend.Parse.Program
  ( ProgramParseError,
    parseRawProgram,
    renderProgramParseError,
  )
import MLF.Frontend.Pretty
  ( prettyEmlfExpr,
    prettyEmlfType,
  )
import MLF.Frontend.Pretty.Program (prettyProgram)
import MLF.Frontend.Syntax
import MLF.Frontend.Syntax.Program (Program)

-- | Number of type nodes in a constraint graph.
constraintNodeCount :: Constraint -> Int
constraintNodeCount = length . toListNode . cNodes

-- | Total number of edges (instantiation + unification) in a constraint graph.
constraintEdgeCount :: Constraint -> Int
constraintEdgeCount c = length (cInstEdges c) + length (cUnifyEdges c)
