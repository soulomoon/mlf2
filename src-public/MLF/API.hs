{-# LANGUAGE DataKinds #-}
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
    parseLocatedProgram,
    parseLocatedProgramWithFile,

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
    LocatedProgram,
    ProgramSpanIndex,
    SourceSpan,
    SourcePosition,
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
import MLF.Constraint.Types.Phase (Phase(Raw))
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
    parseLocatedProgram,
    parseLocatedProgramWithFile,
    parseRawProgram,
    renderProgramParseError,
  )
import MLF.Frontend.Pretty
  ( prettyEmlfExpr,
    prettyEmlfType,
  )
import MLF.Frontend.Pretty.Program (prettyProgram)
import MLF.Frontend.Syntax
import MLF.Frontend.Syntax.Program (LocatedProgram, Program, ProgramSpanIndex, SourcePosition, SourceSpan)

-- | Number of type nodes in a constraint graph.
constraintNodeCount :: Constraint 'Raw -> Int
constraintNodeCount = length . toListNode . cNodes

-- | Total number of edges (instantiation + unification) in a constraint graph.
constraintEdgeCount :: Constraint 'Raw -> Int
constraintEdgeCount c = length (cInstEdges c) + length (cUnifyEdges c)
