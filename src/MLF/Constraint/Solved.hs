{- |
Module      : MLF.Constraint.Solved
Description : Opaque abstraction over solved constraint graphs

Thin public facade over the internal solved-graph implementation.
Compatibility builders that are only needed by local owner modules live in
`MLF.Constraint.Solved.Internal`; this facade only re-exports the long-term
`Solved` surface.
-}
module MLF.Constraint.Solved (
    -- * Opaque type
    Solved,
    fromSolveOutput,
    mkTestSolved,
    fromPreRewriteState,

    -- * Core queries
    canonical,
    canonicalMap,
    originalConstraint,
    canonicalConstraint,
    lookupNode,
    allNodes,
    lookupBindParent,
    bindParents,
    instEdges,
    genNodes,
    lookupVarBound,

    -- * Mutation helpers
    pruneBindParentsSolved,

    -- * Extended queries
    classMembers,
    originalNode,
    originalBindParent,
    wasOriginalBinder,

    -- * Canonical-domain queries
    weakenedVars,
    isEliminatedVar,
    canonicalizedBindParents,

    -- * Validation helpers
    validateCanonicalGraphStrict,
    validateOriginalCanonicalAgreement,
) where

import MLF.Constraint.Solved.Internal
    ( Solved
    , allNodes
    , bindParents
    , canonical
    , canonicalConstraint
    , canonicalMap
    , canonicalizedBindParents
    , classMembers
    , fromPreRewriteState
    , fromSolveOutput
    , genNodes
    , instEdges
    , isEliminatedVar
    , lookupBindParent
    , lookupNode
    , lookupVarBound
    , mkTestSolved
    , originalBindParent
    , originalConstraint
    , originalNode
    , pruneBindParentsSolved
    , validateCanonicalGraphStrict
    , validateOriginalCanonicalAgreement
    , wasOriginalBinder
    , weakenedVars
    )
