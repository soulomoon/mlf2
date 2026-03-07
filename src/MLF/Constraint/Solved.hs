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
    fromPreRewriteState,

    -- * Core queries
    canonical,
    canonicalMap,
    originalConstraint,
    canonicalConstraint,
    lookupNode,
    lookupBindParent,
    bindParents,
    genNodes,
    lookupVarBound,

    -- * Canonical-domain queries
    weakenedVars,
    isEliminatedVar,
    canonicalizedBindParents,

    -- * Validation helpers
    validateCanonicalGraphStrict,
) where

import MLF.Constraint.Solved.Internal
    ( Solved
    , bindParents
    , canonical
    , canonicalConstraint
    , canonicalMap
    , canonicalizedBindParents
    , fromPreRewriteState
    , fromSolveOutput
    , genNodes
    , isEliminatedVar
    , lookupBindParent
    , lookupNode
    , lookupVarBound
    , originalConstraint
    , validateCanonicalGraphStrict
    , weakenedVars
    )
