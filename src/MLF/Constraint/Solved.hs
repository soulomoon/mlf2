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

    -- * Core queries
    canonical,
    canonicalMap,
    originalConstraint,
    canonicalConstraint,

    -- * Validation helpers
    validateCanonicalGraphStrict,
) where

import MLF.Constraint.Solved.Internal
    ( Solved
    , canonical
    , canonicalConstraint
    , canonicalMap
    , fromSolveOutput
    , originalConstraint
    , validateCanonicalGraphStrict
    )
