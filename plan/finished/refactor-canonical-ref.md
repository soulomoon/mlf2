# Refactor plan: centralize canonicalRef helper

## Goal
Deduplicate NodeRef canonicalization by reusing a single helper and removing ad-hoc local definitions.

## Scope
- `src/MLF/Constraint/Canonicalize.hs` (export helper)
- `src/MLF/Constraint/Normalize.hs` (remove local `canonicalRef` in `applyUnionFindToConstraint`)
- `src/MLF/Constraint/Presolution/Base.hs`
- `src/MLF/Constraint/Presolution/Copy.hs`
- `src/MLF/Constraint/Presolution/Driver.hs`
- `src/MLF/Binding/Tree.hs`
- `src/MLF/Elab/Reify.hs`
- `src/MLF/Elab/Run/Generalize.hs`

## Proposed abstraction
Expose `canonicalRef :: (NodeId -> NodeId) -> NodeRef -> NodeRef` from `MLF.Constraint.Canonicalize` and use it (qualified) everywhere.

## Steps
1. Export `canonicalRef` from `MLF.Constraint.Canonicalize`.
2. Replace local `canonicalRef` definitions with `Canonicalize.canonicalRef` uses.
3. Add imports where needed; remove now-unused local helpers.

## Risks
- Ensure no change in canonicalization behavior; only the helper is centralized.
- Watch for name clashes if unqualified imports are used.

## Verification
- `cabal test`
