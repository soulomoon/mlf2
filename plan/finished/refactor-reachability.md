# Refactor plan: unify reachability traversal helpers

## Goal
Deduplicate DFS/reachability code over constraint nodes (with optional bound traversal and canonicalization).

## Scope
- `src/MLF/Binding/Tree.hs` (`reachableFromWithBounds` local)
- `src/MLF/Constraint/Presolution/Base.hs` (`reachableFromWithBounds` local)
- `src/MLF/Constraint/Presolution/Driver.hs` (`reachableFrom` local)
- `src/MLF/Constraint/Presolution/Copy.hs` (`reachableFromMany` local)
- Existing utilities: `src/MLF/Constraint/Traversal.hs`, `src/MLF/Elab/Util.hs`

## Proposed abstraction
Extend `MLF.Constraint.Traversal` with a generic helper:

```
reachableFromNodes
  :: (NodeId -> NodeId)       -- canonicalize
  -> (NodeId -> [NodeId])     -- children (optionally include bounds)
  -> [NodeId]                 -- roots
  -> IntSet
```

Then re-express local helpers in each module in terms of this function, using module-specific child functions.

## Steps
1. Add `reachableFromNodes` (and maybe `reachableFromOne`) to `Constraint.Traversal`.
2. Replace each local DFS with the shared helper, preserving canonicalization and bound-child logic.
3. Remove local helper definitions and unused imports.

## Risks
- Watch differences in whether `structuralChildren` include bounds and when canonicalization is applied (pre/post). Keep semantics identical.

## Verification
- `cabal test` (especially BindingSpec, PresolutionSpec).
