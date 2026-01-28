# Design Document

## Overview
`papers/xmlf.txt` defines rigid nodes as inlined (R(n)=T(n)). Without inlining,
non-Forall generalization that respects rigid edges can yield open types.
This design updates reification to inline rigid TyVars and extends binder
reachability to include rigid-variable bounds so top-level schemes remain
closed and paper-aligned.

## Architecture
- **`MLF.Elab.Reify`**
  - Detect rigid TyVar nodes via the binding tree.
  - Reify rigid nodes by reifying their bound (or ⊥ if absent).
- **`MLF.Elab.Generalize`**
  - Extend reachability to follow bounds of rigid TyVars when computing
    binder candidates and reachable type nodes.
  - Reintroduce rigid-edge filtering in `boundAtScope` for non-Forall scopes,
    excluding only restricted nodes (own rigid edge) and allowing locked nodes.
- **`MLF.Constraint.Solve`**
  - Rewrite `cVarBounds`/`cEliminatedVars` to canonical ids after solve so
    rigid-bound lookups remain stable.

## Components and Interfaces
- `src/MLF/Elab/Reify.hs`
  - `reifyType` / `reifyTypeWithNames`: inline rigid vars using
    `VarStore.lookupVarBound` and `Binding.lookupBindParentUnder`.
- `src/MLF/Elab/Generalize.hs`
  - Add a reachability helper that follows structural edges plus bounds of
    rigid TyVars.
  - Update `boundAtScope` to reject rigid edges only at the first binding step
    (restricted nodes), not for locked descendants.

## Data Models
No changes to runtime data structures. The existing `cBindParents` flags and
`cVarBounds` store are reused to determine rigidity and bounds.

## Error Handling
- Binding-tree lookup failures surface as `ElabError` via `bindingToElab`.
- Rigid TyVars without bounds are reified as `TBottom` to preserve closure.

## Testing Strategy
- Add a regression unit test in `test/ElaborationSpec.hs` that builds a
  constraint with a rigid TyVar bound to a flex var and asserts
  `generalizeAt` produces `∀a. a -> a`.
- Rerun existing paper-alignment baselines (`id id`, `let id = ... in id y`).
