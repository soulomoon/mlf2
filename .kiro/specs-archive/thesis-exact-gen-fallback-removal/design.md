# Design Document

## Overview
Refactor binder enumeration and translation to be thesis-exact: Q(n) comes only from
direct flex children of n; gen nodes are translated explicitly via Q(g); and any graph
that would have relied on gen-ancestor fallback is rejected early. This removes the
implicit fallback paths in `contextToNodeBound` and `reify`, and aligns scheme
translation with the thesis model.

## Architecture
- Binding tree remains the single source of Q(n)/Q(g).
- Type translation (S/T/Q) uses only direct binding edges on type nodes.
- Scheme translation uses explicit gen-node Q(g) with direct binders.
- Validation rejects constraints that still encode binders only on gen ancestors.

## Components and Interfaces
- `src/MLF/Elab/Phi.hs`
  - Remove gen-ancestor fallback in `contextToNodeBoundWithOrderKeys`.
  - Binder enumeration uses `Binding.boundFlexChildrenUnder` on `TypeRef` only.
- `src/MLF/Elab/Reify.hs`
  - Remove fallback binder enumeration (`closestGenAncestor`/`immediateGen`).
  - Add or reuse helper(s) to compute Q(g) (direct gen-node binders) for scheme
    translation, if needed by elaboration entrypoints.
- `src/MLF/Elab/Generalize.hs`
  - For `GenRef` scopes, replace `interiorOfUnder` candidate selection with direct
    Q(g) (`Binding.boundFlexChildrenUnder` on `GenRef`), maintaining ≺ ordering and
    bound dependency ordering.
- `src/MLF/Binding/Tree.hs`
  - Add `checkNoGenFallback` (or similar) that detects cases where type-node binder
    enumeration would require gen-ancestor fallback.
  - Integrate the check into `checkBindingTree`/`checkBindingTreeUnder` or a new
    elaboration-ready validator.
- `test/ElaborationSpec.hs`
  - Update context tests to attach binder edges to `TyForall` nodes.
  - Add tests for gen-node translation (Q(g)) and invariant-check failure.

## Data Models
- No new core data types required.
- Introduce a new `BindingError` or `ElabError` constructor (e.g.,
  `GenFallbackRequired { binder :: NodeId, gen :: GenNodeId, binders :: [NodeId] }`)
  to report invariant violations.

## Error Handling
- Invariant violations are reported as structured errors (binding-tree error or
  elaboration error), containing the binder node and offending gen node(s).
- Context computation errors remain `InstantiationError` with clear messages.

## Testing Strategy
- Unit tests for Q(g) ordering/bounds using direct gen children.
- Regression tests for context computation without fallback.
- Negative test to ensure fallback-dependent graphs are rejected.
- Full suite: `cabal test --test-show-details=direct`.
