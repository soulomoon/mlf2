# Design Document

## Overview
We make binding-edge harmonization paper-faithful by:

1) removing the remaining “no common ancestor ⇒ no-op” behavior, and
2) enforcing a single gen-rooted binding tree so LCA is total for well-formed
   constraints.

Constraints are rooted at a gen node created during constraint generation. That
root lists the term-DAG roots as schemes and flexibly binds them, so Raise-to-LCA
is defined without synthetic type roots.

## Architecture
Changes are localized and mostly additive:
- `src/MLF/Constraint/Types.hs`: `BindingError` uses `NoCommonAncestor` with
  `NodeRef`; binding edges are over `NodeRef`.
- `src/MLF/Frontend/ConstraintGen/*`: allocate a root gen node and bind term-DAG
  roots under it.
- `src/MLF/Binding/Tree.hs` / `src/MLF/Binding/Adjustment.hs`: remove fallback
  and make missing-LCA structured.
- Phase entry points (`MLF.Constraint.Presolution.Driver`,
  `MLF.Constraint.Solve`): maintain gen-rooted constraints via
  `Binding.checkBindingTree`.
- Traversals (`Order`, `Solve`, etc.): use the gen-rooted binding tree (no
  synthetic type root).

## Components and Interfaces

### `BindingError`
Uses:
```hs
NoCommonAncestor NodeRef NodeRef
```
so callers can match the failure without string comparisons.

### Gen-rooted binding tree
- `GenNode` holds `gnSchemes :: [NodeId]` (term-DAG roots).
- The binding tree root is `GenRef rootGen`.
- Each scheme root is flexibly bound to the root gen node unless it already has
  a parent.

### `Binding.bindingLCA` / `BindingAdjustment.harmonizeBindParentsWithTrace`
- Missing LCA returns `Left (NoCommonAncestor ...)` and is propagated.

### Rootedness enforcement
- Root gen node is created in constraint generation.
- Presolution/solve rewrites preserve `cGenNodes` and binding edges;
  `Binding.checkBindingTree` ensures a single gen root.

## Data Models
- `BindingError` includes `NoCommonAncestor NodeRef NodeRef`.
- No synthetic root type node; rooting is done by `GenNode`.

## Error Handling
- In trace APIs (`Either BindingError ...`), `NoCommonAncestor` propagates.
- In total wrappers, missing LCA is an error.
- After rooting via the gen node, `NoCommonAncestor` should be unreachable in
  normal pipeline runs.

## Testing Strategy
- Unit test: harmonizing nodes from disconnected binding trees yields
  `NoCommonAncestor`.
- Unit test: root gen node binds all term-DAG roots so `bindingLCA` succeeds.
- Full suite: `cabal test`.
