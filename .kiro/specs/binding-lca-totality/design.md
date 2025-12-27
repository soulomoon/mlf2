# Design Document

## Overview
We make binding-edge harmonization paper-faithful by:

1) removing the remaining “no common ancestor ⇒ no-op” behavior, and
2) restoring the paper’s rooted-constraint assumption by maintaining a synthetic
   constraint root.

Concretely, we introduce an internal `TyRoot` node that connects all term-DAG
roots via structure edges and flexibly binds those roots under a single binding
root. This makes binding LCA total for well-formed pipeline constraints and
lets Raise-to-LCA be implemented without legacy fallbacks.

## Architecture
Changes are localized and mostly additive:
- `src/MLF/Constraint/Types.hs`: new `BindingError` constructor; new internal `TyRoot` node.
- `src/MLF/Constraint/Root.hs`: pure “shaping” pass that ensures a synthetic root.
- `src/MLF/Binding/Tree.hs` / `src/MLF/Binding/Adjustment.hs`: remove fallback and make
  missing-LCA structured.
- Phase entry points (`MLF.Frontend.ConstraintGen`, `MLF.Constraint.Presolution.Driver`):
  call `ensureConstraintRoot` to guarantee rooted constraints early.
- Remaining traversals (`Solve`, `Order`, etc.): treat `TyRoot` as a structural
  node with children.

## Components and Interfaces

### `BindingError`
Add a dedicated constructor (in `MLF.Constraint.Types`):
```hs
NoCommonAncestor NodeId NodeId
```
so callers can match the failure without string comparisons.

### `TyNode.TyRoot`
Add an internal constructor (in `MLF.Constraint.Types`):
```hs
TyRoot { tnId :: NodeId, tnChildren :: [NodeId] }
```
with the invariant: `TyRoot` is solver-internal and represents only structural
edges (it is not an xMLF type constructor).

### `Binding.bindingLCA` (`MLF.Binding.Tree`)
Change:
- from `Left (InvalidBindingTree "No common ancestor found")`
- to `Left (NoCommonAncestor n1 n2)`

### `BindingAdjustment.harmonizeBindParentsWithTrace` (`MLF.Binding.Adjustment`)
Change behavior:
- previously: `NoCommonAncestor` was treated as “no-op; return (c0, [])”
- now: propagate the error (`Left NoCommonAncestor{...}`)

### `BindingAdjustment.harmonizeBindParents`
Keep it as a convenience wrapper, but remove the special-case no-op; on failure,
raise an explicit exception (as it already does for other binding errors).

### `MLF.Constraint.Root.ensureConstraintRoot`
Expose:
```hs
findConstraintRoot :: Constraint -> Maybe NodeId
ensureConstraintRoot :: Constraint -> Constraint
attachNodeToConstraintRoot :: NodeId -> NodeId -> Constraint -> Constraint
```
Behavior:
- If no `TyRoot` exists and there are multiple term-DAG roots, create a fresh
  `TyRoot` node with `tnChildren = roots` and bind each root under it (flex).
- If a `TyRoot` exists, attach any currently-disconnected term-DAG roots as
  additional children and ensure they are bound under the root.

## Data Models
`BindingError` gains (in `MLF.Constraint.Types`):
- `NoCommonAncestor NodeId NodeId`

`TyNode` gains (in `MLF.Constraint.Types`):
- `TyRoot { tnId :: NodeId, tnChildren :: [NodeId] }`

## Error Handling
- In “trace” APIs (`Either BindingError ...`), `NoCommonAncestor` propagates.
- In “total” convenience wrappers (like `harmonizeBindParents`), `NoCommonAncestor`
is treated as a hard error (exception), consistent with other invariant
violations that cannot be represented in the return type.

After `ensureConstraintRoot`, `NoCommonAncestor` is expected to be unreachable in
normal pipeline runs; if it occurs, it indicates a bug in root maintenance or an
ill-formed constraint.

## Testing Strategy
- Update the existing unit test that currently asserts the fallback behavior for
disconnected binding trees; it should now assert the `NoCommonAncestor` error.
- Add a unit test for `ensureConstraintRoot` showing it connects two disconnected
  components so `bindingLCA` succeeds.
- Prefer a property test: for a generated small constraint with multiple
  components, after `ensureConstraintRoot` every pair of nodes shares an LCA.
- Run the full suite (`cabal --config-file=.cabal-config test --test-show-details=direct`).
