# Design Document

## Overview
`papers/xmlf.txt` models omega execution as a graph transformation from chi_e to chi.
Binders eliminated by Merge/Weaken are removed from Q(n) and their occurrences
are inlined by their bounds (bottom for unbounded). Today we keep eliminated binders
in the graph and rely on `cEliminatedVars` to suppress re-quantification. This
spec replaces that marker with a graph rewrite step so the term-DAG and binding
tree encode elimination directly.

## Architecture
- **Elimination rewrite pass**
  - New pass `rewriteEliminatedBinders` applies after omega execution and solve,
    rewriting the constraint graph to inline eliminated binders and remove them
    from the binding tree.
  - Eliminated binder nodes are removed from `cNodes`; any references are
    substituted to their bounds (or bottom). The solve-time union-find is
    extended with the same substitution so witness ops that mention eliminated
    ids still canonicalize to live nodes.
  - This pass consumes a set of eliminated binders (currently recorded during
    omega execution) and produces a rewritten `Constraint` with no remaining
    elimination markers.
- **Bottom representation**
  - Introduce an explicit `TyBottom` node in the constraint graph so unbounded
    eliminated binders can be substituted by bottom without relying on markers.
  - `TyBottom` is a leaf node (no structural children) and is only introduced
    by the rewrite pass, so solver behavior is unchanged.

## Components and Interfaces
- `src/MLF/Constraint/Types.hs`
  - Add `TyBottom` to `TyNode` and `structuralChildren`.
- `src/MLF/Constraint/Presolution/Driver.hs` (or a new module)
  - Implement `rewriteEliminatedBinders :: Constraint -> EliminatedVars -> Either BindingError Constraint`.
  - The rewrite should:
    - Build a substitution map from eliminated binders to their bounds (or bottom).
    - Rewrite all NodeId references in the term-DAG and var-bound store.
    - Remove binding edges for eliminated binders.
    - Clear `cEliminatedVars` (or drop its usage entirely).
- `src/MLF/Elab/Generalize.hs` / `src/MLF/Elab/Reify.hs`
  - Remove references to `cEliminatedVars`; rely on the rewritten graph.
- `src/MLF/Constraint/Solve.hs`
  - Ensure total pattern matches handle `TyBottom` (even if it never appears
    before solve), to keep `-Wall` clean.

## Data Models
- **`TyBottom` node**: a new leaf constructor in `TyNode` representing bottom.
  - Only introduced after solve in the elimination rewrite pass.
  - Reification maps it to `Elab.TBottom`.

## Error Handling
- `rewriteEliminatedBinders` should return a `BindingError` if a substitution
  would introduce a cycle or if a binding parent becomes invalid.
- After rewrite, run `checkBindingTree` to validate invariants.

## Testing Strategy
- **Unit regression**: build a solved constraint with an eliminated binder
  and verify Q(n) enumeration excludes it after rewrite.
- **Baseline regression**: keep `id id`, bounded aliasing, and `id y` tests
  passing (paper alignment suite).
- **Property test** (if feasible): for any constraint with eliminated binders,
  the rewrite preserves `checkBindingTree` and eliminates all such binders
  from Q(n).

## References
- `papers/xmlf.txt` sec. 3.4 (omega operations) and Figure 9 (Q/R/T translation).
