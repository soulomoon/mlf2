# Findings

- Preserving `Solved` opacity requires a split between a thin public facade and a non-exposed internal implementation module; owner modules cannot call relocated builder functions if they remain hidden inside the public facade.
- The only live production callers of the shared compat builders were `MLF.Constraint.Finalize` and `MLF.Reify.Core`; tests still referenced `fromConstraintAndUf` as part of the old public contract, and one pipeline parity test used it directly.
- `fromSolved` and `solvedFromView` were already local to their owner modules, so the relocation work only needed to move `fromConstraintAndUf` and `rebuildWithConstraint` off the public `MLF.Constraint.Solved` surface.
- The safe implementation shape is:
  - `MLF.Constraint.Solved.Internal` contains the full implementation plus compat builders;
  - `MLF.Constraint.Solved` becomes a thin facade re-exporting only the long-term surface;
  - `MLF.Constraint.Finalize` and `MLF.Reify.Core` import the internal module for compat-builder use;
  - public-surface tests shift from `fromConstraintAndUf` to `mkTestSolved` and add a facade-absence guard.
- Verification stayed green after the split: full gate pass, solved suite pass, pipeline migration parity pass, and the existing presolution narrowing guard remained green.
