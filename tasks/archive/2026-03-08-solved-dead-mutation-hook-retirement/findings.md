# Findings

- Pending dead-hook reference audit.
- Exact reference audit found no live `src/`, `test/`, or `src-public/` call sites for `rebuildWithNodes`, `rebuildWithBindParents`, or `rebuildWithGenNodes`; the remaining mentions were historical docs/audit notes plus the definitions/export lines in `MLF.Constraint.Solved` itself.
- The first cleanup change is therefore the smallest safe one implied by the classification table: retire those three dead mutation hooks from the production `Solved` API and add a source guard preventing them from reappearing.
