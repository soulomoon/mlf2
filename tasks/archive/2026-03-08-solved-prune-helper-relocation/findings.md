# Findings

- `pruneBindParentsSolved` has one live production owner (`MLF.Constraint.Finalize`) and one test call in `ElaborationSpec`; it no longer needs to live on the public `Solved` facade.
