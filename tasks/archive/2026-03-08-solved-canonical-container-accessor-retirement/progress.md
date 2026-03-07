# Progress Log

## 2026-03-08
- Started the cleanup task for retiring the dead raw canonical container accessors from the `Solved` surface.
- Removed `canonicalBindParents` and `canonicalGenNodes` from both the public facade and internal implementation, and added a facade-absence guard in `test/Constraint/SolvedSpec.hs`.
