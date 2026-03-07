# Progress Log

## 2026-03-08
- Started the cleanup task to relocate the remaining shared `Solved` compatibility builders out of the public facade.
- Split `MLF.Constraint.Solved` into a thin facade plus new `MLF.Constraint.Solved.Internal`, redirected Finalize/Reify owner-local builder uses to the internal module, and updated the public-surface tests accordingly.
- Recovered from an initial failed file-creation attempt by creating `src/MLF/Constraint/Solved/` before writing `Internal.hs` and then reapplying the split cleanly.
