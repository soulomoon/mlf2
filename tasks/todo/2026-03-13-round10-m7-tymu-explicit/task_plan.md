# Task Plan

## Summary
Goal: Implement the Round 10 M7 explicit-only `TyMu` slice in the delegated worktree by lowering `STMu` annotations into an acyclic graph `TyMu`, threading it through structural traversal/reification/result-type reconstruction, and adding focused regression coverage without enabling recursive-type inference.

## Phases
1. Establish implementation context from roadmap/design docs, current code paths, and failing regression expectations. - completed
2. Add failing focused tests for explicit recursive-annotation lowering/reconstruction and acyclicity coverage. - completed
3. Implement acyclic graph `TyMu` support through constraint generation, traversal, reification, and structural matching while preserving explicit-only boundaries. - completed
4. Update adjacent docs if the behavior boundary changes, then run the required targeted checks and the full Cabal gate. - completed

## Design Notes
- The design/approval phase is already supplied by `docs/plans/2026-03-13-m7-tymu-design-resolution.md` plus the RoundPlan packet, so this task is executing an approved slice rather than inventing new behavior.
- Automatic recursive-type inference remains out of scope; only explicit `STMu` annotations may produce graph `TyMu`.

## Errors Encountered
| Error | Attempt | Resolution |
|------|---------|------------|
| Parallel `cabal test` runs collided in `dist-newstyle` / package DB during initial RED pass. | 1 | Re-run Cabal checks sequentially only; do not parallelize builds in this worktree. |
| Full-gate verification output exceeded the default tool capture budget. | 1 | Keep the successful command/exit status as the source of truth and rely on the captured tail summary (`1096 examples, 0 failures`) for handoff evidence. |
