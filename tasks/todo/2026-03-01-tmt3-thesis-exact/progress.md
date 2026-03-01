# Progress — Pod C Wave 1

- 2026-03-01: Confirmed branch `codex/tmt3-solve-no-rewrite-layer-wave1` in assigned worktree.
- 2026-03-01: Created pod task files under `tasks/todo/2026-03-01-tmt3-thesis-exact/`.
- 2026-03-01: Ran required audit command `rg -n "rewriteConstraintWithUF" src/MLF/Elab/Run src/MLF/Constraint/Presolution`; found one call in `src/MLF/Elab/Run/Pipeline.hs`.
- 2026-03-01: Added TDD lock assertion to `test/PipelineSpec.hs` requiring no runtime `rewriteConstraintWithUF` reference in pipeline source.
- 2026-03-01: Ran required targeted test command; observed expected red failure only in `PipelineSpec` lock test.
- 2026-03-01: Updated `src/MLF/Elab/Run/Pipeline.hs` to replay canonical solved constraint via `solveResultFromSnapshot` instead of `rewriteConstraintWithUF`.
- 2026-03-01: Updated `test/SolveSpec.hs` snapshot test to assert snapshot replay equality with `solveUnifyWithSnapshot` output.
- 2026-03-01: Re-ran required grep command; zero matches in owned runtime directories.
- 2026-03-01: Re-ran required targeted test command; pass (45 examples, 0 failures).
