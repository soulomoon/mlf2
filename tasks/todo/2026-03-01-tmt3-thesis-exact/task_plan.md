# Task Plan — Pod C Wave 1

## Goal
Remove runtime load-bearing dependence on `rewriteConstraintWithUF` as a boundary layer, ensure solve outputs are canonical for downstream runtime consumers, and preserve solver/type behavior.

## Scope / Ownership
- `src/MLF/Constraint/Solve.hs`
- `src/MLF/Constraint/Presolution/Driver.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
- `test/SolveSpec.hs`
- `test/PipelineSpec.hs`
- `tasks/todo/2026-03-01-tmt3-thesis-exact/*`

## Phases
1. `completed` Baseline + call-site audit + failing/locking tests
2. `completed` Implement canonical solved-artifact path without runtime boundary rewrite dependency
3. `completed` Run targeted checks + regression tests
4. `in_progress` Pod notes update + focused commits + report

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Missing requested task folder in this worktree | 1 | Created `tasks/todo/2026-03-01-tmt3-thesis-exact/` and initialized tracking files. |
