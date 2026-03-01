# Findings — Pod C Wave 1

- 2026-03-01: Initialized pod task tracking folder in assigned worktree because requested path was absent.
- 2026-03-01: Baseline audit showed exactly one runtime boundary use in owned scope: `src/MLF/Elab/Run/Pipeline.hs` called `rewriteConstraintWithUF` to rebuild solved-for-generalization.
- 2026-03-01: Added a lock test inside `PipelineSpec` (`uses presolution-native solved artifacts`) asserting runtime pipeline source has no `rewriteConstraintWithUF` reference; this failed before implementation and passed after.
- 2026-03-01: Replaced direct boundary rewrite in pipeline with snapshot replay (`solveResultFromSnapshot` + `SolveSnapshot`) and rebuild from canonical solved constraint.
- 2026-03-01: Strengthened `SolveSpec` snapshot test to assert `solveResultFromSnapshot` is equal to `soResult` and remains equivalent to UF rewrite reconstruction.
- 2026-03-01: Required grep check now returns no matches in `src/MLF/Elab/Run` and `src/MLF/Constraint/Presolution`.
