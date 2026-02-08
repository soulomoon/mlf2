# Findings: A7 Group 3 Test Harness Dedup

## Discovery Log
- 2026-02-08: Plan requires checkpointed execution in first batch across Tasks 1-3.
- 2026-02-08: Required task flow includes per-task implementer + spec compliance review + code quality review.
- 2026-02-08: Minimal Task 1 migrations chosen per file:
  - `PipelineSpec`: one call site moved to `runToSolvedDefault`.
  - `ElaborationSpec`: `generateConstraintsDefault` now normalizes via shared `unsafeNormalizeExpr`.
  - `ConstraintGenSpec`: one local `firstShow` use replaced with shared `firstShowE`.
- 2026-02-08: Task 2 follow-up review found two regressions:
  - strict solved-graph validation dropped in two elaboration-helper tests.
  - mixed artifacts combined from separate pipeline runs in migrated blocks.
- 2026-02-08: Fixed `PipelineSpec` by recomputing `normalize -> acyclicity -> presolution -> solve` from the same generated constraint per block and restoring fail-fast solved-graph validation in the two affected tests.
- 2026-02-08: Re-review of fix commit `e3ab124024316cc419eada9b40362b978533f72e` against base `b8874a57bbe3552958965f32bcf20f0e36b025c9` confirms both prior issues are resolved:
  - elaboration-helper tests now gate on `validateSolvedGraphStrict` before using solved outputs.
  - migrated blocks no longer mix `PresolutionResult` and `SolveResult` from separate helper runs.
- 2026-02-08 (Task 3): Local helper definitions in `ElaborationSpec`/`ConstraintGenSpec` were removed and replaced with shared `SpecUtil` helpers. Kept local helpers only where they encapsulate spec-unique behavior (e.g., solved+witness+trace tuple assembly).
- 2026-02-08 (Task 3): `--test-option='-m' --test-option='ConstraintGen'` currently matches 0 examples in this suite; command still validates compile/link + filter plumbing.
