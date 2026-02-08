# Task Plan: A7 Group 3 Test Harness Dedup

## Goal
Centralize duplicated test-only pipeline helpers into `test/SpecUtil.hs` and migrate `PipelineSpec`, `ElaborationSpec`, and `ConstraintGenSpec` to shared helpers without behavioral changes.

## Source Plan
`docs/plans/2026-02-08-a7-group-3-test-harness-dedup-implementation-plan.md`

## Phases
- [x] Phase 1: Task 1 - Add shared helpers + initial migrated usages
- [x] Phase 2: Task 2 - Fully migrate PipelineSpec
- [x] Phase 3: Task 3 - Fully migrate ElaborationSpec/ConstraintGenSpec
- [x] Phase 4: Task 4 - Group verification + cleanup/docs

## Decisions
- Execute using subagent-driven task loop per task: implementer -> spec compliance review -> code quality review.
- Keep changes behavior-preserving and limited to test harness refactor scope.
- Post-review remediation for Task 2 must use single-run pipeline artifacts in each test block (no cross-run mixing) and keep strict solved-graph checks.
- Re-review of fix commit range `b8874a57..e3ab1240` is scoped only to verifying the two prior Task 2 code-quality issues.

## Errors Encountered
| Date | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-02-08 | Expected red-phase compile failure (`SpecUtil` missing exports: `runToSolvedDefault`, `unsafeNormalizeExpr`, `firstShowE`) | 1 | Added the three exports + minimal implementations in `test/SpecUtil.hs`; reran focused tests to green |

## Additional Error Log
| Date | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-02-08 | Task 2 review found mixed pipeline artifacts from separate runs in `PipelineSpec` plus dropped strict solved validation | 1 | Reworked flagged test blocks to use single-run pipeline artifacts and restored strict validation checks; re-reviewed to approval |

## Task 3 Execution Notes
- 2026-02-08: Phase 3 entered (red phase). Removing local helper definitions first to force compile/test failure before migrating to `SpecUtil` helpers.
- 2026-02-08: RED verification complete for Phase 3. Focused Elaboration/ConstraintGen run failed with unresolved `firstShow`/`runToPresolution`, confirming test harness migration was not yet implemented.
- 2026-02-08: GREEN implementation done for Task 3 in `test/ElaborationSpec.hs` and `test/ConstraintGenSpec.hs`.
- 2026-02-08: Focused verification status:
  - `Elaboration` matcher: PASS (10 examples)
  - `ConstraintGen` matcher: PASS (0 examples)
- 2026-02-08: Encountered Cabal build artifact race when two test commands were launched in parallel; resolved by rerunning `ConstraintGen` command sequentially.
- 2026-02-08: Task 3 committed as `828d1c06b4d435865fdbb027f2f1211478f9b2af`.
| 2026-02-08 | Task 3 code-quality review found elimination assertion vacuous when eliminated set is empty | 1 | Added explicit `eliminated` non-empty assertion; re-ran focused suites and re-reviewed to approval |
| 2026-02-08 | Task 4 spec review found duplicate local `emptyConstraint` helper still present in PipelineSpec | 1 | Removed duplicate helper, switched to SpecUtil export, reran focused suites, re-reviewed to approval |
