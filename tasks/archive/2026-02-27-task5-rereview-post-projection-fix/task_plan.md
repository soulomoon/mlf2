# Task Plan - 2026-02-27 Task 5 re-review post-projection fix

## Goal
Verify that fix commit `49eb83bfa53ba04eea47a033d39de4389b5d81c3` does not regress Task 5 spec requirements in `/Volumes/src/mlf4-strict-replay-cutover`.

## Scope
- Re-check Task 5 requirements:
  1. `WitnessValidation` includes `ReplayMapTargetOutsideReplayDomain` and replay-domain membership + `TyVar` checks.
  2. `Driver.computePresolution` includes replay-domain codomain `InternalError` check.
  3. Validation-focused test subsets pass.
  4. Original Task 5 commit message requirement remains satisfied (`0597e0e...`).
- Confirm follow-up fix context: validation moved post-projection in WitnessNorm flow via `normalizeInstanceOpsCore` helper.

## Phases
- [completed] Phase 1: Inspect commits and affected code paths for Task 5 requirements.
- [completed] Phase 2: Run validation-focused test subsets and record outcomes.
- [completed] Phase 3: Decide compliance verdict and report evidence.

## Errors Encountered
| Error | Attempt | Resolution |
|-------|---------|------------|
| Concurrent `cabal test` runs raced on `dist-newstyle` path removal (`removeDirectory... does not exist`) | 1 | Re-ran test subsets serially; both required subsets passed. |

## Decisions
- Used direct source inspection (`git show`, `nl`, `rg`) and targeted subset test runs for evidence-backed compliance verdict.
