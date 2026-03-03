# Task Plan: Strict Replay Failure Triage

## Goal
Identify root-cause interactions behind current strict replay regressions and propose the smallest line-level code change set that restores the listed failing tests without regressing protected tests.

## Scope
- Investigate failing tests in `ElaborationSpec` and `PipelineSpec` listed by user.
- Focus files: `WitnessNorm.hs`, `Translate.hs`, `Omega.hs`.
- Preserve behavior for:
  - Driver replay-map boundary tests
  - A6 parity test
  - BUG-2026-02-17-002 test
  - redirected let-use polymorphic scheme behavior
  - OpWeaken alias strict fail-fast test

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Reproduce and capture failures | completed | Initial failures reproduced from target list |
| 2. Trace strict replay data flow across modules | completed | Isolated no-replay interaction across WitnessNorm/Translate/Omega |
| 3. Identify minimal root-cause patch points | completed | Translate scheme-choice widening + Omega producer-trace weaken bridge |
| 4. Validate recommended edits against protected tests | completed | Ran full 11-test matrix; all pass |
| 5. Summarize line-level recommendations | completed | Recommendations prepared for user |

## Errors Encountered
| Timestamp | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-02-28 | `--match` with exact escaped lambda string matched 0 tests due pattern mismatch | 1 | Matched enclosing describe string instead; reproduced gate failure |
| 2026-02-28 | Concurrent `cabal test` runs collided on `package.conf.inplace already exists` | 1 | Re-ran tests sequentially |
| 2026-02-28 | `Omega.hs` failed to compile (`getCopyMapping` not in scope) when module was forced to rebuild | 1 | Import `getCopyMapping` from `MLF.Constraint.Presolution.Base` |
