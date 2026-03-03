# Task Plan: Thesis-Exact Strict Replay Cutover (Fix-First)

## Goal
Execute `/Volumes/src/mlf4/docs/plans/2026-02-27-thesis-exact-strict-fix-first-replay-contract-hard-reject-plan.md` end-to-end as one strict atomic cutover: producer-only replay normalization contract, runtime hard-reject (no repair), strict OpRaise fail-fast restoration, docs sync, and full verification.

## Source Plan
- `/Volumes/src/mlf4/docs/plans/2026-02-27-thesis-exact-strict-fix-first-replay-contract-hard-reject-plan.md`

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Prepare isolated execution context | completed | Worktree created at `/Volumes/src/mlf4-strict-replay-cutover`; baseline targeted slice run (matched 0 examples, 0 failures). |
| 2. Tighten strictness predicates first (red) | completed | Predicate narrowed in 3 specs via `fd8e5f4`; required command executed but matched 0 tests (plan expectation drift), explicit split probes matched 6 tests and all passed. |
| 3. Add red regressions for producer/runtime contract | completed | Red regressions landed via `2f50bb8` + test-hardening follow-up `a2bc3fa`; targeted focused runs confirm expected failures. |
| 4. Rebuild replay-map producer normalization | completed | Landed in `38f9bfc` with deterministic active-source -> replay-binder mapping and replay-map completeness fail-fast. |
| 5. Tighten producer validation in WitnessValidation/Driver | completed | Landed `0597e0e` + regression fix `49eb83b`; replay-domain checks enforced at post-projection producer boundary and driver boundary. |
| 6. Make Phi replay bridge validation-only pass-through | completed | Landed `9df5d34` + hardening `bb7d2c5` + test-fix `d110d0c`; runtime replay repair removed and strict pass-through validation enforced. |
| 7. Restore strict OpRaise fail-fast for unresolved trace-source targets | completed | Behavior in `8fad0bb`; direct regression coverage added in `6c00c8c`. |
| 8. Update docs/tracker artifacts | completed | Docs sync landed in `e134300`, wording polish in `6589e69`; spec and quality reviews approved. |
| 9. Run invariant search and targeted verification | completed | Invariant grep clean; explicit strict-slice execution currently fails (`172 examples, 25 failures`) when using real multi-`--match` selection. |
| 10. Full gate and final handoff checks | completed | Full gate currently fails (`889 examples, 97 failures`, dominant `ReplayMapIncomplete`); final git status/log captured. |

## Decisions
- Execute tasks in plan order with strict review gates per task (spec compliance first, then code quality).
- Keep behavior thesis-faithful and document any unavoidable deviation.
- Preserve existing unrelated workspace changes; do not revert user edits.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| Baseline targeted regex matched zero examples | 1 | Recorded baseline as-is and proceeded; subsequent task slices will use tighter explicit patterns from plan. |
| Task 2 combined `--match` pattern matched zero examples | 1 | Confirmed required command executed; ran explicit split `--match` probes to verify intended tests and recorded expectation drift in plan/progress. |
| Task 3 combined `--match` pattern matched zero examples | 1 | Confirmed required command executed; validated red state with focused `--match` runs (`replay-map`, `codomain`, `malformed`). |
| Task 4 combined `--match` pattern matched zero examples | 1 | Combined alternation pattern did not select tests; confirmed behavior with focused `--match` probes (`replay-map validation`, `normalization maps replay codomain`, `stale source binders`) all passing. |
| Task 5 initially introduced stale-source normalization regression | 1 | Detected by quality review; fixed by moving `validateNormalizedWitness` to post-projection env in WitnessNorm (`49eb83b`) and re-verifying focused subsets. |
| Task 6 mixed scheme-binder parsing under-approximated replay domain | 1 | Fixed by unioning parsed scheme binder IDs with `siSubst` key-space (`bb7d2c5`) and adding regression in `ElaborationSpec`. |
| Task 6 new regression test had permissive `_ -> pure ()` tail | 1 | Tightened to explicit `Left err -> expectationFailure` / `Right _ -> pure ()` in `d110d0c`. |
| Task 7 lacked direct OpRaise fail-fast regression coverage | 1 | Added deterministic `OpRaise` trace-source unresolved-target test in `ElaborationSpec` (`6c00c8c`). |
| Task 9 combined strict-slice `|` matcher produced false green | 1 | Exact command matched 0 examples; reran with explicit multi-`--match` selection and observed 25 real failures for triage. |
| Task 10 full gate remains red | 1 | `cabal build all && cabal test` => 889 examples, 97 failures (ReplayMapIncomplete cluster); captured log + state for handoff. |

## Follow-up (2026-02-28)
- Implemented review item: remove residual runtime replay-target repair in `computeTraceBinderReplayBridge` and enforce strict validation-only pass-through.
- Added dedicated regression for source-space identity replay target hard-fail.
- Verification status:
  - Focused replay strictness slices: green.
  - Full gate: red with 2 known failures (`BUG-2026-02-17-002`, explicit forall let-bound round-trip).

## Follow-up (2026-02-27, continued)
- Resolved remaining explicit-forall round-trip test mismatch in `test/ElaborationSpec.hs` by switching to alpha-equivalence after vacuous-top-forall stripping.
- Re-verified focused failing example and full gate.
- Current status: full validation green (`cabal build all && cabal test --test-show-details=direct` => 890 examples, 0 failures).

## Follow-up (2026-02-28, P1 Driver codomain guard removal)
- Implemented Task 5.2 strictness hardening in `MLF.Constraint.Presolution.Driver`:
  - extracted `validateReplayMapTraceContract` helper and exported it for deterministic unit tests,
  - removed conditional `not (IntSet.null replayBinderDomain)` guard; codomain rejection is now unconditional.
- Added deterministic Driver-boundary tests in `test/Presolution/WitnessSpec.hs`:
  - hard-reject when replay binder domain is empty,
  - pass when replay target is inside replay binder domain.
- Verification status:
  - focused Driver + replay-map validation slices: green,
  - full gate currently red (`892 examples, 115 failures`) with dominant `InternalError "edge replay-map codomain target outside replay binder domain"` from existing source-space replay targets.
