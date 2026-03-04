# Task Plan — 2026-03-04 Elaboration Input Thesis-Exact Agent-Team Plan

## Goal
Make TMT row `Elaboration input` thesis-exact by retiring solved-typed elaboration/Phi compatibility APIs (including test-only helper surfaces) while preserving checked-authoritative behavior and fail-fast Phi invariants.

## Plan Reference
- `docs/plans/2026-03-04-elab-input-thesis-exact-legacy-retirement-agent-team-implementation-plan.md`

## Phases
| Phase | Status | Notes |
|---|---|---|
| 0. Session catchup + baseline reconciliation | complete | Ran planning-with-files catchup, checked git status/diff, read task files, confirmed one pre-existing edit in TMT doc to preserve. |
| 1. Wave 0 Team A guard hardening + Gate A | complete | Added strict legacy API marker guards in `test/PipelineSpec.hs` + `test/ElaborationSpec.hs`; Gate A guard slice is RED (2 failures), and `checked-authoritative` slice is PASS (8 examples, 0 failures). |
| 2. Wave 1 Team B + Team C API retirement + Gate B | complete | Removed legacy solved-typed surfaces from `Elab.Elaborate`, `Elab.Pipeline`, `Phi.Translate`, and `Phi` facade; migrated `Phi.TestOnly` callback contracts to chi-native shape while preserving fail-fast no-trace behavior. |
| 3. Wave 2 Team D test migration + Gate C | complete | `test/ElaborationSpec.hs` migrated to chi-native callback callsites; removed retired `Elab.elaborate` usage; Gate C slices all PASS (`elab-input thesis-exact guard`, `checked-authoritative`, `Dual-path verification`). |
| 4. Wave 3 Team E closeout docs + final gate | complete | Ran all required closeout gates in the integrated workspace (PASS: `2/8/4` matcher examples + full gate `931 examples, 0 failures`), then updated TMT/docs/changelog/TODO/task logs and flipped row `Elaboration input` to `Thesis-exact = Yes`. |

## Decisions
- Treat the user-provided implementation plan as the approved design artifact and execute it directly via strict wave sequencing.
- Use agent teams with non-overlapping file ownership, preserving any unrelated existing edits.
- Preserve fail-fast missing-trace semantics; no compatibility reintroduction in active elaboration/Phi paths.

## Errors Encountered
| Time | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-04 | `cabal` writes to `~/.cache/cabal/logs/build.log` denied in sandbox, returning non-zero despite completed test output | 1 | Switched closeout gates to `HOME=/tmp/codex-home XDG_CACHE_HOME=/tmp/codex-cache` and reran required commands with clean exit codes. |
| 2026-03-04 | Team D worker returned infrastructure error (`HTTP 400 Bad Request` HTML payload) before applying edits | 1 | Re-dispatched Team D; second dispatch completed successfully and applied required migrations. |
