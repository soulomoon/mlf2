# Task Plan: Single-Solved Elaboration Input Implementation

## Goal
Execute `/Volumes/src/mlf4/docs/plans/2026-03-01-single-solved-elab-input-implementation-plan.md` end-to-end in this branch, strictly excluding phi replay bridge plan work.

## Scope Guardrails
- Follow AGENTS.md process and coding constraints.
- Keep changes strictly to the single-solved elab input plan.
- Do not implement phi replay bridge plan work.

## Phases
| Phase | Description | Status |
|---|---|---|
| 1 | Task 1 red-green + API collapse (`ElabEnv`/`elaborate`) | completed |
| 2 | Task 2 guard test + remove legacy `eeRes*` fields | completed |
| 3 | Task 3 ResultTypeContext single solved refactor | completed |
| 4 | Task 4 targeted regression locks + behavior stabilization | completed |
| 5 | Task 5 docs + final verification gate + cleanup | completed |

## Decisions
- Execute plan tasks in order with test-first red/green checks.
- Use targeted test slices during implementation and full gate at the end.
- Commit meaningful checkpoints per plan tasks.
- Single-solved pipeline wiring now uses `solvedForGen` as the authoritative solved snapshot to preserve established elaboration/generalization behavior.

## Errors Encountered
| When | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-01 | Parallel setup race while reading newly created task files | 1 | Re-ran creation/verification sequentially |
| 2026-03-01 | Focused Task 2 test pattern matched 0 examples (`eeRes\*`) | 1 | Re-ran with broader pattern (`single-solved migration removes eeRes`) |
| 2026-03-01 | Task 4 regression slice `Phase 6 — Elaborate` failed with 6 examples | 1 | Reproduced a minimal failing case; switched single solved snapshot from `solvedClean` to `solvedForGen`; reran slices to green |
