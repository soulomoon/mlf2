# Task Plan: Thesis-Exact Strict Replay Cutover (Fix-First)

## Goal
Execute `/Volumes/src/mlf4/docs/plans/2026-02-27-thesis-exact-strict-fix-first-replay-contract-hard-reject-plan.md` end-to-end as one strict atomic cutover: producer-only replay normalization contract, runtime hard-reject (no repair), strict OpRaise fail-fast restoration, docs sync, and full verification.

## Source Plan
- `/Volumes/src/mlf4/docs/plans/2026-02-27-thesis-exact-strict-fix-first-replay-contract-hard-reject-plan.md`

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Prepare isolated execution context | in_progress | Create worktree + baseline targeted test snapshot. |
| 2. Tighten strictness predicates first (red) | pending | Update 3 specs and capture expected red state. |
| 3. Add red regressions for producer/runtime contract | pending | Add Witness/Elaboration replay-map regressions and confirm red. |
| 4. Rebuild replay-map producer normalization | pending | Rewrite WitnessNorm normalization contract and move producer tests toward green. |
| 5. Tighten producer validation in WitnessValidation/Driver | pending | Enforce replay-domain codomain checks at normalization and presolution boundaries. |
| 6. Make Phi replay bridge validation-only pass-through | pending | Remove runtime repair, validate replay-domain membership, pass through unchanged. |
| 7. Restore strict OpRaise fail-fast for unresolved trace-source targets | pending | Reinstate strict trace-source branch, preserve non-trace no-op behavior. |
| 8. Update docs/tracker artifacts | pending | Sync implementation notes/changelog/todo/deviation text to strict policy. |
| 9. Run invariant search and targeted verification | pending | Enforce no fallback symbols + strict targeted suite pass. |
| 10. Full gate and final handoff checks | pending | `cabal build all && cabal test`, clean status, review-ready log trail. |

## Decisions
- Execute tasks in plan order with strict review gates per task (spec compliance first, then code quality).
- Keep behavior thesis-faithful and document any unavoidable deviation.
- Preserve existing unrelated workspace changes; do not revert user edits.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| None yet | - | - |
