# Task Plan: TMT Row Ordering of Transformations Thesis-Exact Agent-Team Plan

## Goal
Implement `docs/plans/2026-03-05-tmt-row-ordering-of-transformations-thesis-exact-agent-team-implementation-plan.md` with Team A-E ownership and Wave 0..4 sequencing, preserving thesis-faithful ordering semantics (§12.1.3, §15.2.1, Def. 15.2.10).

## Scope
- Implement production/test changes for presolution ordering and finalization stages.
- Maintain non-empty gate evidence for each required matcher.
- Update row/docs/ledgers only after all gates pass.
- Preserve unrelated working-tree changes.

## Current Phase
Wave 4 (complete)

## Wave Status
| Wave | Description | Status |
|---|---|---|
| 0 | Team A RED baseline (`row3 ordering thesis-exact guard`) | complete |
| 1 | Team B + Team C ordering core refactors (parallel) | complete |
| 2 | Team D finalization extraction/integration | complete |
| 3 | Team E verification gates | complete |
| 4 | Team E docs/ledger closeout | complete |

## Decisions Made
| Decision | Rationale |
|---|---|
| Execute plan in current workspace instead of creating a worktree | User explicitly required implementation in the current workspace. |
| Keep Wave 0 as strict RED with source-contract guards in `PipelineSpec` | Ensures ordering contract fails before production refactors. |
| Add semantic characterization in `UnificationClosureSpec` alongside source guards | Keeps Wave 0 from being purely string-based. |
| Verify subagent claims with local re-run of gate commands | Required by verification-before-completion discipline. |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| `cabal test` lock collision (`package.cache.lock` missing) when two matcher commands were launched in parallel | 1 | Re-ran matcher gates sequentially; no further collision. |
| Full gate regression after initial Wave 2 (`OperationOnLockedNode` in `make const`/`BUG-002-V1`, frozen parity drift) | 1 | Added BUG-2026-03-05-001, adjusted edge-loop weaken scheduling in `EdgeProcessing`, re-ran all required gates + full suite to green. |

## Wave 0 Evidence
- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 ordering thesis-exact guard"'`
- Result: `FAIL`
- Evidence: `2 examples, 2 failures` (non-empty matcher; fallback not needed).

## Wave 1 Evidence
- `Phase 4 thesis-exact unification closure`: `8 examples, 1 failure` (non-empty matcher; fallback not needed).
- `Translatable presolution`: `8 examples, 0 failures`.
- Failure was isolated to the newly-added semantic characterization example; row3 source-contract half was already progressing (`EdgeProcessing` guard passing after Team B changes).

## Wave 2 Evidence
- `row3 ordering thesis-exact guard`: `2 examples, 0 failures`.
- `Phase 4 thesis-exact unification closure`: `8 examples, 0 failures`.
- `Translatable presolution`: `8 examples, 0 failures`.
- Driver now uses explicit post-loop finalization stage and no longer performs global post-loop `flushPendingWeakens`.

## Wave 3 Evidence
- Required verification gates:
  - `row3 ordering thesis-exact guard`: `2 examples, 0 failures`
  - `Phase 4 thesis-exact unification closure`: `8 examples, 0 failures`
  - `Translatable presolution`: `8 examples, 0 failures`
  - `checked-authoritative`: `8 examples, 0 failures`
  - `Dual-path verification`: `4 examples, 0 failures`
- Initial final gate run surfaced regression:
  - `cabal build all && cabal test` failed (`938 examples, 3 failures`).
- Post-fix re-run:
  - `cabal build all && cabal test` PASS.
  - `cabal test mlf2-test --test-show-details=direct` PASS (`938 examples, 0 failures`).

## Wave 4 Closeout
- Updated:
  - `docs/notes/2026-02-27-transformation-mechanism-table.md` (Ordering row + evidence block)
  - `implementation_notes.md`
  - `CHANGELOG.md`
  - `TODO.md`
  - `Bugs.md` (BUG-2026-03-05-001 resolved entry)
  - this tracker set (`task_plan.md`, `findings.md`, `progress.md`)
- Final recommendation captured in closeout docs:
  - Ordering row remains `Thesis-exact = No` (loop-final weaken flush still not strict per-edge).

## Notes
- Preserve ownership boundaries:
  - Team A: tests only.
  - Team B: `EdgeProcessing.hs` only.
  - Team C: `EdgeUnify.hs` only.
  - Team D: driver/materialization/validation/finalization.
  - Team E: gates + docs/ledgers.
- Re-read this file before each wave transition.

## 2026-03-08 tracker cleanup update
- Closure reason: Completed planning/execution tracker; all waves are complete and the corresponding TODO work is already marked completed.
- Status: ready for archive.
