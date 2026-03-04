# Task Plan: Row3 Ordering Absolute Thesis-Exact Agent-Team Execution

## Goal
Execute `/Volumes/src/mlf4/docs/plans/2026-03-05-tmt-row3-ordering-absolute-thesis-exact-agent-team-implementation-plan.md` end-to-end so TMT row `Ordering of transformations` is as thesis-exact as possible under the strict criterion, while preserving regression/parity behavior.

## Scope
- Wave 0 RED contracts (Team A)
- Wave 1 owner-aware pending weaken model + scheduler (Team B + Team C in parallel)
- Wave 2 integration/regression hardening (Team D)
- Wave 3 sequential gate stack execution (Team E)
- Wave 4 docs/table/ledger closeout (Team E)

## Current Phase
Complete

## Wave Status
| Wave | Team(s) | Description | Status |
|---|---|---|---|
| 0 | A | RED contracts: `row3 absolute thesis-exact guard` + semantic characterization | complete |
| 1 | B + C (parallel) | Owner-aware pending weaken model + boundary scheduler | complete |
| 2 | D | Locked-node regression shield + finalization safety | complete |
| 3 | E | Required matcher/full gates (sequential) | complete |
| 4 | E | Transformation table/docs/ledger closeout | complete |

## Ownership Boundaries
- Team A: `test/PipelineSpec.hs`, `test/Presolution/UnificationClosureSpec.hs`, optional row3 ordering spec module wiring.
- Team B: `src/MLF/Constraint/Presolution/Base.hs`, `src/MLF/Constraint/Presolution/EdgeUnify.hs`, `src/MLF/Constraint/Presolution/StateAccess.hs` (optional `Ops.hs`).
- Team C: `src/MLF/Constraint/Presolution/EdgeProcessing.hs` (optional schedule helper module, optional `Acyclicity.hs`).
- Team D: `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Constraint/Presolution/EdgeUnify.hs`, targeted regressions/tests.
- Team E: verification commands and docs closeout (`docs/notes/...table.md`, `implementation_notes.md`, `CHANGELOG.md`, `TODO.md`, tracker files, `Bugs.md` if needed).

## Decisions Made
| Decision | Rationale |
|---|---|
| Execute the existing 2026-03-05 plan directly rather than rewriting it | User requested full plan execution with the same wave structure and gates. |
| Keep gate commands sequential | Avoid cabal race/state interference and follow explicit verification policy. |
| Preserve strict owner boundaries per wave | Prevent uncontrolled cross-wave edits and preserve auditability. |

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| Gate A RED (expected): strict row3 absolute matcher fails on loop-final flush and missing owner-aware scheduler markers | 1 | Accepted as required RED baseline prior to Wave 1 implementation |
| Gate 2 regression during Wave 3 (`pending weakens` remained after edge-loop boundary) | 1 | Root-caused owner-key mismatch between planner-owner boundary and pending-weaken owner buckets; scheduler now flushes all pending owner buckets at each owner boundary and re-validates boundary emptiness |

## Verification Checklist
- [x] Gate A RED: `row3 absolute thesis-exact guard` fails with non-empty output before Wave 1.
- [x] Gate B green: row3 absolute guard + Phase 4 closure + translatable presolution.
- [x] Gate C green: make const + BUG-002-V1 + frozen parity + checked-authoritative + dual-path verification.
- [x] Final gate green: `cabal build all && cabal test`.
- [x] Evidence counts recorded in tracker files/docs.

## Gate Evidence
- Gate A RED:
  - Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "row3 absolute thesis-exact guard"'`
  - Result: `4 examples, 4 failures` (expected RED, non-empty).
  - Failure signal: `EdgeProcessing` still uses loop-final `flushPendingWeakens` sequence and lacks owner-aware boundary scheduler markers.
- Gate B GREEN:
  - `--match "row3 absolute thesis-exact guard"` -> `4 examples, 0 failures`
  - `--match "Phase 4 thesis-exact unification closure"` -> `10 examples, 0 failures`
  - `--match "Translatable presolution"` -> `8 examples, 0 failures`
- Gate C GREEN:
  - `--match "generalizes reused constructors via make const"` -> `1 example, 0 failures`
  - `--match "BUG-002-V1"` -> `1 example, 0 failures`
  - `--match "Frozen parity artifact baseline"` -> `1 example, 0 failures`
  - `--match "checked-authoritative"` -> `8 examples, 0 failures`
  - `--match "Dual-path verification"` -> `4 examples, 0 failures`
- Final Gate GREEN:
  - `cabal build all && cabal test` -> PASS
  - `mlf2-test` log summary: `942 examples, 0 failures`

## Deliverables
- Runtime/code changes for row3 ordering behavior.
- Updated docs and table classification backed by gate evidence.
- Updated task tracker files and TODO.
- `Bugs.md` updated same iteration if any new bug is discovered.
