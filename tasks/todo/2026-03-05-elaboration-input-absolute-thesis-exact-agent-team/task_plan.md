# Task Plan: Elaboration Input Absolute Thesis-Exact (Agent Team)

## Metadata
- Date: 2026-03-05
- Repo: /Volumes/src/mlf4
- Source row: docs/notes/2026-02-27-transformation-mechanism-table.md (Elaboration input)
- Objective: Write an execution-ready agent-team implementation plan to make Elaboration input closer to absolute thesis-exactness under strict all-path criteria.

## Phase Status
| Phase | Description | Status |
|---|---|---|
| 0 | Initialize task tracking files | completed |
| 1 | Re-audit row + thesis/code evidence for remaining gaps | completed |
| 2 | Draft agent-team implementation plan in docs/plans | completed |
| 3 | Sync rolling TODO priorities | completed |
| 4 | Finalize planning artifact and report to user | completed |

## Ownership Matrix (Planned Execution)
- Team A (`guards-red`): strengthen RED->GREEN guardrails for absolute row-1 contract
- Team B (`phi-env-chi-only`): remove residual solved-handle surfaces in Phi Env/Facade/Translate
- Team C (`scope-strictness`): remove error-swallowing fallback in ga' scope selection path
- Team D (`trace-fixture-hardening`): retire synthetic auto-trace helper usage in test-only Phi path
- Team E (`verification-docs`): integration gates + docs and ledger closeout

## Gate Checklist (for execution phase)
- [ ] Gate A RED: new absolute row-1 guard fails before implementation
- [ ] Gate B GREEN: row-1 guard passes after Teams B/C/D
- [ ] Gate C1: checked-authoritative slice
- [ ] Gate C2: Dual-path verification slice
- [ ] Final Gate: cabal build all && cabal test

## Decisions
- Keep scope focused on Elaboration input row; no unrelated runtime behavior refactors.
- Use agent-team waves with disjoint file ownership to allow parallel execution.

## Errors Encountered
| Timestamp | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-05 | None | 0 | N/A |
