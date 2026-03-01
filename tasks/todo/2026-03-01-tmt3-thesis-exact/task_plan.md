# Task Plan: TMT3 Thesis-Exact Campaign (All Rows Aligned)

## Goal
Eliminate all `DEV-TMT-*` rows from `docs/notes/2026-02-27-transformation-mechanism-table.md` by implementing thesis-exact behavior and reclassifying all rows to `Aligned`.

## Source Plan
- Agent-Team Plan: Make Transformation Mechanism Table Fully Thesis-Exact (All Rows Aligned)

## Phases
| Phase | Status | Notes |
|---|---|---|
| Wave 0: contracts + baseline | completed | Campaign files created and baseline gates captured |
| Wave 1: Pod A + Pod C | in_progress | Preparing parallel worktrees/agents |
| Wave 2: Pod B + Pod D | pending | Omega semantics cleanup and elaboration boundary cleanup |
| Wave 3: Pod E docs closeout + final integration | pending | Reclassify all rows to Aligned and finalize ledger/docs |

## Decisions
- Completion policy: all rows aligned (no active DEV-TMT implementation-choice rows).
- Refactor policy: aggressive runtime cleanup.
- Integration policy: no direct pod -> master merges.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| `--match "Phi alignment|IdentityBridge|replay-map"` matched 0 examples | 1 | Re-ran with separate `--match` clauses; obtained `39 examples, 0 failures` |
