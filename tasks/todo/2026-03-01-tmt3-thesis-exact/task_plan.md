# Task Plan: TMT3 Thesis-Exact Campaign (All Rows Aligned)

## Goal
Eliminate all `DEV-TMT-*` rows from `docs/notes/2026-02-27-transformation-mechanism-table.md` by implementing thesis-exact behavior and reclassifying all rows to `Aligned`.

## Source Plan
- Agent-Team Plan: Make Transformation Mechanism Table Fully Thesis-Exact (All Rows Aligned)

## Wave Scope
- Wave 1: Pod A `phi-source-domain`, Pod C `solve-no-rewrite-layer`
- Wave 2: Pod B `omega-thesis-order`, Pod D `elab-direct-chi`
- Wave 3: Pod E `docs-closeout`

## Phases
| Phase | Status | Notes |
|---|---|---|
| Wave 0: contracts + baseline | completed | Campaign files created and baseline gates captured |
| Wave 1: Pod A + Pod C | completed | Integrated on `codex/tmt3-wave1-integration` |
| Wave 2: Pod B + Pod D | completed | Integrated on `codex/tmt3-wave2-integration` |
| Wave 3: Pod E docs closeout + final integration | in_progress | Reclassify rows to Aligned and finalize ledger/docs |

## Decisions
- Completion policy: all rows aligned (no active DEV-TMT implementation-choice rows).
- Refactor policy: aggressive runtime cleanup.
- Integration policy: no direct pod -> master merges.

## Pod D Wave 2 Addendum
### Goal
Deliver DEV-TMT-ELAB-SOLVED-PROJECTION, DEV-TMT-RESULT-TYPE-CONTEXT, and DEV-TMT-DUAL-PATH-GUARDRAIL outcomes under owned Pod D files.

### Status
1. [completed] Remove runtime `Solved.fromPresolutionResult` dependency in `MLF.Elab.Run` path.
2. [completed] Remove `ResultTypeContext` identifier from run-path result-type API (`ResultTypeInputs` replacement).
3. [completed] Retire active `DualPathSpec` wiring from test harness/cabal.
4. [completed] Run focused acceptance checks and prepare commit.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| `--match "Phi alignment|IdentityBridge|replay-map"` matched 0 examples | 1 | Re-ran with separate `--match` clauses; obtained passing targeted slice. |
| `tasks/todo/2026-03-01-tmt3-thesis-exact` missing in pod worktrees | 1 | Created folder and initialized `task_plan.md`, `findings.md`, and `progress.md`. |
| Add/add merge conflicts on task files during Wave 1 integration | 1 | Consolidated campaign + pod notes into merged task artifacts. |
| Content conflicts on task files during Wave 2 integration | 1 | Merged Wave 2 pod evidence into campaign-level artifacts without dropping prior history. |
