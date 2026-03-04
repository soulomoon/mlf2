# Task Plan: TMT Row2 Absolute Thesis-Exact (Agent Team)

## Metadata
- Date: 2026-03-05
- Repo: /Volumes/src/mlf4
- Source row: docs/notes/2026-02-27-transformation-mechanism-table.md (Result-type context wiring)
- Controller plan: /Volumes/src/mlf4/docs/plans/2026-03-05-tmt-row2-absolute-thesis-exact-agent-team-implementation-plan.md
- Objective: Plan and execute agent-team hardening so row2 is thesis-direct without ResultType-local solved-overlay materialization.

## Phase Status
| Phase | Description | Status |
|---|---|---|
| 0 | Re-audit thesis + row2 codebase surfaces | completed |
| 1 | Draft controller plan with team topology and gates | completed |
| 2 | Create task tracker artifacts under tasks/todo | completed |
| 3 | Agent-team execution (Wave 0..4) | pending |
| 4 | Post-green docs + ledger closeout | pending |
| 5 | Archive task folder | pending |

## Ownership Matrix (Execution)
- Team A (`contracts`)
  - Owns: `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`
- Team B (`view-core`)
  - Owns: `src/MLF/Elab/Run/ResultType/View.hs`, `src/MLF/Elab/Run/Scope.hs`
- Team C (`resulttype-consumers`)
  - Owns: `src/MLF/Elab/Run/ResultType/Ann.hs`, `src/MLF/Elab/Run/ResultType/Fallback.hs`, `src/MLF/Elab/Run/ResultType/Util.hs`
- Team D (`integration-wire`)
  - Owns: `src/MLF/Elab/Run/ResultType.hs`, `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Run/ResultType/Types.hs`
- Team E (`verifier-docs`)
  - Owns integration gate running + docs/task closeout.

## Gate Checklist
- [ ] Gate A RED: `row2 absolute thesis-exact guard` fails before Wave 1
- [ ] Gate B GREEN: `row2 absolute thesis-exact guard` passes after Wave 2
- [ ] Gate C1: `row2 closeout guard` passes
- [ ] Gate C2: `checked-authoritative` passes
- [ ] Gate C3: `Dual-path verification` passes
- [ ] Final Gate: `cabal build all && cabal test` passes

## Decisions
- Treat this as a strict all-path hardening task (same strictness style as Task 41).
- Keep algorithmic behavior unchanged; scope is dependency/wiring simplification toward thesis-direct surfaces.
- Require explicit non-empty gate evidence before any docs reclassification.

## Errors Encountered
| Timestamp | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-05 00:00 +0800 | None yet | 0 | N/A |
