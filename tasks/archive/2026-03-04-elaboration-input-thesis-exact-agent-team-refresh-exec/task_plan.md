# Task Plan: Elaboration Input Thesis-Exact (Agent Team Refresh Execution)

## Metadata
- Date: 2026-03-04
- Repo: /Volumes/src/mlf4
- Source plan: docs/plans/2026-03-04-elaboration-input-thesis-exact-agent-team-plan-refresh.md
- Objective: Make TMT row `Elaboration input` thesis-exact under strict policy including test-only code paths.

## Phase Status
| Phase | Description | Status |
|---|---|---|
| 0 | Re-read source plan + initialize task tracking | completed |
| 1 | Wave 0: Team A guard hardening + Gate A RED proof | completed |
| 2 | Wave 1: Team B + Team C API/callsite migration | completed |
| 3 | Wave 2: Integration fixups + Team E verification gates | completed |
| 4 | Wave 3: Team D docs closeout and row flip (only after green) | completed |
| 5 | Final reporting (team summaries, command outputs, modified files) | completed |

## Ownership Matrix
- Team A (`guards`): `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`
- Team B (`phi-testonly-api`): `src/MLF/Elab/Phi/TestOnly.hs`
- Team C (`spec-migration`): `test/ElaborationSpec.hs`
- Team E (`verification`): gate command execution and evidence capture
- Team D (`docs-closeout`): `docs/notes/2026-02-27-transformation-mechanism-table.md`, `implementation_notes.md`, `CHANGELOG.md`, `TODO.md`

## Gate Checklist
- [x] Gate A: `elab-input thesis-exact guard` (expected RED after Team A)
- [x] Gate B: `elab-input thesis-exact guard` (expected GREEN after Team B+C)
- [x] Gate C1: `checked-authoritative`
- [x] Gate C2: `Dual-path verification`
- [x] Final Gate: `cabal build all && cabal test`

## Decisions
- Keep existing uncommitted changes in workspace untouched unless they overlap required files.
- Enforce strict file ownership and wave order through agent prompts.

## Errors Encountered
| Timestamp | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-04 | None observed during execution | 0 | N/A |
