# Task Plan: Elaboration Input Absolute Thesis-Exact (Agent Team)

## Metadata
- Date: 2026-03-05
- Repo: /Volumes/src/mlf4
- Source row: docs/notes/2026-02-27-transformation-mechanism-table.md (Elaboration input)
- Controller plan: /Volumes/src/mlf4/docs/plans/2026-03-05-elaboration-input-absolute-thesis-exact-agent-team-implementation-plan.md
- Objective: Execute strict-wave, strict-ownership agent-team implementation and close docs/ledger after all required gates pass.

## Phase Status
| Phase | Description | Status |
|---|---|---|
| 0 | Load workflow skills + validate workspace/task artifacts | completed |
| 1 | Wave 0: Team A RED guard baseline + commit | completed |
| 2 | Wave 1: Teams B/C/D parallel implementation + commits | completed |
| 3 | Wave 2: Team E integration + required gates | completed |
| 4 | Docs and ledger closeout (post-green only) | completed |
| 5 | Archive task folder + final controller report | completed |

## Ownership Matrix (Execution)
- Team A (`guards-red`)
  - Owns: `test/PipelineSpec.hs`
- Team B (`phi-env-chi-only`)
  - Owns: `src/MLF/Elab/Phi/Env.hs`, `src/MLF/Elab/Phi.hs`, `src/MLF/Elab/Phi/Translate.hs`
- Team C (`scope-strictness`)
  - Owns: `src/MLF/Elab/Run/Scope.hs`, `test/ScopeSpec.hs`, `test/Main.hs`, `mlf2.cabal`
- Team D (`trace-fixture-hardening`)
  - Owns: `src/MLF/Elab/Phi/TestOnly.hs`, `test/ElaborationSpec.hs`
- Team E (`verification-docs`)
  - Owns integration and gate fixes; may touch cross-team files only during Wave 2 conflict/fix integration.

## Gate Checklist
- [x] Gate A RED: `elab-input absolute thesis-exact guard` fails before Wave 1
- [x] Gate B GREEN: `elab-input absolute thesis-exact guard` passes after Wave 2 integration
- [x] Gate C1: `checked-authoritative` slice passes
- [x] Gate C2: `Dual-path verification` slice passes
- [x] Final Gate: `cabal build all && cabal test` passes

## Decisions
- Execute with subagent teams in strict wave order: A -> (B|C|D parallel) -> E.
- Keep strict file ownership in Wave 1; no edits outside ownership.
- Preserve unrelated workspace state; no destructive reset/revert operations.

## Errors Encountered
| Timestamp | Error | Attempt | Resolution |
|---|---|---|---|
| 2026-03-05 01:07 +0800 | None yet in execution phase | 0 | N/A |
| 2026-03-05 01:10 +0800 | `tr` unavailable during initial worktree branch-name generation command | 1 | Switched to fixed explicit branch names per team; no dependency on `tr`. |
| 2026-03-05 01:11 +0800 | `git` not found inside a zsh function-scoped bootstrap script | 2 | Replaced function wrapper with straight-line worktree creation commands; all 5 worktrees created successfully. |
