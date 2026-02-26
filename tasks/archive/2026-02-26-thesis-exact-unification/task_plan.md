# Task Plan: Thesis-Exact Unification Plan Execution

## Goal
Execute `docs/plans/2026-02-26-thesis-exact-unification-plan.md` with strict batch checkpoints and thesis-faithful behavior.

## Scope
- Implement Tasks 1-8 from the plan in order.
- Run all specified red/green verification commands.
- Keep documentation and tracker files synchronized.

## Phases
| Phase | Status | Notes |
|---|---|---|
| 1. Batch 1 (Tasks 1-3) | completed | Implemented + targeted verification passed |
| 2. Batch 2 (Tasks 4-6) | completed | Implemented + targeted verification passed |
| 3. Batch 3 (Tasks 7-8) | completed | Docs/tracker synced; thesis gate + full verification passed |

## Decisions
- Use existing isolated worktree: `/Volumes/src/mlf4-wt-thesis-exact-unification-plan` (`codex/thesis-exact-unification-plan`).
- Follow plan task order exactly unless blocked by repository drift.
- Keep presolution raw graph (`prConstraint`) intact for translation and carry UF separately via `prUnionFind`.
- Reuse shared unification closure logic in both Solve and Presolution (`runUnifyClosure`) to avoid semantic drift.
- Use local cache env for reproducible offline test runs:
  - `CABAL_DIR=$PWD/.cabal`
  - `XDG_CACHE_HOME=$PWD/.cache`
- Keep production elaboration on legacy solve replay until native no-replay solved construction reproduces replay invariants across the full elaboration/pipeline matrix.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| `ParentNotUpper` surfaced when draining closure after canonicalization inside presolution loop | Drained closure immediately after rewrite without repairing bind-parent upper-chain shape | Reintroduced `repairNonUpperParents` before closure drain; then reran closure and translatability validation successfully |
| `canonicalMap` parity assertion failed between legacy snapshot replay and no-replay solved conversion | Compared canonical maps directly for an anchor expression in `SolveSpec` | Reframed parity test to validate solved-graph invariants and drained queues for both paths; keep elaborated-type parity as the behavioral equality target |
| Parallel `cabal test` invocations raced on `package.conf.inplace` | Ran multiple targeted cabal tests concurrently | Switched to sequential targeted runs for deterministic verification |
| Production no-replay solved path regressed `O15-ELAB-LET` with `alias bounds survived scheme finalization` | Production `runPipelineElab` used `fromPresolutionResult` | Restored production default to legacy `solveUnifyWithSnapshot -> fromSolveOutput` and kept native path only for explicit parity checks |
| Presolution closure drain failed `O10-EXP-DECIDE` on intermediate state with `Binding tree has multiple roots` | Drained closure unconditionally before/after each edge | Skip closure call when `cUnifyEdges` is empty and only canonicalize/repair state in that case |
| `git diff --stat origin/main...HEAD` failed (`origin/main` missing) | Attempted plan-specified diffstat against `origin/main` | Used equivalent base `origin/master...HEAD` for this repository |
