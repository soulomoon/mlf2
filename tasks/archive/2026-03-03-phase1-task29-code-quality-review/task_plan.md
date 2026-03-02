# Task Plan: Phase 1 Task 29 Code-Quality Review

## Goal
Review the scoped Phase 1 Task 29 changes for maintainability, correctness risk, API hygiene, and test quality.

## Scope Files
- src/MLF/Constraint/Presolution/View.hs
- src/MLF/Constraint/Presolution.hs
- src/MLF/Elab/Elaborate.hs
- src/MLF/Elab/Phi/Translate.hs
- src/MLF/Elab/Run/Generalize.hs
- src/MLF/Elab/Run/Pipeline.hs
- test/PipelineSpec.hs

## Phases
| Phase | Description | Status |
|---|---|---|
| 1 | Collect diffs and inspect current implementations | completed |
| 2 | Assess maintainability/correctness/API/test quality | completed |
| 3 | Produce severity-ranked findings with line refs | completed |
| 4 | Re-review fix iteration for previously reported issues | completed |

## Decisions
- Use direct file inspection with line numbers as source of truth.
- Focus on review findings; avoid speculative architecture rewrites.

## Errors Encountered
| Error | Attempt | Resolution |
|---|---|---|
| `zsh: no matches found: src/MLF/Constraint/Solved/*.hs` | 1 | Switched to direct-path `rg` query without glob. |
| `mlf2-test: unexpected argument 'elaboration'` from `--match` with spaces | 1 | Re-ran test with space-free match token (`--match solved-to-presolution`). |
