# Ranked Structure Cleanup Task Plan

## Goal
Implement the ranked code-structure cleanup plan while preserving thesis-exact behavior and keeping the public/runtime surfaces explicit.

## Scope
- Add low-risk guardrails and doc sync
- Add public-surface contract coverage and remove `MyLib`
- Quarantine testing-only façade exports
- Add dual-import boundary guardrails
- Thin selected orchestration hubs without algorithmic rewrites

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| 1. Initialize workspace and map concrete targets | complete | Created task files, mapped affected surfaces, and corrected the dual-import scan approach |
| 2. Guardrails + docs + public contract wave | complete | Synced guidance/docs, added module headers, added harness/public guard specs, and removed `MyLib` |
| 3. Internal boundary tightening wave | complete | Added `MLF.Constraint.Presolution.TestSupport`, narrowed the main facade, and added the dual-import guard |
| 4. Orchestration thinning wave | complete | Reused shared bind-parent rebuilding and extracted explicit pipeline assembly helpers |
| 5. Verification + closeout | complete | Built `mlf2-test`, ran focused guard slices, and passed `cabal build all && cabal test` |

## Decisions
- Follow the ranked plan in order, but batch the first low-risk waves together where validation is shared.
- Keep behavior unchanged; any discovered semantic bug becomes a normal bugfix boundary rather than an opportunistic refactor.
- Treat root-level planning files as historical artifacts in docs unless a safe relocation path becomes clearly mechanical.

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| Exact-match dual-import scan initially treated `MLF.Constraint.Types.Graph` as `MLF.Constraint.Types` and overstated offenders | 1 | Replaced the regex heuristic with exact parsed import matching before adding the guard. |
