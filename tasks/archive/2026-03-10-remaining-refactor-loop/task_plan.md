# Remaining Refactor Loop Task Plan

## Goal
Execute the remaining refactor-loop campaign: hard public API cleanup, shared-structure prep, assembly prep, and the ordered monolith splits (`Omega`, `EdgeUnify`, `Reify.Core`, `Solve`, `Elaborate`) while preserving thesis-exact behavior.

## Scope
- Loop A: make `MLF.Pipeline` the canonical public execution surface and trim `MLF.API`
- Loop B: introduce `EdgeArtifacts`
- Loop C: finish residual `Run.Pipeline` / `Presolution.Driver` assembly cleanup
- Loops 1-5: split the remaining monoliths in the approved order
- Keep docs/tests/task logs synchronized throughout

## Phases
| Phase | Status | Notes |
| --- | --- | --- |
| 1. Initialize workspace + map edit surfaces | complete | Created task files and mapped public callers, shared-structure seams, and monolith owners |
| 2. Loop A public API hard cut | complete | `MLF.Pipeline` is canonical, `MLF.API` is frontend-only, and docs/tests/callers are rewired |
| 3. Loops B/C shared-structure + assembly prep | complete | Added the low-risk `EdgeArtifacts` path and explicit prep helpers in pipeline/driver |
| 4. Loop 1/2 monolith splits | complete | Split `Omega` and `EdgeUnify` into façade + child modules and kept guard coverage green |
| 5. Loop 3/4/5 monolith splits | complete | Split `Reify.Core`, `Solve`, and `Elaborate` into façade + child modules |
| 6. Verification + closeout | complete | Passed `cabal build all && cabal test` and synced docs/task notes |

## Decisions
- Follow the approved loop order exactly.
- Public API cleanup is intentionally aggressive: `MLF.Pipeline` becomes canonical and `MLF.API` loses execution/runtime exports.
- Each loop should preserve behavior first and use source/guard tests to lock boundaries before large moves.

## Errors Encountered
| Error | Attempt | Resolution |
| --- | --- | --- |
| Full `EdgeArtifacts` replacement of `PresolutionResult` / `PresolutionState` caused excessive pattern/update fallout | 1 | Narrowed Loop B to the high-value low-risk path: keep those record shapes stable and use `EdgeArtifacts` for shared plumbing only. |
