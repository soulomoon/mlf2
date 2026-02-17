# Task Plan: BUG-2026-02-16-001 Edge Planner Scheme Introducer Crash

## Goal
Close `BUG-2026-02-16-001` (and the mirrored `BUG-2026-02-16-002`) by finding and fixing the root cause of planner classification crashes with `InternalError "scheme introducer not found"`.

## Scope
- In scope:
  - planner-time scheme-owner resolution for TyExp-left edges
  - let-edge / ann-edge planner classification regressions
  - deterministic unit repros in `test/Presolution/EdgePlannerSpec.hs`
- Out of scope:
  - unrelated Phase-6 replay/Φ issues
  - broad refactors outside edge-planner path

## Phases
1. Root cause investigation (systematic-debugging Phase 1). (completed)
2. Pattern comparison against working paths (Phase 2). (completed)
3. Single-hypothesis test + failing-test lock (Phase 3/4-RED). (completed)
4. Minimal implementation + verification (Phase 4-GREEN). (completed)
5. Tracker/docs sync (`Bugs.md`, `TODO.md`, task files). (completed)

## Errors Encountered
| Error | Attempt | Resolution |
|---|---:|---|
| `cabal repl ... -e` parsing failed while probing bind paths (`target ‘qualified’ is not a module name`) | 1 | Switched to a temporary `runghc` script to inspect binding paths directly. |
| Parallel `cabal test` executions collided on `package.cache.lock` during verification | 1 | Re-ran repro commands sequentially; avoid parallel cabal builds against the same `dist-newstyle` store. |

## Initial Evidence
- Deterministic repros fail in current workspace:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Edge plan types/planner classification/threads let-edge flag into allowTrivial/" --seed 1481579064'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Edge plan types/planner classification/threads ann-edge flag into suppressWeaken/" --seed 1481579064'`
- Throw site:
  - `src/MLF/Constraint/Presolution/StateAccess.hs` (`findSchemeIntroducerM`)
- Planner call path:
  - `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs` calls `findSchemeIntroducerM` unconditionally for TyExp-left edges.

## Root Cause (Confirmed)
- Regression introduced when planner started resolving `eprSchemeOwnerGen` unconditionally from `rteBodyId`.
- In synthesized-wrapper fixtures, body nodes can legitimately lack a direct `GenRef` ancestor in sparse bind-parent maps while the wrapper itself is in a gen scope.
- Unconditional body-root lookup therefore throws before planner can classify let/ann flags.

## Implemented Fix
- `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs`
  - Added `resolveSchemeOwnerGen`:
    - frontend TyExp path stays strict (`findSchemeIntroducerM` on body root),
    - synthesized-wrapper path uses body-first lookup with wrapper-root fallback.
  - Added local `firstGenOnPath` helper based on `bindingPathToRootUnderM`.
- `test/Presolution/EdgePlannerSpec.hs`
  - Strengthened both bug repro tests to assert `eprSchemeOwnerGen == GenNodeId 0` in addition to flag threading.

## Verification
- PASS: `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Edge plan types/planner classification/threads let-edge flag into allowTrivial/" --seed 1481579064'`
- PASS: `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Edge plan types/planner classification/threads ann-edge flag into suppressWeaken/" --seed 1481579064'`
- PASS: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge plan types" --seed 1481579064'`
- PASS: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Edge interpreter" --seed 1481579064'`
