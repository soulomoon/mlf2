# Remove Legacy Replay From Production Pipeline (Guarded Cutover)

## Status (2026-02-26)
- Guarded cutover phase is complete and verified green.
- Follow-up removal/frozen-baseline phase is tracked in:
  - `docs/plans/2026-02-26-remove-legacy-replay-frozen-artifacts.md`

## Summary
Migrate production elaboration from legacy solved replay (`solveUnifyWithSnapshot -> fromSolveOutput`) to presolution-native solved construction, while preserving behavior via a temporary dual-run parity guard.  
The cutover is **guarded** (as selected): production uses native solved artifacts, but still computes legacy solved artifacts in parallel during transition and fails fast on mismatch. After a green window, remove guard and legacy replay path.

## Success Criteria
1. `runPipelineElab` no longer depends on `solveUnifyWithSnapshot` for primary solved artifacts.
2. Native solved construction reproduces replay invariants on all current regression anchors (especially `O15-ELAB-LET` and `O10-EXP-DECIDE`).
3. Guarded dual-run parity check passes across full suite and thesis gate.
4. Final full verification is green:
   - `./scripts/thesis-conformance-gate.sh`
   - `cabal build all && cabal test`

## Scope
- In scope:
  - Solved construction semantics alignment (native == replay-finalized behavior)
  - Production pipeline cutover to native solved path
  - Temporary parity guard in production pipeline
  - Test and docs updates
- Out of scope (for this change):
  - Removing all legacy code immediately (that is Phase 2 cleanup after green window)

## Public APIs / Interfaces / Types
- **No public `src-public` API shape change**.
- Behavior change:
  - `runPipelineElab` (production default) will use native solved artifacts.
- Internal interface changes:
  - `fromPresolutionResult` in `MLF.Constraint.Solved` will be aligned to replay-finalized semantics (not lightweight no-replay finalization).
  - Temporary internal parity guard helper added in pipeline run module.
- Temporary compatibility:
  - `runPipelineElabViaLegacySolve` remains for explicit fallback/parity tests during transition.

## Implementation Plan

## Phase 1: Align Native Solved Semantics With Replay Finalization
### Files
- `src/MLF/Constraint/Solved.hs`
- (reuse logic from) `src/MLF/Constraint/Solve.hs`

### Changes
1. Change `fromPresolutionResult` to use the same finalization path as replay snapshot conversion (the same semantics used by `fromSolveOutput`).
2. Remove or quarantine `fromConstraintAndUfNoReplay` usage from production path.
3. Keep cycle-safe canonicalization behavior intact.
4. Ensure eliminated-binder and bind-parent postprocessing mirrors replay-finalized output.

### Rationale
The prior regression (`alias bounds survived scheme finalization`) indicates native path was missing replay-equivalent finalization steps.

## Phase 2: Switch Production Builder To Native + Add Guard
### Files
- `src/MLF/Elab/Run/Pipeline.hs`

### Changes
1. Add/activate native solved builder:
   - `buildSolvedNativeFromPresolution` -> `Solved.fromPresolutionResult`.
2. Change production default:
   - `runPipelineElabWith` uses native builder instead of `buildSolvedViaLegacySolve`.
3. Add temporary parity guard (always enabled for this migration phase):
   - After native solved construction, compute legacy solved via existing `buildSolvedViaLegacySolve`.
   - Compare:
     - `Solved.canonicalConstraint`
     - `Solved.originalConstraint`
     - `Solved.canonicalMap`
   - On mismatch, fail with `PipelineSolveError (ValidationFailed [...])` containing compact diff context.
4. Keep `runPipelineElabViaLegacySolve` unchanged as explicit fallback/testing path.

### Guard Policy
- Guard is temporary and mandatory during migration.
- It trades performance for confidence and catches non-obvious semantic drift immediately.

## Phase 3: Update Test Helpers To Follow Production Path
### Files
- `test/SpecUtil.hs`

### Changes
1. Make `runPipelineArtifactsDefault` and `runToSolvedDefault` use native solved construction.
2. Add explicit legacy helper variants for tests that need direct legacy path.
3. Ensure test helper naming clearly distinguishes default/native vs legacy.

## Phase 4: Strengthen Parity and Regression Coverage
### Files
- `test/PipelineSpec.hs`
- `test/SolveSpec.hs`
- `test/ElaborationSpec.hs`
- `test/Constraint/SolvedSpec.hs`
- `test/Presolution/UnificationClosureSpec.hs` (only if needed for regression anchoring)

### Changes
1. Replace "production uses legacy replay" expectation with:
   - production uses native solved construction
   - native output equals legacy output under guard baseline checks.
2. Expand parity anchors beyond single expression:
   - identity
   - application
   - `let id = \x.x in id`
   - polymorphic two-use let
   - coercion-heavy bounded alias anchor(s) currently in gate matrix
3. Add solved-construction parity tests at `Solved` layer:
   - `fromPresolutionResult` vs `fromSolveOutput` equivalence for canonical/original constraints.
4. Preserve explicit regression tests:
   - `O15-ELAB-LET`
   - `O10-EXP-DECIDE`

## Phase 5: Docs and Tracker Sync
### Files
- `implementation_notes.md`
- `docs/paper-map.md`
- `TODO.md`
- `/Volumes/src/mlf4/Bugs.md`
- `CHANGELOG.md`

### Changes
1. Record that production is now presolution-native with guarded legacy parity check.
2. Record retained temporary fallback path and guard-removal criteria.
3. Update bug tracker entries tied to this migration (resolved or transition state).

## Phase 6: Verification Matrix (Required)
Run sequentially (no parallel `cabal test` to avoid package-conf races):

1. Targeted:
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "O10-EXP-DECIDE"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "O15-ELAB-LET"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "matches legacy solve path"'`
   - `cabal test mlf2-test --test-show-details=direct --test-options='--match "presolution-native solved conversion"'`
2. Gate:
   - `./scripts/thesis-conformance-gate.sh`
3. Full:
   - `cabal build all && cabal test`

All must pass before considering cutover complete.

## Phase 7 (Follow-up Cleanup After Green Window)
### Trigger
- Two consecutive full green runs (including thesis gate) after cutover.

### Cleanup Changes
1. Remove temporary dual-run parity guard from production path.
2. Remove legacy replay builder from production code path entirely.
3. Optionally remove `runPipelineElabViaLegacySolve` if no longer required by tests/tooling.
4. Remove migration-only parity scaffolding that duplicates permanent invariants.

### Verification
Repeat full matrix after cleanup.

## Test Cases and Scenarios
- Core solved parity:
  - native vs legacy solved constraints equal
- Elaboration parity:
  - anchor expressions produce alpha-equivalent types
- Regression protection:
  - no reappearance of alias-bound finalization failure
  - no presolution closure intermediate-state root errors
- Integration:
  - production pipeline executes successfully with native solved path
  - guard does not report mismatches on existing suite

## Assumptions and Defaults
1. **Selected default:** Guarded cutover (temporary dual-run check) is required.
2. No public API shape changes are introduced in `src-public`.
3. Legacy fallback entrypoint is retained temporarily for safety and parity testing.
4. Guard mismatch is treated as hard failure (`PipelineSolveError`), not logging-only.
5. Sequential test execution is used for deterministic Cabal behavior.
