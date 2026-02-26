# Remove Legacy Replay Completely and Freeze Parity Artifacts

## Summary
Remove the internal legacy replay fallback (`runPipelineElabViaLegacySolve`) and replace live native-vs-legacy parity checks with a frozen JSON artifact baseline generated from the guarded-cutover green state.

## Success Criteria
1. Production elaboration no longer defines or exports legacy fallback entrypoints.
2. Live native-vs-legacy parity tests are removed.
3. Frozen baseline parity test is authoritative and green.
4. Verification matrix is green:
   - `./scripts/thesis-conformance-gate.sh`
   - `cabal build all && cabal test`

## Scope
- In scope:
  - Internal fallback removal from `MLF.Elab.Run.Pipeline` and re-exports
  - Frozen artifact generation infrastructure and baseline file
  - Parity test migration to frozen artifacts
  - Documentation/tracker updates
- Out of scope:
  - Removing low-level snapshot APIs (`solveUnifyWithSnapshot`, `fromSolveOutput`) still used by non-parity unit tests

## Public APIs / Interfaces / Types
- No `src-public` API shape changes.
- Internal API changes:
  - Removed `runPipelineElabViaLegacySolve` from internal pipeline modules.
- Test-only additions:
  - `Parity.FrozenArtifacts`
  - `FrozenParitySpec`
  - `frozen-parity-gen` executable
  - `scripts/update-frozen-parity-artifacts.sh`

## Implementation Phases

### Phase 1: Remove legacy fallback surfaces
- Delete legacy fallback definitions from:
  - `src/MLF/Elab/Run/Pipeline.hs`
- Delete legacy re-exports from:
  - `src/MLF/Elab/Run.hs`
  - `src/MLF/Elab/Pipeline.hs`
- Remove legacy helper utilities from:
  - `test/SpecUtil.hs`

### Phase 2: Introduce frozen artifact infrastructure
- Add deterministic artifact builder/renderer in:
  - `test/Parity/FrozenArtifacts.hs`
- Add baseline generator entrypoint in:
  - `test/Parity/FrozenParityGenMain.hs`
- Add regeneration script in:
  - `scripts/update-frozen-parity-artifacts.sh`
- Add checked-in baseline file:
  - `test/golden/legacy-replay-baseline-v1.json`

### Phase 3: Move parity checks to frozen baseline
- Add spec:
  - `test/FrozenParitySpec.hs`
- Register in harness:
  - `test/Main.hs`
  - `mlf2.cabal`
- Remove live parity checks from:
  - `test/PipelineSpec.hs`
  - `test/ElaborationSpec.hs`
  - `test/SolveSpec.hs`
  - `test/Constraint/SolvedSpec.hs`

### Phase 4: Documentation/tracker sync
- Record migration in:
  - `implementation_notes.md`
  - `TODO.md`
  - `CHANGELOG.md`
  - `docs/plans/2026-02-26-remove-legacy-replay-guarded-cutover.md`
  - `/Volumes/src/mlf4/Bugs.md`

### Phase 5: Verification matrix
1. `cabal test mlf2-test --test-show-details=direct --test-options='--match "Frozen parity artifact baseline"'`
2. `cabal test mlf2-test --test-show-details=direct --test-options='--match "O10-EXP-DECIDE"'`
3. `cabal test mlf2-test --test-show-details=direct --test-options='--match "O15-ELAB-LET"'`
4. `./scripts/thesis-conformance-gate.sh`
5. `cabal build all && cabal test`

## Assumptions
1. Frozen parity checks compare exact solved artifacts (`canonicalConstraint`, `originalConstraint`, `canonicalMap`) plus elaborated type snapshots.
2. Baseline metadata records generation date + source commit for traceability.
3. Snapshot APIs remain temporarily for low-level unit coverage, but no longer define production parity behavior.
