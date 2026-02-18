# Task Plan: BUG-2026-02-17-002 root-cause fix

## Objective
Fix open bug `BUG-2026-02-17-002` where the applied bounded/coercion A6 path should typecheck to `Int` but currently fails with `TCLetTypeMismatch` in the unchecked pipeline.

## Phase Status
- [x] Phase 1: Root cause investigation
- [x] Phase 2: Pattern analysis
- [x] Phase 3: Hypothesis and minimal validation
- [x] Phase 4: Implement fix + regression + verification

## Constraints
- Follow thesis-faithful behavior (`papers/these-finale-english.txt`) and document deviations.
- Use systematic debugging workflow: no fixes before root cause is established.

## Error Log
- `cabal test` launched in parallel caused `package.conf.inplace` lock/cache races (`cannot create ... package.conf.inplace already exists`, missing lock path). Recovery: reran test commands sequentially.

## Execution Summary (2026-02-17)
- Root cause confirmed via traced elaboration:
  - let fallback shape classification missed `AAnn`-wrapped lambdas;
  - application recovery required variable arguments to select `InstApp`, so literal applications defaulted to bottomizing `InstElim`.
- Implemented fix in `/Volumes/src/mlf4/src/MLF/Elab/Elaborate.hs`:
  - recursive RHS-shape classification for annotated lambdas/apps;
  - coherent lambda fallback substitution/closure handling;
  - `AApp` recovery allows non-variable args to trigger `InstApp`.
- Regression updated:
  - converted sentinel to strict pass in `/Volumes/src/mlf4/test/PipelineSpec.hs` (`BUG-2026-02-17-002: ... elaborates to Int ...`).
- Tracking/docs synced:
  - `/Volumes/src/mlf4/Bugs.md` moved `BUG-2026-02-17-002` to resolved.
  - `/Volumes/src/mlf4/CHANGELOG.md`, `/Volumes/src/mlf4/TODO.md`, and `/Volumes/src/mlf4/implementation_notes.md` updated.

## Verification
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-17-002"'` -> PASS
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "A6 parity"'` -> PASS
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002 strict target matrix"'` -> PASS
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002 sentinel matrix"'` -> PASS
- `cabal build all && cabal test` -> PASS
