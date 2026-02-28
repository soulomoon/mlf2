# Findings - 2026-02-27 Task 5 re-review post-projection fix

## Code compliance checks

- `WitnessValidation` still defines `ReplayMapTargetOutsideReplayDomain` and enforces both replay-domain membership and `TyVar` target checks:
  - `src/MLF/Constraint/Presolution/WitnessValidation.hs:55`
  - `src/MLF/Constraint/Presolution/WitnessValidation.hs:97-103`
- `Driver.computePresolution` still performs replay-map codomain replay-domain and `TyVar` checks with `InternalError` failure:
  - `src/MLF/Constraint/Presolution/Driver.hs:185-206`
- Follow-up fix (`49eb83b`) moved witness validation to post-projection in `WitnessNorm` using `normalizeInstanceOpsCore` and `envPost`:
  - core-only normalization call: `src/MLF/Constraint/Presolution/WitnessNorm.hs:348`
  - post-projection validation call: `src/MLF/Constraint/Presolution/WitnessNorm.hs:405-416`
  - helper export and implementation: `src/MLF/Constraint/Presolution/WitnessCanon.hs:303-316`, `:396-401`
- Original Task 5 commit is present and message remains satisfied:
  - `0597e0e fix: enforce replay-domain membership at presolution validation boundaries`

## Test evidence

- Validation subset (`replay-map validation`) passed:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "replay-map validation"'`
  - Result: `3 examples, 0 failures`
- Validation subset (`Witness normalization invariants`) passed:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Witness normalization invariants"'`
  - Result: `10 examples, 0 failures`
- The plan’s combined match string with `|` executed zero examples under current Hspec matching behavior; explicit runs above provide the effective validation gate evidence.
