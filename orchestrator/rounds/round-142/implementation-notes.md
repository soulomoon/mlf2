# Round 142 — Implementation Notes (item-4)

## Scope completed

- Added a dedicated `describe "Automatic μ-introduction (item-4 edge cases)"` block in `test/PipelineSpec.hs`.
- Added five regression/characterization tests for:
  1. nested recursive lets,
  2. polymorphic recursion with annotation,
  3. μ/∀ interaction,
  4. higher-order recursion,
  5. already-annotated μ stability.

## Observed behavior pinned by tests

- **Nested recursive lets**: clear Phase 3; currently fail in Phase 4 with `WitnessNormalizationError`. Constraint after cycle-breaking contains `TyMu`.
- **Polymorphic recursion with annotation**: no Phase-3 cycle-regression; current path does **not** introduce `TyMu` in the broken-cycle constraint.
- **μ/∀ interaction**: clear Phase 3; broken-cycle constraint contains `TyMu`; currently fails closed in Phase 6 with `ValidationFailed ["alias bounds survived scheme finalization: ..."]`.
- **Higher-order recursion**: clear Phase 3; broken-cycle constraint does **not** introduce `TyMu`; currently fails closed in Phase 6 with the same alias-bound finalization class.
- **Already-annotated μ**: both authoritative entrypoints still succeed and preserve recursive shape (`containsMu`), with elaborated term accepted by `typeCheck`.

## Bug-fix contingency result

- No implementation-code changes were applied in `src/` for this round.
- Edge-case gaps were recorded as characterization tests to lock current behavior and prevent silent regressions.

## Verification

- `cabal test --test-show-details=direct --test-options='--match "Automatic μ-introduction (item-4 edge cases)"'`
- `cabal build all && cabal test`

Both passed in this worktree.
