# Task Plan: OpWeaken Fallback Removal

## Context
- Date: 2026-02-26
- Plan source: `docs/plans/2026-02-26-opweaken-fallback-removal-plan.md`
- Goal: Remove non-root `OpWeaken` no-op fallbacks and enforce fail-fast or `InstElim`.

## Phases
- [x] Task 1: Pin current fallback behavior with red tests
- [x] Task 2: Replace fallback no-op paths with strict erroring in Omega
- [x] Task 3: Make error classification consistent and reviewable
- [x] Task 4: Validate no silent OpWeaken skip remains in Omega
- [x] Task 5: Full regression verification
- [x] Task 6: Documentation and tracker sync
- [x] Task 7: Final quality check and handoff

## Decisions
- Red tests assert `PhiTranslatabilityError` for unresolved non-root `OpWeaken`.
- Task 2 implementation uses strict `PhiTranslatabilityError` for both unresolved fallback sites.
- Legacy fallback-dependent regression suites were rebaselined to assert strict fail-fast (`OpWeaken: unresolved non-root binder target`) instead of permissive success.

## Error Log
- Cabal package DB race when running `cabal build` and `cabal test` in parallel; resolved by rerunning verification sequentially.
- Full-suite verification initially failed in 31 legacy fallback-dependent tests; resolved by updating those test expectations to the strict fail-fast contract.
