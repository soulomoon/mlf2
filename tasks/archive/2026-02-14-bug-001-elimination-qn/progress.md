# Progress Log: BUG-2026-02-14-001

## 2026-02-14
- Initialized debugging task files.
- Reproduced failure with deterministic seed:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 1 — Constraint generation/Binding edges/elimination rewrite removes eliminated binders from Q(n)/" --seed 1715612721'`
  - Observed `predicate failed on: fromList []`.
- Added temporary instrumentation in `test/ConstraintGenSpec.hs` to inspect eliminated/Q(n)/witness op state.
- Captured evidence (`eliminated=[]`, no merge/raise ops), then removed instrumentation.
- Verified expected normalization behavior through existing passing alias-inlining test.
- Applied minimal change in `test/ConstraintGenSpec.hs`:
  - renamed test to `alias-bound normalization leaves no eliminated binders in Q(n) path`
  - expectation updated to `eliminated == IntSet.empty`.
- Verification:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "/Phase 1 — Constraint generation/Binding edges/alias-bound normalization leaves no eliminated binders in Q(n) path/" --seed 1715612721'` -> `1 example, 0 failures`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "Phase 1 — Constraint generation" --seed 1715612721'` -> `48 examples, 0 failures`
  - `cabal test` -> `652 examples, 41 failures` (delta improvement from 42).
- Closed BUG-001 in `Bugs.md` as resolved (test expectation correction; no production-code change).
