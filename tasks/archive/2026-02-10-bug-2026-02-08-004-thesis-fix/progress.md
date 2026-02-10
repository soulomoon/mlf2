# Progress Log — BUG-2026-02-08-004 Thesis-Exact Fix

## 2026-02-10

### Setup
- Initialized task folder and planning files.
- Loaded relevant process skills: brainstorming, systematic-debugging, test-driven-development, planning-with-files.

### Baseline / RED lock
- Updated `test/PipelineSpec.hs` BUG-2026-02-08-004 sentinel to thesis target:
  - unchecked pipeline returns `Int`
  - checked pipeline returns `Int`
- Command:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-08-004"'`
- Result:
  - RED as expected (`TCInstantiationError ... InstElim expects forall, got (Int -> Int) -> Int`).

### Iteration 1 (partial fix)
- Patched `src/MLF/Elab/Elaborate.hs` `AApp`:
  - guard witness-derived `InstApp` by function-term type (`TForall` required).
- Retest result:
  - still RED (`TCArgumentMismatch (Int -> Int) (a -> a)`).

### Iteration 2 (final fix)
- Extended `AApp` variable-argument instantiation inference:
  - derive parameter type from typechecked `fApp` arrow when available.
- Retest:
  - `BUG-2026-02-08-004` matcher PASS (`1 example, 0 failures`).

### Regression and full verification
- Commands:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-06-002"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "generalizes reused constructors via make const"'`
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "redirected let-use sites keep polymorphic schemes"'`
  - `cabal build all && cabal test`
- Results:
  - all targeted gates PASS.
  - full suite PASS (`604 examples, 0 failures`).

### Documentation / tracker sync
- Updated:
  - `docs/notes/2026-02-08-bug-2026-02-06-002-direction-matrix.md`
  - `TODO.md`
  - `implementation_notes.md`
  - `CHANGELOG.md`
  - `/Volumes/src/mlf4/Bugs.md` (BUG-2026-02-08-004 moved to Resolved)
