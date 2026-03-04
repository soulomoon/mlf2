# Findings: TMT Row2 Absolute Thesis-Exact Planning

## 2026-03-05 Audit Findings
- Thesis-side result/subterm type construction is defined from translatable-presolution artifacts (`Def. 15.3.2`, `§15.3.2.2`, `§15.3.6`) and does not require a dedicated runtime solved-overlay object at result-type wiring.
- Current row2 table entry is classified `Yes` for adapter retirement, but its own follow-up note still calls out residual `ResultType.View` solved-overlay simplification work.

## Concrete Residual Surfaces in Code
- `src/MLF/Elab/Run/ResultType/View.hs` still materializes/rebuilds `Solved` via:
  - `solveFromInputs`
  - `rtvSolved`
  - `rtvOriginalConstraint`
  - `Solved.rebuildWithConstraint`
- `src/MLF/Elab/Run/ResultType/Ann.hs` and `src/MLF/Elab/Run/ResultType/Fallback.hs` still consume `View.rtvSolved` and solved-based scope/target calls.
- `src/MLF/Elab/Run/ResultType/Util.hs` still threads `Solved` through `generalizeWithPlanView` for fallback reify.

## Planning Direction
- Add an explicit RED->GREEN `row2 absolute thesis-exact guard`.
- Retire local solved-overlay from ResultType view layer first, then migrate consumers to `PresolutionView`/`ChiQuery`/view helpers.
- Preserve behavior with mandatory guard slices:
  - `row2 closeout guard`
  - `checked-authoritative`
  - `Dual-path verification`
  - full gate.

## Risks
- Malformed-presolution failure shape currently piggybacks on solved strict validation; removing local solved materialization must preserve equivalent fast-fail behavior and test expectations.
- Ann/Fallback paths are dense and sensitive to scope resolution edge-cases; migration must be dependency-only, not semantic.

## 2026-03-05 Wave 0 Findings
- Added `row2 absolute thesis-exact guard` under `Pipeline (Phases 1-5) / Integration Tests` in `test/PipelineSpec.hs`.
- Gate A evidence (RED baseline):
  - `cabal test ... --match "row2 absolute thesis-exact guard"` => `1 example, 1 failure` (expected).
- Baseline parity characterization preserved:
  - `cabal test ... --match "checked-authoritative"` => `8 examples, 0 failures`.

## 2026-03-05 Wave 1 Findings
- `ResultType.View` no longer exposes or materializes row2-local solved overlay surfaces (`rtvSolved`, `rtvOriginalConstraint`, `solveFromInputs`).
- Consumer migration (`Ann`, `Fallback`, `Util`) now uses view-first helpers (`resolveCanonicalScopeView`, `canonicalizeScopeRefView`, `schemeBodyTargetView`) and view-based reify/generalize paths.
- Focused post-migration evidence:
  - `row2 absolute thesis-exact guard` => `1 example, 0 failures`
  - `row2 closeout guard` => `3 examples, 0 failures`
  - `result-type reconstruction fails on malformed PresolutionView materialization` => `1 example, 0 failures`
  - `ResultType|Phase 6 — Elaborate|chi-first gate stays green` => `1 example, 0 failures`

## 2026-03-05 Wave 2 Findings
- Integration wiring owned-file pass required only import/signature cleanup in:
  - `src/MLF/Elab/Run/ResultType.hs`
  - `src/MLF/Elab/Run/Pipeline.hs`
  - `src/MLF/Elab/Run/ResultType/Types.hs`
- Integration slices remained green post-cleanup:
  - `Pipeline (Phases 1-5)` => `71 examples, 0 failures`
  - `Dual-path verification` => `4 examples, 0 failures`

## 2026-03-05 Wave 3 Findings
- Gate B GREEN evidence:
  - `row2 absolute thesis-exact guard` => `1 example, 0 failures`
  - `row2 closeout guard` => `3 examples, 0 failures`
- Gate C evidence:
  - `checked-authoritative` => `8 examples, 0 failures`
  - `Dual-path verification` => `4 examples, 0 failures`
- Final gate evidence:
  - `cabal build all && cabal test` => PASS (test log: `935 examples, 0 failures`).
- Matcher fallback execution was not needed (all required matchers returned non-zero examples).

## 2026-03-05 Wave 4 Findings
- Row2 TMT entry is now documented as absolute thesis-exact and no longer tracks local solved-overlay follow-up.
- Repo docs/ledger updated post-green:
  - `docs/notes/2026-02-27-transformation-mechanism-table.md`
  - `implementation_notes.md`
  - `CHANGELOG.md`
  - `TODO.md`
- No new runtime defects were discovered; `Bugs.md` remains unchanged.
