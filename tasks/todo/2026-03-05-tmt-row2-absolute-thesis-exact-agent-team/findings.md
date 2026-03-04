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
