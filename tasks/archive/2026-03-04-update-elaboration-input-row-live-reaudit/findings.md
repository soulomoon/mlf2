# Findings — 2026-03-04 Elaboration Input Live Re-audit

## Target
- `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Row: `Elaboration input`

## Evidence Notes
- Pending code + thesis confirmation.

- Thesis evidence (`papers/these-finale-english.txt`):
  - Def. 15.3.12 defines translation of an instantiation edge `T(e)` from a propagation witness over `χp`, with explicit non-deterministic witness choice ("pick any propagation witness") (`:14087-14097`).
  - §15.3.6 states elaboration of `χp` is inductive on term shape and anchored in Fig. 15.3.5 (`:14112-14117`).
- Code evidence (active/runtime path):
  - Pipeline computes `presolutionViewForGen`, uses `generalizeAtWithBuilderView`, and passes `eePresolutionView` into `elaborateWithEnv` (`src/MLF/Elab/Run/Pipeline.hs:110-141`).
  - `ElabEnv` stores `eePresolutionView :: PresolutionView`; elaboration consumes it and calls `phiFromEdgeWitnessWithTrace` (`src/MLF/Elab/Elaborate.hs:69-76`, `:97-114`, `:864`).
  - Φ translation entrypoint requires `PresolutionView` + `Maybe EdgeTrace`, and fails fast with `MissingEdgeTrace` when absent (`src/MLF/Elab/Phi/Translate.hs:247-260`).
- Remaining non-thesis-exact surfaces under strict criterion (includes test-only paths):
  - `MLF.Elab.Phi.TestOnly` still exposes solved-typed helpers `phiFromEdgeWitnessNoTrace`, alias `phiFromEdgeWitness`, and `phiFromEdgeWitnessAutoTrace :: ... -> Solved -> ...`; auto-trace bridges via `fromSolved solved` (`src/MLF/Elab/Phi/TestOnly.hs:49-87`).
  - Tests still call solved-typed helpers (`test/ElaborationSpec.hs:1521`, `:1917-1919`).
- Conclusion for row classification under current table note (`thesis exact include test-only code paths`): still `Thesis-exact = No`.
