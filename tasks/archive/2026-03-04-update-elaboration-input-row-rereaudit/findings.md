# Findings — 2026-03-04 Elaboration Input Row Re-audit

## Scope
- Target document: `docs/notes/2026-02-27-transformation-mechanism-table.md`
- Target row: `Elaboration input`

## Thesis evidence
- `papers/these-finale-english.txt:14087-14097` defines `T(e)` from a chosen propagation witness `I` and states witness choice is not deterministic (pick any witness).
- `papers/these-finale-english.txt:14112-14117` states elaboration over translatable presolution `χp` is inductive on term shape and defined in Fig. 15.3.5.
- `papers/these-finale-english.txt:14150-14174` details that each translated instantiation edge uses the corresponding computation translation (`T(e)`), with quantifier reordering.

## Code evidence
- Active production path is `χp`-native at pipeline/elab entry:
  - `src/MLF/Elab/Run/Pipeline.hs:109-141` builds `presolutionViewForGen`, `generalizeAtWithView`, and `ElabEnv { eePresolutionView = presolutionViewForGen }`.
  - `src/MLF/Elab/Elaborate.hs:181-205` consumes `eePresolutionView` directly.
  - `src/MLF/Elab/Elaborate.hs:917-949` calls `phiFromEdgeWitnessWithTrace ... presolutionView ...`.
  - `src/MLF/Elab/Phi/Translate.hs:284-293` active Phi entrypoint uses `GeneralizeAtWith` + `PresolutionView`.
- Legacy solved-typed APIs still exist (including test/debug):
  - `src/MLF/Elab/Elaborate.hs:70-75` defines `GeneralizeAtWithLegacy`; `:110-179` keeps solved-typed compatibility entrypoints (`elaborate*`).
  - `src/MLF/Elab/Phi/Translate.hs:253-280` keeps `GeneralizeAtWithLegacy`, `phiFromEdgeWitnessNoTrace`, and deprecated `phiFromEdgeWitness`.
  - `src/MLF/Elab/Phi.hs:15-18` re-exports legacy no-trace/deprecated helpers.
  - `src/MLF/Elab/Phi/TestOnly.hs:46-63` keeps solved-typed callback shape for auto-trace tests.

## Conclusion for row status
- Under the table's current criterion (`Note: thesis exact include test-only code paths`), `Elaboration input` remains `Thesis-exact = No` until solved-typed legacy surfaces are removed.
