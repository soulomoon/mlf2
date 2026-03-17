# Round 032 Implementation Notes

- Kept the production slice bounded to `src/MLF/Elab/Run/ResultType/Fallback.hs` by exposing `rootBindingIsLocalType` in the existing fallback debug trace after final scope resolution.
- Tightened `test/PipelineSpec.hs` so the bounded recursive-control and proxy-rejection examples both cover unchecked and checked pipeline entrypoints.
- Preserved the accepted `U4` refuted result as a fail-closed boundary; no recursive widening path, second interface, or compatibility fallback was introduced.
