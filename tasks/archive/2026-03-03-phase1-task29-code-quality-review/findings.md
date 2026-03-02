# Findings

## Review Notes
- Refactor correctly centralizes `Solved -> PresolutionView` construction into `fromSolved` (`src/MLF/Constraint/Presolution/View.hs`) and removes four local duplicate adapters.
- Runtime call sites in `Elaborate`, `Phi.Translate`, `Run.Generalize`, and `Run.Pipeline` now consistently use the shared adapter; no semantic change found in field mapping.
- `MLF.Constraint.Presolution` now exports `fromSolved`, which broadens the phase-4 entrypoint with a phase-5 (`Solved`) coupling; low immediate risk but increases API surface.
- New test in `test/PipelineSpec.hs` validates refactor via source-string matching (`isInfixOf` / `not . isInfixOf`) rather than behavior; this is brittle and can miss semantic regressions.

## Re-review After Fix Iteration
- `fromSolved` is now consumed from `MLF.Constraint.Presolution.View` at call sites, and `MLF.Constraint.Presolution` does not re-export it, reducing API coupling back to the boundary module.
- `test/PipelineSpec.hs` now includes a behavioral contract test (`shared solved-to-presolution adapter preserves solved query semantics`) that compares `fromSolved` view queries against `Solved` queries across a small corpus.
- Source-text assertions remain in the Integration Tests section for architectural guards; they are still brittle in isolation, but the adapter semantics are no longer covered only by text matching.
