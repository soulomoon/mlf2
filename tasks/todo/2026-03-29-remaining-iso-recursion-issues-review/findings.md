# Findings

## Initial

- Follow-up review packet created for the two remaining issues named after rounds 148-150 of the iso-recursive gap-fix roadmap.

## Roadmap and Round Review

- The live roadmap family `2026-03-29-02-iso-recursive-inference-gap-fixes` records items 1-5 as done, then round 150 reframes the only remaining limitations as:
  - nested-forall-mediated recursive types where μ is absorbed during constraint solving; and
  - a non-local proxy pipeline-entrypoint `PhiTranslatabilityError`.
- Round 149 review explicitly says the nested-forall contrast is not a failed fallback opening: the constraint graph for that expression does not expose `TyMu` from `rootFinal`, so keeping `containsMu == False` is correct for that expression.
- Round 149 also explicitly says the non-local proxy fallback is already open at the result-type level, and the remaining failure is an independent elaboration/Phi-translation blocker.

## Live Code/Test Read

- Current docs now encode the two remaining limitations directly in `implementation_notes.md`.
- The nested-forall contrast still expects `containsMu == False` at the fallback level and `PhiTranslatabilityError` at pipeline entrypoints in `test/Research/P5ClearBoundarySpec.hs`.
- The direct non-local proxy expression now expects `containsMu == True` at the fallback level in `test/PipelineSpec.hs`, but the same expression still expects strict pipeline failure at elaboration entrypoints.
- The current non-local proxy blocker has at least two concrete fail-fast sites in production code:
  - `src/MLF/Reify/Type.hs`: `PhiTranslatabilityError` for `TyMu` without an authoritative binder child.
  - `src/MLF/Elab/Phi/Omega/Interpret.hs`: `PhiTranslatabilityError` for `OpRaise (non-spine): missing computation context`.
- This strongly suggests:
  - nested-forall μ absorption is not a “fix the bug” item unless the desired semantics change;
  - non-local proxy pipeline failure is a real, likely-fixable elaboration/Phi reconstruction gap.
