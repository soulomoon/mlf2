# Round 091 Implementation Notes

## Change Summary

- Cleared the exact item-`3` `Phase 6 (elaboration)` breakpoint for the frozen
  same-lane retained-child packet
  `ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))`
  without widening the live subject beyond edge `3`.
- Added the canonical item-`3` artifact at
  `docs/plans/2026-03-25-same-lane-retained-child-stable-visible-persistence-phase-6-elaboration-resolution.md`
  and recorded the exact frozen tuple, the exact edge-`3` authority, the
  bounded repair, and the verification evidence.
- Added `expInstantiateArgsToInstNoFallback` in
  `src/MLF/Elab/Legacy.hs` so Phase 6 can translate an exact
  `ExpInstantiate [NodeId 31]` payload through `PresolutionView` with no
  fallback behavior.
- Narrowed `src/MLF/Elab/Elaborate/Annotation.hs` so `reifyInst` uses that
  helper only when the current `phi` still carries placeholder instantiation
  authority and the exact edge already provides an expansion payload, keeping
  other routes fail-closed.
- Added one exact-edge regression in `test/ElaborationSpec.hs` and one exact
  public-pipeline regression in `test/PipelineSpec.hs`.
- Normalized `test/ElaborationSpec.hs` to import only
  `MLF.Constraint.Types`, satisfying the repository guard that forbids
  importing both `MLF.Constraint.Types` and `MLF.Constraint.Types.Graph` in
  the same module.

## Verification

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
  - Result: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
  - Result: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "BUG-2026-02-08-004 nested let + annotated lambda now fails fast"'`
  - Result: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "bounded aliasing (b ⩾ a) elaborates to ∀a. a -> a -> a in unchecked and checked pipelines"'`
  - Result: passed.
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  - Result: passed with `21 examples, 0 failures`.
- `cabal build all && cabal test`
  - Result: passed with `1143 examples, 0 failures`.
- `git diff --name-only -- src test src-public app mlf2.cabal`
  - Result: returned only
    `src/MLF/Elab/Elaborate/Annotation.hs`,
    `src/MLF/Elab/Legacy.hs`,
    `test/ElaborationSpec.hs`, and
    `test/PipelineSpec.hs`.
- `git diff --name-only -- orchestrator/state.json orchestrator/roadmap.md orchestrator/retry-subloop.md orchestrator/verification.md Bugs.md`
  - Result: reported only pre-existing controller-owned drift on
    `orchestrator/state.json`, `orchestrator/roadmap.md`,
    `orchestrator/retry-subloop.md`, and `orchestrator/verification.md`.
    `Bugs.md` remained untouched by this round.
