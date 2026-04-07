# Round 207 Implementation Notes

## Status

Completed inside the bounded milestone-2 slice. The selected same-wrapper
nested-`forall` fallback packet now carries its generalization scope together
with its chosen target, the focused slice is green, and the full
`cabal build all && cabal test` gate passed.

## Changes Made

- Reworked `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` inside
  `computeResultTypeFallbackCore` so the retained-child same-wrapper path no
  longer picks a target independently from its owning scope.
- Replaced the old ownerless recursive-target fallback with a single local
  proof that carries:
  the retained child,
  the selected child/body root,
  the chosen recursive target when one is proven, and
  the aligned scope root used by `generalizeWithPlan`.
- Kept `boundHasForallFrom` inside the same proof path by parameterizing it on
  the aligned scope rather than the old unconditional `scopeRoot`.
- Left `src/MLF/Elab/TermClosure.hs` untouched because the fallback rewrite
  turned the selected slice green; there was no truthful downstream blocker to
  justify a term-closure edit.
- Updated `test/PipelineSpec.hs` source-shape guards to assert the new
  proof-carrying `generalizeScopeRoot` path.
- Narrowed the `rtcBaseConstraint` refresh in `test/PipelineSpec.hs` back down
  to the selected nested-`forall` rewiring helper so neighboring preserved
  lanes keep their pre-existing local fallback behavior.
- Kept `test/Research/P5ClearBoundarySpec.hs` on the recursive internal-only
  expectation for the selected nested-`forall` packet while authoritative
  entrypoints remain fail-closed in this round.

## Verification Run

Focused commands run serially:

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback open for recursive types even when the same wrapper crosses a nested forall boundary"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "fail-closed once it leaves the local TypeRef lane"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "does not infer recursive shape for the corresponding unannotated variant"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the P5 guard cluster wired through boundHasForallFrom and authoritative preservation"'`

Bounded/full gates run after the focused slice turned green:

- `rg -n 'scopeRootPre|scopeRootPost|boundHasForallFrom|sameLaneLocalRetainedChildTarget|generalizeWithPlan' src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `git diff --check`
- `git diff --name-only -- src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs`
- `cabal build all`
- `cabal test`

## Notes

- `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Pipeline.hs`, and
  `src-public/MLF/Pipeline.hs` remain untouched.
- `src/MLF/Elab/Run/ResultType/Fallback.hs` did not need a mechanical facade
  change.
