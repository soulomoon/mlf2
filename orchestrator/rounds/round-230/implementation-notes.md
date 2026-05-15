### Changes Made
- `src/MLF/Binding/GraphOps.hs`: tightened the remaining owner-local type-only seam by requiring `NodeRefTag 'TypeTag` for `getBindFlag`, `isInstantiable`, and `isLocked`, and reused `fromNodeRefTag` in the typed raise/weaken helpers. Added an owner-local comment on `applyRaiseTo` documenting why its ancestor target intentionally stays on the mixed `NodeRef` seam.
- `test/GraphOpsSpec.hs`: updated direct GraphOps coverage to call the newly typed predicate/query APIs and added a focused `applyRaiseTo` regression proving the retained mixed ancestor target can still raise to the gen-root boundary.

### Tests
- `test/BindingSharedAbstractionSpec.hs`: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Binding shared abstractions"'` confirms the shared NodeRef helpers still cover both type and gen references correctly.
- `test/BindingSpec.hs`: `cabal test mlf2-test --test-show-details=direct --test-options='--match "harmonizeBindParentsWithTrace"'` and `--match "raiseToParentWithCount"` keep the existing mixed-seam adjustment evidence green.
- `test/GraphOpsSpec.hs`: `cabal test mlf2-test --test-show-details=direct --test-options='--match "applyRaiseTo"'` verifies the typed child API plus the retained mixed ancestor target in GraphOps.
- `repo gate`: `cabal build mlf2-test`, `git diff --check`, and `cabal build all && cabal test` all passed.

### Notes
Audit result for the scoped milestone-1 owners: no `expectTypeRef` / `expectGenRef` / `requireTypeRef` / `requireGenRef` helpers remain. The accepted mixed `NodeRef` seam is now limited to mixed binding-tree storage and ancestor-target boundaries such as `applyRaiseTo` / `raiseToParentWithCount`; the GraphOps predicate/query surface was the only residual type-only gap in scope and is now typed.

`docs/architecture.md` already described `NodeRef` as the mixed key and `NodeRefTag` as the type-only boundary accurately, so no wording change was needed in this round.

The full build/test gate passed with the repo's existing warning baseline in unrelated files; this round did not add new warning classes.
