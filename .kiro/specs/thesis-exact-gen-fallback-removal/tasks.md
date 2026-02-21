# Implementation Plan

- [ ] 1. Remove gen-ancestor fallback for type binders  **Deferred (2026-02-22):** Tracked as DEV-GEN-FALLBACK-PRESENT in docs/thesis-deviations.yaml.
  - [ ] 1.1 Update context computation to use only direct binders
    - Remove `bindersFromGen`/`closestGenAncestor` from `contextToNodeBoundWithOrderKeys`.
    - Ensure missing contexts return `Nothing` or a structured error.
    - Files: `src/MLF/Elab/Phi.hs`
    - Tests: updated context tests
    - _Requirements: 1.1, 1.2_
    - **Verification:** `rg -n "closestGenAncestor|bindersFromGen" src/MLF/Elab/Phi.hs`
  - [ ] 1.2 Remove reify binder fallback paths
    - Delete `closestGenAncestor`/`immediateGen` fallback in binder enumeration.
    - Keep binder ordering/dep logic intact with direct binders only.
    - Files: `src/MLF/Elab/Reify.hs`
    - Tests: compile + existing elaboration tests
    - _Requirements: 1.1_
    - **Verification:** `rg -n "closestGenAncestor|bindersBase|immediateGen" src/MLF/Elab/Reify.hs`

- [ ] 2. Make gen-node translation explicit (Q(g))  **Deferred (2026-02-22):** Tracked as DEV-GEN-FALLBACK-PRESENT in docs/thesis-deviations.yaml.
  - [ ] 2.1 Replace GenRef binder selection in generalize
    - Use `Binding.boundFlexChildrenUnder` on `GenRef` to compute Q(g).
    - Preserve ≺ ordering and bound dependency ordering as today.
    - Files: `src/MLF/Elab/Generalize.hs`
    - Tests: add gen-node translation test
    - _Requirements: 2.1, 2.2_
    - **Verification:** `rg -n "interiorOfUnder|GenRef" src/MLF/Elab/Generalize.hs`
  - [ ] 2.2 Add/route explicit gen-node translation entrypoint (if needed)
    - Introduce a helper that translates a gen node into an `ElabScheme` or
      `ElabType` using Q(g).
    - Use it in elaboration paths that currently depend on implicit fallback.
    - Files: `src/MLF/Elab/Reify.hs`, `src/MLF/Elab/Elaborate.hs` (if call-site changes needed)
    - Tests: gen-node translation regression
    - _Requirements: 2.1, 2.2_
    - **Verification:** `rg -n "GenRef|reify.*Gen" src/MLF/Elab`

- [ ] 3. Add invariant check for fallback-dependent graphs  **Deferred (2026-02-22):** Tracked as DEV-GEN-FALLBACK-PRESENT in docs/thesis-deviations.yaml.
  - [ ] 3.1 Implement validation helper
    - Add `checkNoGenFallback` (or equivalent) that detects type-node binder sets
      that would require gen fallback.
    - Emit a structured `BindingError`/`ElabError` with binder + gen ids.
    - Files: `src/MLF/Binding/Tree.hs`, `src/MLF/Constraint/Types.hs`
    - Tests: negative test exercising the invariant
    - _Requirements: 3.1, 3.2_
    - **Verification:** `rg -n "GenFallback|noGenFallback|fallback" src/MLF/Binding/Tree.hs`
  - [ ] 3.2 Wire validation into elaboration
    - Call the new check from `checkBindingTree` or before elaboration/Φ translation.
    - Files: `src/MLF/Binding/Tree.hs`, `src/MLF/Elab/Phi.hs`
    - Tests: negative test should fail with the new error
    - _Requirements: 3.1, 3.2_
    - **Verification:** `rg -n "checkNoGenFallback" src/MLF`

- [ ] 4. Update and extend tests  **Deferred (2026-02-22):** Tracked as DEV-GEN-FALLBACK-PRESENT in docs/thesis-deviations.yaml.
  - [ ] 4.1 Update context tests to bind via TyForall
    - Rewrite `contextToNodeBound` tests so binders attach to the `TyForall` node,
      not a gen node.
    - Files: `test/ElaborationSpec.hs`
    - _Requirements: 4.1_
    - **Verification:** `rg -n "contextToNodeBound" test/ElaborationSpec.hs`
  - [ ] 4.2 Add gen-node translation and invariant failure tests
    - Add a positive test showing Q(g) quantifiers in a scheme.
    - Add a negative test that triggers the new invariant error.
    - Files: `test/ElaborationSpec.hs` (or new `*Spec.hs`)
    - _Requirements: 4.2_
    - **Verification:** `cabal test --test-show-details=direct`
