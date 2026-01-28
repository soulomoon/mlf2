# Implementation Plan

> Note: For per-task verification, prefer `cabal test --test-show-details=direct --test-options='--match <pattern>'`.
> If `cabal test` cannot write its default build logs on your machine, prefix with `cabal --config-file=.cabal-config`.

- [x] 1. Remove the silent “no LCA ⇒ no-op” fallback
  - [x] 1.1 Add a structured “no binding LCA” error
    - Files: `src/MLF/Constraint/Types.hs`, `src/MLF/Binding/Tree.hs`
    - Steps:
      - Use `NoCommonAncestor NodeRef NodeRef` in `BindingError`.
      - Update `Binding.bindingLCA` to return `Left (NoCommonAncestor n1 n2)` when no common ancestor exists.
    - **Verification:** `rg -n "No common ancestor" src/MLF/Binding/Tree.hs` returns no matches
    - _Requirements: 1.1_
  - [x] 1.2 Remove the silent no-op fallback from binding-edge harmonization
    - Files: `src/MLF/Binding/Adjustment.hs`
    - Steps:
      - Update `harmonizeBindParentsWithTrace` to propagate `NoCommonAncestor` (no `(c0, [])` success path).
      - Update `harmonizeBindParents` to remove the special-case no-op behavior for missing LCAs.
    - **Verification:** `rg -n "No common ancestor" src/MLF/Binding/Adjustment.hs` returns no matches
    - _Requirements: 1.2, 1.3_
  - [x] 1.3 Update the unit test that asserted the old fallback
    - Files: `test/BindingSpec.hs`
    - Steps:
      - Update the “fails when no binding LCA exists” example to assert `Left (NoCommonAncestor …)`.
    - **Verification:** `cabal test --test-show-details=direct --test-options='--match /harmonizeBindParentsWithTrace.*no binding LCA/'` passes
    - _Requirements: 2.1_

- [x] 2. Restore the paper’s rooted-constraint assumption with a root gen node
  - [x] 2.1 Ensure root gen node exists in constraint generation
    - Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`, `src/MLF/Frontend/ConstraintGen/Emit.hs`
    - Steps:
      - Allocate a root gen node and bind term-DAG roots under it (flex).
      - Record term-DAG roots as schemes in the root gen node.
    - **Verification:** `rg -n "allocGenNode|setGenNodeSchemes" src/MLF/Frontend/ConstraintGen` returns matches
    - _Requirements: 3.1, 3.2_
  - [x] 2.2 Preserve the root gen node across rewrites
    - Files: `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Constraint/Solve.hs`
    - Steps:
      - Ensure `cGenNodes` is preserved through canonicalization and elimination rewrites.
      - Fail fast if `Binding.checkBindingTree` reports missing or multiple roots.
    - **Verification:** `rg -n "checkBindingTree" src/MLF/Constraint/Presolution/Driver.hs src/MLF/Constraint/Solve.hs` returns matches
    - _Requirements: 3.1, 3.3_
  - [x] 2.3 Tests: root gen node makes LCA total for disconnected components
    - Files: `test/BindingSpec.hs`
    - Steps:
      - Build a constraint with two term-DAG roots and no shared binding ancestor.
      - Assert `Binding.bindingLCA` fails before rooting and succeeds once both roots are bound under the gen root.
    - **Verification:** `cabal test --test-show-details=direct --test-options='--match /bindingLCA/'` passes
    - _Requirements: 3.1, 3.2_

- [x] 3. Remove synthetic root type-node handling across the pipeline
  - [x] 3.1 Drop the synthetic root type node from the `TyNode` data model
    - Files: `src/MLF/Constraint/Types.hs`
    - Steps:
      - Remove the synthetic root type-node constructor and associated traversal cases.
    - **Verification:** `rg -n "root type" src/MLF/Constraint/Types.hs` returns no matches
    - _Requirements: 4.1_
  - [x] 3.2 Purge legacy synthetic-root paths
    - Files: `src/MLF/Constraint/Normalize.hs`, `src/MLF/Constraint/Solve.hs`,
      `src/MLF/Constraint/Presolution/Copy.hs`, `src/MLF/Elab/*`, `test/*`
    - Steps:
      - Remove synthetic-root cases from structural traversals, reification, and tests.
    - **Verification:** `rg -n "root type" src test` returns no matches
    - _Requirements: 4.1_

- [x] 4. Guard instantiation/copy logic against treating the gen root as a binder
  - Files: `src/MLF/Constraint/Presolution/Copy.hs`, `src/MLF/Constraint/Presolution/Expansion.hs`
  - Steps:
    - Ensure `instantiateSchemeWithTrace` respects `I(g)` without copying the entire constraint.
  - **Verification:** `cabal test --test-show-details=direct --test-options='--match /instantiateSchemeWithTrace/'` passes
  - _Requirements: 4.1_

- [x] 5. Run the full test suite
  - **Verification:** `cabal test --test-show-details=direct` passes
  - _Requirements: 4.1_
