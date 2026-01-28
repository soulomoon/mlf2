# Implementation Plan

- [x] 1. Introduce a gen-rooted scope for top-level generalization
  - Steps:
    - Allocate a root gen node at the top-level scope and rebind scope nodes
      under it.
    - Select the nearest gen ancestor in `runPipelineElab` for the
      generalization scope (fail if none exists).
  - Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`,
    `src/MLF/Frontend/ConstraintGen/Emit.hs`, `src/MLF/Elab/Run.hs`
  - Tests: constraint generation + pipeline regression
  - Verification: `rg -n "bindingScopeRef" src/MLF/Elab/Run.hs`
  - _Requirements: 2.1, 2.2_

- [x] 2. Update binder enumeration in `generalizeAt`
  - Steps:
    - Use Q(n) (`boundFlexChildrenUnder`) for `TyForall` scope roots.
    - For non-Forall scopes, enumerate binders by walking binding-parent paths
      to the scope root and filter by reachability.
    - Canonicalize and deduplicate binder ids before substitution.
  - Files: `src/MLF/Elab/Generalize.hs`
  - Tests: compile-only for this step
  - Verification: `rg -n "boundAtScope|canonical" src/MLF/Elab/Generalize.hs`
  - _Requirements: 1.1, 1.2_

- [x] 3. Scope the free-variable fallback for non-Forall types
  - Steps:
    - Add `freeVarsUnder` only when the scope root is non-Forall and the target
      type root is not `TyForall`.
    - Preserve explicit `TyForall` quantifiers in the reified type when scope
      differs from the target.
  - Files: `src/MLF/Elab/Generalize.hs`
  - Tests: regression on explicit `TyForall` target + top-level generalization
  - Verification: `rg -n "freeVarsUnder|typeRootIsForall" src/MLF/Elab/Generalize.hs`
  - _Requirements: 1.3, 2.1_

- [x] 4. Update regression tests for scoped fallback
  - Steps:
    - Ensure the `generalizeAt` unit test covers non-Forall scope roots.
    - Keep the bounded aliasing baseline and top-level let polymorphism checks.
  - Files: `test/ElaborationSpec.hs`
  - Tests: `cabal --config-file=.cabal-config test --test-show-details=direct`
  - Verification: `rg -n "generalizeAt|bounded aliasing" test/ElaborationSpec.hs`
  - _Requirements: 3.1, 3.2_

- [x] 5. Retire the scoped fallback now that binding edges cover
  - Steps:
    - Remove the free-variable fallback path from `generalizeAt`.
    - Document the retirement in `implementation_notes.md`.
  - Files: `src/MLF/Elab/Generalize.hs`, `implementation_notes.md`
  - Tests: `cabal --config-file=.cabal-config test --test-show-details=direct`
  - Verification: `rg -n "freeVarsUnder|fallback" src/MLF/Elab/Generalize.hs`
  - _Requirements: 2.2, 3.2_
