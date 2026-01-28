# Implementation Plan

- [x] 1. Audit binding-edge assignments for expansion/copy paths
  - Steps:
    - Trace where binder metas and copied nodes are introduced (`instantiateSchemeWithTrace`,
      `applyExpansionEdgeTraced`, `copyBinderBounds`).
    - Ensure each new `TyVar` gets a binding parent under the expansion root or
      the relevant `TyForall` binder.
  - Files: `src/MLF/Constraint/Presolution/Expansion.hs`,
    `src/MLF/Constraint/Presolution/Copy.hs`
  - Tests: compile-only for this step
  - Verification: `rg -n "createFreshVar|bind|setBindParent" src/MLF/Constraint/Presolution`
  - _Requirements: 1.1_

- [x] 2. Stabilize binding parents through presolution rewrite
  - Steps:
    - Review `rewriteConstraint` parent selection and ensure it preserves
      binding-parent paths to the intended scope root.
    - Add guard rails for cases where canonicalization would drop necessary
      edges (prefer expansion-root parents when available).
  - Files: `src/MLF/Constraint/Presolution/Driver.hs`
  - Tests: compile-only for this step
  - Verification: `rg -n "rewriteConstraint|chooseBindParent" src/MLF/Constraint/Presolution/Driver.hs`
  - _Requirements: 1.3_

- [x] 3. Add binding-edge coverage regressions
  - Steps:
    - Add a test that compares free vars in the generalized type to the set of
      vars reachable via binding-parent paths from the scope root (top-level
      gen root).
    - Add a non-Forall scope test where missing binding edges would drop a
      quantifier (bounded aliasing or let-polymorphism case).
  - Files: `test/ElaborationSpec.hs` (or new `test/BindingEdgeSpec.hs`),
    `test/Main.hs`, `mlf2.cabal`
  - Tests: `cabal --config-file=.cabal-config test --test-show-details=direct`
  - Verification: `rg -n "binding-parent|generalizeAt" test`
  - _Requirements: 2.1, 2.2, 2.3_

- [x] 4. Document readiness to remove fallback
  - Steps:
    - Update `implementation_notes.md` with the binding-edge invariant and the
      planned fallback removal once tests pass.
  - Files: `implementation_notes.md`
  - Tests: N/A (doc update)
  - Verification: `rg -n "fallback" implementation_notes.md`
  - _Requirements: 3.1_
