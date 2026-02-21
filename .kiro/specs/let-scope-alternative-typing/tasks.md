# Implementation Plan

- [x] 1. Record the current (leftmost) let constraint and target shape
  - Steps:
    - Capture the current constraint shape for `let x = a in b` in notes/tests.
    - Write the target rightmost constraint shape (Fig. 15.2.6) explicitly.
  - Files: `.kiro/specs/let-scope-alternative-typing/design.md`
  - Tests: N/A (documentation)
  - Verification: `rg -n "ELet|let" src/MLF/Frontend/ConstraintGen -S`
  - _Requirements: 1.1_

- [x] 2. Implement alternative let scoping (rightmost constraint)
  - Steps:
    - Allocate a gen node for the let expression and a trivial scheme root
      (fresh type variable) bound under it.
    - Add the instantiation edge from `b`’s type to the trivial scheme root.
    - Use `AAnn` to carry the let-expression instantiation edge (no new `AnnExpr` fields).
    - Ensure the RHS `a` does not see `b`’s scope (no piggybacking).
  - Files: `src/MLF/Frontend/ConstraintGen/*`, `src/MLF/Frontend/ConstraintGen/Types.hs`
  - Tests: new let-scoping regressions
  - Verification: `rg -n "ELet|let" src/MLF/Frontend/ConstraintGen -S`
  - _Requirements: 1.1, 1.2_

- [x] 3. Enforce and validate translatable presolutions (Def. 15.2.10)
  - Steps:
    - Apply Theorem 15.2.11 order:
      (1) weaken inert-locked + app/abs arrow nodes;
      (2) weaken non-degenerate scheme roots and non-interior nodes bound on gen nodes;
      (3) re-run inert-locked weakening to eliminate newly introduced inert-locked nodes.
    - Skip degenerate scheme roots (scheme root no longer bound on its gen node).
    - Keep expansions unchanged; update witness normalization to reflect weakened nodes.
    - Validate and surface a structured error if any condition still fails.
  - Files: `src/MLF/Constraint/Presolution/*`
  - Tests: presolution validation regression
  - Verification: `rg -n "translatable" src/MLF/Constraint/Presolution -S`
  - _Requirements: 2.1, 2.2_

- [x] 4. Thread let-expression schemes through elaboration/generalization  **Closed (2026-02-22):** Verified — all threading is in place.
  - Steps (verified):
    - Elaboration treats trivial scheme edge as identity: `Elaborate.hs:835` checks
      `canonical target == canonical trivialRoot` and calls `elabStripped` to skip wrapper.
    - Presolution handles trivial edges correctly: `dropTrivialSchemeEdges` (Base.hs:672-684)
      filters let edges from witness/trace/expansion maps; `ExpIdentity` returned for
      trivial targets (Expansion.hs:228-229).
    - Generalization scopes follow new layout: `letScopeOverrides` (Scope.hs:154-179)
      computes base-vs-solved scope divergence; `scopeRootForNode` (Elaborate.hs:249-253)
      applies overrides during generalization.
  - Files: `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Run/Scope.hs`, `src/MLF/Constraint/Presolution/Base.hs`
  - Tests: let elaboration tests at ElaborationSpec.hs lines 188, 201, 342, 355, 496, 511
  - Verification: `rg -n "trivialRoot" src/MLF/Elab/Elaborate.hs` → found at lines 835-836 ✓

- [x] 5. Add tests  **Closed (2026-02-22):** Verified — tests cover all critical paths.
  - Tests present:
    - O15-ELAB-LET (line 188): polymorphic let-binding elaboration
    - O15-ELAB-LET-VAR (line 201): monomorphic let without extra instantiation
    - Polymorphic let instantiation (line 342): multiple instantiations
    - Nested let bindings (line 355): nested scopes
    - Let with RHS annotation (lines 496, 511): coercion and polymorphic bounds
    - letScopeOverrides divergence (line 3113): scope override insertion
    - letScopeOverrides agreement (line 3157): no override when scopes match
    - TranslatablePresolutionSpec.hs:28: let-polymorphic presolution validation
  - Files: `test/ElaborationSpec.hs`, `test/TranslatablePresolutionSpec.hs`
  - Verification: `cabal test --test-show-details=direct` → 766 examples, 0 failures ✓
