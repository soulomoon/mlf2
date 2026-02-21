# Tasks

- Note: Implementation may touch any file needed to preserve thesis-exact binding trees and explicit-forall scoping.
- Status (2026-02-04): `cabal test --test-show-details=direct` passes (419 examples, 0 failures, 1 pending).
- Resolved: κσ / annotated-lambda parameter semantics (US-004) — test is active and passing in `test/ElaborationSpec.hs` (lines 525, 2702).

- [x] 1. Encode explicit `STForall` as gen-bound named nodes
  - Update `internalizeSrcTypeWith` to create a gen node for each explicit forall and bind its variable under that gen.
  - Ensure the scheme root is the forall body node bound to that gen node.
  - Update `internalizeBinders` for bounds that contain `STForall`.
  - Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`
  - Requirements: 1.1, 1.2
  - Verification: `rg -n "STForall|internalizeSrcTypeWith|internalizeBinders" src/MLF/Frontend/ConstraintGen/Translate.hs`

- [x] 2. Keep scheme closure strict (no explicit-forall exemptions)
  - Enforce strict closure under the scheme gen; no “free-name” exemptions for explicit binders.
  - Files: `src/MLF/Binding/Validation.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.3
  - Verification: `rg -n "GenSchemeFreeVars|checkSchemeClosure" src/MLF/Binding/Validation.hs test/ElaborationSpec.hs`

- [x] 3. Update tests for explicit-forall encoding
  - Adjust constraint-gen tests that expect TyForall anchors for annotated schemes.
  - Update any other tests that depend on TyForall-bound explicit binders.
  - Files: `test/ConstraintGenSpec.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.6
  - Verification: `rg -n "TyForall|Scheme Forall|explicit forall" test/ConstraintGenSpec.hs test/ElaborationSpec.hs`

- [x] 4. Update generalization/reification for explicit-forall scoping
  - Treat explicit-forall gen nodes as distinct scheme scopes in generalization.
  - Preserve polymorphic bounds from explicit foralls in reify (rank-2 bounds).
  - Files: `src/MLF/Elab/Generalize.hs`, `src/MLF/Reify/Core.hs`
  - Requirements: 1.5
  - Verification: `rg -n "reifyTypeWithNamesNoFallback|reifyBoundWithNames" src/MLF/Reify/Core.hs && cabal test --test-show-details=direct`

- [x] 5. Fix elaboration tests for rank-2 bounds and ≺ ordering
  - Ensure generalizeAt order keys and binder selection match thesis under explicit-forall gen nodes.
  - Files: `test/ElaborationSpec.hs`, `test/SpecUtil.hs`
  - Requirements: 1.5, 1.7
  - Verification: `rg -n "generalizeAt|rank-2|forall" test/ElaborationSpec.hs test/SpecUtil.hs`

- [x] 6. Validate bounded-aliasing and full suite
  - Ensure bounded-aliasing annotated-let passes without free-name exemptions.
  - Run the full test suite.
  - Requirements: 1.4, 1.7
  - Verification: `cabal test --test-show-details=direct`

- [x] 7. Restore rank-2 annotation typing for λ parameters (thesis §12.3.2/§15.3.8)
  - Ensure annotated λ parameters elaborate to a rank-2 argument type (e.g., `(∀a. a -> a) -> ...`) rather than hoisting ∀ to the top level.
  - Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`, `src/MLF/Elab/Run/ResultType/*.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.5
  - Verification: `rg -n "rank-2 argument" test/ElaborationSpec.hs && cabal test --test-show-details=direct`

- [x] 8. Replace scope-selection heuristics with thesis-backed scope rules
  - Use the binding-tree rule for ga′ (nearest gen ancestor in the binding-parent chain).
  - Files: `src/MLF/Elab/Run/Scope.hs`
  - Requirements: 1.3, 1.5
  - Verification: `rg -n "bindingScopeRef" src/MLF/Elab/Run/Scope.hs`

- [x] 9. Implement thesis-exact Typ(a′) construction (Sχ′p + Γa)
  - Compute Typ(a′) as ∀(Γa) Sχ′p(hga′·1i), where Γa is the named nodes bound on ga′ and each bound uses Sχp (including the scheme root when it is named).
  - Ensure `reifyTypeWithNamesNoFallback` corresponds to Sχ′p and `reifyBoundWithNames` corresponds to Sχp, as in Figure 15.3.2.
  - Thesis: `papers/these-finale-english.txt` Figure 15.3.2; §15.3.2, Def. 15.3.1–15.3.2.
  - Files: `src/MLF/Elab/Generalize.hs`, `src/MLF/Reify/Core.hs`
  - Requirements: 1.5, 1.9
  - Verification: `rg -n "reifyTypeWithNamesNoFallback|reifyBoundWithNames" src/MLF/Reify/Core.hs && cabal test --test-show-details=direct`

- [x] 10. Implement thesis-exact scope selection for subterms (ga′)
  - Scope is the first gen ancestor of the subterm root (or the root itself if none).
  - Thesis: `papers/these-finale-english.txt` §15.3.2, Def. 15.3.2.
  - Files: `src/MLF/Elab/Run/Scope.hs`
  - Requirements: 1.8
  - Verification: `rg -n "bindingScopeRef" src/MLF/Elab/Run/Scope.hs && cabal test --test-show-details=direct`

- [x] 11. Preserve ga′ across redirects/canonicalization (thesis-exact)
  - Select ga′ from the pre-redirect subterm root (from `AnnExpr`) and map binding-parent chains forward through redirects for generalization/result typing.
  - Only fill missing parents in the solved graph; never replace a non-self parent that already preserves ga′.
  - Thesis: `papers/these-finale-english.txt` §15.3.2, Def. 15.3.2.
  - Files: `src/MLF/Elab/Run/Scope.hs`, `src/MLF/Elab/Run/Generalize/*.hs`, `src/MLF/Elab/Run/ResultType/*.hs`
  - Requirements: 1.8, 1.10
  - Verification: `rg -n "resolveCanonicalScope|letScopeOverrides|ga' mismatch" src/MLF/Elab/Run && cabal test --test-show-details=direct`

- [x] 12. Clean up presolution binding-parent conflicts from redirects
  - Drop redirect-induced self-parents and prefer base-parent structure when filling solved bind parents.
  - Thesis: `papers/these-finale-english.txt` §15.3.2 (binding tree for `ga′`, Def. 15.3.2).
  - Files: `src/MLF/Elab/Run/Generalize/Phase3.hs`
  - Requirements: 1.11
  - Verification: `rg -n "override self-parent|bindParents" src/MLF/Elab/Run/Generalize/Phase3.hs && cabal test --test-show-details=direct`

- [x] 13. Keep binding-parent chains stable through TyExp elimination
  - Ensure redirected roots inherit the original root's binding scope (ga′ remains stable).
  - Thesis: `papers/these-finale-english.txt` §15.3.2.
  - Files: `src/MLF/Elab/Run/Util.hs`, `src/MLF/Elab/Run/Scope.hs`
  - Requirements: 1.8, 1.10
  - Verification: `rg -n "chaseRedirects|canonicalizeScopeRef" src/MLF/Elab/Run && cabal test --test-show-details=direct`

- [x] 14. Add regression tests for redirected roots and ga′
  - Includes tests covering redirected let-use sites, dual instantiation, and polymorphic schemes under redirects.
  - Files: `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`
  - Requirements: 1.7, 1.8, 1.10
  - Verification: `rg -n "redirected let-use sites|let-use sites are redirected|dual instantiation" test/ElaborationSpec.hs test/PipelineSpec.hs && cabal test --test-show-details=direct`

- [x] 15. Keep polymorphic let instantiation after redirects
  - Validate `apply used twice` and `polymorphic let instantiated at different types`.
  - Files: `src/MLF/Elab/Elaborate.hs`, `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.7, 1.10
  - Verification: `rg -n "apply used twice|instantiated at different types" test/PipelineSpec.hs test/ElaborationSpec.hs && cabal test --test-show-details=direct`

- [x] 16. Fix annotated instantiation of polymorphic results
  - Ensure term annotations can instantiate a polymorphic result (κσ view: instantiation, not equality).
  - Files: `src/MLF/Elab/Run/ResultType/*.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.5, 1.7, 1.10
  - Verification: `rg -n "term annotation can instantiate a polymorphic result" test/ElaborationSpec.hs && cabal test --test-show-details=direct`

- [x] 17. Preserve explicit foralls inside bounds (no hoisting)
  - Ensure explicit forall binders that appear inside bounds remain nested under their bound scheme and are not promoted to `ga′` quantifiers.
  - Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`, `src/MLF/Elab/Run/Generalize/*.hs`, `src/MLF/Reify/Core.hs`
  - Requirements: 1.5, 1.9, 1.11
  - Verification: `rg -n "explicit forall annotation preserves foralls in bounds" test/ElaborationSpec.hs && cabal test --test-show-details=direct`

- [x] 18. Repair binding-tree projection for explicit-forall bound roots
  - In `constraintForGeneralization`, ensure bound scheme roots retain their gen binding parents after merge/redirect projection so nested schemes remain reachable.
  - Files: `src/MLF/Elab/Run/Generalize/*.hs`
  - Requirements: 1.9, 1.11, 1.12
  - Verification: `rg -n "constraintForGeneralization|scheme root" src/MLF/Elab/Run/Generalize && cabal test --test-show-details=direct`

- [x] 19. Use scheme-body root for ga′ generalization on redirected let uses
  - Use the scheme-body node (TyForall body) as the generalization target when the scheme root is an alias wrapper.
  - Files: `src/MLF/Elab/Run/Scope.hs`, `src/MLF/Elab/Run/ResultType/*.hs`
  - Requirements: 1.5, 1.8, 1.10
  - Verification: `rg -n "schemeBodyTarget" src/MLF/Elab/Run/Scope.hs src/MLF/Elab/Run/ResultType && cabal test --test-show-details=direct`

- [x] 20. Make bound-dependency ordering only track real bound dependencies
  - Compute dependencies on the bound expression graph (Sχp).
  - Files: `src/MLF/Constraint/Presolution/Plan/BinderPlan/*.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.7, 1.9
  - Verification: `rg -n "bound dependencies|boundDeps|orderBinderCandidates" src/MLF/Constraint/Presolution/Plan/BinderPlan test/ElaborationSpec.hs && cabal test --test-show-details=direct`

- [x] 21. Preserve instantiate-then-forall composition when rebound
  - Ensure the pipeline uses `ExpCompose [ExpInstantiate, ExpForall]` when rebinding a scheme root under a new gen.
  - Files: `src/MLF/Constraint/Presolution/Expansion.hs`, `test/PipelineSpec.hs`
  - Requirements: 1.7, 1.10
  - Verification: `rg -n "ExpCompose|ExpInstantiate|ExpForall" src/MLF/Constraint/Presolution/Expansion.hs test/PipelineSpec.hs && cabal test --test-show-details=direct`

- [x] 22. Relax binder filtering for scheme-body aliases
  - Treat binders whose bound is (or aliases) the scheme body root as target scheme binders.
  - Files: `src/MLF/Constraint/Presolution/Plan/BinderPlan/*.hs`
  - Requirements: 1.5, 1.7
  - Verification: `rg -n "isTargetSchemeBinder" src/MLF/Constraint/Presolution/Plan/BinderPlan && cabal test --test-show-details=direct`

- [x] 23. Confirm rigid quantification inlining policy (thesis §15.2.5)
  - Keep “always inline rigid quantification” (no abstractions for rigid nodes).
  - Tests: `generalizeAt inlines rigid vars via bounds at top-level`, `generalizeAt inlines rigid vars with structured bounds`.
  - Files: `src/MLF/Elab/Generalize.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.5, 1.9
  - Verification: `rg -n "rigid quantification is always inlined" src/MLF/Elab/Generalize.hs && cabal test --test-show-details=direct`
  - Note: This supersedes an earlier draft task that proposed restricting rigid-bound inlining; current policy is paper-backed and test-covered.

- [x] 24. Reify instantiation arguments from the solved edge type / witness (not raw nodes)
  - When constructing instantiation witnesses for applications, derive argument types from the solved edge type (or the Φ/Σ witness / EdgeTrace binder-arg mapping) so `id 1` yields `⟨Int⟩` even if the arg node is a variable.
  - Remove any dependency on the raw `ExpInstantiate` argument node id for type reification when it does not resolve to a base/bottom.
  - Files: `src/MLF/Elab/Elaborate.hs`
  - Requirements: 1.5, 1.7, 1.10
  - Verification: `rg -n "reifyInst|instArgBaseBounds|phiFromEdgeWitness" src/MLF/Elab/Elaborate.hs && cabal test --test-show-details=direct`

- [x] 25. Make bound dependency ordering follow bound chains through scheme-body aliases
  - Traverse bounds through scheme-body aliases when computing bound dependencies so `b ⩾ a` is ordered after `a` if `b`’s bound mentions `a`.
  - Files: `src/MLF/Constraint/Presolution/Plan/BinderPlan/*.hs`, `src/MLF/Reify/Core.hs`
  - Requirements: 1.7, 1.9
  - Verification: `rg -n "bound deps|freeNamedDeps|orderBinderCandidates" src/MLF/Constraint/Presolution/Plan/BinderPlan src/MLF/Reify/Core.hs && cabal test --test-show-details=direct`

- [x] 26. Decide/implement κσ for annotated lambda parameters (US-004)
  - Test is active and passing: `annotated lambda parameter should accept a polymorphic argument via κσ (US-004)` at `test/ElaborationSpec.hs:2702`.
  - Rank-2 argument test also active: `elaborates lambda with rank-2 argument (US-004)` at `test/ElaborationSpec.hs:525`.
  - `ELamAnn` desugars to `let` + coercion; checked-authoritative pipeline produces thesis-expected `Int`.
  - Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`, `src/MLF/Elab/Run/ResultType/*.hs`, `test/ConstraintGenSpec.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.5, 1.8, 1.10
  - Verification: `cabal test mlf2-test --test-show-details=direct --test-options='--match “US-004”'`

- [x] 27. Tighten bound scoping for explicit-forall binders (thesis-exact ga′)
  - Covered by Tasks 17–18: nested bound schemes keep their own gen scope; Γa does not capture nested explicit-forall binders.
  - Requirements: 1.9, 1.11, 1.12

- [x] 28. Make ga′ selection and binding-parent projection thesis-exact under redirects
  - Covered by Tasks 10–14: ga′ comes from the binding-parent chain of the pre-redirect root and is preserved through redirects.
  - Requirements: 1.8, 1.10, 1.11

- [x] 29. Add display-only bound inlining for ML-style output (§8.3.1)
  - Implement a display pass that inlines bounds per §8.3.1 without changing core generalization.
  - Files: `src/MLF/Elab/Types.hs`, `src/MLF/Elab/Pipeline.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.13, 1.7
  - Verification: `rg -n "prettyDisplay|inlineBoundsForDisplay" src/MLF/Elab/Types.hs test/ElaborationSpec.hs && cabal test --test-show-details=direct`

# Historical note

- The “Known failing tests (as of 2026-01-13)” list was removed because those cases now pass; use `cabal test --test-show-details=direct` for the current baseline.
