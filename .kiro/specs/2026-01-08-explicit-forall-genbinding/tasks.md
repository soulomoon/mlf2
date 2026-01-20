# Tasks

- Note: Implementation may touch any file needed to preserve thesis-exact binding trees and explicit-forall scoping.
- Status (2026-01-19): full suite passing; remaining unchecked tasks need a thesis-alignment audit to confirm coverage beyond tests.

- [x] 1. Encode explicit `STForall` as gen-bound named nodes
  - Update `internalizeSrcTypeWith` to create a gen node for each explicit forall and bind its variable under that gen.
  - Ensure the scheme root is the forall body node bound to that gen node.
  - Update `internalizeBinders` for bounds that contain `STForall`.
  - Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`
  - Requirements: 1.1, 1.2
  - Verification: `rg -n "STForall|internalizeSrcType|internalizeBinders" src/MLF/Frontend/ConstraintGen/Translate.hs`

- [x] 2. Remove explicit-forall exemptions in generalization (if any remain)
  - Ensure `allowedNames` contains only named nodes bound under the scheme gen.
  - Files: `src/MLF/Elab/Generalize.hs`
  - Requirements: 1.3
  - Verification: `rg -n "allowedNames" src/MLF/Elab/Generalize.hs`

- [x] 3. Update tests for explicit-forall encoding
  - Adjust constraint-gen tests that expect TyForall anchors for annotated schemes.
  - Update any other tests that depend on TyForall-bound explicit binders.
  - Files: `test/ConstraintGenSpec.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.6
  - Verification: `rg -n "TyForall|Scheme Forall|explicit forall" test/ConstraintGenSpec.hs test/ElaborationSpec.hs`

- [x] 4. Update generalization/reification for explicit-forall scoping
  - Treat explicit-forall gen nodes as distinct schemes in generalizeAt/allowedNames.
  - Preserve polymorphic bounds from explicit foralls in reify (rank-2 bounds).
  - Files: `src/MLF/Elab/Generalize.hs`, `src/MLF/Elab/Reify.hs`
  - Requirements: 1.5
  - Verification: `rg -n "allowedNames|scheme|named" src/MLF/Elab/Generalize.hs && rg -n "Reify|forall|named" src/MLF/Elab/Reify.hs`

- [x] 5. Fix elaboration tests for rank-2 bounds and ≺ ordering
  - Ensure generalizeAt order keys and binder selection match thesis under explicit-forall gen nodes.
  - Update failing elaboration tests if expectations reflect TyForall anchors.
  - Files: `test/ElaborationSpec.hs`, `test/SpecUtil.hs`
  - Requirements: 1.5, 1.7
  - Verification: `rg -n "generalizeAt|rank-2|forall" test/ElaborationSpec.hs test/SpecUtil.hs`

- [x] 6. Validate bounded-aliasing and full suite
  - Ensure bounded-aliasing annotated-let passes without free-name exemptions.
  - Run the full test suite.
  - Requirements: 1.4, 1.7
  - Verification: `cabal test --test-show-details=direct`

- [x] 7. Restore rank-2 annotation typing for λ parameters (thesis §12.3.2/§15.3.8)
  - Ensure annotated λ parameters elaborate to a rank-2 argument type (e.g., `(∀a. a -> a) -> ...`) rather than a top-level ∀ over a monomorphic argument.
  - Likely touch `src/MLF/Frontend/ConstraintGen/Translate.hs`, `src/MLF/Elab/Generalize.hs`, `src/MLF/Elab/Run.hs`, and `test/ElaborationSpec.hs`.
  - Requirements: 1.5
  - Verification: `rg -n "rank-2 argument" test/ElaborationSpec.hs && cabal test --test-show-details=direct`

- [x] 8. Replace scope-selection heuristics with thesis-backed scope rules
  - Remove or justify `generalizeAtWith` missing-name retries and `bindingScopeRef` fallback selection with a thesis-aligned rule.
  - Files: `src/MLF/Elab/Generalize.hs`, `src/MLF/Elab/Run.hs`
  - Requirements: 1.3, 1.5
  - Verification: `rg -n "pickScopeFromMissing|bindingScopeRef" src/MLF/Elab/Generalize.hs src/MLF/Elab/Run.hs`
  - Note: `bindingScopeRef` still uses reachable-binder/nearest-gen heuristics; thesis-backed scope rule remains to be formalized.

- [ ] 9. Implement thesis-exact Typ(a′) construction (Sχ′p + Γa)
  - Compute Typ(a′) as ∀(Γa) Sχ′p(hga′·1i), where Γa is the named nodes bound on ga′ and each bound uses Sχp. If the scheme root is named, its bound must be included (no unbounded/inline shortcut).
  - Ensure `reifyTypeWithNamesNoFallback` corresponds to Sχ′p and `reifyBoundWithNames` corresponds to Sχp, as in Figure 15.3.2.
  - Thesis: `papers/these-finale-english.txt` Figure 15.3.2; §15.3.2, Def. 15.3.1–15.3.2.
  - Files: `src/MLF/Elab/Generalize.hs`, `src/MLF/Elab/Reify.hs`
  - Requirements: 1.5, 1.9
  - Verification: `rg -n "Sχ|reifyTypeWithNames|reifyBoundWithNames|scheme root" src/MLF/Elab/Generalize.hs src/MLF/Elab/Reify.hs && cabal test --test-show-details=direct`
  - Partial: `reifyTypeWithNamesNoFallback` is used more widely and ModeBound binders are filtered, but Typ(a′) still does not enforce Γa-only free names or bound inclusion for named scheme roots.

- [ ] 10. Implement thesis-exact scope selection for subterms (ga′)
  - Use the §15.3.2 rule only: scope is the first gen ancestor of the subterm root (or the root itself if none).
  - Remove any special-case overrides (e.g., scheme-root shortcuts) that bypass the binding-parent chain.
  - Ensure subterm roots are bound under their corresponding gen nodes so the nearest-gen rule is well-defined.
  - Thesis: `papers/these-finale-english.txt` §15.3.2, Def. 15.3.2.
  - Files: `src/MLF/Elab/Run.hs`, `src/MLF/Frontend/ConstraintGen/Translate.hs`
  - Requirements: 1.8
  - Verification: `rg -n "bindingScopeRef|scopeRoot" src/MLF/Elab/Run.hs && cabal test --test-show-details=direct`

- [ ] 11. Preserve ga′ across redirects/canonicalization (thesis-exact)
  - Select ga′ using the pre-redirect subterm root from `AnnExpr` (before canonicalization).
  - Project binding parents from the pre-solve graph onto redirected/canonical node ids so the nearest-gen ancestor remains well-defined after presolution/solve.
  - Only fill missing parents in the solved graph; do not override parents established by presolution/solve unless they are self-parents introduced by redirects.
  - Thesis: `papers/these-finale-english.txt` §15.3.2, Def. 15.3.2.
  - Files: `src/MLF/Elab/Run.hs`, `src/MLF/Constraint/Presolution/*` (if redirect helpers are needed)
  - Requirements: 1.8, 1.10
  - Verification: `rg -n "redirect|binding parent|ga′|scope" src/MLF/Elab/Run.hs src/MLF/Constraint/Presolution && cabal test --test-show-details=direct`
  - Partial: `letScopeOverrides` and redirect-aware scope selection exist, but instantiation arguments still reify to variables in Phase 6 (see Task 24).

- [ ] 12. Clean up presolution binding-parent conflicts from redirects
  - When `cBindParents` disagree between base and solved graphs, choose the binding-parent chain that preserves the original `ga′` scope and drop self-parent edges introduced by redirects.
  - Ensure no scheme-root shortcut overwrites a valid binding-parent chain.
  - Thesis: `papers/these-finale-english.txt` §15.3.2 (binding tree for `ga′`, Def. 15.3.2).
  - Files: `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Elab/Run.hs`
  - Requirements: 1.11
  - Verification: `rg -n "bind parent|schemeParents|redirect" src/MLF/Constraint/Presolution/Driver.hs src/MLF/Elab/Run.hs && cabal test --test-show-details=direct`
  - Partial: base binding-parent preference is implemented in `constraintForGeneralization`, but κσ still fails with `GenSchemeFreeVars`.

- [ ] 13. Keep binding-parent chains stable through TyExp elimination
  - Ensure redirected roots (TyExp-eliminated) inherit the original root's bind parent.
  - Confirm `bindingScopeRef` on the pre-redirect root and the redirected root selects the same gen node.
  - Thesis: `papers/these-finale-english.txt` §15.3.2.
  - Files: `src/MLF/Elab/Run.hs`, `src/MLF/Frontend/ConstraintGen/Translate.hs`
  - Requirements: 1.8, 1.10
  - Verification: `rg -n "bindingScopeRef|chaseRedirects|bind parent" src/MLF/Elab/Run.hs src/MLF/Frontend/ConstraintGen/Translate.hs`

- [ ] 14. Add regression tests for redirected roots and ga′
  - Add a test that `let id = (\\x. x) in id id` generalizes at the let-body root even when the body root is redirected.
  - Add a test that dual instantiation does not produce `SchemeFreeVars` after redirects.
  - Add a test that top-level `let` remains polymorphic when the scheme root is redirected.
  - Thesis: `papers/these-finale-english.txt` §15.3.2.
  - Files: `test/ElaborationSpec.hs`, `test/SpecUtil.hs`, `test/PipelineSpec.hs`
  - Requirements: 1.7, 1.8, 1.10
  - Verification: `rg -n "id id|dual instantiation|redirect|scheme root" test/ElaborationSpec.hs test/SpecUtil.hs test/PipelineSpec.hs && cabal test --test-show-details=direct`

- [ ] 15. Keep polymorphic let instantiation after redirects
  - Ensure instantiation uses the generalization-oriented solve result (not the witness-only solve) so multiple instantiations remain fresh.
  - Validate `apply used twice` and `polymorphic let instantiated at different types` remain polymorphic after redirects.
  - Thesis: `papers/these-finale-english.txt` §15.3.2 (Typ(a′), ga′ scope).
  - Files: `src/MLF/Elab/Run.hs`, `src/MLF/Elab/Elaborate.hs`, `test/PipelineSpec.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.7, 1.10
  - Verification: `rg -n "apply used twice|polymorphic let" test/PipelineSpec.hs test/ElaborationSpec.hs && cabal test --test-show-details=direct`
  - Partial: instantiation Φ now uses `resReify` (generalization-oriented solve) in `reifyInst`; `instArgBaseBounds` normalizes binder ids via EdgeTrace copy maps and tracks multi-base instantiations per EdgeTrace root; binding-parent projection now drops non-upper parents and reattaches orphan roots to preserve a single gen root. The polymorphic-let instantiation type still collapses to `Int`, so the `∀(a ⩾ Bool). a` expectation still fails.

- [ ] 16. Fix annotated instantiation of polymorphic results
  - Ensure term annotations can instantiate a polymorphic result even when the annotation target is redirected (rank-2/explicit-forall cases).
  - Update tests to assert the nested forall structure from the thesis.
  - Thesis: `papers/these-finale-english.txt` §15.3.2, Typ(a′).
  - Files: `src/MLF/Elab/Run.hs`, `src/MLF/Elab/Elaborate.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.5, 1.7, 1.10
  - Verification: `rg -n "term annotation can instantiate|forall" test/ElaborationSpec.hs && cabal test --test-show-details=direct`

- [x] 17. Preserve explicit foralls inside bounds (no hoisting)
  - Ensure explicit forall binders that appear inside bounds remain nested under their bound scheme and are not promoted to `ga′` quantifiers.
  - When repairing binding parents for generalization, skip nested scheme roots owned by other gens so bound-scopes keep their binders.
  - If needed, rebind explicit-forall bound roots to their owning binders so `reifyBoundWithNames` emits `∀` in bounds rather than lifting them.
  - Thesis: `papers/these-finale-english.txt` §15.3.1–§15.3.2 (Sχp/Sχ′p), Fig. 15.3.2 (nested schemes), Def. 15.3.1.
  - Files: `src/MLF/Frontend/ConstraintGen/Translate.hs`, `src/MLF/Elab/Run.hs`, `src/MLF/Elab/Generalize.hs`, `src/MLF/Elab/Reify.hs`
  - Requirements: 1.5, 1.9, 1.11
  - Verification: `rg -n "explicit forall annotation preserves foralls in bounds" test/ElaborationSpec.hs && cabal test --test-show-details=direct`

- [x] 18. Repair binding-tree projection for explicit-forall bound roots
  - In `constraintForGeneralization`, ensure bound scheme roots retain their gen binding parents after merge/redirect projection so the bound’s gen is reachable from the bound root.
  - Do not attach bound scheme roots as children of `ga′` unless they are truly owned by `ga′` in the binding tree.
  - Add a focused regression test assertion or debug guard to detect bound root parent loss during generalization.
  - Thesis: `papers/these-finale-english.txt` §15.3.1–§15.3.2, Fig. 15.3.2 (nested bound schemes).
  - Files: `src/MLF/Elab/Run.hs`, `src/MLF/Elab/Generalize.hs`, `test/ElaborationSpec.hs` (if needed)
  - Requirements: 1.9, 1.11, 1.12
  - Verification: `rg -n "constraintForGeneralization|bind parent|scheme root" src/MLF/Elab/Run.hs src/MLF/Elab/Generalize.hs && cabal test --test-show-details=direct`
  - Partial: scheme-root parents are restored, but bound-derived instantiation arguments still do not resolve to base types (see Task 24), so this is not thesis-complete yet.

- [ ] 19. Use scheme-body root for ga′ generalization on redirected let uses
  - When a let scheme is explicit or redirected, set the generalization target to the scheme-body node (the TyForall body), not the bound node.
  - Ensure ga′ binders come from Q(g) of the scheme gen (per Def. 15.3.2), not from bound-root aliases.
  - Thesis: `papers/these-finale-english.txt` §15.3.2, Def. 15.3.2; Fig. 15.3.2 (Typ(a′) uses scheme-body root).
  - Files: `src/MLF/Elab/Run.hs`, `src/MLF/Elab/Generalize.hs`
  - Requirements: 1.5, 1.8, 1.10
  - Verification: `rg -n "generalizeAt|schemeRootByBody|ga'" src/MLF/Elab/Run.hs src/MLF/Elab/Generalize.hs && cabal test --test-show-details=direct`

- [ ] 20. Make bound-dependency ordering only track real bound dependencies
  - Compute bound dependencies on the bound expression graph (Sχp) without treating scheme-root aliases or bound-as-root indirections as separate binders.
  - Fix `generalizeAt respects binder bound dependencies (a ≺ b if b’s bound mentions a)`.
  - Thesis: `papers/these-finale-english.txt` §15.2.4, §15.3.2 (binder ordering by bound mentions).
  - Files: `src/MLF/Elab/Generalize.hs`, `src/MLF/Elab/Reify.hs`, `src/MLF/Util/OrderKey.hs` (if ordering traversal needs adjustment)
  - Requirements: 1.7, 1.9
  - Verification: `rg -n "freeNames|bound deps|orderKeys" src/MLF/Elab/Generalize.hs src/MLF/Elab/Reify.hs src/MLF/Util/OrderKey.hs && cabal test --test-show-details=direct`

- [ ] 21. Preserve instantiate-then-forall composition when rebound
  - Ensure the pipeline uses `ExpCompose [ExpInstantiate, ExpForall]` when a scheme root is rebound under a new gen (no ExpInstantiate on non-forall).
  - Fix `Pipeline: composes instantiate then forall when rebound at new level`.
  - Thesis: `papers/these-finale-english.txt` §15.2.6 (rightmost constraint), §15.3.2 (instantiation under rebound).
  - Files: `src/MLF/Constraint/Presolution/Driver.hs`, `src/MLF/Constraint/Presolution/DecideExpansion.hs`, `src/MLF/Elab/Generalize.hs`, `test/PipelineSpec.hs`
  - Requirements: 1.7, 1.10
  - Verification: `rg -n "decideMinimalExpansion|ExpCompose|ExpInstantiate|ExpForall" src/MLF/Constraint/Presolution src/MLF/Elab/Generalize.hs && cabal test --test-show-details=direct`

- [ ] 22. Relax binder filtering for scheme-body aliases
  - Do not drop binders whose bound is (or aliases) the scheme body root; treat them as target scheme binders.
  - Fix failures: generalizeAt quantifies vars bound under scope root; dual instantiation; polymorphic let instantiation at different types.
  - Thesis: `papers/these-finale-english.txt` §15.3.2 (Q(g) and Typ(a′) for named binders under ga′).
  - Files: `src/MLF/Elab/Generalize.hs`
  - Requirements: 1.5, 1.7
  - Verification: `rg -n "boundIsSchemeRoot|isTargetSchemeBinder" src/MLF/Elab/Generalize.hs && cabal test --test-show-details=direct`
  - Partial: binder filters were relaxed for local scheme roots/bodies, but bound dependency ordering and polymorphic instantiation at different types still fail.

- [ ] 23. Tighten rigid-bound inlining to thesis conditions
  - Inline rigid bounds only for base/⊥ or variable-only bounds (no nested forall/arrow), matching §15.3.2 Typ(a′) side conditions.
  - Fix `generalizeAt inlines rigid vars via bounds at top-level` and related baseline failures.
  - Thesis: `papers/these-finale-english.txt` §15.3.2, Fig. 15.3.2 (Sχp vs Sχ′p for bounds).
  - Files: `src/MLF/Elab/Generalize.hs`, `src/MLF/Elab/Reify.hs`
  - Requirements: 1.5, 1.9
  - Verification: `rg -n "inline|rigid|bound|Sχ" src/MLF/Elab/Generalize.hs src/MLF/Elab/Reify.hs && cabal test --test-show-details=direct`

- [x] 24. Reify instantiation arguments from the solved edge type / witness (not raw nodes)
  - When constructing instantiation witnesses for applications, derive argument types from the solved edge type (or the Φ/Σ witness / EdgeTrace binder-arg mapping) so `id 1` yields `⟨Int⟩` even if the arg node is a variable.
  - Remove any dependency on the raw `ExpInstantiate` argument node id for type reification when it does not resolve to a base/bottom.
  - Thesis: `papers/these-finale-english.txt` §15.3.2 (Typ(a′)), Fig. 15.3.2 (Sχ′p), §15.2.6 (instantiation under solved constraints).
  - Files: `src/MLF/Elab/Elaborate.hs`, `src/MLF/Elab/Run.hs`, `src/MLF/Elab/Phi.hs` (if using Φ/Σ).
  - Requirements: 1.5, 1.7, 1.10
  - Verification: `rg -n "ExpInstantiate|expansionToInst|phiFromEdgeWitness" src/MLF/Elab/Elaborate.hs src/MLF/Elab/Run.hs src/MLF/Elab/Phi.hs && cabal test --test-show-details=direct`
  - Result: `Phase 6 — Elaborate (xMLF) / elaborates polymorphic instantiation` now passes (no more `⟨t6⟩` for `id 1`).

- [ ] 25. Make bound dependency ordering follow bound chains through scheme-body aliases
  - When computing binder dependencies, traverse bounds through scheme-body aliases so `b ⩾ a` is ordered after `a` when `b`’s bound mentions `a`.
  - Ensure dependencies are computed on the bound expression graph (Sχp) rather than the post-redirect scheme-root alias graph.
  - Thesis: `papers/these-finale-english.txt` §15.2.4 (≺ ordering), §15.3.2 (bound dependencies).
  - Files: `src/MLF/Elab/Generalize.hs`, `src/MLF/Elab/Reify.hs`
  - Requirements: 1.7, 1.9
  - Verification: `rg -n "bound deps|freeNamedDeps|orderBinderCandidates" src/MLF/Elab/Generalize.hs src/MLF/Elab/Reify.hs && cabal test --test-show-details=direct`

- [ ] 26. Fix κσ baseline by enforcing ga′-scope named-node closure
  - Ensure generalization at ga′ only quantifies named nodes bound under that gen, and rejects/rewrites any bound that reaches named nodes outside ga′ (instead of throwing `GenSchemeFreeVars`).
  - Use the thesis-allowed set Γa (named under ga′) as the only allowed free names in Typ(a′).
  - Thesis: `papers/these-finale-english.txt` §15.3.2 (Γa, Typ(a′)), Def. 15.3.2.
  - Files: `src/MLF/Elab/Generalize.hs`, `src/MLF/Elab/Run.hs`
  - Requirements: 1.5, 1.8, 1.10
  - Verification: `rg -n "allowedNames|namedSet|SchemeFreeVars|Gamma" src/MLF/Elab/Generalize.hs src/MLF/Elab/Run.hs && cabal test --test-show-details=direct`

- [ ] 27. Tighten bound scoping for explicit-forall binders (thesis-exact ga′)
  - Ensure explicit-forall binders that appear inside bounds stay under their own gen nodes and are not treated as ga′ children when computing Γa.
  - Preserve binding parents for bound scheme roots through `constraintForGeneralization` so `Sχp` can see nested schemes and avoid hoisting their binders into outer quantifiers.
  - Verify that a bound like `b ⩾ (∀a. a -> a)` remains nested in the bound and does not add `a` to Γa.
  - Thesis: `papers/these-finale-english.txt` §15.3.1–§15.3.2, Fig. 15.3.2 (nested Sχp), Def. 15.3.1–15.3.2.
  - Files: `src/MLF/Elab/Run.hs`, `src/MLF/Elab/Generalize.hs`, `src/MLF/Elab/Reify.hs`, `src/MLF/Constraint/Presolution/Driver.hs`
  - Requirements: 1.9, 1.11, 1.12
  - Verification: `rg -n "constraintForGeneralization|bind parent|bound scheme|Sχp" src/MLF/Elab/Run.hs src/MLF/Elab/Generalize.hs src/MLF/Elab/Reify.hs && cabal test --test-show-details=direct`

- [ ] 28. Make ga′ selection and binding-parent projection thesis-exact under redirects
  - Select ga′ from the pre-redirect subterm root (from `AnnExpr`) and map its binding-parent chain through redirects before generalization.
  - Only repair missing parents in the solved graph; never replace a non-self parent that already preserves ga′.
  - Add a guard or regression assertion to ensure redirected roots yield the same ga′ as their pre-redirect roots.
  - Thesis: `papers/these-finale-english.txt` §15.3.2, Def. 15.3.2 (ga′ as nearest gen ancestor).
  - Files: `src/MLF/Elab/Run.hs`, `src/MLF/Constraint/Presolution/Driver.hs`, `test/ElaborationSpec.hs`, `test/PipelineSpec.hs`
  - Requirements: 1.8, 1.10, 1.11
  - Verification: `rg -n "ga'|bindingScopeRef|redirect|bind parent" src/MLF/Elab/Run.hs src/MLF/Constraint/Presolution/Driver.hs test/ElaborationSpec.hs test/PipelineSpec.hs && cabal test --test-show-details=direct`

- [ ] 29. Add display-only bound inlining for ML-style output (§8.3.1)
  - Implement a display pass that inlines bounds per §8.3.1 using a conservative syntactic approximation (single covariant occurrence, binder not used in its bound, and base/variable-only bounds), without changing core generalization.
  - Expose `PrettyDisplay` (or equivalent) for Elab types/schemes/terms and update ML-style tests to use it.
  - Thesis: `papers/these-finale-english.txt` §8.3.1 (Inlining bounds).
  - Files: `src/MLF/Elab/Types.hs`, `src/MLF/Elab/Pipeline.hs`, `test/ElaborationSpec.hs`
  - Requirements: 1.13, 1.7
  - Verification: `rg -n "prettyDisplay|inlineBoundsForDisplay" src/MLF/Elab/Types.hs test/ElaborationSpec.hs`

# Known failing tests (as of 2026-01-13)

- Phase 6 — Elaborate (xMLF): polymorphic let instantiated at different types returns `∀a. a` (expects `∀(a ⩾ Bool). a`).
  Rerun: `cabal test --test-show-details=direct --test-options='--match=/Phase 6 — Elaborate (xMLF)\\/Polymorphism and Generalization\\/elaborates usage of polymorphic let \\(instantiated at different types\\)/'`
- Phase 6 — Elaborate (xMLF): rank-2 bound annotation mismatches.
  Rerun: `cabal test --test-show-details=direct --test-options='--match=/Phase 6 — Elaborate (xMLF)\\/Elaboration of Bounded Quantification \\(Flexible Bounds\\)\\/elaborates annotated let with polymorphic bound \\(Rank-2ish\\)/'`
- Phase 6 — Elaborate (xMLF): rank-2 lambda argument hoists ∀ incorrectly.
  Rerun: `cabal test --test-show-details=direct --test-options='--match=/Phase 6 — Elaborate (xMLF)\\/Elaboration of Bounded Quantification \\(Flexible Bounds\\)\\/elaborates lambda with rank-2 argument/'`
- Phase 6 — Elaborate (xMLF): binder bound dependency ordering drops bounds.
  Rerun: `cabal test --test-show-details=direct --test-options='--match=/Phase 6 — Elaborate (xMLF)\\/Paper ≺ ordering \\(leftmost-lowermost\\)\\/generalizeAt respects binder bound dependencies \\(a ≺ b if b’s bound mentions a\\)/'`
- Paper baselines: `let id = (\\x. x) in id id` mismatch.
  Rerun: `cabal test --test-show-details=direct --test-options='--match=/Phase 6 — Elaborate (xMLF)\\/Paper alignment baselines\\/let id = \\(\\\\x\\. x\\) in id id should have type ∀a\\. a -> a/'`
- Paper baselines: `\\y. let id = (\\x. x) in id y` mismatch.
  Rerun: `cabal test --test-show-details=direct --test-options='--match=/Phase 6 — Elaborate (xMLF)\\/Paper alignment baselines\\/\\\\y\\. let id = \\(\\\\x\\. x\\) in id y should have type ∀a\\. a -> a/'`
- Paper baselines: term annotation instantiation mismatch.
  Rerun: `cabal test --test-show-details=direct --test-options='--match=/Phase 6 — Elaborate (xMLF)\\/Paper alignment baselines\\/term annotation can instantiate a polymorphic result/'`
- Paper baselines: κσ annotated lambda parameter hits `GenSchemeFreeVars`.
  Rerun: `cabal test --test-show-details=direct --test-options='--match=/Phase 6 — Elaborate (xMLF)\\/Paper alignment baselines\\/annotated lambda parameter should accept a polymorphic argument via κσ/'`
