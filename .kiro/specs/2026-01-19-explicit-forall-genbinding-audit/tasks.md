# Implementation Plan

- [ ] 9. Audit Typ(a′) construction (Sχ′p + Γa)
  - Verify Sχ′p/Sχp mapping and schemeScope fallback impact.
  - Evidence: src/MLF/Elab/Generalize.hs:2706-2779, src/MLF/Elab/Reify.hs:649-715.
  - Result: Partially covered. Typ(a′) uses reifyTypeWithNamesNoFallback* and reifyBoundWithNames*, but schemeScope fallback can re-root the scheme when owner differs from ga′, so strict ∀(Γa) Sχ′p(hga′·1i) is not proven.
  - Status: Keep unchecked; requires thesis-exact audit or additional proof/tests.
  - **Verification:** `cabal test --test-show-details=direct`

- [x] 10. Audit thesis-exact ga′ scope selection
  - Review bindingScopeRef/preferGenScope and pre/post scope mapping for Def. 15.3.2 alignment.
  - Evidence: src/MLF/Elab/Run/Scope.hs — Note [ga′ scope selection — Def. 15.3.2 alignment].
  - Result: Thesis-aligned. bindingScopeRef computes nearest gen ancestor (Def. 15.3.2). preferGenScope is redundant but harmless (error-swallowing documented). canonicalizeScopeRef correctly passes GenRef through and applies redirect+UF to TypeRef. letScopeOverrides preserves base scope when solved diverges. Direct `letScopeOverrides` divergence test added. Cross-gen mapping hard-fail invariant enforced in `resolveContext`.
  - Status: Closed. Documented in Note, regression tests added. Executable evidence covers identity and divergence paths.
  - **Verification:** `cabal test --test-show-details=direct`

- [x] 11. Audit ga′ preservation across redirects
  - Confirm redirect-aware scope canonicalization and binding-parent projection preserve ga′.
  - Evidence: src/MLF/Elab/Run/Scope.hs — Note [ga′ preservation across redirects].
  - Result: GenRef passthrough is correct (gen nodes stable). TypeRef edge case handled by letScopeOverrides. chaseRedirectsStable is deterministic with cycle detection. Annotation rewriting covered by existing PipelineSpec tests. Direct `letScopeOverrides` divergence test added. Cross-gen mapping hard-fail invariant enforced in `resolveContext`.
  - Status: Closed. Documented in Note, regression tests added. Executable evidence covers identity and divergence paths.
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 12. Audit presolution binding-parent conflict handling
  - Check constraintForGeneralization conflict resolution preserves ga′ chain.
  - Evidence: src/MLF/Elab/Run.hs (constraintForGeneralization path).
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 13. Audit binding-parent stability through TyExp elimination
  - Confirm allocExpNode preserves binding-parent chains for TyExp.
  - Evidence: src/MLF/Frontend/ConstraintGen/Emit.hs:40-76.
  - **Verification:** `cabal test --test-show-details=direct`

- [x] 14. Add redirect/ga′ regression tests
  - Added 8 tests in test/ElaborationSpec.hs under "ga′ redirect stability":
    (1) TyExp redirect preserves ga′, (2) UF merge under same gen scope,
    (3) self-edge drop in canonicalizeBindParentsUnder, (4) end-to-end pipeline,
    (5) letScopeOverrides divergence, (6) letScopeOverrides identity,
    (7) ga-invariant negative (conflicting cross-gen), (8) ga-invariant positive (consistent).
  - Direct `letScopeOverrides` divergence test added. Cross-gen mapping hard-fail invariant enforced in `resolveContext`.
  - Files: test/ElaborationSpec.hs, src/MLF/Constraint/Presolution/Plan/Context.hs.
  - Status: Closed. Executable evidence covers identity and divergence paths.
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 15. Audit polymorphic let instantiation after redirects
  - Confirm tests cover apply-used-twice and redirected let-use sites.
  - Evidence: test/PipelineSpec.hs, test/ElaborationSpec.hs.
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 16. Audit annotated instantiation of polymorphic results
  - Confirm term annotation instantiation coverage and evidence.
  - Evidence: test/ElaborationSpec.hs.
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 19. Audit scheme-body root usage for ga′ generalization
  - Confirm schemeBodyTarget/bound-var targeting and redirect interactions.
  - Evidence: src/MLF/Elab/Run.hs:940-1040, src/MLF/Elab/Run.hs:1587-1608.
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 20. Audit bound-dependency ordering (real bound deps)
  - Confirm ≺ ordering respects bound dependencies.
  - Evidence: test/ElaborationSpec.hs, src/MLF/Elab/Generalize.hs:2185-2240.
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 21. Audit instantiate-then-forall composition when rebound
  - Confirm pipeline test coverage and code path.
  - Evidence: test/PipelineSpec.hs.
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 22. Audit binder filtering for scheme-body aliases
  - Confirm alias filtering logic coverage with passing tests.
  - Evidence: src/MLF/Elab/Generalize.hs:1750-2060.
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 23. Audit rigid-bound inlining conditions
  - Review display-only inlining vs generalization fallbacks for §15.3.2 alignment.
  - Evidence: src/MLF/Elab/Types.hs:162-210, src/MLF/Elab/Generalize.hs:2706-2779.
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 25. Audit bound dependency ordering through scheme-body aliases
  - Confirm bound dependency ordering follows bound chains through aliases.
  - Evidence: src/MLF/Elab/Generalize.hs:2185-2240, test/ElaborationSpec.hs.
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 26. Audit κσ baseline (ga′-scope named-node closure)
  - Confirm κσ baseline coverage and evidence.
  - Evidence: test/ElaborationSpec.hs.
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 27. Audit bound scoping for explicit-forall binders
  - Confirm explicit-forall bound preservation coverage.
  - Evidence: test/ElaborationSpec.hs.
  - **Verification:** `cabal test --test-show-details=direct`

- [x] 28. Audit ga′ selection & binding-parent projection under redirects
  - Verify ga′ selection and parent projection invariants under redirects.
  - Evidence: src/MLF/Elab/Run/Generalize.hs — Note [binding-parent projection — ga′ invariants].
  - Result: Phase 2 keepOld is safe (redirects merge structurally equivalent nodes sharing same gen ancestor). Phase 3 base-domain KeepOld takes priority. quotientBindParentsUnder "keep first parent" only applies to solved-domain quotient, not base-domain ga′. resolveContext Nothing→root fallback is correct for solver-introduced nodes. Direct `letScopeOverrides` divergence test added. Cross-gen mapping hard-fail invariant enforced in `resolveContext`.
  - Status: Closed. Documented in Note, regression tests added. Executable evidence covers identity and divergence paths.
  - **Verification:** `cabal test --test-show-details=direct`

- [ ] 29. Audit display-only bound inlining (§8.3.1)
  - Confirm inlineBoundsForDisplay usage in pretty display paths.
  - Evidence: src/MLF/Elab/Types.hs:162-210, src/MLF/Elab/Types.hs:295-305.
  - **Verification:** `cabal test --test-show-details=direct`
