# Findings

- High: ga′ scope selection and redirect projection are still not demonstrably thesis-exact. bindingScopeRef/preferGenScope and pre/post scope mapping are used with redirect-aware fallbacks, but there’s no proof they match Def. 15.3.2 for all redirect/canonicalization cases. Evidence: src/MLF/Elab/Run.hs:1560-1616 (bindingScopeRef/preferGenScope), src/MLF/Elab/Run.hs:900-918 (pre/post scope handling), and redirect-aware scope canonicalization in src/MLF/Elab/Run.hs:840-900.
- Medium: Typ(a′) is likely close but still has scheme-scope fallback behavior that can bypass the strict “∀(Γa) Sχ′p(hga′·1i)” rule when a scheme owner differs from the chosen scope. Evidence: src/MLF/Elab/Generalize.hs:2706-2768 (schemeScope fallback), plus reification paths src/MLF/Elab/Generalize.hs:2755-2781 using reifyTypeWithNamesNoFallback*.
- Medium: Redirect regression tests are still missing; the suite passes but there isn’t a dedicated test exercising ga′ selection stability across redirects. This leaves tasks 14/28 unverified (no new redirect-focused tests in test/ElaborationSpec.hs/test/PipelineSpec.hs).
- Low: Rigid-bound inlining conditions for Typ(a′) side-conditions are not explicitly audited; display inlining exists, but the generalization path still has several fallbacks that might inline more than §15.3.2 allows. Evidence: src/MLF/Elab/Types.hs:162-210 (display-only inlining), generalization fallbacks src/MLF/Elab/Generalize.hs:2706-2779.

# Remaining unchecked tasks — coverage assessment

- 9. Typ(a′) construction (Sχ′p + Γa) — Partially covered, needs thesis audit. Uses reifyTypeWithNamesNoFallback* for Sχ′p and reifyBoundWithNames* for bounds, but schemeScope fallback can change the scope root. Evidence: src/MLF/Elab/Generalize.hs:2706-2779, src/MLF/Elab/Reify.hs:649-715.
- 10. Thesis-exact ga′ scope selection — Partially covered. bindingScopeRef uses nearest gen ancestor, but preferGenScope and pre/post scope reconciliation remain. Evidence: src/MLF/Elab/Run.hs:1560-1616, src/MLF/Elab/Run.hs:900-918.
- 11. Preserve ga′ across redirects — Partially covered. There is redirect-aware scope canonicalization and binding-parent projection, but not formally verified. Evidence: src/MLF/Elab/Run.hs:840-918.
- 12. Presolution binding-parent conflicts — Partially covered. Conflict resolution exists in constraint projection, but no proof that it always preserves ga′. Evidence: src/MLF/Elab/Run.hs (constraintForGeneralization path), plus binding-parent projections.
- 13. Binding parents through TyExp elimination — Likely covered but not audited. allocExpNode preserves binding parent chains (emit/translate). Evidence: src/MLF/Frontend/ConstraintGen/Emit.hs:40-76.
- 14. Redirect/ga′ regression tests — Not covered. No explicit new tests for redirect stability and ga′ selection in test/ElaborationSpec.hs / test/PipelineSpec.hs.
- 15. Polymorphic let instantiation after redirects — Likely covered (tests pass). Tests “apply used twice” and redirected let-use sites pass. Evidence: test/PipelineSpec.hs and test/ElaborationSpec.hs passing suite.
- 16. Annotated instantiation of polymorphic results — Likely covered (tests pass). “term annotation can instantiate a polymorphic result” passes. Evidence: test/ElaborationSpec.hs.
- 19. Use scheme-body root for ga′ generalization — Partially covered. schemeBodyTarget and bound-var targeting are used in Run; still no redirect-specific test. Evidence: src/MLF/Elab/Run.hs:940-1040, src/MLF/Elab/Run.hs:1587-1608.
- 20. Bound-dependency ordering (real bound deps) — Likely covered (tests pass). Ordering tests pass; still complex to audit. Evidence: test/ElaborationSpec.hs (≺ ordering tests), binder dependency logic in src/MLF/Elab/Generalize.hs:2185-2240.
- 21. Instantiate-then-forall composition when rebound — Likely covered (tests pass). Pipeline test passes. Evidence: test/PipelineSpec.hs “composes instantiate then forall when rebound”.
- 22. Relax binder filtering for scheme-body aliases — Likely covered (tests pass). Alias filtering logic is extensive; tests now passing. Evidence: src/MLF/Elab/Generalize.hs:1750-2060.
- 23. Tighten rigid-bound inlining — Not audited. Display inlining exists; generalization inlining still uses fallbacks. Evidence: src/MLF/Elab/Types.hs:162-210, src/MLF/Elab/Generalize.hs:2706-2779.
- 25. Bound dependency ordering through scheme-body aliases — Likely covered (tests pass). Ordering tests pass but alias-path audit still needed. Evidence: src/MLF/Elab/Generalize.hs:2185-2240, test/ElaborationSpec.hs.
- 26. κσ baseline (ga′-scope named-node closure) — Likely covered (tests pass). κσ baseline test passes. Evidence: test/ElaborationSpec.hs.
- 27. Bound scoping for explicit-forall binders — Likely covered (tests pass). Explicit-forall bound preservation tests pass. Evidence: test/ElaborationSpec.hs.
- 28. ga′ selection & binding-parent projection under redirects — Partially covered. Same risk as tasks 10/11; no redirect-specific regression tests yet.
- 29. Display-only bound inlining (§8.3.1) — Covered. inlineBoundsForDisplay is applied in pretty display of types and schemes. Evidence: src/MLF/Elab/Types.hs:162-210, src/MLF/Elab/Types.hs:295-305.

# Open questions / assumptions

- Do you want me to mark any of the above as done based solely on passing tests, or should we require a targeted thesis audit (or new tests) before checking them off?
- For tasks 10/11/28, should I draft a minimal redirect-focused test to make the ga′ selection rule observable?
