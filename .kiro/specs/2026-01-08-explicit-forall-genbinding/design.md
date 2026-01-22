# Design

## Summary
Refactor explicit `forall` representation so binders are gen-bound named nodes. This aligns with §15.3.1’s definition of named nodes and ensures Γa/Typ(a′) are computed exactly as in §15.3.2 without ad hoc generalization exemptions. Thesis: `papers/these-finale-english.txt` §15.3.1–§15.3.2, Figure 15.3.2.

## Thesis Alignment
- Named nodes: flexibly bound type nodes under gen nodes. (Thesis §15.3.1)
- Translation Sχp/Sχ′p: named nodes translate to variables (Sχ′p), with bounds computed by Sχp. (Thesis Fig. 15.3.2)
- Typ(a′): ∀(Γa) Sχ′p(hga′·1i); Γa enumerates named nodes bound on ga′ ordered by <P. (Thesis §15.3.2, Def. 15.3.1–15.3.2)
- Therefore explicit binders must be introduced as gen-bound named nodes, and scheme roots that are named must be quantified with their Sχp bounds rather than inlined. (Thesis §15.3.1–§15.3.2)

## Approach
1) Internalize explicit `forall` via gen nodes
- In `internalizeSrcTypeWith` for `STForall`, introduce a fresh gen node to represent the scheme, bind the quantifier variable under that gen node, and treat the scheme body as the scheme root.
- The returned type node for the `STForall` should be the scheme root with a binding parent to the gen node, so expansion/instantiation can use the existing gen-node machinery.
- Apply the same encoding for scheme bounds in `internalizeBinders` so `SrcScheme` bounds are also gen-bound.

2) Update annotation/constraint expectations
- Tests that assert TyForall anchors for explicit annotations should be updated to assert the new scheme-root shape.
- Ensure any helper assumptions about explicit foralls are adjusted (e.g., “binds explicit forall variables to their TyForall”).

3) Keep strict generalization
- Remove any explicit-forall free-name exemptions (already intended by previous spec) and rely on gen-bound binders to satisfy closure.

4) Preserve explicit-forall scoping in generalization/reification
- In `generalizeAt`, compute Typ(a′) per §15.3.2: quantifiers are the named nodes bound on ga′ (Γa), and the body is Sχ′p(hga′·1i). If the scheme root is a named node, the result should be `∀(α ⩾ Sχp(root)) α` (not `α` or an inlined bound).
- Ensure `reifyTypeWithNamesNoFallback` implements Sχ′p (named nodes become variables) and `reifyBoundWithNames` implements Sχp (bounds may inline), mirroring Figure 15.3.2.
- Keep binding-tree invariants intact; avoid gen-ancestor fallback by using direct ga′ binders. (Thesis §15.3.1–§15.3.2, Fig. 15.3.2)

5) Preserve rank-2 lambda annotation types
- Do not desugar `ELamAnn` into `let`+`EAnn`. Translate annotated parameters directly so the parameter type is the annotated scheme (e.g., `(∀a. a -> a) -> ...`).

6) Thesis-exact scope selection for generalization
- Use the gen node of the subterm (`ga′`) as the scope root for generalization, per §15.3.2.
- Implement this as: follow the binding-parent chain from the subterm root and select the first `GenRef` encountered; if none exists, the scope is the root itself.
- Ensure subterm roots are bound under their corresponding gen nodes so the nearest-gen rule is well-defined. (Thesis §15.3.2, Def. 15.3.2)

7) Preserve `ga′` across redirects/canonicalization
- When presolution redirects/canonicalization replace the subterm root, project binding parents from the pre-solve graph onto the solved graph for redirected node ids.
- Use the pre-redirect subterm root to select `ga′`, then map that binding-parent chain forward through redirects before calling `generalizeAt*`.
- Only fill missing `cBindParents` in the solved graph; do not overwrite binding parents already set by presolution/solve.
- If redirects introduce binding-parent conflicts (or self-parent edges), prefer the chain that preserves the original `ga′` and drop self-parent edges. (Thesis §15.3.2, binding tree for `ga′`)

7a) Presolution binding-parent conflict cleanup
- Normalize binding-parent chains after redirects by: (1) removing self-parents, (2) selecting the chain that yields the same ga′ as the pre-redirect root, and (3) only overriding solved parents when they are redirect artifacts. (Thesis §15.3.2)

7b) Preserve bound scheme scope for explicit foralls
- Ensure explicit-forall binders that appear inside bounds remain under their own gen nodes and are not treated as ga′-children during Typ(a′) computation.
- When projecting binding parents through presolution/solve, preserve binding parents for bound scheme roots so `Sχp` can see nested schemes and reify them into bounds rather than hoisting them.
- Avoid overriding binding parents for nested scheme roots owned by other gens when repairing the binding tree. (Thesis §15.3.1–§15.3.2, Fig. 15.3.2, Def. 15.3.1)

8) Keep polymorphic let instantiations after redirects
- Validate that let-bound generalization uses the `ga′` scope derived from the pre-redirect root so schemes remain polymorphic. (Thesis §15.3.2, Typ(a′))
- Ensure instantiation uses the generalization-oriented solve result rather than a witness-only solve, so repeated instantiations (e.g. apply used twice) remain fresh. (Thesis §15.3.2)

9) Display-only inlining for presentation
- Provide a display pass that inlines bounds per §8.3.1 when rendering types, while keeping the core scheme unchanged.
- Apply the display pass only in pretty-printing (user-facing output/tests expecting ML-style types), not in generalization/reification logic. (Thesis §8.3.1)

## Key Files
- `src/MLF/Frontend/ConstraintGen/Translate.hs`
- `src/MLF/Frontend/ConstraintGen/Translate.hs` (internalizeSrcType/internalizeBinders)
- `src/MLF/Elab/Generalize.hs`
- `src/MLF/Elab/Reify.hs`
- `test/ConstraintGenSpec.hs`
- `test/ElaborationSpec.hs`

## Error Handling
- Binding-tree checks should catch invalid gen parent assignments; no new error types required.

## Testing Strategy
- Update constraint-gen tests to the new explicit-forall encoding.
- Fix elaboration/generalization tests that depend on explicit-forall scoping (rank-2 bounds, Q(g), ≺ ordering).
- Re-run the bounded-aliasing baseline and full suite.

## Risks
- Re-encoding explicit foralls may affect instantiation edges; ensure returned scheme roots remain instantiable under existing presolution logic.
- Must preserve <P ordering and avoid binding cycles.
- If scheme-root bounds are not propagated into Γa, Typ(a′) degenerates to ∀α. α (monomorphic in practice); this is explicitly non-thesis-aligned. (Thesis §15.3.2, Fig. 15.3.2)

## Thesis Audit Findings (Strict)
Matches thesis intent:
- Explicit foralls are represented as gen-node schemes (named-node alignment with §9.1.3/§15.3.1).
- Reify avoids inlining explicit-forall binders under gen nodes so rank-2 bounds are preserved where present (`src/MLF/Elab/Reify.hs:236`).
- `bindingScopeRef` now picks the nearest gen ancestor (`ga′`) from the binding-parent chain (`src/MLF/Elab/Run/Scope.hs:32-37`).

Deviations / heuristics to document:
- Expansion copying widens the interior with reachable scheme interiors to keep explicit-forall binders inside instantiation copies (`src/MLF/Constraint/Presolution/Copy.hs:122`). This is a preservation measure not stated in the thesis text.
- Redirected/canonicalized nodes currently drop binding-parent chains; until projected, `ga′` selection can diverge from thesis when TyExp nodes are eliminated.
- Let-bound generalization can become monomorphic after redirects; investigate scope projection and solve result usage for instantiation.
- Scheme-root shortcuts for redirected let bodies bypass the binding-parent chain; thesis mandates ga′ from the subterm root.
- Binding-parent projection still preserves self-parents introduced by redirects; these must be repaired so ga′ is stable. (Thesis §15.3.2)
- Scheme-root named nodes are being treated as unbounded in generalization; per Fig. 15.3.2, they must be quantified with Sχp bounds to yield Typ(a′).
- Explicit-forall bound schemes can be hoisted into outer quantifiers when bound roots lose their binding parents; this violates Fig. 15.3.2’s nested Sχp structure.
