# Requirements

## Goal
Represent all explicit `forall` binders from source annotations and schemes as gen-bound named nodes (not TyForall-bound), so scheme closure and subterm typing are thesis-exact without free-name exemptions. Thesis: `papers/these-finale-english.txt` §15.3.1–§15.3.2, Figure 15.3.2, Definitions 15.3.1–15.3.2.

## User Stories
- As a developer, I want explicit `forall` binders to be treated as named nodes under a gen node so Sχ′p/Γa matches the thesis and generalization remains strict. (Thesis: §15.3.1–§15.3.2, Fig. 15.3.2)
- As a maintainer, I want all annotation-related tests to reflect the gen-bound encoding rather than TyForall anchors.

## Acceptance Criteria (EARS)
1.1 When internalizing a source `STForall` (including bounds inside `SrcScheme`), the implementation shall create gen-bound named nodes for its binders instead of TyForall-bound binders. (Thesis: §15.3.1, named nodes)
1.2 The implementation shall no longer rely on TyForall-bound binders for explicit annotations; any necessary scheme structure shall be encoded via gen nodes and scheme roots. (Thesis: §15.3.1–§15.3.2, Fig. 15.3.2)
1.3 The implementation shall keep SchemeFreeVars/GenSchemeFreeVars strict; no explicit-forall free-name exemptions remain. (Thesis: §15.3.1–§15.3.2)
1.4 The bounded-aliasing annotated-let baseline shall pass without any free-name exemptions.
1.5 Generalization and reification shall treat explicit-forall gen nodes as their own scheme scopes so rank-2 bounds are preserved and outer schemes do not capture their binders. (Thesis: §15.3.1–§15.3.2, Fig. 15.3.2)
1.6 Tests that previously asserted TyForall anchors for explicit annotations shall be updated to the new gen-bound representation.
1.7 The full test suite shall pass.
1.8 Scope selection for generalization shall use the gen node of the subterm (`ga′`) as defined in §15.3.2, i.e. the nearest gen ancestor in the binding tree for the subterm root (or the root itself if none). No heuristic fallback based on reachable binders or scheme owners shall be used. (Thesis: §15.3.2, Def. 15.3.2)
1.9 Generalization shall compute Typ(a′) exactly as ∀(Γa) Sχ′p(hga′·1i), where Γa enumerates named nodes bound on ga′ and uses Sχp for their bounds, including the scheme root when it is named. (Thesis: §15.3.1–§15.3.2, Fig. 15.3.2, Def. 15.3.1–15.3.2)
1.10 Generalization shall remain polymorphic for let-bound terms even when the subterm root is redirected/canonicalized; repeated instantiation (e.g., apply used twice) shall yield fresh types consistent with §15.3.2. (Thesis: §15.3.2, Typ(a′))
1.11 Presolution binding-parent conflict handling shall be thesis-exact: when redirects/canonicalization introduce binding-parent conflicts, prefer the binding-parent chain that preserves the original `ga′` scope (gen ancestor of the subterm root), and eliminate self-parent edges introduced by redirects. (Thesis: §15.3.2, binding tree for `ga′`)
1.12 Explicit `forall` binders that appear inside bounds shall remain scoped under their owning gen (not hoisted into `ga′` quantifiers), and binding parents for bound-scheme roots shall survive constraint projection so `Sχp` can observe nested schemes. (Thesis: §15.3.1–§15.3.2, Fig. 15.3.2)
1.13 When presenting types for display, the implementation shall optionally inline bounds per §8.3.1 (display-only) while leaving the core scheme unchanged. (Thesis: §8.3.1)

## Non-goals
- Do not change witness normalization, instantiation semantics, or constraint solving beyond the representation change.
- Do not relax binding-tree invariants.

## References
- `papers/these-finale-english.txt` §15.3.1–§15.3.2 (named nodes, Sχ′p, Typ(a′), `ga′`).

## Thesis Audit Status (Strict)
Aligned:
- Explicit `STForall` binders are encoded as gen-bound named nodes (matches named-node treatment for schemes in §9.1.3/§15.3.1).
- Scheme closure checks remain strict; no explicit-forall exemptions remain.
- Annotated λ parameters now preserve rank-2 argument types (e.g. `(∀a. a -> a) -> ...`) rather than lifting to a top-level ∀.

Deviations / Open Gaps:
- `bindingScopeRef` still uses a pragmatic scope-selection fallback (reachable-binder / nearest-gen heuristics) rather than the thesis rule that the scope is exactly `ga′` (see §15.3.2, `src/MLF/Elab/Run.hs`).
- Redirected/canonicalized nodes may lose binding-parent chains, making `ga′` selection unstable across presolution/solve; binding parents must be projected onto redirected ids to remain thesis-exact.
- Polymorphic let generalization still fails in the pipeline when the let-body root is redirected, causing monomorphic instantiations in `apply used twice`.
- Scope selection contains a scheme-root shortcut for let-bound terms when the root is redirected; this bypasses the binding-parent chain and is not thesis-exact.
- Binding-parent projection currently prefers solved parents over base parents, which can preserve self-parents introduced by redirects; these must be corrected so ga′ is stable.
- Explicit-forall binders inside bounds can be lifted into outer `ga′` quantifiers when bound scheme roots lose their binding parents; the nested forall structure must remain intact per Fig. 15.3.2.
