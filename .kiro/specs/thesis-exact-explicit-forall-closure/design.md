# Design

## Summary
Align scheme closure with the thesis by ensuring explicit-forall binders in annotated schemes are bound under the scheme’s gen node. This removes the need for free-name exemptions in generalization and makes Sχ′p/Γa behavior match §15.3.1–§15.3.2.

## Thesis Alignment
- §15.3.1: named nodes are exactly the flexibly bound type nodes on gen nodes.
- §15.3.2: Typ(a′) uses ∀(Γa) Sχ′p(hga′·1i); Γa only binds named nodes under ga′.
- Therefore, any variables referenced by a scheme body must either be named nodes under that gen or be inlined by Sχ′p. We will achieve this by rebinding explicit-forall variables under the scheme gen.

## Approach
1) Fix binding structure for annotated schemes
- In constraint generation for `ELetAnn`, after constructing the explicit scheme root (`schemeRoot`) and `schemeGen`, rebind any quantifier variables introduced by the explicit scheme so they are bound under `schemeGen` (not under outer forall nodes that sit above it).
- This can be done by traversing the explicit scheme structure to find the quantified variable nodes and setting their bind parent to `GenRef schemeGen` with `BindFlex`, consistent with named nodes being flexibly bound under a gen node.
- Ensure this does not disturb rigid binding of the scheme root itself.

2) Remove generalization exemption
- Delete the “type-bound outside” allowance in `generalizeAt` that permits free names for vars bound under explicit `TyForall` outside the current gen.
- Keep strict SchemeFreeVars behavior otherwise unchanged.

## Key Files
- `src/MLF/Frontend/ConstraintGen/Translate.hs` (annotated let translation / binding adjustments)
- `src/MLF/Elab/Generalize.hs` (remove explicit-forall free-name exemption)
- `test/ElaborationSpec.hs` (bounded-aliasing annotated let case)

## Error Handling
- If rebinding creates an invalid binding tree, rely on existing binding checks; no new error types needed.

## Testing Strategy
- Existing bounded-aliasing test must pass without free-name exemptions.
- Run full suite to ensure no regressions in elaboration/generalization.

## Risks
- Rebinding quantifier vars under the scheme gen could alter scope relationships if those vars were intended to be shared across multiple schemes. Limit changes to annotated schemes only.
- Must preserve ordering (<P) and avoid cycles; rely on existing `setBindParentOverride` and validation.
