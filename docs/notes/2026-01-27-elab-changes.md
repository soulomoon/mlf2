# Elab change notes (2026-01-27)

This note captures the rationale behind recent elaboration adjustments that
followed removal of reify fallback and updated annotation handling.

## Note [No-fallback reify preserves explicit bounds]

We now prefer the no-fallback reify functions when a scheme already provides
explicit binders. This avoids inventing ancestor-quantifiers that would mask
explicit bounds and break annotation tests. The surrounding code assumes
quantifier structure comes from the scheme/witness, not from a fallback
ancestor lookup.

Affected code:
- MLF.Reify.Core: `reifyTypeWithNamedSetNoFallback`, `reifyTypeWithNamesNoFallback`
- MLF.Elab.Phi / MLF.Elab.Elaborate / MLF.Elab.Run.Pipeline: call sites that
  reify targets or bounds without fallback.

## Note [Scope-aware bound/alias inlining]

Bound/alias inlining must respect locally bound names. After removing fallback
reification, inlining a locally bound variable could collapse explicit forall
bounds to `TBottom` or inline an alias across a binder, which changes meaning.
All inlining traversals now track bound names and refuse to inline variables
that are locally bound in the current scope.

Affected code:
- MLF.Elab.Phi: `inlineAliasBoundsWith`
- MLF.Elab.Run.TypeOps: `inlineBoundVarsTypeWith`
- MLF.Elab.Run.Pipeline: `inlineAllBoundsType`
- MLF.Elab.Generalize: `inlineNamedBounds`

## Note [Instantiation arg sanitization]

Instantiation application payloads (`InstApp`) must not contain raw named vars
that would later be treated as bounds. We sanitize instantiation arguments by
inlining bound variables (or mapping them to `TBottom` when appropriate) so
`elabToBound` never sees a stray named variable.

Affected code:
- MLF.Elab.Elaborate: `expansionToInst` uses `sanitizeArg`
- MLF.Elab.Run.Annotation: `sanitizeBoundTop`

## Note [Annotation instantiation preserves foralls]

Explicit annotations should not eliminate binders; they should update bounds
instead. We therefore interpret annotation instantiation as an inside-bound
update (`InstInside (InstBot ...)`) and drop `InstElim`. This preserves nested
quantifiers in explicit annotations.

Affected code:
- MLF.Elab.Run.Annotation: `adjustAnnotationInst`

## Note [Skip target-derived instantiation when bounds include foralls]

If the instantiation payload or target type contains forall bounds, we avoid
building instantiation arguments from the target type because doing so erases
explicit bounds. In that case we keep the witness-derived instantiation.

Affected code:
- MLF.Elab.Run.Pipeline: `targetHasBoundForall` and `phiFromTarget`

## Note [Verification]

- `cabal test` (2026-01-27) passed after these changes.
