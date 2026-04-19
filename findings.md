# Findings

## Requirements
- Implement the proposed deferred-obligation refactor for `.mlfp`.
- Move all 7 `emlfPendingSuccessMatrix` rows into strict positive coverage.
- Keep public raw eMLF syntax/parser clean and preserve the shared eMLF pipeline route.
- Preserve fail-closed behavior for bare methods, missing/duplicate instances,
  bad constructors, and non-data cases.

## Current Observations
- The method-only deferred map has been replaced by a unified obligation map
  covering methods, constructors, and cases.
- Public eMLF pipeline APIs remain tuple-returning; `.mlfp` finalization uses
  the new internal detailed entrypoint.
- Ordinary ADT constructor applications now lower through typed placeholders.
  Nullary constructor values remain inline Church values because they are values,
  not applications. GADT/existential constructor applications stay on the direct
  Church path until the obligation metadata carries constructor-local `forall`
  evidence; runtime constructor definitions remain the Church-encoded executable
  bindings.
- The former pending-success rows have moved into strict positive coverage.
- Recursive overloaded method calls in explicit and derived `Eq Nat` instances
  stay covered: instance method bodies finalize as their runtime bindings,
  method runtime resolution remains deferred, and constructor placeholders are
  rewritten during constructor finalization.

## Technical Decisions
| Decision | Rationale |
|----------|-----------|
| Refactor the method-only map into a sum-typed obligation map | Constructor, case, and method resolution need one ordered post-inference pass |
| Resolve constructor/case obligations before methods | Method instance recovery can depend on source ADT heads recovered after constructor/case rewrites |
| Keep full validation gate at the end | Behavior-changing Haskell work under repo guidelines requires `cabal build all && cabal test` |
| Add an internal detailed pipeline result in `MLF.Elab.Run.Pipeline` | Program finalization needs the term, type, annotated root, and typecheck env while public pipeline APIs remain unchanged |
| Keep source-known method argument type guidance but defer runtime selection | Large constructor arguments need expected-type guidance, while instance choice still belongs to post-eMLF finalization |
