# Findings

## Requirements
- Implement the proposed deferred-obligation refactor for `.mlfp`.
- Move all 7 `emlfPendingSuccessMatrix` rows into strict positive coverage.
- Carry constructor-local `forall` evidence in deferred constructor
  obligations so GADT, existential, nullary, and ordinary source constructor
  uses all resolve after eMLF inference.
- Keep public raw eMLF syntax/parser clean and preserve the shared eMLF pipeline route.
- Preserve fail-closed behavior for bare methods, missing/duplicate instances,
  ambiguous constructors, bad constructors, and non-data cases.

## Current Observations
- The method-only deferred map has been replaced by a unified obligation map
  covering methods, constructors, and cases.
- Public eMLF pipeline APIs remain tuple-returning; `.mlfp` finalization uses
  the new internal detailed entrypoint.
- Every source constructor occurrence now lowers through a deferred placeholder;
  runtime constructor definitions remain Church-encoded executable bindings.
- Deferred constructor metadata now records expected-type seeds, occurrence
  type, implicit/runtime instantiation binder order, constructor-local
  forall binders, and placeholder binding mode.
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
| Treat constructor and case placeholders as inference-local bindings | Their source types are retained for finalization, but eMLF does not need recursive μ structures as external schemes |
| Skip generated constructor bindings whose local forall evidence cannot be recovered | These constructors cannot be instantiated into runtime bindings safely; source use fails with a constructor-specific ambiguity |

## 2026-04-19 Constructor-local forall slice
- Public pipeline APIs remain unchanged. `.mlfp` uses an internal detailed
  external-binding entrypoint and an unchecked variant only when program
  placeholders must be finalized before the guard typecheck.
- Constructor obligations now solve substitutions from expected-type seeds,
  eMLF head instantiations when present, rewritten argument types, and
  occurrence result types. Argument/result matching contributes evidence but
  leaves final mismatch detection to the final typecheck guard.
- Deferred cases are lowered directly as `casePlaceholder scrutinee handlers`
  so constructor finalization and case finalization see the same occurrence tree.
- Focused `MLF.Program eMLF` and broader `MLF.Program` suites pass with new
  strict rows for ordinary nullary/nested constructors, GADT indexed
  constructors, existential constructors, nullary indexed constructors,
  explicit polymorphic nullary constructors, and unrecoverable constructor-local
  forall ambiguity.
- Full validation passes after the repository guard was updated to require the
  external-binding detailed/unchecked internal pipeline markers and the final
  post-rewrite typecheck guard.
