# Findings

## Current Observations

- Current focused validation is green:
  `cabal test mlf2-test --test-show-details=direct --test-options='--fail-fast'`
  reports 1582 examples, 0 failures;
  `MLF.Program execution corpus` reports 9 examples, 0 failures; and
  `MLF.Program` reports 34 examples, 0 failures.
- Final serial gates are green:
  `cabal test` (1582 examples, 0 failures), `cabal build all`, and
  `cabal test` again (1582 examples, 0 failures).
- Public-surface guards are green:
  `authoritative-overloaded-method.mlfp -> true` and
  `authoritative-case-analysis.mlfp -> 1`.
- The earlier `MLF.Program` failure clusters are resolved without adding a
  direct `.mlfp -> ElabTerm` fallback route, new `.mlfp` syntax, permissive
  `EUnroll`, or broad `TypeCheck` weakening.
- `.mlfp` still flows through the shared eMLF/typecheck boundary; no separate
  direct Program-layer elaboration route was added.
- Producer-side closure freshening in `MLF.Elab.Run.Pipeline` removes the
  `TCTypeAbsVarInScope "b"` failure for `recursive-list-tail`.
- The recursive-ADT corpus is green after local producer fixes:
  `freshenTypeAbsAgainstEnv` reserves lambda parameter types, let schemes, and
  nested type-abstraction bounds; rank-2 result fallback inlines raw bound
  variables before `elabToBound`; synthetic constructor handler types strip
  foralls that become vacuous after recursive ADT lowering.
- `recursive-existential` specifically failed because `forall a. Expr a ->
  $SomeExpr_result` lowered to a handler type where `a` was vacuous but the
  outer Church result was still free. Stripping the vacuous constructor forall
  prevents Phase 6 from trying to generalize a scheme with an out-of-owner
  result node.
- The public constructor-case guard needs canonical Church result naming for
  non-existential constructor groups, but existential constructor groups keep
  their explicit `$Type_result` name to avoid capture in nested existential
  contexts.
- Final closure pruning removes vacuous leading bounded abstractions from
  xMLF output; `church-true` and `choose` goldens now expect the non-vacuous
  `forall a. forall b. a -> b -> a` shape.
