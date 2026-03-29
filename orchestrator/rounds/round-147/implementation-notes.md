# Round 147 — Phase 6 Fix Alias-Bound Resolution for Recursive Types

## Problem

During Phase 6 (elaboration), `bindingFor` in `ReifyPlan.hs` rejected any
`TVar` alias bound with "alias bounds survived scheme finalization". This was
overly strict: in recursive types the presolution solver legitimately introduces
alias edges between co-recursive binders. These inter-binder alias bounds are
constraint-graph artefacts with no surface-language representation, and rejecting
them caused Phase 6 failures for μ/∀ interactions and higher-order recursive
types.

## Fix

In `bindingFor` (ReifyPlan.hs), the `mbBoundTyped` case for `Just (TVar v)` now
distinguishes:

- **Inter-binder alias bound** (`v ∈ binderNameSet`): normalized to `Right
  Nothing` (unbounded). This is a safe over-approximation — dropping an alias
  bound can only widen the instantiation set, never narrow it.
- **Non-binder alias bound** (`v ∉ binderNameSet`): rejected as before with
  `ValidationFailed`.

`binderNameSet` is computed as `Set.fromList (IntMap.elems subst)` — the set of
all binder names in the current generalization group's substitution.

## Thesis alignment

The thesis (§5, graphic constraints) permits unbounded quantification whenever a
tighter bound cannot be expressed in the target language. Inter-binder alias
bounds fall into this category: they record constraint-graph relationships that
have no xMLF type-language counterpart.

## Test changes

### PipelineSpec.hs
- **μ/∀ interaction test**: No longer blocked by alias-bound rejection. Phase 6
  succeeds; Phase 7 type-check hits `TCRollBodyMismatch` (a downstream issue in
  recursive roll/unroll elaboration, not caused by this fix). Test characterizes
  the new behavior: asserts "alias bounds survived" is gone, tolerates Phase 7
  failure.
- **Higher-order recursion test**: No longer blocked by alias-bound rejection.
  Now fails with `PhiTranslatabilityError` (missing authoritative instantiation
  translation for edge 0). Test characterizes: asserts "alias bounds survived"
  is gone, accepts the new error.

### ElaborationSpec.hs
- **`generalizeAt rejects alias bounds`** → renamed to **`generalizeAt
  normalizes inter-binder alias bounds to unbounded`**. Now expects success
  (Right) instead of failure (Left with "alias bounds survived").

## Files changed

| File | Change |
|------|--------|
| `src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs` | Added `binderNameSet`, updated `mbBoundTyped` to normalize inter-binder alias bounds, added Note block |
| `test/PipelineSpec.hs` | Updated μ/∀ and higher-order recursion tests to characterize new failure modes |
| `test/ElaborationSpec.hs` | Updated `generalizeAt` alias bounds test to expect success |

## Remaining downstream blockers

1. **TCRollBodyMismatch** (Phase 7): The μ/∀ interaction now passes Phase 6 but
   the elaborated recursive term fails type-checking. This is a separate issue
   in roll/unroll body alignment.
2. **PhiTranslatabilityError** (Phase 6): Higher-order recursion passes the
   alias-bound check but lacks an authoritative instantiation translation for
   edge 0. This is a separate Phi translation coverage gap.
