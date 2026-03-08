# Thesis-Exact Fallback Rework Design

## Goal

Make the remaining fallback-like behavior in elaboration/generalization strictly thesis-exact by removing compatibility recovery and keeping only witness-/scheme-authoritative paths. Any case that cannot be justified by those authoritative inputs must fail explicitly.

## Thesis Anchors

- `papers/these-finale-english.txt` Definition 15.3.12 — translation of an instantiation edge from the chosen propagation witness.
- `papers/these-finale-english.txt` §15.3.6 — elaboration from a translatable presolution.
- Repository rule: `papers/these-finale-english.txt` is the source of truth; when the thesis is silent, supplementary detail may come from `papers/xmlf.txt`, but no compatibility recovery should be kept silently.

## Current Audit Findings

The current tree is green, but not fully thesis-exact.

Residual live behavior still includes:

1. Let-level fallback scheme selection in `src/MLF/Elab/Elaborate.hs`
   - `fallbackChoiceFromVar`
   - `fallbackChoiceFromApp`
   - `fallbackChoiceFromLam`
   - `fallbackChoice`
   - subsequent `schChosen` / `substChosen` selection
2. Secondary `reifyInst` refinement in `src/MLF/Elab/Elaborate.hs`
   - `targetArgs <|> expansionArgs`
3. Recursive generalization fallback callback in `src/MLF/Elab/Run/Generalize.hs`
   - `fallback scope' target' = fst <$> go mbGa scope' target'`
4. Recursive scheme-type fallback in `src/MLF/Elab/Generalize.hs`
   - `generalizeAtForScheme schemeScope typeRootC`

These are narrower than the previously removed GA→no-GA→reify ladders and trace-helper recovery, but they still violate the approved strictness target.

## Chosen Approach

Use a surgical strictness pass.

- Remove only the residual live fallback behaviors.
- Preserve the existing translatable-presolution / witness-authoritative structure.
- Replace any remaining compatibility recovery with structured failure.
- Prove the new behavior with semantic tests, not only marker-string guards.

This is the least risky path that still meets the thesis-exact goal.

## Design

### 1. Let Generalization Becomes Single-Path

`ALet` elaboration must accept exactly one authoritative generalization result.

- Keep the existing generalization entrypoint used for let-bound schemes.
- Remove all RHS-derived or environment-derived fallback candidate construction.
- Remove coherence-based chooser logic that picks among alternate schemes.
- If the authoritative scheme is unusable for closure/alignment, surface the corresponding elaboration/generalization error rather than replacing the scheme.

This means the let body sees the single generalized scheme produced by the authoritative path, not a compatibility-shaped alternative.

### 2. Generalization Stops Recursing Through Fallback Scopes

The runtime generalization layer must not retry through alternate scheme scopes or recursive callbacks.

- Remove the fallback callback parameter from `applyGeneralizePlan`.
- Make scheme-type reconstruction use only explicit structural authority already computed in the plan.
- If a required scheme owner/scope cannot be justified directly, fail with a structured error.

This prevents hidden “generalize somewhere else and reuse that result” behavior.

### 3. `reifyInst` Becomes Witness/Domain-Only

`reifyInst` must fail unless witness/domain authority alone is sufficient.

- Keep `phiFromEdgeWitnessWithTrace` and direct witness/domain interpretation.
- Remove secondary refinement via `expansionArgs`.
- Remove any alternate source that is not directly justified by the witness/domain path.
- If the authoritative path yields `InstId` while the edge still demands a nontrivial instantiation, raise `PhiTranslatabilityError`.

Per the approved decision, even the current narrow `targetArgs <|> expansionArgs` recovery is too permissive and must go.

### 4. Tests Become Semantic

The current guard stack catches deleted names but misses surviving behavior. Add semantic tests that fail on behavior, not only symbols.

Required semantic slices:

- a let-elaboration case that only succeeds when the fallback chooser rewrites the scheme shape;
- a `reifyInst` case that only succeeds when expansion-based recovery fills missing authoritative instantiation arguments;
- a generalization case that only succeeds when recursive scheme fallback is allowed.

These tests should assert strict failure after the patch.

### 5. Documentation Must Match Reality

Update:

- `TODO.md`
- `implementation_notes.md`
- `docs/architecture.md`
- `CHANGELOG.md`
- task artifacts under `historical task-tracker path (not retained as a live folder)`

The closeout must describe the actually strict behavior and record any discovered keeper explicitly. Silent residual fallback is not allowed.

## Non-Goals

- Do not change `checkNoGenFallback`.
- Do not remove `NoFallback` reify entrypoints.
- Do not broaden the campaign into unrelated `Solved`/result-type cleanup.
- Do not preserve compatibility behavior unless thesis evidence requires it and the deviation is documented.

## Error Policy

Prefer explicit structured failure over recovery:

- `SchemeFreeVars`-class failures surface directly.
- missing owner/scope authority surfaces directly.
- missing authoritative instantiation translation surfaces directly.

## Verification Strategy

1. Add failing semantic tests for each residual family.
2. Remove the behavior with minimal production edits.
3. Run focused slices after each family.
4. Run `cabal build all && cabal test` at the end.
5. Sync docs only after the code/test story is true.

## Approval Record

Approved by user on 2026-03-08 with the additional explicit requirement:

> remove `reifyInst` secondary recovery too and make it fail unless the witness/domain path alone is sufficient.
