# Findings

## High-confidence non-candidates

- `checkNoGenFallback` is explicitly documented as **thesis-faithful defense-in-depth**, not a deviation or compatibility fallback. It should stay as a guardrail.
  - Evidence: `src/MLF/Binding/Validation.hs:185`
  - Supporting register note: `CHANGELOG.md:417`
- The `NoFallback` reify entry points in `MLF.Reify.Core` are not fallback behavior to remove; they are explicit stricter variants used to avoid ancestor fallback quantifiers.
  - Evidence: `src/MLF/Reify/Core.hs:41`, `src/MLF/Reify/Core.hs:697`, `src/MLF/Reify/Core.hs:756`
- `MLF.Elab.Run.ResultType.Fallback` is partly a naming artifact: it is the non-annotation result-type path, not automatically a thesis deviation by itself.
  - Evidence: `src/MLF/Elab/Run/ResultType/Fallback.hs:56`

## Strong removal candidates

### 1. Generalization fallback ladders
These are the clearest remaining live fallback mechanisms.

- `MLF.Elab.Elaborate` still does:
  - GA generalization attempt
  - then `Nothing`/no-GA retry
  - then direct `reifyType` / `reifyNodeTypePreferringBound`
  - Evidence: `src/MLF/Elab/Elaborate.hs:138`, `src/MLF/Elab/Elaborate.hs:146`, `src/MLF/Elab/Elaborate.hs:177`, `src/MLF/Elab/Elaborate.hs:184`
- `MLF.Elab.Run.Pipeline` still does the same root-level ladder:
  - GA attempt
  - then no-GA retry
  - then direct `reifyType`
  - Evidence: `src/MLF/Elab/Run/Pipeline.hs:150`, `src/MLF/Elab/Run/Pipeline.hs:157`, `src/MLF/Elab/Run/Pipeline.hs:162`
- `MLF.Elab.Run.ResultType.Util.generalizeWithPlan` mirrors that same ladder:
  - GA attempt
  - then no-GA retry
  - then `fallbackToReify`
  - Evidence: `src/MLF/Elab/Run/ResultType/Util.hs:30`, `src/MLF/Elab/Run/ResultType/Util.hs:41`, `src/MLF/Elab/Run/ResultType/Util.hs:48`
- `MLF.Elab.Generalize` still contains a deeper `fallbackSchemeType` / `fallbackTy` ladder around rigid-bound recovery.
  - Evidence: `src/MLF/Elab/Generalize.hs:515`, `src/MLF/Elab/Generalize.hs:588`, `src/MLF/Elab/Generalize.hs:663`

Assessment:
- These are the strongest remaining candidates if the goal is stricter thesis-exactness.
- They are not obviously dead: docs and bugs show they were deliberately aligned for stability and sentinel control, not already proven redundant.
- Removing them would be semantics-changing and needs a fresh paper audit plus focused sentinels.

### 2. `reifyInst` / annotation-local fallback paths
- `MLF.Elab.Elaborate` still contains explicit fallback logic around `reifyInst`, `resolveFallbackArgNodes`, `allowFallbackFromTrace`, and localized fallback reification for certain annotation-edge situations.
  - Evidence: `src/MLF/Elab/Elaborate.hs:869`, `src/MLF/Elab/Elaborate.hs:883`, `src/MLF/Elab/Elaborate.hs:924`, `src/MLF/Elab/Elaborate.hs:1011`
- Bugs/history show some of this fallback behavior was intentionally retained in narrow producer-side cases.
  - Evidence: `Bugs.md:677`, `Bugs.md:678`

Assessment:
- This is a real thesis-exactness candidate, but lower-confidence than the generalization ladders because some of these fallbacks may be compensating for upstream witness/source-shape gaps rather than being pure convenience logic.

## Medium-confidence / situational candidates

### 3. Planner scheme-owner synthesized-wrapper fallback
- `planEdge` / related planner logic still uses body-root first with wrapper-root fallback for synthesized `ExpVarId`s only.
  - Evidence: `CHANGELOG.md:432`, `Bugs.md:817`, `Bugs.md:834`

Assessment:
- This is probably removable only if synthesized-wrapper ownership is normalized earlier so the fallback case becomes unrepresentable.
- Not the first thing to attack.

### 4. Instantiation inference fallback
- `MLF.Elab.Run.Instantiation.inferInstAppArgsFromScheme` defines a real `fallback` branch that is used when bound-driven matching fails.
  - Evidence: `src/MLF/Elab/Run/Instantiation.hs:20`, `src/MLF/Elab/Run/Instantiation.hs:33`, `src/MLF/Elab/Run/Instantiation.hs:76`, `src/MLF/Elab/Run/Instantiation.hs:89`

Assessment:
- This looks heuristic and may be a candidate, but without a direct thesis cross-reference it is not as clear as the generalization ladders.
- Needs a separate paper/code audit before removal.

## Recommendation ranking

1. **Audit/remove the GA -> no-GA -> reify fallback ladders** in:
   - `src/MLF/Elab/Elaborate.hs`
   - `src/MLF/Elab/Run/Pipeline.hs`
   - `src/MLF/Elab/Run/ResultType/Util.hs`
   - `src/MLF/Elab/Generalize.hs`
2. **Then audit/remove `reifyInst` fallback paths** in `src/MLF/Elab/Elaborate.hs`.
3. **Then inspect synthesized-wrapper planner fallback** and `inferInstAppArgsFromScheme` fallback if you still want stricter cleanup.

## Bottom line

- **Yes, there are still fallbacks that can plausibly be removed for greater thesis-exactness.**
- The most important remaining ones are the **generalization fallback ladders**.
- The strongest non-candidate is `checkNoGenFallback`, which is already a thesis-faithful guard rather than a compatibility fallback.

## 2026-03-08 tracker cleanup
- Completed audit; its recommendations were consumed by the later fallback-removal implementation work.
