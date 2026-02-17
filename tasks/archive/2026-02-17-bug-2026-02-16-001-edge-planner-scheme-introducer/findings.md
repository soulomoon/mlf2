# Findings: BUG-2026-02-16-001 Planner Scheme Introducer Crash

## 2026-02-17 Phase 1
- Failure source is deterministic and localized:
  - `planEdge` in `src/MLF/Constraint/Presolution/EdgeProcessing/Planner.hs` resolves `schemeGen` via `findSchemeIntroducerM canonical constraint0 (rteBodyId leftTyExp)`.
  - `findSchemeIntroducerM` throws `InternalError "scheme introducer not found for ..."` when no `GenRef` exists on binding path.
- For the failing planner-classification fixtures, root cause is missing `GenRef` on TyExp body path, not missing TyExp itself.
- Reproducer data-flow evidence (constructed from the failing fixture shape) shows:
  - binding path for TyExp body (`NodeId 0`) is only `[TypeRef 0]` (no gen ancestor),
  - binding path for wrapper (`NodeId 2`) includes root gen,
  - therefore body-based scheme-owner lookup fails even though the wrapper sits under a gen root.
- Recent-change check (`git blame`) points to regression introduction:
  - commit `a8e5781` added `eprSchemeOwnerGen` to `EdgePlan` and moved scheme-introducer lookup into planner.

## 2026-02-17 Phase 2/3 Outcome
- Working vs failing pattern comparison:
  - Working planner case (`returns a resolved TyExp payload ...`) provides explicit body→forall→TyExp bind-parent chain, so body-root lookup finds a gen ancestor.
  - Failing let/ann flag cases use synthesized wrappers and sparse bind parents: wrapper has a gen ancestor, body does not.
- Confirmed root-cause hypothesis:
  - planner-level unconditional body-root lookup is too strict for synthesized-wrapper fixture topology.
  - This is a regression from `a8e5781` planner coupling.

## 2026-02-17 Fix Validation
- Implemented synthesized-wrapper fallback in planner:
  - body-root first; wrapper-root fallback only for synthesized exp vars.
  - frontend TyExp path remains strict body-root lookup.
- Deterministic repros now pass:
  - let-edge `allowTrivial` case: PASS
  - ann-edge `suppressWeaken` case: PASS
- Added stronger regression assertions:
  - both tests now also check resolved `eprSchemeOwnerGen` (`GenNodeId 0`).
