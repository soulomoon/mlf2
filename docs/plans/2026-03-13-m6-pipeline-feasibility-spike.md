# M6 Pipeline Feasibility Spike (Docs-Only Retry)

Date: 2026-03-13  
Milestone: M6 — Pipeline acceptance for explicit annotations only  
Decision: keep recursive types explicit-layer-only in Phase 1; do not add `TyMu` pipeline/graph support in this slice.

## Scope

- This retry is docs-only and limited to this file.
- Phase 1 boundary remains unchanged: recursive surface annotations are still rejected as `RecursiveAnnotationNotSupported`.

## Blocker Class 3 (Reconstruction evidence on current master)

The prior blocker-class-3 rationale was too weak because it cited module presence instead of concrete reconstruction code paths. On current master, the relevant annotation result-type flow is explicit inside `computeResultTypeFromAnnWithView`:

- `generalizeWithPlan` is used to rebuild a scheme from the annotation target node: `src/MLF/Elab/Run/ResultType/Ann.hs:118` and again on the explicit-annotation branch at `src/MLF/Elab/Run/ResultType/Ann.hs:298`.
- That scheme is converted back into an elaborated type via `schemeToType`: `src/MLF/Elab/Run/ResultType/Ann.hs:121` and `src/MLF/Elab/Run/ResultType/Ann.hs:299`.
- A direct no-fallback reification probe is used with `reifyTypeWithNamedSetNoFallback`: `src/MLF/Elab/Run/ResultType/Ann.hs:131`.

So annotation result types are reconstructed through existing generalize/reify/scheme translation paths; there is no dedicated recursive-node reconstruction flow added in this slice.

The reifier evidence is also concrete in `MLF.Reify.Type` `goFull`:

- `goFull` entry: `src/MLF/Reify/Type.hs:217`.
- Reconstruction cases in the main node match:
  - `TyBase` -> `TBase`: `src/MLF/Reify/Type.hs:359`
  - `TyBottom` -> `TBottom`: `src/MLF/Reify/Type.hs:360`
  - `TyArrow` -> `TArrow`: `src/MLF/Reify/Type.hs:361`
  - `TyCon` -> `TCon`: `src/MLF/Reify/Type.hs:365`
  - `TyForall` -> recurse into body: `src/MLF/Reify/Type.hs:373`
  - `TyExp` -> recurse via `goFull`: `src/MLF/Reify/Type.hs:376`

Current-master evidence therefore supports the narrower statement: this reconstruction path handles `TyBase`, `TyBottom`, `TyArrow`, `TyCon`, `TyForall`, and `TyExp`; no `TyMu` branch is present in `MLF.Reify.Type`.

## Outcome for M6

- Keep M6 as a docs-only acceptance decision for explicit annotations only.
- Keep the explicit-layer-only stance unchanged.
- Keep `RecursiveAnnotationNotSupported` as the Phase 1 boundary.
