# Findings

- Mechanism queue seeded from `tasks/archive/2026-03-10-dead-export-sweep/findings.md`.
- Rows 1-2 are the highest-confidence stale exports; rows 3-5 need fresh liveness revalidation immediately before any code changes.
- Row 1 research baseline: `src/MLF/Elab/Run/ChiQuery.hs` still re-exports `chiLookupBindParent` and `chiBindParents`, both thin aliases over `PresolutionView` bind-parent accessors.
- Current docs already frame Row 1 as chi-query cleanup: the active task plan names `Remove chiLookupBindParent / chiBindParents if still dead`, and archived simplification notes say `ChiQuery` should remain the chi-first façade while dropping dead derived helpers rather than retiring the whole module.
- Doc/history alignment for Row 1 is strong: `CHANGELOG.md`, `implementation_notes.md`, and archived simplification notes all record the 2026-03-09 `chiCanonicalBindParents` retirement as a deliberate narrowing of `ChiQuery` while preserving the chi-first façade boundary. That makes removing the remaining dead bind-parent aliases directionally consistent with the latest cleanup policy.
- Guard-home recommendation: `test/PipelineSpec.hs` is the best source-guard home because the existing chi-first regression block already owns `ChiQuery` boundary assertions (`internals use shared ChiQuery facade`, `ChiQuery no longer defines chiCanonicalBindParents`, `ResultType|Phase 6 — Elaborate|chi-first gate stays green`).
- Working-tree note: the current uncommitted diff already removes `chiLookupBindParent` / `chiBindParents` from `src/MLF/Elab/Run/ChiQuery.hs` and adds a sibling `PipelineSpec` source guard proving those names stay absent.

- Row 1 verifier check confirmed `chiLookupBindParent` and `chiBindParents` had zero live call sites outside `src/MLF/Elab/Run/ChiQuery.hs`.
- Row 1 landed cleanly by removing only the two dead ChiQuery exports/definitions and adding a focused `PipelineSpec` guard.
- Row 2 verifier check confirmed `validateSingleGenRoot` had no live uses outside `MLF.Binding.Validation`; only internal self-calls remained.
- Row 2 landed by removing only the stale `validateSingleGenRoot` export while keeping the helper local to `Validation`.
- Row 3 verifier recheck confirmed `canonicalizeRef` still had zero live uses outside `MLF.Constraint.Canonicalizer`.
- Row 3 landed by removing the dead `canonicalizeRef` export/definition and adding a focused `CanonicalizerSpec` guard.
- Row 4 verifier recheck confirmed `edgeOrigins` still had zero live uses outside `MLF.Elab.Run.Debug`.
- Row 4 landed by removing the dead debug export/definition and adding a focused ga-scope guard in `PipelineSpec`.
- Row 5 verifier recheck confirmed `closeTermWithSchemeSubst` still had zero live uses outside `MLF.Elab.TermClosure`.
- Row 5 landed by removing the dead export/definition and adding a focused Phase 6 guard in `PipelineSpec`.
