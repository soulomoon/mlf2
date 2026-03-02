# Findings

## 2026-03-03 Wave 0 Initialization
- Initiative goal is to reduce internal compatibility-oriented `Solved` reads in result-type/generalize internals while preserving behavior.
- Planned approach is multi-PR phased execution with a high-risk final wave replacing fallback local rebuild (`rebuildWithNodes`) with overlay/view adjustment.
- No public API changes are targeted (`src-public/*` out of scope).
- New internal module target identified: `src/MLF/Elab/Run/ResultType/View.hs`.
- Critical parity locks are explicitly required in both generalize fallback ladder and result-type fallback mapping branches (`mapped`, `same-domain`, `missing`).

## Baseline Capture Checklist (Completed)
- Hotspot inventory for direct `Solved.*` reads in:
  - `src/MLF/Elab/Run/ResultType/*`
  - `src/MLF/Elab/Generalize.hs`
  - `src/MLF/Constraint/Presolution/Plan/Context.hs`
  - `src/MLF/Elab/Run/Generalize.hs`
- Existing focused test matrix execution evidence.
- Full gate evidence: `cabal build all && cabal test`.

## Risk Register (Initial)
- Highest risk: Wave 4 fallback replacement can change target selection or bind-parent mutation semantics unless parity comparator is enforced before legacy deletion.
- Medium risk: Wave 2 mechanical refactor can accidentally alter query ordering or canonicalization assumptions if view accessors are incomplete.
- Low/medium risk: Wave 1 cleanup may expose latent coupling in generalize fallback/reify flows.

## 2026-03-03 Wave 0 Baseline Findings
- Baseline canonical gate passed before edits: `cabal build all && cabal test` -> `913 examples, 0 failures`.
- Hotspots confirmed:
  - Result-type internals had scattered direct `Solved.*` reads across `ResultType.hs`, `Ann.hs`, `Fallback.hs`.
  - `Fallback.hs` still used local `Solved.rebuildWithNodes` patching path.
  - `Context.hs` carried compatibility-only `gcConstraintForReify` / `rbConstraintForReify` fields.

## 2026-03-03 Wave 1 Findings (Generalize + Context)
- Removed compatibility-only context fields and associated wiring:
  - `gcConstraintForReify`
  - `rbConstraintForReify`
- Added lightweight invariant tracing in context resolution when `SolvedToBaseMissing` occurs on a node that exists in the base-domain constraint.
- In `MLF.Elab.Generalize`, alias solved rebuild now runs only when not using OnConstraint reification.
- Explicit-bound helper reads now route through OnConstraint bound reification when structural-scheme path is authoritative.
- Added fallback-ladder tests for:
  - GA -> no-GA fallback on `SchemeFreeVars`.
  - reify-type fallback after double `SchemeFreeVars`.

## 2026-03-03 Wave 2 Findings (Result-Type View)
- Added `src/MLF/Elab/Run/ResultType/View.hs` as the read boundary for result-type internals.
- `rtcSolveLike` usage is now confined to `buildResultTypeView`.
- `ResultType.hs`, `ResultType/Ann.hs`, and `ResultType/Fallback.hs` now consume view accessors instead of scattered direct solved reads.
- Exposed `generalizeWithPlan` via `MLF.Elab.Run.ResultType` for testability without hidden-module imports.

## 2026-03-03 Wave 3 Findings (Mapping Branches)
- Added integrated fallback-core coverage for:
  - `gaSolvedToBase` same-domain roots
  - `gaSolvedToBase` missing roots
- Both branches preserve parity against baseline fallback result types.

## 2026-03-03 Wave 4 Findings (Fallback Overlay)
- Replaced fallback-core local `rebuildWithNodes` patching with a bound-overlay path at the result-type view boundary.
- Overlay materialization uses view-based solved reconstruction (`rebuildWithConstraint`) only when overlay exists.
- `Fallback.hs` no longer calls `Solved.rebuildWithNodes`.
- Existing pipeline parity guard (`pipeline output unchanged after patchNode elimination for:`) stays green.

## Verification Summary
- Focused regression matrix (existing + new checks): PASS.
- Canonical full gate after implementation:
  - `cabal build all && cabal test` -> `917 examples, 0 failures`.
