# Duplicate Logic Candidates

This list captures duplicated logic that could be centralized for clarity and maintenance. Line numbers are approximate and should be re-checked when working on the item.

## 1) Base-bound resolution logic
- `src/MLF/Elab/Elaborate.hs:89` — local `resolveBaseBound` in `expansionToInst`.
- `src/MLF/Reify/TypeOps.hs:301` — `resolveBaseBoundForInstConstraint` (canonical helper).
- `src/MLF/Elab/Run/Pipeline.hs:456` — `resolveBoundBodyBase` in annotation handling.

Suggestion: replace local helpers with `resolveBaseBoundForInstConstraint` (or extract a shared helper that handles “stop on TyBase/TyBottom or follow bound” in one place).

## 2) Scope-aware bound/alias inlining logic
- `src/MLF/Elab/Run/TypeOps.hs:47` — `inlineBoundVarsTypeWith` (custom recursion on `TVar`, `TArrow`, `TForall` with `parseNameId` and `boundNames`).
- `src/MLF/Elab/Run/Pipeline.hs:368` — `inlineAllBoundsType` (same pattern with base-constraint lookup and `toBase`).
- `src/MLF/Reify/TypeOps.hs:350` — `inlineAliasBoundsWithBy` (shared implementation used elsewhere).

Suggestion: add a small wrapper around `inlineAliasBoundsWithBy` (or extend it) to cover both remaining call sites, so the traversal is defined once.

## 3) “Resolve bound body before reify”
- `src/MLF/Elab/Run/TypeOps.hs:53` — `resolveBoundBody`.
- `src/MLF/Elab/Run/Pipeline.hs:456` — `resolveBoundBodyBase`.

Suggestion: extract a shared helper that resolves a bound chain with cycle protection and reuse it in both places (and potentially in base-bound resolution helpers).
