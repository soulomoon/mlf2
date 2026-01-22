# Refactor plan: centralize instantiation binder renaming

## Goal
Remove duplicate `renameInstBound` implementations and keep a single authoritative definition.

## Scope
- `src/MLF/Elab/Inst.hs`
- `src/MLF/Elab/TypeCheck.hs`
- `src/MLF/Elab/Reduce.hs`

## Proposed abstraction
Move `renameInstBound :: String -> String -> Instantiation -> Instantiation` to `MLF.Elab.Inst` (or `MLF.Elab.Inst.Util`) and re-export. All other modules import it.

## Steps
1. Keep the canonical implementation in `MLF.Elab.Inst`.
2. Replace local definitions in `TypeCheck` and `Reduce` with imports.
3. Update call sites and remove now-unused `para` imports if applicable.
4. Ensure behavior for shadowing (`InstUnder` with same binder) remains identical.

## Risks
- Slight difference if a local version diverged; check they are syntactically identical before removal.

## Verification
- Run `cabal test` (or targeted tests covering instantiation semantics, e.g., `ElaborationSpec`, `TypeCheckSpec`).
