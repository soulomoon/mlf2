# Refactor plan: consolidate type substitution/renaming helpers

## Goal
Reduce duplication between type substitution/renaming utilities in `Elab.TypeOps`, `Elab.Types`, and `Elab.Run.TypeOps`.

## Scope
- `src/MLF/Elab/TypeOps.hs` (`substTypeSimple`, `substTypeCapture`)
- `src/MLF/Elab/Types.hs` (`substType` + `renameVar` local)
- `src/MLF/Elab/Run/TypeOps.hs` (`substVar` local)

## Proposed abstraction
Create a dedicated helper module or extend `MLF.Elab.TypeOps` with:
- `substTypeSimple` (already)
- `substTypeCapture` (already)
- `renameTypeVar` (new) for capture-aware rename

Then:
- Replace `substVar` in `Run.TypeOps` with `substTypeSimple` or `renameTypeVar`.
- Replace the local `substType`/`renameVar` in `Elab.Types` with shared helpers, keeping any display-only behavior intact.

## Steps
1. Add `renameTypeVar` to `TypeOps` (likely capture-avoiding by parameterizing bound set).
2. Update `Run.TypeOps.substVar` to use new helper.
3. Update `Elab.Types.substType` and `renameVar` to use shared helpers.
4. Remove now-duplicate local definitions.

## Risks
- Capture-avoidance rules must remain identical, especially in `Elab.Types` display inliner.

## Verification
- `cabal test`.
