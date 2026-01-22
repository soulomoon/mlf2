# Refactor plan: unify ElabType free-variable computation

## Goal
Remove duplicated free-variable folds over `ElabType` by centralizing a single implementation and wrapping it for `Set` vs list output.

## Scope
- `src/MLF/Elab/TypeOps.hs` (freeTypeVarsType)
- `src/MLF/Elab/FreeNames.hs` (freeNamesFrom/freeNamesOf)
- `src/MLF/Elab/Phi.hs` (freeTypeVars local)
- `src/MLF/Elab/Run/TypeOps.hs` (freeVarsType)
- `src/MLF/Elab/Types.hs` (freeVarsType in occInfo)

## Proposed abstraction
Add a single helper in `MLF.Elab.TypeOps` or `MLF.Elab.FreeNames`:

```
freeVarsFrom :: Set.Set String -> ElabType -> Set.Set String
```

Then expose small adapters:
- `freeTypeVarsType :: ElabType -> Set.Set String` (calls `freeVarsFrom Set.empty`)
- `freeTypeVarsList :: ElabType -> [String]` (nub . Set.toList)

Update call sites to use the shared helper and remove local duplicates.

## Steps
1. Choose canonical implementation location (prefer `MLF.Elab.TypeOps` since it already exports `freeTypeVarsType`).
2. Add `freeVarsFrom` (with bound-set parameter) and `freeTypeVarsList` wrapper.
3. Replace:
   - `MLF.Elab.FreeNames.freeNamesFrom` with re-export or wrapper to the canonical helper.
   - `MLF.Elab.Phi.freeTypeVars` with `freeTypeVarsList`.
   - `MLF.Elab.Run.TypeOps.freeVarsType` with `freeVarsFrom` from the shared module.
4. Remove local helper definitions and unused imports.

## Risks
- Be careful that list ordering changes only in non-semantic contexts (logs/debug). If order matters, preserve existing behavior or keep a local wrapper.

## Verification
- `cabal test`.
