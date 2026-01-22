# Refactor plan: unify forall flatten/rebuild helpers

## Goal
Consolidate duplicated forall flattening/reconstruction logic across Elab modules.

## Scope
- `src/MLF/Elab/TypeOps.hs` (`splitForalls`, `stripForallsType`)
- `src/MLF/Elab/Types.hs` (`schemeToTypeLocal`, `splitForallsLocal`)
- `src/MLF/Elab/Run/TypeOps.hs` (`stripForalls` local)

## Proposed abstraction
Add shared helpers in `MLF.Elab.TypeOps`:
- `splitForalls :: ElabType -> ([(String, Maybe ElabType)], ElabType)` (already exists)
- `buildForalls :: [(String, Maybe ElabType)] -> ElabType -> ElabType` (new)

Then:
- Replace `schemeToTypeLocal` with `buildForalls` or reuse `MLF.Elab.Inst.schemeToType` directly.
- Replace `splitForallsLocal` in `Types` and `stripForalls` in `Run.TypeOps` with `splitForalls`.

## Steps
1. Add `buildForalls` to `MLF.Elab.TypeOps`.
2. Update `schemeToTypeLocal` to call `buildForalls`.
3. Replace `splitForallsLocal` and `stripForalls` local in `Run.TypeOps` with `splitForalls` (and adjust pretty-display paths).
4. Remove duplicated local helpers and unused imports.

## Risks
- Ensure display-only pretty printing uses the same semantics; avoid breaking any display-specific inline rules.

## Verification
- `cabal test`.
