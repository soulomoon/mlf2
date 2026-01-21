# Plan: dedup split/strip foralls

## Scope
Unify helpers that split or strip outer `forall` binders:
- `splitForalls` in `MLF.Elab.Inst` (`src/MLF/Elab/Inst.hs:28`)
- `splitForallsLocal` in `MLF.Elab.Types` (`src/MLF/Elab/Types.hs:470`)
- `stripForallsType` in `MLF.Elab.Run` (`src/MLF/Elab/Run.hs:1573`)
- `stripForalls` in `MLF.Elab.Generalize` (`src/MLF/Elab/Generalize.hs:1138`)

## Target location
`MLF.Elab.TypeOps` with:
- `splitForalls :: ElabType -> ([(String, Maybe ElabType)], ElabType)`
- `stripForallsType :: ElabType -> ElabType` (can use `snd . splitForalls`)

## Steps
1. Implement canonical `splitForalls` in `TypeOps` (use existing `Inst.splitForalls` logic).
2. Replace `splitForallsLocal` usage:
   - If `Types` requires `apo` semantics for pretty display, keep `schemeToTypeLocal`, but call `TypeOps.splitForalls` where semantics match.
3. Replace `stripForallsType` in `Run` and `stripForalls` in `Generalize` with `TypeOps.stripForallsType`.
4. Delete local duplicate definitions.
5. Build/test.

## Risks
- `splitForallsLocal` uses `embed` for non‑forall; ensure the canonical version preserves behavior in all call sites.
- Generalization logic assumes specific stripping semantics; verify no binder order changes.

