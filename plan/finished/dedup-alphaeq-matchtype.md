# Plan: dedup alphaEqType + matchType

## Scope
Unify alpha‑equivalence and type‑matching helpers duplicated across:
- `src/MLF/Elab/Run.hs:1583` and `src/MLF/Elab/Run.hs:1618`
- `src/MLF/Elab/Elaborate.hs:461` and `src/MLF/Elab/Elaborate.hs:663`
- `src/MLF/Elab/Phi.hs:668` and `src/MLF/Elab/Phi.hs:723`
- `src/MLF/Elab/TypeCheck.hs:145` (alphaEqType only)

## Target location
`MLF.Elab.TypeOps` (new module), or `MLF.Elab.Types` if you prefer to keep core type ops together.

## Canonical behavior choice
- Use the `alphaEqType` semantics from `MLF.Elab.TypeCheck` (compares bounds via `boundType`).
- Use the most general `matchType` signature:
  `Set.Set String -> ElabType -> ElabType -> Either ElabError (Map.Map String ElabType)`
  (current `Run` version), then adapt callers that currently pass `[String]`.

## Steps
1. Add `alphaEqType` and `matchType` to `MLF.Elab.TypeOps`.
2. Replace local definitions in `Run`, `Elaborate`, `Phi`, `TypeCheck` with imports.
3. Adjust call sites:
   - `Elaborate` and `Phi` currently pass `[String]`; convert to `Set.fromList` at call sites.
4. Remove now‑dead local helper definitions.
5. Build and run tests.

## Risks
- Error message strings differ between implementations; verify no tests assert exact message text.
- Bounds comparison semantics: `Run`/`Phi` versions differ subtly; confirm the chosen semantics match all usages.

