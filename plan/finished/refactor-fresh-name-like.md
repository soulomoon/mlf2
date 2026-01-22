# Refactor plan: centralize freshNameLike

## Goal
Remove duplicate `freshNameLike` implementations and reuse a single helper.

## Scope
- `src/MLF/Elab/TypeOps.hs`
- `src/MLF/Elab/Types.hs`

## Proposed abstraction
Expose `freshNameLike` from `MLF.Elab.TypeOps` and import it in `MLF.Elab.Types`, removing the local definition.

## Steps
1. Confirm both implementations are identical (string/numbering policy).
2. Delete the local definition in `Elab.Types`.
3. Add an import of `freshNameLike` in `Elab.Types` and update call sites.
4. Remove any now-unused imports.

## Risks
- Minimal; just ensure no name clash with other helpers.

## Verification
- Run `cabal test` or at least any tests that exercise substitution/capture avoidance (`Elab` specs).
