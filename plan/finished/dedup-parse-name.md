# Plan: dedup parseName (t<id> convention)

## Scope
Replace repeated `parseName` helpers:
- `src/MLF/Elab/Run.hs:1441`
- `src/MLF/Elab/Elaborate.hs:580` and `src/MLF/Elab/Elaborate.hs:615`
- `src/MLF/Elab/Phi.hs:653`

## Existing canonical helper
`MLF.Elab.Generalize.Names.parseNameId` (`src/MLF/Elab/Generalize/Names.hs:12`).

## Steps
1. Reuse `parseNameId` directly where possible (import it in the above modules).
2. If you want to avoid cross‑package imports, re‑export `parseNameId` from `MLF.Elab.TypeOps` and update call sites to use `TypeOps.parseNameId`.
3. Delete local `parseName` definitions.
4. Build/test.

## Risks
- None, as all variants parse the same `t<id>` format with `reads`.

