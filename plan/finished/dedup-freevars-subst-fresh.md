# Plan: dedup free vars + substitution + fresh naming

## Scope
Unify `freeTypeVarsType`, `substType` (capture‑avoiding), and fresh name helpers:
- `freeTypeVarsType`: `TypeCheck`, `Reduce`, `Generalize/ReifyPlan`.
- `substType` capture‑avoiding: `Inst`, `TypeCheck`, `Reduce`, `Types`.
- fresh naming: `Inst.freshName`, `TypeCheck.freshName`, `Reduce.freshTypeName`, `Types.freshNameLike`.

## Target location
`MLF.Elab.TypeOps` (or `MLF.Elab.Types` if you prefer core type ops to live there).

## Canonical behavior choice
- `freeTypeVarsType`: Set‑based version (used in `TypeCheck` / `Reduce` / `ReifyPlan`).
- `substTypeCapture`: use `MLF.Elab.Types` or `TypeCheck` capture‑avoiding logic with renaming.
- `freshNameLike`: keep `Types.freshNameLike` as canonical; provide helpers that adapt to `uN` or base‑name semantics.

## Steps
1. Implement in `TypeOps`:
   - `freeTypeVarsType :: ElabType -> Set.Set String`
   - `substTypeCapture :: String -> ElabType -> ElabType -> ElabType`
   - `freshNameLike :: String -> Set.Set String -> String` (re‑export or delegate to `Types`)
   - `freshTypeName :: Set.Set String -> String` (thin wrapper around `freshNameLike "u"` semantics)
2. Replace local definitions in `TypeCheck`, `Reduce`, `Inst`, `ReifyPlan` with imports.
3. Update dependent helpers (`substTypeVarTerm`, `substTypeVarInst`, etc.) to call the shared `substTypeCapture`.
4. Delete local definitions.
5. Build/test.

## Risks
- Capture‑avoidance semantics must remain identical; verify substitution with `forall` renaming paths.
- `freshName` in `Inst` uses list‑based uniqueness; confirm Set‑based version preserves behavior.

