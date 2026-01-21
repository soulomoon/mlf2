# Plan: consolidate duplication into `MLF.Elab.TypeOps`

Goal: centralize duplicated `ElabType` helpers (alpha‑equivalence, matching, free vars, substitution, forall splitting/stripping, name parsing, base‑bound resolution) into a single module without changing external signatures or behavior.

## 1) New module + exports
Create `src/MLF/Elab/TypeOps.hs` exporting:

- `alphaEqType :: ElabType -> ElabType -> Bool`
- `matchType :: Set.Set String -> ElabType -> ElabType -> Either ElabError (Map.Map String ElabType)`
- `freeTypeVarsType :: ElabType -> Set.Set String`
- `freshNameLike :: String -> Set.Set String -> String` (or reuse existing in `MLF.Elab.Types`)
- `substTypeCapture :: String -> ElabType -> ElabType -> ElabType` (capture‑avoiding)
- `substTypeSimple :: String -> ElabType -> ElabType -> ElabType` (non‑capture‑avoiding)
- `splitForalls :: ElabType -> ([(String, Maybe ElabType)], ElabType)` (shared version)
- `stripForallsType :: ElabType -> ElabType`
- `parseNameId :: String -> Maybe Int` (re‑export from `MLF.Elab.Generalize.Names`)

Optional (if you want to fold base‑bound logic here as well):
- `resolveBaseBoundForInstConstraint :: Constraint -> (NodeId -> NodeId) -> NodeId -> Maybe NodeId`
- `resolveBaseBoundForInstSolved :: SolveResult -> NodeId -> Maybe NodeId`
- `inlineBaseBoundsType :: Constraint -> (NodeId -> NodeId) -> ElabType -> ElabType`

## 2) Decide canonical semantics
Pick canonical implementations based on existing behavior:

- `alphaEqType`: prefer the version from `MLF.Elab.TypeCheck` (includes bound type comparison via `boundType`).
- `matchType`: prefer `MLF.Elab.Run`’s binder‑set‑driven version (more general), but align error strings with current callers where needed.
- `freeTypeVarsType`: use the `Set`‑based version (currently in `TypeCheck`/`Reduce`/`ReifyPlan`).
- `substTypeCapture`: use `MLF.Elab.Types` or `TypeCheck` capture‑avoiding version (with renaming).
- `substTypeSimple`: use `MLF.Elab.Generalize.Normalize` simple substitution (no capture handling).
- `splitForalls`: use `MLF.Elab.Inst.splitForalls` logic (para; stable order).
- `stripForallsType`: implement via `splitForalls` or keep existing logic.
- `parseNameId`: use `MLF.Elab.Generalize.Names.parseNameId` as single source.

## 3) Replace call sites (no behavior change)

### Core duplicates to replace
- `alphaEqType` in:
  - `src/MLF/Elab/Run.hs:1618`
  - `src/MLF/Elab/Elaborate.hs:663`
  - `src/MLF/Elab/Phi.hs:723`
  - `src/MLF/Elab/TypeCheck.hs:145`
- `matchType` in:
  - `src/MLF/Elab/Run.hs:1583`
  - `src/MLF/Elab/Elaborate.hs:461`
  - `src/MLF/Elab/Phi.hs:668`
- `freeTypeVarsType` in:
  - `src/MLF/Elab/TypeCheck.hs:132`
  - `src/MLF/Elab/Reduce.hs:99`
  - `src/MLF/Elab/Generalize/ReifyPlan.hs:467`
- `substType` (capture‑avoiding) in:
  - `src/MLF/Elab/Inst.hs:119`
  - `src/MLF/Elab/TypeCheck.hs:174`
  - `src/MLF/Elab/Reduce.hs:237`
  - `src/MLF/Elab/Types.hs:385` (if moving to TypeOps, keep Types wrapper)
- `splitForalls`/`stripForallsType` in:
  - `src/MLF/Elab/Inst.hs:28`
  - `src/MLF/Elab/Types.hs:470`
  - `src/MLF/Elab/Run.hs:1573`
  - `src/MLF/Elab/Generalize.hs:1138`
- `parseName` helpers in:
  - `src/MLF/Elab/Run.hs:1441`
  - `src/MLF/Elab/Elaborate.hs:580`
  - `src/MLF/Elab/Elaborate.hs:615`
  - `src/MLF/Elab/Phi.hs:653`

### Base‑bound helpers (optional but high‑value)
- `resolveBaseBoundForInst` / inline base bounds in:
  - `src/MLF/Elab/Run.hs:1204` + `src/MLF/Elab/Run.hs:1223`
  - `src/MLF/Elab/Elaborate.hs:612` + `src/MLF/Elab/Elaborate.hs:634`
  - `src/MLF/Elab/Phi.hs:640`

## 4) Cabal updates
Add `MLF.Elab.TypeOps` to `mlf2.cabal` `other-modules` list under `mlf2-internal`.

## 5) Migration steps
1. Create `MLF.Elab.TypeOps` with chosen canonical implementations.
2. Update imports in each caller module and delete local duplicates.
3. If keeping wrappers (e.g., `MLF.Elab.Types`), re‑export or delegate to `TypeOps` to minimize churn.
4. Build: `cabal build`.
5. Run tests: `cabal test`.

## 6) Risk notes
- Ensure error strings from `matchType` remain consistent for callers that pattern‑match on messages (if any).
- If `alphaEqType` behavior differs (bound types vs ignoring bounds), add an explicit unit test before swapping.
- When unifying `substType`, ensure capture‑avoidance semantics stay consistent in `TypeCheck`/`Inst`/`Reduce`.

