# MLF.Elab.Run split plan (keep exports/signatures stable)

Goal: split `MLF.Elab.Run` into focused submodules without changing any public signatures or exported names. `MLF.Elab.Run` remains a thin facade that re-exports the same four symbols.

## Target module layout

- `src/MLF/Elab/Run/Pipeline.hs`
  - Move: `runPipelineElab`, `runPipelineElabChecked`, `runPipelineElabWith`, `firstShow`
  - From: `src/MLF/Elab/Run.hs:43-1130` and `src/MLF/Elab/Run.hs:1652`

- `src/MLF/Elab/Run/Annotation.hs`
  - Move: `applyRedirectsToAnn`, `canonicalizeAnn`, `annNode`, `adjustAnnotationInst`
  - From: `src/MLF/Elab/Run.hs:1144`, `src/MLF/Elab/Run.hs:1159`, `src/MLF/Elab/Run.hs:1173`, `src/MLF/Elab/Run.hs:1658`

- `src/MLF/Elab/Run/TypeOps.hs`
  - Move: `resolveBaseBoundForInst`, `inlineBaseBoundsType`, `inlineBoundVarsType`, `simplifyAnnotationType`, `parseName`
  - From: `src/MLF/Elab/Run.hs:1204`, `src/MLF/Elab/Run.hs:1223`, `src/MLF/Elab/Run.hs:1243`, `src/MLF/Elab/Run.hs:1279`, `src/MLF/Elab/Run.hs:1441`

- `src/MLF/Elab/Run/Instantiation.hs`
  - Move: `inferInstAppArgsFromScheme`, `varsInType`, `substTypeSelective`, `instInsideFromArgsWithBounds`, `containsForallType`, `stripForallsType`, `matchType`, `alphaEqType`
  - From: `src/MLF/Elab/Run.hs:1450`, `src/MLF/Elab/Run.hs:1518`, `src/MLF/Elab/Run.hs:1530`, `src/MLF/Elab/Run.hs:1551`, `src/MLF/Elab/Run.hs:1565`, `src/MLF/Elab/Run.hs:1573`, `src/MLF/Elab/Run.hs:1583`, `src/MLF/Elab/Run.hs:1618`

- `src/MLF/Elab/Run/Scope.hs`
  - Move: `bindingScopeRef`, `preferGenScope`, `schemeBodyTarget`, `canonicalizeScopeRef`, `letScopeOverrides`
  - From: `src/MLF/Elab/Run.hs:1673`, `src/MLF/Elab/Run.hs:1680`, `src/MLF/Elab/Run.hs:1691`, `src/MLF/Elab/Run.hs:1725`, `src/MLF/Elab/Run.hs:3070`

- `src/MLF/Elab/Run/Generalize.hs`
  - Move: `pruneBindParentsConstraint`, `instantiationCopyNodes`, `constraintForGeneralization`
  - From: `src/MLF/Elab/Run.hs:1184`, `src/MLF/Elab/Run.hs:1733`, `src/MLF/Elab/Run.hs:1759`

- `src/MLF/Elab/Run/Debug.hs`
  - Move: `debugGaScope`, `debugGaScopeEnabled`, `edgeOrigins`
  - From: `src/MLF/Elab/Run.hs:3097`, `src/MLF/Elab/Run.hs:3103`, `src/MLF/Elab/Run.hs:3110`

- `src/MLF/Elab/Run/Util.hs`
  - Move: `chaseRedirects` (and optionally `firstShow` if you want it here instead of `Pipeline`)
  - From: `src/MLF/Elab/Run.hs:1642`

## Facade (`MLF.Elab.Run`) after split

Keep `src/MLF/Elab/Run.hs` as a thin wrapper exporting the same symbols:

- `runPipelineElab`
- `runPipelineElabChecked`
- `applyRedirectsToAnn`
- `chaseRedirects`

It should import these from the new submodules and re-export them unchanged.

## Cabal updates

Add the new modules to `mlf2.cabal` under `mlf2-internal` `other-modules`:

- `MLF.Elab.Run.Pipeline`
- `MLF.Elab.Run.Annotation`
- `MLF.Elab.Run.TypeOps`
- `MLF.Elab.Run.Instantiation`
- `MLF.Elab.Run.Scope`
- `MLF.Elab.Run.Generalize`
- `MLF.Elab.Run.Debug`
- `MLF.Elab.Run.Util`

## Execution steps

1. Create the new modules and move the corresponding functions wholesale (no signature changes).
2. Update internal imports (most of them will move from `MLF.Elab.Run` to `MLF.Elab.Run.*`).
3. Replace `src/MLF/Elab/Run.hs` with a minimal re-export facade.
4. Update `mlf2.cabal` `other-modules` to include the new submodules.
5. Run `cabal build` to verify no cycles / missing imports.

## Invariants

- No exported names or signatures change.
- `MLF.Elab.Pipeline` continues to import from `MLF.Elab.Run` unchanged.
- Debug behavior (`MLF_DEBUG_BINDING`) remains intact, only relocated.
