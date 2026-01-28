# Design Document

## Overview

This change removes the last cross-boundary dependencies by moving core xMLF types and reify helpers into neutral namespaces and by lifting the last scheme-type fallback decision into presolution planning output. Elab remains apply-only and uses plan outputs.

## Architecture

### Neutral type module

- New: `src/MLF/Types/Elab.hs`
  - Holds `ElabType`, `ElabTypeF`, `ElabScheme`, `SchemeInfo`, `ElabTerm`, `ElabTermF`, `Instantiation`, `InstantiationF` and their Recursive/Corecursive instances.
  - Imports only neutral dependencies (`MLF.Constraint.Types`, `MLF.Frontend.Syntax`, `MLF.Util.Order`).

- Existing: `src/MLF/Elab/Types.hs`
  - Imports `MLF.Types.Elab` and re-exports the types.
  - Keeps Elab-only helpers (Pretty instances, ContextStep/applyContext, utilities) intact.

### Neutral reify helpers

- New: `src/MLF/Reify/Core.hs`
  - Hosts `reifyBoundWithNames`, `reifyBoundWithNamesOnConstraint`,
    `reifyTypeWithNamesNoFallback`, `reifyTypeWithNamesNoFallbackOnConstraint`.
- New: `src/MLF/Reify/TypeOps.hs`
  - Hosts `freeTypeVarsType`, `substTypeSimple`, `stripForallsType`.
- Existing: `src/MLF/Elab/Reify.hs` and `src/MLF/Elab/TypeOps.hs` become thin wrappers that re-export from the neutral modules.

### Plan-driven scheme fallback

- Add a `SchemeTypeChoice` enum (or equivalent) in `MLF.Constraint.Presolution.Plan.ReifyPlan`.
- Compute the choice in `planReify` based on the existing conditions currently inside `MLF.Elab.Generalize`.
- Update `MLF.Elab.Generalize` to select the plan-specified branch (no in-function decision logic).

## Dependency untangling

- Presolution plan modules should import from `MLF.Types.Elab` and `MLF.Reify.*`.
- `MLF.Elab.*` should re-export from neutral modules to avoid churn.

## Testing Strategy

- Run `cabal test`.

