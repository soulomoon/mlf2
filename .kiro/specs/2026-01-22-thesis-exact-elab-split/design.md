# Design Document

## Overview

This refactor makes the Elab layer thesis-faithful by relocating planning/decision logic into presolution. Elab becomes a thin application layer that consumes plan records plus Ω/Φ witnesses and expansion recipes. The pipeline becomes: Frontend → Normalize → Presolution (solve + plan) → Elab (apply).

## Architecture

### New/expanded modules

- `MLF.Constraint.Presolution.Plan`
  - Owns planning data structures and builders.
  - Produces explicit `GeneralizePlan` and `ReifyPlan` records.

- `MLF.Elab.Apply` (new)
  - Applies `GeneralizePlan`/`ReifyPlan` plus Ω/Φ witnesses to annotated terms/types.
  - Contains only structural traversals and rewrites; no new policy decisions.

### Simplified modules

- `MLF.Elab.Generalize`
  - Becomes a thin orchestrator (or is removed in favor of `Elab.Apply`).
  - No longer computes binder selection, scheme roots, ordering dependencies, alias policy, or plan metadata.

- `MLF.Elab.Reify`
  - Consumes `ReifyPlan`; remains a pure structural reification pass.

- `MLF.Elab.Run.Pipeline`
  - Wires presolution planning outputs into Elab apply functions.

## Interfaces and Data Models

### Plan records

```haskell
-- MLF.Constraint.Presolution.Plan

data GeneralizePlan = GeneralizePlan
  { gpScopeRoot      :: NodeId
  , gpSchemeRoots    :: SchemeRootsPlan
  , gpBinderPlan     :: BinderPlan
  , gpOrderingDeps   :: IntMap.IntMap IntSet.IntSet
  , gpAliasPolicy    :: AliasPolicy
  , gpBindParents    :: BindParents
  , gpDebugInfo      :: Maybe DebugInfo
  }

data ReifyPlan = ReifyPlan
  { rpRootChoices    :: RootChoices
  , rpSubstMap       :: IntMap.IntMap NodeId
  , rpAliasMap       :: IntMap.IntMap NodeId
  , rpBaseSolvedLink :: BaseSolvedAlignment
  }
```

### Builder functions

```haskell
planGeneralize :: PresolutionEnv -> AnnExpr -> Either ElabError GeneralizePlan
planReify      :: PresolutionEnv -> GeneralizePlan -> Either ElabError ReifyPlan
```

### Elab apply functions

```haskell
applyGeneralizePlan :: GeneralizePlan -> ReifyPlan -> Witnesses -> AnnExpr -> Either ElabError ElabTerm
```

(Exact names can be adjusted; the key requirement is that Elab applies plans rather than making planning decisions.)

## Error Handling

- Plan builders reuse existing `ElabError` and presolution error types.
- Elab apply functions remain total (return `Either ElabError`).
- Preserve current error messages to avoid behavior drift.

## Testing Strategy

- `cabal test` after completing the refactor.
- No new tests required unless behavior changes are observed.

## Migration Notes

- Avoid circular imports: presolution planning must not depend on Elab modules.
- Keep `GeneralizePlan`/`ReifyPlan` opaque outside presolution and Elab apply modules where possible.

