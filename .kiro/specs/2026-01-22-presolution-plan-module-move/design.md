# Design Document

## Overview

Physically move the generalization planning modules out of `MLF.Elab.Generalize.*` into a presolution planning namespace under `MLF.Constraint.Presolution.Plan.*`. This removes the remaining Elab → Presolution boundary violation and keeps Elab as a pure plan consumer.

## Module Move Map (file-by-file)

| Current module | New module | New path |
|---|---|---|
| `MLF.Elab.Generalize.Context` | `MLF.Constraint.Presolution.Plan.Context` | `src/MLF/Constraint/Presolution/Plan/Context.hs` |
| `MLF.Elab.Generalize.Plan` | `MLF.Constraint.Presolution.Plan.Target` (or `Plan`) | `src/MLF/Constraint/Presolution/Plan/Target.hs` (or `Plan.hs` if kept) |
| `MLF.Elab.Generalize.SchemeRoots` | `MLF.Constraint.Presolution.Plan.SchemeRoots` | `src/MLF/Constraint/Presolution/Plan/SchemeRoots.hs` |
| `MLF.Elab.Generalize.BinderPlan` | `MLF.Constraint.Presolution.Plan.BinderPlan` | `src/MLF/Constraint/Presolution/Plan/BinderPlan.hs` |
| `MLF.Elab.Generalize.BinderHelpers` | `MLF.Constraint.Presolution.Plan.BinderHelpers` | `src/MLF/Constraint/Presolution/Plan/BinderHelpers.hs` |
| `MLF.Elab.Generalize.Ordering` | `MLF.Constraint.Presolution.Plan.Ordering` | `src/MLF/Constraint/Presolution/Plan/Ordering.hs` |
| `MLF.Elab.Generalize.ReifyPlan` | `MLF.Constraint.Presolution.Plan.ReifyPlan` | `src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs` |
| `MLF.Elab.Generalize.Normalize` | `MLF.Constraint.Presolution.Plan.Normalize` | `src/MLF/Constraint/Presolution/Plan/Normalize.hs` |
| `MLF.Elab.Generalize.Helpers` | `MLF.Constraint.Presolution.Plan.Helpers` | `src/MLF/Constraint/Presolution/Plan/Helpers.hs` |
| `MLF.Elab.Generalize.BindingUtil` | `MLF.Constraint.Presolution.Plan.BindingUtil` | `src/MLF/Constraint/Presolution/Plan/BindingUtil.hs` |
| `MLF.Elab.Generalize.Names` | `MLF.Constraint.Presolution.Plan.Names` | `src/MLF/Constraint/Presolution/Plan/Names.hs` |
| `MLF.Elab.Generalize.Util` | `MLF.Constraint.Presolution.Plan.Util` | `src/MLF/Constraint/Presolution/Plan/Util.hs` |

Notes:
- Keep the top-level `MLF.Constraint.Presolution.Plan` module as the public entry point that assembles `GeneralizePlan` and `ReifyPlan`.
- The new `Plan.*` modules should not depend on `MLF.Elab.Generalize.*` at all.
- If any module names are already taken (e.g., `Plan.hs`), prefer `Target.hs` or `Core.hs` to avoid ambiguity.

## Dependency Untangling

1. **Remove Elab → Presolution cycles**
   - `MLF.Constraint.Presolution.Plan` currently imports `MLF.Elab.Generalize.*`. After the move, update imports to `MLF.Constraint.Presolution.Plan.*`.
   - `MLF.Elab.Generalize` should import only `MLF.Constraint.Presolution.Plan` and the specific `Plan.*` modules it needs for applying plans.

2. **Shared types and errors**
   - `MLF.Elab.Types` (e.g., `ElabError`) may still be imported by presolution plan modules; this is acceptable if no cycles are created.
   - If a cycle appears, introduce a small neutral error module under `MLF.Util` or `MLF.Constraint` and update imports.

3. **Elab apply layer**
   - `MLF.Elab.Generalize` remains an orchestrator; it should no longer reference old `MLF.Elab.Generalize.*` helpers.
   - `MLF.Elab.Reify` should consume `MLF.Constraint.Presolution.Plan.ReifyPlan` types instead of `MLF.Elab.Generalize.ReifyPlan`.

## Build System Updates

- Update `mlf2.cabal` `other-modules` to remove old `MLF.Elab.Generalize.*` planning modules and add the new `MLF.Constraint.Presolution.Plan.*` modules.
- Keep `MLF.Elab.Generalize` as a public module for applying plans unless replaced by a new `MLF.Elab.Apply` module (optional, not required in this change).

## Testing Strategy

- Run `cabal test` after refactor.

