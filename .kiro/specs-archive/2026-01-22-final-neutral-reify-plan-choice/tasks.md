# Implementation Plan

- [x] 1. Create neutral Elab type module and re-export
  - Add `src/MLF/Types/Elab.hs` with core xMLF types and instances.
  - Update `src/MLF/Elab/Types.hs` to import and re-export `MLF.Types.Elab`.
  - Update presolution plan modules to import types from `MLF.Types.Elab`.
  - Requirements: 1.1, 1.2
  - Verification: `rg -n "module MLF.Types.Elab|ElabType" src/MLF/Types/Elab.hs src/MLF/Elab/Types.hs`

- [x] 2. Move reify helpers to neutral modules
  - Create `src/MLF/Reify/Core.hs` and `src/MLF/Reify/TypeOps.hs` with the helper functions.
  - Update `MLF.Elab.Reify` and `MLF.Elab.TypeOps` to re-export from neutral modules.
  - Update presolution plan modules to import from `MLF.Reify.*`.
  - Requirements: 2.1, 2.2
  - Verification: `rg -n "module MLF.Reify.(Core|TypeOps)" src/MLF/Reify`

- [x] 3. Lift scheme-type fallback decision into plan output
  - Add `SchemeTypeChoice` (or equivalent) to `MLF.Constraint.Presolution.Plan.ReifyPlan`.
  - Compute the choice in `planReify` and thread it through `ReifyPlan`.
  - Update `MLF.Elab.Generalize` to execute only the plan-selected branch.
  - Requirements: 3.1, 3.2
  - Verification: `rg -n "SchemeTypeChoice" src/MLF/Constraint/Presolution/Plan/ReifyPlan.hs src/MLF/Elab/Generalize.hs`

- [x] 4. Update cabal module list and test
  - Add new modules to `mlf2.cabal` `other-modules`.
  - Run `cabal test`.
  - Requirements: 4.1
  - Verification: `rg -n "MLF.Types.Elab|MLF.Reify" mlf2.cabal && cabal test`
