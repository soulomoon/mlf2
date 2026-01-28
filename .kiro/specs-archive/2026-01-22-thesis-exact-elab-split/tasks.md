# Implementation Plan

- [x] 1. Add presolution planning module and data records
  - Create `MLF.Constraint.Presolution.Plan` with `GeneralizePlan`/`ReifyPlan` and any needed supporting types.
  - Add builder stubs for `planGeneralize` and `planReify`.
  - Requirements: 1.1
  - Verification: `rg -n "data GeneralizePlan|data ReifyPlan|planGeneralize|planReify" src/MLF/Constraint/Presolution/Plan.hs`

- [x] 2. Move generalization planning logic into presolution
  - Relocate Phase 1–6 logic (scope root, scheme-root metadata, binder selection, ordering deps, alias policy decisions) from Elab into `planGeneralize`.
  - Adjust imports to avoid Elab → Presolution dependency cycles.
  - Requirements: 1.2, 2.2
  - Verification: `rg -n "planGeneralize" src/MLF/Constraint/Presolution/Plan.hs`

- [x] 3. Move reify planning logic into presolution
  - Relocate Phase 8–10 planning (root selection, alias/subst maps, base/solved alignment) into `planReify`.
  - Leave reify application in Elab.
  - Requirements: 1.2, 2.2
  - Verification: `rg -n "planReify" src/MLF/Constraint/Presolution/Plan.hs`

- [x] 4. Thin Elab to apply plans
  - Introduce `MLF.Elab.Apply` (or update existing modules) to apply `GeneralizePlan`/`ReifyPlan` plus witnesses.
  - Remove/trim planning logic from `MLF.Elab.Generalize` and keep it as orchestration only.
  - Requirements: 2.1, 2.2
  - Verification: `rg -n "GeneralizePlan|ReifyPlan" src/MLF/Elab`

- [x] 5. Update pipeline wiring
  - Update `MLF.Elab.Run.Pipeline` to build plans in presolution and pass them into Elab apply functions.
  - Requirements: 3.1, 3.2
  - Verification: `rg -n "planGeneralize|planReify" src/MLF/Elab/Run/Pipeline.hs`

- [x] 6. Test
  - Run full test suite.
  - Requirements: 4.1
  - Verification: `cabal test`
