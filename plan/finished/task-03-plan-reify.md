# Task 3: plan-reify

## Goal
- Move reify planning logic (Phase 8–10) into Presolution.Plan.planReify.

## Scope
- src/MLF/Constraint/Presolution/Plan.hs
- src/MLF/Elab/Generalize.hs and MLF.Elab.Generalize.ReifyPlan

## Steps
1. Identify Phase 8–10 planning logic in Elab.Generalize.
2. Relocate the reify plan construction into planReify.
3. Update imports and keep Elab applying the plan.

## Verification
- rg -n "planReify" src/MLF/Constraint/Presolution/Plan.hs
