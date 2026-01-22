# Task 4: apply-plans

## Goal
- Thin Elab to apply GeneralizePlan/ReifyPlan instead of recomputing planning logic.

## Scope
- src/MLF/Elab/Generalize.hs
- src/MLF/Elab/Apply.hs (new or existing)
- src/MLF/Elab/Reify.hs

## Steps
1. Introduce apply functions that consume GeneralizePlan/ReifyPlan.
2. Remove planning logic from Elab.Generalize, delegating to plans.
3. Keep Elab behavior unchanged while using plan data.

## Verification
- rg -n "GeneralizePlan|ReifyPlan" src/MLF/Elab
