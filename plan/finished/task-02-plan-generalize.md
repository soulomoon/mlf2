# Task 2: plan-generalize

## Goal
- Move generalization planning logic (Phase 1–6) into Presolution.Plan.planGeneralize.

## Scope
- src/MLF/Constraint/Presolution/Plan.hs
- src/MLF/Elab/Generalize.hs and related helper modules

## Steps
1. Identify Phase 1–6 planning logic in Elab.Generalize.
2. Relocate logic into planGeneralize, avoiding Elab -> Presolution cycles.
3. Adjust imports and callers as needed.

## Verification
- rg -n "planGeneralize" src/MLF/Constraint/Presolution/Plan.hs
