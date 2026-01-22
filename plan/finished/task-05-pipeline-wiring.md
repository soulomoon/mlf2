# Task 5: pipeline-wiring

## Goal
- Build generalize/reify plans in presolution and pass them into Elab apply functions.

## Scope
- src/MLF/Elab/Run/Pipeline.hs
- src/MLF/Elab/Generalize.hs (apply entry point)

## Steps
1. Identify generalizeAt call sites in pipeline.
2. Replace with planGeneralizeAt/planReify + applyGeneralizePlan.
3. Keep behavior identical and preserve debug logging.

## Verification
- rg -n "planGeneralize|planReify" src/MLF/Elab/Run/Pipeline.hs
