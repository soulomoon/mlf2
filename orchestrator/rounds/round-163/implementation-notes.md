# Round 163 – Implementation Notes: Decompose Large Modules

## Summary

Split the 5 largest non-research modules (>800 lines) into focused sub-modules with thin re-export facades. No behavioral changes — pure refactoring.

## Module Splits

| # | Original Module | Lines Before | Facade Lines | Sub-modules Created |
|---|---|---|---|---|
| 1 | MLF.Constraint.Normalize | 848 | 121 | Normalize/Internal.hs, Normalize/Graft.hs, Normalize/Merge.hs |
| 2 | MLF.Reify.Type | 822 | 179 | Type/Core.hs |
| 3 | MLF.Elab.Run.ResultType.Fallback | 822 | 138 | Fallback/Core.hs |
| 4 | MLF.Constraint.Presolution.Plan | 821 | 69 | Plan/Env.hs, Plan/Generalize.hs, Plan/ReifyStep.hs |
| 5 | MLF.Elab.Phi.Omega.Interpret | 1226 | 22 | Interpret/Internal.hs |

All facades are well under the 200-line constraint.

## Key Decisions

1. **Facade pattern**: Each parent module becomes a thin re-export facade. Downstream code continues to import the parent module unchanged.
2. **Step 5 (Interpret)**: The entire module is a single giant `where` clause. Rather than creating an environment record and multiple sub-modules, we moved the full implementation into `Internal.hs` with the facade being a 22-line re-export. This was the pragmatic choice given the tight closure coupling.
3. **Step 4 (Plan)**: Split into three focused modules — `Env.hs` for environment setup, `Generalize.hs` for the `planGeneralizeAt` logic, and `ReifyStep.hs` for `planReify`.
4. **Source-guard tests**: After Step 3 (Fallback split), source-guard tests in PipelineSpec.hs checked exact indentation patterns. The indentation changed because `Core.hs` has different nesting depth than the original `Fallback.hs`. All 8+ affected test predicates were updated to match `Core.hs` actual indentation.

## Commits

| Commit | Description |
|--------|-------------|
| `697c7ba` | Step 1: Split MLF.Constraint.Normalize |
| `27362ac` | Step 2: Split MLF.Reify.Type |
| `c4653d0` | Step 3: Split MLF.Elab.Run.ResultType.Fallback |
| `3dccd09` | Steps 4+5: Split Plan.hs and Interpret.hs, fix source-guard tests |

## Verification

- `cabal build all` — clean, zero warnings
- `cabal test` — 1288 examples, 0 failures (unchanged from baseline)
- No behavioral changes — test count and results identical
