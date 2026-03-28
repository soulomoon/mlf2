# Round 139 Implementation Notes

## Change summary

- Added `breakCyclesAndCheckAcyclicity` to `MLF.Constraint.Acyclicity`.
- Implemented deterministic single-cycle rewriting that picks the smallest `EdgeId`, clones the pivot RHS, substitutes cycle-reachable nodes with a fresh μ binder, inserts a `TyMu`, and rewires binding parents for the new nodes.
- Wired `MLF.Elab.Run.Pipeline` to feed the rewritten constraint graph into presolution and all downstream reconstruction paths.
- Added focused regression coverage in `test/AcyclicitySpec.hs` and `test/PipelineSpec.hs` for cycle breaking, `TyMu` insertion, recursive pipeline smoke, and non-recursive stability.

## Focused evidence

- `dist-newstyle/build/aarch64-osx/ghc-9.12.2/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "Acyclicity Check"`
- `dist-newstyle/build/aarch64-osx/ghc-9.12.2/mlf2-0.2.0.0/t/mlf2-test/build/mlf2-test/mlf2-test --match "Pipeline"`
