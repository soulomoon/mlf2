# Round 139 Merge Notes

## Squash commit title
Implement cycle detection and automatic μ-introduction in the constraint solver

## Commit body summary
Added a cycle-breaking acyclicity pass that detects dependency cycles, rewrites one cycle at a time, and introduces `TyMu` nodes deterministically. Wired the pipeline to use the rewritten constraint before presolution so recursive inputs now proceed through the solver instead of failing at the acyclicity stage. Added focused regression coverage for cycle breaking and non-recursive stability.

## Changed files
- `src/MLF/Constraint/Acyclicity.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
- `test/AcyclicitySpec.hs`
- `test/PipelineSpec.hs`

## Follow-up notes
Item-2 should address elaboration-side roll/unroll shaping so the newly introduced recursive structure is reflected cleanly in explicit xMLF output.

## Merge readiness
Round 139 is ready for squash merge.
