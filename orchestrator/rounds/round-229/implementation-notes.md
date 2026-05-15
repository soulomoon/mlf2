### Changes Made
- `src/MLF/Binding/Adjustment.hs`: changed the type-only harmonization and raise entrypoints to accept `NodeRefTag 'TypeTag` child refs, deleted the local untyped child-ref discrimination helper, and kept `NodeRef` only on retained ancestor/parent target parameters.
- `src/MLF/Constraint/Normalize/Merge.hs`: updated normalization-time var/var harmonization to pass typed child refs into `MLF.Binding.Adjustment`.
- `src/MLF/Constraint/Presolution/Unify.hs`: updated presolution harmonization to pass typed UF roots into the traced `Binding.Adjustment` seam.
- `src/MLF/Constraint/Solve/Harmonize.hs`: updated solve-phase batch harmonization to pass typed equivalence-class members into multi-node harmonization.
- `src/MLF/Constraint/Unify/Closure.hs`: updated solve-closure batch harmonization to use typed equivalence-class members at the `Binding.Adjustment` boundary.
- `test/BindingSpec.hs`: updated direct `Binding.Adjustment` call sites to the typed child-ref API and added focused coverage that a typed child can still raise to a retained mixed `GenRef` ancestor target.

### Tests
- `test/BindingSpec.hs`: verifies typed child-ref harmonization still preserves binding-tree invariants, replay traces, error behavior, and the retained mixed-ancestor `raiseToParentWithCount` seam.
- `test/GraphOpsSpec.hs`: verifies typed child-ref raise helpers still raise to ancestor targets and preserve the existing `applyRaiseTo` behavior.
- `test/Main.hs`: the full `mlf2-test` suite remained green after the boundary reconciliation.

### Notes
No direct caller required mixed child refs in this round. The retained `NodeRef` seam now stays only on ancestor/parent targets and mixed binding-tree storage, matching the selected plan.

`orchestrator/state.json` was already dirty in this worktree and was intentionally left untouched because controller state is out of scope for the implementer role.
