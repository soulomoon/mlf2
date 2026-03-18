# Round 035 Implementation Notes

- Kept the production slice bounded to `src/MLF/Elab/Run/ResultType/Fallback.hs` by turning `rootBindingIsLocalType` into the retained-target gate for the non-annotated fallback path.
- Retargeted the bounded `PipelineSpec` entrypoint-negative evidence to the same non-local proxy wrapper case `let g = (\x : mu a. a -> Int. x) in g g`, so the block now records direct `computeResultTypeFallback`, unchecked `runPipelineElab`, and checked `runPipelineElabChecked` fail-closed evidence for one shared expression.
- Refreshed the canonical `C2` artifact to mark `attempt-2` and record that same-case triplet without reopening production logic.
- Preserved the repaired `URI-R2-C1` boundary: no `MLF.Elab.Inst` edit, no replay reopen, no equi-recursive/cyclic widening, and no compatibility fallback path.
