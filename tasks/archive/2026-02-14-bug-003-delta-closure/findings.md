# Findings: BUG-003 Delta Closure

## 2026-02-14 Initial Findings
- Existing worktree already modifies core files in this scope:
  - src/MLF/Constraint/Presolution/Base.hs
  - src/MLF/Constraint/Presolution/Witness.hs
  - src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs
  - src/MLF/Elab/Phi/Translate.hs
  - src/MLF/Elab/Phi/Omega.hs
  - test/ElaborationSpec.hs
  - test/Presolution/EdgePlannerSpec.hs
- BUG-003 currently fails with `OpGraft targets non-binder node` and binder key mismatch in Phi translation.
- `implicitBindersM` remains present in Base.hs and should be removed once replacement path is complete.

## 2026-02-14 Delta-closure findings
- `psBinderCache` is now provenance-aware (`IntMap (IntMap [NodeId])`) keyed by `(GenNodeId, NodeId)` via `binderCacheLookup`/`binderCacheInsert`, preventing cross-scheme binder cache leakage.
- `instantiationBindersFromGenM` in `src/MLF/Constraint/Presolution/Base.hs` is already using ForallIntro-aligned ordering primitives (`boundFlexChildrenUnder`, reachability under body root, canonical bind-parent map, order-key sorting, dependency topo-order), so no new structural rewrite was required in this pass.
- `test/Presolution/EdgePlannerSpec.hs` ann-edge suppress-weaken regression is green with current planner logic (`suppressWeaken = IntSet.member eidInt (cAnnEdges constraint0)`).
- Removed unconditional `Debug.Trace`/`DEBUG ...` output from:
  - `src/MLF/Constraint/Presolution/Witness.hs`
  - `src/MLF/Constraint/Presolution/EdgeProcessing/Witness.hs`
  - `src/MLF/Elab/Phi/Translate.hs`
  - `src/MLF/Elab/Phi/Omega.hs`
  - `src/MLF/Elab/Elaborate.hs`
  - `src/MLF/Elab/Run/Pipeline.hs`
- Targeted regression evidence after cleanup:
  - `--match "BUG-003-V"`: `2 examples, 0 failures`
  - `--match "BUG-004"`: `4 examples, 0 failures`
  - `--match "threads ann-edge flag into suppressWeaken"`: `1 example, 0 failures`
