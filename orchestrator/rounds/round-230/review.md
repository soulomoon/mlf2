# Round 230 Review

Date: 2026-05-15
Round: `round-230`
Milestone: `milestone-1`
Direction: `direction-1a-noderefgadt-reftag-kind`
Extracted item: `milestone-1-closeout-audit-and-guard`
Base branch: `master`
Branch: `orchestrator/round-230-noderef-boundary-closeout`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Checks Run

- Command: `python3 -m json.tool orchestrator/state.json`
  Result: pass. The live round lineage matches the assigned worktree and branch: `roadmap_id = 2026-05-05-00-type-level-safety-singletons-roadmap`, `roadmap_revision = rev-001`, `roadmap_dir = orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001`, and `active_rounds[0] = round-230` at stage `review`.

- Command: `git status --short`
  Result: pass. Implementation payload is limited to `src/MLF/Binding/GraphOps.hs` and `test/GraphOpsSpec.hs`; `orchestrator/state.json` is also dirty, but only as controller-owned stage bookkeeping and is not treated as round payload.

- Command: `rg -n "expectTypeRef|expectGenRef|requireTypeRef|requireGenRef" src test`
  Result: pass. No live runtime discriminator helper remains in `src/` or `test/`; the only match is a comment in [src/MLF/Constraint/Types/Graph/NodeEdge.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Constraint/Types/Graph/NodeEdge.hs:199) noting that the typed extractor is total.

- Command: `rg -n "getBindFlag ::|isInstantiable ::|isLocked ::|applyRaiseTo ::|raiseToParentWithCount ::|harmonizeBindParentsWithTrace ::|type BindParents =|allNodeRefs ::|nodeRefExists ::" src/MLF/Binding/GraphOps.hs src/MLF/Binding/Adjustment.hs src/MLF/Binding/NodeRefs.hs src/MLF/Constraint/Types/Graph/Binding.hs`
  Result: pass. The selected owners now show the intended split: type-only GraphOps and Adjustment child seams use `NodeRefTag 'TypeTag` at [src/MLF/Binding/GraphOps.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/GraphOps.hs:71), [src/MLF/Binding/GraphOps.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/GraphOps.hs:82), [src/MLF/Binding/GraphOps.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/GraphOps.hs:108), [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/Adjustment.hs:67), [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/Adjustment.hs:118), and [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/Adjustment.hs:171), while the retained mixed seam is limited to binding-tree storage and mixed ancestor or parent targets at [src/MLF/Constraint/Types/Graph/Binding.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Constraint/Types/Graph/Binding.hs:49), [src/MLF/Binding/NodeRefs.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/NodeRefs.hs:12), [src/MLF/Binding/NodeRefs.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/NodeRefs.hs:17), [src/MLF/Binding/GraphOps.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/GraphOps.hs:196), and [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/Adjustment.hs:171).

- Command: `git diff --check`
  Result: pass. No whitespace or patch-format issues.

- Command: `git diff --name-only -- src-public`
  Result: pass. No public API files changed.

- Command: `git diff --name-only -- mlf2.cabal test/Main.hs`
  Result: pass. No new modules or specs were introduced, so no registration changes were required.

- Command: `python3 - <<'PY' ...`
  Result: pass. `roadmap-view.json` reports `milestone-1` currently `in-progress`, and the status anchor `milestone-1`, completion anchor `milestone-1-completion`, and history anchor `roadmap-history-completed-rounds` all resolve.

- Command: `cabal build mlf2-test`
  Result: pass. The focused test target builds successfully.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "Binding shared abstractions"'`
  Result: pass. `7 examples, 0 failures`.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "harmonizeBindParentsWithTrace"'`
  Result: pass. `6 examples, 0 failures`.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "applyRaiseTo"'`
  Result: pass. `5 examples, 0 failures`.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "raiseToParentWithCount"'`
  Result: pass. `1 example, 0 failures`.

- Command: `cabal build all && cabal test`
  Result: pass. Full behavior-changing gate passed with `2564 examples, 0 failures` in `359.8944` seconds.

## Plan Compliance

- `Audit MLF.Constraint.Types.Graph.NodeEdge, MLF.Constraint.Types.Graph.Binding, MLF.Binding.GraphOps, MLF.Binding.Adjustment, MLF.Binding.NodeRefs, and their direct tests/docs against the milestone-1 completion signal`: met. The selected-owner audit shows the only remaining mixed `NodeRef` entrypoints are the documented binding-tree storage and ancestor/parent target seams at [src/MLF/Constraint/Types/Graph/Binding.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Constraint/Types/Graph/Binding.hs:49), [src/MLF/Binding/NodeRefs.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/NodeRefs.hs:12), [src/MLF/Binding/NodeRefs.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/NodeRefs.hs:17), [src/MLF/Binding/GraphOps.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/GraphOps.hs:201), and [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/Adjustment.hs:172).

- `If the audit finds one residual type-only gap, migrate only that owner-local seam to NodeRefTag or SomeNodeRef and update bounded callers`: met. The only residual type-only gap in scope was the GraphOps predicate/query surface, which now takes `NodeRefTag 'TypeTag` for `getBindFlag`, `isInstantiable`, and `isLocked` at [src/MLF/Binding/GraphOps.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/GraphOps.hs:71), [src/MLF/Binding/GraphOps.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/GraphOps.hs:82), and [src/MLF/Binding/GraphOps.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/GraphOps.hs:108). No broader pipeline or public-surface spill occurred.

- `Tighten the acceptance evidence for the retained mixed seam with focused tests and directly affected docs/comments only if needed`: met. The retained mixed-target seam is now covered explicitly in [test/GraphOpsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/test/GraphOpsSpec.hs:295) and [test/BindingSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/test/BindingSpec.hs:1427), and the durable architecture wording was already accurate at [docs/architecture.md](/Volumes/src/mlf4/orchestrator/worktrees/round-230/docs/architecture.md:112), so no docs drift remained to fix.

- `Prove the runtime discriminator story directly with a focused source scan and inspection of any remaining NodeRef pattern matches in the touched area`: met. The helper-name source scan found no runtime type/gen discriminator helpers, and the remaining `NodeRef` pattern matches are owner-local mixed operations in [src/MLF/Binding/NodeRefs.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/NodeRefs.hs:17) and the existential conversion point in [src/MLF/Constraint/Types/Graph/NodeEdge.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Constraint/Types/Graph/NodeEdge.hs:193), not ad hoc type-only conversions.

- `Run the narrow binding test slices touched by the round, then diff hygiene, then the full cabal gate`: met. Focused build/tests, `git diff --check`, and the full `cabal build all && cabal test` gate all passed.

## Decision

**APPROVED**

## Evidence

- The round stays inside the selected `milestone-1` / `direction-1a-noderefgadt-reftag-kind` closeout-audit scope from [orchestrator/rounds/round-230/plan.md](/Volumes/src/mlf4/orchestrator/worktrees/round-230/orchestrator/rounds/round-230/plan.md:1). The only implementation edits are the residual GraphOps type-only seam and its direct tests; `src-public/` is untouched.

- The milestone-1 typed-boundary story is now complete in the selected owners. GraphOps predicate/query helpers now accept typed references at [src/MLF/Binding/GraphOps.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/GraphOps.hs:71), [src/MLF/Binding/GraphOps.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/GraphOps.hs:82), and [src/MLF/Binding/GraphOps.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/GraphOps.hs:108), matching the already-typed Adjustment child boundary at [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/Adjustment.hs:67), [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/Adjustment.hs:118), and [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/Adjustment.hs:171).

- The retained mixed `NodeRef` seam is limited to the uses the roadmap allows: binding-tree storage and mixed ancestor/parent targets. That boundary is explicit in the storage owner [src/MLF/Constraint/Types/Graph/Binding.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Constraint/Types/Graph/Binding.hs:49), in mixed enumeration/existence helpers [src/MLF/Binding/NodeRefs.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/NodeRefs.hs:12), [src/MLF/Binding/NodeRefs.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/NodeRefs.hs:17), and in the explicit mixed-target comments and signatures for Raise-to-parent operations at [src/MLF/Binding/GraphOps.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/GraphOps.hs:196) and [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Binding/Adjustment.hs:171). The durable architectural description already matches that shape at [docs/architecture.md](/Volumes/src/mlf4/orchestrator/worktrees/round-230/docs/architecture.md:112).

- The runtime discriminator helper story is closed for milestone-1. `expectTypeRef`, `expectGenRef`, `requireTypeRef`, and `requireGenRef` are absent from live code and tests; the only remaining mention is the explanatory comment at [src/MLF/Constraint/Types/Graph/NodeEdge.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Constraint/Types/Graph/NodeEdge.hs:199). The typed/untyped conversion points are now explicit boundary helpers at [src/MLF/Constraint/Types/Graph/NodeEdge.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Constraint/Types/Graph/NodeEdge.hs:188) and [src/MLF/Constraint/Types/Graph/NodeEdge.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/src/MLF/Constraint/Types/Graph/NodeEdge.hs:193), not ad hoc call-site checks.

- Focused tests now cover both halves of the accepted boundary: typed GraphOps predicates/queries and the retained mixed ancestor target in [test/GraphOpsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/test/GraphOpsSpec.hs:94), [test/GraphOpsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/test/GraphOpsSpec.hs:110), and [test/GraphOpsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/test/GraphOpsSpec.hs:295); typed harmonization plus the retained mixed ancestor target in [test/BindingSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/test/BindingSpec.hs:1371) and [test/BindingSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-230/test/BindingSpec.hs:1427).

- Milestone closeout can stay `status-only`. The active roadmap meaning, direction meaning, sequencing, and verification contract do not change; the round closes the last selected-owner residual seam and supplies the focused plus full-gate evidence the milestone completion signal requires. `milestone-1` may therefore move from `in-progress` to `done` without a semantic roadmap update.
