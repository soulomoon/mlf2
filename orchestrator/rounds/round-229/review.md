# Round 229 Review

Date: 2026-05-15
Round: `round-229`
Milestone: `milestone-1`
Direction: `direction-1a-noderefgadt-reftag-kind`
Extracted item: `binding-adjustment-typed-child-ref-boundary`
Base branch: `master`
Branch: `orchestrator/round-229-noderef-boundary-reconciliation`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Checks Run

- Command: `python3 -m json.tool orchestrator/state.json`
  Result: pass. The live round lineage matches the assigned worktree and branch: `roadmap_id = 2026-05-05-00-type-level-safety-singletons-roadmap`, `roadmap_revision = rev-001`, `roadmap_dir = orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001`, and `active_rounds[0] = round-229` at stage `review`.

- Command: `git status --short`
  Result: pass. Implementation payload is limited to `src/MLF/Binding/Adjustment.hs`, the four bounded internal callers, and `test/BindingSpec.hs`; `orchestrator/state.json` is also dirty, but only as controller-owned stage bookkeeping and is not treated as implementer payload.

- Command: `rg -n "harmonizeBindParentsWithTrace|harmonizeBindParentsMulti|harmonizeBindParents\\b|raiseToParentWithCount\\b|raiseToParent\\b" src test`
  Result: pass. The only direct production callers are the bounded plan scope in [src/MLF/Constraint/Normalize/Merge.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Constraint/Normalize/Merge.hs:172), [src/MLF/Constraint/Presolution/Unify.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Constraint/Presolution/Unify.hs:61), [src/MLF/Constraint/Solve/Harmonize.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Constraint/Solve/Harmonize.hs:15), and [src/MLF/Constraint/Unify/Closure.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Constraint/Unify/Closure.hs:133), plus the focused binding tests.

- Command: `rg -n "expectTypeRef|expectGenRef|requireTypeRef|requireGenRef" src test`
  Result: pass. No live runtime discriminator helper remains in the changed surface; the only match is a comment in `src/MLF/Constraint/Types/Graph/NodeEdge.hs` noting that a runtime `expectTypeRef` check is unnecessary.

- Command: `git diff --check`
  Result: pass. No whitespace or patch-format issues.

- Command: `git diff --name-only -- src-public`
  Result: pass. No public API files changed.

- Command: `git diff --name-only -- mlf2.cabal test/Main.hs`
  Result: pass. No new modules or specs were introduced, so no registration changes were required.

- Command: `cabal build mlf2-test`
  Result: pass. The focused test target builds successfully.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "harmonizeBindParentsWithTrace"'`
  Result: pass. `6 examples, 0 failures`.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "applyRaiseTo"'`
  Result: pass. `4 examples, 0 failures`.

- Command: `cabal test mlf2-test --test-show-details=direct --test-options='--match "raiseToParentWithCount"'`
  Result: pass. `1 example, 0 failures`.

- Command: `cabal build all && cabal test`
  Result: pass. Full behavior-changing gate passed with `2563 examples, 0 failures` in `358.0868` seconds.

## Plan Compliance

- `Inspect MLF.Binding.Adjustment and its direct callers to classify child refs versus mixed parent or ancestor targets`: met. The changed API now takes typed child refs at [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Binding/Adjustment.hs:67), [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Binding/Adjustment.hs:118), and [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Binding/Adjustment.hs:171), while the retained mixed `NodeRef` seam remains only on ancestor or parent targets in [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Binding/Adjustment.hs:171) and [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Binding/Adjustment.hs:199). No direct caller still needs mixed child refs.

- `Change the type-only Binding.Adjustment entrypoints to accept NodeRefTag 'TypeTag and delete the local requireTypeRef runtime conversion`: met. `requireTypeRef` is gone, `harmonizeBindParentsWithTrace` / `harmonizeBindParents` / `harmonizeBindParentsMulti` / `raiseToParentWithCount` / `raiseToParent` / `raiseToRoot` now use `NodeRefTag 'TypeTag`, and the retained mixed `NodeRef` appears only on target parameters and binding-tree lookups ([src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Binding/Adjustment.hs:67), [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Binding/Adjustment.hs:118), [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Binding/Adjustment.hs:171)).

- `Update the bounded caller set and focused tests to use the typed child-ref API`: met. Normalization, presolution, solve, and solve-closure callers now pass `TypeRefTag` values at [src/MLF/Constraint/Normalize/Merge.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Constraint/Normalize/Merge.hs:174), [src/MLF/Constraint/Presolution/Unify.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Constraint/Presolution/Unify.hs:72), [src/MLF/Constraint/Solve/Harmonize.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Constraint/Solve/Harmonize.hs:19), and [src/MLF/Constraint/Unify/Closure.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Constraint/Unify/Closure.hs:139). Binding tests were updated accordingly at [test/BindingSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/test/BindingSpec.hs:1291).

- `Tighten or add focused coverage for the typed child boundary while preserving the retained mixed ancestor seam`: met. `BindingSpec` still exercises harmonization, and it now adds the mixed-ancestor target regression at [test/BindingSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/test/BindingSpec.hs:1426). Existing `applyRaiseTo` ancestor behavior remains covered at [test/GraphOpsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/test/GraphOpsSpec.hs:272).

- `Align only directly affected note/docs wording if drift appears`: met. The round stayed inside the planned internal/test slice and did not widen into `docs/`, `src-public/`, `AGENTS.md`, or other guidance surfaces.

- `Run focused checks first, then diff hygiene and the full gate`: met. Focused build and test slices passed before the full `cabal build all && cabal test` gate, satisfying the verification contract at [orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001/verification.md](/Volumes/src/mlf4/orchestrator/worktrees/round-229/orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001/verification.md:11).

## Decision

**APPROVED**

## Evidence

- The round stays inside the selected `milestone-1` / `direction-1a-noderefgadt-reftag-kind` scope from [orchestrator/rounds/round-229/plan.md](/Volumes/src/mlf4/orchestrator/worktrees/round-229/orchestrator/rounds/round-229/plan.md:1) and does not broaden into unrelated `NodeRef` cleanup or public-surface changes.

- `MLF.Binding.Adjustment` now enforces a typed child boundary: type-only harmonization and raise entrypoints consume `NodeRefTag 'TypeTag`, while mixed `NodeRef` remains only where the plan allows it, on ancestor or parent targets and storage-backed traversal ([src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Binding/Adjustment.hs:67), [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Binding/Adjustment.hs:171), [src/MLF/Binding/Adjustment.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Binding/Adjustment.hs:199)).

- The direct caller set is fully migrated within the bounded scope. No production caller still passes an untyped child ref into `MLF.Binding.Adjustment`; all four direct production call sites now use `TypeRefTag` ([src/MLF/Constraint/Normalize/Merge.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Constraint/Normalize/Merge.hs:174), [src/MLF/Constraint/Presolution/Unify.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Constraint/Presolution/Unify.hs:72), [src/MLF/Constraint/Solve/Harmonize.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Constraint/Solve/Harmonize.hs:19), [src/MLF/Constraint/Unify/Closure.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/src/MLF/Constraint/Unify/Closure.hs:139)).

- Focused tests prove both sides of the intended boundary: typed harmonization still works, and mixed ancestor targets still work through the retained `NodeRef` seam ([test/BindingSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/test/BindingSpec.hs:1291), [test/BindingSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/test/BindingSpec.hs:1426), [test/GraphOpsSpec.hs](/Volumes/src/mlf4/orchestrator/worktrees/round-229/test/GraphOpsSpec.hs:272)).

- `src-public/` is untouched, and no `mlf2.cabal` or `test/Main.hs` registration changes were required. No new type-level dependency, compatibility helper, or public facade widening was introduced.

- `orchestrator/state.json` is dirty in this worktree because the controller records live round stage there. The implementation payload excludes that file; review treats it as controller-owned bookkeeping, not implementer output.

- Milestone closeout should be `status-only` as `pending -> in-progress`, not `done`. The selected round cleanly closes the `Binding.Adjustment` typed-child boundary item, but the broader milestone-1 completion signal in [roadmap.md](/Volumes/src/mlf4/orchestrator/worktrees/round-229/orchestrator/roadmaps/2026-05-05-00-type-level-safety-singletons-roadmap/rev-001/roadmap.md:82) is wider than this one extracted item, so marking the milestone complete would overclaim repo-wide evidence.
