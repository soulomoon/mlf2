# Merge Preparation (`round-207` / `milestone-2` / `direction-2a`)

## Roadmap Identity

- `roadmap_id`: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-001`
- `milestone_id`: `milestone-2`
- `direction_id`: `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation`
- `roadmap_item_id`: absent in the active runtime state and round selection for this roadmap family; the selected item is recorded via `extracted_item_id`
- `extracted_item_id`: `implement-same-wrapper-nested-forall-target-selection-and-term-closure-seams`

## Squash Commit Title

`Align same-wrapper nested-forall fallback target and scope`

## Scope Summary

- Merge the approved code-bearing `milestone-2` /
  `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation` /
  `implement-same-wrapper-nested-forall-target-selection-and-term-closure-seams`
  slice for the active broader-positive explicit-boundary enactment roadmap.
- This was a code-bearing `milestone-2` / `direction-2a` round. Its
  implementation-owned payload is exactly
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`, plus the round-local artifacts under
  `orchestrator/rounds/round-207/`.
- The production change stays inside `computeResultTypeFallbackCore`: the
  same-wrapper retained-child path now carries the chosen recursive target
  together with the aligned generalization scope, threads
  `boundHasForallFrom` through that aligned proof, and feeds
  `generalizeWithPlan` the matching scope instead of preserving the old
  ownerless target/scope split.
- The focused tests now prove the selected internal behavior shift honestly:
  the same-wrapper nested-`forall` packet preserves recursive fallback shape
  internally, the clear-boundary controls remain recursive, the preserved
  negative/control families remain fail-closed, and the authoritative
  pipeline/public entrypoints still fail closed in this round.
- The round stayed inside the frozen writable slice authorized by accepted
  `round-206`. It did not widen into `src/MLF/Elab/Run/Scope.hs`, any
  `src/MLF/Constraint/**` planner surface, cyclic or equi-recursive
  reasoning, fallback rescue, or a second interface.
- Pipeline/public threading files stayed untouched:
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`, and
  `src-public/MLF/Pipeline.hs` are outside this payload.
  `src/MLF/Elab/Run/ResultType/Fallback.hs` and
  `src/MLF/Elab/TermClosure.hs` also remained untouched because the bounded
  fallback-core rewrite made the selected slice green without a truthful need
  to widen further.
- Keep the squash scope honest: this merge is one bounded code-bearing
  `milestone-2` / `direction-2a` mechanism slice plus round-local artifacts
  only. It does not claim milestone-2 completion, milestone-3 authoritative
  success, or full broader-positive closure, and the controller-owned
  `orchestrator/state.json` edit remains outside the merge payload.

## Review Confirmation

- `orchestrator/rounds/round-207/review.md` records `Decision: APPROVED` for
  this code-bearing `milestone-2` / `direction-2a` round and verifies the
  focused tests plus the full `cabal build all && cabal test` gate.
- `orchestrator/rounds/round-207/review-record.json` matches the same roadmap
  identity and records `decision: approved` for
  `implement-same-wrapper-nested-forall-target-selection-and-term-closure-seams`.
- The approved review evidence explicitly says the diff stayed inside the
  frozen writable slice once controller-owned bookkeeping is excluded, kept
  pipeline/public threading untouched, preserved the retained-child and
  negative/control guardrails, and left authoritative entrypoints fail-closed
  in this round.

## Merge Readiness

- Merge readiness: confirmed for the approved code-bearing
  `milestone-2` / `direction-2a` round, provided the squash stays limited to
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
  `test/PipelineSpec.hs`,
  `test/Research/P5ClearBoundarySpec.hs`, and the round-local artifacts under
  `orchestrator/rounds/round-207/`, while continuing to exclude the
  controller-owned `orchestrator/state.json`.
- Base branch freshness: confirmed locally.
  `HEAD` resolves to `3ce69d5b3945e8d520a7a6270cfb3a020a83fc84`,
  `codex/automatic-recursive-type-inference` resolves to the same commit,
  `git merge-base HEAD codex/automatic-recursive-type-inference` resolves to
  the same commit, and
  `git rev-list --left-right --count codex/automatic-recursive-type-inference...HEAD`
  reports `0 0`.
- Base branch freshness: also confirmed against the current remote head.
  `git ls-remote origin refs/heads/codex/automatic-recursive-type-inference`
  resolves to `fde4339b0c8c554ede0d246129bd2c9bf5d4c112`, and
  `git rev-list --left-right --count codex/automatic-recursive-type-inference...fde4339b0c8c554ede0d246129bd2c9bf5d4c112`
  reports `277 0`, so the local base branch already contains the current
  remote tip.
- Round `round-207` is ready for squash merge.

## Follow-Up Notes

- Post-merge controller bookkeeping should preserve the roadmap identity above
  unchanged and keep the selected
  `milestone-2` / `direction-2a-implement-core-polymorphic-mediation-recursive-structure-preservation` /
  `implement-same-wrapper-nested-forall-target-selection-and-term-closure-seams`
  association intact; no `roadmap_item_id` exists in the active runtime
  state.
- Keep later summaries exact: this round repairs one bounded internal
  mechanism slice and its focused evidence. It does not yet thread the
  selected behavior through the authoritative pipeline/public seams and does
  not by itself establish broader-positive success on both
  `runPipelineElab` and `runPipelineElabChecked`.
