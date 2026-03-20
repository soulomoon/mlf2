# Round 055 Selection

Date: 2026-03-20
Round: `round-055`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 22: execute the `I2` bounded local-binding single-base
`baseTarget -> baseC` / same-lane `targetC` fail-closed implementation slice
frozen by `I1`.

## Why This Item Should Run Now

`orchestrator/state.json` already fixes the live controller state at
`active_round_id: "round-055"`, `active_round_dir:
"orchestrator/rounds/round-055"`, `stage: "select-task"`,
`current_task: null`, `retry: null`, and `last_completed_round:
"round-054"`. That means there is no same-round retry to resume, no earlier
interrupted plan/implement/review stage to continue, and no lawful reason to
skip fresh roadmap selection.

`orchestrator/roadmap.md` marks items 1 through 21 done and leaves item 22
(`I2`) as the lowest-numbered unfinished roadmap entry. Items 23 (`I3`) and
24 (`I4`) both depend on item 22, so the guider contract requires selecting
`I2` now unless retry state forces otherwise. The live retry state does not:
`retry` is `null`, and `orchestrator/retry-subloop.md` therefore does not
authorize staying inside any prior round.

The accepted `I1` evidence makes the immediate successor precise rather than
speculative. `orchestrator/rounds/round-054/review-record.json` finalized
`I1` as authoritative with `stage_id: "I1"`, `attempt: 1`,
`attempt_verdict: "accepted"`, `stage_action: "finalize"`, `status:
"authoritative"`, and canonical artifact path
`docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md`. That accepted `I1`
artifact freezes exactly one lawful next slice under repaired `URI-R2-C1`:
the adjacent local-binding single-base `baseTarget -> baseC` fail-closed lane
in `src/MLF/Elab/Run/ResultType/Fallback.hs`, together with its same-lane
`targetC` target-selection use, with future ownership limited to:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-055/src/MLF/Elab/Run/ResultType/Fallback.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-055/test/PipelineSpec.hs`

Because `I1` already performed the bind/selection step, the next lawful stage
is the bounded implementation slice `I2`, not another bind, not direct
verification, and not a broader roadmap amendment.

Accepted predecessor continuity still narrows the subject instead of widening
it. The authoritative `H4` decision chain carried into `I1` preserves the
`continue-bounded` outcome under repaired `URI-R2-C1`, and the completed
predecessor packet in
`tasks/todo/2026-03-11-recursive-types-orchestration/` remains context only:
explicit recursive annotations are accepted on the explicit-only acyclic path,
while automatic recursive-type inference is still not cleared as a broad
successor. That predecessor evidence continues to bind this round to the
explicit-only / non-equi-recursive / non-cyclic-graph boundary.

The accepted `I1` artifact also corrected the stale bug reference that had
appeared in the previous guider selection text. Current bug authority comes
from `/Volumes/src/mlf4/Bugs.md`, whose `Open` section is empty and whose
`BUG-2026-03-16-001` entry is resolved continuity context only. So there is
no live replay bug, `MLF.Elab.Inst` / `InstBot` defect, or other bug-tracker
state that lawfully reopens replay work or displaces the `I1`-frozen next
slice.

Current repository status before writing this selection showed only the
expected controller-state preparation (`M orchestrator/state.json`) and no
conflicting implementation diff. That status does not override roadmap order,
but it does confirm there is no repository-local conflict forcing a different
selection.

## Round Scope Guard

- This round is limited to roadmap item `I2` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.
- Keep the implementation bounded to the exact `I1`-frozen local single-base
  lane: harden only the local-binding `baseTarget -> baseC` path and the same
  lane's `targetC` use in `Fallback.hs`, with matched coverage in
  `PipelineSpec.hs`.
- Preserve the reviewer-auditable proof shape named by `I1`: the selected lane
  must stay local-binding-only and derive from `rootBindingIsLocalType`,
  singleton `rootBoundCandidates`, `not rootHasMultiInst`, and
  `not instArgRootMultiBase`.
- Do not reopen or widen into replay reopen, `MLF.Elab.Inst`, `InstBot`,
  retained-child `boundVarTarget`, `boundTarget` overlay materialization,
  `src/MLF/Elab/Run/ResultType/View.hs`, `schemeBodyTarget` consolidation,
  non-local widening, or any broader trigger family.
- Do not treat this round as verification, review, merge, or roadmap-update
  authority. No controller-state edits, no roadmap edits, and no bug-tracker
  edits belong to this guider selection.
