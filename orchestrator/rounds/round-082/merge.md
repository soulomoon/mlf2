# Merge Preparation (`round-082` / `item-1`)

## Squash Commit Title

`Define general iso-recursive inference capability contract and corpus`

## Summary

- Merge the approved docs-only `item-1` packet for the refreshed strategic
  automatic iso-recursive control plane.
- The canonical artifact
  `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  freezes `round-082` / `item-1` / `attempt-1` / `retry: null` as one
  capability-definition and evaluation-corpus round only.
- The accepted payload turns the strategic roadmap into one concrete repo-level
  capability contract: it defines what "general automatic iso-recursive
  inference" means in this repo, separates bounded predecessor evidence from a
  later repo-level claim, establishes the positive and negative family matrix,
  and records later success and no-claim gates without claiming feasibility,
  architecture fit, search correctness, or reconstruction correctness.
- The approved round diff remains docs/orchestrator only in stage effect: the
  canonical capability-contract artifact, `selection.md`, `plan.md`,
  `implementation-notes.md`, `review.md`, `review-record.json`,
  `reviews/attempt-1.md`, and this merge-preparation note. No implementation
  code, tests, public-surface, executable, Cabal, roadmap, retry-contract,
  verification-contract, bug-tracker, or controller-state edit belongs to this
  merge payload.

## Review And Retry Confirmation

- The latest review snapshot is
  `orchestrator/rounds/round-082/reviews/attempt-1.md`, and `review.md`
  matches that snapshot.
- The latest review snapshot is lawful `accepted + finalize` and records:
  - Implemented stage result: `pass`
  - Attempt verdict: `accepted`
  - Stage action: `finalize`
  - Retry reason: `none`
  - Fix hypothesis: `none`
- The authoritative retry summary matches `review-record.json` exactly:
  - `stage_id: "item-1"`
  - `attempt: 1`
  - `attempt_verdict: "accepted"`
  - `stage_result: "pass"`
  - `stage_action: "finalize"`
  - `retry_reason: "none"`
  - `fix_hypothesis: "none"`
  - `status: "authoritative"`
  - `authoritative_attempt: 1`
  - `authoritative_result: "pass"`
  - `artifact_path: "docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md"`
  - `review_snapshot: "orchestrator/rounds/round-082/reviews/attempt-1.md"`
  - `final_outcome: "repo-level-capability-contract-and-evaluation-corpus-defined"`
- `orchestrator/retry-subloop.md` permits merge preparation only after
  `accepted + finalize`, and the active worktree controller state for this
  round is already `active_round_id: "round-082"`, `stage: "merge"`,
  `current_task: "item-1"`, and `retry: null`.
- No same-round retry remains open: `attempt-1` is authoritative and no
  further retry is lawful under the current review result.

## Predecessor Continuity Note

- This round does not reset authority. Completed rounds `round-001` through
  `round-081` remain authoritative historical evidence for the refreshed
  control plane exactly as `orchestrator/roadmap.md` and
  `orchestrator/retry-subloop.md` describe.
- The inherited baseline contract from
  `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains binding: automatic recursive-type inference is still unresolved at
  repo level, recursive meaning remains explicit and iso-recursive, and the
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
  boundary remains mandatory unless a later accepted roadmap item explicitly
  changes it.
- The accepted `N14` decision artifact at
  `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-next-cycle-decision-gate.md`
  remains the immediate predecessor authority. It still contributes one exact
  same-lane retained-child `boundVarTarget -> targetC` packet as bounded
  predecessor evidence only, not proof of general capability and not authority
  to widen the solver, representation, or interface boundary.
- This round updates strategic authority only by defining the repo-level
  capability contract and evaluation-corpus matrix for later items to audit
  against. It does not reinterpret bounded predecessor packets as broader
  accepted evidence, it does not revise the inherited boundary, and it does
  not choose the next roadmap item.
- Nothing in this merge note authorizes implementation work, solver
  experiments, packet-to-mechanism generalization, search design,
  reconstruction design, coverage claims, equi-recursive reasoning, cyclic
  structural graphs, multi-SCC handling, second interfaces, or
  compatibility/default-path widening.

## Follow-Up Notes

- Post-merge guider/controller work should treat the canonical capability
  contract artifact, `review.md`, and `review-record.json` as the
  authoritative `item-1` outcome for the refreshed strategic control plane.
- Later work must use this contract as the bounded authority for item `2` and
  beyond; this note does not perform the architectural audit, mechanism-map
  generalization, search design, reconstruction contract, coverage campaign,
  or architecture decision.
- This note prepares squash merge only. It does not merge the round, edit
  controller-owned state, or select the next roadmap item.

## Ready For Squash Merge

Yes. The latest review snapshot is `accepted + finalize`, the authoritative
review record matches the finalized review snapshot, the active round is
already at `stage: "merge"` with `retry: null`, predecessor authority and the
inherited baseline remain unchanged, and the approved payload stays within one
docs-only `item-1` capability-contract-and-corpus definition round.
