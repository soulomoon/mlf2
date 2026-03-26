# Round 117 Plan (`item-1` Retry Delta Attempt-2 Recovery Repair)

## Objective

Execute only the same-round `attempt-2` recovery repair for roadmap item `1`.

This plan does not reopen the docs-only successor-boundary freeze itself.
It only repairs artifact ownership so the current reviewer can lawfully write
`orchestrator/rounds/round-117/review.md` for `attempt-2`.

The round remains docs-only, aggregate-only, and limited to roadmap item `1`
only. No parallel lane split is authorized.

## Locked Retry Context

- Round id: `round-117`
- Roadmap item: `item-1`
- Stage: `plan`
- Active attempt: `attempt-2`
- Latest attempt verdict: `rejected`
- Latest stage action: `retry`
- Retry reason:
  the recovery-investigator found that the live `attempt-2` plan froze the
  reviewer-owned `orchestrator/rounds/round-117/review.md`, which conflicts
  with the reviewer role contract and makes the next review attempt unlawful
  as planned.
- Recorded fix hypothesis:
  remove `review.md` from the immutable prior-attempt set, keep
  `reviews/attempt-1.md` and the other true prior-attempt surfaces frozen,
  and leave `review.md` available for the `attempt-2` reviewer to write.
- Active actionable slice:
  successor-boundary freeze contract repair only.

## Carried-Forward Item-1 Boundary That Still Binds

This recovery repair changes only artifact ownership and writability. The
already-established item-1 successor-boundary freeze remains unchanged:

- `docs/plans/2026-03-27-post-rev-004-repo-scope-successor-authority-evidence-inputs-and-non-widening-boundary-freeze.md`
  remains the only authoritative item-1 freeze artifact;
- `orchestrator/rounds/round-117/implementation-notes.md` remains an
  authorized non-authoritative companion notes surface only;
- the March 14 baseline contract, the March 25 capability /
  full-pipeline / architecture-decision contracts, the March 26 global gate
  as historical aggregate evidence only, and the accepted rev-004 same-lane
  settlement chain remain the binding predecessor read carried into item `1`;
- the same-lane `C2` / `C5` / `C7` pocket remains settled predecessor truth,
  not live repo-scope debt;
- current local packets and research harnesses remain non-authoritative
  planning inputs until republished in round-owned artifacts;
- roadmap item `2` remains the next lawful move after this freeze; and
- no implementation work, hardening, rollout, equi-recursive reasoning,
  cyclic structural graphs, multi-SCC search, second interfaces, fallback
  widening, or broad capability claims are authorized.

## Artifact Ownership And Writability

### Authoritative And Companion Output Surfaces

The lawful round-owned output set for item `1` remains exactly:

1. `docs/plans/2026-03-27-post-rev-004-repo-scope-successor-authority-evidence-inputs-and-non-widening-boundary-freeze.md`
   - role: the only authoritative item-1 freeze artifact.
2. `orchestrator/rounds/round-117/implementation-notes.md`
   - role: authorized companion notes surface only;
   - allowed content: concise change summary and verification notes for the
     same docs-only item-1 freeze;
   - forbidden content: any new authority claim, alternate decision, side
     ledger, or replacement freeze surface.

No additional round-local output surface is authorized.

### Reviewer-Owned Live Surface

`orchestrator/rounds/round-117/review.md` is not part of the immutable
prior-attempt set.

Per `roadmap_dir/retry-subloop.md`, `review.md` remains the single-writer
reviewer-owned live review surface for the current attempt. For this repair:

- the `attempt-2` reviewer must be free to rewrite `review.md`;
- planner and implementer stages must not treat `review.md` as a frozen
  attempt-1 artifact;
- byte-for-byte preservation of the attempt-1 review lives at
  `orchestrator/rounds/round-117/reviews/attempt-1.md`; and
- `review.md` may be replaced during the lawful `attempt-2` review stage
  without violating prior-attempt immutability.

## Immutable Prior-Attempt Surfaces

The following true prior-attempt artifacts must remain byte-for-byte
unchanged during `attempt-2` implementation and review:

- `orchestrator/rounds/round-117/selection.md`
  - sha256:
    `0a472d438c594c1402acdb67b32e2a6e45dbf5a7b3d6afa9207ba77fffe084d2`
- `orchestrator/rounds/round-117/reviews/attempt-1.md`
  - sha256:
    `27afc5d1d3da62c0ba1a62d43766761814de3be73110f6e28d7fbb5e0269db1d`
- `orchestrator/rounds/round-117/attempt-log.jsonl`
  - sha256:
    `4a86d00c9a0f50d45c17e146a55b9c37d980270bbbd54f0869fdee25ad6bf4c5`
- `orchestrator/rounds/round-117/implementation-notes.md`
  - sha256:
    `b81c2d863e3be16bce940b5053d381e1be06e9f228bdffdae98efc2089b8b447`
- `docs/plans/2026-03-27-post-rev-004-repo-scope-successor-authority-evidence-inputs-and-non-widening-boundary-freeze.md`
  - sha256:
    `7369507bb08b3adaa3dd78e107e47c638991101352354c2fe7f146d8876adb0b`

If any of those hashes changes during `attempt-2`, the retry has widened
past the recorded fix hypothesis and must fail closed rather than repairing
those surfaces in place.

## Read-Only Recovery And Control-Plane Inputs

The following inputs must remain read-only for this repair pass:

- `orchestrator/state.json`
  - controller-owned machine state; do not edit it.
- `orchestrator/rounds/round-117/controller-recovery-note.md`
  - read-only recovery input for this repair;
  - current sha256:
    `e787444c68eceed869b84d94108bf9124877e8c04d9c5c506a229e5c2fd3f1a0`
- the active roadmap bundle under
  `orchestrator/roadmaps/2026-03-27-00-repo-scope-refreshed-matrix-and-narrowed-blocker-successor-roadmap/rev-001/`
- the predecessor docs named in the planner contract
- `Bugs.md`

## File Map

### Planning-Stage Rewrite

- `orchestrator/rounds/round-117/plan.md`
  - responsibility: repair the attempt-2 artifact-ownership contract without
    reopening the item-1 freeze substance.

### Authorized Attempt-2 Reuse Without Editing

- `docs/plans/2026-03-27-post-rev-004-repo-scope-successor-authority-evidence-inputs-and-non-widening-boundary-freeze.md`
- `orchestrator/rounds/round-117/implementation-notes.md`

### Reviewer-Owned Current-Attempt Surface

- `orchestrator/rounds/round-117/review.md`
  - writable only by the current reviewer when the round returns to review.

### Read-Only Evidence

- `orchestrator/state.json`
- `orchestrator/rounds/round-117/selection.md`
- `orchestrator/rounds/round-117/reviews/attempt-1.md`
- `orchestrator/rounds/round-117/attempt-log.jsonl`
- `orchestrator/rounds/round-117/controller-recovery-note.md`
- `orchestrator/roadmaps/2026-03-27-00-repo-scope-refreshed-matrix-and-narrowed-blocker-successor-roadmap/rev-001/roadmap.md`
- `orchestrator/roadmaps/2026-03-27-00-repo-scope-refreshed-matrix-and-narrowed-blocker-successor-roadmap/rev-001/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-27-00-repo-scope-refreshed-matrix-and-narrowed-blocker-successor-roadmap/rev-001/verification.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-settlement-same-family-handoff-decision.md`
- `Bugs.md`

## Exact Retry Delta (Exactly One)

The only live retry delta in this recovery repair is:

remove `orchestrator/rounds/round-117/review.md` from the immutable
prior-attempt freeze and restore it to its lawful role as the current
reviewer-owned writable review surface for `attempt-2`.

This repair is allowed to:

- rewrite `plan.md` so `review.md` is no longer frozen as attempt-1 evidence;
- keep `reviews/attempt-1.md` and the other true prior-attempt artifacts
  frozen byte-for-byte;
- keep the canonical freeze artifact authoritative;
- keep `implementation-notes.md` as the authorized non-authoritative
  companion surface; and
- rerun the same docs-only baseline and item-1 successor-boundary checks,
  plus an ownership check proving that `review.md` remains writable for the
  current reviewer.

This repair is not allowed to:

- rewrite the canonical freeze artifact;
- rewrite `implementation-notes.md`;
- rewrite `selection.md`, `reviews/attempt-1.md`, `attempt-log.jsonl`,
  `controller-recovery-note.md`, `orchestrator/state.json`, or `Bugs.md`;
- freeze `review.md` as if it were an immutable attempt-1 artifact;
- create a side ledger, alternate memo, replacement artifact, or second
  decision surface; or
- widen into refreshed-matrix publication, narrowed successor-gate work, or
  any production/code path.

## Sequential Retry Tasks

### Task 1 - Freeze only the lawful prior-attempt evidence

- Treat the five hashed files above as immutable prior-attempt inputs.
- Preserve `controller-recovery-note.md` and `orchestrator/state.json` as
  read-only control-plane inputs.
- Do not add `review.md` to the immutable prior-attempt set.

### Task 2 - Preserve the carried-forward item-1 output contract

- Keep the canonical freeze artifact as the only authoritative item-1 output.
- Keep `implementation-notes.md` as the authorized non-authoritative
  companion surface only.
- Add no new authoritative, companion, or sidecar surface.

### Task 3 - Reserve `review.md` for the current reviewer

- State explicitly that `review.md` remains writable only for the `attempt-2`
  reviewer.
- State explicitly that the immutable attempt-1 review record lives only at
  `reviews/attempt-1.md`.
- Keep planner and implementer work off `review.md`.

### Task 4 - Rerun the docs-only checks plus the ownership repair check

- Rerun the baseline control-plane checks from `roadmap_dir/verification.md`.
- Rerun the same item-1 successor-boundary checks that already passed before
  the controller recovery interruption.
- Add one retry-specific check proving that `plan.md` leaves `review.md`
  writable for the current reviewer while `reviews/attempt-1.md` and the
  other true prior-attempt artifacts remain frozen.
