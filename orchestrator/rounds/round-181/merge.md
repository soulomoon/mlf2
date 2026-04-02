# Merge Preparation (`round-181` / `item-5`)

## Roadmap Identity

- `roadmap_id`: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`
- `roadmap_revision`: `rev-001`
- `roadmap_dir`: `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`
- `roadmap_item_id`: `item-5`

## Squash Commit Title

`Remove packet-local C1 shortcut from authoritative pipeline`

## Squash Summary

- Merge the approved first bounded `item-5` positive-family slice for the
  active general automatic iso-recursive full-inference roadmap.
- The canonical payload is the packet-local shortcut removal in
  `src/MLF/Elab/Run/Pipeline.hs`, the focused regression guard in
  `test/PipelineSpec.hs`, and the round-local bounded-result record in
  `orchestrator/rounds/round-181/implementation-notes.md`.
- The approved scope is one exact `P2` `C1` packet only: the admitted
  non-local scheme-alias / base-like representative anchored by
  `rootNonLocalSchemeAliasBaseLike` with the fallback lane still read through
  `baseTarget -> baseC`.
- The accepted diff proves the recursive authoritative result on
  `runPipelineElab` and `runPipelineElabChecked` does not require the deleted
  packet-local `preserveC1AuthoritativeRecursiveAlias` /
  `isBlockedC1AliasScheme` rescue in `Run/Pipeline`; the fallback surface
  remains honestly non-recursive, and no new route family, candidate search,
  same-lane logic, fallback widening, or public-interface change is added.
- This squash payload remains bounded to one representative `P2` packet and
  its review artifacts. It does not claim aggregate `P2` closure, broader
  positive-family readiness, item-5 completion, item-6 negative-family
  evidence, item-7 repo-level readiness, or any architecture-boundary
  revision.
- No controller-state or roadmap edit belongs to this squash payload. The
  tracked `orchestrator/state.json` transitions are controller-owned
  bookkeeping only and remain out of scope for merge substance.

## Review Confirmation

- `orchestrator/rounds/round-181/review.md` is finalized as
  `accepted + finalize`.
- `orchestrator/rounds/round-181/review-record.json` matches the finalized
  review and records:
  - `attempt: 1`
  - `attempt_verdict: accepted`
  - `stage_result: pass`
  - `stage_action: finalize`
  - `retry_reason: none`
  - `fix_hypothesis: none`
  - `decision: approved`

## Merge Readiness

- Merge readiness: confirmed. The approved review covers exactly one bounded
  `item-5` `C1` packet slice, no same-round retry remains open, and
  `orchestrator/state.json` places `round-181` at `stage: merge` for
  `item-5`.
- Base branch freshness: confirmed. `HEAD`,
  `orchestrator/round-181-bounded-positive-family-slice`, and
  `codex/automatic-recursive-type-inference` all resolve to
  `664bef7a1172392727bf4411e9518686677891b5` (`664bef7` short), and
  `git merge-base HEAD codex/automatic-recursive-type-inference` returns the
  same commit, so no newer committed base-branch divergence is present.
- The round remains ready for squash merge without reopening selection,
  planning, implementation, review, or roadmap state.

## Follow-Up Notes

- Post-merge controller work should treat the `Run/Pipeline` shortcut removal,
  the focused `PipelineSpec` guard, `implementation-notes.md`, `review.md`,
  and `review-record.json` as the authoritative completed outcome for this
  first bounded `item-5` slice.
- `item-5` remains pending after this merge. The roadmap update should record
  that the exact `C1` representative packet now survives without a
  packet-local rescue while the fallback lane stays non-recursive, then keep
  later positive-family slices and the final aggregate artifact explicitly
  outstanding.
