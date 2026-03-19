# Round `round-051` Merge Preparation (`H2`)

## Proposed Squash Commit Title

`elab(h2): harden local inst-arg multi-base lane`

## Summary

- The accepted `H2` round completes the exact `H1`-frozen local-binding
  `instArgRootMultiBase` `keepTargetFinal` / `targetC` hardening slice for
  repaired `URI-R2-C1` inside
  `src/MLF/Elab/Run/ResultType/Fallback.hs` without widening beyond the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.
- `Fallback.hs` introduces the reviewer-auditable
  `rootLocalInstArgMultiBase` proof, threads `keepTargetFinal` through that
  explicit local gate, and lets the selected `targetC` branch retain
  `rootFinal` only for that bounded local multi-base lane while leaving the
  upstream `instArgRootMultiBase` aggregation and fail-closed `baseTarget`
  guards intact.
- `test/PipelineSpec.hs` adds the bounded behavioral evidence required by `H2`
  in the existing `ARI-C1 feasibility characterization (bounded prototype-only)`
  block: one local inst-arg multi-base success example, one matched non-local
  fail-closed contrast, and a refreshed source guard naming the selected
  authority.
- The canonical `H2` artifact
  `docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md` and
  `orchestrator/rounds/round-051/implementation-notes.md` record the same
  bounded scope plus fresh focused verification (`15 examples, 0 failures`) and
  a fresh full repo gate (`1136 examples, 0 failures`).

## Review And Retry Consistency Check

- `orchestrator/rounds/round-051/review.md` records `Implemented stage result:
  pass`, `Attempt verdict: accepted`, `Stage action: finalize`, `Retry reason:
  none`, and `Fix hypothesis: none` for the latest review snapshot.
- The latest review snapshot is
  `orchestrator/rounds/round-051/reviews/attempt-1.md`, and it matches the
  finalized review state.
- `orchestrator/rounds/round-051/review-record.json` is authoritative and
  matches that finalized review state: `attempt: 1`,
  `attempt_verdict: accepted`, `stage_result: pass`, `stage_action: finalize`,
  `status: authoritative`, `authoritative_attempt: 1`,
  `authoritative_result: pass`,
  `review_snapshot: orchestrator/rounds/round-051/reviews/attempt-1.md`, and
  `artifact_path: docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`.
- This round stayed at `retry: null`, so the repo-local retry contract is
  satisfied directly by the authoritative `accepted + finalize` review
  snapshot and matching `review-record.json`.

## Readiness Statement

Round `round-051` is ready for squash merge. The latest review snapshot is
lawful `accepted + finalize`, the authoritative review record matches it, all
recorded `H2-*` checks pass, and the accepted round satisfies the `H2` stage
gate as the bounded local-binding `instArgRootMultiBase` implementation slice
frozen by `H1`.

## Predecessor Continuity

- This round preserves continuity from completed rounds `round-001` through
  `round-050`, the recursive-types packet, the replay-repair track, the
  accepted first follow-on cycle in `round-034` through `round-037`, the
  accepted `E1` / `E2` / `E3` / `E4` chain in `round-038` through `round-041`,
  the accepted `F1` / `F2` / `F3` / `F4` chain in `round-042` through
  `round-045`, the accepted `G1` / `G2` / `G3` / `G4` chain in `round-046`
  through `round-049`, and the accepted `H1` bind in `round-050`.
- The live subject remains repaired `URI-R2-C1`; accepted
  `G4 = continue-bounded` and accepted `H1` remain the controlling predecessor
  outcomes; and the accepted retained-child, scheme-alias/base-like, and local
  multi-inst lanes remain inherited context only while `H2` lands the
  remaining bounded local `instArgRootMultiBase` slice.
- Nothing in this merge note authorizes replay reopen, `MLF.Elab.Inst` /
  `InstBot` work, reinterpretation of accepted `U2` / `U3` / `U4` negatives as
  widening clearance, `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, or
  `boundVarTarget` reopening, non-local widening, equi-recursive reasoning,
  cyclic structural graph encoding, second interfaces, or
  compatibility/default-path widening.

## Follow-Up Notes

- Controller/guider-owned post-merge work should treat the accepted
  `review-record.json` plus the canonical `H2` artifact as the authoritative
  implementation result and advance only into the pending `H3` bounded
  verification gate.
- Future `H3` verification work must preserve the exact `H2` lane and keep the
  inherited ownership and boundary constraints unchanged while rechecking the
  accepted focused evidence and full repo gate.
- Preserve reviewer-owned artifacts exactly as written: `review.md`,
  `reviews/attempt-1.md`, and `review-record.json` are the authoritative
  acceptance trail for this finalized round.
- No merge was executed in this stage; this note records merger readiness only.
