# Round `round-047` Merge Preparation (`G2`)

## Proposed Squash Commit Title

`elab(g2): harden local multi-inst fallback lane`

## Summary

- The accepted `G2` round completes the exact `G1`-frozen local-binding
  `rootHasMultiInst` `keepTargetFinal` / `targetC` hardening slice for
  repaired `URI-R2-C1` inside
  `src/MLF/Elab/Run/ResultType/Fallback.hs` without widening beyond the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.
- `Fallback.hs` introduces the reviewer-auditable `rootLocalMultiInst` proof,
  threads it through the selected `keepTargetFinal` lane, and makes `targetC`
  choose `rootFinal` only for that one bounded local multi-inst case while
  leaving `instArgRootMultiBase`, `boundVarTarget`, replay reopen,
  `MLF.Elab.Inst`, `InstBot`, and non-local widening out of scope.
- `test/PipelineSpec.hs` adds the bounded behavioral evidence required by `G2`
  in the existing `ARI-C1 feasibility characterization (bounded prototype-only)`
  block: one local multi-inst success example plus one matched fail-closed
  contrast.
- The canonical `G2` artifact
  `docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md` and
  `orchestrator/rounds/round-047/implementation-notes.md` record the same
  bounded scope plus fresh focused verification (`13 examples, 0 failures`) and
  a fresh full repo gate (`1134 examples, 0 failures`).

## Review And Retry Consistency Check

- `orchestrator/rounds/round-047/review.md` records `Implemented stage result:
  pass`, `Attempt verdict: accepted`, `Stage action: finalize`, `Retry reason:
  none`, and `Fix hypothesis: none` for the latest review snapshot.
- `orchestrator/rounds/round-047/review-record.json` is authoritative and
  matches that finalized review state: `attempt: 1`,
  `attempt_verdict: accepted`, `stage_result: pass`, `stage_action: finalize`,
  `status: authoritative`, `authoritative_attempt: 1`,
  `authoritative_result: pass`,
  `review_snapshot: orchestrator/rounds/round-047/reviews/attempt-1.md`, and
  `artifact_path: docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md`.
- This round did not use a retry loop beyond the idle `retry: null` state, so
  the repo-local retry contract is satisfied directly by the authoritative
  `accepted + finalize` review snapshot and matching `review-record.json`.

## Readiness Statement

Round `round-047` is ready for squash merge preparation. The latest review
snapshot is lawful `accepted + finalize`, the authoritative review record
matches it, all recorded `G2` checks pass, and the accepted round satisfies the
`G2` stage gate as the bounded local-binding `rootHasMultiInst`
implementation slice frozen by `G1`.

## Predecessor Continuity

- This round preserves continuity from completed rounds `round-001` through
  `round-046`, the recursive-types packet, the replay-repair track, the
  accepted first follow-on cycle in `round-034` through `round-037`, the
  accepted `E1` / `E2` / `E3` / `E4` chain in `round-038` through `round-041`,
  and the accepted `F1` / `F2` / `F3` / `F4` / `G1` chain in `round-042`
  through `round-046`.
- The live subject remains repaired `URI-R2-C1`; accepted
  `F4 = continue-bounded` and accepted `G1` remain the controlling predecessor
  outcomes; and the accepted local-binding scheme-alias/base-like `F2` / `F3`
  baseline remains inherited context only while `G2` lands the next bounded
  local multi-inst slice.
- Nothing in this merge note authorizes replay reopen, `MLF.Elab.Inst` /
  `InstBot` work, reinterpretation of accepted `U2` / `U3` / `U4` negatives as
  widening clearance, equi-recursive reasoning, cyclic structural graph
  encoding, multi-SCC or cross-family widening, second interfaces, or
  compatibility/default-path widening.

## Follow-Up Notes

- Controller/guider-owned post-merge work should treat the accepted
  `review-record.json` plus the canonical `G2` artifact as the authoritative
  implementation result and advance only into the pending `G3` bounded
  verification gate.
- `instArgRootMultiBase` remains the only unopened `keepTargetFinal` trigger
  family after this merge and must stay out of scope unless a later bounded
  round selects it explicitly.
- Preserve reviewer-owned artifacts exactly as written: `review.md`,
  `reviews/attempt-1.md`, and `review-record.json` are the authoritative
  acceptance trail for this finalized round.
- No merge was executed in this stage; this note records merger readiness only.
