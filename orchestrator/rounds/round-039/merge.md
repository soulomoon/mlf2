# Round `round-039` Merge Preparation (`E2`)

## Proposed Squash Commit Title

`elab: harden retained-child fallback to same-lane local TypeRef roots`

## Summary

- The accepted `E2` round completes the exact `E1`-frozen retained-child
  `boundVarTarget` / nested-`forall` hardening slice for repaired `URI-R2-C1`
  inside `src/MLF/Elab/Run/ResultType/Fallback.hs` without widening beyond the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.
- `Fallback.hs` keeps `rootBindingIsLocalType` as the mandatory gate and limits
  retained-child survival to the same canonical local `TypeRef` lane via
  `sameLocalTypeLane` / `scopeRootPost`, while `boundHasForallFrom` continues to
  reject nested-`forall` / nested-owner crossings and the other
  `keepTargetFinal` trigger families remain fail-closed.
- `test/PipelineSpec.hs` now carries the required bounded behavioral evidence in
  the existing `ARI-C1 feasibility characterization (bounded prototype-only)`
  block: one new same-lane retained-child success example plus the matched
  nested-`forall` fail-closed contrast, and the canonical `E2` artifact /
  implementation notes record both.
- The accepted review records fresh focused verification
  (`9 examples, 0 failures`) and a fresh full gate
  (`cabal build all && cabal test`, `1130 examples, 0 failures`) while keeping
  replay repair, `MLF.Elab.Inst`, `InstBot`, alternate interfaces, and broader
  widening lanes out of scope.

## Review And Retry Consistency Check

- `orchestrator/rounds/round-039/review.md` matches
  `orchestrator/rounds/round-039/reviews/attempt-2.md`, and the latest review
  snapshot records `Implemented stage result: pass`, `Attempt verdict:
  accepted`, `Stage action: finalize`, `Retry reason: none`, and `Fix
  hypothesis: none`.
- `orchestrator/rounds/round-039/review-record.json` is authoritative and
  matches that finalized `attempt-2` snapshot: `attempt: 2`,
  `attempt_verdict: accepted`, `stage_result: pass`, `stage_action: finalize`,
  `status: authoritative`, `authoritative_attempt: 2`,
  `authoritative_result: pass`,
  `review_snapshot: orchestrator/rounds/round-039/reviews/attempt-2.md`, and
  `artifact_path: docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`.
- The authoritative retry summary in `review-record.json` matches the
  retry-subloop contract for a finalized retry round:
  `attempts_run: 2`, `max_attempts: 100`, `latest_accepted_attempt: 2`,
  `finalization_mode: accepted-final`, and `latest_retry_reason: none`.
- `orchestrator/rounds/round-039/attempt-log.jsonl` preserves the rejected
  `attempt-1` retry entry with
  `review_snapshot: orchestrator/rounds/round-039/reviews/attempt-1.md` and the
  same canonical `E2` artifact path. No later retry entry was added, which is
  consistent with the retry-subloop transition rules because `attempt-2`
  finalized as `accepted + finalize` and is therefore captured by the
  authoritative `review-record.json` instead of another retry log append.

## Readiness Statement

Round `round-039` is ready for squash merge preparation. The latest review
snapshot is lawful `accepted + finalize`, the authoritative review record
matches it, the retry lineage is internally consistent, and the accepted round
satisfies the `E2` stage gate as the bounded retained-child implementation slice
frozen by `E1`.

## Predecessor Continuity

- This round preserves continuity from completed rounds `round-001` through
  `round-033`, the replay-repair track, the recursive-types packet, and the
  accepted first follow-on cycle in `round-034` (`C1`), `round-035` (`C2`),
  `round-036` (`C3`), `round-037` (`C4`), plus the accepted `E1` bind in
  `round-038`.
- The live subject remains repaired `URI-R2-C1`; accepted `C4 = continue-bounded`
  and accepted `E1` remain the controlling predecessor outcomes for this second
  bounded cycle; and the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph boundary stays unchanged.
- Nothing in this merge note authorizes replay reopen, `MLF.Elab.Inst` /
  `InstBot` work, reinterpretation of accepted `U2` / `U3` / `U4` negatives as
  widening clearance, equi-recursive reasoning, cyclic structural graph
  encoding, cross-family widening, second interfaces, or
  compatibility/default-path widening.

## Follow-Up Notes

- Controller/guider-owned post-merge work should treat the accepted
  `review-record.json` plus the canonical `E2` artifact as the authoritative
  retained-child implementation result and advance only into the pending `E3`
  bounded verification gate.
- Preserve reviewer-owned artifacts exactly as written: `review.md`,
  `reviews/attempt-1.md`, `reviews/attempt-2.md`, and `review-record.json` are
  the authoritative acceptance trail for this retry round.
- No merge was executed in this stage; this note records merger readiness only.
