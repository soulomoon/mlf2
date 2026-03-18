# Round `round-035` Merge Preparation (`C2`)

## Proposed Squash Commit Title

`C2: harden local-only fallback retention and fail-closed coverage`

## Summary

- The approved `C2` slice keeps production changes bounded to `src/MLF/Elab/Run/ResultType/Fallback.hs`, where retained-target fallback selection now stays behind the audited `rootBindingIsLocalType` gate so non-local wrapper/proxy roots fall back fail-closed instead of preserving recursive-looking targets.
- `test/PipelineSpec.hs` now records the exact same non-local proxy-wrapper case `let g = (\x : mu a. a -> Int. x) in g g` across all three required lanes: direct `computeResultTypeFallback`, unchecked `runPipelineElab`, and checked `runPipelineElabChecked`.
- The canonical `C2` artifact and round notes were refreshed for `attempt-2` to record that repaired same-case evidence without reopening `MLF.Elab.Inst`, replay repair, roadmap/state surfaces, or any widening lane beyond repaired `URI-R2-C1`.

## Review And Retry Consistency Check

- `review.md` matches `orchestrator/rounds/round-035/reviews/attempt-2.md` exactly, and the latest review snapshot records `Implemented stage result: pass`, `Attempt verdict: accepted`, `Stage action: finalize`, `Retry reason: none`, and `Fix hypothesis: none`.
- `orchestrator/rounds/round-035/review-record.json` is authoritative and matches that same finalized `attempt-2` snapshot: `attempt: 2`, `attempt_verdict: accepted`, `stage_action: finalize`, `authoritative_attempt: 2`, `authoritative_result: pass`, `review_snapshot: orchestrator/rounds/round-035/reviews/attempt-2.md`, and retry summary `attempts_run: 2` with `finalization_mode: accepted-final`.
- `orchestrator/rounds/round-035/attempt-log.jsonl` preserves exactly one retry entry for rejected `attempt-1` pointing at `orchestrator/rounds/round-035/reviews/attempt-1.md`. That history is consistent with finalization: the only logged retry is the rejected first attempt, while `attempt-2` finalized and therefore does not introduce a conflicting retry entry.

## Readiness Statement

Round `round-035` is ready for squash merge preparation. The latest review snapshot is lawful `accepted + finalize`, the authoritative review record matches it, the retry history is internally consistent, and the accepted round stays inside the bounded `C2` contract with passing focused and full-repo verification evidence.

## Predecessor Continuity

- This round preserves the inherited evidence chain from the recursive-types packet, replay-repair rounds `round-024` through `round-027`, initial successor rounds `round-028` through `round-033`, and accepted `C1` in `round-034`.
- The live subject remains repaired `URI-R2-C1` only under the accepted explicit-only / non-equi-recursive / non-cyclic-graph boundary; nothing in this merge note widens the subject, reinterprets accepted `U2`/`U3`/`U4` negatives as clearance, or introduces a second interface or fallback path.

## Follow-Up Notes

- Controller/guider-owned post-merge work should consume this finalized `C2` result without rewriting preserved retry artifacts (`attempt-log.jsonl`, `reviews/attempt-1.md`, `reviews/attempt-2.md`, `review-record.json`).
- No merger-side implementation follow-up is required inside this round beyond preserving the bounded `C2` evidence chain for the next stage.
