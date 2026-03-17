# Round `round-027` Merge Notes (`R4`)

## Squash Commit Title

`docs: finalize URI-R2-C1 repair decision gate`

## Squash Summary

- Records the aggregate-only `R4` decision artifact for `URI-R2-C1` / `uri-r2-c1-only-v1`, carrying forward the authoritative `R1` through `R3` pass chain and closing the bounded repair track with final outcome `repair-accepted`.
- Confirms the terminal gate stays inside the locked repair boundary `witness-replay/applyInstantiation-instbot-precondition` and owner lane `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch), with no reopened implementation work, no second executable interface, and no fallback behavior.
- Keeps the round docs-only: the approved diff is limited to the `R4` stage artifact and round notes, with controller-owned files and prior-round authority left untouched.

## Readiness

- Latest review snapshot is `accepted` + `finalize` at attempt `1`, and `review.md` agrees with `review-record.json` on `stage_result: pass`, `attempt_verdict: accepted`, `stage_action: finalize`, `retry_reason: none`, `fix_hypothesis: none`, and final outcome `repair-accepted`.
- The authoritative terminal summary matches `review-record.json` for bounded `R4`: `stage_id: R4`, `authoritative_attempt: 1`, `authoritative_result: pass`, `status: authoritative`, `terminal_reason: none`, and `final_outcome: repair-accepted`.
- The approved round is ready for squash merge under the bounded `R4` terminal contract: it remains aggregate-only, consumes only accepted predecessor authority, and introduces no production/test diff, no scope widening, and no forbidden `accepted + retry` path.
- Predecessor continuity remains intact: this round carries forward the immutable `P1`, `P2`, `D1`, `D2`, `D3`, `D4`, `R1`, `R2`, and `R3` authority chain, including the historical repair-track evidence from rounds `round-016` through `round-023` and the accepted stage records from rounds `round-024` through `round-026`.

Round `round-027` is squash-ready for the bounded `R4` terminal contract.

## Follow-Up Notes

- This note prepares the approved round for squash merge only; it does not merge the branch.
- Preserve the inherited predecessor and retry-subloop evidence unchanged after merge; `R4` is terminal and may not reopen the repair track through an `accepted + retry` path.
