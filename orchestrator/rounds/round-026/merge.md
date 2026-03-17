# Round `round-026` Merge Notes (`R3`)

## Squash Commit Title

`docs: record locked URI-R2-C1 replay verification`

## Squash Summary

- Records the bounded `R3` verification artifact for `URI-R2-C1` / `uri-r2-c1-only-v1`, confirming the accepted `R2` owner-lane repair still succeeds on the locked replay path and that the old `InstBot expects ⊥, got: t9 -> t9` mismatch no longer appears on that accepted lane.
- Confirms the strict non-replay `InstBot` rejection checks and inherited continuity reruns remain green, so `R3` introduces no fallback path, no second executable interface, and no widening beyond `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).
- Keeps the round docs-and-evidence only: the work is limited to the stage artifact and round notes, with no new production or test diff, and leaves the final `repair-accepted` / `repair-blocked` decision to `R4`.

## Readiness

- Latest review snapshot is `accepted` + `finalize` at attempt `1`, and `review.md` agrees with `review-record.json` on `stage_result: pass`, `attempt_verdict: accepted`, `stage_action: finalize`, `retry_reason: none`, and `fix_hypothesis: none`.
- The authoritative retry summary is consistent with the final review record for this bounded `R3` stage: attempt `1` finalized immediately, `authoritative_attempt: 1`, `authoritative_result: pass`, `status: authoritative`, and no active retry remains.
- The approved round stays inside the bounded `R3` contract: it verifies the locked replay path and bounded non-replay failures against the accepted `R2` repair without reopening production work, broadening scenario scope, or introducing compatibility behavior.
- Predecessor continuity remains intact: this round carries forward the immutable `P1`, `P2`, `D1`, `D2`, `D3`, `D4`, `R1`, and `R2` authority chain strictly as inherited evidence.

Round `round-026` is squash-ready for the bounded `R3` contract.

## Follow-Up Notes

- `R4` is the only remaining stage allowed to aggregate `R1` through `R3` and decide `repair-accepted` versus `repair-blocked`.
- Preserve the inherited predecessor records and the accepted `R2` repair evidence unchanged when `R4` consumes this verification result.
