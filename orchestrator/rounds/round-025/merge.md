# Round `round-025` Merge Notes (`R2`)

## Squash Commit Title

`elab: repair bounded URI-R2-C1 InstBot witness replay`

## Squash Summary

- Threads replay-bound substitutions through `evalInstantiationWith` and reuses them in `applyInstantiation` so the `InstBot` branch accepts a non-bottom bound only when the same replay path resolves to the explicit bound up to alpha-equivalence.
- Keeps `MLF.Elab.TypeCheck` plumbing-only, adds locked-lane regression coverage for the repaired replay success plus strict non-replay `InstBot` failures, and does not widen into a second interface, compatibility fallback, or broader replay search.
- Contract-scopes the inherited prototype reruns in `test/Research/UriR2C1PrototypeP1Spec.hs` so live post-repair continuity drift is reported without rewriting the historical `P1`/`P2`/`D1`/`D2`/`D3`/`D4`/`R1` authority chain.

## Readiness

- Latest review snapshot is `accepted` + `finalize` at attempt `3`, and `review.md` agrees with `review-record.json` on `stage_result: pass`, `attempt_verdict: accepted`, `stage_action: finalize`, `retry_reason: none`, and `fix_hypothesis: none`.
- The authoritative retry summary in `review-record.json` matches the retry-subloop contract for finalized `R2`: `attempts_run: 3`, `max_attempts: 100`, `latest_accepted_attempt: 3`, `finalization_mode: accepted-final`, and `latest_retry_reason: none`.
- The approved diff stays inside the bounded `R2` contract: only `src/MLF/Elab/Inst.hs`, `src/MLF/Elab/TypeCheck.hs`, `test/ElaborationSpec.hs`, and `test/Research/UriR2C1PrototypeP1Spec.hs` change, with the full gate already recorded green in the accepted review (`cabal build all && cabal test`).
- Predecessor continuity remains intact: historical rounds `round-016`, `round-017`, `round-020`, `round-021`, `round-022`, `round-023`, and `round-024` stay read-only authoritative evidence, while attempt `3` only narrows how live reruns are interpreted against that inherited record.

Round `round-025` is squash-ready for the bounded `R2` contract.

## Follow-Up Notes

- Carry forward the accepted retry lineage from attempts `1` through `3` and the inherited predecessor records unchanged when later stages consume `R2`.
- Preserve the bounded owner boundary at `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch); any later work should treat this round as the authoritative localized repair rather than reopen broader replay semantics.
