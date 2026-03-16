# Round `round-024` Merge Notes (`R1`)

## Squash Commit Title

`test: capture URI-R2-C1 InstBot replay reproducer`

## Squash Summary

- Adds the bounded `URI-R2-C1` / `uri-r2-c1-only-v1` implementation-facing replay reproducer in `test/ElaborationSpec.hs`, rebuilding the live generalization, no-fallback reification, and witness replay inputs before asserting the inherited `applyInstantiation` failure `InstBot expects ⊥, got: t9 -> t9`.
- Records the `R1` repair-boundary reproduction artifact in `docs/plans/2026-03-17-uri-r2-c1-r1-repair-boundary-reproduction.md`, explicitly preserving the locked subject, scenario, repair boundary, and owner boundary.
- Keeps the round reproduction-only: no production diff under `src/`, no second executable interface, no fallback behavior, and no widening beyond the approved `R1` contract.

## Readiness

- Latest review snapshot is `accepted` + `finalize` at attempt `1`, and `review.md` agrees with `review-record.json` on `stage_result: pass`, `retry_reason: none`, and `fix_hypothesis: none`.
- `orchestrator/retry-subloop.md` is satisfied: `R1` may finalize, `review-record.json` is authoritative on finalization, and no retry remains active for this round.
- The approved diff stays bounded to the `R1` reproduction slice: tracked code changes are limited to `test/ElaborationSpec.hs`, with supporting plan/doc/round artifacts only.
- Predecessor continuity is intact: this round consumes the inherited `P1`, `P2`, `D2`, `D3`, and `D4 = reopen-repair-track` authority without rewriting earlier rounds.

Round `round-024` is squash-ready for the bounded `R1` contract.

## Follow-Up Notes

- Successor work, if selected later, is `R2` only: one bounded repair attempt at `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).
- Preserve the predecessor chain from `round-016`, `round-017`, and `round-021` through `round-023` as authoritative evidence during any later repair or verification round.
