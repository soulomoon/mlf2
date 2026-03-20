# Round `round-059` Merge Preparation (`J2`)

## Proposed Squash Commit Title

`elab(j2): harden local inst-arg singleton-base fallback lane`

## Summary

- The accepted `J2` round completes the exact `J1`-frozen local-binding
  inst-arg-only singleton-base `baseTarget -> baseC` / same-lane `targetC`
  implementation slice for repaired `URI-R2-C1` inside
  `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`,
  while preserving the inherited explicit-only / non-equi-recursive /
  non-cyclic-graph / no-second-interface / no-fallback boundary.
- `Fallback.hs` keeps the selected `baseTarget` branch unchanged, adds the
  reviewer-auditable `rootLocalInstArgSingleBase` proof from the five
  `J1`-frozen ingredients, and routes `targetC` to `baseC` only for that
  bounded local singleton-base lane. The completed `rootLocalSingleBase`
  arm, preserved scheme-alias/base-like route, `rootLocalMultiInst`,
  `rootLocalInstArgMultiBase`, `keepTargetFinal`, and the inherited non-local
  fail-closed paths remain unchanged.
- `test/PipelineSpec.hs` extends only the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block with
  one focused helper, one local same-lane success example, one matched
  non-local fail-closed contrast, and a refreshed source guard that requires
  `rootLocalInstArgSingleBase` plus the dedicated `targetC` arm.
- The canonical `J2` artifact
  `docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md` and
  `orchestrator/rounds/round-059/implementation-notes.md` record the same
  bounded scope plus fresh focused verification (`19` examples, `0` failures)
  and a fresh full repo gate (`1140` examples, `0` failures).
- Accepted review evidence keeps the merge payload bounded to the selected
  code/test slice plus round-local documentation. No out-of-scope production,
  interface, roadmap, controller, or bug-tracker drift is part of this merge
  note.

## Review And Retry Consistency Check

- `orchestrator/rounds/round-059/review.md` records `Implemented stage result:
  pass`, `Attempt verdict: accepted`, `Stage action: finalize`, `Retry reason:
  none`, and `Fix hypothesis: none`.
- The latest review snapshot is
  `orchestrator/rounds/round-059/reviews/attempt-1.md`, and it matches that
  finalized `accepted + finalize` review state.
- `orchestrator/rounds/round-059/review-record.json` is authoritative and
  matches that finalized review state: `stage_id: J2`, `attempt: 1`,
  `attempt_verdict: accepted`, `stage_result: pass`, `stage_action: finalize`,
  `retry_reason: none`, `fix_hypothesis: none`, `status: authoritative`,
  `authoritative_attempt: 1`, `authoritative_result: pass`,
  `review_snapshot: orchestrator/rounds/round-059/reviews/attempt-1.md`, and
  `artifact_path: docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`.
- This round stayed at `retry: null` under the live `contract_version: 2`
  retry-subloop contract, so the retry/finalization state is satisfied
  directly by the authoritative `accepted + finalize` review snapshot and the
  matching `review-record.json`. No retry-subloop reconciliation is needed
  beyond that exact match.

## Readiness Statement

Round `round-059` is ready for squash merge. The latest review snapshot is
lawful `accepted + finalize`, the authoritative review record matches it, all
recorded `J2-*` checks pass, and the accepted round satisfies the `J2` stage
gate as the bounded `J1`-selected implementation slice for repaired
`URI-R2-C1` without reopening any excluded family or widening the inherited
boundary.

## Predecessor Continuity

- This round preserves continuity from completed rounds `round-001` through
  `round-058`, the recursive-types packet, the replay-repair track, the
  inherited automatic-recursive boundary documents, the accepted initial
  successor cycle through `round-033`, the bounded follow-on chains through
  `H4`, the accepted `I1` / `I2` / `I3` / `I4` chain, and the accepted `J1`
  bind in `round-058`.
- The live subject remains repaired `URI-R2-C1`; accepted
  `I4 = continue-bounded` and accepted `J1` remain the controlling predecessor
  outcomes; and `J2` lands only the bounded local inst-arg-only
  singleton-base lane while carrying the already accepted local single-base,
  scheme-alias/base-like, local multi-inst, local inst-arg multi-base, and
  retained-target families forward as inherited continuity only.
- Nothing in this merge note authorizes replay reopen,
  `MLF.Elab.Inst` / `InstBot` work, reinterpretation of accepted
  `U2` / `U3` / `U4` negatives as widening clearance, reopening
  `boundVarTarget`, `boundTarget`, `schemeBodyTarget`, `ResultType.View`,
  non-local widening, cross-family widening, equi-recursive reasoning, cyclic
  structural graph encoding, second interfaces, or fallback/convenience/
  default-path widening.

## Follow-Up Notes

- Controller/guider-owned post-merge work should treat the accepted
  `review-record.json` plus the canonical `J2` artifact as the authoritative
  implementation result and advance only into the pending bounded verification
  gate.
- Any successor verification work must preserve the exact `J2` lane and keep
  the inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary unchanged while rechecking the
  accepted focused evidence and full repo gate.
- Preserve reviewer-owned artifacts exactly as written: `review.md`,
  `reviews/attempt-1.md`, and `review-record.json` are the authoritative
  acceptance trail for this finalized round.
- No merge was executed in this stage; this note records merger readiness
  only.
