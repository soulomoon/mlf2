# Round-021 Plan (`D2` Replay Mismatch Localization)

## Scope Lock

- contract version: `2`
- roadmap item: `2` (`D2` replay mismatch localization)
- active stage: `D2`
- active attempt: `1`
- retry status at plan time: `null` (fresh attempt; not a retry delta plan)
- shared research entrypoint: `uri-r2-c1-p2-replay-root-cause-v1`
- stage selector: `D2-mismatch-localization`
- scenario id: `uri-r2-c1-only-v1`
- subject boundary: authoritative inherited `P1` subject token only (`URI-R2-C1`)

## Authoritative Inputs To Preserve

- `orchestrator/rounds/round-016/evidence/P1/attempt-2/subject-token.json` (authoritative `P1` subject token)
- `orchestrator/rounds/round-017/evidence/P2/attempt-2/check-P2-W.json` and `stage-verdict.json` (accepted `P2` failure boundary)
- `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md` (accepted mismatch context)
- `docs/plans/2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md` and `orchestrator/rounds/round-020/review-record.json` (now-authoritative `D1` reproduction result: `pass`, `accepted + finalize`)

## Sequential Execution Plan (Bounded)

1. Confirm boundary continuity before execution.
   - Re-verify the stage is still `D2`, retry remains inactive (`retry: null`), and no scenario/subject drift exists.
   - Carry forward the exact accepted mismatch signature from `P2` and `D1`: `InstBot expects ⊥, got: t9 -> t9`.

2. Initialize attempt-local `D2` evidence lane.
   - Use only the shared entrypoint tuple:
     `{ research_entrypoint_id: uri-r2-c1-p2-replay-root-cause-v1, stage_selector: D2-mismatch-localization, scenario_id: uri-r2-c1-only-v1, attempt_id: 1 }`.
   - Emit outputs only under `orchestrator/rounds/round-021/evidence/D2/attempt-1/`.

3. Execute `D2-T` trace alignment under one correlation id.
   - Align trace segments for generalization, scheme conversion, no-fallback reification, and witness replay in one bounded chain.
   - Reject any data path that introduces a second executable interface, widened scenario, or replacement subject.

4. Execute `D2-L` first-divergence localization.
   - Identify one exact earliest replay divergence boundary where expected replay identity first splits from observed shape.
   - Keep localization bounded to the inherited replay lane; do not introduce speculative secondary causes.

5. Execute `D2-O` single-owner account assignment.
   - Attribute the localized divergence to exactly one bounded owner account/module boundary (from allowed D2 owner categories).
   - Record why adjacent components are not the owner for this attempt to avoid widened blame.

6. Produce the stage artifact and classification.
   - Write `docs/plans/2026-03-16-uri-r2-c1-d2-replay-mismatch-localization.md` with:
     - `D2-T`, `D2-L`, `D2-O` outcomes,
     - exact divergence boundary statement,
     - exact owner account statement,
     - bounded stage result (`pass` or `semantic-negative` or `inconclusive`).
   - Keep this as diagnosis-only output; no production implementation milestone and no code-path change.

7. Prepare review handoff evidence for contract-v2 fields.
   - Ensure attempt artifacts are review-ready for explicit `Implemented stage result`, `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
   - Preserve prior round artifacts/history as immutable.

## Explicit Non-Goals (Enforced)

- No production implementation milestone.
- No widened scenario/subject beyond `URI-R2-C1` and `uri-r2-c1-only-v1`.
- No second executable interface.
- No rewrite of inherited authoritative artifacts (`P1`, accepted `P2`, authoritative `D1`).
