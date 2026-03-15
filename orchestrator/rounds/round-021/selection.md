# Round-021 Selection

Selected roadmap item: **2. Execute `D2` replay mismatch localization for `URI-R2-C1`**.

Why this must run now:
- `orchestrator/state.json` is at `select-task` for `round-021` with `retry: null`, so no same-round retry is required.
- `orchestrator/rounds/round-020/review-record.json` finalized `D1` as authoritative (`attempt_verdict: accepted`, `stage_action: finalize`, `stage_result: pass`).
- Under the guider contract and roadmap ordering, the next action is the lowest-numbered unfinished bounded item, which is `D2`.
- Running `D2` now preserves the fixed `URI-R2-C1` / `uri-r2-c1-only-v1` boundary and advances the root-cause track by localizing one exact divergence boundary and owner account before any fixability probe (`D3`) or decision gate (`D4`).
