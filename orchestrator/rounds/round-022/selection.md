# Round-022 Selection (`URI-R2-C1` bounded replay root-cause track)

## Roadmap Provenance

- Roadmap ID: `2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap`
- Roadmap Revision: `rev-003`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-003`
- State Snapshot: `orchestrator/rounds/round-022/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected roadmap item

- Item 3 (`D3`): Execute the `D3` bounded fixability probe for `URI-R2-C1`.

## Why this must run now

- Roadmap ordering and guider policy require selecting the lowest-numbered unfinished item unless retry state forces otherwise.
- `orchestrator/rounds/round-022/state-snapshot.json` shows `retry: null`, so no same-round retry is required.
- Round-020 (`D1`) and round-021 (`D2`) both finalized as authoritative `pass` with `stage_action: finalize`, satisfying item-3 dependencies.
- The track remains bounded to `URI-R2-C1` / `uri-r2-c1-only-v1`; `D3` is the next required diagnostic gate to determine whether evidence supports one bounded repair direction or stays non-reopen.
