# `URI-R2-C1` P2 Replay Root-Cause Successor Roadmap

## Context

- This top-level `orchestrator/` now succeeds the completed prototype-evidence campaign whose accepted execution record lives in `orchestrator/rounds/round-016` through `round-019`.
- Completed rounds `round-001` through `round-015` remain inherited baseline and predecessor evidence.
- The completed recursive-types packet under `tasks/todo/2026-03-11-recursive-types-orchestration/` remains immutable predecessor evidence.
- The authoritative prototype-evidence result remains controlling for what was established on the prior track:
  - `P1 = pass`
  - `P2 = semantic-negative`
  - `P3 = semantic-negative`
  - `P4 = hard-stop`
- The approved design source for this successor track is `docs/superpowers/specs/2026-03-16-uri-r2-c1-p2-replay-root-cause-roadmap-design.md`.
- This control plane continues to use `contract_version: 2` retry semantics from `docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md` and `orchestrator/roadmaps/2026-03-16-00-uri-r2-c1-p2-replay-root-cause-successor-roadmap/rev-004/retry-subloop.md`.
- Scope stays research-first: the target is a bounded root-cause verdict for the authoritative `P2` replay mismatch, not a production implementation milestone.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [done] Execute the `D1` replay-failure reproduction contract for `URI-R2-C1`
   Depends on: the completed `P1` through `P4` prototype-evidence track, the authoritative `P1` subject token, and the accepted `P2` replay-failure artifact
   Completion notes: complete when `docs/plans/2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md` is accepted, the reviewer record names the authoritative `D1` attempt, and the round either reproduces the authoritative bounded replay failure under the root-cause entrypoint or records a bounded non-pass without widening.

2. [done] Execute the `D2` replay mismatch localization for `URI-R2-C1`
   Depends on: item 1
   Completion notes: complete when `docs/plans/2026-03-16-uri-r2-c1-d2-replay-mismatch-localization.md` is accepted, the reviewer record names the authoritative `D2` attempt, and the round either localizes one exact bounded divergence boundary and owner account or records a bounded non-pass explicitly.

3. [done] Execute the `D3` bounded fixability probe for `URI-R2-C1`
   Depends on: item 2
   Completion notes: complete when `docs/plans/2026-03-16-uri-r2-c1-d3-bounded-fixability-probe.md` is accepted, the reviewer record names the authoritative `D3` attempt, and the round records either one bounded repair-supporting direction or an explicit bounded non-reopen result.

4. [pending] Execute the `D4` repair-track decision gate for `URI-R2-C1`
   Depends on: item 3
   Completion notes: complete when `docs/plans/2026-03-16-uri-r2-c1-d4-repair-track-decision-gate.md` is accepted, the reviewer record names the authoritative `D4` attempt, and the final bounded outcome is recorded as exactly one of `reopen-repair-track` or `remain-stop`.
