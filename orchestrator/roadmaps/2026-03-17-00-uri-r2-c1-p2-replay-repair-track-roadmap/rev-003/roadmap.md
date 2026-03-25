# `URI-R2-C1` P2 Replay Repair-Track Roadmap

## Context

- This top-level `orchestrator/` now succeeds the completed replay root-cause diagnostic campaign whose accepted execution record lives in `orchestrator/rounds/round-020` through `round-023`.
- Completed rounds `round-001` through `round-019` remain inherited baseline and predecessor evidence.
- The completed recursive-types packet under `tasks/todo/2026-03-11-recursive-types-orchestration/` remains immutable predecessor evidence.
- The authoritative predecessor results that control entry into this repair track are:
  - `P1 = pass`
  - `P2 = semantic-negative`
  - `D1 = pass`
  - `D2 = pass`
  - `D3 = pass`
  - `D4 = reopen-repair-track`
- The controlling defect is `BUG-2026-03-16-001`, localized to `witness-replay/applyInstantiation-instbot-precondition` with owner account `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).
- The approved design source for this repair track is `docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`.
- This control plane continues to use `contract_version: 2` retry semantics from `docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md` and `orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-003/retry-subloop.md`.
- Scope is implementation-first but still bounded: the target is one paper-faithful repair at the localized replay boundary for `URI-R2-C1` / `uri-r2-c1-only-v1`, not a broadened regression or replay-semantics campaign.

## Status Legend

- `pending`
- `in-progress`
- `done`

## Items

1. [done] Execute the `R1` repair-boundary reproduction contract for `URI-R2-C1`
   Depends on: the completed `D1` through `D4` diagnostic track and `BUG-2026-03-16-001`
   Completion notes: complete when `docs/plans/2026-03-17-uri-r2-c1-r1-repair-boundary-reproduction.md` is accepted, the reviewer record names the authoritative `R1` attempt, and the round reproduces the localized repair target for the locked `URI-R2-C1` / `uri-r2-c1-only-v1` lane without widening beyond the accepted owner boundary.

2. [done] Execute the `R2` bounded `InstBot` repair for `URI-R2-C1`
   Depends on: item 1
   Completion notes: complete when `docs/plans/2026-03-17-uri-r2-c1-r2-bounded-instbot-repair.md` is accepted, the reviewer record names the authoritative `R2` attempt, and the round lands one bounded paper-faithful repair at `MLF.Elab.Inst.applyInstantiation` without widening scope or introducing convenience fallback behavior.

3. [pending] Execute the `R3` locked replay-path verification for `URI-R2-C1`
   Depends on: item 2
   Completion notes: complete when `docs/plans/2026-03-17-uri-r2-c1-r3-locked-replay-verification.md` is accepted, the reviewer record names the authoritative `R3` attempt, and the round records bounded success of the authoritative replay path for `URI-R2-C1` / `uri-r2-c1-only-v1` without widened regression scope or second-interface behavior.

4. [pending] Execute the `R4` repair decision gate for `URI-R2-C1`
   Depends on: item 3
   Completion notes: complete when `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` is accepted, the reviewer record names the authoritative `R4` attempt, and the final bounded outcome is recorded as exactly one of `repair-accepted` or `repair-blocked`.
