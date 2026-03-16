# `URI-R2-C1` `P2` Replay Repair-Track Roadmap Design

Date: 2026-03-17
Status: approved scaffold source
Scope: bounded single-scenario repair-track successor control plane

## Goal

Translate the accepted `D1` through `D4` diagnosis into a bounded implementation campaign that repairs the localized replay failure at `witness-replay/applyInstantiation-instbot-precondition` without widening scope, adding a second executable interface, or introducing convenience fallback behavior.

## Predecessor Evidence

- Completed prototype-evidence rounds `round-016` through `round-019`
- Completed replay root-cause rounds `round-020` through `round-023`
- `BUG-2026-03-16-001` in `Bugs.md`
- `docs/plans/2026-03-16-uri-r2-c1-d3-bounded-fixability-probe.md`
- `docs/plans/2026-03-16-uri-r2-c1-d4-repair-track-decision-gate.md`

Controlling predecessor facts:

- `P1 = pass`
- `P2 = semantic-negative`
- `D1 = pass`
- `D2 = pass`
- `D3 = pass`
- `D4 = reopen-repair-track`

## Scope

- Subject stays fixed to `URI-R2-C1`.
- Scenario stays fixed to `uri-r2-c1-only-v1`.
- Repair scope stays fixed to the localized owner boundary `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).
- The repair track may change production code, but only at the bounded replay boundary justified by `D2` and `D3`.
- `contract_version: 2` retry semantics remain active for early stages.

## Non-Goals

- No broadened replay-regression campaign.
- No second executable interface for repair work.
- No default-path convenience fallback or compatibility layer.
- No widening to other subjects, scenarios, SCCs, or recursive-inference lanes.
- No reopening of completed `D1` through `D4` diagnosis rounds as live work.

## Roadmap

1. `R1` repair-boundary reproduction contract
   - Create the concrete bounded repair target for the localized `InstBot` mismatch.
   - Outcome: a reviewer-accepted artifact that reproduces the locked replay failure in implementation-facing terms without widening beyond the accepted scenario and owner boundary.

2. `R2` bounded `InstBot` repair
   - Implement the smallest paper-faithful production fix at `MLF.Elab.Inst.applyInstantiation`.
   - Outcome: a reviewer-accepted bounded repair diff plus any focused tests or artifacts required to show the change is localized.

3. `R3` locked replay-path verification
   - Verify that the authoritative replay path now succeeds for the locked scenario and that the old mismatch no longer occurs.
   - Outcome: a reviewer-accepted verification artifact showing bounded success and no prohibited widening or fallback behavior.

4. `R4` repair decision gate
   - Aggregate `R1` through `R3` and record the final bounded status.
   - Outcome: exactly one of `repair-accepted` or `repair-blocked`.

## Stage Semantics

- `R1`, `R2`, and `R3` may use the same-round retry subloop under `contract_version: 2`.
- `R4` is terminal and may not emit `accepted + retry`.
- Each stage must preserve predecessor evidence from rounds `round-020` through `round-023` plus the earlier prototype-evidence track.

## Acceptance Boundary

The repair track is successful only if all of the following hold together:

- the localized repair boundary from `D2` remains unchanged;
- the implemented code change remains bounded to the accepted owner area;
- the locked replay path succeeds for `URI-R2-C1` / `uri-r2-c1-only-v1`;
- no second executable interface, convenience fallback, or broader regression scope is introduced.

If those conditions are not met, the repair track must fail closed as `repair-blocked`.
