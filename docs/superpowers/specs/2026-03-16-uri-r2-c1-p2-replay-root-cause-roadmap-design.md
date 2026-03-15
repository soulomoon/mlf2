# Staged Roadmap Design: `URI-R2-C1` P2 Replay Root-Cause Track

Date: 2026-03-16
Status: Approved design

## Purpose

Define the next bounded successor track after the completed `URI-R2-C1` prototype-evidence roadmap.

This track does not reopen the earlier roadmap or pretend `P4 = hard-stop` was mistaken. Its purpose is narrower:

- determine whether the authoritative `P2` replay failure is a fixable implementation mismatch inside the bounded research model; or
- confirm that the failure remains a bounded stop condition and should not reopen any repair track.

## Relationship To The Finished Prototype-Evidence Track

The completed prototype-evidence track remains authoritative for what it established:

- `P1 = pass`
- `P2 = semantic-negative`
- `P3 = semantic-negative`
- `P4 = hard-stop`

This new track starts from that result and investigates only the primary failure source named there: the bounded `P2-W` `partial-replay` mismatch.

It does not authorize implementation. At most, it may justify a later, separate repair-track roadmap.

## Starting Point

The starting point is the completed prototype-evidence execution record:

- `docs/plans/2026-03-15-uri-r2-c1-p1-subject-discovery-prototype.md`
- `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md`
- `docs/plans/2026-03-15-uri-r2-c1-p3-safety-validation-prototype.md`
- `docs/plans/2026-03-15-uri-r2-c1-p4-prototype-decision-gate.md`
- `orchestrator/rounds/round-016/review-record.json`
- `orchestrator/rounds/round-017/review-record.json`
- `orchestrator/rounds/round-018/review-record.json`
- `orchestrator/rounds/round-019/review-record.json`

Controlling inherited facts:

- the authoritative subject token was emitted by `P1` attempt `2`;
- the first real failure is `P2-W = partial-replay`;
- the observed bounded mismatch is `InstBot expects ⊥, got: t9 -> t9`;
- `P3` and `P4` are downstream consequences, not separate primary causes.

## Final Goal Of The New Roadmap

This roadmap ends at a decisive bounded successor result.

That result must choose exactly one of:

- `reopen-repair-track`: the root-cause evidence is strong enough to justify a later, separate repair-track roadmap focused on the replay mismatch; or
- `remain-stop`: the mismatch still does not justify reopening any repair track inside the bounded model.

## Non-Goals

- No production implementation milestone.
- No reopening of broad automatic recursive inference.
- No widening beyond `URI-R2-C1`.
- No new subset selection or candidate search.
- No second executable interface outside the shared root-cause research entrypoint.
- No rewriting of accepted `P1` through `P4` artifacts.

## Fixed Boundary Model

These constraints remain mandatory:

- the active subject stays exactly `URI-R2-C1`;
- the active scenario stays exactly `uri-r2-c1-only-v1`;
- the inherited `P1` authoritative subject token remains the only admissible subject;
- the investigation stays local to the bounded `P2` replay path and its direct prerequisites;
- no widened search, heuristic replacement subject, or alternate subset is admissible;
- no production-path semantic change is admissible in this roadmap.

## Shared Research Lane

Use one shared research entrypoint for this track:

- `research_entrypoint_id`: `uri-r2-c1-p2-replay-root-cause-v1`
- invocation shape: `{ stage_selector, scenario_id, attempt_id }`
- `stage_selector` enum:
  - `D1-replay-reproduction`
  - `D2-mismatch-localization`
  - `D3-fixability-probe`
- `scenario_id`: must be exactly `uri-r2-c1-only-v1`
- `attempt_id`: integer `>= 1`, monotonically increasing within the active stage under the live retry-subloop contract

Outputs:

- machine-readable evidence under `orchestrator/rounds/<round-id>/evidence/<stage>/attempt-<n>/`
- reviewer-facing artifact inputs under the same attempt directory
- no writes to production outputs or default runtime caches

## Artifact Contract

Each executable stage writes exactly one reviewer-facing artifact under `docs/plans/`:

- `D1`: `docs/plans/2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md`
- `D2`: `docs/plans/2026-03-16-uri-r2-c1-d2-replay-mismatch-localization.md`
- `D3`: `docs/plans/2026-03-16-uri-r2-c1-d3-bounded-fixability-probe.md`
- `D4`: `docs/plans/2026-03-16-uri-r2-c1-d4-repair-track-decision-gate.md`

`D1` through `D3` use the live retry-subloop contract. `D4` is terminal and may not emit an accepted semantic retry outcome.

## Chosen Roadmap Shape

Use a four-stage bounded diagnosis ladder:

- `D0`: inherited prototype-evidence hard-stop state
- `D1`: replay-failure reproduction contract
- `D2`: replay mismatch localization
- `D3`: bounded fixability probe
- `D4`: repair-track decision gate

## Roadmap Milestones

### `D1` Replay-Failure Reproduction Contract

Deliverable:

- one artifact proving whether the authoritative `P2-W` failure can be reproduced exactly from the inherited subject, scenario, and bounded replay lane.

Purpose:

- establish that the root-cause track is anchored on the real failure and not on stale or widened diagnostics.

Required checks:

- `D1-I` inherited-input continuity:
  - consume only the authoritative `P1` subject token and accepted `P2` evidence;
- `D1-R` replay reproduction:
  - reproduce the same bounded replay failure or record a bounded inability to do so without widening;
- `D1-M` mismatch signature:
  - preserve the exact failure signature or explain any bounded diagnostic drift.

Exit condition:

- `pass` only if the authoritative failure is reproduced exactly enough for bounded diagnosis to proceed;
- otherwise record `semantic-negative` or `inconclusive` without widening.

### `D2` Replay Mismatch Localization

Deliverable:

- one artifact localizing the replay mismatch to one exact bounded divergence point and one exact ownership account.

Purpose:

- determine where the replay lane first stops preserving identity.

Allowed ownership outcomes:

- witness construction
- replay-domain reconstruction
- no-fallback reification output
- `applyInstantiation` semantics

Required checks:

- `D2-T` trace alignment:
  - align generalization, scheme conversion, reification, and replay traces under one shared correlation id;
- `D2-L` localization:
  - identify one exact divergence boundary where expectations and actual replay shape first split;
- `D2-O` owner account:
  - name one exact responsible module or boundary account without widened blame.

Exit condition:

- `pass` only if the track can point to one bounded divergence site and owner account;
- otherwise record `semantic-negative` or `inconclusive`.

### `D3` Bounded Fixability Probe

Deliverable:

- one artifact showing whether the localized mismatch supports one bounded paper-faithful repair candidate or instead collapses back to a bounded non-fixable stop.

Purpose:

- decide whether the root-cause evidence justifies reopening a dedicated repair track.

Required checks:

- `D3-H` fix hypothesis discipline:
  - test one exact bounded repair or impossibility hypothesis;
- `D3-B` boundary preservation:
  - prove the probe does not widen subject, scenario, ownership, or production behavior;
- `D3-V` verdict:
  - classify the probe as repair-supporting, bounded-negative, or inconclusive.

Exit condition:

- `pass` only if one bounded repair direction is justified strongly enough to support a later repair-track roadmap;
- `semantic-negative` if the bounded evidence still points to no lawful repair reopening;
- `inconclusive` if the probe cannot stabilize even after bounded retries.

### `D4` Repair-Track Decision Gate

Deliverable:

- one terminal artifact choosing exactly `reopen-repair-track` or `remain-stop`.

Purpose:

- convert the bounded diagnostic evidence into one final successor decision.

Decision threshold:

- `reopen-repair-track` only when `D1`, `D2`, and `D3` together provide a stable, bounded, non-widening repair direction;
- otherwise `remain-stop`.

## Retry Contract

This roadmap inherits the live `contract_version: 2` retry behavior from:

- `docs/superpowers/specs/2026-03-16-uri-r2-c1-prototype-evidence-retry-subloop-amendment.md`
- `orchestrator/retry-subloop.md`

For this track, the retry-eligible stages are `D1`, `D2`, and `D3`. `D4` is terminal.
