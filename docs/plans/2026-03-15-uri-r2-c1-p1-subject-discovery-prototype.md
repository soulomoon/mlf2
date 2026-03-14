# `P1` Subject-Discovery Prototype For `URI-R2-C1`

Date: 2026-03-15
Roadmap item: 1
Stage: `P1`
Attempt: 2
Active subject: `URI-R2-C1`
Active scenario: `uri-r2-c1-only-v1`
Artifact kind: subject-discovery prototype

## Inherited Inputs

- `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
- `docs/plans/2026-03-14-uri-r2-c1-re5-final-successor-recommendation.md`
- `orchestrator/rounds/round-016/selection.md`

## Stage Input Interface

- Fixed inherited subject boundary: `URI-R2-C1`, `single-SCC`, `single-binder-family`, non-equi-recursive, non-cyclic structural graph.
- `research_entrypoint_id`: `uri-r2-c1-prototype-entrypoint-v1`
- `stage_selector`: `P1-subject-discovery`
- `scenario_id`: `uri-r2-c1-only-v1`
- Candidate universe: bounded local recursive roots or equivalent local clusters inside `URI-R2-C1`, one SCC, one binder family, normalized by `cluster-equivalence-v1`.

## Method

- Shared research entrypoint: `uri-r2-c1-prototype-entrypoint-v1`.
- Bounded scenario: `uri-r2-c1-only-v1` only.
- Check sequence: `P1-C`, `P1-N`, `P1-U`.
- Evidence directory: `orchestrator/rounds/round-016/evidence/P1/attempt-2`.

## Evidence

Normalized candidate set:
- `cluster-1` (`equivalent-local-cluster`), normalization basis `cluster-equivalence-v1`, verdict `admissible`, rejection trigger `none`.

Every inadmissible candidate would fail inside the fixed boundary. This accepted attempt observed no non-`none` rejection trigger, so no inadmissible normalized candidate remained after bounded discovery.

## Rejection Triggers

No non-`none` rejection triggers were observed.

## Stage Result

`pass`

## Next-Stage Handoff

Canonical subject token: `orchestrator/rounds/round-016/evidence/P1/attempt-2/subject-token.json`.
