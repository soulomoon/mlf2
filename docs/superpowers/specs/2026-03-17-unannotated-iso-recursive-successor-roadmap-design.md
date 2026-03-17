# Unannotated Iso-Recursive Inference Successor Roadmap Design

Date: 2026-03-17
Status: approved scaffold source
Scope: bounded repo-wide successor control plane for repeated evidence and implementation loops toward fully unannotated iso-recursive-type synthesis

## Goal

Replace the completed `URI-R2-C1` replay repair-track control plane with a successor campaign that can keep making bounded progress toward fully unannotated iso-recursive-type synthesis in the solver/pipeline.

This design does not authorize a one-shot broad implementation push. It authorizes a dynamic repo-wide loop that repeatedly selects one bounded next step, lands evidence or one bounded implementation slice, verifies it, and then revises the remaining roadmap without rewriting accepted history.

## Predecessor Evidence

- Completed recursive-types packet under `tasks/todo/2026-03-11-recursive-types-orchestration/`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`
- `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- Completed replay repair rounds `round-024` through `round-027`
- `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`

Controlling predecessor facts:

- explicit recursive types are implemented and accepted on the explicit-only path;
- automatic recursive-type inference remains unresolved and disabled as the inherited baseline;
- bounded candidate `ARI-C1` established one annotation-anchored non-widening handoff, not broad unannotated inference;
- the prior unannotated `URI-R2-C1` track stopped because unannotated authority, uniqueness, and feasibility obligations were not cleared;
- the bounded `URI-R2-C1` replay repair track is now complete with final outcome `repair-accepted`.

## Design Decision

Use the top-level `orchestrator/` as the live successor control plane again.

Do not create a sidecar task-folder loop as the primary authority. The top-level orchestrator already encodes the repo-wide successor pattern, and the new goal is the next long-running campaign rather than an isolated side task.

Completed rounds `round-001` through `round-027` remain immutable historical evidence. The successor loop resumes from that history and should start new live rounds at `round-028`.

## Scope

- The long-horizon direction is fully unannotated iso-recursive-type synthesis in the solver/pipeline.
- The live subject starts bounded to `URI-R2-C1` as the repaired and best-audited unannotated-adjacent lane.
- Progress must remain staged and fail-closed: evidence first, then one bounded implementation slice, then verification, then only bounded widening if accepted evidence justifies it.
- The inherited mandatory boundary remains in force unless a future accepted roadmap update changes it explicitly:
  - explicit-only / non-equi-recursive / non-cyclic-graph as the current baseline;
  - no implicit unfolding or equi-recursive equality;
  - no cyclic structural graph encoding;
  - no silent widening to multi-SCC, cross-family, or broad automatic recursive inference.

## Non-Goals

- No single round or first roadmap cycle that attempts fully broad automatic recursive inference.
- No default-on widening from the repaired `URI-R2-C1` lane to arbitrary unannotated recursive programs.
- No second executable interface or fallback path for experimental recursive inference.
- No rewriting of predecessor packet logs or accepted round artifacts.
- No reset of round numbering or machine-state continuity.

## Chosen Roadmap Shape

Use one bounded cycle with six initial items. The cycle is intentionally dynamic: after an accepted aggregate decision round, the guider may refine or append later pending items so the next cycle stays concrete and bounded.

Initial cycle:

1. `U1` inherited baseline and repaired-subject bind
2. `U2` provenance-stable unannotated authority clearance
3. `U3` uniqueness and owner-stability clearance
4. `U4` constructor-directed / acyclicity / termination clearance
5. `U5` bounded solver/pipeline implementation slice
6. `U6` end-to-end verification and next-widening decision gate

Later cycles may replace or append future pending items after accepted `update-roadmap` stages, but they must preserve completed-item truth and keep the next unfinished item concrete.

## Item Intent

### `U1` inherited baseline and repaired-subject bind

Outcome:

- one accepted artifact that restates the inherited automatic-recursive baseline after the completed replay repair track;
- one explicit live-subject bind to the repaired `URI-R2-C1` lane; and
- one reviewer-visible list of hard stop triggers that still forbid broad automatic inference.

This is the first concrete roadmap item.

### `U2` provenance-stable unannotated authority clearance

Outcome:

- one accepted artifact that either clears or narrows the missing provenance-stable unannotated root or cluster authority for the live subject.

This attacks the prior decisive blocker first.

### `U3` uniqueness and owner-stability clearance

Outcome:

- one accepted artifact proving or refuting that the live subject has one admissible local root or cluster with stable ownership and no heuristic ranking.

### `U4` constructor-directed / acyclicity / termination clearance

Outcome:

- one accepted artifact proving or refuting that the current solver discipline can admit the live subject without equi-recursion, cyclic graphs, or weakened termination guarantees.

### `U5` bounded solver/pipeline implementation slice

Outcome:

- one accepted bounded implementation slice that handles exactly the currently cleared subject and no broader automatic recursive behavior.

### `U6` end-to-end verification and next-widening decision gate

Outcome:

- one accepted aggregate decision that records exactly one bounded next-step result:
  - `continue-bounded`
  - `widen-approved`
  - `stop-blocked`

This round does not itself perform broader implementation. It decides what the next roadmap update may do.

## Retry And Terminal Semantics

- `U1` through `U5` may use same-round retry under `contract_version: 2`.
- `U6` is aggregate-only and may not emit `accepted + retry`.
- After accepted rounds, `update-roadmap` may refine later pending items or append the next bounded cycle, but it may not rewrite completed items or silently widen the live subject.

## Acceptance Boundary

The successor campaign is still bounded unless an accepted roadmap update explicitly says otherwise.

Every accepted round must preserve all of the following unless the roadmap itself is amended first:

- no equi-recursive reasoning;
- no cyclic structural graph encoding;
- no hidden default-on recursive inference;
- no widening past the currently bound live subject;
- no breakage of acyclicity, binding-tree discipline, occurs-check termination, replay, reification, or principality boundaries already accepted in predecessor evidence.

## Recommendation

Scaffold the new top-level successor control plane now, rooted in this design, with:

- `last_completed_round = round-027`;
- idle machine state at `stage: select-task`;
- a fresh initial roadmap for `U1` through `U6`; and
- role prompts and verification rules that explicitly allow dynamic roadmap refinement only after accepted rounds.
