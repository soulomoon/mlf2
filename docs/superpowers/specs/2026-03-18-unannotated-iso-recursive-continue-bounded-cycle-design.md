# Unannotated Iso-Recursive Inference Continue-Bounded Follow-On Cycle Design

Date: 2026-03-18
Status: approved scaffold source
Scope: one new bounded non-widening repo-wide orchestrator cycle after the accepted initial `U1` through `U6` successor run

## Goal

Refresh the live top-level `orchestrator/` so it succeeds the completed initial `U1` through `U6` successor cycle with one new bounded cycle rooted in the accepted `continue-bounded` outcome.

This scaffold does not authorize broad automatic recursive type inference. It authorizes one more bounded non-widening cycle that must stay inside repaired `URI-R2-C1` and the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.

## Predecessor Evidence

- Completed recursive-types packet under `tasks/todo/2026-03-11-recursive-types-orchestration/`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`
- `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- Completed replay repair rounds `round-024` through `round-027`
- Completed initial successor rounds `round-028` through `round-033`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`

Controlling predecessor facts:

- explicit recursive types are implemented and accepted on the explicit-only path;
- automatic recursive-type inference remains unresolved and disabled as the inherited baseline;
- repaired `URI-R2-C1` is still the only accepted live subject;
- `U2` ended at `authority-narrowed`;
- `U3` ended at `uniqueness-owner-stable-refuted`;
- `U4` ended at `constructor-acyclic-termination-refuted`;
- `U5` landed one bounded non-widening `ResultType.Fallback` hardening slice with focused `PipelineSpec` coverage and a passing full repo gate;
- `U6` ended at `continue-bounded`, not `widen-approved` and not `stop-blocked`.

## Design Decision

Use the same top-level `orchestrator/` control plane again and refresh it in place.

Do not keep the finished `U1` through `U6` roadmap live. That cycle is complete and should become predecessor evidence for a fresh bounded cycle.

Do not widen the live subject. The new cycle must still stay inside repaired `URI-R2-C1`.

Completed rounds `round-001` through `round-033` remain immutable historical evidence. The refreshed live control plane should begin new execution at `round-034`.

## Scope

- The long-horizon direction remains fully unannotated iso-recursive-type synthesis in the solver/pipeline.
- The immediate cycle remains bounded to repaired `URI-R2-C1`.
- The new cycle should build on the accepted `U5` hardening slice and the accepted `U6` `continue-bounded` outcome.
- The inherited mandatory boundary remains in force unless a future accepted roadmap update changes it explicitly:
  - explicit-only / non-equi-recursive / non-cyclic-graph as the current baseline;
  - no implicit unfolding or equi-recursive equality;
  - no cyclic structural graph encoding;
  - no silent widening to multi-SCC, cross-family, or broad automatic recursive inference.

## Non-Goals

- No broad automatic recursive inference roadmap.
- No reinterpretation of accepted `U2`/`U3`/`U4` negative findings as if they were clearance.
- No second executable interface or fallback path for experimental recursive inference.
- No reset of round numbering or machine-state continuity.
- No runtime round execution inside this scaffold task.

## Chosen Roadmap Shape

Use one new bounded cycle with four items. Keep the first item concrete and later items progressively coarser.

Follow-on cycle:

1. `C1` continue-bounded bind and exact next-slice target selection
2. `C2` bounded fail-closed solver/pipeline implementation slice
3. `C3` bounded verification and evidence consolidation for that slice
4. `C4` next-cycle decision gate

This shape keeps the new cycle short and concrete. It does not reopen the old evidence ladder from scratch; it starts from the accepted `continue-bounded` outcome and moves directly into one more bounded non-widening implementation cycle.

## Item Intent

### `C1` continue-bounded bind and exact next-slice target selection

Outcome:

- one accepted artifact that binds the new cycle to repaired `URI-R2-C1` under the accepted `continue-bounded` result; and
- one exact bounded next-slice target selection that does not reinterpret the accepted negative evidence as clearance.

This is the first concrete roadmap item.

### `C2` bounded fail-closed solver/pipeline implementation slice

Outcome:

- one accepted bounded implementation slice at the exact target selected in `C1`, with focused tests and preserved fail-closed out-of-scope rejection behavior.

### `C3` bounded verification and evidence consolidation

Outcome:

- one accepted verification artifact that records whether the selected slice stays stable under current bounded verification and continuity evidence.

### `C4` next-cycle decision gate

Outcome:

- one accepted aggregate decision that records exactly one bounded next-step result:
  - `continue-bounded`
  - `widen-approved`
  - `stop-blocked`

This round does not itself widen the roadmap. It records the decision only.

## Retry And Terminal Semantics

- `C1`, `C2`, and `C3` may use same-round retry under `contract_version: 2`.
- `C4` is aggregate-only and may not emit `accepted + retry`.
- After accepted rounds, `update-roadmap` may refine later pending items or append another bounded cycle, but it may not rewrite completed items or silently widen the live subject.

## Acceptance Boundary

Every accepted round in the new cycle must preserve all of the following unless the roadmap itself is amended first:

- no equi-recursive reasoning;
- no cyclic structural graph encoding;
- no hidden default-on recursive inference;
- no widening past repaired `URI-R2-C1`;
- no breakage of replay, reification, principality, or the fail-closed implications already accepted in `U2` through `U4`.

## Recommendation

Refresh the live top-level `orchestrator/` now with:

- `last_completed_round = round-033`;
- idle machine state at `stage: "select-task"`;
- a fresh live roadmap for `C1` through `C4`; and
- verification/role prompts that explicitly treat the accepted `U6` `continue-bounded` result as the predecessor for one more bounded non-widening cycle.
