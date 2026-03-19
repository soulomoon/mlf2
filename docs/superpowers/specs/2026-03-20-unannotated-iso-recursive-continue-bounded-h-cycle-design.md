# Unannotated Iso-Recursive Inference Continue-Bounded `H`-Cycle Design

Date: 2026-03-20
Status: approved scaffold source
Scope: one new bounded non-widening repo-wide orchestrator cycle after the accepted `G1` through `G4` follow-on run

## Goal

Refresh the live top-level `orchestrator/` so it succeeds the completed `G1`
through `G4` cycle with one more bounded cycle rooted in the accepted
`continue-bounded` outcome from `G4`.

This scaffold does not authorize broad automatic recursive type inference. It
authorizes one more bounded non-widening cycle that must stay inside repaired
`URI-R2-C1` and the inherited explicit-only / non-equi-recursive /
non-cyclic-graph boundary.

## Predecessor Evidence

- Completed recursive-types packet under `tasks/todo/2026-03-11-recursive-types-orchestration/`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`
- `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
- Completed replay repair rounds `round-024` through `round-027`
- Completed initial successor rounds `round-028` through `round-033`
- Completed continue-bounded follow-on rounds `round-034` through `round-049`
- `docs/plans/2026-03-19-uri-r2-c1-g4-next-cycle-decision-gate.md`

Controlling predecessor facts:

- explicit recursive types remain implemented and accepted only on the explicit-only path;
- automatic recursive-type inference remains unresolved and disabled as the inherited baseline;
- repaired `URI-R2-C1` remains the only accepted live subject;
- accepted negative findings remain binding: `U2 = authority-narrowed`, `U3 = uniqueness-owner-stable-refuted`, `U4 = constructor-acyclic-termination-refuted`;
- the accepted `E` and `F` cycles remain bounded predecessor evidence only;
- the accepted `G1` bind froze `G2` to the local-binding `rootHasMultiInst` lane and left `instArgRootMultiBase` explicitly unselected;
- the accepted `G2` slice implemented only the local `rootLocalMultiInst` / `targetC -> rootFinal` lane with a passing full repo gate;
- the accepted `G3` slice reverified exactly that `G2` lane under read-only anchors, a fresh focused rerun, and a fresh full repo gate;
- `G4` ended at `continue-bounded`, not `widen-approved` and not `stop-blocked`.

## Design Decision

Use the same top-level `orchestrator/` control plane again and refresh it in
place.

Do not keep the finished `G1` through `G4` cycle as live work. That cycle is
complete and should become predecessor evidence for one fresh bounded cycle.

Do not widen the live subject. The new cycle must still stay inside repaired
`URI-R2-C1`.

Completed rounds `round-001` through `round-049` remain immutable historical
evidence. The refreshed live control plane should return to idle
`stage: "select-task"` with `last_completed_round: "round-049"` so runtime
execution can open a fresh `round-050` only when the next real run starts.

## Scope

- The long-horizon direction remains fully unannotated iso-recursive-type synthesis in the solver/pipeline.
- The immediate cycle remains bounded to repaired `URI-R2-C1`.
- The new cycle should build on the accepted `G2` / `G3` bounded implementation-and-verification chain plus the accepted `G4` `continue-bounded` outcome.
- The inherited mandatory boundary remains in force unless a future accepted roadmap update changes it explicitly:
  - explicit-only / non-equi-recursive / non-cyclic-graph as the current baseline;
  - no implicit unfolding or equi-recursive equality;
  - no cyclic structural graph encoding;
  - no silent widening to multi-SCC, cross-family, or broad automatic recursive inference.

## Non-Goals

- No broad automatic recursive inference roadmap.
- No reinterpretation of accepted `U2` / `U3` / `U4` negative findings as if they were clearance.
- No reopening of the accepted `E`, `F`, or `G` cycles as live work.
- No second executable interface or fallback path for experimental recursive inference.
- No runtime round execution inside this scaffold task.

## Chosen Roadmap Shape

Use one new bounded cycle with four items. Keep the first item concrete and
later items progressively coarser.

Follow-on cycle:

1. `H1` continue-bounded bind and exact next-slice target selection
2. `H2` bounded fail-closed solver/pipeline implementation slice
3. `H3` bounded verification and evidence consolidation for that slice
4. `H4` next-cycle decision gate

This shape keeps the new cycle short and concrete. It does not reopen the old
evidence ladder from scratch; it starts from the accepted `G4`
`continue-bounded` result and moves directly into one more bounded
non-widening implementation cycle.

## Item Intent

### `H1` continue-bounded bind and exact next-slice target selection

Outcome:

- one accepted artifact that binds the new cycle to repaired `URI-R2-C1` under
  the accepted `G4 = continue-bounded` result; and
- one exact bounded next-slice target selection that chooses only the remaining
  local-binding `instArgRootMultiBase` `keepTargetFinal` / `targetC` family
  without reinterpreting accepted negative evidence as clearance.

This is the first concrete roadmap item.

### `H2` bounded fail-closed solver/pipeline implementation slice

Outcome:

- one accepted bounded implementation slice at the exact target selected in
  `H1`, with focused tests and preserved fail-closed out-of-scope rejection
  behavior.

### `H3` bounded verification and evidence consolidation

Outcome:

- one accepted verification artifact that records whether the selected slice
  stays stable under current bounded verification and continuity evidence.

### `H4` next-cycle decision gate

Outcome:

- one accepted aggregate decision that records exactly one bounded next-step
  result:
  - `continue-bounded`
  - `widen-approved`
  - `stop-blocked`

This round does not itself widen the roadmap. It records the decision only.

## Retry And Terminal Semantics

- `H1`, `H2`, and `H3` may use same-round retry under `contract_version: 2`.
- `H4` is aggregate-only and may not emit `accepted + retry`.
- After accepted rounds, `update-roadmap` may refine later pending items or
  append another bounded cycle, but it may not rewrite completed items or
  silently widen the live subject.

## Acceptance Boundary

Every accepted round in the new cycle must preserve all of the following unless
the roadmap itself is amended first:

- no equi-recursive reasoning;
- no cyclic structural graph encoding;
- no hidden default-on recursive inference;
- no widening past repaired `URI-R2-C1`;
- no reinterpretation of accepted `U2` / `U3` / `U4` negatives as clearance;
- no breakage of replay, reification, principality, or the accepted fail-closed
  implications already established through the `E`, `F`, and `G` cycles.

## Recommendation

Refresh the live top-level `orchestrator/` now with:

- `last_completed_round = round-049`;
- idle machine state at `stage: "select-task"`;
- a fresh live roadmap for `H1` through `H4`; and
- verification guidance that explicitly treats the accepted `G4`
  `continue-bounded` result as permission for one more bounded cycle only,
  centered on the still-unselected `instArgRootMultiBase` family.
