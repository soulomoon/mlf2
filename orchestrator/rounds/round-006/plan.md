# Round 006 Plan (Roadmap Item 1: `R1` Gap Map)

## Round Objective

Deliver the smallest docs-only research slice that closes successor roadmap item 1 by writing one concrete `R1` gap-map artifact from accepted `ARI-C1` to unannotated, single-SCC, single-binder-family iso-recursive inference, while preserving the approved `R1` -> `R5` staging and keeping the inherited item-2 invariant audit authoritative.

## Scope Mode (Strictly Docs-Only Research Artifact)

- This round is docs-only.
- Do not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Do not edit `orchestrator/state.json` or `orchestrator/roadmap.md`.
- Do not rewrite predecessor packet history or prior round artifacts.
- Do not select the `R2` subset, write the `R3` obligation contract, make the `R4` feasibility decision, or draft the `R5` handoff.

Rationale: the selected item is `R1` only, and the approved successor design says this stage should produce a finite gap map before any subset selection or feasibility gate is attempted.

## Scope Guardrails

- Preserve the fixed boundary model throughout:
  - single-SCC obligation-level recursion only;
  - single binder family only;
  - non-equi-recursive semantics only;
  - non-cyclic structural graph only;
  - fail closed if the gap map discovers that forbidden widening would be required.
- Treat `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` as the authoritative invariant audit for acyclicity, binding-tree discipline, occurs-check/termination, reconstruction/reification/witness replay, and principality risk boundaries.
- Use completed automatic-recursive-inference rounds `001` through `005` as inherited baseline evidence only; do not reopen them as pending work.
- Keep the round bounded to docs/orchestrator/task-surface edits only.

## Exact Target Files

1. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md`
2. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/orchestrator/rounds/round-006/implementation-notes.md`

## Sequential Tasks

### Task 1 - Write the `R1` gap-map artifact

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md`

Required contents:
- A short inherited-baseline section that cites the accepted `ARI-C1` handoff as `R0` and names the authoritative prior evidence inputs:
  - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md`
  - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/docs/plans/2026-03-14-automatic-recursive-inference-candidate-subset-selection.md`
  - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/docs/plans/2026-03-14-automatic-recursive-inference-ari-c1-feasibility-spike.md`
  - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`
  - `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/docs/superpowers/specs/2026-03-14-unannotated-iso-recursive-roadmap-design.md`
- A fixed-boundary section that restates, without weakening, the mandatory boundary phrases:
  - unannotated target is still single-SCC and single-binder-family only;
  - recursion is obligation-level, not cyclic structural-graph encoding;
  - no equi-recursive reasoning or implicit unfolding;
  - no default-on widening.
- One finite gap table or equivalent structured list that, for each concrete delta between `ARI-C1` and the unannotated target, records:
  - what information is currently supplied only by explicit anchors;
  - what information might be recovered locally without widening semantics;
  - which unannotated single-SCC cases remain blocked;
  - which inherited item-2 invariant classes constrain that gap;
  - what later stage owns the unresolved decision (`R2`, `R3`, `R4`, or `R5`).
- The gap map must cover, at minimum, the research deltas around:
  - recursive-binder obligation discovery without explicit anchors;
  - SCC detection/bounding under the single-SCC rule;
  - single-binder-family ownership and no-cross-family-linking discipline;
  - local recoverability limits imposed by occurs-check/termination constraints;
  - reconstruction/reification/witness replay consequences of removing explicit anchors;
  - principality-risk boundaries for unannotated recursive-shape admission.
- A stage-discipline section that explicitly says:
  - this document does not choose the bounded subset for `R2`;
  - this document does not define the `R3` obligation contract;
  - this document does not record an `R4` feasibility outcome;
  - this document does not draft the `R5` implementation handoff.
- A continuity section that states prior completed rounds and the predecessor recursive-types packet are cited as evidence only and are not rewritten.

### Task 2 - Record round implementation notes

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/orchestrator/rounds/round-006/implementation-notes.md`

Required contents:
- Short summary of the `R1` gap-map artifact and why it satisfies successor roadmap item 1.
- Explicit statement that the inherited item-2 invariant audit remains authoritative and was not reopened.
- Explicit statement that this round is docs-only and introduced no production behavior changes.
- Explicit statement that `R2` through `R5` remain future stages and were not executed here.

## Acceptance Criteria (All Required)

1. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md` exists and presents a finite, concrete gap list from `ARI-C1` to the unannotated single-SCC target.
2. The gap-map artifact explicitly separates:
   - anchor-supplied information,
   - potentially locally recoverable information,
   - still-blocked unannotated cases.
3. The gap-map artifact keeps the inherited item-2 invariant audit authoritative and maps each gap to relevant invariant classes instead of re-auditing them from scratch.
4. The artifact explicitly preserves the approved `R1` -> `R5` staging by deferring subset selection, obligation-contract writing, feasibility judgment, and handoff drafting.
5. `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/orchestrator/rounds/round-006/implementation-notes.md` exists and records docs-only execution plus staging continuity.
6. Diff remains limited to planned docs/round artifacts and does not modify:
   - `orchestrator/state.json`,
   - `orchestrator/roadmap.md`,
   - `src/`, `src-public/`, `app/`, `test/`,
   - `mlf2.cabal`,
   - predecessor packet/task history.

## Reviewer And Verification Checks

Baseline checks (from `orchestrator/verification.md`):
- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmap.md`
- `cabal build all && cabal test` is not required this round if the diff excludes `src/`, `src-public/`, `app/`, `test/`, and `mlf2.cabal`; reviewer must record that skip rationale explicitly.
- Reviewer-recorded continuity check against inherited evidence from completed rounds `001` through `005`, the approved successor design spec, and the predecessor recursive-types packet.

Round-specific checks:
- Docs-only boundary:
  - `git diff --name-only`
  - `git diff --name-only | rg '^(src/|src-public/|app/|test/|mlf2\\.cabal$)'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/state\\.json$'` (expected: no matches)
  - `git diff --name-only | rg '^orchestrator/roadmap\\.md$'` (expected: no matches)
  - `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'` (expected: no matches)
- Gap-map content markers present:
  - `rg -n 'ARI-C1|R0|explicit anchor|locally recoverable|blocked|single-SCC|single-binder-family|acyclic|binding|occurs-check|termination|reconstruction|reification|witness|principality|R2|R3|R4|R5' docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md orchestrator/rounds/round-006/implementation-notes.md`
- Staging-preservation checks:
  - `rg -n '^Candidate ID:' docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md` (expected: no matches)
  - `rg -n 'feasible-continue|not-yet-go|implementation-handoff' docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md` (expected: no matches)
- Continuity-reference checks:
  - `rg -n '2026-03-14-automatic-recursive-inference-baseline-contract|2026-03-14-automatic-recursive-inference-invariant-audit|2026-03-14-automatic-recursive-inference-candidate-subset-selection|2026-03-14-automatic-recursive-inference-ari-c1-feasibility-spike|2026-03-14-automatic-recursive-inference-item5-handoff-decision|2026-03-14-unannotated-iso-recursive-roadmap-design' docs/plans/2026-03-14-unannotated-iso-recursive-r1-gap-map.md`

Review output requirements:
- Reviewer writes `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-006/orchestrator/rounds/round-006/review.md` with:
  - baseline checks evidence,
  - round-specific checks evidence,
  - explicit decision (`approve` or `reject`),
  - explicit confirmation that this round stayed at `R1` and preserved inherited invariant-audit authority.
