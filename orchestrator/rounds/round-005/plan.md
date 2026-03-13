# Round 005 Plan (Roadmap Item 5: Decide Implementation Handoff or Research Stop)

## Round Objective

Produce the smallest docs-only decision slice that converts round-004 signal `feasible-continue` into a concrete implementation-handoff target for `ARI-C1`, unless current evidence forces `no-go/not-yet-go`.

This round must keep boundaries unchanged: **explicit-only / non-equi-recursive / non-cyclic-graph**.

## Scope Mode (Strictly Docs-Only)

- Allowed: decision/handoff docs under `docs/plans/` and round artifacts under `orchestrator/rounds/round-005/`.
- Not allowed: edits under `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`; solver/inference implementation; roadmap reordering/editing.

Rationale: item 5 is a decision-and-handoff milestone, and round 004 already supplied bounded feasibility evidence.

## Scope Guardrails

- Do not edit `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-005/orchestrator/roadmap.md`.
- Do not modify predecessor packet history under `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-005/tasks/todo/2026-03-11-recursive-types-orchestration/`.
- Do not widen behavior claims beyond `ARI-C1`.
- If contradictory evidence is discovered while drafting the decision artifact, switch outcome to `no-go/not-yet-go` and document blockers explicitly.

## Exact Target Files

1. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-005/docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`
2. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-005/orchestrator/rounds/round-005/implementation-notes.md`

## Sequential Tasks

### Task 1 — Write the roadmap-item-5 decision + handoff contract

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-005/docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`

Required contents:
- Decision statement with one explicit outcome:
  - default expected outcome: `implementation-handoff` (because round 004 concluded `feasible-continue`);
  - fallback allowed outcome: `no-go/not-yet-go` only if newly cited contradictory evidence is presented.
- Evidence chain summary referencing:
  - baseline contract (item 1),
  - invariant audit (item 2),
  - candidate selection (item 3),
  - feasibility spike (item 4, including `feasible-continue`).
- Concrete handoff target for implementation phase, bounded to `ARI-C1`:
  - ingress boundary (annotation-anchored only),
  - shape boundary (single recursive binder family, single SCC),
  - explicit exclusions (fully unannotated synthesis, equi-recursive reasoning, cyclic graph encoding, default-on widening).
- Exact implementation-entry file set (names only, no edits this round) that the next implementation round is authorized to touch first:
  - `src/MLF/Elab/Run/Pipeline.hs`
  - `src/MLF/Constraint/Solve/Worklist.hs`
  - `src/MLF/Constraint/Unify/Decompose.hs`
  - `src/MLF/Reify/Type.hs`
  - `test/PipelineSpec.hs`
  - `test/PresolutionSpec.hs`
- Mandatory stop/revert-to-research triggers for the implementation round.
- Continuity statement that predecessor packet truth is referenced, not rewritten.

### Task 2 — Record round implementation notes and explicit handoff signal

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-005/orchestrator/rounds/round-005/implementation-notes.md`

Required contents:
- Concise record of the item-5 decision taken (`implementation-handoff` or `no-go/not-yet-go`).
- If handoff is chosen, include the exact handoff target doc path and a one-line summary of bounded scope.
- If no-go/not-yet-go is chosen, include concrete blocker evidence and what must be resolved before reopening implementation.
- Explicit statement that this round is docs-only and introduced no production behavior changes.

## Acceptance Criteria (All Required)

1. `/docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md` exists and records exactly one explicit item-5 decision outcome.
2. The decision doc includes:
   - evidence chain from items 1-4,
   - preserved boundary phrase `explicit-only / non-equi-recursive / non-cyclic-graph`,
   - explicit `ARI-C1` implementation boundaries and exclusions,
   - concrete first-touch implementation file set,
   - stop triggers.
3. `/orchestrator/rounds/round-005/implementation-notes.md` exists and mirrors the same decision signal without ambiguity.
4. Diff is docs/orchestrator-round artifacts only, with no edits to:
   - `orchestrator/roadmap.md`,
   - predecessor packet authoritative logs,
   - production/test code paths.
5. Decision artifact is sufficient for a subsequent implementer to start a bounded `ARI-C1` implementation round without reopening research scope selection.

## Reviewer and Verification Checks

Baseline checks (from `orchestrator/verification.md`):
- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
- `cabal build all && cabal test` is **not required this round** if diff excludes `src/`, `src-public/`, `app/`, `test/`, and `mlf2.cabal`; reviewer must record this skip rationale.
- Reviewer-recorded guidance sync check for `AGENTS.md`, `tasks/readme`, `TODO.md`, `CHANGELOG.md`, `implementation_notes.md` (updated or intentionally unchanged with rationale).

Round-specific checks:
- Planned-file boundary:
  - `git diff --name-only`
  - `git diff --name-only | rg '^orchestrator/roadmap\.md$'` (expected: no matches)
  - `git diff --name-only | rg '^(src|src-public|app|test)/'` (expected: no matches)
  - `git diff --name-only | rg '^mlf2\.cabal$'` (expected: no matches)
  - `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'` (expected: no matches)
- Decision-contract markers present:
  - `rg -n 'ARI-C1|feasible-continue|implementation-handoff|no-go|not-yet-go|explicit-only / non-equi-recursive / non-cyclic-graph|single SCC|single recursive binder family|stop trigger' docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md orchestrator/rounds/round-005/implementation-notes.md`
- Handoff concreteness check:
  - `rg -n 'src/MLF/Elab/Run/Pipeline\.hs|src/MLF/Constraint/Solve/Worklist\.hs|src/MLF/Constraint/Unify/Decompose\.hs|src/MLF/Reify/Type\.hs|test/PipelineSpec\.hs|test/PresolutionSpec\.hs' docs/plans/2026-03-14-automatic-recursive-inference-item5-handoff-decision.md`

Review output requirement:
- Reviewer writes `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-005/orchestrator/rounds/round-005/review.md` with baseline evidence, task-specific evidence, explicit decision (`approve`/`reject`), and explicit item-5 outcome confirmation (`implementation-handoff` or `no-go/not-yet-go`).
