# Round 002 Plan (Roadmap Item 2)

## Round Objective

Deliver the smallest docs-only invariant-audit slice that closes roadmap item 2 by writing one thesis/solver risk audit for automatic recursive-type inference, while preserving takeover continuity and the explicit-only / non-equi-recursive / non-cyclic-graph boundary.

## Scope Guardrails

- This round is docs-only.
- Do not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Do not edit `orchestrator/roadmap.md` in this round.
- Do not implement solver or inference behavior changes.
- Do not rewrite predecessor authoritative packet history under `tasks/todo/2026-03-11-recursive-types-orchestration/`.
- Preserve the explicit-only / non-equi-recursive / non-cyclic-graph boundary as mandatory current behavior.

## Target Files

1. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-002/docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md`
2. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-002/orchestrator/rounds/round-002/implementation-notes.md`

## Sequential Tasks

### Task 1 — Write the bounded invariant audit document

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-002/docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md`

Required contents:
- State inherited baseline from item 1: automatic recursive-type inference remains unresolved; explicit-only recursive behavior is the active boundary.
- Audit exactly the roadmap-item-2 threat classes and no implementation plan:
  - acyclicity assumptions (graph shape, no cyclic term-DAG encoding),
  - binding/tree discipline obligations,
  - occurs-check and unification termination risks,
  - reconstruction/reification/witness obligations,
  - principality and termination risk boundaries.
- For each threat class, record:
  - concrete invariants currently relied on,
  - specific modules/files where those invariants are enforced or consumed,
  - what would be threatened by automatic recursive inference,
  - bounded proof obligations any future spike must satisfy before code changes.
- Include a strict boundaries section that explicitly forbids:
  - equi-recursive reasoning,
  - cyclic graph encodings,
  - silent widening from explicit annotations to inferred recursion.
- Include predecessor continuity references (cite as evidence only, no mutation):
  - `tasks/todo/2026-03-11-recursive-types-orchestration/findings.md`
  - `tasks/todo/2026-03-11-recursive-types-orchestration/task_plan.md`
  - `docs/plans/2026-03-13-m6-pipeline-feasibility-spike.md`
  - `docs/plans/2026-03-13-m7-tymu-design-resolution.md`

### Task 2 — Record round implementation notes

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-002/orchestrator/rounds/round-002/implementation-notes.md`

Required contents:
- Short summary of the audit artifact and why it satisfies roadmap item 2.
- Explicit statement that the round is docs-only with no production-code edits.
- Explicit statement that predecessor packet history was referenced, not rewritten.

## Acceptance Criteria (Round Must Meet All)

1. `docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md` exists and covers all five required threat classes.
2. The audit names concrete invariant obligations and module/file ownership for each threat class.
3. The audit preserves and explicitly restates the explicit-only / non-equi-recursive / non-cyclic-graph boundary.
4. The audit defines bounded proof obligations for any later feasibility spike and does not include implementation steps.
5. `orchestrator/rounds/round-002/implementation-notes.md` exists and records docs-only execution plus continuity handling.
6. Diff remains limited to planned docs/round artifacts and does not modify production code, `orchestrator/roadmap.md`, or predecessor authoritative logs.

## Reviewer And Verification Checks

Baseline checks (from `orchestrator/verification.md`):
- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmap.md`
- Reviewer-recorded guidance sync check for `AGENTS.md`, `tasks/readme`, `TODO.md`, `CHANGELOG.md`, and `implementation_notes.md` (updated or intentionally unchanged, with rationale).

Round-specific checks:
- Docs-only boundary:
  - `git diff --name-only`
  - `git diff --name-only | rg '^(src/|src-public/|app/|test/|mlf2\\.cabal$)'` (expected: no matches)
- Roadmap/predecessor immutability:
  - `git diff --name-only | rg '^orchestrator/roadmap\\.md$'` (expected: no matches)
  - `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'` (expected: no matches)
- Audit-content presence:
  - `rg -n 'acyclic|binding|occurs-check|termination|reif|reconstruct|principality|explicit-only|non-equi-recursive|non-cyclic' docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md`
- Evidence references present:
  - `rg -n '2026-03-11-recursive-types-orchestration|2026-03-13-m6-pipeline-feasibility-spike|2026-03-13-m7-tymu-design-resolution' docs/plans/2026-03-14-automatic-recursive-inference-invariant-audit.md`

Review output requirements:
- Reviewer writes `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-002/orchestrator/rounds/round-002/review.md` with:
  - baseline checks evidence,
  - round-specific checks evidence,
  - explicit decision (`approve` or `reject`),
  - takeover continuity statement confirming predecessor history was referenced and left immutable.
