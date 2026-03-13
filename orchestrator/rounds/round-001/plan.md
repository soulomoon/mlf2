# Round 001 Plan (Roadmap Item 1)

## Round Objective

Deliver the smallest docs-only slice that closes roadmap item 1 by writing a single inherited-baseline + acceptance-contract document for automatic recursive-type inference, while preserving predecessor milestone truth and the explicit-only / non-equi-recursive boundary.

## Scope Guardrails

- This round is docs-only.
- Do not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Do not change `orchestrator/roadmap.md` in this round.
- Do not reopen predecessor campaign milestones as pending.
- Do not plan or imply solver-behavior implementation, inference widening, or a feasibility spike.

## Target Files

1. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-001/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
2. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-001/orchestrator/rounds/round-001/implementation-notes.md`

## Sequential Tasks

### Task 1 — Write inherited baseline and scope contract document

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-001/docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`

Required contents:
- A baseline section that explicitly states inherited truth:
  - explicit-only acyclic `TyMu` support is complete;
  - automatic recursive-type inference remains unresolved/disabled;
  - non-equi-recursive and non-cyclic-graph boundaries remain in force.
- A “what automatic recursive-type inference means in this repo” section with concrete in-scope behavior definition and explicit exclusions.
- A non-goals section that forbids:
  - solver-wide recursive inference implementation in this round,
  - equi-recursive equality,
  - cyclic graph encoding,
  - silent boundary widening from explicit annotations to inferred recursion.
- An evidence-before-spike acceptance contract section defining mandatory prerequisites before any later code-changing spike is allowed (docs/invariant audit/approval gates only; no implementation here).
- A continuity section citing predecessor evidence sources (without modifying their historical records):
  - `tasks/todo/2026-03-11-recursive-types-orchestration/`
  - `docs/plans/2026-03-11-recursive-types-roadmap.md`
  - `docs/plans/2026-03-13-m6-pipeline-feasibility-spike.md`
  - `docs/plans/2026-03-13-m7-tymu-design-resolution.md`

### Task 2 — Record round implementation notes

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-001/orchestrator/rounds/round-001/implementation-notes.md`

Required contents:
- Short summary of what was added and why it satisfies roadmap item 1.
- Explicit statement that this round made no production-code changes.
- List of predecessor artifacts referenced for continuity.

## Acceptance Criteria (Round Must Meet All)

1. The new contract doc exists and includes all required sections from Task 1.
2. The doc clearly preserves the explicit-only / non-equi-recursive boundary and states that automatic recursive-type inference is still unresolved.
3. The contract defines concrete evidence gates that must be satisfied before any future code-changing spike.
4. `implementation-notes.md` exists and accurately summarizes docs-only execution.
5. Diff is limited to planned docs/round artifacts and does not modify production code or predecessor authoritative logs.

## Reviewer And Verification Checks

Baseline checks (from `orchestrator/verification.md`):
- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmap.md`
- Reviewer-recorded guidance sync check for `AGENTS.md`, `tasks/readme`, `TODO.md`, `CHANGELOG.md`, and `implementation_notes.md` (updated or intentionally unchanged, with rationale).

Round-specific checks:
- Docs-only diff boundary:
  - `git diff --name-only`
  - `git diff --name-only | rg '^(src/|src-public/|app/|test/|mlf2\\.cabal$)'` (expected: no matches)
- Contract-content presence:
  - `rg -n 'inherited baseline|automatic recursive-type inference|non-goals|acceptance contract|explicit-only|non-equi-recursive|acyclic' docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- Continuity references present:
  - `rg -n '2026-03-11-recursive-types-orchestration|2026-03-11-recursive-types-roadmap|2026-03-13-m6-pipeline-feasibility-spike|2026-03-13-m7-tymu-design-resolution' docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`

Review output requirements:
- Reviewer writes `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-001/orchestrator/rounds/round-001/review.md` with:
  - baseline checks evidence,
  - round-specific checks evidence,
  - explicit decision (`approve` or `reject`),
  - takeover continuity statement confirming predecessor history was referenced, not rewritten.
