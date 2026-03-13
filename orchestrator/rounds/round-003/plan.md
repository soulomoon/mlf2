# Round 003 Plan (Roadmap Item 3)

## Round Objective

Deliver the smallest docs-only slice that closes roadmap item 3 by selecting exactly one bounded automatic-recursive-inference candidate subset and writing a verifier-visible research plan (including spike success/failure gates), while preserving the explicit-only / non-equi-recursive / non-cyclic-graph boundary.

## Scope Guardrails

- This round is docs-only.
- Do not edit `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Do not edit `orchestrator/roadmap.md` in this round.
- Do not implement solver or inference behavior changes.
- Do not rewrite predecessor authoritative packet history under `tasks/todo/2026-03-11-recursive-types-orchestration/`.
- Preserve the explicit-only / non-equi-recursive / non-cyclic-graph boundary as mandatory current behavior.

## Chosen Candidate Subset (Exactly One)

- **Candidate ID:** `ARI-C1`
- **Name:** Annotation-anchored recursive-shape propagation (single SCC, single recursive binder family)
- **Boundary definition:**
  - Research target is limited to cases where recursion is already anchored by an explicit recursive annotation path.
  - No fully unannotated recursive SCC inference is in scope.
  - No equi-recursive equality/unfolding and no cyclic graph encoding are in scope.
  - Any future spike for this candidate must remain opt-in and evidence-driven, not silently enabled.

## Target Files

1. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-003/docs/plans/2026-03-14-automatic-recursive-inference-candidate-subset-selection.md`
2. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-003/orchestrator/rounds/round-003/implementation-notes.md`

## Sequential Tasks

### Task 1 — Write candidate-subset selection + bounded research-plan artifact

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-003/docs/plans/2026-03-14-automatic-recursive-inference-candidate-subset-selection.md`

Required contents:
- Inherited-baseline restatement from items 1 and 2 (automatic recursive inference unresolved; explicit-only boundary still active).
- A **single selected candidate** section naming `ARI-C1` and giving precise inclusion/exclusion boundaries.
- A deferred/rejected alternatives section that explicitly marks at least:
  - broad fully-unannotated recursive inference as deferred/rejected for this roadmap stage;
  - any equi-recursive or cyclic-graph representation option as rejected for this stage.
- A bounded spike research plan section for roadmap item 4 that includes verifier-visible gates:
  - preconditions required before spike execution,
  - concrete success evidence,
  - concrete failure/no-go triggers,
  - explicit “stop” conditions if boundaries drift.
- A verification-facing checklist/table that reviewers can inspect without inferring intent.
- Continuity references to prior accepted item-1/item-2 artifacts without mutating predecessor history.

### Task 2 — Record round implementation notes

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-003/orchestrator/rounds/round-003/implementation-notes.md`

Required contents:
- Short summary of the selected subset and why it satisfies roadmap item 3.
- Explicit statement that exactly one candidate (`ARI-C1`) was selected and alternatives were deferred/rejected.
- Explicit statement that the round is docs-only with no production-code edits.
- Explicit statement that predecessor packet history was referenced only and not rewritten.

## Acceptance Criteria (Round Must Meet All)

1. `docs/plans/2026-03-14-automatic-recursive-inference-candidate-subset-selection.md` exists and selects exactly one candidate subset (`ARI-C1`).
2. The document explicitly defers/rejects alternatives and does not leave multiple active candidate subsets.
3. The document includes verifier-visible spike success/failure gates for the next bounded feasibility spike.
4. The document explicitly preserves explicit-only / non-equi-recursive / non-cyclic-graph boundaries.
5. `orchestrator/rounds/round-003/implementation-notes.md` exists and accurately records docs-only execution and continuity handling.
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
- Single-candidate check:
  - `rg -n '^Candidate ID:\\s*`ARI-C[0-9]+`' docs/plans/2026-03-14-automatic-recursive-inference-candidate-subset-selection.md | wc -l` (expected: `1`)
  - `rg -n 'ARI-C1|chosen candidate|deferred|rejected' docs/plans/2026-03-14-automatic-recursive-inference-candidate-subset-selection.md`
- Boundary and spike-gate presence:
  - `rg -n 'explicit-only|non-equi-recursive|non-cyclic-graph|success|failure|no-go|stop condition|verifier' docs/plans/2026-03-14-automatic-recursive-inference-candidate-subset-selection.md`

Review output requirements:
- Reviewer writes `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-003/orchestrator/rounds/round-003/review.md` with:
  - baseline checks evidence,
  - round-specific checks evidence,
  - explicit decision (`approve` or `reject`),
  - takeover continuity statement confirming predecessor history was referenced and left immutable.
