# Round 004 Plan (Roadmap Item 4, Candidate `ARI-C1`)

## Round Objective

Execute the smallest feasible `ARI-C1` spike that produces concrete, reviewer-verifiable feasibility evidence for roadmap item 4 while keeping boundaries unchanged: **explicit-only ingress**, **non-equi-recursive semantics**, and **non-cyclic graph representation**.

## Spike Mode (Explicitly Bounded)

This round should be **prototype-only + docs**, not a solver implementation round.

- Allowed: docs artifact updates and tightly scoped test-level prototype evidence.
- Not allowed: production behavior changes in recursive inference, default-on feature paths, or broad solver rewrites.

Rationale: this is the safest slice that can still produce concrete feasibility/no-go evidence for `ARI-C1` without silently widening scope.

## Scope Guardrails

- Do not edit `orchestrator/roadmap.md`.
- Do not edit predecessor packet history under `tasks/todo/2026-03-11-recursive-types-orchestration/`.
- Do not add equi-recursive reasoning or unfolding semantics.
- Do not encode recursion as cyclic structural graph edges.
- Do not enable fully unannotated recursive-type synthesis.
- Keep any prototype logic test-local and non-default.

## Exact Target Files

1. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-004/docs/plans/2026-03-14-automatic-recursive-inference-ari-c1-feasibility-spike.md`
2. Update `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-004/test/PipelineSpec.hs`
3. Create `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-004/orchestrator/rounds/round-004/implementation-notes.md`

## Sequential Tasks

### Task 1 — Write the `ARI-C1` feasibility evidence document

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-004/docs/plans/2026-03-14-automatic-recursive-inference-ari-c1-feasibility-spike.md`

Required contents:
- Restate inherited constraints from items 1-3 and quote the active boundary verbatim: explicit-only / non-equi-recursive / non-cyclic-graph.
- Define the exact spike hypothesis for `ARI-C1`: annotation-anchored recursive-shape propagation feasibility under one recursive binder family and one SCC.
- Include a small experiment matrix with:
  - in-scope anchored example(s),
  - intentionally out-of-scope example(s) (fully unannotated or cross-family/multi-SCC),
  - expected pass/fail outcomes and why each outcome is boundary-safe.
- Record explicit success, no-go, and stop outcomes for this round’s evidence run.
- Include a “no hidden widening” checklist tied to concrete artifacts (tests + diff checks).

### Task 2 — Add bounded prototype evidence in existing pipeline spec

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-004/test/PipelineSpec.hs`

Required change shape:
- Add one new `describe` block named for `ARI-C1` feasibility.
- Add characterization tests that demonstrate all of the following:
  - An annotation-anchored recursive shape case remains processable (in-scope anchor path).
  - A corresponding unannotated variant does **not** become inferred automatically.
  - An out-of-scope case (multi-family/cross-SCC or equivalent proxy) is rejected or remains unresolved under current boundary.
- Keep helpers local to this block unless already shared in the file.
- Do not alter pipeline defaults, solver strategy wiring, or exported APIs.

### Task 3 — Record round implementation notes

Target file:
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-004/orchestrator/rounds/round-004/implementation-notes.md`

Required contents:
- What evidence was gathered and whether it supports “feasible” vs “no-go/not-yet-go” for `ARI-C1`.
- Exact statement that this was prototype-only + docs (no production recursive-inference enablement).
- Explicit continuity statement: predecessor packet truth was referenced, not rewritten.

## Acceptance Criteria (All Required)

1. `docs/plans/2026-03-14-automatic-recursive-inference-ari-c1-feasibility-spike.md` exists and contains hypothesis, experiment matrix, and explicit success/no-go/stop outcomes for `ARI-C1`.
2. `test/PipelineSpec.hs` contains a bounded `ARI-C1` test block proving anchored in-scope behavior and non-widening for unannotated/out-of-scope cases.
3. No edits introduce equi-recursive behavior, cyclic graph encoding, or automatic unannotated recursive-type inference.
4. `orchestrator/rounds/round-004/implementation-notes.md` exists and records feasibility decision evidence plus continuity handling.
5. Diff is limited to planned files and does not modify `orchestrator/roadmap.md` or predecessor authoritative logs.

## Reviewer And Verification Checks

Baseline checks (from `orchestrator/verification.md`):
- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmap.md`
- Since `test/` is touched: `cabal build all && cabal test`
- Reviewer-recorded guidance sync check for `AGENTS.md`, `tasks/readme`, `TODO.md`, `CHANGELOG.md`, and `implementation_notes.md` (updated or intentionally unchanged, with rationale).

Round-specific checks:
- Planned-file boundary:
  - `git diff --name-only`
  - `git diff --name-only | rg '^orchestrator/roadmap\\.md$'` (expected: no matches)
  - `git diff --name-only | rg '^tasks/todo/2026-03-11-recursive-types-orchestration/'` (expected: no matches)
- `ARI-C1` scope markers present in docs/tests:
  - `rg -n 'ARI-C1|explicit-only|non-equi-recursive|non-cyclic-graph|single SCC|single recursive binder family|no-go|stop' docs/plans/2026-03-14-automatic-recursive-inference-ari-c1-feasibility-spike.md test/PipelineSpec.hs`
- Non-widening evidence present:
  - `rg -n 'unannotated|out-of-scope|reject|not inferred|fails|boundary' docs/plans/2026-03-14-automatic-recursive-inference-ari-c1-feasibility-spike.md test/PipelineSpec.hs`
- Focused spike test execution evidence:
  - `cabal test --test-show-details=direct --test-options='--match "ARI-C1"'`

Review output requirements:
- Reviewer writes `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-004/orchestrator/rounds/round-004/review.md` with:
  - baseline check evidence,
  - task-specific check evidence,
  - explicit decision (`approve` or `reject`),
  - explicit statement whether the round outcome is `feasible-continue` or `no-go/not-yet-go` for item 5 handoff.
