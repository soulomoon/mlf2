# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace/conflict-marker damage, including docs-only rounds.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must stay valid JSON after every round.
- Command: `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmaps/2026-03-14-00-automatic-recursive-type-inference-research-roadmap/rev-004/roadmap.md`
  Why: the roadmap must keep a parseable ordered item list with explicit status markers.
- Command: `cabal build all && cabal test`
  Why: this repo’s full verification gate is required whenever a round touches `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Command: reviewer-recorded guidance sync check
  Why: when a round changes workflow/control-plane docs, the reviewer must record whether `AGENTS.md`, `tasks/readme`, `TODO.md`, `CHANGELOG.md`, and `implementation_notes.md` were updated or intentionally left unchanged.

## Task-Specific Checks

- Add focused checks that are specific to the selected round, such as:
- theory/roadmap acceptance-gate consistency for research-only rounds;
- source/path checks proving predecessor packet history is referenced rather than silently rewritten;
- focused Hspec selectors or prototype commands for bounded feasibility spikes;
- docs-diff review when a round intentionally changes only `orchestrator/`, `docs/`, or task artifacts.

## Approval Criteria

- Every baseline check passes.
- Every task-specific check passes.
- `review.md` records evidence for the round.
- The reviewer decision is explicit.
- The round preserves takeover continuity: predecessor recursive-types packet truth remains intact unless the round explicitly updates its human-facing summaries without rewriting the old authoritative log.

## Reviewer Record Format

### Round `<round-id>`

- Baseline checks:
- Task-specific checks:
- Decision:
- Evidence:
