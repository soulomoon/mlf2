# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker damage, including docs-only research rounds.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON after every round.
- Command: `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmaps/2026-03-14-02-uri-r2-c1-re-entry-evidence-successor-roadmap/rev-004/roadmap.md`
  Why: the re-entry roadmap must keep a parseable ordered item list with explicit status markers.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Command: reviewer-recorded continuity check against inherited evidence
  Why: each round must record whether it preserved completed rounds `001` through `010`, the accepted `R5` stop decision, the approved re-entry design spec, and the predecessor recursive-types packet.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- provenance-authority checks proving `RE1` does not manufacture replay/reification authority, late repair, or widened provenance roots for `URI-R2-C1`;
- uniqueness checks proving `RE2` does not rely on heuristic ranking, competing roots, or widening-dependent comparisons;
- prototype-free positive-evidence checks proving `RE3` does not rely on prototype runs, implementation drift, or widened ownership/search while staying inside the fixed `URI-R2-C1` boundary;
- re-entry-gate checks proving `RE4` records explicit `reopen-handoff-track` or `not-yet-reopen` evidence without broadening the active subject;
- final-recommendation checks proving `RE5` matches the accepted `RE4` result and does not silently become implementation clearance;
- docs-diff review when a round intentionally changes only `orchestrator/`, `docs/`, or task artifacts.

## Approval Criteria

- Every baseline check passes.
- Every task-specific check passes.
- `review.md` records evidence for the round.
- The reviewer decision is explicit.
- The round preserves continuity with inherited evidence unless the plan explicitly authorized a human-facing summary update without rewriting old authoritative records.

## Reviewer Record Format

### Round `<round-id>`

- Baseline checks:
- Task-specific checks:
- Decision:
- Evidence:
