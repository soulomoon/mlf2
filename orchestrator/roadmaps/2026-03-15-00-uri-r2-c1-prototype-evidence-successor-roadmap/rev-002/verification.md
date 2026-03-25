# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker damage, including docs-only research rounds.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON after every round.
- Command: `rg -n '^\d+\\. \\[(pending|in-progress|done)\\]' orchestrator/roadmaps/2026-03-15-00-uri-r2-c1-prototype-evidence-successor-roadmap/rev-002/roadmap.md`
  Why: the prototype-evidence roadmap must keep a parseable ordered item list with explicit status markers.
- Command: `test -f docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`
  Why: every round in this campaign is anchored to the approved prototype-evidence design.
- Command: `cabal build all && cabal test`
  Why: the full repo gate is mandatory whenever a round touches `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- Command: reviewer-recorded continuity check against predecessor evidence
  Why: each round must record whether it preserved completed rounds `001` through `015`, the accepted prototype-free `RE4` and `RE5` stop results, the approved prototype-evidence design spec, and the predecessor recursive-types packet.

## Task-Specific Checks

- Add round-specific checks required by the selected roadmap item, especially:
- shared-entrypoint isolation checks proving prototype evidence enters only through `uri-r2-c1-prototype-entrypoint-v1` and does not create a second executable interface;
- bounded-scenario checks proving the active scenario remains exactly `uri-r2-c1-only-v1` and does not widen beyond `URI-R2-C1`;
- `P1` checks proving candidate discovery and normalization produce either one canonical subject token or an explicit bounded negative result without heuristic ranking, widened search, or late repair;
- `P2` checks proving the exact `P1` token survives the bounded replay or reification path with one shared `correlation_id`, or else records the bounded loss-of-identity failure explicitly;
- `P3` checks proving the exact carried subject stays inside one local SCC, one acyclic binder-mediated structural slice, one deterministic single-family ownership account, and one constructor-directed reasoning path, or else records the bounded failing obligation explicitly;
- `P4` checks proving the final decision is exactly `reopen-handoff-track` or `hard-stop` and matches the accumulated `P1` through `P3` stage results and retry-budget outcomes;
- docs-diff review when a round intentionally changes only `orchestrator/`, `docs/`, or task artifacts;
- skip-note review when a round does not trigger the full Cabal gate, including the exact reason the reviewer judged the code-path gate out of scope.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, and an explicit approve or reject decision.
- The round preserves prototype isolation, bounded-scenario identity, and predecessor-evidence continuity.
- No unresolved blocking issue remains.

## Reviewer Record Format

### Round `<round-id>`

- Baseline checks:
- Task-specific checks:
- Stage result:
- Decision:
- Evidence:
