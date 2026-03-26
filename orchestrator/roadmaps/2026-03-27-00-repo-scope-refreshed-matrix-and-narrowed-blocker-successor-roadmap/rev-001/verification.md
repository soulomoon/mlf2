# Verification Contract

## Baseline Checks

- Command: `git diff --check`
  Why: every round must leave the tree free of whitespace and conflict-marker
  damage.
- Command: `python3 -m json.tool orchestrator/state.json >/dev/null`
  Why: `orchestrator/state.json` is machine state and must remain valid JSON
  after every round.
- Command: `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
  Why: the live controller must preserve the v2 retry schema and the active
  roadmap locator contract.
- Command: `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
  Why: the live control plane must resolve one authoritative roadmap bundle
  through `orchestrator/state.json`.
- Command: `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
  Why: the active roadmap bundle must keep a parseable ordered item list with
  explicit status markers.
- Command: `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  Why: the inherited explicit-only / iso-recursive / non-equi-recursive /
  no-fallback baseline remains the live boundary.
- Command: `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  Why: the repo-level success bar and representative family matrix remain
  binding predecessor authority.
- Command: `test -f docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-settlement-same-family-handoff-decision.md`
  Why: accepted rev-004 handoff is the direct authority that closes the
  exact-pocket settlement lane and forces this successor family to stay
  repo-scope.
- Command: `test -f docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
  Why: the March 26 global gate remains historical aggregate evidence that
  the refreshed repo-scope family must reinterpret honestly rather than erase.
- Conditional command: `if git diff --name-only -- src src-public app test mlf2.cabal | grep -q .; then cabal build all && cabal test; else printf 'skip full cabal gate for docs-only round\n'; fi`
  Why: code-changing rounds must satisfy the repo's full Cabal gate, while
  docs-only rounds must record an explicit skip rather than silently omitting
  it.

## Task-Specific Checks

- successor-boundary checks proving item `1` freezes the post-rev-004
  predecessor authority chain, keeps the same-lane `C2` / `C5` / `C7` pocket
  settled, and treats local planning packets or research harnesses as
  non-authoritative until republished in round-owned artifacts;
- refreshed-matrix checks proving item `2` carries forward the accepted
  same-lane repaired read honestly, records current reads for the remaining
  rows, validates provenance for any new `C1` / `P5` carry-forward, and
  preserves the older March 26 matrix and gate as immutable historical
  evidence;
- conditional evidence checks: if item `2` cites
  `test/Research/C1AuthoritativeSurfaceSpec.hs` or
  `test/Research/P5ClearBoundarySpec.hs`, verify the file exists and any cited
  focused Cabal evidence was rerun under serialized or isolated build-output
  conditions;
- successor-gate checks proving item `3` records exactly one live posture and
  exactly one next move without reusing the old March 26 keep-vs-reopen
  framing unchanged;
- parallel-agent discipline checks proving that any round using multiple
  subagents identified bounded independent sub-slices, assigned disjoint
  write scopes, kept one authoritative owner per writable file, and recorded
  the consolidation clearly in `plan.md`, `implementation-notes.md`,
  `review.md`, and a `Parallel execution summary`;
- build-isolation checks proving focused Cabal evidence was serialized or
  isolated by build dir; shared `dist-newstyle` parallel runs do not count as
  domain evidence;
- roadmap-update checks proving completed-item truth remains immutable, the
  next unfinished item stays concrete, and no revision rewrite occurs after a
  round has used that revision; and
- docs-diff review when a round intentionally changes only `orchestrator/`
  and `docs/`, plus an explicit skip note when the full Cabal gate is not
  required.

## Approval Criteria

- Every baseline check passes, or an omitted conditional check is explicitly
  justified in `review.md`.
- Every task-specific check required by the selected stage passes.
- `review.md` records commands, evidence, `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- `selection.md` and `review-record.json` preserve the active
  `roadmap_id`, `roadmap_revision`, and `roadmap_dir`.
- No unresolved blocking issue remains within the selected stage contract; any
  remaining repo-scope blocker debt is explicitly recorded.

## Reviewer Record Format

- Commands run
- Pass or fail result
- Evidence summary
- `Implemented stage result`
- `Attempt verdict`
- `Stage action`
- `Retry reason`
- `Fix hypothesis`
- Approve or reject decision
