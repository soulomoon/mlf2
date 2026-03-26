# Round 118 Implementation Notes

Date: 2026-03-27
Round: `round-118`
Roadmap item: `item-2`
Stage: `implement`

## Change Summary

- Wrote the canonical refreshed repo-scope representative family-matrix
  settlement surface at
  `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`.
- Wrote the lane-local `C1` provenance summary and the lane-local `P5`
  provenance summary after isolated focused reruns.
- Kept the accepted same-lane `C2` / `C5` / `C7` pocket closed and carried it
  forward only from the accepted rev-004 settlement docs.

## Parallel Execution Summary

- `C1` provenance lane
  write scope: `orchestrator/rounds/round-118/lanes/c1-provenance-summary.md`
- `P5` provenance lane
  write scope: `orchestrator/rounds/round-118/lanes/p5-provenance-summary.md`
- aggregate synthesis owner
  write scope:
  `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`
  and `orchestrator/rounds/round-118/implementation-notes.md`
- Merge discipline:
  both provenance lanes wrote only their own scratch summaries, both focused
  reruns used isolated build dirs, and aggregate synthesis consolidated those
  results only after both lane reruns passed. No lane edited
  `orchestrator/state.json`, the roadmap bundle, prior accepted docs, or the
  other lane's file. The isolated build outputs were moved out of the round
  worktree after capture so the round leaves only the authorized artifact set
  behind.

## Focused Provenance Evidence

- `test -f test/Research/C1AuthoritativeSurfaceSpec.hs` -> `pass`
- `cabal test mlf2-test --builddir=dist-newstyle-round118-c1 --test-show-details=direct --test-options='--match "C1 authoritative-surface harness"'`
  -> `pass` (`2 examples, 0 failures`)
- `test -f test/Research/P5ClearBoundarySpec.hs` -> `pass`
- `cabal test mlf2-test --builddir=dist-newstyle-round118-p5 --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
  -> `pass` (`2 examples, 0 failures`)

Both cited harnesses exist in the round worktree, and both focused reruns used
isolated build outputs rather than shared `dist-newstyle`.

## Baseline Verification

This section records the exact baseline verification commands from
`verification.md` plus the docs-only full-gate outcome.

- `git diff --check` -> `pass`
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> `pass`
- `rg -n '"contract_version": 2|"roadmap_id":|"roadmap_revision":|"roadmap_dir":|"retry": null|"retry": \{' orchestrator/state.json`
  -> `pass`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
  -> `pass`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n '^\d+\. \[(pending|in-progress|done)\]' "$roadmap_dir/roadmap.md"`
  -> `pass`
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  -> `pass`
- `test -f docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
  -> `pass`
- `test -f docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-post-settlement-same-family-handoff-decision.md`
  -> `pass`
- `test -f docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
  -> `pass`
- `git diff --name-only -- src src-public app test mlf2.cabal Bugs.md`
  -> no output
- `if git diff --name-only -- src src-public app test mlf2.cabal | grep -q .; then cabal build all && cabal test; else printf 'skip full cabal gate for docs-only round\n'; fi`
  -> `skip full cabal gate for docs-only round`

No `src/`, `src-public/`, `app/`, `test/`, `mlf2.cabal`, or `Bugs.md` paths
entered the round diff. The only non-round-local modified paths visible in
`git status --short` remain the preexisting controller-owned
`orchestrator/state.json` and roadmap updates.
