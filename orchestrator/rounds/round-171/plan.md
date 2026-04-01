# Round 171 Plan

- Round: `round-171`
- Roadmap: `2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap` / `rev-001`
- Item: `item-3`
- Retry: `attempt-2` (`provenance gaps` around merged-commit attribution for `45d765b`)
- Execution shape: docs-only, item-3-only, serial, citation-rebind only

## Objective

Revise the existing item-3 settlement publication so the merged-commit
provenance for `45d765b` is bound only to accepted sources that actually
record that commit. Keep everything else fixed: the settled packet remains
`sameLaneAliasFrameClearBoundaryExpr` only, the exact post-item-2
`narrow success` read stays unchanged, the focused/full-gate evidence remains
predecessor-only round-170 provenance, and the round must not expand into
item-4, new implementation work, or source/test edits.

## Write Scope

Implementer-owned writes for retry attempt `2` are limited to:

- `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md`
- `orchestrator/rounds/round-171/implementation-notes.md`

Do not modify `selection.md`, `review.md`, `orchestrator/state.json`,
`orchestrator/roadmaps/**`, `orchestrator/rounds/round-170/**`, `src/`,
`src-public/`, `app/`, `test/`, `mlf2.cabal`, `TODO.md`, `Bugs.md`, or any
item-4 artifact.

## Sequential Plan

1. Rebind the merged-commit citation in
   `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md`
   only.
   - Update the artifact header to `Attempt: attempt-2` and reflect the live
     retry state for this round.
   - In `## Authority Ledger`, replace
     `orchestrator/rounds/round-170/merge.md` as the source for `Accepted merged provenance`
     with
     `orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md`.
   - Bind that row to the exact roadmap completion note that says item `2`
     was accepted in `round-170` and merged as commit `45d765b`
     (`Preserve recursive output for frozen alias-frame packet`).
   - Keep `orchestrator/rounds/round-170/implementation-notes.md`,
     `orchestrator/rounds/round-170/review.md`, and
     `orchestrator/rounds/round-170/review-record.json` as the only round-170
     sources for the bounded packet outcome plus focused/full-gate evidence.

2. Rewrite the provenance narrative, but not the packet read, in that same
   settlement artifact.
   - In `## Provenance Validation`, remove any sentence that attributes
     `45d765b` to `orchestrator/rounds/round-170/merge.md`.
   - Restate the provenance chain as:
     item-1 freeze ->
     round-170 implementation notes ->
     round-170 approved review and review record ->
     active roadmap item-2 completion notes recording merged commit `45d765b`.
   - Preserve the exact settled packet read verbatim in substance:
     `sameLaneAliasFrameClearBoundaryExpr` preserves recursive output on both
     `runPipelineElab` and `runPipelineElabChecked` within the inherited
     current architecture.
   - Preserve all non-claims verbatim in substance:
     no broader `P3` / `P4` / `P6` settlement, no repo-level readiness,
     no item-4 outcome, no successor decision, and no handoff.

3. Align the round-local summary with that corrected authority split in
   `orchestrator/rounds/round-171/implementation-notes.md`.
   - Keep the file docs-only and round-local.
   - State explicitly that focused reruns and the accepted full gate still come
     from round-170 approved artifacts, while merged-commit provenance for
     `45d765b` is carried from the active roadmap item-2 completion notes.
   - Do not add any claim that round-171 reran focused tests, reran
     `cabal build all && cabal test`, or produced a new merge artifact.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `git rev-parse --verify 45d765b^{commit} >/dev/null`
- `git merge-base --is-ancestor 45d765b codex/automatic-recursive-type-inference`
- `python3 - <<'PY'\nimport pathlib, sys\nroot = pathlib.Path('.')\nartifact = (root / 'docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md').read_text()\nnotes171 = (root / 'orchestrator/rounds/round-171/implementation-notes.md').read_text()\nroadmap = (root / 'orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md').read_text()\nmerge170 = (root / 'orchestrator/rounds/round-170/merge.md').read_text()\nrequired_artifact_tokens = [\n    'Attempt: `attempt-2`',\n    'Retry state:',\n    '`orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md`',\n    'merged as commit `45d765b`',\n    'sameLaneAliasFrameClearBoundaryExpr',\n    'runPipelineElab',\n    'runPipelineElabChecked',\n    '1303 examples, 0 failures',\n    'P3', 'P4', 'P6', 'repo-level readiness', 'item-4', 'successor decision', 'handoff',\n]\nfor token in required_artifact_tokens:\n    if token not in artifact:\n        print(f'artifact missing token: {token}')\n        sys.exit(1)\nif '| Accepted merged provenance | `orchestrator/rounds/round-170/merge.md` |' in artifact:\n    print('artifact still attributes accepted merged provenance to round-170/merge.md')\n    sys.exit(1)\nif 'round-170 merge at `45d765b`' in artifact:\n    print('artifact still narrates 45d765b as a round-170 merge artifact')\n    sys.exit(1)\nif 'Completion notes: accepted in `round-170`, merged as commit `45d765b`' not in roadmap:\n    print('roadmap.md does not record the accepted merged commit')\n    sys.exit(1)\nif '45d765b' in merge170:\n    print('round-170/merge.md unexpectedly records 45d765b; review the authority split')\n    sys.exit(1)\nfor token in [\n    'round-170 approved artifacts',\n    '45d765b',\n    'roadmap item-2 completion notes',\n    'docs-only',\n]:\n    if token not in notes171:\n        print(f'implementation-notes missing token: {token}')\n        sys.exit(1)\nprint('MERGED_COMMIT_PROVENANCE_REBOUND_OK')\nPY`
- `rg -n '45d765b|roadmap item-2 completion notes|orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md|orchestrator/rounds/round-170/merge.md|sameLaneAliasFrameClearBoundaryExpr|runPipelineElab|runPipelineElabChecked|1303 examples, 0 failures|P3|P4|P6|repo-level readiness|item-4|successor decision|handoff' docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md orchestrator/rounds/round-171/implementation-notes.md orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md`
- `git diff --check`
- `python3 - <<'PY'\nimport subprocess, sys\nallowed = {\n    'docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md',\n    'orchestrator/rounds/round-171/implementation-notes.md',\n    'orchestrator/rounds/round-171/plan.md',\n    'orchestrator/rounds/round-171/selection.md',\n}\ntracked = subprocess.check_output(['git', 'diff', '--name-only', 'codex/automatic-recursive-type-inference...HEAD'], text=True).splitlines()\nuntracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines()\npaths = [p for p in tracked + untracked if p]\nextra = [p for p in paths if p not in allowed]\nforbidden = [\n    p for p in paths\n    if p == 'mlf2.cabal' or p.startswith('src/') or p.startswith('src-public/') or p.startswith('app/') or p.startswith('test/')\n]\nif forbidden or extra:\n    if forbidden:\n        print('FORBIDDEN_PATHS:')\n        print('\\n'.join(forbidden))\n    if extra:\n        print('OUT_OF_SCOPE_PATHS:')\n        print('\\n'.join(extra))\n    sys.exit(1)\nprint('ROUND171_RETRY2_DOCS_ONLY_SCOPE_OK')\nfor p in paths:\n    print(p)\nPY`
