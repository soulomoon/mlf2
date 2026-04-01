# Round 171 Implementation Notes

## Summary

- published the canonical item-3 settlement artifact:
  `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md`
- republished the exact post-item-2 narrow-success read for
  `sameLaneAliasFrameClearBoundaryExpr` only:
  recursive output is preserved on both `runPipelineElab` and
  `runPipelineElabChecked` within the inherited current architecture
- kept the accepted round-170 approved artifacts as the provenance source for:
  focused reruns for `sameLaneAliasFrameClearBoundaryExpr`,
  `alias-frame clear-boundary packet`, and `retained-child`;
  accepted full-gate evidence `cabal build all && cabal test` with
  `1303 examples, 0 failures`
- rebound merged commit `45d765b` to roadmap item-2 completion notes in
  `orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md`;
  round-170 approved artifacts still carry the bounded packet outcome and
  focused/full-gate evidence, while the active roadmap completion notes carry
  the accepted merged provenance
- recorded the exact repo-impact read as one settled packet only and left
  broader `P3` / `P4` / `P6`, repo-level readiness, item-4, successor
  decision, and handoff questions unresolved
- kept round-171 docs-only and round-owned only; no `src/`, `src-public/`,
  `app/`, `test/`, or `mlf2.cabal` edits were introduced

## Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `python3 - <<'PY'
import json, pathlib, sys
root = pathlib.Path('.')
state = json.loads((root / 'orchestrator/state.json').read_text())
expected = [state['roadmap_id'], state['roadmap_revision'], state['roadmap_dir']]
for rel in ['orchestrator/roadmap.md', 'orchestrator/verification.md', 'orchestrator/retry-subloop.md', 'orchestrator/rounds/round-171/selection.md']:
    text = (root / rel).read_text()
    missing = [token for token in expected if token not in text]
    if missing:
        print(f'{rel}: missing {missing}')
        sys.exit(1)
print('ROADMAP_POINTERS_OK')
PY`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `git rev-parse --verify 45d765b^{commit} >/dev/null`
- `git merge-base --is-ancestor 45d765b codex/automatic-recursive-type-inference`
- `python3 - <<'PY'
import pathlib, sys
root = pathlib.Path('.')
artifact = (root / 'docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md').read_text()
notes171 = (root / 'orchestrator/rounds/round-171/implementation-notes.md').read_text()
roadmap = (root / 'orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md').read_text()
merge170 = (root / 'orchestrator/rounds/round-170/merge.md').read_text()
required_artifact_tokens = [
    'Attempt: `attempt-2`',
    'Retry state:',
    '`orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md`',
    'merged as commit `45d765b`',
    'sameLaneAliasFrameClearBoundaryExpr',
    'runPipelineElab',
    'runPipelineElabChecked',
    '1303 examples, 0 failures',
    'P3', 'P4', 'P6', 'repo-level readiness', 'item-4', 'successor decision', 'handoff',
]
for token in required_artifact_tokens:
    if token not in artifact:
        print(f'artifact missing token: {token}')
        sys.exit(1)
if '| Accepted merged provenance | `orchestrator/rounds/round-170/merge.md` |' in artifact:
    print('artifact still attributes accepted merged provenance to round-170/merge.md')
    sys.exit(1)
if 'round-170 merge at `45d765b`' in artifact:
    print('artifact still narrates 45d765b as a round-170 merge artifact')
    sys.exit(1)
if 'Completion notes: accepted in `round-170`, merged as commit `45d765b`' not in roadmap:
    print('roadmap.md does not record the accepted merged commit')
    sys.exit(1)
if '45d765b' in merge170:
    print('round-170/merge.md unexpectedly records 45d765b; review the authority split')
    sys.exit(1)
for token in [
    'round-170 approved artifacts',
    '45d765b',
    'roadmap item-2 completion notes',
    'docs-only',
]:
    if token not in notes171:
        print(f'implementation-notes missing token: {token}')
        sys.exit(1)
print('MERGED_COMMIT_PROVENANCE_REBOUND_OK')
PY`
- `rg -n '45d765b|roadmap item-2 completion notes|orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md|orchestrator/rounds/round-170/merge.md|sameLaneAliasFrameClearBoundaryExpr|runPipelineElab|runPipelineElabChecked|1303 examples, 0 failures|P3|P4|P6|repo-level readiness|item-4|successor decision|handoff' docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md orchestrator/rounds/round-171/implementation-notes.md orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md`
- `git diff --check`
- `python3 - <<'PY'
import subprocess, sys
allowed = {
    'docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md',
    'orchestrator/rounds/round-171/implementation-notes.md',
    'orchestrator/rounds/round-171/plan.md',
    'orchestrator/rounds/round-171/selection.md',
}
tracked = subprocess.check_output(['git', 'diff', '--name-only', 'codex/automatic-recursive-type-inference...HEAD'], text=True).splitlines()
untracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [p for p in paths if p not in allowed]
forbidden = [
    p for p in paths
    if p == 'mlf2.cabal' or p.startswith('src/') or p.startswith('src-public/') or p.startswith('app/') or p.startswith('test/')
]
if forbidden or extra:
    if forbidden:
        print('FORBIDDEN_PATHS:')
        print('\n'.join(forbidden))
    if extra:
        print('OUT_OF_SCOPE_PATHS:')
        print('\n'.join(extra))
    sys.exit(1)
print('ROUND171_RETRY2_DOCS_ONLY_SCOPE_OK')
for p in paths:
    print(p)
PY`

## Outcome

- bounded outcome published:
  one canonical item-3 settlement surface that republishes the accepted
  narrow success for `sameLaneAliasFrameClearBoundaryExpr` only
- verification posture:
  round-171 reruns only the docs/roadmap/diff guards above
- full-gate posture:
  because round-171 is docs-only and does not touch `src/`, `src-public/`,
  `app/`, `test/`, or `mlf2.cabal`, it does not rerun the full Cabal gate;
  the accepted full-gate evidence remains predecessor-only provenance from
  round-170 approved artifacts, and merged commit `45d765b` is carried from
  roadmap item-2 completion notes
