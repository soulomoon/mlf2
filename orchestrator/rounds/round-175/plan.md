# Round 175 Plan

- Round: `round-175`
- Roadmap: `2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap` / `rev-001`
- Item: `item-3`
- Retry: `null`
- Execution shape: docs-only, item-3-only, serial

## Objective

Publish one canonical item-3 settlement surface for the exact frozen packet
`sameLaneDoubleAliasFrameClearBoundaryExpr` only, rebinding accepted item-2
evidence without adding new implementation or new verification authority.

The artifact must republish the accepted `narrow success` read from round-174,
bind its focused and full-gate provenance to accepted sources only, record the
exact repo-impact read as one settled packet only, and keep broader `P3` /
`P4` / `P6`, repo-level readiness, item-4, and boundary-revision questions
unresolved.

## Write Scope

Implementer-owned writes are limited to:

- `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-2-settlement-surface-and-exact-repo-impact-read.md`
- `orchestrator/rounds/round-175/implementation-notes.md`

Do not modify `selection.md`, `review.md`, `review-record.json`,
`orchestrator/state.json`, `orchestrator/roadmaps/**`, `src/`, `src-public/`,
`app/`, `test/`, `mlf2.cabal`, `TODO.md`, `Bugs.md`, or any item-4 artifact.

## Sequential Plan

1. Author the canonical item-3 settlement artifact only.
   - Create
     `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-2-settlement-surface-and-exact-repo-impact-read.md`
     as a docs-only aggregate settlement record for
     `sameLaneDoubleAliasFrameClearBoundaryExpr`.
   - In `## Authority Ledger`, bind the exact frozen packet and inherited
     architecture to the accepted item-1 freeze, the exact item-2 outcome to
     `orchestrator/rounds/round-174/implementation-notes.md`, the focused/full
     verification evidence to `orchestrator/rounds/round-174/review.md`, the
     bounded approval to `orchestrator/rounds/round-174/review-record.json`,
     and the merged provenance to the active roadmap item-2 completion notes in
     `orchestrator/roadmaps/.../roadmap.md`.

2. Republish the exact post-item-2 settled read, but no more.
   - State that `sameLaneDoubleAliasFrameClearBoundaryExpr` is one settled
     `narrow success` packet only.
   - Record that both authoritative entrypoints `runPipelineElab` and
     `runPipelineElabChecked` preserve recursive output for that packet within
     the inherited explicit-only / iso-recursive / non-equi-recursive /
     non-cyclic-graph / no-fallback architecture.
   - Keep the non-claims explicit: no broader `P3` / `P4` / `P6` settlement,
     no repo-level readiness, no item-4 outcome, no successor decision, and no
     handoff.

3. Record the exact accepted provenance, but do not invent new authority.
   - Cite the round-174 focused reruns for
     `sameLaneDoubleAliasFrameClearBoundaryExpr` and
     `double-alias clear-boundary packet`.
   - Cite the accepted round-174 full gate
     `cabal build all && cabal test` with `1306 examples, 0 failures`.
   - Make explicit that round-175 contributes no new focused reruns and no new
     full-gate authority.

4. Align the round-local summary with that bounded docs-only settlement.
   - In `orchestrator/rounds/round-175/implementation-notes.md`, summarize the
     exact settled packet read, the accepted provenance split, the packet-only
     repo-impact read, and the docs-only/no-code-change posture.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `python3 - <<'PY'
import pathlib, sys
root = pathlib.Path('.')
artifact = (root / 'docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-2-settlement-surface-and-exact-repo-impact-read.md').read_text()
notes = (root / 'orchestrator/rounds/round-175/implementation-notes.md').read_text()
roadmap = (root / 'orchestrator/roadmaps/2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap/rev-001/roadmap.md').read_text()
required_artifact_tokens = [
    'sameLaneDoubleAliasFrameClearBoundaryExpr',
    'narrow success',
    'runPipelineElab',
    'runPipelineElabChecked',
    '0f44acd',
    '1306 examples, 0 failures',
    'P3', 'P4', 'P6', 'repo-level readiness', 'item-4', 'successor decision', 'handoff',
]
for token in required_artifact_tokens:
    if token not in artifact:
        print(f'artifact missing token: {token}')
        sys.exit(1)
if 'Completion notes: accepted in `round-174` attempt `2`, merged as commit\n   `0f44acd`' not in roadmap:
    print('roadmap.md does not record merged item-2 provenance')
    sys.exit(1)
for token in ['docs-only', 'round-174', '0f44acd', 'sameLaneDoubleAliasFrameClearBoundaryExpr']:
    if token not in notes:
        print(f'implementation-notes missing token: {token}')
        sys.exit(1)
print('ROUND175_ITEM3_SETTLEMENT_OK')
PY`
- `git diff --check`
- `python3 - <<'PY'
import subprocess, sys
allowed = {
    'docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-2-settlement-surface-and-exact-repo-impact-read.md',
    'orchestrator/rounds/round-175/implementation-notes.md',
    'orchestrator/rounds/round-175/plan.md',
    'orchestrator/rounds/round-175/selection.md',
}
tracked = subprocess.check_output(['git', 'diff', '--name-only', 'HEAD'], text=True).splitlines()
untracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [p for p in paths if p not in allowed and p != 'orchestrator/state.json' and not p.startswith('orchestrator/roadmaps/')]
forbidden = [
    p for p in paths
    if p == 'mlf2.cabal' or p.startswith('src/') or p.startswith('src-public/') or p.startswith('app/') or p.startswith('test/')
]
if forbidden or extra:
    if forbidden:
        print('FORBIDDEN_PATHS:')
        print('\\n'.join(forbidden))
    if extra:
        print('OUT_OF_SCOPE_PATHS:')
        print('\\n'.join(extra))
    sys.exit(1)
print('ROUND175_DOCS_ONLY_SCOPE_OK')
PY`
