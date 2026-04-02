# Round 176 Plan

- Round: `round-176`
- Roadmap: `2026-04-02-00-general-automatic-iso-recursive-current-architecture-follow-on-roadmap` / `rev-001`
- Item: `item-4`
- Retry: `null`
- Execution shape: docs-only, item-4-only, serial, aggregate decision/handoff

## Objective

Publish one docs-only item-4 aggregate artifact at:
`docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md`.

The artifact must consume the accepted item-3 settlement only and record
exactly:

- one explicit outcome token: `continue-bounded`; and
- one immediate handoff token: `open one bounded current-architecture family`.

Current planning read: those are the only honest selections on the accepted
record, because `sameLaneDoubleAliasFrameClearBoundaryExpr` is now one settled
`narrow success` packet on both `runPipelineElab` and
`runPipelineElabChecked` within the inherited explicit-only / iso-recursive /
non-equi-recursive / non-cyclic-graph / no-fallback architecture, while
broader `P3` / `P4` / `P6` and repo-level readiness remain unresolved.

## Write Scope

Implementer-owned writes are limited to:

- `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md`
- `orchestrator/rounds/round-176/implementation-notes.md`

Do not modify `selection.md`, `review.md`, `orchestrator/state.json`,
`orchestrator/roadmaps/**`, prior item-1/item-2/item-3 artifacts, `TODO.md`,
`implementation_notes.md`, `Bugs.md`, `src/`, `src-public/`, `app/`, `test/`,
or `mlf2.cabal`.

## Sequential Plan

1. Author the item-4 stage contract and authority ledger in the canonical
   decision artifact.
   - Bind the accepted predecessor chain to the exact sources that matter
     here:
     the item-1 freeze document,
     the active roadmap completion notes for items `2` and `3`,
     `orchestrator/rounds/round-174/implementation-notes.md`,
     `orchestrator/rounds/round-174/review.md`,
     `orchestrator/rounds/round-174/review-record.json`, and
     `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-2-settlement-surface-and-exact-repo-impact-read.md`.
   - Restate the accepted item-3 baseline exactly in substance:
     `sameLaneDoubleAliasFrameClearBoundaryExpr` is one settled `narrow success`
     packet, recursive output is preserved on both authoritative entrypoints,
     the inherited boundary is unchanged, and broader `P3` / `P4` / `P6`
     plus repo-level readiness remain unresolved.

2. Add an explicit outcome-evaluation section and select exactly one outcome
   token.
   - Evaluate only the three lawful item-4 tokens:
     `continue-bounded`,
     `stop-blocked`,
     `reopen-boundary-question`.
   - Record the selected line exactly as:
     `Authoritative item-4 outcome token:`
     followed by
     `` `continue-bounded` ``.
   - Explain why `stop-blocked` is not lawful on the accepted item-3 record:
     the exact packet is already settled narrow success.
   - Explain why `reopen-boundary-question` is not lawful on the accepted
     item-3 record:
     the settled packet succeeded within the inherited current architecture,
     and the accepted evidence does not prove a need for cyclic search,
     multi-SCC search, fallback widening, or any other boundary revision.

3. Add an immediate-handoff section and select exactly one handoff token.
   - Evaluate only the three lawful handoff branches:
     `stop`,
     `open one bounded current-architecture family`,
     `open one explicit boundary-revision family`.
   - Record the selected line exactly as:
     `Immediate handoff token:`
     followed by
     `` `open one bounded current-architecture family` ``.
   - Bind that handoff to one fresh successor family for one still-unresolved
     representative-gap packet inside the inherited current architecture only.
     Do not choose the next exact packet here, do not publish a readiness
     claim, and do not smuggle in a boundary-revision family.

4. Mirror the same bounded decision in
   `orchestrator/rounds/round-176/implementation-notes.md`.
   - State explicitly that the round is docs-only and item-4-only.
   - Record the selected outcome token `continue-bounded`.
   - Record the selected handoff token
     `open one bounded current-architecture family`.
   - State explicitly that no code/test files, roadmap files, or controller
     state were changed by the round artifact itself.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `python3 - <<'PY'
import pathlib, re, sys
root = pathlib.Path('.')
artifact = (root / 'docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md').read_text()
notes = (root / 'orchestrator/rounds/round-176/implementation-notes.md').read_text()
required_artifact_tokens = [
    'sameLaneDoubleAliasFrameClearBoundaryExpr',
    'runPipelineElab',
    'runPipelineElabChecked',
    'P3', 'P4', 'P6',
    'repo-level readiness',
    'continue-bounded',
    'stop-blocked',
    'reopen-boundary-question',
    'open one bounded current-architecture family',
    'open one explicit boundary-revision family',
    'explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback',
]
for token in required_artifact_tokens:
    if token not in artifact:
        print(f'artifact missing token: {token}')
        sys.exit(1)
outcome_match = re.search(r'Authoritative item-4 outcome token:\s*\n`([^`]+)`', artifact)
if not outcome_match or outcome_match.group(1) != 'continue-bounded':
    print('unexpected outcome token')
    sys.exit(1)
handoff_match = re.search(r'Immediate handoff token:\s*\n`([^`]+)`', artifact)
if not handoff_match or handoff_match.group(1) != 'open one bounded current-architecture family':
    print('unexpected handoff token')
    sys.exit(1)
for token in ['docs-only', 'item-4-only', 'continue-bounded', 'open one bounded current-architecture family', 'no code/test files']:
    if token not in notes:
        print(f'implementation-notes missing token: {token}')
        sys.exit(1)
print('ROUND176_ITEM4_TOKENS_OK')
PY`
- `git diff --check`
- `python3 - <<'PY'
import subprocess, sys
allowed = {
    'docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md',
    'orchestrator/rounds/round-176/implementation-notes.md',
    'orchestrator/rounds/round-176/plan.md',
    'orchestrator/rounds/round-176/selection.md',
    'orchestrator/rounds/round-176/review.md',
    'orchestrator/rounds/round-176/review-record.json',
    'orchestrator/rounds/round-176/merge.md',
}
tracked = subprocess.check_output(['git', 'diff', '--name-only', 'HEAD'], text=True).splitlines()
untracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [p for p in paths if p not in allowed and p != 'orchestrator/state.json' and not p.startswith('orchestrator/roadmaps/')]
forbidden = [p for p in paths if p == 'mlf2.cabal' or p.startswith('src/') or p.startswith('src-public/') or p.startswith('app/') or p.startswith('test/')]
if forbidden or extra:
    if forbidden:
        print('FORBIDDEN_PATHS:')
        print('\\n'.join(forbidden))
    if extra:
        print('OUT_OF_SCOPE_PATHS:')
        print('\\n'.join(extra))
    sys.exit(1)
print('ROUND176_DOCS_ONLY_SCOPE_OK')
PY`
