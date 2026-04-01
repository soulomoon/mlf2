# Round 172 Plan

- Round: `round-172`
- Roadmap: `2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap` / `rev-001`
- Item: `item-4`
- Retry: `null`
- Execution shape: docs-only, item-4-only, serial, aggregate decision/handoff

## Objective

Publish one docs-only item-4 aggregate artifact at:
`docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md`.

The artifact must consume the accepted item-3 settlement only and record
exactly:

- one explicit outcome token: `continue-bounded`; and
- one immediate handoff token: `open one bounded current-architecture family`.

Current planning read: those are the only honest selections on the accepted
record, because `sameLaneAliasFrameClearBoundaryExpr` is now one settled
`narrow success` packet on both `runPipelineElab` and
`runPipelineElabChecked` within the inherited explicit-only /
iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback
architecture, while broader `P3` / `P4` / `P6` and repo-level readiness
remain unresolved. The round must not broaden that narrow success into
`stop-blocked`, repo-level readiness, or an implicit boundary revision.

## Write Scope

Implementer-owned writes for this round are limited to:

- `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md`
- `orchestrator/rounds/round-172/implementation-notes.md`

Do not modify `selection.md`, `review.md`, `orchestrator/state.json`,
`orchestrator/roadmaps/**`, `docs/plans/2026-04-01-general-automatic-iso-recursive-successor-authority-exact-inherited-blocker-lane-and-writable-slice-freeze.md`,
`docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md`,
`orchestrator/rounds/round-170/**`, `orchestrator/rounds/round-171/**`,
`TODO.md`, `implementation_notes.md`, `Bugs.md`, `src/`, `src-public/`,
`app/`, `test/`, or `mlf2.cabal`.

## Sequential Plan

1. Author the item-4 stage contract and authority ledger in
   `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md`.
   - Bind the accepted predecessor chain to the exact sources that matter
     here:
     `docs/plans/2026-04-01-general-automatic-iso-recursive-successor-authority-exact-inherited-blocker-lane-and-writable-slice-freeze.md`,
     `orchestrator/roadmaps/2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap/rev-001/roadmap.md`,
     `orchestrator/rounds/round-170/implementation-notes.md`,
     `orchestrator/rounds/round-170/review.md`,
     `orchestrator/rounds/round-170/review-record.json`, and
     `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md`.
   - Restate the accepted item-3 baseline exactly in substance:
     `sameLaneAliasFrameClearBoundaryExpr` is one settled `narrow success`
     packet, recursive output is preserved on both authoritative entrypoints,
     the inherited boundary is unchanged, and broader `P3` / `P4` / `P6`
     plus repo-level readiness remain unresolved.

2. Add an explicit outcome-evaluation section in that same artifact and
   select exactly one outcome token.
   - Evaluate only the three lawful item-4 tokens:
     `continue-bounded`,
     `stop-blocked`,
     `reopen-boundary-question`.
   - Record the selected line exactly as:
     `Authoritative item-4 outcome token:`
     followed by
     `` `continue-bounded` ``.
   - Explain why `stop-blocked` is not lawful on the accepted item-3 record:
     the exact packet is already settled narrow success, so the family is not
     blocked on that packet.
   - Explain why `reopen-boundary-question` is not lawful on the accepted
     item-3 record:
     the settled packet succeeded within the inherited current architecture,
     and the accepted evidence does not prove a need for cyclic search,
     multi-SCC search, fallback widening, or any other boundary revision.

3. Add an immediate-handoff section in that same artifact and select exactly
   one handoff token.
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
   - State the immediate operational consequence in substance:
     the current item-4 family stops after recording this decision, and the
     next lawful orchestrator move is a new bounded current-architecture
     successor family, not another attempt to widen this round in place.

4. Mirror the same bounded decision in
   `orchestrator/rounds/round-172/implementation-notes.md`.
   - State explicitly that the round is docs-only and item-4-only.
   - Record the same selected outcome token `continue-bounded`.
   - Record the same selected handoff token
     `open one bounded current-architecture family`.
   - State explicitly that no code/test files, roadmap files, controller
     state, or predecessor settlement artifacts were changed.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `python3 - <<'PY'\nimport pathlib, re, sys\nroot = pathlib.Path('.')\nartifact = (root / 'docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md').read_text()\nnotes = (root / 'orchestrator/rounds/round-172/implementation-notes.md').read_text()\nrequired_artifact_tokens = [\n    'sameLaneAliasFrameClearBoundaryExpr',\n    'runPipelineElab',\n    'runPipelineElabChecked',\n    'P3', 'P4', 'P6',\n    'repo-level readiness',\n    'continue-bounded',\n    'stop-blocked',\n    'reopen-boundary-question',\n    'open one bounded current-architecture family',\n    'open one explicit boundary-revision family',\n    'explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback',\n]\nfor token in required_artifact_tokens:\n    if token not in artifact:\n        print(f'artifact missing token: {token}')\n        sys.exit(1)\noutcome_match = re.search(r'Authoritative item-4 outcome token:\\s*\\n\\s*`([^`]+)`', artifact)\nif not outcome_match:\n    print('missing authoritative outcome token block')\n    sys.exit(1)\nif outcome_match.group(1) != 'continue-bounded':\n    print(f'unexpected outcome token: {outcome_match.group(1)}')\n    sys.exit(1)\nhandoff_match = re.search(r'Immediate handoff token:\\s*\\n\\s*`([^`]+)`', artifact)\nif not handoff_match:\n    print('missing immediate handoff token block')\n    sys.exit(1)\nif handoff_match.group(1) != 'open one bounded current-architecture family':\n    print(f'unexpected handoff token: {handoff_match.group(1)}')\n    sys.exit(1)\nfor token in [\n    'docs-only',\n    'item-4-only',\n    'continue-bounded',\n    'open one bounded current-architecture family',\n    'no code/test files',\n]:\n    if token not in notes:\n        print(f'implementation-notes missing token: {token}')\n        sys.exit(1)\nprint('ROUND172_ITEM4_TOKENS_OK')\nPY`
- `rg -n 'Authoritative item-4 outcome token:|Immediate handoff token:|sameLaneAliasFrameClearBoundaryExpr|runPipelineElab|runPipelineElabChecked|continue-bounded|stop-blocked|reopen-boundary-question|open one bounded current-architecture family|open one explicit boundary-revision family|P3|P4|P6|repo-level readiness' docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md orchestrator/rounds/round-172/implementation-notes.md`
- `git diff --check`
- `python3 - <<'PY'\nimport subprocess, sys\nallowed = {\n    'docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md',\n    'orchestrator/rounds/round-172/implementation-notes.md',\n    'orchestrator/rounds/round-172/plan.md',\n    'orchestrator/rounds/round-172/selection.md',\n}\ntracked = subprocess.check_output(['git', 'diff', '--name-only', 'codex/automatic-recursive-type-inference...HEAD'], text=True).splitlines()\nuntracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines()\npaths = [p for p in tracked + untracked if p]\nextra = [p for p in paths if p not in allowed]\nforbidden = [\n    p for p in paths\n    if p == 'mlf2.cabal' or p.startswith('src/') or p.startswith('src-public/') or p.startswith('app/') or p.startswith('test/')\n]\nif forbidden or extra:\n    if forbidden:\n        print('FORBIDDEN_PATHS:')\n        print('\\n'.join(forbidden))\n    if extra:\n        print('OUT_OF_SCOPE_PATHS:')\n        print('\\n'.join(extra))\n    sys.exit(1)\nprint('ROUND172_DOCS_ONLY_SCOPE_OK')\nfor p in paths:\n    print(p)\nPY`
