# Round 197 Plan

- Round: `round-197`
- Roadmap: `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap` / `rev-001`
- Milestone: `milestone-2`
- Direction: `direction-2b-publish-post-implementation-p5-settlement`
- Extracted item: `publish-post-implementation-p5-settlement`
- Retry: `null`
- Execution shape: docs-only, serial, one settlement artifact only, no worker fan-out

## Objective

Publish one canonical milestone-2 settlement surface for the already selected
retained-child guard-cluster `P5` lane only.

The artifact must turn the merged `round-196` execution result into one exact
repo-impact read for this lane: `sameLaneAliasFrameClearBoundaryExpr` now has
reviewer-visible bounded current-architecture support on
`runPipelineElab` / `runPipelineElabChecked`, while
`nestedForallContrastExpr` remains the preserved fail-closed contrast.

This round must not create new evidence, reopen predecessor packets, relitigate
the milestone-1 gate, classify fresh boundary pressure, or route ahead to
`P2` / milestone-3. Its job is to publish the one stable settlement surface
that milestone-3 will later consume.

## Write Scope

Implementer-owned writes are limited to exactly one docs artifact:

- `docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md`

Do not modify `orchestrator/state.json`, `selection.md`, roadmap files,
review/merge artifacts, `implementation_notes.md`, `TODO.md`, `Bugs.md`,
`README.md`, or any `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`
path in this round. Do not author a second settlement file, a handoff file, or
any round-local summary file outside the single settlement artifact above.

## Locked Round Context

- Accepted `round-194` froze the only lawful subject: the retained-child
  guard-cluster lane centered on `boundHasForallFrom`,
  `sameLaneLocalRetainedChildTarget`,
  `keepTargetFinal`,
  `targetC`, and
  `preserveRetainedChildAuthoritativeResult`, plus the authoritative surfaces
  and writable slice.
- Accepted `round-195` classified that exact lane as
  `bounded current-architecture continuation` and made one bounded
  milestone-2 campaign the only lawful next move.
- Merged `round-196` commit `34f88bc`
  (`Pin the selected P5 retained-child lane to authoritative pipeline tests`)
  is the entire new evidence payload that this round may settle:
  `test/Research/P5ClearBoundarySpec.hs` now binds
  `sameLaneAliasFrameClearBoundaryExpr` to both authoritative entrypoints,
  `test/PipelineSpec.hs` keeps the lane pinned to
  `boundHasForallFrom`,
  `preserveRetainedChildAliasBoundary`, and
  `preserveRetainedChildAuthoritativeResult`,
  `nestedForallContrastExpr` remains fail-closed with
  `PhiTranslatabilityError`, and the accepted full gate passed with
  `1338 examples, 0 failures`.
- The current round must publish that merged evidence as one bounded settlement
  read only; it must not add fresh focused reruns, fresh full-gate authority,
  or broader family claims.

## Sequential Plan

1. Create the one settlement artifact only.
   - Create
     `docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md`
     as the sole implementation-owned file for this round.
   - Use a stage-contract header that records `round-197`,
     `milestone-2`,
     `direction-2b-publish-post-implementation-p5-settlement`,
     `attempt-1`,
     `retry: null`,
     and the live subject as one post-implementation settlement surface for
     the selected retained-child guard-cluster `P5` lane only.
   - State explicitly that the artifact republishes merged `round-196`
     evidence; it does not generate new implementation, new tests, or new
     verification authority.

2. Bind the predecessor chain and merged milestone-2 evidence inside that file.
   - In `## Authority Ledger` or an equivalent section, cite the exact
     predecessor freeze/classification sources:
     `docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`,
     `orchestrator/rounds/round-194/review-record.json`,
     `docs/plans/2026-04-06-post-item-7-p5-current-architecture-vs-boundary-pressure-gate-and-immediate-handoff-decision.md`,
     and `orchestrator/rounds/round-195/review-record.json`.
   - Bind the merged milestone-2 evidence to
     `orchestrator/rounds/round-196/review-record.json`,
     `orchestrator/rounds/round-196/review.md`,
     and the active roadmap progress notes that record merged commit
     `34f88bc`.
   - Cite the concrete evidence anchors without widening them:
     `test/Research/P5ClearBoundarySpec.hs` for
     `sameLaneAliasFrameClearBoundaryExpr`,
     the authoritative-entrypoint check on `runPipelineElab` and
     `runPipelineElabChecked`,
     and the preserved `nestedForallContrastExpr` fail-closed contrast;
     `test/PipelineSpec.hs` for the guard-cluster wiring check over
     `boundHasForallFrom`,
     `preserveRetainedChildAliasBoundary`, and
     `preserveRetainedChildAuthoritativeResult`.
   - Make explicit that round-197 contributes no new focused reruns and no new
     `cabal build all && cabal test` result; it republishes the accepted
     round-196 evidence only.

3. Publish the exact post-implementation settlement read, but no broader claim.
   - In `## Exact Post-Implementation Read`, state that the selected
     retained-child guard-cluster lane now has bounded current-architecture
     support on both authoritative entrypoints for the alias-frame specimen
     `sameLaneAliasFrameClearBoundaryExpr`.
   - Record the exact contrast that remains preserved:
     `nestedForallContrastExpr` still fails closed at the authoritative
     entrypoints with `PhiTranslatabilityError`, so the settlement remains
     lane-bounded and does not erase the reject-side quantified-crossing read.
   - Record that the merged implementation payload was test-only:
     no `src/`, `src-public/`, or public-interface widening was needed to make
     this exact lane reviewer-visible on the authoritative surfaces.

4. State the exact repo-impact read and keep the non-claims explicit.
   - In `## Exact Repo-Impact Read`, say that milestone-2 now has one stable
     settlement surface for this retained-child guard-cluster lane only:
     the inherited current architecture can carry this exact alias-frame
     specimen on `runPipelineElab` / `runPipelineElabChecked` without widening
     production behavior.
   - Keep the impact read narrow and explicit:
     this settles one retained-child lane only, not general `P5` family
     closure, not fresh `P2` routing, not repo-level readiness, not a new
     boundary-pressure decision, and not any cyclic, multi-SCC, fallback, or
     second-interface claim.
   - End with the bounded operational consequence only:
     milestone-2 now has the stable settlement surface that later milestone-3
     work must consume, but this artifact itself does not choose that routing.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `git rev-parse --verify 34f88bc^{commit} >/dev/null`
- `git merge-base --is-ancestor 34f88bc HEAD`
- `python3 - <<'PY'\nimport pathlib, sys\nartifact = pathlib.Path('docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md').read_text()\nrequired = [\n    'round-197',\n    'direction-2b-publish-post-implementation-p5-settlement',\n    'sameLaneAliasFrameClearBoundaryExpr',\n    'nestedForallContrastExpr',\n    'runPipelineElab',\n    'runPipelineElabChecked',\n    'boundHasForallFrom',\n    'preserveRetainedChildAliasBoundary',\n    'preserveRetainedChildAuthoritativeResult',\n    '34f88bc',\n    '1338 examples, 0 failures',\n    'PhiTranslatabilityError',\n    'test-only',\n    'general `P5` family closure',\n    'P2',\n    'repo-level readiness',\n]\nfor token in required:\n    if token not in artifact:\n        print(f'missing token: {token}')\n        sys.exit(1)\nprint('ROUND197_SETTLEMENT_ARTIFACT_OK')\nPY`
- `git diff --check`
- `python3 - <<'PY'\nimport subprocess, sys\nartifact = 'docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md'\nallowed = {\n    artifact,\n    'orchestrator/rounds/round-197/selection.md',\n    'orchestrator/rounds/round-197/plan.md',\n}\ntracked = subprocess.check_output(['git', 'diff', '--name-only', 'HEAD'], text=True).splitlines()\nuntracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines()\npaths = [p for p in tracked + untracked if p]\nextra = [p for p in paths if p not in allowed and p != 'orchestrator/state.json']\nforbidden = [\n    p for p in paths\n    if p == 'mlf2.cabal'\n    or p.startswith('src/')\n    or p.startswith('src-public/')\n    or p.startswith('app/')\n    or p.startswith('test/')\n    or (p.startswith('docs/') and p != artifact)\n]\nif forbidden or extra:\n    if forbidden:\n        print('FORBIDDEN_PATHS:')\n        print('\\n'.join(forbidden))\n    if extra:\n        print('OUT_OF_SCOPE_PATHS:')\n        print('\\n'.join(extra))\n    sys.exit(1)\nprint('ROUND197_DOCS_ONLY_SCOPE_OK')\nPY`

## Review Focus

- The diff stays bounded to one settlement artifact plus round-owned
  `selection.md` / `plan.md`; no code, test, roadmap, or auxiliary docs change
  appears.
- The artifact settles exactly the retained-child guard-cluster lane selected
  in `round-194` / `round-195` and grounded in merged `round-196` evidence.
- The repo-impact read is honest about substance:
  one lane now has authoritative-surface support,
  `nestedForallContrastExpr` remains fail-closed contrast,
  and the merged evidence was test-only rather than production widening.
- The non-claims remain explicit:
  no broader `P5` closure,
  no fresh `P2` routing,
  no readiness claim,
  and no boundary-pressure reclassification inside this artifact.
