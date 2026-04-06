# Round 199 Plan

- Round: `round-199`
- Roadmap: `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap` / `rev-001`
- Milestone: `milestone-3`
- Direction: `direction-3c-record-p5-dominant-boundary-pressure`
- Extracted item: `record-p5-dominant-boundary-pressure`
- Retry: `null`
- Execution shape: serial, docs-only, aggregate/routing-only, one implementation-owned docs artifact only, no worker fan-out, and no production/test/Cabal/roadmap/controller-state edits

## Objective

Publish one canonical milestone-3 routing note only:
`docs/plans/2026-04-07-post-item-7-p5-dominant-boundary-pressure-routing-note-keeping-p2-unopened-on-the-current-ledger.md`.

That note must start from the accepted `round-198` remaining-frontier ledger
rather than rerunning the milestone-3 frontier from scratch. Its job is to
record the routing consequence of the already accepted ranking:
`P5 remains the stronger blocker / pressure source`, so
`direction-3b-freeze-one-bounded-p2-follow-on-lane` stays unopened on the
current ledger and the family routes forward only to the later milestone-4
decision surface where any refreshed readiness or architecture call can be
made lawfully.

This round must stay docs-only, serial, aggregate/routing-only, and
non-widening. The note must not:

- reopen the March 28 exact `P5` packet, the accepted round-151
  reclassification, prior same-lane settlements, or the accepted exact `C1`
  packet as live family debt;
- create a fresh `P2` freeze, implementation slice, test slice, or second
  routing artifact;
- claim general `P5` family closure;
- record an immediate boundary revision; or
- choose a milestone-4 readiness / architecture end-state early.

## Authorized Write Scope

Implementation-owned milestone content in this round is limited to exactly one
new docs artifact:

- `docs/plans/2026-04-07-post-item-7-p5-dominant-boundary-pressure-routing-note-keeping-p2-unopened-on-the-current-ledger.md`

Round-owned artifacts that may appear later in the packet and must not be
misclassified as extra milestone docs are:

- `orchestrator/rounds/round-199/implementation-notes.md`
- `orchestrator/rounds/round-199/review.md`
- `orchestrator/rounds/round-199/review-record.json`
- `orchestrator/rounds/round-199/merge.md`

Do not create or modify:

- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- `README.md`
- any accepted predecessor `docs/plans/**` artifact
- any file under `src/`, `src-public/`, `app/`, or `test/`
- `mlf2.cabal`
- any second `docs/plans/**` artifact, helper scratch file, or handoff addendum

## Locked Round Context

- Accepted `round-198` is the binding immediate predecessor for this round.
  Its canonical artifact
  `docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md`
  and approved `orchestrator/rounds/round-198/review-record.json` already
  conclude `P5 remains the stronger blocker / pressure source`, keep
  `direction-3b-freeze-one-bounded-p2-follow-on-lane` gated, and make
  `direction-3c-record-p5-dominant-boundary-pressure` the only lawful next
  move.
- Accepted `round-197` fixed the settled post-milestone-2 `P5` evidence that
  `round-198` reused: `sameLaneAliasFrameClearBoundaryExpr` has bounded
  current-architecture support on `runPipelineElab` /
  `runPipelineElabChecked`, `nestedForallContrastExpr` remains fail-closed
  with `PhiTranslatabilityError`, and the merged payload stayed `test-only`.
- Accepted `round-193` fixed the pre-milestone-1 repo-level decision surface:
  `continue-bounded` won on that ledger, `P5 polymorphism-nested-forall`
  remained the sharper blocker family than `P2 non-local-propagation`, and
  no readiness or architecture revision was earned there.
- Accepted `round-191` still controls the positive-family aggregate
  classification for `P2`: `P2` remains `packet-specific folklore`, while
  `P5` remained in the blocker lane.
- Accepted `round-181` remains the exact `P2` packet anchor:
  one `C1` non-local scheme-alias / base-like packet stays recursive on
  `runPipelineElab` / `runPipelineElabChecked`, while the fallback
  `baseTarget -> baseC` read remains the packet boundary rather than a
  family-wide non-local closure.
- Accepted `round-151` and the current top-level `implementation_notes.md`
  keep nested-forall `mu` absorption under polymorphic mediation closed as
  known correct behavior. That reclassification is preserved predecessor
  truth only and must not be recast as the current live blocker.
- `milestone-4` is still pending in `rev-001`. This round may only route
  toward that later decision surface; it may not pre-choose any milestone-4
  end-state.

The inherited boundary remains explicit-only / iso-recursive /
non-equi-recursive / non-cyclic-graph / no-fallback unless a later accepted
milestone explicitly changes it.

## Sequential Plan

1. Freeze the accepted routing ledger before drafting the new note; modify no
   files in this step.
   - Read-only inputs:
     `orchestrator/rounds/round-199/selection.md`,
     `orchestrator/state.json`,
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/roadmap.md`,
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/verification.md`,
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/retry-subloop.md`,
     `docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md`,
     `orchestrator/rounds/round-198/review-record.json`,
     `orchestrator/rounds/round-198/merge.md`,
     `docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md`,
     `orchestrator/rounds/round-197/review-record.json`,
     `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`,
     `orchestrator/rounds/round-193/review-record.json`,
     `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`,
     `orchestrator/rounds/round-191/review-record.json`,
     `orchestrator/rounds/round-181/review-record.json`,
     `orchestrator/rounds/round-181/implementation-notes.md`,
     `orchestrator/rounds/round-151/review-record.json`,
     `orchestrator/rounds/round-151/review.md`,
     and `implementation_notes.md`.
   - End this step with one fixed routing chain only:
     accepted `round-198` already ranks `P5` above `P2`;
     accepted `round-197` narrows only one retained-child clear-boundary `P5`
     lane;
     accepted `round-191` / `round-181` still keep `P2` at one exact `C1`
     packet;
     accepted `round-193` still treats `P5` as the sharper unresolved
     blocker; and accepted `round-151` remains settled correct-behavior
     context rather than live blocker debt.
   - The step-1 exit question is narrow:
     what exact accepted-ledger reasoning keeps
     `direction-3b-freeze-one-bounded-p2-follow-on-lane` unopened right now,
     while still stopping short of a milestone-4 readiness or boundary
     decision?
   - Verification:
     `python3 -m json.tool orchestrator/state.json >/dev/null`
     `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/verification.md" && test -f "$roadmap_dir/retry-subloop.md"`
     `rg -n 'P5 remains the stronger blocker / pressure source|direction-3c-record-p5-dominant-boundary-pressure|direction-3b-freeze-one-bounded-p2-follow-on-lane' docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md orchestrator/rounds/round-198/review-record.json orchestrator/rounds/round-198/merge.md`
     `rg -n 'sameLaneAliasFrameClearBoundaryExpr|nestedForallContrastExpr|PhiTranslatabilityError|test-only|runPipelineElab|runPipelineElabChecked' docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md orchestrator/rounds/round-197/review-record.json`
     `rg -n 'continue-bounded|P5 polymorphism-nested-forall|P2 non-local-propagation|sharper blocker' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md orchestrator/rounds/round-193/review-record.json`
     `rg -n 'packet-specific folklore|C1|baseTarget -> baseC|runPipelineElab|runPipelineElabChecked' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md orchestrator/rounds/round-191/review-record.json orchestrator/rounds/round-181/review-record.json orchestrator/rounds/round-181/implementation-notes.md`
     `rg -n 'Known correct behavior under polymorphic mediation|Nested-forall-mediated recursive types|implementation_notes_reclassification' implementation_notes.md orchestrator/rounds/round-151/review.md orchestrator/rounds/round-151/review-record.json`
     `rg -n 'milestone-4|direction-4a-publish-refreshed-readiness-decision|direction-4b-bind-final-enablement-or-next-family' orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/roadmap.md`

2. Draft the one milestone-3 routing note artifact and keep it strictly
   downstream of the accepted `round-198` ledger.
   - Create
     `docs/plans/2026-04-07-post-item-7-p5-dominant-boundary-pressure-routing-note-keeping-p2-unopened-on-the-current-ledger.md`.
   - Use a stage-contract header that records `round-199`,
     `milestone-3`,
     `direction-3c-record-p5-dominant-boundary-pressure`,
     `attempt-1`,
     `retry: null`,
     and the live subject as one docs-only routing note only.
   - The document must include, in order:
     `## Stage Contract Freeze`,
     `## Accepted Routing Ledger`,
     `## Why P5 Pressure Still Dominates`,
     `## Why P2 Stays Unopened On The Current Ledger`,
     `## Routing Consequence`,
     and `## Non-Claims`.
   - In `## Accepted Routing Ledger`, cite the exact settled sources above,
     with `round-198` as the binding immediate predecessor and the earlier
     `round-197` / `round-193` / `round-191` / `round-181` / `round-151`
     material carried only as preserved authority beneath that accepted
     milestone-3 ledger.
   - In `## Why P5 Pressure Still Dominates`, restate the exact accepted
     dominant-pressure read only:
     milestone-2 settled one retained-child clear-boundary `P5` lane, but it
     did not produce general `P5` family closure; the broader
     `P5 polymorphism-nested-forall` family still lacks reusable positive
     support, and `nestedForallContrastExpr` still preserves fail-closed
     quantified-crossing pressure. Keep the accepted round-151
     polymorphic-mediation reclassification explicit as closed predecessor
     truth, not as fresh blocker evidence.
   - In `## Why P2 Stays Unopened On The Current Ledger`, explain the exact
     routing consequence of that accepted pressure read:
     the current ledger still preserves `P2` as packet-specific `C1`
     folklore only; no second non-local packet, adjacent family lane, or new
     representative evidence has been added; opening `direction-3b` now would
     therefore over-promote packet-bounded `P2` evidence and outrun the
     accepted `round-198` ranking.
   - In `## Routing Consequence`, record only one bounded routing result:
     `P2` stays unopened on the current ledger because `P5` remains the
     stronger blocker / pressure source, so the only lawful downstream route
     is the later milestone-4 decision surface where refreshed readiness or
     architecture outcomes can be compared honestly. This section must say
     explicitly that the note is not itself an immediate boundary revision and
     not itself a milestone-4 readiness decision.
   - In `## Non-Claims`, make the boundaries explicit:
     no fresh `P2` freeze;
     no new implementation evidence or tests;
     no general `P5` family closure;
     no repo-level readiness claim;
     and no immediate architecture revision.
   - Do not author a second handoff note, addendum, or milestone-4 decision
     artifact.
   - Verification:
     `test -f docs/plans/2026-04-07-post-item-7-p5-dominant-boundary-pressure-routing-note-keeping-p2-unopened-on-the-current-ledger.md`
     `rg -n 'round-199|direction-3c-record-p5-dominant-boundary-pressure|round-198|round-197|round-193|round-191|round-181|round-151|P5 remains the stronger blocker / pressure source|P2 stays unopened on the current ledger|sameLaneAliasFrameClearBoundaryExpr|nestedForallContrastExpr|PhiTranslatabilityError|packet-specific folklore|C1|milestone-4' docs/plans/2026-04-07-post-item-7-p5-dominant-boundary-pressure-routing-note-keeping-p2-unopened-on-the-current-ledger.md`
     `python3 - <<'PY'\nimport pathlib, sys\nartifact = pathlib.Path('docs/plans/2026-04-07-post-item-7-p5-dominant-boundary-pressure-routing-note-keeping-p2-unopened-on-the-current-ledger.md').read_text()\nrequired_sections = [\n    '## Stage Contract Freeze',\n    '## Accepted Routing Ledger',\n    '## Why P5 Pressure Still Dominates',\n    '## Why P2 Stays Unopened On The Current Ledger',\n    '## Routing Consequence',\n    '## Non-Claims',\n]\nrequired_tokens = [\n    'round-199',\n    'milestone-3',\n    'direction-3c-record-p5-dominant-boundary-pressure',\n    'round-198',\n    'round-197',\n    'round-193',\n    'round-191',\n    'round-181',\n    'round-151',\n    'P5 remains the stronger blocker / pressure source',\n    'P2 stays unopened on the current ledger',\n    'sameLaneAliasFrameClearBoundaryExpr',\n    'nestedForallContrastExpr',\n    'PhiTranslatabilityError',\n    'packet-specific folklore',\n    'C1',\n    'milestone-4',\n    'not itself an immediate boundary revision',\n    'not itself a milestone-4 readiness decision',\n]\nfor token in required_sections + required_tokens:\n    if token not in artifact:\n        print(f'missing token: {token}')\n        sys.exit(1)\nprint('ROUND199_PRESSURE_ROUTING_NOTE_OK')\nPY`

3. Verify docs-only scope and keep the round helper honest about later
   round-owned artifacts.
   - Expected implementation-stage result:
     the one new routing note above plus the round-owned
     `selection.md`, `plan.md`, and `implementation-notes.md` only.
   - Expected later round-owned artifacts after review / merge:
     `review.md`,
     `review-record.json`,
     and `merge.md`
     under `orchestrator/rounds/round-199/`.
   - Workflow:
     run diff hygiene;
     confirm no second `docs/plans/**` artifact exists;
     confirm no code, test, Cabal, roadmap, top-level-doc, or controller-state
     path is being pulled into the round;
     and keep the allowlist explicit about round-owned notes/review/merge
     artifacts so later stage files do not look like scope drift.
   - Verification:
     `git diff --check`
     `python3 - <<'PY'\nimport subprocess, sys\nartifact = 'docs/plans/2026-04-07-post-item-7-p5-dominant-boundary-pressure-routing-note-keeping-p2-unopened-on-the-current-ledger.md'\nallowed = {\n    artifact,\n    'orchestrator/rounds/round-199/selection.md',\n    'orchestrator/rounds/round-199/plan.md',\n    'orchestrator/rounds/round-199/implementation-notes.md',\n    'orchestrator/rounds/round-199/review.md',\n    'orchestrator/rounds/round-199/review-record.json',\n    'orchestrator/rounds/round-199/merge.md',\n}\ntracked = subprocess.check_output(['git', 'diff', '--name-only', 'HEAD'], text=True).splitlines()\nuntracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines()\npaths = sorted({p for p in tracked + untracked if p})\nextra = [p for p in paths if p not in allowed and p != 'orchestrator/state.json']\nforbidden = [\n    p for p in paths\n    if p == 'mlf2.cabal'\n    or p.startswith('src/')\n    or p.startswith('src-public/')\n    or p.startswith('app/')\n    or p.startswith('test/')\n    or p in {'TODO.md', 'implementation_notes.md', 'Bugs.md', 'README.md'}\n    or (p.startswith('docs/') and p != artifact)\n    or p.startswith('orchestrator/roadmaps/')\n]\nif forbidden or extra:\n    if forbidden:\n        print('FORBIDDEN_PATHS:')\n        print('\\n'.join(forbidden))\n    if extra:\n        print('OUT_OF_SCOPE_PATHS:')\n        print('\\n'.join(extra))\n    sys.exit(1)\nprint('ROUND199_DOCS_ONLY_SCOPE_OK')\nPY`

## Review Focus

- The round treats accepted `round-198` as the binding milestone-3 ledger and
  explains the routing consequence of that ledger instead of recomputing the
  frontier or inventing new evidence.
- The note keeps `P2` unopened because the accepted ledger still leaves `P5`
  as the stronger unresolved blocker, not because `P2` has been erased or
  family-closed.
- The routing consequence stays bounded to milestone-3 and only feeds the
  later milestone-4 decision surface. It does not itself choose a readiness
  outcome or architecture revision.
- The actual round scope remains one implementation-owned docs artifact plus
  round-owned control-plane artifacts only, and the scope helper must keep
  those round-owned notes/review/merge paths allowed explicitly.
