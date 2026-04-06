# Round 198 Plan

- Round: `round-198`
- Roadmap: `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap` / `rev-001`
- Milestone: `milestone-3`
- Direction: `direction-3a-refresh-the-p5-vs-p2-gap-ledger`
- Extracted item: `refresh-the-p5-vs-p2-gap-ledger`
- Retry: `null`
- Execution shape: docs-only, serial, one aggregate reread artifact only, no worker fan-out

## Objective

Publish one canonical milestone-3 reread artifact only:
`docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md`.

That artifact must refresh the exact remaining positive-family frontier from
the settled milestone-2 baseline, keeping the refreshed `P5` settlement read
and the accepted `P2 packet-specific folklore` ledger distinct.

The reread must end with exactly one remaining-frontier conclusion only:

- `P5` still remains the stronger unresolved blocker / pressure source; or
- `P2` is now the next lawful bounded follow-on lane.

Current planning read: accepted `round-197` narrowed `P5` by settling one
clear-boundary retained-child lane, but it did not promote `P5` to family
closure and it preserved `nestedForallContrastExpr` as fail-closed contrast.
Accepted `P2` evidence still appears to be one exact `C1` packet only. Unless
the refreshed ledger can honestly show that the settled milestone-2 baseline
demotes `P5` below that packet-specific `P2` read, fail closed to
`P5 remains the stronger blocker` rather than forcing a premature `P2`
freeze.

This round must stay docs-only, aggregate-only, current-architecture-only, and
non-widening. It must not reopen the March 28 exact packet, the accepted
round-151 reclassification, prior same-lane settlements, the accepted exact
`C1` packet as live family closure, repo-level readiness, cyclic search,
multi-SCC behavior, fallback behavior, or a second interface.

## Write Scope

Implementer-owned repo writes are limited to exactly one reread artifact:

- `docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md`

Do not modify `orchestrator/state.json`, `selection.md`, roadmap files,
review/merge artifacts, `TODO.md`, `implementation_notes.md`, `Bugs.md`,
`README.md`, or any file under `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal` in this round. Do not author a separate `P5` note, a separate
`P2` freeze draft, a routing addendum, or any second docs artifact.

## Locked Round Context

- Accepted `round-193` selected `continue-bounded` from the item-4 / item-5 /
  item-6 aggregate ledger and named `P5 polymorphism-nested-forall` as the
  sharper blocker family than `P2 non-local-propagation` at that pre-
  milestone-1 baseline.
- Accepted `round-191` fixed the positive-family aggregate classification that
  still governs `P2`: `P2` remained `packet-specific folklore`, while
  `P3`, `P4`, and `P6` rose to `credible general support` and `P5` remained
  `current-architecture blockers`.
- Accepted `round-181` is still the exact `P2` evidence anchor underneath that
  aggregate read: one non-local `C1` scheme-alias / base-like packet stays
  recursively visible on `runPipelineElab` / `runPipelineElabChecked`, while
  the fallback `baseTarget -> baseC` surface remains the non-recursive packet
  boundary.
- Accepted `round-197` completed milestone-2 by publishing the settled `P5`
  reread surface: `sameLaneAliasFrameClearBoundaryExpr` now has bounded
  current-architecture support on `runPipelineElab` /
  `runPipelineElabChecked`, `nestedForallContrastExpr` remains fail-closed
  with `PhiTranslatabilityError`, and the merged implementation payload stayed
  `test-only`.
- The current round must integrate those accepted reads only. It must not
  create new implementation evidence, new tests, a new `P2` lane freeze, or a
  broader readiness / architecture decision.

## Sequential Plan

1. Freeze the exact reread ledger before drafting the artifact; modify no
   files in this step.
   - Read-only inputs:
     `orchestrator/rounds/round-198/selection.md`,
     `orchestrator/state.json`,
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/roadmap.md`,
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/verification.md`,
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/retry-subloop.md`,
     `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`,
     `orchestrator/rounds/round-193/review-record.json`,
     `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`,
     `orchestrator/rounds/round-191/review-record.json`,
     `orchestrator/rounds/round-181/review-record.json`,
     `orchestrator/rounds/round-181/implementation-notes.md`,
     `docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md`,
     and `orchestrator/rounds/round-197/review-record.json`.
   - End this step with one fixed comparison ledger only:
     the pre-milestone-1 routing baseline from `round-193`;
     the preserved `P2 = packet-specific folklore` aggregate read plus the
     exact `C1` packet provenance from `round-181`;
     and the refreshed milestone-2 `P5` settlement read from `round-197`.
   - The step-1 exit question is narrow:
     does the accepted milestone-2 settlement reduce `P5` enough that `P2`
     becomes the next strongest unresolved positive-family front, or does the
     refreshed ledger still leave `P5` as the stronger blocker / pressure
     source?
   - Verification:
     `python3 -m json.tool orchestrator/state.json >/dev/null`
     `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/verification.md" && test -f "$roadmap_dir/retry-subloop.md"`
     `rg -n 'continue-bounded|P2 non-local-propagation|P5 polymorphism-nested-forall|sharper blocker' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`
     `rg -n 'packet-specific folklore|P2 non-local-propagation|P5 polymorphism-nested-forall|current-architecture blockers' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`
     `rg -n 'sameLaneAliasFrameClearBoundaryExpr|nestedForallContrastExpr|PhiTranslatabilityError|test-only|runPipelineElab|runPipelineElabChecked' docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md`
     `rg -n 'C1|baseTarget -> baseC|runPipelineElab|runPipelineElabChecked|packet-specific folklore' orchestrator/rounds/round-181/review-record.json orchestrator/rounds/round-181/implementation-notes.md docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`

2. Draft the one milestone-3 remaining-frontier ledger artifact and keep it
   tied to that fixed accepted ledger only.
   - Create
     `docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md`.
   - Use a stage-contract header that records `round-198`,
     `milestone-3`,
     `direction-3a-refresh-the-p5-vs-p2-gap-ledger`,
     `attempt-1`,
     `retry: null`,
     and the live subject as one docs-only reread of the post-milestone-2
     `P5` vs `P2` remaining frontier only.
   - The document must include, in order:
     a stage-contract freeze;
     one authority / evidence ledger;
     one refreshed two-row comparison ledger that keeps `P5` and `P2`
     separate;
     one remaining-frontier evaluation matrix;
     one section naming exactly one remaining-frontier conclusion; and
     one non-claims section.
   - In the authority / evidence ledger, cite the exact settled sources
     without widening them:
     `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`,
     `orchestrator/rounds/round-193/review-record.json`,
     `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`,
     `orchestrator/rounds/round-191/review-record.json`,
     `orchestrator/rounds/round-181/review-record.json`,
     `orchestrator/rounds/round-181/implementation-notes.md`,
     `docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md`,
     and `orchestrator/rounds/round-197/review-record.json`.
   - In the refreshed `P5` row, state the exact settled milestone-2 claim
     only:
     `sameLaneAliasFrameClearBoundaryExpr` now has bounded
     current-architecture support on `runPipelineElab` /
     `runPipelineElabChecked`,
     `nestedForallContrastExpr` remains fail-closed with
     `PhiTranslatabilityError`,
     and the merged payload stayed `test-only`.
     Then state the limiting claim explicitly:
     this is one retained-child clear-boundary lane only, not general `P5`
     family closure and not a repo-level readiness result.
   - In the preserved `P2` row, restate the exact accepted folklore read
     only:
     the `C1` non-local scheme-alias / base-like packet remains recursively
     visible on `runPipelineElab` / `runPipelineElabChecked`,
     the fallback `baseTarget -> baseC` surface remains the packet boundary,
     and the accepted aggregate classification remains
     `packet-specific folklore` because no second non-local packet, adjacent
     chain, or family frontier has been added.
   - In the remaining-frontier evaluation matrix, compare only two lawful
     routing outcomes:
     `P5 remains the stronger blocker / pressure source`
     versus
     `P2 is now the next lawful bounded follow-on`.
     Evaluate each against the fixed accepted ledger above, not against new
     implementation ideas.
   - If the ledger still leaves `P5` stronger, say exactly why:
     milestone-2 settled one clear-boundary lane, but the broader
     `P5 polymorphism-nested-forall` family still lacks reusable positive
     support and still preserves fail-closed quantified-crossing contrast.
     Route only to the later `direction-3c-record-p5-dominant-boundary-pressure`
     follow-on; do not open `direction-3b`.
   - If the ledger instead makes `P2` next, say exactly why the settled `P5`
     baseline no longer dominates, then route only to
     `direction-3b-freeze-one-bounded-p2-follow-on-lane`.
     Do not promote the accepted exact `C1` packet into family closure while
     making that call.
   - End with exactly one remaining-frontier conclusion and exactly one next
     lawful milestone-3 routing consequence inside the same artifact. Do not
     author a separate handoff file.
   - Verification:
     `test -f docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md`
     `rg -n 'round-198|direction-3a-refresh-the-p5-vs-p2-gap-ledger|sameLaneAliasFrameClearBoundaryExpr|nestedForallContrastExpr|PhiTranslatabilityError|C1|packet-specific folklore|runPipelineElab|runPipelineElabChecked' docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md`
     `python3 - <<'PY'\nimport pathlib, sys\nartifact = pathlib.Path('docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md').read_text()\nrequired = [\n    'round-198',\n    'milestone-3',\n    'direction-3a-refresh-the-p5-vs-p2-gap-ledger',\n    'sameLaneAliasFrameClearBoundaryExpr',\n    'nestedForallContrastExpr',\n    'PhiTranslatabilityError',\n    'C1',\n    'packet-specific folklore',\n    'runPipelineElab',\n    'runPipelineElabChecked',\n    'round-193',\n    'round-197',\n]\nfor token in required:\n    if token not in artifact:\n        print(f'missing token: {token}')\n        sys.exit(1)\nphrases = [\n    'P5 remains the stronger blocker / pressure source',\n    'P2 is now the next lawful bounded follow-on',\n]\ncount = sum(phrase in artifact for phrase in phrases)\nif count != 1:\n    print('artifact must contain exactly one remaining-frontier conclusion phrase')\n    sys.exit(1)\nprint('ROUND198_REMAINING_FRONTIER_LEDGER_OK')\nPY`

3. Verify docs-only scope and exact round closure.
   - Keep the authored diff limited to the one milestone-3 reread artifact
     plus the round-owned `selection.md` / `plan.md`.
   - Run diff hygiene and scope checks. If any `src/`, `src-public/`, `app/`,
     `test/`, `mlf2.cabal`, roadmap, controller-state, `TODO.md`,
     `implementation_notes.md`, `Bugs.md`, or extra `docs/plans/**` path
     appears in the round diff, stop and remove it rather than widening the
     round.
   - Verification:
     `git diff --check`
     `python3 - <<'PY'\nimport subprocess, sys\nartifact = 'docs/plans/2026-04-06-post-item-7-p5-vs-p2-remaining-frontier-ledger.md'\nallowed = {\n    artifact,\n    'orchestrator/rounds/round-198/selection.md',\n    'orchestrator/rounds/round-198/plan.md',\n}\ntracked = subprocess.check_output(['git', 'diff', '--name-only', 'HEAD'], text=True).splitlines()\nuntracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines()\npaths = [p for p in tracked + untracked if p]\nextra = [p for p in paths if p not in allowed and p != 'orchestrator/state.json']\nforbidden = [\n    p for p in paths\n    if p == 'mlf2.cabal'\n    or p.startswith('src/')\n    or p.startswith('src-public/')\n    or p.startswith('app/')\n    or p.startswith('test/')\n    or p in {'TODO.md', 'implementation_notes.md', 'Bugs.md'}\n    or (p.startswith('docs/') and p != artifact)\n    or p.startswith('orchestrator/roadmaps/')\n]\nif forbidden or extra:\n    if forbidden:\n        print('FORBIDDEN_PATHS:')\n        print('\\n'.join(forbidden))\n    if extra:\n        print('OUT_OF_SCOPE_PATHS:')\n        print('\\n'.join(extra))\n    sys.exit(1)\nprint('ROUND198_DOCS_ONLY_SCOPE_OK')\nPY`

## Review Focus

- The diff stays bounded to one reread artifact plus round-owned
  `selection.md` / `plan.md`; no code, test, roadmap, or extra docs path
  appears.
- The artifact distinguishes the refreshed milestone-2 `P5` settlement read
  from the accepted `P2 packet-specific folklore` ledger explicitly rather
  than collapsing them into one blended claim.
- Any conclusion that opens `P2` still names one exact future non-local lane
  only and does not promote the accepted `C1` packet into family closure.
- Any conclusion that keeps `P5` dominant stays routing-only and does not
  smuggle in repo-level readiness or an immediate boundary-revision decision.
