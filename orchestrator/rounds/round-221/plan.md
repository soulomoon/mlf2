# Round 221 Plan

- Round: `round-221`
- Roadmap:
  `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
  / `rev-026`
- Milestone: `milestone-4`
- Direction: `direction-4a-publish-broader-positive-enactment-closeout`
- Extracted item:
  `publish-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier`
- Retry: `null`
- Execution shape: serial, docs-only, milestone-4 closeout-only, one
  canonical closeout artifact plus the required repo-facing note sync, no
  worker fan-out, and no production/test/Cabal/roadmap/controller-state edits

## Objective

Publish the canonical milestone-4 closeout on top of merged base-branch
`HEAD = ea8db76`:

`docs/plans/2026-04-10-p5-polymorphism-nested-forall-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier.md`

and sync the repo-facing notes that `rev-026` assigns to milestone-4:

- `TODO.md`
- `implementation_notes.md`
- `CHANGELOG.md`

The closeout must record exactly one enacted broader-positive frontier only:
the selected same-wrapper nested-`forall` packet plus the explicit
clear-boundary anchors from `sameLaneClearBoundaryExpr` through
`sameLaneNonupleAliasFrameClearBoundaryExpr` on both `runPipelineElab` and
`runPipelineElabChecked`, while keeping
`sameLaneAliasFrameClearBoundaryExpr` as predecessor truth only and keeping
the accepted decuple/deeper alias frontier, `P2`, `N1 ambiguity-reject`,
`N2 unsoundness-guard`, and `N6 termination-pressure` closed.

This round must stay docs-only, serial, closeout-only, and non-widening. It
must not reopen milestone-3 publication work, production/test changes,
thesis-deviation bookkeeping unless the accepted evidence truly requires it,
or any boundary claim beyond the merged `ea8db76` record.

## Authorized Write Scope

Implementation-owned writes for this round are limited to:

- `docs/plans/2026-04-10-p5-polymorphism-nested-forall-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier.md`
- `TODO.md`
- `implementation_notes.md`
- `CHANGELOG.md`
- `orchestrator/rounds/round-221/implementation-notes.md`
  only if the implementer needs a round-local summary and keeps it strictly
  derivative of the canonical closeout plus repo-facing note sync

Round-owned artifacts that may appear later in the packet and must not be
misclassified as extra milestone docs are:

- `orchestrator/rounds/round-221/review.md`
- `orchestrator/rounds/round-221/review-record.json`
- `orchestrator/rounds/round-221/merge.md`

Do not create or modify:

- `orchestrator/rounds/round-221/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmap.md`
- `orchestrator/verification.md`
- `orchestrator/retry-subloop.md`
- `orchestrator/roadmaps/**`
- `docs/thesis-deviations.yaml`
- `Bugs.md`
- `README.md`
- any accepted predecessor `docs/plans/**` artifact
- any file under `src/`, `src-public/`, `app/`, or `test/`
- `test/Main.hs`
- `mlf2.cabal`
- any second `docs/plans/**` artifact, thesis-facing addendum, roadmap
  amendment, or follow-on family-routing note

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input:
  `orchestrator/rounds/round-221/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`,
  `roadmap_revision = rev-026`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-026`
- Current merged baseline:
  `HEAD`,
  `codex/automatic-recursive-type-inference`, and
  `git merge-base HEAD codex/automatic-recursive-type-inference`
  all resolve to `ea8db763ded783654c4eab40bd15dd070a2724dc`

Current worktree state is already non-pristine. Respect unrelated and
controller-owned edits and do not revert them:

- `M orchestrator/state.json`
- `M orchestrator/roadmap.md`
- `M orchestrator/verification.md`
- `M orchestrator/retry-subloop.md`
- `?? orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-026/`
- `?? orchestrator/rounds/round-221/selection.md`

Accepted `round-206` is the controlling family contract freeze. Its canonical
artifact
`docs/plans/2026-04-08-p5-polymorphism-nested-forall-broader-positive-enactment-family-contract-authoritative-frontier-representative-corpus-and-writable-slice-freeze.md`
and approved `orchestrator/rounds/round-206/review-record.json` fix the
broader-positive frontier, the authoritative entrypoints, the preserved
closed guardrails, and the fact that milestone-4 owns `TODO.md`,
`implementation_notes.md`, and `CHANGELOG.md` after milestone-3 evidence is
accepted.

Accepted `round-220`, now merged as `ea8db76`, is the controlling milestone-3
publication result. Its approved `orchestrator/rounds/round-220/review-record.json`,
`orchestrator/rounds/round-220/merge.md`, and
`orchestrator/rounds/round-220/implementation-notes.md` establish that:

- `sameLaneNonupleAliasFrameClearBoundaryExpr` is explicit on the research,
  pipeline, and elaboration surfaces;
- the selected same-wrapper nested-`forall` packet remains successful on both
  authoritative entrypoints;
- `./scripts/thesis-conformance-gate.sh` passed; and
- `cabal build all && cabal test` passed with `1365 examples, 0 failures`.

Accepted `round-197` remains the controlling predecessor-truth settlement for
`sameLaneAliasFrameClearBoundaryExpr`: one settled retained-child lane only,
not whole-family closure.

Accepted `round-190` and accepted `round-191` remain the controlling frontier
honesty record: the alias-through-nonuple chain is the bounded positive
support, the fresh decuple probe stays fail-closed on both authoritative
entrypoints, and deeper alias shells remain outside the live positive
extraction.

The current top-level `TODO.md`, `implementation_notes.md`, and
`CHANGELOG.md` still lack the milestone-4 closeout summary that the accepted
`rev-026` contract assigns to this round.

The read-only authority chain for this round is:

- `orchestrator/rounds/round-221/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-026/roadmap.md`
- `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-026/verification.md`
- `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-026/retry-subloop.md`
- `docs/plans/2026-04-08-p5-polymorphism-nested-forall-broader-positive-enactment-family-contract-authoritative-frontier-representative-corpus-and-writable-slice-freeze.md`
- `docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-broader-positive-p5-ledger-under-the-revised-freeze.md`
- `docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md`
- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`
- `orchestrator/rounds/round-206/review-record.json`
- `orchestrator/rounds/round-197/review-record.json`
- `orchestrator/rounds/round-220/review-record.json`
- `orchestrator/rounds/round-220/merge.md`
- `orchestrator/rounds/round-220/implementation-notes.md`
- `orchestrator/rounds/round-190/review-record.json`
- `orchestrator/rounds/round-191/review-record.json`
- `orchestrator/rounds/round-191/merge.md`
- `TODO.md`
- `implementation_notes.md`
- `CHANGELOG.md`
- `test/Research/P5ClearBoundarySpec.hs`
- `test/PipelineSpec.hs`
- `test/ElaborationSpec.hs`

## Sequential Plan

### Task 1: Author the canonical milestone-4 closeout artifact

**Files:**

- Create:
  `docs/plans/2026-04-10-p5-polymorphism-nested-forall-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier.md`
- Verify-only:
  `orchestrator/rounds/round-221/selection.md`
- Verify-only:
  `orchestrator/state.json`
- Verify-only:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-026/roadmap.md`
- Verify-only:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-026/verification.md`
- Verify-only:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-026/retry-subloop.md`
- Verify-only:
  `docs/plans/2026-04-08-p5-polymorphism-nested-forall-broader-positive-enactment-family-contract-authoritative-frontier-representative-corpus-and-writable-slice-freeze.md`
- Verify-only:
  `docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-broader-positive-p5-ledger-under-the-revised-freeze.md`
- Verify-only:
  `docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md`
- Verify-only:
  `orchestrator/rounds/round-206/review-record.json`
- Verify-only:
  `orchestrator/rounds/round-197/review-record.json`
- Verify-only:
  `orchestrator/rounds/round-220/review-record.json`
- Verify-only:
  `orchestrator/rounds/round-220/merge.md`
- Verify-only:
  `orchestrator/rounds/round-220/implementation-notes.md`
- Verify-only:
  `orchestrator/rounds/round-190/review-record.json`
- Verify-only:
  `orchestrator/rounds/round-191/review-record.json`
- Verify-only:
  `test/Research/P5ClearBoundarySpec.hs`
- Verify-only:
  `test/PipelineSpec.hs`
- Verify-only:
  `test/ElaborationSpec.hs`

- [ ] Create a stage-contract header that records `round-221`,
      `milestone-4`,
      `direction-4a-publish-broader-positive-enactment-closeout`,
      `publish-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier`,
      `attempt-1`,
      `retry: null`,
      and the live subject as one docs-only milestone-4 closeout on top of
      merged `ea8db76`.
- [ ] Require the document sections, in order:
      `## Stage Contract Freeze`,
      `## Closeout Authority Ledger`,
      `## Enacted Broader-Positive Frontier On The Merged ea8db76 Baseline`,
      `## Preserved Closed Boundaries And Excluded Families`,
      `## Repo-Facing Closeout Consequences`,
      and `## Non-Claims`.
- [ ] In `## Closeout Authority Ledger`, cite the exact accepted lineage only:
      `rev-026` selection/state;
      the milestone-1 freeze from `round-206`;
      the predecessor-truth retained-child settlement from `round-197`;
      the merged nonuple-baseline evidence from `round-220`; and
      the bounded frontier-honesty record from `round-190` / `round-191`.
- [ ] In
      `## Enacted Broader-Positive Frontier On The Merged ea8db76 Baseline`,
      record exactly this enacted support and nothing broader:
      the selected same-wrapper nested-`forall` packet plus the explicit
      clear-boundary anchors from `sameLaneClearBoundaryExpr` through
      `sameLaneNonupleAliasFrameClearBoundaryExpr` are now reviewer-visible on
      both `runPipelineElab` and `runPipelineElabChecked`;
      accepted `round-220` carried
      `./scripts/thesis-conformance-gate.sh` and
      `cabal build all && cabal test` with `1365 examples, 0 failures`;
      and no further milestone-3 publication debt remains live on top of
      `ea8db76`.
- [ ] In `## Preserved Closed Boundaries And Excluded Families`, state
      explicitly that
      `sameLaneAliasFrameClearBoundaryExpr` remains predecessor truth only;
      `sameLaneDecupleAliasFrameClearBoundaryExpr` remains fail-closed on both
      authoritative entrypoints;
      deeper alias shells stay outside the live extraction;
      `P2`, `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
      `N6 termination-pressure` remain closed;
      and no cyclic, multi-SCC, equi-recursive, fallback, or second-interface
      widening is authorized.
- [ ] In `## Repo-Facing Closeout Consequences`, limit the follow-through to
      the accepted note surfaces only:
      `TODO.md`,
      `implementation_notes.md`, and
      `CHANGELOG.md`.
      State directly that `docs/thesis-deviations.yaml` stays unchanged unless
      the accepted evidence forces a new thesis-facing claim; the current
      closeout read is that no new deviation record is needed because this
      round republishes already accepted evidence rather than a new semantic
      extension.
- [ ] In `## Non-Claims`, forbid overclaiming beyond the merged nonuple
      frontier, forbid relabeling decuple/deeper alias shells as positive
      support, forbid reopening milestone-3 publication or production/test
      work, and forbid turning the closeout into a fresh readiness or
      boundary-revision decision.

**Verification commands:**

```bash
test "$(git rev-parse HEAD)" = "ea8db763ded783654c4eab40bd15dd070a2724dc"
python3 -m json.tool orchestrator/state.json >/dev/null
roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test "$roadmap_dir" = "orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-026" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/verification.md" && test -f "$roadmap_dir/retry-subloop.md"
rg -n '^## (Stage Contract Freeze|Closeout Authority Ledger|Enacted Broader-Positive Frontier On The Merged ea8db76 Baseline|Preserved Closed Boundaries And Excluded Families|Repo-Facing Closeout Consequences|Non-Claims)$' docs/plans/2026-04-10-p5-polymorphism-nested-forall-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier.md
rg -n 'round-206|round-197|round-220|round-190|round-191|ea8db76|sameLaneClearBoundaryExpr|sameLaneAliasFrameClearBoundaryExpr|sameLaneNonupleAliasFrameClearBoundaryExpr|sameLaneDecupleAliasFrameClearBoundaryExpr|runPipelineElab|runPipelineElabChecked|1365 examples, 0 failures|thesis-conformance-gate.sh|P2|N1 ambiguity-reject|N2 unsoundness-guard|N6 termination-pressure|docs/thesis-deviations.yaml' docs/plans/2026-04-10-p5-polymorphism-nested-forall-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier.md orchestrator/rounds/round-206/review-record.json orchestrator/rounds/round-197/review-record.json orchestrator/rounds/round-220/review-record.json orchestrator/rounds/round-220/merge.md orchestrator/rounds/round-190/review-record.json orchestrator/rounds/round-191/review-record.json
rg -n 'selected same-wrapper nested-forall preserved merged-baseline packet stays recursive on both authoritative entrypoints|sameLaneClearBoundaryExpr is the first explicit milestone-3 representative broader-positive clear-boundary packet on both authoritative entrypoints|sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints|sameLaneNonupleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet after the merged octuple-alias anchor on both authoritative entrypoints|sameLaneDecupleAliasFrameClearBoundaryExpr|sameLaneNonupleAliasFrameClearBoundaryExpr exact edge authoritative instantiation translation' test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs test/ElaborationSpec.hs
```

### Task 2: Sync repo-facing notes to the accepted closeout and nothing more

**Files:**

- Modify:
  `TODO.md`
- Modify:
  `implementation_notes.md`
- Modify:
  `CHANGELOG.md`
- Verify-only:
  `docs/plans/2026-04-10-p5-polymorphism-nested-forall-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier.md`

- [ ] In `TODO.md`, insert a new completed section above `## Task 107 ...`
      with the exact heading
      `## Task 108 P5 broader-positive enactment closeout (completed 2026-04-10)`.
      The body should record:
      `round-206` froze the contract;
      accepted rounds `round-211` through `round-220`, now merged through
      `ea8db76`, made the selected same-wrapper nested-`forall` packet plus
      `sameLaneClearBoundaryExpr` through
      `sameLaneNonupleAliasFrameClearBoundaryExpr` reviewer-visible on both
      authoritative entrypoints;
      milestone-4 closeout keeps
      `sameLaneAliasFrameClearBoundaryExpr` as predecessor truth only and
      keeps decuple/deeper alias shells plus `P2` / `N1` / `N2` / `N6`
      closed;
      and no new thesis-deviation record is required.
      Keep the task marked complete and do not add a fresh rolling-priority
      queue inside this family.
- [ ] In `implementation_notes.md`, prepend a new top entry with the exact
      heading
      `## 2026-04-10 - P5 broader-positive enactment family closed on the merged nonuple frontier`.
      The bullets must say that the family now closes on merged `ea8db76`,
      that the enacted positive frontier is the selected same-wrapper
      nested-`forall` packet plus the explicit clear-boundary anchors from
      `sameLaneClearBoundaryExpr` through
      `sameLaneNonupleAliasFrameClearBoundaryExpr` on both authoritative
      entrypoints,
      that `sameLaneAliasFrameClearBoundaryExpr` remains predecessor truth
      only,
      that decuple/deeper alias shells and `P2` / `N1` / `N2` / `N6` stay
      closed,
      and that the closeout republishes already accepted evidence only, so
      `docs/thesis-deviations.yaml` remains unchanged.
- [ ] In `CHANGELOG.md`, add one new bullet at the top of
      `## Unreleased` / `### Changed` that summarizes the full family closeout:
      the enactment contract froze in `round-206`,
      milestone-3 reached the merged nonuple frontier in `round-220`,
      the canonical closeout now records the selected same-wrapper
      nested-`forall` packet plus
      `sameLaneClearBoundaryExpr` through
      `sameLaneNonupleAliasFrameClearBoundaryExpr` as the current
      broader-positive frontier on both authoritative entrypoints,
      `sameLaneAliasFrameClearBoundaryExpr` stays predecessor truth only,
      decuple/deeper alias shells stay closed,
      and the repo-facing notes were synced without adding a new
      thesis-deviation entry.
- [ ] Keep the repo-facing note sync bounded to those three files only.
      Do not modify `docs/thesis-deviations.yaml`, `README.md`, `Bugs.md`, or
      any accepted predecessor artifact.

**Verification commands:**

```bash
rg -n 'Task 108 P5 broader-positive enactment closeout \(completed 2026-04-10\)|ea8db76|sameLaneAliasFrameClearBoundaryExpr|sameLaneNonupleAliasFrameClearBoundaryExpr|sameLaneDecupleAliasFrameClearBoundaryExpr|P2|N1 ambiguity-reject|N2 unsoundness-guard|N6 termination-pressure|1365 examples, 0 failures|thesis-conformance-gate.sh|no new thesis-deviation record is required|docs/thesis-deviations.yaml remains unchanged' TODO.md implementation_notes.md CHANGELOG.md
rg -n 'broader-positive enactment closeout for the merged nonuple frontier|sameLaneClearBoundaryExpr|sameLaneAliasFrameClearBoundaryExpr|sameLaneNonupleAliasFrameClearBoundaryExpr|sameLaneDecupleAliasFrameClearBoundaryExpr' docs/plans/2026-04-10-p5-polymorphism-nested-forall-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier.md TODO.md implementation_notes.md CHANGELOG.md
```

## Final Verification

After Tasks 1 and 2 land, verify the round remains a docs-only milestone-4
closeout and that the note sync matches the canonical artifact.

```bash
git diff --check -- docs/plans/2026-04-10-p5-polymorphism-nested-forall-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier.md TODO.md implementation_notes.md CHANGELOG.md orchestrator/rounds/round-221/implementation-notes.md
git status --short --untracked-files=all
python3 - <<'PY'
import subprocess, sys
artifact = 'docs/plans/2026-04-10-p5-polymorphism-nested-forall-broader-positive-enactment-closeout-for-the-merged-nonuple-frontier.md'
allowed = {
    artifact,
    'TODO.md',
    'implementation_notes.md',
    'CHANGELOG.md',
    'orchestrator/rounds/round-221/selection.md',
    'orchestrator/rounds/round-221/plan.md',
    'orchestrator/rounds/round-221/implementation-notes.md',
    'orchestrator/rounds/round-221/review.md',
    'orchestrator/rounds/round-221/review-record.json',
    'orchestrator/rounds/round-221/merge.md',
}
controller_owned = {
    'orchestrator/state.json',
    'orchestrator/roadmap.md',
    'orchestrator/verification.md',
    'orchestrator/retry-subloop.md',
}
allowed_prefixes = (
    'orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-026/',
)
tracked = subprocess.check_output(['git', 'diff', '--name-only', 'HEAD'], text=True).splitlines()
untracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines()
paths = sorted({p for p in tracked + untracked if p})
extra = [
    p for p in paths
    if p not in allowed
    and p not in controller_owned
    and not any(p.startswith(prefix) for prefix in allowed_prefixes)
]
forbidden = [
    p for p in paths
    if p == 'mlf2.cabal'
    or p.startswith('src/')
    or p.startswith('src-public/')
    or p.startswith('app/')
    or p.startswith('test/')
    or p in {'Bugs.md', 'README.md', 'docs/thesis-deviations.yaml'}
    or (p.startswith('docs/') and p != artifact)
]
if forbidden or extra:
    if forbidden:
        print('FORBIDDEN_PATHS:')
        print('\n'.join(forbidden))
    if extra:
        print('OUT_OF_SCOPE_PATHS:')
        print('\n'.join(extra))
    sys.exit(1)
print('ROUND221_CLOSEOUT_SCOPE_OK')
PY
```

No fresh `cabal build all && cabal test` rerun or fresh
`./scripts/thesis-conformance-gate.sh` rerun is required in this round
because the allowed diff must stay docs-only. The milestone-4 artifact and
repo-facing note sync must instead cite the accepted merged `round-220`
verification record honestly.

## Verification Intent

Before review, the implementer should be able to show all of the following
from the authored artifact and note sync:

- the canonical closeout is grounded in merged `ea8db76`, not in stale
  pre-merge or recovery lineage
- the artifact names exactly one enacted broader-positive frontier on both
  authoritative entrypoints:
  the selected same-wrapper nested-`forall` packet plus
  `sameLaneClearBoundaryExpr` through
  `sameLaneNonupleAliasFrameClearBoundaryExpr`
- `sameLaneAliasFrameClearBoundaryExpr` remains predecessor truth only
- `sameLaneDecupleAliasFrameClearBoundaryExpr`, deeper alias shells, `P2`,
  `N1 ambiguity-reject`, `N2 unsoundness-guard`, and `N6 termination-pressure`
  remain closed
- `TODO.md`, `implementation_notes.md`, and `CHANGELOG.md` summarize the
  closeout without inventing a new follow-on queue inside this family
- `docs/thesis-deviations.yaml` stays unchanged because the round republishes
  already accepted evidence only
- the round stays docs-only and does not widen into production, tests, Cabal,
  roadmap edits, or controller-owned state
