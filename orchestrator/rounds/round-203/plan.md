# Round 203 Plan

- Round: `round-203`
- Roadmap:
  `2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap`
  / `rev-002`
- Milestone: `milestone-1`
- Direction:
  `direction-1a-refreeze-inherited-boundary-after-reopening-polymorphic-mediation-semantics`
- Extracted item:
  `refreeze-inherited-boundary-after-reopening-polymorphic-mediation-semantics`
- Retry: `null`
- Execution shape: serial, docs-only, planning-only, one milestone-1 refreeze
  artifact only, no worker fan-out, and no source/test/Cabal changes

## Objective

Publish one canonical milestone-1 refreeze artifact only:

`docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-round-151-polymorphic-mediation-mu-preservation-reclassification-and-inherited-boundary-refreeze.md`

That artifact must turn the active `rev-002` roadmap into one exact
milestone-1 refreeze that:

- explicitly supersedes the `rev-001` / `round-202` controlling freeze only
  where it closed the round-151 polymorphic-mediation `mu`-absorption story
  as settled predecessor truth;
- states the exact reopened semantic question around
  `nestedForallContrastExpr`-class broader positive `P5` cases;
- freezes the exact inherited boundary clauses now under pressure, the exact
  preserved predecessor truth that stays closed, the exact live broader
  positive `P5` subject, and the exact lawful later outcomes for `rev-002`;
- keeps
  `recursive meaning remains iso-recursive only`,
  `non-equi-recursive = keep`,
  `non-cyclic-graph = unknown`,
  `no-fallback = keep`,
  and `no second interface is authorized`
  out of silently reopened live debt unless the refreeze explicitly says
  otherwise; and
- stops before any milestone-2 broader ledger, any
  current-boundary-versus-explicit-revision comparison, and any milestone-3
  downstream handoff.

## Authorized Write Scope

Implementation-owned writes for this round are limited to:

- `docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-round-151-polymorphic-mediation-mu-preservation-reclassification-and-inherited-boundary-refreeze.md`
- `orchestrator/rounds/round-203/implementation-notes.md`
  only if the implementer needs a round-local summary and keeps it strictly
  derivative of the canonical docs artifact

Round-owned artifacts that may appear later in the packet and must not be
misclassified as extra milestone docs are:

- `orchestrator/rounds/round-203/review.md`
- `orchestrator/rounds/round-203/review-record.json`
- `orchestrator/rounds/round-203/merge.md`

Do not create or modify:

- `orchestrator/rounds/round-203/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- `README.md`
- any accepted predecessor `docs/plans/**` artifact
- any file under `src/`, `src-public/`, `app/`, or `test/`
- `mlf2.cabal`
- `orchestrator/rounds/round-203/worker-plan.json`
- any second `docs/plans/**` artifact, milestone-2 ledger, comparison artifact,
  downstream handoff artifact, roadmap-amendment draft, or implementation
  slice note

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input:
  `orchestrator/rounds/round-203/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap`,
  `roadmap_revision = rev-002`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap/rev-002`

Current worktree state is already non-pristine. Respect unrelated edits and do
not revert them:

- `M orchestrator/state.json` is controller-owned and must remain untouched.
- `?? orchestrator/rounds/round-203/selection.md` is the round input and must
  remain untouched after this plan lands.

Accepted `round-202` is the binding immediate predecessor freeze for the
historical `rev-001` surface, but it is no longer the controlling family read
on round-151 semantics. Its canonical artifact
`docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-inherited-boundary-pressure-and-planning-only-decision-surface-freeze.md`
must now be treated as predecessor evidence that `rev-002` supersedes on that
one semantic point while preserving every historical artifact unchanged.

The preserved supporting lineage that must stay visible in this round is:

- accepted `round-201` and `round-200`, which keep
  `explicit boundary-revision candidate`
  as the family-entry planning pressure only;
- accepted `round-197`, which keeps
  `sameLaneAliasFrameClearBoundaryExpr`
  as one settled retained-child clear-boundary success on
  `runPipelineElab` / `runPipelineElabChecked`,
  while `nestedForallContrastExpr` remains fail-closed with
  `PhiTranslatabilityError`;
- accepted `round-191` and accepted `round-181`, which keep `P2` packet-
  bounded to the exact `C1` packet only; and
- accepted `round-192`, which keeps
  `N1 ambiguity-reject`,
  `N2 unsoundness-guard`, and
  `N6 termination-pressure`
  closed as predecessor truth.

The reopened pressure that must be handled explicitly in the refreeze is:

- accepted `round-151` was docs-only and changed wording only;
- `implementation_notes.md` uses `let rec f = id f` as the motivating
  polymorphic-mediation example;
- `nestedForallContrastExpr` is instead the concrete repo witness term for the
  broader positive `P5` contrast, and the refreeze must state whether that
  specific quantified-crossing pattern keeps `mu` live, absorbs it lawfully,
  or remains unresolved under the inherited boundary.

The read-only authority chain for this round is:

- `orchestrator/rounds/round-203/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap/rev-002/roadmap.md`
- `orchestrator/roadmaps/2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap/rev-002/verification.md`
- `orchestrator/roadmaps/2026-04-07-00-p5-polymorphism-nested-forall-explicit-boundary-revision-roadmap/rev-002/retry-subloop.md`
- `docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-inherited-boundary-pressure-and-planning-only-decision-surface-freeze.md`
- `docs/plans/2026-04-07-post-item-7-explicit-boundary-revision-candidate-final-handoff-to-one-planning-only-p5-polymorphism-nested-forall-boundary-revision-family.md`
- `docs/plans/2026-04-07-post-item-7-refreshed-repo-level-readiness-and-architecture-decision-from-the-updated-p5-vs-p2-ledger-and-preserved-negative-family-settlements.md`
- `docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md`
- `orchestrator/rounds/round-202/review-record.json`
- `orchestrator/rounds/round-151/review.md`
- `orchestrator/rounds/round-151/review-record.json`
- `implementation_notes.md`
- `test/Research/P5ClearBoundarySpec.hs`

## Sequential Plan

1. Freeze the revised milestone-1 refreeze contract before drafting or editing
   anything.
   - Modify no files in this step.
   - End this step with one fixed frame only:
     accepted `round-202` remains historical predecessor evidence,
     accepted `round-197` keeps the retained-child lane settled,
     accepted `round-191` / `round-181` keep `P2` packet-bounded,
     accepted `round-192` keeps the representative negative-family rows
     closed,
     accepted `round-151` remains docs-only historical evidence only,
     and `rev-002` reopens the exact nested-forall polymorphic-mediation
     `mu`-preservation question for refreeze.
2. Draft the canonical docs artifact.
   - Write only the canonical `docs/plans/**` artifact named above.
   - Required sections:
     `## Stage Contract Freeze`,
     `## Accepted Predecessor Authority Ledger`,
     `## Exact Reopened Semantic Question`,
     `## Exact Inherited Boundary Clauses Under Pressure`,
     `## Preserved Predecessor Truth That Stays Closed`,
     `## Exact Live Broader Positive P5 Subject`,
     `## Exact Planning-Only Decision Surface`,
     and `## Rev-002 No-Go Claims`.
3. Self-check the artifact against the active revision before review.
   - Confirm the artifact does not silently rewrite `round-151`,
     `round-202`, or any historical `docs/plans/**` file.
   - Confirm the artifact stops before milestone-2 ledger construction,
     comparison, or downstream handoff.
   - Confirm the diff remains docs-only and planning-only.

## Verification Intent

Before review, the implementer should be able to show all of the following
from the authored artifact and round diff:

- `rev-002` rather than `rev-001` controls the active round lineage
- the artifact explicitly names the reopened round-151 semantic question
- the artifact preserves `P2`, the representative negative-family rows, and
  the settled retained-child lane as closed predecessor truth
- the artifact remains docs-only and does not widen into code, tests, Cabal,
  or implementation authorization
