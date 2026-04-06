# General Automatic Iso-Recursive P5 And P2 Follow-On Roadmap

Roadmap family: `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap`
Revision: `rev-001`
Base branch: `codex/automatic-recursive-type-inference`
Created: 2026-04-05

## Goal

Advance from the accepted `round-193 = continue-bounded` decision toward a
stronger repo-level answer on full automatic iso-recursive type inference by:

- freezing the exact still-live post-item-7 `P5 polymorphism-nested-forall`
  successor lane;
- determining whether that lane still belongs to the inherited current
  architecture or matures into an explicit boundary-pressure candidate;
- routing the remaining positive-family frontier back through
  `P2 non-local-propagation` only after the refreshed `P5` read is honest; and
- reissuing the repo-level readiness / architecture decision only when the
  updated `P5` / `P2` ledger is strong enough to support it.

## Outcome Boundaries

- Preserve the inherited production / architecture boundary unless a later
  accepted milestone explicitly earns revision:
  explicit recursive annotations remain the production baseline,
  recursive meaning remains iso-recursive only,
  `non-equi-recursive = keep`,
  `non-cyclic-graph = unknown`,
  `no-fallback = keep`,
  no second interface is authorized,
  and no cyclic or multi-SCC widening is authorized.
- Keep accepted predecessor truth closed:
  the March 28 exact `nestedForallContrastExpr` packet,
  the accepted same-lane alias-through-nonuple chain,
  the exact `C1` packet,
  the item-5 / item-6 aggregates,
  and the accepted round-193 repo-level decision are authority inputs, not
  live debt to relitigate.
- Preserve the accepted round-151 reclassification:
  nested-forall `mu` absorption under polymorphic mediation is known correct
  behavior, not itself the live blocker. The live question is whether any
  bounded positive `P5` lane remains beyond that settled exact packet.
- Treat `runPipelineElab`, `runPipelineElabChecked`, and the matching
  internal / public pipeline facades fixed by the accepted item-4 readiness
  contract as the authoritative success surfaces.
- Keep `N3` through `N5` out of scope unless a later accepted revision
  explicitly reopens them.
- Planning-only gates may freeze lanes, rubrics, and successor choices, but
  they may not pre-authorize implementation, hardening, or boundary revision.

## Global Sequencing Rules

- `milestone-1` must complete before any fresh `P5` implementation slice or
  boundary-pressure claim. It owns the exact follow-on lane selection and
  writable-slice freeze.
- `rev-001` stays serial. No parallel round extraction is pre-authorized while
  post-item-7 `P5` authority and post-`P5` routing are still coupled.
- Any explicit boundary-revision candidate must first land as a docs-only
  decision surface grounded in accepted `P5` / `P2` evidence. No production
  widening may precede that decision.
- Do not advance into a fresh `P2` follow-on until the post-item-7 `P5` lane
  has either been settled inside the current architecture or classified as the
  stronger blocker / boundary-pressure source.
- Revisit repo-level readiness only after the refreshed positive-family ledger
  is strong enough to compare against the accepted item-4
  representative-authoritative bar without guesswork.

## Parallel Lanes

- `lane-main`: default serial lane for every `rev-001` milestone. All
  extraction stays here until a later accepted revision explicitly opens
  independent co-runnable fronts.

## Milestones

### 1. [done] Freeze and classify the post-item-7 `P5` successor gate and exact live blocker ledger

- Milestone id: `milestone-1`
- Depends on: none
- Intent: turn the accepted round-193 planning-only handoff into one exact
  post-item-7 `P5` successor control surface that distinguishes settled
  predecessor packets from the still-live positive-family blocker, names one
  exact follow-on lane, and then classifies whether that frozen lane still
  reads as bounded current-architecture continuation or later
  boundary-pressure.
- Completion signal: an accepted docs-only authority / decision artifact fixes
  the direct post-item-7 `P5` decision ledger, names one exact successor lane
  plus writable slice, and states whether the current strongest read is
  current-architecture continuation or boundary-pressure classification.
- Parallel lane: `lane-main`
- Coordination notes: keep the March 28 exact `P5` packet and round-151
  reclassification as predecessor truth only; do not reopen settled same-lane
  or exact `P2` packets while freezing the `P5` successor gate.
- Progress notes: accepted `round-194`, merged as base commit `693444b`
  (`Freeze post-item-7 P5 successor authority, success bar, and writable slice`),
  finalized `direction-1a-freeze-p5-authority-and-success-bar` through
  `docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`
  with authoritative review in
  `orchestrator/rounds/round-194/review-record.json`. Accepted `round-195`,
  merged as base commit `182f63f`
  (`Document post-item-7 P5 current-architecture gate and lawful handoff`),
  finalized `direction-1b-publish-p5-current-architecture-vs-boundary-gate`
  through
  `docs/plans/2026-04-06-post-item-7-p5-current-architecture-vs-boundary-pressure-gate-and-immediate-handoff-decision.md`
  with authoritative review in
  `orchestrator/rounds/round-195/review-record.json`. Together those accepted
  docs-only rounds keep the March 28 exact packet and round-151
  reclassification closed as predecessor truth, freeze the retained-child
  guard-cluster lane plus the bounded writable slice, classify the direct
  post-item-7 `P5` read as `bounded current-architecture continuation`, and
  bind one bounded `milestone-2` current-architecture campaign on that frozen
  lane as the only lawful immediate handoff.

Accepted direction lineage:

- Direction id: `direction-1a-freeze-p5-authority-and-success-bar`
  Status: accepted in `round-194`, merged as `693444b`.
  Outcome: froze the exact retained-child guard-cluster `P5` lane, the
  authoritative-surface success bar on `runPipelineElab` /
  `runPipelineElabChecked`, and the bounded writable slice without
  authorizing implementation or boundary revision.

- Direction id: `direction-1b-publish-p5-current-architecture-vs-boundary-gate`
  Status: accepted in `round-195`, merged as `182f63f`.
  Outcome: selected `bounded current-architecture continuation` for the
  already frozen retained-child guard-cluster lane and bound one bounded
  `milestone-2` campaign on that exact lane and writable slice as the only
  lawful next move in `rev-001`.

### 2. [in-progress] Run one bounded post-item-7 `P5` authoritative-surface campaign

- Milestone id: `milestone-2`
- Depends on: `milestone-1`
- Intent: use the milestone-1 freeze to execute one exact post-item-7 `P5`
  lane that can either demonstrate lawful positive support on the authoritative
  surfaces or record an exact fail-closed / boundary-pressure read without
  widening the architecture.
- Completion signal: an accepted settlement surface exists for one exact
  post-item-7 `P5` lane with authoritative verification and an honest
  classification inside the inherited current architecture or as explicit
  boundary pressure. Any code / test round in this milestone passes the full
  `cabal build all && cabal test` gate.
- Parallel lane: `lane-main`
- Coordination notes: keep the same-lane chain, the exact March 28 `P5`
  packet, the accepted `P2` packet, and the negative-family settlements closed
  as predecessor or aggregate truth only.
- Progress notes: entry into `milestone-2` became lawful when accepted
  `round-195` fixed the milestone-1 outcome as
  `bounded current-architecture continuation`. Accepted `round-196`, merged
  as base commit `34f88bc`
  (`Pin the selected P5 retained-child lane to authoritative pipeline tests`),
  then finalized `direction-2a-implement-the-selected-p5-lane` with
  authoritative review in
  `orchestrator/rounds/round-196/review-record.json`. That accepted bounded
  execution slice stayed inside
  `test/Research/P5ClearBoundarySpec.hs` and `test/PipelineSpec.hs`, pinned
  the selected alias-frame retained-child specimen to `runPipelineElab` and
  `runPipelineElabChecked`, preserved `nestedForallContrastExpr` as the
  fail-closed contrast, and passed `cabal build all && cabal test`. Milestone-2
  therefore now carries accepted current-architecture execution evidence for
  the frozen lane, and
  `direction-2b-publish-post-implementation-p5-settlement` is the next
  unfinished move before any post-`P5` routing.

Accepted direction lineage:

- Direction id: `direction-2a-implement-the-selected-p5-lane`
  Status: accepted in `round-196`, merged as `34f88bc`.
  Outcome: promoted the frozen retained-child guard-cluster `P5` lane into
  reviewer-visible authoritative pipeline tests and source guards without
  widening production code, confirming bounded current-architecture support on
  `runPipelineElab` / `runPipelineElabChecked` while keeping the nested-forall
  contrast fail-closed.

Candidate directions:

- Direction id: `direction-2b-publish-post-implementation-p5-settlement`
  Summary: publish the exact post-implementation settlement surface and
  repo-impact read for the selected post-item-7 retained-child guard-cluster
  `P5` lane, grounded in the merged `round-196` authoritative-entrypoint
  evidence without silently upgrading it into general family closure.
  Why it matters now: accepted `round-196` supplied the bounded
  current-architecture execution evidence, so `rev-001` now needs one stable
  accepted settlement surface before comparing `P5` against the remaining `P2`
  frontier.
  Preconditions: accepted `round-196` / `direction-2a` is merged on the base
  branch, including the authoritative evidence in
  `test/Research/P5ClearBoundarySpec.hs`,
  `test/PipelineSpec.hs`, and
  `orchestrator/rounds/round-196/review-record.json`.
  Parallel hints: serial only; the settlement surface must consume the merged
  milestone-2 evidence before any post-`P5` routing begins.
  Boundary notes: docs-only; keep the claim bounded to the selected lane, do
  not relitigate predecessor packets, and do not jump ahead to repo-level
  readiness.
  Extraction notes: expected extracted item shape is a settlement-surface
  artifact that records the exact current-architecture impact, cites the
  merged authoritative evidence anchors, and preserves the non-claims.

- Direction id: `direction-2c-classify-p5-boundary-pressure-if-needed`
  Summary: if the merged milestone-2 evidence cannot honestly be settled as
  current-architecture support for the selected lane, record the narrowest
  explicit boundary-pressure classification for that same lane.
  Why it matters now: keep one lawful contingency only if the post-implementation
  settlement reread shows that bounded continuation is no longer the strongest
  honest read despite the accepted `round-196` evidence.
  Preconditions: `direction-2b-publish-post-implementation-p5-settlement` or
  an equivalent accepted reread shows bounded continuation is no longer the
  strongest honest classification for the selected lane.
  Parallel hints: serial after the merged execution evidence is reread.
  Boundary notes: docs-only first; do not widen production code or tests in
  the same extracted item that records the pressure classification.
  Extraction notes: expected extracted item shape is a decision-surface round
  that names the exact boundary under pressure and the precise evidence making
  it stronger than the merged bounded-execution read.

### 3. [pending] Route the refreshed positive-family frontier after `P5`

- Milestone id: `milestone-3`
- Depends on: `milestone-2`
- Intent: integrate the refreshed post-item-7 `P5` read with the accepted
  `P2 packet-specific folklore` ledger and decide the exact next unresolved
  frontier without prematurely reopening repo-level readiness.
- Completion signal: an accepted routing artifact either freezes one bounded
  `P2` follow-on lane or records that `P5` boundary pressure still dominates
  and should feed a later architecture-decision family instead.
- Parallel lane: `lane-main`
- Coordination notes: do not reopen settled same-lane or negative-family
  material; this milestone exists only to route the remaining positive-family
  frontier honestly after the refreshed `P5` read.

Candidate directions:

- Direction id: `direction-3a-refresh-the-p5-vs-p2-gap-ledger`
  Summary: publish a docs-only reread of the remaining positive-family gap
  after milestone-2, keeping `P5` and `P2` distinct and naming the current
  strongest blocker honestly.
  Why it matters now: accepted round-193 left both `P5` and `P2` unresolved,
  but it ranked `P5` as the sharpest blocker. After milestone-2, that ranking
  must be refreshed from evidence rather than carried forward by inertia.
  Preconditions: milestone-2 settlement surface exists.
  Parallel hints: serial only; the refreshed ledger determines what the next
  lawful frontier actually is.
  Boundary notes: docs-only; no implementation or roadmap-wide boundary
  revision in this reread.
  Extraction notes: expected extracted item shape is an aggregate reread that
  ends with one exact remaining-frontier conclusion only.

- Direction id: `direction-3b-freeze-one-bounded-p2-follow-on-lane`
  Summary: if `P5` no longer dominates, freeze one exact non-local
  propagation lane that can test whether `P2` can be upgraded beyond
  packet-specific folklore.
  Why it matters now: repo-level readiness remains blocked if `P2` cannot move
  beyond one exact packet even after the refreshed `P5` read.
  Preconditions: `direction-3a-refresh-the-p5-vs-p2-gap-ledger` or an
  equivalent accepted reread identifies `P2` as the next strongest unresolved
  front.
  Parallel hints: serial after the refreshed gap ledger; no co-scheduling with
  fresh `P5` work in `rev-001`.
  Boundary notes: docs-only freeze first; do not reopen the exact accepted `C1`
  packet or silently widen into broad non-local search.
  Extraction notes: expected extracted item shape is a freeze artifact naming
  one exact `P2` lane, success bar, and writable slice.

- Direction id: `direction-3c-record-p5-dominant-boundary-pressure`
  Summary: if `P5` remains the stronger blocker after milestone-2, record why
  opening a fresh `P2` lane would be weaker than routing the family through a
  later explicit architecture-decision branch.
  Why it matters now: the roadmap needs an honest off-ramp when the refreshed
  post-`P5` read still outranks `P2`.
  Preconditions: `direction-3a-refresh-the-p5-vs-p2-gap-ledger` shows that
  `P5` remains the dominant unresolved frontier or boundary-pressure source.
  Parallel hints: serial after the refreshed gap ledger.
  Boundary notes: docs-only; do not promote this pressure note into an
  immediate boundary revision without the later decision milestone.
  Extraction notes: expected extracted item shape is a routing artifact that
  names why `P2` should stay unopened in the current revision.

### 4. [pending] Reissue the repo-level readiness / architecture decision from the refreshed `P5` / `P2` ledger

- Milestone id: `milestone-4`
- Depends on: `milestone-3`
- Intent: consume the updated post-item-7 `P5` / `P2` ledger and publish a
  refreshed repo-level end-state decision only when the evidence is strong
  enough to compare honestly against the accepted item-4 readiness contract.
- Completion signal: an accepted docs-only aggregate decision records exactly
  one end-state
  (`repo-level readiness reached inside the current architecture`,
  `continue-bounded`, or
  `explicit boundary-revision candidate`)
  and exactly one next handoff or enablement step.
- Parallel lane: `lane-main`
- Coordination notes: this milestone owns the next repo-level reread only. Do
  not pre-authorize production work, boundary revision, or a new family until
  the refreshed decision itself is accepted.

Candidate directions:

- Direction id: `direction-4a-publish-refreshed-readiness-decision`
  Summary: publish the refreshed aggregate readiness / architecture decision
  from the updated `P5` / `P2` ledger and the preserved negative-family
  settlements.
  Why it matters now: the prior item-7 decision is authoritative only for the
  pre-follow-on ledger. The repo needs a fresh end-state read after the
  post-item-7 frontier moves.
  Preconditions: milestone-3 has produced an accepted routing artifact.
  Parallel hints: serial only; the refreshed decision consumes the entire
  current ledger.
  Boundary notes: docs-only; no implementation or roadmap amendment in the
  same extracted item.
  Extraction notes: expected extracted item shape is one aggregate decision
  artifact that records exactly one end-state and one supporting evidence
  ledger only.

- Direction id: `direction-4b-bind-final-enablement-or-next-family`
  Summary: bind the concrete consequence of the refreshed decision, whether
  that is a final enablement / hardening handoff, another bounded continuation
  family, or an explicit boundary-revision family.
  Why it matters now: the roadmap must end with one executable next move
  rather than a free-floating decision token.
  Preconditions: `direction-4a-publish-refreshed-readiness-decision` or an
  equivalent accepted aggregate decision exists.
  Parallel hints: serial after the refreshed decision.
  Boundary notes: keep the handoff precise; do not silently widen the scope of
  the refreshed decision.
  Extraction notes: expected extracted item shape is a bounded handoff artifact
  that names one exact next family or enablement step only.
