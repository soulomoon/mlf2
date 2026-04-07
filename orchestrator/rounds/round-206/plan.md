# Round 206 Plan

- Round: `round-206`
- Roadmap:
  `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
  / `rev-001`
- Milestone: `milestone-1`
- Direction:
  `direction-1a-freeze-broader-positive-enactment-contract`
- Extracted item:
  `freeze-broader-positive-enactment-contract`
- Retry: `null`
- Execution shape: serial, docs/control-plane-only, one milestone-1
  enactment-contract artifact only, no worker fan-out, and no
  production/test/Cabal changes

## Objective

Publish one canonical milestone-1 enactment-contract artifact only:

`docs/plans/2026-04-08-p5-polymorphism-nested-forall-broader-positive-enactment-family-contract-authoritative-frontier-representative-corpus-and-writable-slice-freeze.md`

That artifact must turn the accepted `round-205` handoff into one exact
`milestone-1` / `direction-1a` family-entry freeze that:

- consumes accepted `round-205` as the binding family-entry authority,
  accepted `round-204` as the exact broader-positive ledger beneath it, and
  accepted `round-203` as the controlling revised semantic freeze beneath
  that;
- freezes the exact broader-positive `P5 polymorphism-nested-forall`
  enactment frontier beyond the one settled retained-child clear-boundary
  lane, without silently upgrading that lane into family closure;
- freezes the exact expected behavior shift away from treating
  round-151-style polymorphic-mediation `mu` absorption as the controlling
  broader-positive read, while still keeping this round non-enacting;
- freezes the exact authoritative success surfaces for later code-bearing
  rounds, with success required on both `runPipelineElab` and
  `runPipelineElabChecked` and continuity kept explicit through
  `src/MLF/Elab/Pipeline.hs` and `src-public/MLF/Pipeline.hs`;
- freezes one exact representative corpus ledger that distinguishes
  preserved positive controls, live broader-positive enactment targets, and
  preserved fail-closed / closed-guardrail rows; and
- freezes one exact writable slice for later milestone-2 and milestone-3
  work while keeping `P2`, `N1 ambiguity-reject`, `N2 unsoundness-guard`,
  `N6 termination-pressure`, cyclic widening, multi-SCC widening,
  equi-recursive reasoning, fallback rescue, and a second interface closed.

## Authorized Write Scope

Implementation-owned writes for this round are limited to:

- `docs/plans/2026-04-08-p5-polymorphism-nested-forall-broader-positive-enactment-family-contract-authoritative-frontier-representative-corpus-and-writable-slice-freeze.md`
- `orchestrator/rounds/round-206/implementation-notes.md`
  only if the implementer needs a round-local summary and keeps it strictly
  derivative of the canonical docs artifact

Round-owned artifacts that may appear later in the packet and must not be
misclassified as extra milestone docs are:

- `orchestrator/rounds/round-206/review.md`
- `orchestrator/rounds/round-206/review-record.json`
- `orchestrator/rounds/round-206/merge.md`

Do not create or modify:

- `orchestrator/rounds/round-206/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `TODO.md`
- `implementation_notes.md`
- `CHANGELOG.md`
- `Bugs.md`
- `README.md`
- any accepted predecessor `docs/plans/**` artifact
- any file under `src/`, `src-public/`, `app/`, or `test/`
- `test/Main.hs`
- `mlf2.cabal`
- `orchestrator/rounds/round-206/worker-plan.json`
- any second `docs/plans/**` artifact, milestone-2 implementation note,
  milestone-3 corpus-expansion note, roadmap-amendment draft, or closeout
  draft

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input:
  `orchestrator/rounds/round-206/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-001`

Current worktree state is already non-pristine. Respect unrelated edits and do
not revert them:

- `M orchestrator/state.json` is controller-owned and must remain untouched.
- `?? orchestrator/rounds/round-206/selection.md` is the round input and must
  remain untouched after this plan lands.

Accepted `round-205` is the binding family-entry authority. Its canonical
artifact
`docs/plans/2026-04-08-p5-polymorphism-nested-forall-explicit-boundary-revision-family-final-handoff-binding-one-exact-downstream-consequence-from-the-revised-planning-ledger.md`
opened exactly one later enactment / implementation family for the still-live
broader-positive pressure and intentionally deferred the exact code-bearing
contract to this round.

Accepted `round-204` remains the exact broader-positive planning ledger. Its
canonical artifact
`docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-broader-positive-p5-ledger-under-the-revised-freeze.md`
fixes the live frontier beyond the one settled retained-child lane and keeps
that lane bounded as predecessor truth only.

Accepted `round-203` remains the controlling revised semantic freeze. Its
canonical artifact
`docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-round-151-polymorphic-mediation-mu-preservation-reclassification-and-inherited-boundary-refreeze.md`
reclassifies the round-151 polymorphic-mediation `mu`-absorption story as
live broader-positive pressure rather than settled controlling truth.

The preserved supporting lineage that must stay visible in this round is:

- accepted `round-201` and `round-200`, which keep
  `explicit boundary-revision candidate`
  as predecessor pressure only rather than an already enacted slice;
- accepted `round-197`, which keeps
  `sameLaneAliasFrameClearBoundaryExpr`
  as one settled retained-child clear-boundary success on
  `runPipelineElab` / `runPipelineElabChecked`,
  while `nestedForallContrastExpr` remains fail-closed with
  `PhiTranslatabilityError`;
- accepted `round-191` and accepted `round-181`, which keep `P2`
  packet-bounded to the exact `C1` packet only; and
- accepted `round-192`, which keeps
  `N1 ambiguity-reject`,
  `N2 unsoundness-guard`, and
  `N6 termination-pressure`
  closed as predecessor truth.

The live family-entry pressure that must be frozen explicitly is:

- the broader-positive nested-forall / quantified-crossing frontier beyond
  the one settled retained-child clear-boundary lane;
- the revised semantic pressure around whether recursive structure should
  survive rather than disappear when polymorphic mediation crosses nested
  `forall`;
- the requirement that later success be honest on both
  `runPipelineElab` and `runPipelineElabChecked`, not helper-only; and
- the need to freeze a later writable slice and representative corpus without
  pre-committing the exact milestone-2 diff.

The read-only authority chain for this round is:

- `orchestrator/rounds/round-206/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-001/roadmap.md`
- `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-001/verification.md`
- `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-001/retry-subloop.md`
- `TODO.md`
- `implementation_notes.md`
- `docs/plans/2026-04-08-p5-polymorphism-nested-forall-explicit-boundary-revision-family-final-handoff-binding-one-exact-downstream-consequence-from-the-revised-planning-ledger.md`
- `docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-broader-positive-p5-ledger-under-the-revised-freeze.md`
- `docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-round-151-polymorphic-mediation-mu-preservation-reclassification-and-inherited-boundary-refreeze.md`
- `docs/plans/2026-04-07-post-item-7-explicit-boundary-revision-candidate-final-handoff-to-one-planning-only-p5-polymorphism-nested-forall-boundary-revision-family.md`
- `docs/plans/2026-04-07-post-item-7-refreshed-repo-level-readiness-and-architecture-decision-from-the-updated-p5-vs-p2-ledger-and-preserved-negative-family-settlements.md`
- `docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`
- `docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md`
- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`
- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md`
- `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`
- `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`
- `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`
- `docs/plans/2026-03-29-same-lane-alias-frame-representative-gap-post-item-2-settlement-surface-and-repo-impact-read.md`
- `docs/plans/2026-03-28-p5-polymorphism-nested-forall-successor-authority-success-bar-and-writable-slice-freeze.md`
- `docs/plans/2026-03-28-post-implementation-p5-polymorphism-nested-forall-settlement-surface-and-exact-repo-impact-read.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `orchestrator/rounds/round-205/review-record.json`
- `orchestrator/rounds/round-204/review-record.json`
- `orchestrator/rounds/round-203/review-record.json`
- `orchestrator/rounds/round-201/review-record.json`
- `orchestrator/rounds/round-200/review-record.json`
- `orchestrator/rounds/round-197/review-record.json`
- `orchestrator/rounds/round-192/review-record.json`
- `orchestrator/rounds/round-191/review-record.json`
- `orchestrator/rounds/round-181/review-record.json`
- `orchestrator/rounds/round-151/review.md`
- `orchestrator/rounds/round-151/review-record.json`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `src/MLF/Elab/TermClosure.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`
- `test/Research/P5ClearBoundarySpec.hs`
- `test/PipelineSpec.hs`

## Sequential Plan

1. Freeze the milestone-1 family-entry contract before drafting or editing
   anything.
   - Modify no files in this step.
   - Read the full authority chain above and end this step with one fixed
     frame only:
     accepted `round-205` opens exactly one later enactment family;
     accepted `round-204` fixes the broader-positive frontier beyond the one
     settled retained-child lane;
     accepted `round-203` keeps round-151-style polymorphic-mediation
     `mu` absorption as live pressure rather than settled broader-positive
     truth;
     accepted `round-201` / `round-200` preserve
     `explicit boundary-revision candidate`
     as predecessor pressure only;
     accepted `round-197` keeps one settled retained-child clear-boundary
     control while `nestedForallContrastExpr` remains fail-closed;
     accepted `round-191` / `round-181` keep `P2` packet-only;
     accepted `round-192` keeps the representative negative-family rows
     closed; and the current repo anchors expose the supporting guard cluster
     and authoritative pipeline surfaces without yet enacting a broader law.
   - The step-1 exit question is narrow:
     what exact contract must later code-bearing rounds satisfy before they
     may honestly claim broader-positive support beyond the one settled lane?
   - Verification:
     `python3 -m json.tool orchestrator/state.json >/dev/null`
     `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/verification.md" && test -f "$roadmap_dir/retry-subloop.md"`
     `rg -n 'milestone-1|direction-1a-freeze-broader-positive-enactment-contract|freeze-broader-positive-enactment-contract|docs/control-plane-only|runPipelineElab|runPipelineElabChecked|representative corpus|writable slice' orchestrator/rounds/round-206/selection.md "$roadmap_dir/roadmap.md" "$roadmap_dir/verification.md" "$roadmap_dir/retry-subloop.md"`
     `rg -n 'round-205|round-204|round-203|round-201|round-200|round-197|round-192|round-191|round-181|explicit boundary-revision candidate|sameLaneAliasFrameClearBoundaryExpr|nestedForallContrastExpr|Known correct behavior under polymorphic mediation|Nested-forall-mediated recursive types|runPipelineElab|runPipelineElabChecked' docs/plans/2026-04-08-p5-polymorphism-nested-forall-explicit-boundary-revision-family-final-handoff-binding-one-exact-downstream-consequence-from-the-revised-planning-ledger.md docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-broader-positive-p5-ledger-under-the-revised-freeze.md docs/plans/2026-04-07-p5-polymorphism-nested-forall-explicit-boundary-revision-family-round-151-polymorphic-mediation-mu-preservation-reclassification-and-inherited-boundary-refreeze.md docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md docs/plans/2026-04-06-post-item-7-p5-post-implementation-settlement-surface-and-exact-repo-impact-read.md docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md implementation_notes.md`
     `rg -n 'sameLaneAliasFrameClearBoundaryExpr|sameLaneDoubleAliasFrameClearBoundaryExpr|sameLaneNonupleAliasFrameClearBoundaryExpr|nestedForallContrastExpr|boundHasForallFrom|sameLaneLocalRetainedChildTarget|keepTargetFinal|targetC|preserveRetainedChildAuthoritativeResult|runPipelineElab|runPipelineElabChecked' src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs`

2. Draft the canonical milestone-1 enactment-contract artifact and keep it
   strictly docs/control-plane-only.
   - Create
     `docs/plans/2026-04-08-p5-polymorphism-nested-forall-broader-positive-enactment-family-contract-authoritative-frontier-representative-corpus-and-writable-slice-freeze.md`.
   - Use a stage-contract header that records `round-206`,
     `milestone-1`,
     `direction-1a-freeze-broader-positive-enactment-contract`,
     `freeze-broader-positive-enactment-contract`,
     `attempt-1`,
     `retry: null`,
     and the live subject as one docs-only milestone-1 enactment-contract
     freeze before any code-bearing round.
   - The document must include, in order:
     `## Stage Contract Freeze`,
     `## Family-Entry Authority Ledger`,
     `## Exact Broader-Positive Enactment Frontier`,
     `## Expected Behavior Shift`,
     `## Authoritative Success Surfaces`,
     `## Representative Corpus Obligations`,
     `## Exact Writable Slice`,
     `## Preserved Closed Guardrails And Exclusions`,
     `## Milestone-2 And Milestone-3 Consequences`,
     and `## Non-Claims`.
   - In `## Family-Entry Authority Ledger`, cite the exact lineage from step
     `1` with accepted `round-205` as the binding family-entry authority,
     accepted `round-204` / `round-203` as the controlling planning ledger,
     accepted `round-201` / `round-200` as preserved family-entry pressure,
     accepted `round-197` as the settled retained-child predecessor lane,
     accepted `round-191` / `round-181` as `P2`-closed authority,
     accepted `round-192` as negative-family-closed authority, and the March
     25 / 28 / 29 docs as earlier corpus / freeze provenance only.
   - In `## Exact Broader-Positive Enactment Frontier`, define the frontier as
     broader positive nested-forall / quantified-crossing support beyond the
     one settled retained-child clear-boundary lane, still inside the
     explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph /
     no-fallback / no-second-interface boundary.
   - In `## Expected Behavior Shift`, state explicitly that later code-bearing
     rounds must stop treating round-151-style polymorphic-mediation
     `mu` absorption as the controlling broader-positive read for this
     frontier, while this milestone only freezes that expected shift and does
     not claim the code already earns it.
   - In `## Authoritative Success Surfaces`, freeze both
     `runPipelineElab` and `runPipelineElabChecked` as mandatory evidence
     surfaces and name
     `src/MLF/Elab/Run/Pipeline.hs`,
     `src/MLF/Elab/Pipeline.hs`, and
     `src-public/MLF/Pipeline.hs`
     as the authoritative continuity seams; treat
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
     `src/MLF/Elab/TermClosure.hs`
     as supporting but non-authoritative mechanism seams only.
   - In `## Representative Corpus Obligations`, classify exact rows into
     preserved controls, live enactment targets, and preserved closed
     negatives. The section must cite at least:
     `sameLaneClearBoundaryExpr`,
     `sameLaneAliasFrameClearBoundaryExpr`, and
     `nestedForallContrastExpr`
     from `test/Research/P5ClearBoundarySpec.hs`;
     the authoritative-entrypoint continuity cluster in `test/PipelineSpec.hs`
     from
     `sameLaneAliasFrameClearBoundaryExpr`
     through
     `sameLaneNonupleAliasFrameClearBoundaryExpr`;
     the broader quantified-crossing baseline
     `keeps retained-child fallback open for recursive types even when the same wrapper crosses a nested forall boundary`;
     the preserved fail-closed local-lane exit tests
     `keeps the same single-base wrapper fail-closed once it leaves the local TypeRef lane`,
     `keeps the same multi-inst wrapper fail-closed once it leaves the local TypeRef lane`,
     `keeps the same inst-arg multi-base wrapper fail-closed once it leaves the local TypeRef lane`,
     `keeps the same inst-arg-only singleton-base wrapper fail-closed once it leaves the local TypeRef lane`,
     and
     `does not infer recursive shape for the corresponding unannotated variant`;
     plus the docs-only preserved `P2`, `N1`, `N2`, and `N6` guardrails from
     the accepted review records.
   - In `## Exact Writable Slice`, freeze the later code-bearing slice to:
     `src/MLF/Elab/Run/ResultType/Fallback.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
     `src/MLF/Elab/TermClosure.hs`,
     `src/MLF/Elab/Run/Pipeline.hs`,
     `src/MLF/Elab/Pipeline.hs`,
     `src-public/MLF/Pipeline.hs`,
     `test/Research/P5ClearBoundarySpec.hs`, and
     `test/PipelineSpec.hs`
     only, with explicit exclusions for `test/Main.hs`, `mlf2.cabal`,
     `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`, and any
     thesis-facing files until a later accepted milestone or revision says
     otherwise.
   - In `## Preserved Closed Guardrails And Exclusions`, keep the one settled
     retained-child lane as predecessor truth only, keep `P2`,
     `N1 ambiguity-reject`, `N2 unsoundness-guard`, and
     `N6 termination-pressure` closed, and forbid cyclic search, multi-SCC
     widening, equi-recursive reasoning, fallback rescue, and a second
     interface.
   - In `## Milestone-2 And Milestone-3 Consequences`, state that
     milestone-2 may change production only inside the frozen writable slice,
     milestone-3 must prove the frozen broader-positive corpus honestly on
     both authoritative entrypoints, and milestone-4 owns repo-facing notes
     such as `TODO.md`, `implementation_notes.md`, and `CHANGELOG.md`.
   - Verification after drafting:
     `rg -n '^## (Stage Contract Freeze|Family-Entry Authority Ledger|Exact Broader-Positive Enactment Frontier|Expected Behavior Shift|Authoritative Success Surfaces|Representative Corpus Obligations|Exact Writable Slice|Preserved Closed Guardrails And Exclusions|Milestone-2 And Milestone-3 Consequences|Non-Claims)$' docs/plans/2026-04-08-p5-polymorphism-nested-forall-broader-positive-enactment-family-contract-authoritative-frontier-representative-corpus-and-writable-slice-freeze.md`
     `rg -n 'round-205|round-204|round-203|round-201|round-200|round-197|round-192|round-191|round-181|sameLaneAliasFrameClearBoundaryExpr|sameLaneNonupleAliasFrameClearBoundaryExpr|nestedForallContrastExpr|runPipelineElab|runPipelineElabChecked|boundHasForallFrom|sameLaneLocalRetainedChildTarget|keepTargetFinal|targetC|preserveRetainedChildAuthoritativeResult|P2|N1 ambiguity-reject|N2 unsoundness-guard|N6 termination-pressure|test/Main.hs|mlf2.cabal' docs/plans/2026-04-08-p5-polymorphism-nested-forall-broader-positive-enactment-family-contract-authoritative-frontier-representative-corpus-and-writable-slice-freeze.md`

3. Self-check the artifact against the active roadmap revision and round
   scope before review.
   - Confirm the artifact freezes the exact frontier, expected behavior
     shift, authoritative success surfaces, representative corpus, and later
     writable slice concretely enough for later code-bearing review without
     enacting any of it now.
   - Confirm the artifact distinguishes preserved positive controls from live
     broader-positive enactment targets and from preserved closed negatives,
     rather than silently treating one already-settled retained-child lane as
     whole-frontier closure.
   - Confirm the artifact keeps milestone-2 / milestone-3 inside the frozen
     slice and keeps milestone-4 repo-facing notes out of scope for this
     round.
   - Verification:
     `git diff --check -- docs/plans/2026-04-08-p5-polymorphism-nested-forall-broader-positive-enactment-family-contract-authoritative-frontier-representative-corpus-and-writable-slice-freeze.md orchestrator/rounds/round-206/implementation-notes.md`
     `git status --short --untracked-files=all`
     `sh -lc 'if git status --short --untracked-files=all | rg -q "^(.. )?(src/|src-public/|app/|test/|mlf2\\.cabal|test/Main\\.hs|CHANGELOG\\.md|TODO\\.md|implementation_notes\\.md)"; then echo unauthorized-path-touched; exit 1; else echo docs-control-plane-only; fi'`
   - No `cabal build all && cabal test` gate is required in this round because
     the planned diff must stay docs/control-plane-only.

## Verification Intent

Before review, the implementer should be able to show all of the following
from the authored artifact and round diff:

- accepted `round-205` is consumed as the binding family-entry authority for
  this new enactment family
- the artifact names one exact broader-positive enactment frontier beyond the
  one settled retained-child clear-boundary lane
- the artifact freezes the exact expected behavior shift away from
  controlling polymorphic-mediation `mu` absorption without claiming the code
  already earns that shift
- the artifact fixes both `runPipelineElab` and `runPipelineElabChecked` as
  the authoritative later success bar
- the artifact freezes one exact representative corpus ledger spanning
  preserved controls, live broader-positive targets, and preserved closed
  negatives
- the artifact freezes one exact later writable slice and names explicit
  exclusions
- the round stays docs/control-plane-only and does not widen into production,
  tests, Cabal, roadmap edits, or repo-facing closeout work
