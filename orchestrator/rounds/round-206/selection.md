# Round 206 - Task Selection

**Selected milestone**: `milestone-1`
**Selected direction**: `direction-1a-freeze-broader-positive-enactment-contract`
**Selected extraction**: `freeze-broader-positive-enactment-contract`
**Current task/title**: `Freeze the broader-positive enactment contract, authoritative frontier, writable slice, and representative corpus before any code-bearing round`

**Roadmap identity**:
- roadmap_id: `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-001`
- milestone_id: `milestone-1`
- direction_id: `direction-1a-freeze-broader-positive-enactment-contract`
- extracted_item_id: `freeze-broader-positive-enactment-contract`

## Why now

`milestone-1` is the lawful lowest-numbered unfinished milestone in the active
serial roadmap family, and `retry` remains `null`, so there is no same-round
retry state or accepted prior freeze that would justify skipping ahead.

The active roadmap's sequencing rules make this first move mandatory:
`milestone-1` must complete before any code-bearing enactment round, and
inside that milestone
`direction-1a-freeze-broader-positive-enactment-contract`
is the first and only ready extraction. `milestone-2`, `direction-2a`,
`direction-2b`, `milestone-3`, and `milestone-4` all depend on this freeze
existing first.

Accepted `round-205` bound exactly one downstream consequence from the prior
planning-only family:
`open one later enactment / implementation family for the still-live explicit boundary-revision pressure on broader positive P5 polymorphism-nested-forall support beyond the one settled retained-child clear-boundary lane`.
That accepted handoff intentionally did not freeze the exact code-bearing
frontier, the authoritative success corpus, or the writable slice for later
implementation rounds. This new family exists precisely to bind those things
before any production edit claims to enact the broader-positive frontier.

The current repo baseline still lacks the canonical milestone-1 authority
artifact that names:

- the exact broader positive `P5 polymorphism-nested-forall` frontier beyond
  the one settled retained-child lane;
- the exact expected behavior shift away from treating
  polymorphic-mediation `mu` absorption as the controlling broader-positive
  read;
- the exact authoritative success bar on both `runPipelineElab` and
  `runPipelineElabChecked`;
- the exact representative positive and negative corpus obligations; and
- the exact writable production/test/docs slice for later code-bearing rounds.

Without that freeze, any milestone-2 implementation round would be
under-specified and could not be reviewed honestly against the active family
contract.

## Current baseline

- Round-206 branch/worktree `HEAD`: `8bc3c66`
- Controller-prepared round branch:
  `orchestrator/round-206-freeze-broader-positive-enactment-contract`
- Controller-prepared round worktree: `orchestrator/worktrees/round-206`
- Active roadmap family and revision remain
  `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
  at `rev-001`; `milestone-1`, `milestone-2`, `milestone-3`, and
  `milestone-4` are all still pending
- No retry state is active (`retry: null`, no `resume_error`)
- Accepted `round-205` published the binding final-handoff artifact at
  `docs/plans/2026-04-08-p5-polymorphism-nested-forall-explicit-boundary-revision-family-final-handoff-binding-one-exact-downstream-consequence-from-the-revised-planning-ledger.md`
  and recorded approval in
  `orchestrator/rounds/round-205/review-record.json`; that handoff selected
  one later enactment family only and explicitly deferred the exact
  code-bearing contract to this new family
- Accepted `round-204` and `round-203` remain the controlling predecessor
  planning authority beneath that handoff: `round-204` froze the broader
  positive frontier beyond the one settled retained-child lane, and
  `round-203` preserved the revised freeze that reclassified the old
  round-151 polymorphic-mediation story as narrower live pressure rather than
  settled broader-positive truth
- Current repo-facing notes in `implementation_notes.md` still record
  `Known correct behavior under polymorphic mediation` and
  `Nested-forall-mediated recursive types` as the live production baseline:
  under current behavior, nested-forall-mediated recursive types absorb `mu`
  through polymorphic mediation and do not yet earn broader-positive
  recursive-structure visibility on authoritative outputs
- Accepted `round-197` still keeps
  `sameLaneAliasFrameClearBoundaryExpr` as one settled retained-child
  clear-boundary success on `runPipelineElab` / `runPipelineElabChecked`,
  while `nestedForallContrastExpr` remains fail-closed with
  `PhiTranslatabilityError`; that pair is predecessor truth only, not whole-
  frontier closure
- Accepted `round-191` / `round-181` keep `P2` packet-bounded to the exact
  `C1` packet only, and accepted `round-192` keeps
  `N1 ambiguity-reject`,
  `N2 unsoundness-guard`, and
  `N6 termination-pressure`
  closed as predecessor truth only
- `TODO.md` and the cited March plan documents still provide the broader
  capability contract, the narrowed-successor posture, and the earlier exact-
  packet settlement history as predecessor context only; they do not freeze
  this new family's code-bearing enactment contract
- Repository status in the canonical round worktree before writing this
  selection was one pre-existing controller-owned modification:
  `M orchestrator/state.json`

## Scope

- Keep this round docs/control-plane-only, serial, and faithful to exactly
  one milestone-1 / direction-1a extraction:
  freeze the broader-positive enactment contract before any code-bearing
  round
- Publish one canonical milestone-1 enactment-contract artifact under
  `docs/plans/` plus round bookkeeping only; that artifact must freeze the
  exact broader frontier, the exact expected behavior shift, the exact
  authoritative success surfaces, the exact representative corpus, the exact
  writable slice, and the exact preserved closed guardrails for later rounds
- Ground the freeze in the accepted lineage and guider inputs already named by
  the active roadmap and role contract:
  `round-205`, `round-204`, `round-203`, `round-201`, `round-200`,
  `round-197`, `round-192`, `round-191`, `round-181`, `TODO.md`,
  `implementation_notes.md`, the cited March 25/27/28/29 plan documents,
  the active roadmap bundle, the retry contract, repository status, and prior
  round artifacts where they materially constrain the frontier
- Use the roadmap extraction notes to inventory the likely authoritative seams
  around
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `test/Research/P5ClearBoundarySpec.hs`, and
  `test/PipelineSpec.hs`
  without mechanically pre-committing the later milestone-2 diff
- Keep preserved predecessor truth closed: do not reopen the one settled
  retained-child lane as broad closure, do not reopen `P2`, and do not
  reopen `N1 ambiguity-reject`, `N2 unsoundness-guard`, or
  `N6 termination-pressure` as substitute live debt
- Do not start production edits, focused test edits, Cabal wiring, milestone-2
  mechanism work, milestone-3 corpus-widening work, or milestone-4 closeout
  work yet
- Do not widen into cyclic search, multi-SCC behavior, equi-recursive
  reasoning, fallback rescue, a second interface, or any silent reopening of
  families the active roadmap keeps closed
