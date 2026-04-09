# Round 211 - Task Selection

**Selected milestone**: `milestone-2`
**Selected direction**:
`direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
**Selected extraction**:
`continue-round-211-selected-same-wrapper-nested-forall-through-broader-authoritative-application-and-let-polymorphism-handoff-seam`
**Current task/title**:
`Continue the selected same-wrapper nested-forall packet through the broader authoritative application / let-polymorphism handoff seam while preserving the live round-211 baseline`

**Round metadata**:
- round_id: `round-211`
- branch:
  `orchestrator/round-211-repair-same-wrapper-nested-forall-across-authoritative-annotation-and-post-annotation-handoff-seams`
- worktree_path: `orchestrator/worktrees/round-211`
- active_round_dir: `orchestrator/rounds/round-211`

**Roadmap identity**:
- roadmap_id:
  `2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap`
- roadmap_revision: `rev-014`
- roadmap_dir:
  `orchestrator/roadmaps/2026-04-08-00-p5-polymorphism-nested-forall-broader-positive-explicit-boundary-enactment-roadmap/rev-014`
- milestone_id: `milestone-2`
- direction_id:
  `direction-2b-thread-the-selected-semantics-through-authoritative-pipeline-seams`
- extracted_item_id:
  `continue-round-211-selected-same-wrapper-nested-forall-through-broader-authoritative-application-and-let-polymorphism-handoff-seam`

## Why now

`orchestrator/state.json` already keeps the same live `round-211`
branch/worktree at `stage = select-task` under `roadmap_revision = rev-014`.
The rejected `attempt-13` review forced a same-round `update-roadmap`
continuation, not a fresh round, because the inherited `rev-013`
helper-flattening audit is now honestly exhausted even though the runtime
evidence is green.

`rev-014` preserves the controlling truth exactly. The inherited `round-211`
baseline still keeps the selected same-wrapper nested-`forall` packet green on
both authoritative entrypoints, preserves checked-authoritative representative
parity, `BUG-2026-02-06-002`, the retained-child exact packet,
`BUG-2026-02-17-002`, and the `g g` control, and now also keeps the A6 /
nested-let / representative let-polymorphism cluster green, with
`./scripts/thesis-conformance-gate.sh` passing and
`cabal build all && cabal test` passing at `1341` examples / `0` failures.
The honest next move is therefore the same `milestone-2` /
`direction-2b` extraction on the preserved `round-211` baseline, but with the
currently local `AAppF` / `ALetF` helper scaffold admitted explicitly rather
than demanding another flatten-only cleanup or a broader structural rewrite.

## Current baseline

- Live `round-211` remains the preserved same-round continuation baseline.
  Keep its existing round-owned diff in
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `src/MLF/Elab/Elaborate/Algebra.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`
  as inherited work rather than discarded draft.
- Preserve the protected wins already visible on that baseline:
  the selected same-wrapper nested-`forall` packet on both authoritative
  entrypoints, checked-authoritative representative parity,
  `BUG-2026-02-06-002`,
  the retained-child exact packet,
  `BUG-2026-02-17-002`, and
  the non-local proxy-wrapper `g g` control, plus the now-green A6 /
  nested-let / representative let-polymorphism cluster,
  `./scripts/thesis-conformance-gate.sh`, and
  `cabal build all && cabal test` with `1341` examples / `0` failures.
- Treat blocked `round-208`, `round-209`, and `round-210` as immutable
  predecessor evidence only.
- Treat the earlier `TermClosure.hs` retry, the bounded post-closure retry,
  the broader `rev-012` handoff attempt, and the `rev-013`
  helper-flattening audit as blocker evidence only, not as reopened writable
  surfaces.
- The live `rev-014` structural contract now admits the helper-local scaffold
  under
  `funInstRecovered`,
  `argInstFromFun`,
  `fApp`,
  `scheme`,
  `rhsAbs0`, and
  `rhsAbs`, including
  `containsMuType`,
  `containsMuBound`,
  `isIdentityLikeSchemeType`,
  `shouldInlineParamTy`,
  `shouldInferArgInst`,
  `isInternalTyVar`,
  `isIdentityLambdaBody`,
  `muAnnotationTy`, and
  the current `schemeTy` / strip-candidate staging locals used by
  `rhsAbs0` / `rhsAbs`.
- Parent-workspace and canonical round-worktree pointer stubs must stay
  aligned with the active `roadmap_id` / `roadmap_revision` / `roadmap_dir`
  as continuity housekeeping, not as scope expansion.

## Scope

- Keep this round faithful to exactly one `milestone-2` / `direction-2b`
  extraction:
  `continue-round-211-selected-same-wrapper-nested-forall-through-broader-authoritative-application-and-let-polymorphism-handoff-seam`
- Preserve the current `round-211` branch/worktree and inherited diff; do not
  reset it, restart on a fresh round, or relitigate the settled packet,
  `BUG-2026-02-17-002`, or earlier narrower repairs as if they were still
  unresolved.
- Keep the writable seam unchanged from `rev-013`, but treat the current
  runtime behavior as fixed; do not reopen `Annotation.hs`, `Legacy.hs`, the
  round-owned tests, or the closed continuity anchors merely to erase helper
  locals that `rev-014` already admits.
- Admit `src/MLF/Elab/Elaborate/Algebra.hs` only inside the existing
  instantiation-recovery and `ALetF` scheme/handoff locals around
  `funInstByFunType`,
  `funInst'`,
  `normalizeFunInst`,
  `funInstNorm`,
  `funInstRecovered`,
  `fAppForArgInference`,
  `argInstFromFun`,
  `argInst'`,
  `argInstFinal`,
  `fApp`,
  `aApp`,
  `schemeBase`,
  `scheme`,
  `subst0`,
  `subst`,
  `schemeInfo`,
  `env'`,
  `rhsAbs0`,
  `rhsAbs`,
  `bodyElab`, and
  `rhsFinal`,
  plus the admitted helper-local scaffold under
  `funInstRecovered`,
  `argInstFromFun`,
  `fApp`,
  `scheme`,
  `rhsAbs0`, and
  `rhsAbs`.
- Keep
  `src/MLF/Elab/Elaborate/Annotation.hs`,
  `src/MLF/Elab/Legacy.hs`,
  `test/ElaborationSpec.hs`,
  `test/PipelineSpec.hs`, and
  `test/Research/P5ClearBoundarySpec.hs`
  as preserved carry-forward evidence for this continuation; `rev-014` does
  not authorize fresh edits there unless later accepted proof shows the
  helper-local contract itself is false.
- Reject widenings whose only purpose is to restyle already-admitted helper
  locals, and reject any regression that reopens the green runtime evidence.
- Keep
  `src/MLF/Elab/TermClosure.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/Pipeline.hs`,
  `src-public/MLF/Pipeline.hs`,
  `src/MLF/Elab/Run/ResultType/Fallback.hs`, and
  `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  as closed read-only continuity anchors for this continuation.
- Do not widen into fallback rescue, a second interface, cyclic or multi-SCC
  search, equi-recursive reasoning, milestone-3 corpus widening, or
  milestone-4 closeout work.
- Stop again with explicit proof if the selected repair still requires a seam
  outside this `rev-014` continuation or if the helper-local structure must
  grow beyond the admitted scaffold above.
