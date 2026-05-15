# Round 230 Attempt 1

Date: 2026-05-15
Round: `round-230`
Milestone: `milestone-1`
Direction: `direction-1a-noderefgadt-reftag-kind`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Attempt Summary

- Scope matched the plan exactly: the round touched only `src/MLF/Binding/GraphOps.hs`, `test/GraphOpsSpec.hs`, and the reviewer-owned round artifacts; `orchestrator/state.json` remained dirty only as controller-owned bookkeeping.
- The residual type-only GraphOps query and predicate seam now uses `NodeRefTag 'TypeTag` for `getBindFlag`, `isInstantiable`, and `isLocked`.
- The retained mixed `NodeRef` seam is confined to the documented owners: `BindParents`, mixed node enumeration/existence helpers, and ancestor or parent target parameters for raise operations.
- No runtime `expectTypeRef` / `expectGenRef` / `requireTypeRef` / `requireGenRef` helper remains in live code or tests; only the explanatory comment in `NodeEdge` remains.
- No public API files changed, and no `mlf2.cabal` or `test/Main.hs` registration edits were needed.
- Focused verification passed:
  `Binding shared abstractions` (`7 examples, 0 failures`),
  `harmonizeBindParentsWithTrace` (`6 examples, 0 failures`),
  `applyRaiseTo` (`5 examples, 0 failures`),
  `raiseToParentWithCount` (`1 example, 0 failures`).
- Full verification passed:
  `cabal build all && cabal test` reported `2564 examples, 0 failures` in `359.8944 seconds`.

## Outcome

The round satisfies the selected closeout-audit extraction and is approved as
`accepted + finalize`, with status-only closeout limited to moving
`milestone-1` from `in-progress` to `done`.
