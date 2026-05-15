# Round 229 Attempt 1

Date: 2026-05-15
Round: `round-229`
Milestone: `milestone-1`
Direction: `direction-1a-noderefgadt-reftag-kind`

## Retry Contract

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `not needed`

## Attempt Summary

- Scope matched the plan exactly: `MLF.Binding.Adjustment`, its four direct internal callers, and `test/BindingSpec.hs`.
- The type-only child boundary in `MLF.Binding.Adjustment` now uses `NodeRefTag 'TypeTag`, and the old local `requireTypeRef` discriminator is gone.
- Mixed `NodeRef` remains only on retained ancestor or parent target parameters, consistent with the selected extraction.
- No public API files changed, and no `mlf2.cabal` or `test/Main.hs` registration edits were needed.
- Focused verification passed:
  `harmonizeBindParentsWithTrace` (`6 examples, 0 failures`),
  `applyRaiseTo` (`4 examples, 0 failures`),
  `raiseToParentWithCount` (`1 example, 0 failures`).
- Full verification passed:
  `cabal build all && cabal test` reported `2563 examples, 0 failures` in `358.0868 seconds`.
- `orchestrator/state.json` remained dirty only as controller-owned stage bookkeeping and is excluded from the implementation payload.

## Outcome

The round satisfies the selected `milestone-1` extraction and is approved as
`accepted + finalize`, with status-only closeout limited to moving
`milestone-1` from `pending` to `in-progress`.
