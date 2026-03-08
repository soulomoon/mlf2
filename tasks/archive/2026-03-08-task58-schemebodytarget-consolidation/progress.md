# Progress

- 2026-03-08: Created dedicated task folder for Task 58 item 3.
- 2026-03-08: Inspected both `schemeBodyTarget` implementations and current call sites.
- 2026-03-08: Added direct `ScopeSpec` cases and a `PipelineSpec` source guard so the refactor had a RED signal before code movement.
- 2026-03-08: Removed the duplicate local helper from `MLF.Elab.Elaborate` and initially routed `generalizeAtNode` through the richer shared helper.
- 2026-03-08: Full gate failed with nested-let / BUG-002 alias regressions (`TCLetTypeMismatch`), which traced back to `generalizeAtNode` needing `S`-style bound descent rather than `S′`-style named-node preservation.
- 2026-03-08: Added owner-local `generalizeTargetNode` in `MLF.Elab.Run.Scope`, kept `schemeBodyTarget` for `S′`-style subterm translation, and left `MLF.Elab.Elaborate` with no local target-selection helper.
- 2026-03-08: Verification passed: `schemeBodyTarget` (`6 examples, 0 failures`), `nested` (`27 examples, 0 failures`), `BUG-002-V2` (`1 example, 0 failures`), and `cabal build all && cabal test` (`1004 examples, 0 failures`).
