# Findings

- `Bugs.md` currently has no open entries, so the backlog reset is not hiding an active defect queue.
- `tasks/todo/` still held multiple 2026-03-09 verifier/thinker folders whose targets are already landed or superseded by current repository state.
- Landed-and-guarded items still left in `tasks/todo/`:
  - `WithCanonicalT` retirement is already recorded in `CHANGELOG.md` and guarded in `test/PipelineSpec.hs`.
  - `rtvSchemeBodyTarget` retirement is already recorded in `CHANGELOG.md` and guarded in `test/PipelineSpec.hs`.
  - `preferGenScope` retirement is already recorded in `CHANGELOG.md` and guarded in `test/PipelineSpec.hs`.
- The old `Solved.fromPreRewriteState` compile blocker cited in several 2026-03-09 task folders is stale; the focused `ga scope` test slice passes again on the current tree.
- The round-5 witness/trace canonicalization verifier is effectively stale too: `MLF.Elab.Run.Util` no longer defines local copies and instead imports `canonicalizeWitness` / `canonicalizeTrace` from `MLF.Constraint.Presolution.Rewrite`, with source guards preventing reintroduction.
- The only remaining plausible bounded cleanup found during the audit is the `pendingWeakenOwners` diagnostic aggregation seam: it is implemented in `MLF.Constraint.Presolution.EdgeUnify.Omega`, re-exported through `MLF.Constraint.Presolution.EdgeUnify`, and consumed only by `Driver` / `EdgeProcessing` diagnostics and boundary checks.
- That seam is narrower than the original round-3 verifier premise because the owner-specific aliases (`pendingWeakenOwnerForNode`, `pendingWeakenOwnerForEdge`) are already gone from live source; only stale test/task text still mentions them.
