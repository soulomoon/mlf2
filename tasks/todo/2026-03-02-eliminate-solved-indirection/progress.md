# Progress: Eliminate Solved Indirection

## 2026-03-02 — Planning session

- Researched full dependency chain: `Solved` type, `ElabEnv`, `PresolutionResult`, `EdgeTrace`
- Mapped every `Solved.*` API call site across 25+ source modules
- Identified that only `prConstraint` + `prUnionFind` feed into `Solved`; other 5 presolution fields bypass it
- Created 7-phase plan in task_plan.md
- Key risk: `Presolution/Plan` captures `Solved` in closure (`prPlanBuilder`)
- Highest-effort modules: `Phi/Omega.hs` (~30 calls), `Reify/Core.hs` (~25 calls)

### Next steps
- Phase 1: Create `PresolutionView` module
