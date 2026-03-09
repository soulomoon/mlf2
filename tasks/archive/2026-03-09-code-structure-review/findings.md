# Findings

## Repository Review
- Pending.

## High-Level Layout
- Repository has a large internal surface: `src/` contains 152 files, while `src-public/` exposes only 4 modules and `test/` contains 50 files.
- Internal modules are grouped by domain (`Constraint`, `Elab`, `Frontend`, `XMLF`, `Witness`, etc.), suggesting a deliberate architecture split between algorithm phases and public API wrappers.
- `mlf2.cabal` defines a private internal library, a public library, an app executable, a parity generator executable, and a single Hspec test suite.
- Repository root also contains legacy/root planning files (`task_plan.md`, `findings.md`, `progress.md`) alongside the newer `tasks/` workflow, which may indicate process drift.

## Structural Metrics
- Internal surface is dominated by `Constraint` (69 modules) and `Elab` (43 modules); other domains are much smaller, so architectural complexity is concentrated in those two areas.
- The private internal library exposes 54 modules and hides 98 more in `other-modules`, which gives tests/tooling wide reach into implementation details while the public library stays narrow at 4 exposed modules.
- Central fan-in modules include `src/MLF/Constraint/Types/Graph.hs`, `src/MLF/Constraint/Types.hs`, `src/MLF/Binding/Tree.hs`, `src/MLF/Constraint/NodeAccess.hs`, and `src/MLF/Constraint/Presolution.hs`; these are key coupling hubs.
- Large internal hotspots include `src/MLF/Elab/Phi/Omega.hs` (1468 LOC), `src/MLF/Constraint/Presolution/EdgeUnify.hs` (967 LOC), `src/MLF/Reify/Core.hs` (907 LOC), `src/MLF/Constraint/Normalize.hs` (846 LOC), `src/MLF/Constraint/Solve.hs` (827 LOC), `src/MLF/Elab/Elaborate.hs` (821 LOC), `src/MLF/Constraint/Presolution/Plan.hs` (815 LOC), and `src/MLF/Constraint/Presolution/Base.hs` (768 LOC).
- Large test hotspots include `test/ElaborationSpec.hs` (5133 LOC, 25 `describe`, 196 `it`) and `test/Presolution/WitnessSpec.hs` (2360 LOC), indicating strong coverage but low navigability.

## Boundary Observations
- `src/MLF/Constraint/Presolution.hs` is a broad testing-oriented facade that re-exports state, traces, builders, operational helpers, and validation helpers in one place.
- `src/MLF/Constraint/Types.hs` is a compatibility facade on top of `src/MLF/Constraint/Types/Graph.hs`, while many modules still import both `MLF.Constraint.Types` and `MLF.Constraint.Types.Graph`, suggesting a partially completed layering split.
- Cross-domain import counts show a mostly sensible `Elab -> Constraint` dependency direction, but also reveal smaller leaks such as `Constraint -> Elab` via `src/MLF/Constraint/Presolution/Plan/Finalize.hs` and `Util` depending on domain modules via `src/MLF/Util/RecursionSchemes.hs`.
- `src-public/MLF/API.hs` and `src-public/MLF/Pipeline.hs` are intentionally thin, but `src-public/MyLib.hs` still re-exports `MLF.Constraint.Types` and a placeholder `someFunc`, which looks like legacy/demo baggage rather than a crisp API boundary.
- `test/Main.hs` manually imports and registers many spec modules, which keeps order explicit but adds maintenance burden and makes the test harness itself a coupling point.

## Explorer Cross-Check
- Independent review confirms the strongest architectural baseline is the package split in `mlf2.cabal`: a narrow public library in front of a private internal library.
- Independent review also reinforces that the main risk is boundary porosity: broad compatibility/testing facades like `src/MLF/Constraint/Types.hs` and `src/MLF/Constraint/Presolution.hs` are still common import anchors.
- The same review highlights `src/MLF/Elab/Run/Pipeline.hs` and `src/MLF/Constraint/Presolution/Driver.hs` as thin-but-dependency-dense orchestration layers that may amplify ripple effects.


## Prioritized Improvement Areas
1. **Tighten façade boundaries.** Treat `src/MLF/Constraint/Types.hs` and `src/MLF/Constraint/Presolution.hs` as compatibility shells only. New/refactored code should import narrower owner modules directly; testing helpers should move out of production façades. Evidence: 82 modules currently import both `MLF.Constraint.Types` and `MLF.Constraint.Types.Graph`, and `MLF.Constraint.Presolution` remains a central testing-oriented hub.
2. **Split the largest algorithm modules by invariant.** Highest-value candidates are `src/MLF/Elab/Phi/Omega.hs`, `src/MLF/Constraint/Presolution/EdgeUnify.hs`, `src/MLF/Reify/Core.hs`, `src/MLF/Constraint/Solve.hs`, `src/MLF/Elab/Elaborate.hs`, and `src/MLF/Constraint/Presolution/Plan.hs`. These files are large enough that even good internal structure becomes hard to navigate.
3. **Thin orchestration layers further.** Modules like `src/MLF/Elab/Run/Pipeline.hs` and `src/MLF/Constraint/Presolution/Driver.hs` should stay assembly-focused and push rule-specific logic into smaller typed stage modules/records.
4. **Decompose giant specs and reduce manual test wiring.** `test/ElaborationSpec.hs` and `test/Presolution/WitnessSpec.hs` are coverage strengths but maintenance liabilities; consider domain-sliced spec modules plus lighter `test/Main.hs` registration.
5. **Consolidate operational/documentation surfaces.** Repo knowledge is spread across `tasks/`, root planning files, `docs/plans/`, and top-level tracking docs. There is also at least one concrete doc drift: `docs/architecture.md`'s public API section omits `MLF.XMLF`, although `mlf2.cabal` exposes it.

## Bottom Line
- The repository has a strong architectural base: clear public/private package separation, meaningful domain decomposition, and unusually rich thesis/documentation support.
- The biggest improvement space is not “invent a new architecture”; it is **reducing porous compatibility seams and shrinking the few remaining hubs** so the existing architecture is enforced more consistently.

## Public API / Process Notes
- `src-public/MyLib.hs` still blurs the public-boundary story: it is publicly exposed, re-exports `MLF.Constraint.Types`, and still contains the template-style `someFunc` placeholder.
- `src-public/MLF/API.hs`, `src-public/MLF/Pipeline.hs`, and `src-public/MLF/XMLF.hs` are sensible surfaces, but they lack short module-level guidance about intended audience and stability tier.
- The test tree mirrors subsystem boundaries well, but package-boundary regression coverage is weaker than internal algorithm coverage.
- Workflow guidance is split between `AGENTS.md`, `tasks/readme`, root planning files, and multiple active `tasks/todo/` folders; this makes the current source of truth less obvious than it should be.
- Minor repo-hygiene noise exists (`.DS_Store` artifacts in tracked directories), though this is secondary to the architectural issues above.
