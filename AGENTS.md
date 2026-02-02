# Repository Guidelines

## Project Goals

- Keep the implementation paper-faithful to `papers/these-finale-english.txt` (thesis + graphic-constraint pipeline; more detailed than `papers/xmlf.txt`). Document and test any intentional deviations.
- Treat `papers/these-finale-english.txt` as the source of truth. Implement supplementary details from `papers/xmlf.txt` only when the thesis is silent, and document any conflicts or deviations.

## Project Structure & Module Organization

- `src/` contains the private implementation library (`mlf2-internal`). Most logic lives in `src/MLF/` and is organized by domain: `MLF.Frontend.*`, `MLF.Constraint.*`, `MLF.Binding.*`, `MLF.Witness.*`, `MLF.Elab.*`, `MLF.Util.*`.
- `src-public/` contains the public library entry points intended for downstream users: `MLF.API`, `MLF.Pipeline`, and legacy `MyLib`.
- `app/` contains the executable entry point (`app/Main.hs`) for the `mlf2` binary.
- `test/` contains the Hspec suite (`*Spec.hs`) and a manual test runner (`test/Main.hs`).
- `papers/` holds reference material (PDF/TXT) used to align the implementation with the xMLF/MLF papers; it is not required to build.
- `MLF.Constraint.Types.EdgeWitness` now records `ewSteps` (interleaved O/Ω steps for Φ) alongside Ω-only `ewWitness`.
- Shared unification logic lives in `MLF.Constraint.Unify.Core`; configure phase-specific behavior via `UnifyStrategy` instead of duplicating unification loops.
- Shared structural decomposition lives in `MLF.Constraint.Unify.Decompose`; presolution structural unification should call `decomposeUnifyChildren` after handling TyVar/TyExp special cases.
- Legacy expansion-to-instantiation translation lives in `MLF.Elab.Legacy`; `MLF.Elab.Elaborate`/`MLF.Elab.Pipeline` re-export `expansionToInst` for compatibility.

## Build, Test, and Development Commands

- `cabal build` — build the library and executable into `dist-newstyle/`.
- `cabal test` — run the `mlf2-test` suite (Hspec).
- `cabal test --test-show-details=direct` — rerun tests with per-example output (useful when debugging failures).
- `cabal run mlf2` — run the demo executable.
- `cabal repl mlf2` / `cabal repl mlf2-test` — open GHCi with the chosen target loaded.

## Coding Style & Naming Conventions

- Match existing formatting: 4-space indentation, explicit module export lists, and GHC-style `{- Note [...] -}` blocks for design rationale.
- Keep builds warning-free (`-Wall` is enabled in `mlf2.cabal`). Prefer total pattern matches and clear error constructors.
- When adding new modules under `src/`, update `mlf2.cabal` `other-modules`/`exposed-modules` so Cabal compiles them.
- Many modules import `MLF.Constraint.Types` unqualified; when adding new exports with common names, check for clashes and use `hiding` or explicit imports as needed.
- Constraint graph identifiers (`NodeId`, `NodeMap`, `GenNodeId`, `GenNodeMap`, `NodeRef` + helpers) live in `MLF.Constraint.Types.Graph`; `MLF.Constraint.Types` re-exports them for compatibility.
- Core graph node/edge/constraint types (`BaseTy`, `TyNode`, `InstEdge`, `UnifyEdge`, `Constraint`, `BindFlag`/`BindParents`, `GenNode`, `EdgeId`, `ExpVarId`) live in `MLF.Constraint.Types.Graph`; `MLF.Constraint.Types` re-exports them for compatibility, while witness types live in `MLF.Constraint.Types.Witness`.
- Presolution-only types (`Presolution`, `SolverState`, `DepGraph`) live in `MLF.Constraint.Types.Presolution`; `MLF.Constraint.Types` re-exports them for compatibility.
- Binding tree errors (`BindingError`) live in `MLF.Constraint.Types.Graph` and are re-exported from `MLF.Constraint.Types` for compatibility.
- Gen-node maps use `GenNodeMap` (`cGenNodes`); prefer `lookupGen`/`insertGen`/`fromListGen` or unwrap with `getGenNodeMap` when an `IntMap` API is required.
- Presolution.Base defines node-set newtypes (`InteriorNodes`, `FrontierNodes`); prefer these + helpers over raw `IntSet` when plumbing node sets across modules.
- Presolution traces use `CopyMapping` (`EdgeTrace.etCopyMap`); prefer `lookupCopy`/`insertCopy`/`copiedNodes`/`originalNodes` and unwrap with `getCopyMapping` when an `IntMap` API is required.
- Tracing is explicit: pass `TraceConfig` (e.g., `defaultTraceConfig` or `pcTraceConfig defaultPipelineConfig`) into presolution/solve/elab helpers and `runPresolutionM`.
- Elaboration entry points now bundle inputs as `ElabConfig`/`ElabEnv`; prefer `elaborateWithEnv` for new call sites.
- Presolution state access should go through `MonadPresolution` plus `MLF.Constraint.Presolution.Ops`/`StateAccess`; avoid new direct `gets psConstraint`/`gets psUnionFind` and manual `Binding` error lifting.
- Edge-level presolution helpers in `MLF.Constraint.Presolution.EdgeProcessing` should use `EdgeCtx` to snapshot let/ann edge checks and trace config instead of ad hoc state reads.
- For redirect + union-find canonicalization, prefer `MLF.Constraint.Canonicalizer` (idempotent and cycle-safe) over ad hoc chase functions.
- Naming conventions:
  - Modules: `src/MLF/Foo/Bar.hs` defines `module MLF.Foo.Bar`.
  - Public entry modules: `src-public/MLF/API.hs` defines `module MLF.API` (similarly `MLF.Pipeline`).
  - Tests: `test/FooSpec.hs` defines `spec :: Spec`.

## Testing Guidelines

- Framework: Hspec (`hspec`).
- `mlf2-test` can only import exposed modules; if tests need helpers/types from internal modules, re-export them from an exposed entrypoint (e.g., `MLF.Constraint.Presolution`).
- When adding a new spec module, wire it into both:
  - `mlf2.cabal` → `test-suite mlf2-test` → `other-modules`
  - `test/Main.hs` (import the module and call `spec`)

## Commit & Pull Request Guidelines

- Commit messages typically use imperative, descriptive subjects (examples from history: `Add …`, `Improve …`, `Phase6: …`).
- PRs should include: a short problem statement, approach summary, new/updated tests, and any relevant doc updates (`implementation_notes.md`, `roadmap.md`) when changing algorithmic behavior.

## Task Management (Ralph)

The `tasks/` folder contains PRDs and progress tracking for autonomous agent execution:

```
tasks/
├── prd.json                    # Current Ralph PRD (JSON format for autonomous execution)
├── prd-*.md                    # Human-readable PRD documentation
├── progress.txt                # Progress log updated by Ralph during execution
└── archive/                    # Completed PRDs organized by date and feature
    └── YYYY-MM-DD-feature-name/
        ├── prd.json
        ├── prd-*.md
        └── progress.txt
```

**For autonomous agents (Ralph):**
- Read `tasks/prd.json` to get the current user stories and acceptance criteria
- Update `tasks/progress.txt` with iteration results
- Stories are ordered by priority/dependency — execute in order
- Each story should be completable in one iteration (one context window)
- Mark `passes: true` in prd.json when a story's acceptance criteria are met

**Validation command:** `cabal build all && cabal test`
