# Repository Guidelines

## Guideline Maintenance

- `AGENTS.md` is the repository guidance entry point. Keep it accurate, well-structured, and up to date when gaps or stale instructions are discovered.
- Before starting substantive work, review these guidelines and update them if they are stale, contradictory, or missing important workflow constraints.
- Apply instruction precedence in this order: direct system/developer/user instructions, deeper nested `AGENTS.md` files, this file, then supporting project docs.
- Keep adjacent guidance surfaces synchronized when relevant: `tasks/readme`, `TODO.md`, `implementation_notes.md`, `CHANGELOG.md`, and `Bugs.md`.
- Respect existing workspace changes: do not overwrite, revert, or “clean up” unrelated uncommitted edits unless the user explicitly asks.

## Project Goals

- Keep the implementation paper-faithful to `papers/these-finale-english.txt` (thesis + graphic-constraint pipeline; more detailed than `papers/xmlf.txt`). Document and test any intentional deviations.
- Treat `papers/these-finale-english.txt` as the source of truth. Implement supplementary details from `papers/xmlf.txt` only when the thesis is silent, and document any conflicts or deviations.
- When thesis-faithfulness and code convenience conflict, prefer the thesis and record the reasoning in tests/docs.
- You do not need to be backwards-compatible, you should not add new compatibility layers or convenience fallbacks, and you should fix problems at the root cause even if that requires more work upfront. The goal is a clean, faithful implementation, not a smooth migration or a convenient API.

## Project Structure & Module Organization

- `src/` contains the private implementation library (`mlf2-internal`). Most logic lives in `src/MLF/` and is organized by domain: `MLF.Frontend.*`, `MLF.Constraint.*`, `MLF.Binding.*`, `MLF.Witness.*`, `MLF.Elab.*`, `MLF.XMLF.*`, `MLF.Reify.*`, `MLF.Types.*`, `MLF.Util.*`.
- `src-public/` contains the public library entry points intended for downstream users: `MLF.API`, `MLF.Pipeline`, and `MLF.XMLF`.
- `app/` contains the executable entry point (`app/Main.hs`) for the `mlf2` binary.
- `test/` contains the Hspec suite (`*Spec.hs`), the manual test runner (`test/Main.hs`), and frozen parity tooling/artifacts (`test/Parity/FrozenParityGenMain.hs`, `test/golden/legacy-replay-baseline-v1.json`).
- `papers/` holds reference material (PDF/TXT) used to align the implementation with the xMLF/MLF papers; it is not required to build.
- Witness metadata lives in `MLF.Constraint.Types.Witness`; `EdgeWitness` records per-edge reconstruction metadata including `ewForallIntros` and Ω witness payload `ewWitness`.
- Shared unification logic lives in `MLF.Constraint.Unify.Core`; configure phase-specific behavior via `UnifyStrategy` instead of duplicating unification loops.
- Shared structural decomposition lives in `MLF.Constraint.Unify.Decompose`; presolution structural unification should call `decomposeUnifyChildren` after handling `TyVar`/`TyExp` special cases.
- Legacy expansion-to-instantiation translation lives in `MLF.Elab.Legacy`; `MLF.Elab.Elaborate`/`MLF.Elab.Pipeline` re-export `expansionToInst` for compatibility.

### Key Data Types

- `Expr` (`MLF.Frontend.Syntax`): Surface language (eMLF terms)
- `Constraint` (`MLF.Constraint.Types`): The constraint graph with nodes, edges, and binding tree
- `TyNode`: Type nodes in the term-DAG (`TyVar`, `TyArrow`, `TyForall`, `TyBase`, `TyExp`, `TyBottom`)
- `InstEdge`: Instantiation edges (`≤`) between nodes
- `BindParents`: Binding tree as child → `(parent, BindFlag)` map
- `Expansion`: Presolution recipes (identity, ∀-intro, instantiation, composition)
- `EdgeWitness`: Per-edge witness metadata for xMLF instantiation reconstruction

## Build, Test, and Development Commands

- `cabal build` — build the library and executable into `dist-newstyle/`.
- `cabal test` — run the `mlf2-test` suite (Hspec).
- `cabal test --test-show-details=direct` — rerun tests with per-example output (useful when debugging failures).
- `cabal run mlf2` — run the demo executable.
- `cabal run frozen-parity-gen -- --generated-on YYYY-MM-DD --source-commit <sha>` — regenerate the frozen parity baseline JSON (defaults to `test/golden/legacy-replay-baseline-v1.json`).
- `cabal repl mlf2` / `cabal repl mlf2-test` — open GHCi with the chosen target loaded.
- Full verification gate for code changes: `cabal build all && cabal test`.

## Working Norms

- Fix problems at the root cause when feasible; do not introduce new compatibility layers or convenience fallbacks without a paper-backed reason.
- Keep changes minimal, local, and warning-free; avoid unrelated refactors unless they are required for correctness or guideline hygiene.
- Update tests and docs in the same change whenever behavior, architecture, or thesis-alignment expectations move.
- Prefer targeted validation first; run the full Cabal gate before claiming behavior-changing work is complete. Pure guidance-only edits do not require the full gate.
- Guidance-only cleanups belong in docs/changelog, not `Bugs.md`, unless they reveal a real implementation defect or thesis-faithfulness gap.

## Coding Style & Naming Conventions

- Use `haskell-pro` (`/Users/ares/.agents/skills/haskell-pro/SKILL.md`) as the default style guide for Haskell design decisions, including expressive types, pure IO boundaries, and total functions.
- Match existing formatting: 4-space indentation, explicit module export lists, and GHC-style `{- Note [...] -}` blocks for design rationale.
- Keep builds warning-free (`-Wall` is enabled in `mlf2.cabal`). Prefer total pattern matches and clear error constructors.
- When adding new modules under `src/`, `src-public/`, or `test/`, update the matching `mlf2.cabal` `other-modules`/`exposed-modules` stanzas so Cabal compiles them.
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
- Edge-level presolution flow is split across planner/interpreter passes (`MLF.Constraint.Presolution.EdgeProcessing.Planner` + `.Interpreter`) via typed `EdgePlan`; carry new edge checks/flags in the plan payload instead of ad hoc interpreter state reads.
- For redirect + union-find canonicalization, prefer `MLF.Constraint.Canonicalizer` (idempotent and cycle-safe) over ad hoc chase functions.
- Naming conventions:
  - Modules: `src/MLF/Foo/Bar.hs` defines `module MLF.Foo.Bar`.
  - Public entry modules: `src-public/MLF/API.hs` defines `module MLF.API` (similarly `MLF.Pipeline` and `MLF.XMLF`).
  - Tests: `test/FooSpec.hs` defines `spec :: Spec`.

## Testing Guidelines

- Framework: Hspec (`hspec`).
- `mlf2-test` can only import exposed modules; if tests need low-level internal helpers/types, expose them through an explicit internal test-support entrypoint instead of widening a production facade.
- When adding a new spec module, wire it into both:
  - `mlf2.cabal` → `test-suite mlf2-test` → `other-modules`
  - `test/Main.hs` (import the module and call `spec`)
- Prefer the narrowest test slice that covers the change, then expand to broader suites as confidence increases.

## Commit & Pull Request Guidelines

- Commit messages typically use imperative, descriptive subjects (examples from history: `Add …`, `Improve …`, `Phase6: …`).
- PRs should include: a short problem statement, approach summary, new/updated tests, and any relevant doc updates (`implementation_notes.md`, `roadmap.md`) when changing algorithmic behavior.
- When a change makes meaningful project progress, add a concise corresponding entry to `CHANGELOG.md` in the same update.

## Task Management (Skill-Driven File Planning)

The `tasks/` folder contains task plans and execution tracking for autonomous agent execution.

Preferred process skills:
- `[$using-superpowers](/Users/ares/.codex/superpowers/skills/using-superpowers/SKILL.md)`
- `[$planning-with-files](/Users/ares/.codex/skills/planning-with-files/SKILL.md)`

Parallel execution helpers, when the work genuinely splits into independent tracks:
- `[$dispatching-parallel-agents](/Users/ares/.codex/superpowers/skills/dispatching-parallel-agents/SKILL.md)`
- `[$subagent-driven-development](/Users/ares/.codex/superpowers/skills/subagent-driven-development/SKILL.md)`

Create a new task under `tasks/todo/` as a folder named `YYYY-MM-DD-description/` (for example `2026-02-03-thesis-exact-coercions/`). Completed tasks move to `tasks/archive/` with the same folder name.

```
tasks/
├── readme
├── todo/
│   └── YYYY-MM-DD-description/
│       ├── task_plan.md        # Phase plan, status, decisions, and error log
│       ├── findings.md         # Key discoveries and paper/code alignment notes
│       ├── progress.md         # Iteration log, commands run, and outcomes
│       ├── mechanism_table.md  # Optional YES/NO goal table for orchestrated loop tasks
│       ├── orchestrator_prompt.md  # Optional role-separated orchestration brief
│       └── orchestrator-log.jsonl  # Optional authoritative machine-readable loop log
└── archive/                    # Completed tasks organized by date and description
    └── YYYY-MM-DD-description/
        ├── task_plan.md
        ├── findings.md
        ├── progress.md
        ├── mechanism_table.md
        ├── orchestrator_prompt.md
        └── orchestrator-log.jsonl
```

**For autonomous agents (using-superpowers + planning-with-files):**
- Invoke `[$using-superpowers](/Users/ares/.codex/superpowers/skills/using-superpowers/SKILL.md)` first, then follow `[$planning-with-files](/Users/ares/.codex/skills/planning-with-files/SKILL.md)` for execution.
- Work from a single active task folder under `tasks/todo/YYYY-MM-DD-description/` for the current effort.
- Initialize and maintain `task_plan.md`, `findings.md`, and `progress.md` in that task folder before substantial multi-step work.
- For orchestrated loop campaigns, also keep `mechanism_table.md`, `orchestrator_prompt.md`, and `orchestrator-log.jsonl` in the same task folder; treat the JSONL file as the authoritative round log.
- When a repo-local top-level `orchestrator/` exists, treat it as the live successor control plane for repo-wide round execution: `orchestrator/state.json` is machine state and must name the authoritative roadmap bundle through `roadmap_id`, `roadmap_revision`, and `roadmap_dir`; the live roadmap contract is `roadmap_dir/roadmap.md`, `roadmap_dir/retry-subloop.md`, `roadmap_dir/verification.md`, plus `orchestrator/roles/*.md`. The top-level `orchestrator/roadmap.md`, `orchestrator/retry-subloop.md`, and `orchestrator/verification.md` files are pointer stubs only.
- If that top-level `orchestrator/` explicitly takes over an older task-folder campaign, keep the predecessor packet immutable as historical evidence unless a round explicitly says it is updating a human-facing summary; do not rewrite the predecessor authoritative log.
- For repo-local orchestrator runtime, `stage: "done"` is non-terminal whenever the authoritative `roadmap_dir/roadmap.md` still has `[pending]` or `[in-progress]` items. In that state, the controller must continue immediately into the next `select-task` transition instead of stopping or replying as if the loop were complete.
- Re-read `task_plan.md` before major decisions, and update phase status after each completed phase.
- Log all errors and recovery attempts in `task_plan.md`; do not repeat the same failed action unchanged.
- Write discoveries to `findings.md` throughout execution and keep `progress.md` as the running session log.
- Treat root-level `task_plan.md`, `findings.md`, and `progress.md` as historical artifacts unless a task explicitly says otherwise; current work belongs in the task folder.
- Maintain the repository root `TODO.md` as the rolling list of next goals; update it whenever priorities or upcoming work change. If priorities stay the same, leave it untouched.
- Maintain `implementation_notes.md` when behavior, architecture, or thesis-alignment details change, so documentation stays in sync with implementation.
- Close a task by marking all phases complete in `task_plan.md` and moving the folder to `tasks/archive/`.

## Bug Tracking

- Maintain `/Volumes/src/mlf4/Bugs.md` as the canonical bug tracker for implementation defects and thesis-faithfulness gaps.
- For every newly discovered bug, add an entry in `Bugs.md` in the same iteration with:
  - unique bug ID, status, and discovery date
  - minimal reproducer (expression and/or test command)
  - expected vs actual behavior
  - suspected module/file ownership
  - thesis impact note (if any)
- When a bug is fixed, move it to a resolved section in `Bugs.md` and link the regression test path(s).

## Paper References

- Primary source: `papers/these-finale-english.txt` (thesis)
- Supplementary: `papers/xmlf.txt` (used only when the thesis is silent)
- Document any conflicts or deviations from the papers.

## Subagent Model
- When creating a subagent via `spawn_agent`, explicitly set `model` to `"gpt-5.4"` by default.
- Thinking level set to xhigh for all subagents by default.
- Do not rely on inherited or platform-default subagent model selection.
- Only use a different subagent model if the user explicitly requests it.
