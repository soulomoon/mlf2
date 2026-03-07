# Findings

## Thesis-side constraints
- Primary thesis anchors for this campaign are Chapters/sections `§4.4`, `§5.3.2`, `§5.4.3`, `§6.2`, `§15.2`, and `§15.3.4–§15.3.8`.
- `§9.2.6` defines constraint interiors / structural interiors / frontiers; these are graph notions, not plain trees.
- `§10.4.1` treats presolutions as fully solved constraints.
- `§15.2.4` introduces the leftmost-lowermost `<P` node order so translated binders remain in scope.
- `§15.2.6` changes `let` translation shape for easier scope handling and states that trivial scheme edges elaborate to identity computations in principal presolutions.
- `Definition 15.2.10` / `Theorem 15.2.11` define translatable presolutions and the conditions required for elaboration.
- `§15.2.8` says that when xMLF is used as an internal language, the translation should be modular and weakens all inert nodes in a presolution.
- `§15.3.3` / `§15.3.4` define typing environments and computation contexts; these depend on binder order and exact placement of operations.
- `§15.3.6` / `§15.3.9` tie elaboration correctness to those exact translation obligations.

## Thesis implication for refactoring
- True tree layers are suitable for recursion-schemes refactors.
- Constraint / presolution / reify / witness-ordering layers are thesis-sensitive graph algorithms and need explicit guardrail rows if this campaign is meant to be thesis exact.

## Codebase-side evidence
- Existing recursion-schemes infrastructure already exists in `src/MLF/Util/RecursionSchemes.hs` and indexed recursion support in `src/Util/IndexedRecursion.hs`.
- Existing `Base`/`Recursive` coverage exists for frontend/raw types in `src/MLF/Frontend/Syntax.hs`, elaboration types/terms in `src/MLF/Types/Elab.hs`, and witness expansions in `src/MLF/Constraint/Types/Witness.hs`.
- Likely tree-shaped simplification targets:
  - `src/MLF/Frontend/Desugar.hs`
  - `src/MLF/Frontend/Normalize.hs`
  - `src/MLF/Elab/TermClosure.hs`
  - `src/MLF/Elab/Sigma.hs`
- Likely guardrail / do-not-force-recursion-schemes zones:
  - `src/MLF/Reify/Core.hs`
  - `src/MLF/Reify/TypeOps.hs`
  - `src/MLF/Constraint/Presolution/WitnessCanon.hs`
  - broader presolution / solve / graph-rewrite pipeline where DAG identity, interior/frontier, and phase ordering matter.

## Existing artifact conventions
- Reuse the repo’s goal-table shape from `docs/notes/2026-03-05-goal-transformation-mechanism-table.md`.
- Reuse the current improving-loop role split and strict JSONL logging pattern from `docs/prompts/goal-improving-loop-agent.prompt.md` and `docs/prompts/improving-loop-agent.prompt2.md`.
- Keep gate vocabulary exactly `YES` / `NO` and terminal status vocabulary exactly `COMPLETED` / `FAILED` / `MAXIMUMRETRY`.

## Design fork to resolve with user
- Narrow scope: only tree-shaped refactor rows.
- Broad scope: tree-shaped refactor rows plus explicit thesis guardrails for graph/presolution/reify phases.
- Recommendation: broad scope, because the guardrails are part of the thesis-exact story.

## User-approved orchestrator simplification
- The improving loop should not use separate `Researcher A` / `Researcher B` roles.
- `Planner` is responsible for both thesis/code research and evidence reconciliation before implementation begins.
- Keep the rest of the strict gate-owning roles (`Verifier`, `Implementer`, `Reviewer`, `QA`, `Integrator`).
