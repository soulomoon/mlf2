# Thesis-Exact Recursion Refactor Goal Loop Design

**Goal:** Build a thesis-exact mechanism table and improving-loop prompt for a Haskell refactor/simplification campaign that uses recursion-schemes only where they preserve the thesis semantics exactly.

## Source of truth
- Primary: `papers/these-finale-english.txt`
- Supplementary only when the thesis is silent: `papers/xmlf.txt`
- Repository evidence index: `docs/thesis-obligations.yaml`

## Approved scope
The campaign is intentionally **broad**:
- it includes positive rows for tree-shaped refactors where recursion-schemes can simplify code safely;
- it also includes explicit guardrail rows for graph/presolution/reify machinery that must stay explicit because the thesis depends on DAG identity, binding trees, structural interiors/frontiers, permission evolution, `<P` ordering, and witness-order normalization.

This was chosen to keep the table thesis exact rather than merely style-driven.

## Thesis basis
The mechanism set is anchored primarily in:
- `§4.4`, `§5.3.2`, `§5.4.3`, `§6.2` for ordered graphic transformations / permissions;
- `§9.2.6` and `§10.4.1` for interiors and presolutions;
- `§15.2.4`–`§15.2.8` for node ordering, let-scope handling, translatable presolutions, and xMLF as an internal language;
- `§15.3.3`–`§15.3.6` for typing environments, computation contexts, edge translation, and elaboration;
- `§14.4.2` / `§14.4.3` where binder-preserving substitution matters for tree refactors.

## Approved mechanism rows
1. `Surface Preprocessing Exactness`
2. `Leftmost-Lowermost Quantifier Ordering`
3. `Let-Scope Translation Discipline`
4. `Translatable Presolution Boundary`
5. `Typing Environment Construction`
6. `Computation Context Construction`
7. `Binder-Safe Tree Recursion Coverage`
8. `Graph-Phase Explicitness Guardrail`

## Evidence model
Each row records:
- current codebase behavior;
- target behavior phrased from thesis obligations first;
- a concise gap summary;
- evidence from three layers: thesis anchor, code anchor, and regression/obligation tests;
- a strict `YES` / `NO` gate;
- a concrete next action.

The initial table starts all rows at `NO` until a fresh verifier-owned sweep confirms the current state row by row.

## Orchestrator design
Approved role model:
- `Orchestrator`
- `Verifier`
- `Planner`
- `Implementer`
- `Reviewer`
- `QA`
- `Integrator`

Design decisions:
- no separate `Researcher A` / `Researcher B` roles;
- `Planner` performs both thesis-side and code-side research, then reconciles evidence before implementation begins;
- `Verifier` owns row refreshes and the thesis-exact `YES` / `NO` gate;
- the planner must classify the selected row as either `positive_refactor` or `guardrail`;
- guardrail rows may legitimately succeed by preserving explicit graph-aware code plus tests/docs rather than introducing new abstractions.

## Artifact contract
The campaign produces four top-level artifacts:
- `docs/notes/2026-03-07-thesis-exact-recursion-refactor-mechanism-table.md`
- `docs/prompts/thesis-exact-recursion-refactor-improving-loop-agent.prompt.md`
- `docs/prompts/2026-03-07-orchestrated-execution-thesis-exact-recursion-refactor-codex-subagents-fresh-round-2.jsonl`
- `docs/plans/2026-03-07-thesis-exact-recursion-refactor-goal-loop.md`

`orchestrator-log.jsonl` is the single authoritative orchestrator event log for future runs. Human-readable summaries belong in run-local `findings.md` / `progress.md` files.

## Non-goals
- Do not force recursion-schemes onto graph/presolution/reify code just for stylistic uniformity.
- Do not collapse thesis-owned phase boundaries into generic traversals if that obscures order, scope, or witness semantics.
- Do not mark a row `YES` without verifier-owned evidence and the required validation.
