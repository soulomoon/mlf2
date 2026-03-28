# Round 141 Selection

## Selected roadmap item

- **Item:** `item-3` — Ensure reification and elaboration produce correct μ-types and roll/unroll terms.

## Roadmap metadata (from `orchestrator/state.json`)

- **roadmap_id:** `2026-03-29-00-automatic-iso-recursive-type-inference-implementation`
- **roadmap_revision:** `rev-001`
- **roadmap_dir:** `orchestrator/roadmaps/2026-03-29-00-automatic-iso-recursive-type-inference-implementation/rev-001`

## Why this item is selected now

- The roadmap ordering requires selecting the lowest-numbered unfinished item.
- Items `item-1` and `item-2` are already marked `[done]`; `item-3` is the first `[pending]` item.
- Dependency for `item-3` is satisfied (`Depends on: item-2`).
- `orchestrator/state.json` also points at `current_task: "item-3"` for active `round-141`, matching the next actionable item.

## Why it should run in this round

- This roadmap is an implementation loop with `max_parallel_rounds: 1`; no parallel round should be opened.
- `item-3` validates that automatic μ-introduction (completed in items 1–2) is correctly carried through reification/elaboration and checked by Phase 7.
- Advancing to later items (`item-4+`) before this would violate dependency order because edge-case hardening and end-to-end validation depend on correct `TMu`/`ERoll`/`EUnroll` behavior first.

## Key implementation context for planner

- Scope is strictly `item-3` only (no roadmap expansion).
- Required acceptance signals from roadmap completion notes:
  - Automatically inferred `TyMu` reifies to `TMu` in elaborated output.
  - Elaborated terms include `ERoll`/`EUnroll` at recursive use sites.
  - `MLF.Elab.TypeCheck` accepts those elaborated terms.
  - Focused tests cover:
    - self-recursive elaborated type includes `μ`
    - `ERoll`/`EUnroll` presence in elaborated term
    - `typeCheck` success on elaborated term
    - `runPipelineElabChecked` end-to-end success
- Repository guidance constraints (`AGENTS.md`):
  - Keep changes minimal and root-cause oriented; avoid compatibility fallbacks.
  - Preserve thesis-faithful behavior for non-recursive programs and document intentional deviations.
  - Validate with `cabal build all && cabal test` before completion claims for behavior-changing work.
- Bug tracker context (`Bugs.md`):
  - No currently open bugs are listed; proceed without an active blocker override.
