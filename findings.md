# Findings

## Current Observations

- The design is implementable as a docs-first skill-contract update. It does not require any change to `$run-orchestrator-loop`, and it explicitly keeps runtime rounds out of scope.
- The current `scaffold-orchestrator-loop` materials are bootstrap-only: `SKILL.md` says to use the skill only when no top-level `orchestrator/` exists, `references/repo-contract.md` documents only initial scaffold creation, and `references/roadmap-generation.md` speaks only about an initial roadmap.
- The existing scaffold assets already expose the state fields that `next-family` needs. `assets/orchestrator/state.json` and `assets/orchestrator/state-schema.md` do not appear to require changes if `next-family` rewrites the live repo-local state directly and bootstrap behavior stays as-is.
- The current scaffold assets do not include top-level pointer-stub templates such as `orchestrator/roadmap.md`, `orchestrator/verification.md`, or `orchestrator/retry-subloop.md`. The cleanest reading of the design is to keep those files optional compatibility surfaces that must be refreshed when present in `next-family`, not new required bootstrap assets in this change.
- There is one real contract drift to fix in the plan: the design and live repo-local state both treat `retry` as part of terminality and reset semantics, but the scaffold skill's documented state schema and bootstrap `assets/orchestrator/state.json` do not currently include that field.
