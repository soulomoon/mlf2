# Round 142 Selection

- Selected item: **item-4** — *Harden edge cases: nested recursion, polymorphic recursion, interaction with ∀*
- Why: it is the **lowest pending** roadmap item and its dependency chain is satisfied (`item-3` is done).
- roadmap_id: `2026-03-29-00-automatic-iso-recursive-type-inference-implementation`
- roadmap_revision: `rev-001`
- roadmap_dir: `orchestrator/roadmaps/2026-03-29-00-automatic-iso-recursive-type-inference-implementation/rev-001`

## Planner context

Focus on the roadmap completion notes for item-4:

- nested recursive lets
- recursive functions with polymorphic type annotations
- interaction between `μ` and `∀` quantifiers (recursive polymorphic types)
- recursive functions that return functions (higher-order recursion)
- already-annotated `μ` types continue to work unchanged

Add regression tests for each case; keep behavior unchanged for already-working non-edge cases.
