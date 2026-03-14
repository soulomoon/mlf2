# Round 006 Selection

Selected roadmap item: **1. Write the `R1` gap map from `ARI-C1` to unannotated single-SCC, single-binder-family inference** (`orchestrator/roadmap.md`, item 1).

Why this item now:
- The successor roadmap is at `select-task` with all items still pending, and item 1 is the required first dependency for items 2 through 5.
- The approved successor design explicitly says the next track should widen from accepted `ARI-C1` by a conservative ladder; the smallest research slice is to map the exact delta before choosing any unannotated subset or feasibility gate.
- Round-005 already fixed the bounded `ARI-C1` baseline, so the next useful move is to name what information still comes only from explicit anchors, what might be recoverable locally without widening semantics, and which unannotated cases stay blocked.
- This keeps the path honest and fail-closed while preserving the fixed boundary model: single-SCC obligation-level recursion only, single binder family only, no cross-family SCC linking, non-equi-recursive semantics, and no cyclic structural graph encoding.

Round-006 scope guard:
- Produce only the `R1` gap-map artifact for the successor track.
- Keep the inherited `ARI-C1` baseline and accepted item-2 invariant audit authoritative; use them to frame the delta, not to reopen prior rounds or rewrite predecessor evidence.
- Make the boundary explicit throughout: single-SCC only, single binder family only, non-equi-recursive only, non-cyclic structural graph only.
- Do not select the `R2` subset, do not write the `R3` obligation contract, do not run the `R4` feasibility decision, and do not write the `R5` handoff/stop artifact in this round.
- Do not update `orchestrator/state.json` or `orchestrator/roadmap.md`, and do not edit production code, tests, or task packets outside this round worktree.
