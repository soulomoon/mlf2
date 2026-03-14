# Round 007 Selection

Selected roadmap item: **2. Select exactly one bounded unannotated candidate subset and admissibility contract** (`orchestrator/roadmap.md`, item 2).

Why this item now:
- `R1` was completed in accepted `round-006`, so the roadmap dependency chain now points directly to item 2 as the next pending stage.
- The `R1` gap map already reduced the problem to a finite set of deltas and assigned the first narrowing work to `R2`, especially recursive-binder obligation discovery and `single-SCC` bounding (`G1`, `G2`), which makes subset selection the smallest research slice that materially sharpens the path from bounded `ARI-C1` toward the unannotated target.
- Choosing exactly one stable candidate subset now prevents later rounds from drifting across multiple unannotated designs or silently widening beyond the fixed boundary model.
- Item 3 cannot be written coherently before one subset is chosen, and items 4 and 5 are explicitly downstream of that choice, so selecting item 2 is the only bounded move that advances the successor track without acting as controller or reopening prior evidence.

Round-007 scope guard:
- Produce only the `R2` selection artifact: choose exactly one bounded unannotated candidate subset, give it a stable identifier, and state its admissibility contract.
- Keep the fixed boundary explicit throughout: `single-SCC` obligation-level recursion only, `single-binder-family` ownership only, no cross-family SCC linking, non-equi-recursive semantics, and no cyclic structural graph encoding.
- Record deferred alternatives that may remain plausible later and rejected alternatives that already breach the fixed boundary model.
- Do not write the `R3` inference-obligation contract, do not run the `R4` bounded feasibility decision, and do not draft the `R5` implementation-handoff or research-stop artifact in this round.
- Do not update `orchestrator/state.json` or `orchestrator/roadmap.md`, and do not edit production code, tests, `TODO.md`, `CHANGELOG.md`, `implementation_notes.md`, or any task packet outside this round worktree.
