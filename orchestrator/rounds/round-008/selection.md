# Round 008 Selection

Selected roadmap item: **3. Write the `R3` inference-obligation contract for the chosen subset** (`orchestrator/roadmap.md`, item 3).

Why this item now:
- `R1` was completed in accepted `round-006` and `R2` was completed in accepted `round-007`, so item 3 is now the next pending dependency in the successor roadmap.
- `R2` already fixed exactly one bounded candidate subset, `URI-R2-C1`, which means the smallest research slice that sharpens the path toward the unannotated target is to write the obligation contract for that one subset rather than revisit subset selection or jump ahead to feasibility.
- The approved roadmap design requires `R3` to carry forward the inherited invariant classes into an explicit contract covering acyclicity, binder ownership and scope discipline, occurs-check/termination, reconstruction/reification/witness replay, principality boundaries, and fail-closed rejection cases.
- Running `R4` now would be premature because the bounded feasibility decision depends on a reviewer-visible statement of exactly which obligations `URI-R2-C1` must satisfy; without that contract, feasibility would drift or silently widen beyond the accepted boundary.
- Choosing item 3 keeps the path conservative and explicit: single-SCC obligation-level recursion only, single-binder-family ownership only, no cross-family SCC linking, non-equi-recursive semantics only, and no cyclic structural graph encoding.

Round-008 scope guard:
- Produce only the `R3` inference-obligation contract artifact for the already-selected subset `URI-R2-C1`.
- Keep the fixed boundary explicit throughout: single-SCC obligation-level recursion only, single-binder-family ownership only, no cross-family SCC linking, non-equi-recursive semantics only, and non-cyclic structural graph only.
- Carry forward the inherited invariant audit and prior `R1`/`R2` artifacts as authoritative inputs; do not reopen completed rounds, rewrite predecessor evidence, or reselect the subset.
- Define the required obligations and fail-closed rejection cases for `URI-R2-C1`, including acyclicity, binding-tree discipline, occurs-check/termination, reconstruction/reification/witness replay, and principality-risk boundaries.
- Do not run the `R4` bounded feasibility decision, do not draft the `R5` implementation-handoff or research-stop artifact, do not update `orchestrator/state.json` or `orchestrator/roadmap.md`, and do not edit production code, tests, `TODO.md`, `CHANGELOG.md`, `implementation_notes.md`, or any task packet outside this round-008 worktree.
