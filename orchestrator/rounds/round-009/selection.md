# Round 009 Selection

Selected roadmap item: **4. Execute the bounded feasibility decision for the chosen subset** (`orchestrator/roadmap.md`, item 4).

Why this item should run now:
- `R1` was completed in accepted `round-006`, `R2` was completed in accepted `round-007`, and `R3` was completed in accepted `round-008`, so item 4 is now the next pending dependency in the successor roadmap.
- The chosen bounded subject is already fixed as `URI-R2-C1`, and `R3` already states the exact obligation classes that feasibility must judge, so the smallest research slice that further sharpens the path from bounded `ARI-C1` toward the unannotated target is to make the bounded `R4` decision for that one subset rather than reopen selection or jump ahead to handoff.
- Item 5 depends on an explicit reviewer-visible `feasible-continue` or `not-yet-go` result; without that bounded decision, a handoff artifact would be premature and would risk silently widening beyond the accepted research boundary.
- Selecting item 4 keeps the successor track conservative and fail-closed: feasibility is judged only for unannotated `single-SCC` obligation-level recursion in one `single-binder-family`, with no cross-family SCC linking, no equi-recursive reasoning, and no cyclic structural-graph encoding.

Round-009 scope guard:
- Produce only the `R4` bounded feasibility-decision artifact for the already-selected subset `URI-R2-C1`.
- Keep the fixed boundary explicit throughout: `single-SCC` obligation-level recursion only, `single-binder-family` ownership only, no cross-family SCC linking, non-equi-recursive semantics only, and non-cyclic structural graph only.
- Judge feasibility against the accepted `R3` obligation contract using explicit positive example classes, negative example classes, success evidence, no-go triggers, immediate stop conditions, and one final outcome: `feasible-continue` or `not-yet-go`.
- Keep the work research-first and bounded; any prototype evidence, if used at all, must remain non-default, fail-closed, and strictly inside the `URI-R2-C1` boundary.
- Do not write the `R5` implementation-handoff or research-stop artifact, do not update `orchestrator/state.json` or `orchestrator/roadmap.md`, and do not edit plan/review/merge/implementation-notes artifacts, production code, tests, `TODO.md`, `CHANGELOG.md`, `implementation_notes.md`, or any task packet outside this round-009 worktree.
