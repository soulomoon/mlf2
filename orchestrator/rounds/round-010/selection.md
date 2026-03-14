# Round 010 Selection

Selected roadmap item: **5. Write the final implementation-handoff spec or explicit research-stop decision** (`orchestrator/roadmap.md`, item 5).

Why this item should run now:
- Item 4 was completed in accepted `round-009`, and item 5 is now the only pending roadmap item in the successor track.
- `round-009` recorded the bounded `R4` outcome `not-yet-go` for `URI-R2-C1`, so the smallest research slice that sharpens the path from bounded `ARI-C1` toward the unannotated target is not a new feasibility pass or a widened redesign; it is to write the explicit item-5 research-stop decision that captures why the track stops under the current evidence.
- The accepted `R4` artifact already identifies the decisive unresolved blockers for positive unannotated admission inside the fixed boundary, especially missing docs-only clearance for provenance-stable unannotated replay authority, unique unannotated root/cluster selection, and prototype-free satisfaction of the inherited `URI-R3-O1` through `URI-R3-O5` obligations.
- Choosing item 5 now keeps the track fail-closed and reviewer-visible: the round should convert the `not-yet-go` result into an explicit bounded research-stop decision for unannotated `single-SCC`, `single-binder-family` iso-recursive inference rather than pretending the roadmap has implementation clearance.

Round-010 scope guard:
- Produce only the item-5 decision artifact, and take the **explicit research-stop / not-yet-go branch**, not the implementation-handoff branch, unless new contradictory inherited evidence is discovered within the same fixed boundary.
- Keep the fixed boundary explicit throughout: `single-SCC` obligation-level recursion only, `single-binder-family` ownership only, no cross-family SCC linking, non-equi-recursive semantics only, and non-cyclic structural graph only.
- Tie the stop decision directly to the accepted `round-009` `R4` evidence for `URI-R2-C1`; do not reopen `R1`, reselect `R2`, rewrite the `R3` obligation contract, or rerun `R4`.
- Keep the round research-first and docs-only; do not convert the `not-yet-go` record into implementation clearance, and do not widen toward multi-SCC, multi-family, equi-recursive, implicit-unfolding, or cyclic-graph designs.
- Do not update `orchestrator/state.json` or `orchestrator/roadmap.md`, do not write `plan.md`, `review.md`, `merge.md`, or `implementation-notes.md`, and do not edit production code, tests, `TODO.md`, `CHANGELOG.md`, `implementation_notes.md`, or any task packet outside this round-010 worktree.
