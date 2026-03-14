# Round 008 Merge Preparation

## Proposed squash commit title

`Docs: add R3 inference-obligation contract for unannotated iso-recursive roadmap (round 008)`

## Concise merge summary

- Adds the roadmap-item-3 docs-only contract at `docs/plans/2026-03-14-unannotated-iso-recursive-r3-inference-obligation-contract.md`.
- Defines the explicit `R3` obligation matrix for the already-selected subset `URI-R2-C1`, covering structural acyclicity, binder ownership and scope discipline, occurs-check and termination, reconstruction/reification/witness replay, and principality-risk boundaries.
- Preserves the fixed boundary model by keeping the work `single-SCC`, `single-binder-family`, obligation-level only, non-equi-recursive, non-cyclic on the structural graph, and fail-closed on widening-dependent or ambiguous cases.
- Records docs-only execution and staging continuity in `orchestrator/rounds/round-008/implementation-notes.md`, while the approved review confirms the round stopped at `R3` and left production/test surfaces untouched.

## Follow-up notes

- Successor work should use this `R3` contract as the sole input to the bounded `R4` feasibility decision for `URI-R2-C1`; it should not reopen `R1`, `R2`, or the inherited invariant audit.
- Any later `R4` evidence must remain within the same fixed boundary and fail closed on cross-family SCC linking, implicit unfolding, cyclic structural encoding, provenance-unstable replay, or any widening-dependent admission path.

## Takeover continuity note

- Continuity is preserved by treating completed rounds `001` through `007`, the predecessor recursive-types packet, the inherited invariant audit, and the successor roadmap-design spec as reference-only authoritative evidence.
- The approved review record confirms those inherited sources stayed immutable, `URI-R2-C1` remained the fixed chosen subset, and no prior round artifact, task packet, control-plane state, or roadmap state was rewritten.

## Readiness statement

Round 008 is ready for squash merge preparation: the review decision is approved, the delivered artifact satisfies the `R3` obligation-contract gate for `URI-R2-C1`, continuity references align with the accepted review record, and the round stayed docs-only with no forbidden file-surface changes.
