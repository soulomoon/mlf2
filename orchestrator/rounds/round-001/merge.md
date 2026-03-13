# Round 001 Merge Preparation

## Proposed squash commit title

`Docs: define automatic recursive-type inference baseline contract (round 001)`

## Concise merge summary

- Adds the roadmap-item-1 docs-only contract at `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`.
- Captures inherited baseline truth: explicit-only acyclic `TyMu` support is complete, while automatic recursive-type inference remains unresolved/disabled.
- Locks non-goals and evidence-before-spike gates so no code-changing recursive-inference work proceeds without the required audit/selection/approval artifacts.
- Keeps round scope docs-only with no production-code edits.

## Takeover continuity and predecessor-history note

- Continuity is preserved by referencing predecessor evidence sources (`tasks/todo/2026-03-11-recursive-types-orchestration/` and the cited 2026-03-11/13 plan docs) without rewriting predecessor authoritative history.
- Review record for this round explicitly confirms predecessor history was referenced, not modified.

## Follow-up notes

- Next round should execute roadmap item 2 (invariant audit) and keep the same explicit-only / non-equi-recursive / non-cyclic boundaries as hard constraints.
- Any proposal for a code-changing spike remains blocked until the acceptance-contract prerequisites are fully satisfied and reviewer-approved.

## Readiness statement

Round 001 is ready for squash merge from a merger-preparation standpoint: review is approved, scope boundaries were respected, continuity references are consistent with predecessor history, and no implementation files were changed.
