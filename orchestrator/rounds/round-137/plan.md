# Round 137 Plan

- Roadmap item: `item-3`
- Retry state: active after `attempt-1` rejection (`stage_action: retry`)
- Execution shape: one serial aggregate-only docs-only retry lane; no parallel sidecars authorized
- Retry slice: republish the already-authored exact-packet settlement under the exact docs path frozen by this round and keep controller-owned machine state out of implementer scope
- Live subject: exact packet `sameLaneAliasFrameClearBoundaryExpr` only

## Retry-only authority inputs

- Existing authored settlement content source:
  `docs/plans/2026-03-28-post-item-2-same-lane-representative-gap-settlement-surface-and-exact-repo-impact-read.md`
- Item-1 freeze authority:
  `docs/plans/2026-03-28-same-lane-retained-child-representative-gap-successor-authority-exact-subject-success-bar-and-writable-slice-freeze.md`
- Accepted item-2 evidence only:
  `orchestrator/rounds/round-136/selection.md`
  `orchestrator/rounds/round-136/implementation-notes.md`
  `orchestrator/rounds/round-136/review.md`
  `orchestrator/rounds/round-136/merge.md`

Do not add new evidence sources, reinterpret the blocker, or widen beyond the
accepted round-136 packet read.

## Retry-only write scope

Write only:

- `docs/plans/2026-03-29-same-lane-alias-frame-representative-gap-post-item-2-settlement-surface-and-repo-impact-read.md`
- `orchestrator/rounds/round-137/implementation-notes.md`

The retry must not leave a round-authored settlement artifact at:

- `docs/plans/2026-03-28-post-item-2-same-lane-representative-gap-settlement-surface-and-exact-repo-impact-read.md`

Do not touch `orchestrator/state.json`, any roadmap file, `src/`, `src-public/`,
`app/`, `test/`, `mlf2.cabal`, `Bugs.md`, or any other artifact. Treat
controller-owned machine state as out of implementer scope and not part of the
retry evidence or notes.

## Sequential delta plan

1. Republish the same narrow settlement content at the frozen March 29 docs
   path.
   Use the already-authored March 28 settlement artifact only as source text.
   Preserve the exact packet name, the `narrower current-architecture blocker`
   classification, the exact `runPipelineElab` / `runPipelineElabChecked`
   surfaces, the exact `PhiTranslatabilityError` blocker text, the accepted
   provenance chain, the exact repo-impact read, and the non-widening boundary.

2. Collapse the settlement output to the frozen canonical path only.
   Remove or rename the off-scope March 28 settlement artifact so the round
   diff no longer contains a settlement publication outside the plan-frozen
   write scope. Do not create aliases, shadow copies, redirect docs, or extra
   summary artifacts.

3. Refresh `orchestrator/rounds/round-137/implementation-notes.md` as a retry
   note only.
   Record that the retry republishes accepted item-1 and round-136 authority
   without changing substance, and point only to the frozen March 29 artifact.
   Do not mention, diff, justify, or edit controller-owned machine state.

4. Close with docs-only retry verification.
   Confirm the frozen March 29 artifact exists, the off-scope March 28
   settlement artifact is absent from round-owned output, and the republished
   content remains exact-packet-only and non-widening. Record this as a docs-
   only retry; do not run code/test work or use machine-state movement as
   evidence.
