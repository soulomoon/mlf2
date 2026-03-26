# Round 111 Selection

Date: 2026-03-26
Round: `round-111`
Role: guider
Active subject: rev-003 exact-pocket validation for the bounded same-family
authoritative-handoff amendment on the selected same-lane `C2` / `C5` /
`C7` pocket

## Roadmap Provenance

- Roadmap ID:
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
- Roadmap Revision: `rev-003`
- Roadmap Dir:
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003`
- Selection-time controller state: `orchestrator/state.json`
- Selection-time repository status in the dedicated round worktree:
  clean at branch creation time before the new round-owned packet

## Selected Roadmap Item

Roadmap item `3`: validate the bounded amendment on the frozen same-pocket
evidence surface.

## Why This Item Should Run Now

Accepted `round-109` finalized rev-003 item `1` and froze the exact
same-pocket contract, writable slice, and read-only anchors. Accepted
`round-110` then finalized rev-003 item `2` and landed the bounded
authoritative-handoff amendment inside that exact writable slice. With items
`1` and `2` now complete, item `3` is the next lawful move.

The active roadmap bundle remains `rev-003`. Its item list now records items
`1` and `2` as `done`, item `3` as the lowest-numbered unfinished item with
all dependencies satisfied, and item `4` as dependent on items `1`, `2`, and
`3`. The next lawful move is therefore the bounded validation round, not a
second implementation round and not the post-amendment handoff decision.

Accepted `round-110` is the decisive implementation handoff. It changed only
`src/MLF/Elab/Run/Pipeline.hs`,
`src/MLF/Elab/TermClosure.hs`, and
`test/PipelineSpec.hs`
to carry one bounded recursive component through the exact same-pocket
authoritative public-output path. Item `3` exists to rerun only the exact
frozen evidence surface and record whether that bounded implementation:

- preserves helper-visible/internal continuity;
- changes authoritative public output on the same one public-entrypoint
  chain as intended;
- preserves the same clear-boundary and fail-closed guards; and
- avoids subject drift or broader family claims.

## Round Scope Guard

- This round is limited to roadmap item `3` only.
- The output must remain exact-pocket-only.
- The validation must rerun only the exact rows `C2`, `C5`, and `C7`.
- The validation must rerun only the same exact packet and exact command set
  frozen by accepted rev-002 item `4`.
- The validation must stay on the exact six review-visible surfaces already
  frozen by accepted rev-002 item `4`.
- The validation must preserve `iso-recursive = keep`,
  `non-equi-recursive = keep`, and `no-fallback = keep`.
- The validation must not widen into a second packet, second interface,
  multi-SCC search, fallback widening, hardening, rollout, or broad
  capability narration.

## Blockers

No live retry obligation is present.

Active bounded blockers that remain in play for item `3`:

- the exact-pocket validation must prove or honestly fail-close whether the
  new bounded public-output change stays on the same selected subject;
- the validation must keep the read-only audit anchors read-only;
- the validation must avoid narrating repo-level success from one exact
  pocket; and
- the post-amendment follow-on decision remains deferred to item `4`.
