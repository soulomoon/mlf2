# Findings

## 2026-03-26

- `orchestrator/state.json` records `contract_version: 2`,
  `active_round_id: null`, `stage: "done"`, `last_completed_round:
  "round-098"`, `retry: null`, and roadmap locator
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
  / `rev-001`.
- The resolved live roadmap bundle at
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001`
  still has items `1` through `8` marked `[pending]`, so the current
  `stage: "done"` state is stale and non-terminal under the shared resume
  rules.
- Repo-local role definitions exist in `.codex/agents/` for guider, planner,
  implementer, reviewer, and merger, so the controller should use those
  sources rather than `orchestrator/roles/*.md`.
- The active retry contract allows same-round retry for roadmap items `1`
  through `4`, `6`, and `7`; item `5` and item `8` are aggregate-only and
  may not use `accepted + retry`.
- The repository is currently on branch
  `codex/automatic-recursive-type-inference` and is clean aside from the new
  task-packet files created for this controller run.
- `round-099` is now open on branch `codex/round-099` with dedicated worktree
  `.worktrees/round-099`, and both controller-visible `orchestrator/state.json`
  copies are parked at `stage: "select-task"` for that round.
- The first required built-in guider delegation for `round-099`
  `select-task` returned unusable output and did not write
  `orchestrator/rounds/round-099/selection.md` after repeated controller
  waits.
- No qualifying recovery-investigator could be launched through the same
  built-in subagent mechanism in this packet, so the controller recorded a
  precise `resume_error` blockage instead of simulating `select-task`.
- A later exact-stage resume succeeded: the guider wrote
  `.worktrees/round-099/orchestrator/rounds/round-099/selection.md`,
  selecting roadmap item `1` as the lawful next step for the global
  settlement roadmap.
- After controller-visible `selection.md` evidence appeared, both
  `orchestrator/state.json` copies were advanced from `select-task` to
  `plan`, `current_task` was set to `item-1`, and `resume_error` was
  cleared.
- The planner wrote `round-099/plan.md`, keeping the round bounded to a
  docs-only item-1 freeze of the global settlement contract and unresolved
  family ledger.
- The implementer wrote the canonical item-1 docs artifact plus
  `round-099/implementation-notes.md` without touching controller state,
  production code, or roadmap contracts.
- The reviewer finalized `round-099` `item-1` `attempt-1` as
  `accepted + finalize`, and the merger authored `round-099/merge.md`
  confirming the round is ready for squash merge with title
  `Freeze global non-cyclic-graph settlement contract and evidence ledger`.
- The controller committed the approved round payload on `codex/round-099`
  as `486a411` and cherry-picked that payload onto
  `codex/automatic-recursive-type-inference` as `60561e4`.
- After the squash merge bookkeeping, both controller-visible
  `orchestrator/state.json` copies were advanced to `stage: "update-roadmap"`
  with `active_round_id: null` and `last_completed_round: "round-099"`.
- The base-branch workspace currently shows a user-owned modification to
  `orchestrator/state.json` plus untracked task packets; those changes are
  unrelated to the guider-owned roadmap update and must be preserved.
- The authoritative active roadmap bundle is
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001`.
- `orchestrator/rounds/round-099/review-record.json` finalizes `item-1`
  as `accepted + finalize` for the same roadmap identity, so the guider-owned
  `update-roadmap` stage may now mark roadmap item `1` done.
- The current `rev-001` roadmap still describes item `2` as
  "Implement the minimum production-surface acyclic proof slices...", which
  is too wide for the pre-item-5 settlement phase under the guider contract;
  the next unfinished item must stay bounded to settlement evidence rather
  than broad implementation.
- The guider-owned roadmap update was completed in place in
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/roadmap.md`:
  item `1` is now `[done]`, and item `2` is retuned to a bounded
  settlement-evidence slice for frozen cases `C1`, `C2`, and `C5`.
- The updated item `2` now requires honest case classification for `P2`,
  `P3`, and `P4` on existing solver -> elaboration -> reconstruction ->
  internal/public output surfaces, and it explicitly forbids repo-level
  settlement claims or broad production implementation before item `5`.
- `round-100` is now open on branch `codex/round-100` with dedicated worktree
  `.worktrees/round-100`, and the guider selected roadmap item `2` as the
  next lawful step.
- The current round-100 scope is bounded to the settlement-evidence slice for
  frozen cases `C1`, `C2`, and `C5`, with honest classification against the
  frozen ledger for `P2`, `P3`, and `P4`.
- The planner wrote `round-100/plan.md`, fixing item `2` as a docs-first,
  evidence-first round that creates one canonical settlement-evidence
  artifact plus round-local `implementation-notes.md` and preserves the
  current blocker-debt classification unless fresh read-only evidence
  honestly contradicts it.
- After controller-visible `plan.md` evidence appeared, both
  `orchestrator/state.json` copies were advanced from `plan` to
  `implement` for `round-100` `item-2`.
- The implementer wrote the canonical item-2 settlement-evidence dossier at
  `docs/plans/2026-03-26-global-non-cyclic-graph-c1-c2-c5-production-surface-settlement-evidence-slice.md`
  plus `round-100/implementation-notes.md`, keeping `C1`, `C2`, and `C5`
  classified as `admitted but not reconstruction-visible / blocker debt`
  and leaving code, tests, `Bugs.md`, and controller state untouched.
- After controller-visible implementation artifacts appeared, both
  `orchestrator/state.json` copies were advanced from `implement` to
  `review` for `round-100` `item-2`.
- The reviewer finalized `round-100` `item-2` `attempt-1` as
  `accepted + finalize`, and the authoritative `review-record.json`
  fixes the accepted outcome to a bounded settlement slice that keeps
  `C1`, `C2`, and `C5` as blocker-debt rows for `P2`, `P3`, and `P4`.
- After controller-visible review evidence appeared, both
  `orchestrator/state.json` copies were advanced from `review` to
  `merge` for `round-100` `item-2`.
- The merger wrote `round-100/merge.md`, confirming the latest reviewer
  snapshot is lawful `accepted + finalize` and the approved payload is ready
  for squash merge.
- The controller committed the approved round payload on `codex/round-100`
  as `e4a9d26` and cherry-picked that payload onto
  `codex/automatic-recursive-type-inference` as `257eaaa`.
- After the squash-merge bookkeeping, both controller-visible
  `orchestrator/state.json` copies were advanced to
  `stage: "update-roadmap"` with `active_round_id: null` and
  `last_completed_round: "round-100"`.
- The guider-owned roadmap update was completed in place in
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/roadmap.md`:
  item `2` is now `[done]`, and item `3` is retuned to a bounded
  settlement-evidence slice for `C3` nested-`forall` pressure and `C7`
  output-surface continuity.
- The updated item `3` now requires honest reclassification of the remaining
  unresolved `P5` / `P6` pressure rows on the existing
  solver -> elaboration -> reconstruction -> internal/public output
  surfaces, and it explicitly forbids representative-campaign, item-5
  settlement, or production-implementation widening.
- The roadmap-only `update-roadmap` change for accepted item `2` was
  committed on the base branch as `d98d268`.
- `round-101` is now open on branch `codex/round-101` with dedicated
  worktree `.worktrees/round-101`, and both controller-visible
  `orchestrator/state.json` copies are parked at `stage: "select-task"` for
  the next lawful round.
- The guider selected roadmap item `3` for `round-101`, bounding the round
  to the `C3` nested-`forall` pressure and `C7` output-surface continuity
  settlement-evidence slice for the remaining unresolved `P5` / `P6`
  pressure rows.
- After controller-visible `selection.md` evidence appeared, both
  `orchestrator/state.json` copies were advanced from `select-task` to
  `plan`, and `current_task` was set to `item-3`.
- The planner wrote `round-101/plan.md`, fixing item `3` as a docs-first,
  evidence-first round that creates one canonical settlement-evidence
  artifact plus round-local `implementation-notes.md` and preserves the
  accepted `C3 = fail-closed rejection` / `C7 = blocker debt` read unless
  fresh read-only evidence honestly contradicts it.
- After controller-visible `plan.md` evidence appeared, both
  `orchestrator/state.json` copies were advanced from `plan` to
  `implement` for `round-101` `item-3`.
- The implementer wrote the canonical item-3 settlement-evidence dossier at
  `docs/plans/2026-03-26-global-non-cyclic-graph-c3-c7-production-surface-settlement-evidence-slice.md`
  plus `round-101/implementation-notes.md`, keeping `C3` classified as
  `fail-closed rejection` and `C7` classified as
  `admitted but not reconstruction-visible / blocker debt` while leaving
  code, tests, `Bugs.md`, and controller state untouched.
- After controller-visible implementation artifacts appeared, both
  `orchestrator/state.json` copies were advanced from `implement` to
  `review` for `round-101` `item-3`.
- The reviewer finalized `round-101` `item-3` `attempt-1` as
  `accepted + finalize`, and the authoritative `review-record.json`
  fixes the accepted outcome to a bounded settlement slice that keeps
  `C3` reject-side and `C7` blocker-debt on the current production surfaces.
- After controller-visible review evidence appeared, both
  `orchestrator/state.json` copies were advanced from `review` to
  `merge` for `round-101` `item-3`.
- The merger wrote `round-101/merge.md`, confirming the latest reviewer
  snapshot is lawful `accepted + finalize` and the approved payload is ready
  for squash merge.
- The controller committed the approved round payload on `codex/round-101`
  as `8f13efc` and cherry-picked that payload onto
  `codex/automatic-recursive-type-inference` as `220ee31`.
- After the squash-merge bookkeeping, both controller-visible
  `orchestrator/state.json` copies were advanced to
  `stage: "update-roadmap"` with `active_round_id: null` and
  `last_completed_round: "round-101"`.
- The guider-owned roadmap update was completed in place in
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/roadmap.md`:
  item `3` is now `[done]`, and item `4` is retuned to a bounded
  representative replay grounded in the accepted item-2 / item-3 row reads.
- The updated item `4` keeps the representative replay honest about the
  frozen pre-campaign truths: `P5` remains reject-side only through `C3`,
  `P6` remains bounded blocker debt through `C7`, and the matrix still has
  zero `stable visible persistence` rows until new replay evidence proves
  otherwise.
- The roadmap-only `update-roadmap` change for accepted item `3` was
  committed on the base branch as `d8e6063`.
- `round-102` is now open on branch `codex/round-102` with dedicated
  worktree `.worktrees/round-102`, and both controller-visible
  `orchestrator/state.json` copies are parked at `stage: "select-task"` for
  the next lawful round.
- The guider selected roadmap item `4` for `round-102`, bounding the round
  to the representative end-to-end settlement campaign across the family
  matrix before the item-5 settlement gate.
- After controller-visible `selection.md` evidence appeared, both
  `orchestrator/state.json` copies were advanced from `select-task` to
  `plan`, and `current_task` was set to `item-4`.
- The planner wrote `round-102/plan.md`, fixing item `4` as one bounded
  representative family-matrix replay grounded in the accepted item-2 /
  item-3 settlement slices and explicit `N4` pressure accounting.
- After controller-visible `plan.md` evidence appeared, both
  `orchestrator/state.json` copies were advanced from `plan` to
  `implement` for `round-102` `item-4`.
- The implementer wrote the canonical item-4 representative settlement
  dossier at
  `docs/plans/2026-03-26-global-non-cyclic-graph-representative-family-matrix-end-to-end-settlement-campaign.md`
  plus `round-102/implementation-notes.md`, preserving zero
  `stable visible persistence` rows, five blocker-debt rows, three
  fail-closed rows, and `N4` as boundary pressure only.
- After controller-visible implementation artifacts appeared, both
  `orchestrator/state.json` copies were advanced from `implement` to
  `review` for `round-102` `item-4`.
- The reviewer finalized `round-102` `item-4` `attempt-1` as
  `accepted + finalize`, and the authoritative `review-record.json`
  fixes the accepted outcome to a bounded representative replay that keeps
  the matrix at `bounded subset only` with zero
  `stable visible persistence` rows.
- After controller-visible review evidence appeared, both
  `orchestrator/state.json` copies were advanced from `review` to
  `merge` for `round-102` `item-4`.
- The merger wrote `round-102/merge.md`, confirming the latest reviewer
  snapshot is lawful `accepted + finalize` and the approved payload is ready
  for squash merge.
- The controller committed the approved round payload on `codex/round-102`
  as `9f10bac` and cherry-picked that payload onto
  `codex/automatic-recursive-type-inference` as `5a9efb5`.
- After the squash-merge bookkeeping, both controller-visible
  `orchestrator/state.json` copies were advanced to
  `stage: "update-roadmap"` with `active_round_id: null` and
  `last_completed_round: "round-102"`.
- The guider-owned roadmap update was completed in place in
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-001/roadmap.md`:
  item `4` is now `[done]`, and item `5` is retuned to a bounded
  aggregate-only settlement gate over accepted item-1 through item-4
  evidence.
- The updated item `5` now fixes the aggregate inputs explicitly:
  zero `stable visible persistence` rows, five blocker-debt rows, three
  fail-closed rows, `P5` reject-side only, `P6` below visible persistence,
  and `N4` still pressure context only unless the gate itself concludes
  otherwise.
- The roadmap-only `update-roadmap` change for accepted item `4` was
  committed on the base branch as `f5852c8`.
- `round-103` is now open on branch `codex/round-103` with dedicated
  worktree `.worktrees/round-103`, and both controller-visible
  `orchestrator/state.json` copies are parked at `stage: "select-task"` for
  the next lawful round.
- The guider selected roadmap item `5` for `round-103`, bounding the round
  to the aggregate `non-cyclic-graph` settlement gate over accepted item-1
  through item-4 evidence.
- After controller-visible `selection.md` evidence appeared, both
  `orchestrator/state.json` copies were advanced from `select-task` to
  `plan`, and `current_task` was set to `item-5`.
- The planner wrote `round-103/plan.md`, fixing item `5` as an
  aggregate-only decision round that creates exactly one canonical decision
  artifact at
  `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
  and forbids new reruns, implementation notes, or production edits.
- After controller-visible `plan.md` evidence appeared, both
  `orchestrator/state.json` copies were advanced from `plan` to
  `implement` for `round-103` `item-5`.
- The implementer wrote the canonical item-5 decision artifact at
  `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
  and selected the only lawful aggregate outcome supported by the accepted
  item-1 through item-4 evidence chain:
  `reopen the non-cyclic-graph revision question`, leaving item `6`
  blocked pending a same-family roadmap revision.
- After controller-visible implementation evidence appeared, both
  `orchestrator/state.json` copies were advanced from `implement` to
  `review` for `round-103` `item-5`.
- The reviewer finalized `round-103` `item-5` `attempt-1` as
  `accepted + finalize`, and the authoritative `review-record.json`
  fixes the accepted aggregate outcome to
  `reopen the non-cyclic-graph revision question`.
- After controller-visible review evidence appeared, both
  `orchestrator/state.json` copies were advanced from `review` to
  `merge` for `round-103` `item-5`.
- The merger wrote `round-103/merge.md`, confirming the latest reviewer
  snapshot is lawful `accepted + finalize` and the approved payload is ready
  for squash merge.
- The controller committed the approved round payload on `codex/round-103`
  as `1235828` and cherry-picked that payload onto
  `codex/automatic-recursive-type-inference` as `9e62c66`.
- After the squash-merge bookkeeping, both controller-visible
  `orchestrator/state.json` copies were advanced to
  `stage: "update-roadmap"` with `active_round_id: null` and
  `last_completed_round: "round-103"`.
- The guider-owned roadmap update after accepted item `5` published a
  successor revision instead of continuing rev-001 directly into item `6`:
  rev-001 now records item `5` as done and keeps items `6` through `8`
  blocked, while new revision `rev-002` carries the reopened same-family
  planning lane.
- The published `rev-002` bundle narrows the next lawful work to
  planning-only reopened-architecture questions. Its item `1` is a docs-only
  freeze of the reopened `non-cyclic-graph` revision authority and candidate
  boundary, and the new bundle includes matching `retry-subloop.md` and
  `verification.md`.
- After the guider published `rev-002`, both controller-visible
  `orchestrator/state.json` copies were updated to point at the new
  authoritative bundle through
  `roadmap_revision: "rev-002"` and the matching `roadmap_dir`.
- `round-104` is now open on branch `codex/round-104` with dedicated
  worktree `.worktrees/round-104`, and both controller-visible
  `orchestrator/state.json` copies are parked at `stage: "select-task"` on
  the new `rev-002` bundle.
- The guider selected rev-002 roadmap item `1` for `round-104`, bounding the
  round to a docs-only freeze of the reopened `non-cyclic-graph` revision
  authority and candidate boundary.
- After controller-visible `selection.md` evidence appeared, both
  `orchestrator/state.json` copies were advanced from `select-task` to
  `plan`, and `current_task` was set to `item-1` on `rev-002`.
