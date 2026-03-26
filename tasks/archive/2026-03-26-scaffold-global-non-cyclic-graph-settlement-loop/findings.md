# Findings

## 2026-03-26

- The repo already has a live repo-local orchestrator control plane under
  `orchestrator/` and repo-local role agents under `.codex/agents/`; this
  request is about scaffolding the next roadmap family, not introducing
  orchestration from scratch.
- `orchestrator/state.json` currently records the just-finished roadmap family
  `2026-03-26-00-same-lane-retained-child-public-output-vs-non-cyclic-graph-successor-orchestrator-roadmap`
  at `stage: "done"` with `last_completed_round: "round-098"`.
- The active roadmap bundle for that family is complete: items `1` through `5`
  are done.
- The accepted exact-pocket result does not globally settle
  `non-cyclic-graph`; it only says this exact same-lane retained-child pocket
  does not force reopening that boundary.
- The earlier strategic docs still classify `non-cyclic-graph` as `unknown`
  at general architecture scope and require broader family-matrix evidence
  before it can be kept or revised globally.
- The repo is in a dirty state unrelated to this new scaffold packet:
  `orchestrator/state.json` plus task-packet files under
  `tasks/todo/2026-03-25-run-successor-orchestrator-loop/` are modified, and
  there are unrelated untracked packet artifacts. These must not be reverted.
- `.worktrees/` is already ignored in `.gitignore`, so no ignore-file change is
  needed for round worktrees.
- The scaffold references expect a fresh roadmap family with a stable
  `YYYY-MM-DD-NN-<slug>` `roadmap_id`, a concrete first item, later coarse
  items, and immutable used revisions.
- The archived 2026-03-26 scaffold packet confirms the project’s recent
  pattern is to mint a new roadmap family for each new bounded successor
  campaign rather than rewriting a completed roadmap family in place.
- The current general strategic record still does not globally settle
  `non-cyclic-graph`:
  [the architectural audit](../../../../docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md)
  keeps `non-cyclic-graph = unknown`.
- The repo-level settlement bar remains the family matrix in
  [the capability contract](../../../../docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md):
  positive families `P1` through `P6` plus negative/bounded families `N1`
  through `N6`.
- The accepted representative campaign and item-7 architecture decision still
  say the current architecture is not yet globally proven sufficient, only
  that `continue within the current architecture` remained the strongest lawful
  read on the then-accepted evidence.
- The user approved a proof-then-build successor shape for this campaign:
  the same roadmap family must first settle `non-cyclic-graph` globally with
  production-surface evidence and then continue directly into implementation,
  hardening, and capability-claim work if item `5` keeps that boundary.
- The approved settlement bar is implement-to-prove rather than docs-only:
  do not call `non-cyclic-graph` globally settled until representative
  `P1` through `P6` behavior plus bounded `N1` / `N2` / `N6` is demonstrated
  on the inherited acyclic model and existing production/output surfaces.
- The new roadmap family is
  `2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap`
  at revision `rev-001`.
- The new family’s control-plane shape is eight items:
  settlement contract freeze,
  propagation/placement proof slices,
  polymorphism/output proof slices,
  representative settlement campaign,
  global `non-cyclic-graph` gate,
  post-settlement implementation,
  hardening,
  and final capability-claim gate.
