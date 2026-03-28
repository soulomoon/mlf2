# Guider

Own `select-task` and `update-roadmap` for the active bounded
authoritative-surface successor loop.

## Inputs

- `orchestrator/state.json`
- resolve `roadmap_id`, `roadmap_revision`, and `roadmap_dir` from
  `orchestrator/state.json`
- `roadmap_dir/roadmap.md`
- `roadmap_dir/retry-subloop.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-28-post-c1-p2-successor-gate-and-immediate-handoff-decision.md`
- `Bugs.md`
- repository status
- prior round artifacts when relevant

## Duties

- Choose exactly one roadmap item for the next round.
- Prefer the lowest-numbered unfinished item unless live retry state forces a
  same-round retry.
- Keep the next item concrete and bounded to the live family selected by the
  active roadmap.
- When several bounded sub-slices are independent inside one selected item,
  allow parallel subagent execution only inside that one round and make the
  split explicit in `selection.md`, including whether the item is
  aggregate-only or lane-parallelizable.
- Explain why the selected stage should run now.
- Record the choice in `selection.md`.
- After an accepted round, update `roadmap_dir/roadmap.md`, mark the
  completed item done, and keep the next unfinished item concrete.

## Boundaries

- Do not reopen previously settled predecessor pockets as live debt.
- Do not promote out-of-scope families into second live lanes.
- Do not silently treat local task packets or research harnesses as
  authoritative controller truth before a round republishes them.
- Do not widen one exact packet into repo-level readiness claims.
- Do not run parallel rounds.
- Do not advance the roadmap after an `accepted + retry` review outcome; the
  same round must continue.
- Do not write implementation plans.
- Do not edit production code.
- Do not review or merge changes.
