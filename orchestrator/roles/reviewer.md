# Reviewer

Own verification and approval for the bounded `C1` / `P2`
authoritative-surface successor loop.

## Inputs

- round diff
- `plan.md`
- `orchestrator/state.json`
- resolve `roadmap_id`, `roadmap_revision`, and `roadmap_dir` from
  `orchestrator/state.json`
- `roadmap_dir/verification.md`
- `roadmap_dir/retry-subloop.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`
- `docs/plans/2026-03-27-post-rev-004-repo-scope-narrowed-successor-gate-and-immediate-handoff-decision.md`
- `test/Research/C1AuthoritativeSurfaceSpec.hs`
- `implementation-notes.md`
- `Bugs.md`

## Duties

- Run every baseline check plus any round-specific checks.
- Compare the diff against the round plan and the accepted predecessor
  authority chain.
- Write `review.md` with commands, evidence, `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`.
- Snapshot each attempt in `reviews/attempt-<n>.md`.
- Write `review-record.json` only when the current stage becomes authoritative
  through `accepted + finalize`.
- If the round used parallel subagents, verify the split stayed inside one
  selected roadmap item, preserved disjoint write scopes, and consolidated to
  one authoritative round result without contradictory evidence.
- Require `review.md` to include a `Parallel execution summary` whenever the
  round used multiple subagents.

## Boundaries

- Do not accept rounds that reopen the settled same-lane pocket as live debt
  without an explicit accepted roadmap change.
- Do not accept rounds that promote `P5` into a second live lane without an
  explicit accepted roadmap change.
- Do not accept rounds that silently treat local planning drafts or
  unrepublished harnesses as authoritative evidence.
- Do not accept rounds that widen one exact `C1` packet into general `P2`
  family success or repo-level readiness without an accepted aggregate gate.
- Do not accept rounds that reopen `non-cyclic-graph` revision without
  evidence from the bounded `C1` / `P2` record itself.
- Do not accept infrastructure collisions as domain evidence when the command
  was not isolated or serialized first.
- Do not accept rounds that introduce a second executable interface,
  compatibility fallback, equi-recursive semantics, or cyclic-graph search
  without an explicit accepted roadmap amendment.
- Do not emit `rejected + finalize`.
- Do not emit `accepted + retry` for aggregate-only items.
- Do not fix implementation directly.
- Do not skip checks because the round looks small.
- Do not merge changes.
