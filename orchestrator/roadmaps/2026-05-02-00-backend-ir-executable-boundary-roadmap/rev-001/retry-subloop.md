# Retry Subloop Contract

Roadmap family: `2026-05-02-00-backend-ir-executable-boundary-roadmap`
Revision: `rev-001`

## Scope

- `milestone-1` retries for:
  missing or contradictory one-backend-IR guidance,
  failure to state xMLF versus backend-IR role separation explicitly,
  missing criteria for any future lower IR, or
  accidental introduction of a duplicate public backend IR surface.
- `milestone-2` retries for:
  eager-runtime responsibilities that remain implicit,
  docs/code/tests that disagree about where lowering/runtime ownership lives,
  or any attempt that introduces or normalizes lazy STG machinery.
- `milestone-3` retries for:
  callable-shape ambiguity between direct calls and closure calls,
  missing confused-call diagnostics,
  or evidence that still depends on convention instead of an explicit callable
  contract.
- `milestone-4` retries for:
  semantic ADT/case ownership remaining unclear,
  lowerer-owned layout policy that is still undocumented and untested, or
  representation changes that are treated as semantic backend facts without an
  explicit contract.
- `milestone-5` retries for:
  primitive-operation or eager-evaluation-order assumptions remaining implicit,
  effect-sensitive ordering claims without evidence, or
  unsupported shapes changing behavior without explicit diagnostics.
- `milestone-6` retries for:
  polymorphism lowerability remaining ambiguous,
  unsupported polymorphic executable shapes being accepted silently, or
  disagreement between docs, diagnostics, and tests about what must be erased,
  specialized, or rejected before LLVM emission.
- `milestone-7` retries for:
  mechanism-table rows flipping without evidence,
  stale guidance mismatch across the backend-owned surfaces, or
  closeout claims broader than the accepted backend docs/tests/code actually
  earned.

Review may reject and return the same round to `plan` only when the latest
attempt lands materially new within-scope evidence or materially narrows a
newly exposed blocker while keeping `rev-001` fit for a direct follow-up.
Maximum 3 retry attempts per round before escalation.

## Machine State

`orchestrator/state.json` must carry:

- `roadmap_id`
- `roadmap_revision`
- `roadmap_dir`
- `retry: null` when idle, or a retry object when the same round is looping

## Review Output

Every review must record:

- `Implemented stage result`
- `Attempt verdict`
- `Stage action`
- `Retry reason`
- `Fix hypothesis`

Allowed combinations:

- `accepted + finalize`
- `rejected + retry`
- `rejected + update-roadmap`

Forbidden combinations:

- `rejected + finalize`

## Transition Rules

After review:

- `accepted + finalize`
  - controller clears `retry`
  - controller advances to `merge`
- `rejected + retry`
  - controller records the attempt
  - controller increments `retry.attempt`
  - controller returns the same round to `plan`
- `rejected + update-roadmap`
  - controller records the attempt
  - controller increments `retry.attempt`
  - controller enters `update-roadmap`

Two consecutive no-progress attempts on the same round must change strategy
shape materially or escalate to `update-roadmap`; do not keep replaying the
same failed idea under different wording.

## Boundary Rules

- Keep the fixed mechanism order. The next round anchor is always the first
  `NO` row in
  `docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md`.
- Keep the family serial. Do not run parallel repo-local rounds.
- Do not introduce a second public backend IR, public `LowerableBackend.IR`,
  or equivalent duplicate executable IR surface unless a later accepted
  revision explicitly authorizes it after evidence proves the current boundary
  is insufficient.
- Do not widen into lazy STG machinery such as thunks, update frames, CAF
  update semantics, or graph reduction.
- Keep xMLF as the typed elaboration IR and `MLF.Backend.IR` as the first
  backend-owned executable representation after checked-program acceptance.
- Keep `docs/architecture.md`, backend module notes, the mechanism table, and
  any changed backend-native docs aligned with the accepted contract.
- Keep the completed `rev-027` family immutable predecessor evidence; do not
  treat it as live debt inside this family.
- Do not treat a docs-only claim as mechanism closure when the active
  milestone requires code/test evidence or explicit diagnostic coverage.
- If a round proves the family needs a broader public backend boundary than
  `rev-001` allows, reject with `update-roadmap` and publish a same-family
  successor revision instead of silently widening inside the same round.

## Escalation

After 3 consecutive rejected attempts on the same round:

- controller records the failure summary in `resume_error`
- controller must automatically enter the same-round recovery ladder and, when
  necessary, `update-roadmap`
- same-mechanism retry exhaustion is escalation, not terminal stop
