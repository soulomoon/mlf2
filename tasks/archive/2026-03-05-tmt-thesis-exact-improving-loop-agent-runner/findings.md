# Findings: 2026-03-05 TMT Thesis-Exact Improving Loop Agent Runner

## Baseline
- Located transformation table at `docs/notes/2026-02-27-transformation-mechanism-table.md`.
- Baseline row statuses before Round 1 sweep:
  - Row 1 `Elaboration input`: Yes
  - Row 2 `Result-type context wiring`: Yes
  - Row 3 `Ordering of transformations`: No

## Round 1 planner sweep (agent team)
- Ordered gate decisions:
  1. Elaboration input -> YES
  2. Result-type context wiring -> YES
  3. Ordering of transformations -> NO
- `target_mechanism`: `Ordering of transformations`

## Round 1 planner phase (agent team)
- Plan focused on row3 strict gap:
  - remove flush-all-owner fallback behavior,
  - make pending-weaken owner provenance stable,
  - preserve fail-fast boundary/finalization invariants,
  - strengthen row3 guards and run required gates.
- Implementer prompt generated for attempt 1.

## Round 1 implementation findings
- Removing flush-all boundary fallback alone exposed a latent owner-bucket leak in `Phase 4 thesis-exact unification closure`.
- Root cause: pending weaken ownership was inferred at flush-time from mutable graph state.
- Resolution integrated in same attempt:
  - owner-stamp pending weakens at enqueue time in `EdgeUnify`,
  - keep strict boundary scheduling (`closed owner` flush + post-boundary fail-fast),
  - keep improved diagnostics in `EdgeProcessing`/`Driver`.
- Required matcher/full gates all passed after integration.

## Round 2 planner sweep (agent team)
- Ordered gate decisions:
  1. Elaboration input -> YES
  2. Result-type context wiring -> YES
  3. Ordering of transformations -> YES
  4. Per-edge propagation transform -> NO
- `target_mechanism`: `Per-edge propagation transform`

## Round 2 implementation findings
- Attempt 1 removed synthesized-wrapper branching from Interpreter and added row4 guard/tests.
- Focused row4 gates passed, but full gate failed with Phase 6 `PhiTranslatabilityError` (`OpRaise (non-spine): missing computation context`) on `\y. let id = (\x. x) in id y`.
- Attempt 2 restored synthesized-wrapper-safe behavior to recover regressions while keeping `solveNonExpInstantiation` absent; full gate passed but objective remained unmet because synthesized-wrapper special handling still exists.
- Conclusion: current implementation method for row4 does not converge to thesis-exact objective under required regression constraints.
