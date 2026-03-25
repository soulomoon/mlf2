# Round 024 Plan (`R1` Repair-Boundary Reproduction Contract)

## Objective

Execute only roadmap item `R1` for the fixed `URI-R2-C1` / `uri-r2-c1-only-v1` lane and produce one reviewer-auditable implementation-facing reproduction contract for `BUG-2026-03-16-001`.

This round must restate the already accepted replay failure as a bounded production-owner target at `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch), without implementing the repair, widening replay scope, adding a second executable interface, or introducing fallback behavior.

## Locked Round Context

- Round id: `round-024`
- Stage: `plan` for roadmap item `R1`
- Active attempt: `attempt-1` (fresh attempt; `retry: null`)
- Scenario boundary: `uri-r2-c1-only-v1`
- Subject boundary: `URI-R2-C1`
- Repair boundary: `witness-replay/applyInstantiation-instbot-precondition`
- Owner boundary: `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
- Inherited authoritative audit that must remain unchanged:
  - `P1 = pass`
  - `P2 = semantic-negative`
  - `D1 = pass`
  - `D2 = pass`
  - `D3 = pass`
  - `D4 = reopen-repair-track`

## Authoritative Inputs To Preserve

- `orchestrator/rounds/round-024/selection.md`
- `docs/superpowers/specs/2026-03-17-uri-r2-c1-p2-replay-repair-roadmap-design.md`
- `docs/plans/2026-03-16-uri-r2-c1-d3-bounded-fixability-probe.md`
- `docs/plans/2026-03-16-uri-r2-c1-d4-repair-track-decision-gate.md`
- `Bugs.md` (`BUG-2026-03-16-001`)
- Predecessor evidence under `orchestrator/rounds/round-020/` through `orchestrator/rounds/round-023/`

## Scope

1. Reproduce the accepted mismatch in implementation-facing terms for the locked lane only.
2. Bind that reproduction to the existing production owner boundary in `src/MLF/Elab/Inst.hs`.
3. Record the canonical `R1` artifact at:
   - `docs/plans/2026-03-17-uri-r2-c1-r1-repair-boundary-reproduction.md`
4. Add only the smallest focused harness/tests or evidence needed to make the localized owner-boundary failure reviewer-auditable.
5. Preserve the roadmap order: this round prepares `R2`; it does not implement `R2`, run `R3`, or decide `R4`.

## Files Expected In Scope

Primary working set:

1. `src/MLF/Elab/Inst.hs`
2. `test/ElaborationSpec.hs`
3. `docs/plans/2026-03-17-uri-r2-c1-r1-repair-boundary-reproduction.md`
4. `orchestrator/rounds/round-024/implementation-notes.md` if a bounded reviewer handoff note is needed

Historical evidence and controller-owned files must stay untouched:

- `orchestrator/rounds/round-024/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-001/roadmap.md`
- all prior round artifacts under `orchestrator/rounds/round-020/` through `orchestrator/rounds/round-023/`

## Sequential Tasks

### Task 1 - Reconfirm the bounded repair contract before touching code-facing artifacts

- Re-read the accepted `D2`, `D3`, and `D4` outputs and restate the non-negotiable constraints inside the `R1` artifact:
  - fixed subject `URI-R2-C1`
  - fixed scenario `uri-r2-c1-only-v1`
  - fixed divergence boundary `witness-replay/applyInstantiation-instbot-precondition`
  - fixed owner `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch)
  - no second executable interface, no compatibility fallback, no broad replay rewrite

### Task 2 - Build one implementation-facing reproducer at the accepted owner boundary

- Identify the smallest direct call path that exposes the localized mismatch through existing production code, centered on `applyInstantiation`.
- Reuse the accepted shape facts from predecessor evidence:
  - no-fallback replay shape reaches `t5 -> t5`
  - witness replay reaches the localized non-bottom shape rejected by `InstBot`
  - authoritative mismatch remains `InstBot expects ⊥, got: t9 -> t9`
- Express the reproducer so reviewer evidence shows the failure belongs to the `InstBot` precondition and not to witness construction, generalization, or reification.

### Task 3 - Add bounded regression coverage for the reproduction contract only

- Add one focused test slice that fails for the localized owner-boundary reason and is specific to `URI-R2-C1` replay semantics.
- Keep the test/harness bounded:
  - no new executable entrypoint
  - no alternate production path
  - no generalized replay campaign
  - no repair logic in `applyInstantiation`
- If helper extraction is required, keep it local to the same owner/test area and avoid widening public API surface.

### Task 4 - Emit the canonical `R1` artifact

- Write `docs/plans/2026-03-17-uri-r2-c1-r1-repair-boundary-reproduction.md`.
- The artifact must record:
  - `Attempt: 1`
  - inherited authoritative inputs and audit chain (`P1` through `D4`)
  - exact locked lane: `URI-R2-C1` / `uri-r2-c1-only-v1`
  - exact repair and owner boundaries
  - the implementation-facing reproducer and where it lives
  - the observed failure signature proving `R1` reproduced the bounded repair target
  - a handoff statement that `R2` may attempt one bounded production repair at the same owner boundary and nothing broader

### Task 5 - Prepare reviewer handoff

- Ensure review can verify:
  - bounded-scenario continuity
  - localized-owner continuity
  - no-second-interface
  - no-fallback
  - `R1` reproduced the localized target in implementation-facing terms
- If the round changes only docs/tests around the bounded owner area, the reviewer may use the docs-diff path and record why the full Cabal gate is or is not required.
- Acceptance for this round is only that `R1` is reproduced and documented; `R2` through `R4` remain pending.

## Non-Goals

- No repair implementation in `applyInstantiation`.
- No change to roadmap ordering or controller state.
- No mutation of predecessor evidence or review history.
- No widened replay-path refactor, no compatibility shim, and no repair-only executable interface.
- No decision work for `R4`.

## Reviewer Checks

Baseline checks from `orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-001/verification.md` still apply.

Round-specific checks:

1. The diff stays inside the bounded `R1` slice and does not alter `orchestrator/rounds/round-024/state-snapshot.json` or `orchestrator/roadmaps/2026-03-17-00-uri-r2-c1-p2-replay-repair-track-roadmap/rev-001/roadmap.md`.
2. The round remains fixed to `URI-R2-C1` and `uri-r2-c1-only-v1`.
3. The reproduced failure is explicitly tied to `witness-replay/applyInstantiation-instbot-precondition` and `MLF.Elab.Inst.applyInstantiation` (`InstBot` branch).
4. No second executable interface, compatibility fallback, or broad replay rewrite appears in the diff.
5. `docs/plans/2026-03-17-uri-r2-c1-r1-repair-boundary-reproduction.md` names `Attempt: 1` and preserves the inherited authoritative-boundary audit.
6. Review records `Implemented stage result`, `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis` under the contract-v2 retry rules.
