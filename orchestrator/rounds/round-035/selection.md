# Round 035 Selection

Date: 2026-03-18
Round: `round-035`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 2: execute `C2` bounded fail-closed local-binding-only result-type target-retention hardening slice frozen by `C1`.

## Why This Item Should Run Now

`orchestrator/state.json` is parked at `stage: select-task` for `round-035` with `current_task: null` and `retry: null`, so no same-round retry is active and no prior review artifact forces a retry resume ahead of normal roadmap selection.

`orchestrator/roadmap.md` marks item 1 (`C1`) done and item 2 (`C2`) as the lowest-numbered unfinished roadmap entry. Under the guider contract, that makes `C2` the next lawful selection.

The authoritative `C1` bind in `docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md` already froze exactly one future implementation target: local-binding-only result-type target-retention hardening using the existing `rootBindingIsLocalType` signal, bounded to `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs` with focused coverage in `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`. Because `C1` has already selected the exact slice, the next round should execute that slice rather than reopen selection.

The approved follow-on design in `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md` and the accepted `U6` decision in `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md` both require the live campaign to remain bounded and non-widening. Running `C2` now advances the frozen repaired `URI-R2-C1` slice without widening scope, while still preserving the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.

Accepted `U2`, `U3`, and `U4` results remain binding: `authority-narrowed`, `uniqueness-owner-stable-refuted`, and `constructor-acyclic-termination-refuted`. `C2` is therefore limited to fail-closed retention hardening inside the already-selected result-type lane only; it may not reinterpret those negative findings as clearance for replay-path repair, solver widening, or broader recursive inference.

`Bugs.md` still lists open replay-path defect `BUG-2026-03-16-001` in `MLF.Elab.Inst.applyInstantiation`, but `C1` explicitly says that bug is continuity context only and does not authorize choosing `MLF.Elab.Inst` / `InstBot` as active `C2` work. The next lawful round is still the frozen local-binding-only hardening slice in `ResultType.Fallback`.

Current repository status is non-pristine (`orchestrator/state.json` modified and `tasks/todo/2026-03-18-continue-bounded-orchestrator-run/` untracked), so selecting the already-frozen narrow `C2` slice best respects existing work while keeping the round bounded to the repaired live subject.

## Round Scope Guard

- This round is limited to roadmap item `C2` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary.
- Implement only the `C1`-frozen local-binding-only target-retention hardening slice in `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs` with focused coverage in `/Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`.
- Keep wrapper/proxy-shaped retention fail-closed unless the retained target still resolves to a local `TypeRef`.
- Treat accepted `U2`/`U3`/`U4` negative findings as still binding; do not reinterpret them as authority, uniqueness, owner-stability, or constructor-feasibility clearance.
- Do not reopen replay repair in `/Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Inst.hs`, including `MLF.Elab.Inst.applyInstantiation` / `InstBot`, as part of this round.
- Do not introduce a second executable interface, compatibility fallback, convenience widening path, equi-recursive reasoning, cyclic structural encoding, or broad automatic recursive inference selection.
