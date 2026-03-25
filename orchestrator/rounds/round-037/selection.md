# Round 037 Selection

Date: 2026-03-18
Round: `round-037`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Roadmap Provenance

- Roadmap ID: `2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap`
- Roadmap Revision: `rev-004`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-004`
- State Snapshot: `orchestrator/rounds/round-037/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 4: execute `C4` bounded next-cycle decision gate for the verified repaired `URI-R2-C1` local-binding-only fail-closed slice.

## Why This Item Should Run Now

`orchestrator/rounds/round-037/state-snapshot.json` is parked at `active_round_id: round-037`, `stage: select-task`, `current_task: null`, and `retry: null`, so no same-round retry is active and no prior review outcome forces a retry resume ahead of normal roadmap selection.

`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-004/roadmap.md` marks item 1 (`C1`), item 2 (`C2`), and item 3 (`C3`) done, leaving item 4 (`C4`) as the lowest-numbered unfinished roadmap entry. Under the guider contract, that makes `C4` the next lawful selection.

The accepted `C3` artifact in `docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md` already recorded that the accepted `C2` local-binding-only `rootBindingIsLocalType` fail-closed retention slice remained stable under the bounded `ARI-C1 feasibility characterization (bounded prototype-only)` rerun, the fresh full-repo gate, and predecessor continuity checks. The next round should therefore make the bounded next-cycle decision for that verified slice rather than reopen selection, implementation, or verification.

The approved follow-on design in `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md` defines `C4` as the aggregate-only next-cycle decision gate that must record exactly one bounded result for the already-verified repaired `URI-R2-C1` lane: `continue-bounded`, `widen-approved`, or `stop-blocked`. `C4` is where the round decides the next bounded step; it is not permission to widen the live subject or silently reinterpret accepted negative findings as clearance.

Accepted predecessor findings remain binding. `C1` kept the cycle fixed to repaired `URI-R2-C1`, `C2` stayed local-binding-only and fail-closed, and `C3` verified only that bounded slice. The inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary still applies, while accepted `U2`, `U3`, and `U4` negative findings remain `authority-narrowed`, `uniqueness-owner-stable-refuted`, and `constructor-acyclic-termination-refuted`. `C4` must make the next-cycle decision within those constraints and must not reopen `C1` through `C3`.

`Bugs.md` still carries open replay-path bug `BUG-2026-03-16-001`, but it remains replay-lane continuity context only and does not authorize replay reopen, `MLF.Elab.Inst` edits, or broader automatic recursive inference selection in this round. Current repository status is already non-pristine (`orchestrator/rounds/round-037/state-snapshot.json` modified and `tasks/todo/2026-03-18-continue-bounded-orchestrator-run/` untracked), so selecting the narrow aggregate `C4` decision gate best respects existing work while advancing the next bounded roadmap item.

## Round Scope Guard

- This round is limited to roadmap item `C4` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the inherited explicit-only / non-equi-recursive / non-cyclic-graph boundary unless a later accepted roadmap update explicitly changes it.
- Decide exactly one bounded next-step result for the already-verified local-binding-only fail-closed lane: `continue-bounded`, `widen-approved`, or `stop-blocked`.
- Do not reopen `C1` selection, `C2` implementation, or `C3` verification.
- Treat accepted `U2`/`U3`/`U4` negative findings as still binding; do not reinterpret them as authority, uniqueness, owner-stability, or constructor-feasibility clearance.
- Do not introduce replay-lane reopen, `MLF.Elab.Inst` repair work, equi-recursive reasoning, cyclic structural encoding, multi-SCC widening, cross-family widening, compatibility fallbacks, or a second executable interface in this round.
