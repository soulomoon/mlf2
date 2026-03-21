# Round 064 Selection

Date: 2026-03-21
Round: `round-064`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Selected Roadmap Item

Roadmap item 31: execute the `K3` bounded verification and evidence
consolidation gate for the accepted local-binding empty-candidate /
no-inst-arg scheme-alias / base-like `K2` slice.

## Why This Item Should Run Now

`orchestrator/state.json` fixes the live controller state at
`active_round_id: "round-064"`, `stage: "select-task"`, `current_task: null`,
`retry: null`, `branch: "codex/round-064-k3-verification-gate"`, and
`last_completed_round: "round-063"`. Under `orchestrator/roles/guider.md`
and `orchestrator/retry-subloop.md`, that means there is no same-round retry
to resume, no interrupted earlier stage to continue, and no authority to
reopen a completed predecessor item instead of selecting the next roadmap
entry for the repaired live subject.

`orchestrator/roadmap.md` marks item 30 (`K2`) done and item 31 (`K3`)
pending. Because the guider must prefer the lowest-numbered unfinished item
when `retry` is `null`, item 31 is the next lawful selection. The roadmap's
completion notes for item 31 already define the exact `K3` contract: verify
that the accepted repaired `URI-R2-C1` `K2`
`rootLocalEmptyCandidateSchemeAliasBaseLike` local empty-candidate /
no-inst-arg scheme-alias / base-like `baseTarget -> baseC` /
same-lane `targetC` lane remains stable under read-only `Fallback.hs` /
`PipelineSpec.hs` anchor checks, a fresh focused rerun of
`ARI-C1 feasibility characterization (bounded prototype-only)`, a fresh full
`cabal build all && cabal test` gate, predecessor continuity checks, and
docs-only diff scope.

The accepted predecessor authority makes `K3` the exact next stage rather
than a new implementation choice. `orchestrator/rounds/round-063/review-record.json`
finalized `K2` as the authoritative accepted record with
`stage_id: "K2"`, `attempt: 1`, `attempt_verdict: "accepted"`,
`stage_action: "finalize"`, `status: "authoritative"`, and canonical artifact
path `docs/plans/2026-03-21-uri-r2-c1-k2-bounded-implementation-slice.md`.
That accepted `K2` artifact records that the exact `K1`-frozen slice stayed
bounded to `src/MLF/Elab/Run/ResultType/Fallback.hs` and
`test/PipelineSpec.hs`, introduced exactly one reviewer-auditable local proof
`rootLocalEmptyCandidateSchemeAliasBaseLike`, routed only the selected
empty-candidate / no-inst-arg scheme-alias / base-like `baseTarget -> baseC`
path and its same-lane `targetC` use through that proof, and preserved the
completed `rootLocalSingleBase` lane, the completed
`rootLocalInstArgSingleBase` lane, and the already-accepted
`rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
lane as inherited continuity only. Because `K2` is already accepted and
finalized, the next lawful stage is bounded verification of that accepted
slice, not another bind, not another implementation slice, and not a roadmap
update.

Accepted `K1` continuity narrows `K3` further. The controlling accepted bind
in `docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md` froze exactly one
future successor slice under repaired `URI-R2-C1`: the local-binding
empty-candidate / no-inst-arg scheme-alias / base-like
`baseTarget -> baseC` lane plus its same-lane `targetC` use, with ownership
frozen to `src/MLF/Elab/Run/ResultType/Fallback.hs` and
`test/PipelineSpec.hs`. `K3` therefore must verify only that exact bounded
lane and may not reinterpret accepted `U2 = authority-narrowed`,
`U3 = uniqueness-owner-stable-refuted`, or
`U4 = constructor-acyclic-termination-refuted` as clearance for widening,
replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`, `boundTarget`,
`schemeBodyTarget`, `View.hs`, non-local widening, or any broader
trigger-family work.

The broader accepted design lineage points the same way.
`docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
defines the bounded cycle shape as bind, implementation, verification, then
next-cycle decision. The accepted `U6` decision in
`docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md` carries
forward only another bounded non-widening cycle inside repaired `URI-R2-C1`
and the inherited explicit-only / non-equi-recursive / non-cyclic-graph
boundary. Selecting `K3` honors that accepted authority because it is the
verification stage immediately after the accepted `K2` implementation slice,
and it keeps the live subject fixed to repaired `URI-R2-C1` without widening
beyond the inherited no-second-interface / no-fallback boundary.

Repository status preserves that continuity rather than changing selection:
the worktree shows only the existing controller-state modification
`M orchestrator/state.json`, and `/Volumes/src/mlf4/Bugs.md` still has an
empty `## Open` section. Those facts do not create authority to skip the
pending verification gate or reorder the roadmap.

## Round Scope Guard

- This round is limited to roadmap item `K3` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond
  the inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary.
- Treat accepted `K2` as the controlling implementation authority for this
  round. `K3` may verify only the accepted
  `rootLocalEmptyCandidateSchemeAliasBaseLike` local empty-candidate /
  no-inst-arg scheme-alias / base-like `baseTarget -> baseC` /
  same-lane `targetC` lane.
- Keep verification ownership read-only over
  `src/MLF/Elab/Run/ResultType/Fallback.hs` and `test/PipelineSpec.hs`, plus
  docs-only round evidence as required by the verification stage.
- Preserve the completed `rootLocalSingleBase` lane, the completed
  `rootLocalInstArgSingleBase` lane, and the already-accepted
  `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
  lane as inherited continuity only. Do not reopen them as substitute `K3`
  families.
- Treat accepted `U2` / `U3` / `U4` negatives as binding. Do not reinterpret
  them as clearance for replay reopen, constructor widening, or any broader
  recursive-inference family.
- Do not reopen `boundVarTarget`, `boundTarget`, `schemeBodyTarget`,
  `src/MLF/Elab/Run/ResultType/View.hs`, replay reopen,
  `MLF.Elab.Inst`, `InstBot`, non-local widening, any broader
  trigger-family widening, a second executable interface, or any
  compatibility, convenience, or default-path fallback in this round.
