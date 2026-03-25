# Round 063 Selection

Date: 2026-03-21
Round: `round-063`
Role: guider
Active subject: repaired `URI-R2-C1`
Successor lane: continue-bounded unannotated iso-recursive inference

## Roadmap Provenance

- Roadmap ID: `2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap`
- Roadmap Revision: `rev-030`
- Roadmap Dir: `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-030`
- State Snapshot: `orchestrator/rounds/round-063/state-snapshot.json`
- Migration note: backfilled from git history using the last authoritative control-plane anchor available for this round.

## Selected Roadmap Item

Roadmap item 30: execute the `K2` bounded local-binding empty-candidate /
no-inst-arg scheme-alias / base-like `baseTarget -> baseC` / same-lane
`targetC` fail-closed implementation slice frozen by `K1`.

## Why This Item Should Run Now

`orchestrator/rounds/round-063/state-snapshot.json` already fixes the live controller state at
`active_round_id: "round-063"`, `stage: "select-task"`, `current_task: null`,
`retry: null`, `branch: "codex/round-063-k2-empty-candidate-hardening"`, and
`last_completed_round: "round-062"`. Under the guider contract and
`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-030/retry-subloop.md`, that means there is no same-round retry to
resume, no interrupted earlier stage to continue, and no lawful basis to skip
selection or reopen an older round instead of choosing the next roadmap item.

`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-030/roadmap.md` marks item 29 (`K1`) done and item 30 (`K2`)
pending. Because item 30 is now the lowest-numbered unfinished roadmap entry,
`K2` is the next lawful selection unless current continuity shows a blocker.
Current continuity does not: repository status shows only the existing
controller-state edit (`M orchestrator/rounds/round-063/state-snapshot.json`), and
`/Volumes/src/mlf4/Bugs.md` still has an empty `## Open` section. Those facts
preserve continuity only; they do not create authority to widen, replay, or
reorder the roadmap.

The accepted predecessor authority makes this successor exact rather than
speculative. `orchestrator/rounds/round-062/review-record.json` finalized
`K1` as the authoritative accepted record with `stage_id: "K1"`,
`attempt: 1`, `attempt_verdict: "accepted"`, `stage_action: "finalize"`,
`status: "authoritative"`, and canonical artifact path
`docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md`. That accepted `K1`
artifact freezes exactly one future `K2` slice: local-binding
empty-candidate / no-inst-arg scheme-alias / base-like
`baseTarget -> baseC` fail-closed hardening in
`src/MLF/Elab/Run/ResultType/Fallback.hs`, limited to the existing
`baseTarget` branch and its downstream same-lane `targetC` decision, with
future ownership frozen to `src/MLF/Elab/Run/ResultType/Fallback.hs` and
`test/PipelineSpec.hs` only.

That accepted `K1` freeze also narrows what `K2` may not do. It preserves the
accepted `F2` / `F3` `rootLocalSchemeAliasBaseLike`
`keepTargetFinal` / `targetC -> rootFinal` lane, the completed
`rootLocalSingleBase` lane, and the completed `rootLocalInstArgSingleBase`
lane as inherited continuity only rather than live successor work. It also
keeps replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
`boundTarget`, `schemeBodyTarget`,
`src/MLF/Elab/Run/ResultType/View.hs`, non-local widening, and broader
trigger-family widening out of scope. Because `K1` already performed the
required fresh exact-target bind after accepted `J4 = continue-bounded`,
`K2` is now the lawful next stage: bounded implementation of that one frozen
slice, not a new bind, not a review gate, and not a roadmap update.

The broader accepted design lineage points the same way.
`docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
and the accepted `U6` decision in
`docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md` both keep
the live subject fixed to repaired `URI-R2-C1` and permit only bounded
non-widening successor work inside the inherited explicit-only /
non-equi-recursive / non-cyclic-graph boundary. Accepted `U2 =
authority-narrowed`, `U3 = uniqueness-owner-stable-refuted`, and
`U4 = constructor-acyclic-termination-refuted` therefore remain binding
negative findings, not clearance. Selecting `K2` honors that contract because
it implements only the accepted `K1` frozen slice, preserves the explicit-only
/ non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback
boundary, and does not treat any accepted negative result as a reason to widen
beyond repaired `URI-R2-C1`.

## Round Scope Guard

- This round is limited to roadmap item `K2` only.
- Keep the live subject fixed to repaired `URI-R2-C1`; do not widen beyond the
  inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary.
- Treat accepted `K1` as the controlling authority for this round. `K2` may
  harden only the one local-binding empty-candidate / no-inst-arg
  scheme-alias / base-like `baseTarget -> baseC` path plus its same-lane
  `targetC` use, using the accepted local refinement ingredients
  `rootBindingIsLocalType`, `rootIsSchemeAlias`, `rootBoundIsBaseLike`,
  `IntSet.null rootBoundCandidates`, `IntSet.null instArgBaseBounds`,
  `not rootHasMultiInst`, and `not instArgRootMultiBase`.
- Keep future ownership frozen to `src/MLF/Elab/Run/ResultType/Fallback.hs`
  and `test/PipelineSpec.hs` only.
- Preserve the accepted `F2` / `F3`
  `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
  lane, the completed `rootLocalSingleBase` lane, and the completed
  `rootLocalInstArgSingleBase` lane as inherited continuity only. Do not
  reopen them as parallel or substitute `K2` families.
- Treat accepted `U2` / `U3` / `U4` negatives as binding. Do not reinterpret
  them as clearance for replay reopen, constructor widening, or any broader
  recursive-inference family.
- Do not reopen `boundVarTarget`, `boundTarget`, `schemeBodyTarget`,
  `src/MLF/Elab/Run/ResultType/View.hs`, replay reopen,
  `MLF.Elab.Inst`, `InstBot`, non-local widening, any broader trigger-family
  widening, a second executable interface, or any compatibility, convenience,
  or default-path fallback in this round.
