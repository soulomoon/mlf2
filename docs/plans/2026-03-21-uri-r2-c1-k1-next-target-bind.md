# `K1` Next-Target Bind For Repaired `URI-R2-C1`

Date: 2026-03-21
Round: `round-062`
Roadmap item: `K1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only bind/selection freeze

## Stage Contract Freeze

This artifact implements only roadmap item `K1` for `attempt-1` with
`retry: null`.

`K1` is a bind/selection stage only. It carries forward the accepted
`J4 = continue-bounded` outcome and freezes exactly one future bounded `K2`
slice. It does not perform production edits, test edits, review or merge
bookkeeping, roadmap mutation, controller-state edits, bug-tracker updates,
or subject widening.

The inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path widening.

This artifact therefore does not reinterpret accepted `U2`, `U3`, or `U4`
negative findings as clearance for replay reopen, `MLF.Elab.Inst`,
`InstBot`, `boundVarTarget`, `boundTarget`,
`src/MLF/Elab/Run/ResultType/View.hs`, `schemeBodyTarget`, non-local
widening, or any broader recursive-inference family.

## Accepted `J4` Evidence Chain And Current Anchor Packet

Only accepted bounded predecessor facts and current read-only anchors are
carried forward here:

1. `orchestrator/rounds/round-061/review-record.json` remains the
   authoritative acceptance proof that `J4` finalized as `attempt: 1`,
   `attempt_verdict: "accepted"`, `stage_action: "finalize"`,
   `status: "authoritative"`, with canonical artifact path
   `docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`.
2. The accepted `J4` artifact records result token `continue-bounded` and
   explicitly requires any successor work to begin with a fresh bounded exact
   bind rather than implementation, review, widening, or replay reopen.
3. The accepted `J4` artifact already carries forward the accepted
   `I4` / `J1` / `J2` / `J3` chain as immutable predecessor evidence only.
   `K1` does not reopen the accepted `I4` decision logic, `J1` target
   selection, `J2` implementation, or `J3` verification.
4. The accepted `J1` / `J2` / `J3` chain already consumed the repaired
   `URI-R2-C1` local-binding inst-arg-only singleton-base
   `rootLocalInstArgSingleBase` / `baseTarget -> baseC` / same-lane `targetC`
   lane. In the current source anchor at `Fallback.hs:382-387`, that
   completed lane is still present and is not reopened here.
5. The accepted `F2` / `F3` chain already consumed the local-binding
   `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
   lane. The current helper, examples, and source-guard anchors in
   `PipelineSpec.hs:1244-1269`, `PipelineSpec.hs:1560-1587`, and
   `PipelineSpec.hs:1660-1708` still preserve that accepted lane as
   predecessor continuity only, not as a second live `K2` family.
6. The current source anchor at `Fallback.hs:377-381` still exposes one
   adjacent empty-candidate / no-inst-arg `baseTarget` branch guarded by
   `IntSet.null rootBoundCandidates && IntSet.null instArgBaseBounds &&
   not rootHasMultiInst && not instArgRootMultiBase`.
7. The current proof cluster at `Fallback.hs:531-545` still names
   `rootLocalInstArgSingleBase`, `rootLocalSchemeAliasBaseLike`, and
   `rootLocalSingleBase`, but no dedicated reviewer-auditable local proof yet
   isolates the empty-candidate / no-inst-arg scheme-alias / base-like
   `baseTarget` consumer as its own bounded authority.
8. The current `targetC` ordering at `Fallback.hs:692-700` still consumes the
   completed `rootLocalSingleBase` lane first, the completed
   `rootLocalInstArgSingleBase` lane second, and then the broader
   scheme-alias / base-like `baseTarget` route, but it does not yet name a
   dedicated local empty-candidate / no-inst-arg scheme-alias / base-like
   proof as its own bounded local lane.
9. The current helper cluster in `PipelineSpec.hs:1244-1398` still defines
   `schemeAliasBaseLikeFallback` and `localInstArgSingleBaseFallback`, but no
   focused helper yet isolates the local empty-candidate / no-inst-arg
   scheme-alias / base-like `baseTarget` authority as a separate bounded
   packet.
10. `/Volumes/src/mlf4/Bugs.md` remains continuity context only. Its `## Open`
    section is empty, so current bug state does not authorize replay reopen,
    `InstBot`, or any broader successor family here.
11. Accepted negative findings remain binding:
    `U2 = authority-narrowed`,
    `U3 = uniqueness-owner-stable-refuted`,
    `U4 = constructor-acyclic-termination-refuted`.

Taken together, the accepted `J4` packet requires one fresh exact bind, the
accepted `F2` / `F3` and `J1` / `J2` / `J3` chains already consumed their
selected local lanes, and the current anchors expose exactly one adjacent
unselected local empty-candidate / no-inst-arg scheme-alias / base-like
`baseTarget` sub-branch still lacking a dedicated bounded proof.

## Exact Selected `K2` Target (Exactly One)

The only frozen future `K2` target is:

local-binding empty-candidate / no-inst-arg scheme-alias / base-like
`baseTarget -> baseC` fail-closed hardening in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-062/src/MLF/Elab/Run/ResultType/Fallback.hs`
limited to the existing `baseTarget` branch at `Fallback.hs:377-381` and the
downstream same-lane `targetC` decision at `Fallback.hs:698-700`.

Required interpretation of that one bounded slice:

- the reviewer-auditable local proof must stay confined to a local-binding
  refinement of the existing ingredients already visible in `Fallback.hs`,
  using all of:
  `rootBindingIsLocalType`,
  `rootIsSchemeAlias`,
  `rootBoundIsBaseLike`,
  `IntSet.null rootBoundCandidates`,
  `IntSet.null instArgBaseBounds`,
  `not rootHasMultiInst`,
  `not instArgRootMultiBase`;
- future selected behavior may harden only the one local `baseTarget` path
  where both candidate sets stay empty but scheme-alias / base-like evidence
  still collapses to a concrete base-like target on the repaired local
  `TypeRef` lane;
- future same-lane `targetC` work may refine only the ordering and proof
  needed to consume that one selected local `baseTarget` path, without
  changing the accepted `F2` / `F3`
  `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
  lane, the completed `rootLocalSingleBase` lane, or the completed
  `rootLocalInstArgSingleBase` lane; and
- `rootLocalSingleBase`, `rootLocalInstArgSingleBase`,
  `rootLocalSchemeAliasBaseLike` as already-accepted `rootFinal` authority,
  `rootLocalMultiInst`, `rootLocalInstArgMultiBase`, `boundVarTarget`,
  `boundTarget`, `schemeBodyTarget`, replay reopen, and every non-local or
  broader family remain inherited context only rather than selected `K2`
  authority.

## Why This Is The Only Lawful Next Slice

This is the only lawful next slice because the accepted `J4` continuity and
the current bounded anchors expose one adjacent unselected local
scheme-alias / base-like `baseTarget` sub-branch and no broader successor
authority:

- `J4` already required a fresh bounded exact bind before any further
  implementation and did not authorize widening or direct code changes.
- `Fallback.hs:372-376` is the completed local single-base lane already
  consumed by the accepted `I` chain.
- `Fallback.hs:382-387` is the completed local inst-arg-only singleton-base
  lane already consumed by the accepted `J` chain.
- `Fallback.hs:537-540` plus `Fallback.hs:685-707` preserve the accepted
  local `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
  lane from the `F2` / `F3` chain as continuity only.
- `Fallback.hs:377-381` is the remaining adjacent local `baseTarget` branch
  where both candidate sets stay empty while scheme-alias / base-like
  evidence can still hold and both multi-inst guards remain false.
- `Fallback.hs:698-700` still consumes the broader scheme-alias / base-like
  `baseTarget` route without a dedicated local empty-candidate / no-inst-arg
  proof, making that exact local refinement the reviewer-auditable gap.
- `PipelineSpec.hs:1244-1269`, `PipelineSpec.hs:1560-1587`, and
  `PipelineSpec.hs:1660-1708` still cover the accepted scheme-alias /
  base-like continuity and current source guards, but no focused helper,
  example, or source assertion yet names the empty-candidate / no-inst-arg
  local `baseTarget` authority as its own bounded `K2` packet.

No replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
`boundTarget`, `src/MLF/Elab/Run/ResultType/View.hs`, `schemeBodyTarget`,
non-local widening, or cross-family widening is selected here.

## Future File Ownership

Future `K2` ownership is frozen to exactly these files only:

- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-062/src/MLF/Elab/Run/ResultType/Fallback.hs`
- `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-062/test/PipelineSpec.hs`

## Future Focused Verification Intent

Future `K2` evidence remains one bounded extension of the existing
`ARI-C1 feasibility characterization (bounded prototype-only)` block in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-062/test/PipelineSpec.hs`
only:

- add one local-binding empty-candidate / no-inst-arg scheme-alias /
  base-like success helper/example that keeps the root on the local
  `TypeRef` lane, keeps both `rootBoundCandidates` and `instArgBaseBounds`
  empty, preserves the scheme-alias / base-like inputs, and expects the
  concrete selected base result;
- add one matched local contrast using the same wrapper family but changing
  one selected-lane precondition, so the result stays on already-accepted
  continuity rather than the new `K2` lane; and
- refresh source-guard assertions only enough to name the selected local
  proof and prove that `targetC` consults the new empty-candidate /
  no-inst-arg local `baseTarget` lane without changing the preserved
  completed `rootLocalSingleBase` lane, the preserved completed
  `rootLocalInstArgSingleBase` lane, or the already-accepted `F2` local
  `rootFinal` lane.

No second implementation family is selected in this artifact.

## Explicit Non-Selection And Non-Authorization

This `K1` bind does not authorize:

- implementation, review approval, merge action, or roadmap mutation;
- any edit to `orchestrator/state.json`, `orchestrator/roadmap.md`, or
  `/Volumes/src/mlf4/Bugs.md`;
- reopening the accepted `I4` / `J1` / `J2` / `J3` / `J4` chain as live
  work;
- reopening the accepted `F2` / `F3`
  `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
  lane as a second `K2` family;
- reopening `boundVarTarget`, `boundTarget`,
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-062/src/MLF/Elab/Run/ResultType/View.hs`,
  or `schemeBodyTarget`;
- replay reopen, `MLF.Elab.Inst`, `InstBot`, or any replay-path repair as
  active `K2` work;
- non-local widening, cross-family widening, equi-recursive reasoning,
  implicit unfolding, cyclic structural graph encoding, or broader recursive
  inference beyond repaired `URI-R2-C1`; or
- any second interface, compatibility shim, convenience fallback, or
  default-path widening.

Any broader successor family requires a separate accepted roadmap change
before implementation.

## Retry-Aware Reviewer Handoff

This artifact is the immutable `attempt-1` bind/selection record for `K1`.
If `round-062` retries later, future attempts must be additive and must not
rewrite this artifact.

Per `orchestrator/retry-subloop.md`, reviewer outcomes supported by this
artifact remain:

- `accepted + finalize`
- `accepted + retry`
- `rejected + retry`

Reviewer record fields expected for this round remain:

- `Implemented stage result`
- `Attempt verdict`
- `Stage action`
- `Retry reason`
- `Fix hypothesis`

## Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-062`

### Baseline Commands

- `git diff --check` -> pass
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass

### Required File-Presence And Continuity Commands

- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  -> pass
- `test -f orchestrator/retry-subloop.md` -> pass
- `test -f docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`
  -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
  -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-j2-bounded-implementation-slice.md`
  -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-j3-bounded-verification-gate.md`
  -> pass
- `test -f docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`
  -> pass
- `test -f docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
  -> pass
- `test -f /Volumes/src/mlf4/Bugs.md` -> pass

### Accepted-`J4` Continuity Commands

- `python3 -m json.tool orchestrator/rounds/round-061/review-record.json >/dev/null`
  -> pass
- short `python3` assertion over `orchestrator/rounds/round-061/review-record.json`
  -> pass:
  - `PASS round-061 stage_id=J4 attempt=1 verdict=accepted action=finalize status=authoritative artifact=docs/plans/2026-03-21-uri-r2-c1-j4-next-cycle-decision-gate.md`

### Source/Test Anchor Commands

- `rg -n 'IntSet.null rootBoundCandidates|IntSet.null instArgBaseBounds|rootLocalInstArgSingleBase|rootLocalSchemeAliasBaseLike|rootLocalSingleBase|rootIsSchemeAlias|rootBoundIsBaseLike' src/MLF/Elab/Run/ResultType/Fallback.hs`
  -> pass
- `rg -n 'schemeAliasBaseLikeFallback|localInstArgSingleBaseFallback|keeps local scheme-alias/base-like fallback on the local TypeRef lane|keeps local inst-arg-only singleton-base fallback on the local TypeRef lane|uses the local-binding gate when deciding local single-base and retained fallback targets' test/PipelineSpec.hs`
  -> pass

### Docs-Only Diff Discipline

- `git status --short --untracked-files=all` -> pass for bounded docs-only
  output plus the same pre-existing controller / guider / planner state:
  - `M orchestrator/state.json` (pre-existing controller state; not edited
    here)
  - `?? orchestrator/rounds/round-062/selection.md` (pre-existing guider
    artifact; not edited here)
  - `?? orchestrator/rounds/round-062/plan.md` (pre-existing planner
    artifact; not edited here)
  - `?? docs/plans/2026-03-21-uri-r2-c1-k1-next-target-bind.md`
  - `?? orchestrator/rounds/round-062/implementation-notes.md`
- `git diff --name-only` -> `orchestrator/state.json`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> pass (no output)

The full Cabal gate was intentionally not run because this round is docs-only
and bind-only; no files under `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal` were edited.
