# `L1` Next-Target Bind For Repaired `URI-R2-C1`

Date: 2026-03-21
Round: `round-066`
Roadmap item: `L1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only bind/selection fail-closed record

## Stage Contract Freeze

This artifact implements only roadmap item `L1` for `attempt-1` with
`retry: null`.

`L1` is docs-only bind/selection work only. It carries forward the accepted
`K4 = continue-bounded` result and answers one bounded question: does the
current accepted continuity packet still support exactly one fresh lawful local
successor slice inside repaired `URI-R2-C1`, or must the round fail closed?

The inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path widening.

This artifact does not authorize implementation, verification of a future `L2`
slice, merge action, roadmap mutation, controller-state edits, bug-tracker
edits, replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
`boundTarget`, `schemeBodyTarget`,
`src/MLF/Elab/Run/ResultType/View.hs`, non-local widening, or any broader
trigger-family widening.

## Accepted `K4` Authority And Binding Predecessor Ownership

Only accepted continuity evidence is carried forward here:

1. `orchestrator/rounds/round-065/review-record.json` remains the
   authoritative acceptance proof that `K4` finalized as `attempt: 1`,
   `attempt_verdict: "accepted"`, `stage_action: "finalize"`,
   `status: "authoritative"`, with canonical artifact path
   `docs/plans/2026-03-21-uri-r2-c1-k4-next-cycle-decision-gate.md`.
2. The accepted `K4` artifact records `continue-bounded` and explicitly
   requires any successor work to begin with one fresh exact bind before any
   further implementation. It does not authorize widening or reopening.
3. The accepted `I1` / `I2` / `I3` / `I4` chain already consumed the repaired
   local single-base `rootLocalSingleBase` / `baseTarget -> baseC` /
   same-lane `targetC` family.
4. The accepted `J1` / `J2` / `J3` / `J4` chain already consumed the repaired
   local inst-arg-only singleton-base `rootLocalInstArgSingleBase` /
   `baseTarget -> baseC` / same-lane `targetC` family.
5. The accepted `K1` / `K2` / `K3` / `K4` chain already consumed the repaired
   local empty-candidate / no-inst-arg scheme-alias / base-like
   `rootLocalEmptyCandidateSchemeAliasBaseLike` /
   `baseTarget -> baseC` / same-lane `targetC` family.
6. The accepted `F2` / `F3` chain already consumed the local
   `rootLocalSchemeAliasBaseLike` `keepTargetFinal` / `targetC -> rootFinal`
   lane. It remains preserved predecessor continuity only and may not be
   rebound here as a new `L2` family.
7. Accepted negative findings remain binding:
   `U2 = authority-narrowed`,
   `U3 = uniqueness-owner-stable-refuted`,
   `U4 = constructor-acyclic-termination-refuted`.
8. `/Volumes/src/mlf4/Bugs.md` remains continuity context only. Its `## Open`
   section is empty, so current bug state does not authorize replay reopen,
   `InstBot`, or any broader successor family.

## Current `Fallback.hs` And `PipelineSpec.hs` Anchor Packet

The current read-only source/test anchors relevant to `L1` are:

1. `Fallback.hs:367-406` still computes `baseTarget` from one of four shapes:
   singleton `rootBoundCandidates`, empty `rootBoundCandidates` plus empty
   `instArgBaseBounds`, empty `rootBaseBounds` plus singleton
   `instArgBaseBounds`, or the generic singleton fallback over
   `rootBoundCandidates`.
2. `Fallback.hs:521-553` still names the local proof cluster:
   `rootLocalInstArgSingleBase`,
   `rootLocalEmptyCandidateSchemeAliasBaseLike`,
   `rootLocalSchemeAliasBaseLike`, and
   `rootLocalSingleBase`.
3. `Fallback.hs:693-720` still orders `targetC` so that `baseTarget` is first
   consumed by `rootLocalSingleBase`, then `rootLocalInstArgSingleBase`, then
   `rootLocalEmptyCandidateSchemeAliasBaseLike`, then the generic
   `rootIsSchemeAlias && rootBoundIsBaseLike` arm, while the accepted
   `rootLocalSchemeAliasBaseLike` continuity lane remains under
   `keepTargetFinal` and selects `rootFinal`.
4. `PipelineSpec.hs:1244-1310` still keeps both helper families distinct:
   `schemeAliasBaseLikeFallback` for the quantified/root-final continuity lane
   and `localEmptyCandidateSchemeAliasBaseLikeFallback` for the local
   empty-candidate `baseTarget` lane.
5. `PipelineSpec.hs:1382-1460` still keeps the adjacent
   `localInstArgSingleBaseFallback` and `localSingleBaseFallback` helpers.
6. `PipelineSpec.hs:1594-1620` still records the positive local examples for
   the accepted empty-candidate, scheme-alias/root-final, single-base, and
   inst-arg-only singleton-base lanes.
7. `PipelineSpec.hs:1667-1758` still asserts the corresponding source guards,
   including the presence of
   `rootLocalEmptyCandidateSchemeAliasBaseLike`,
   `rootLocalSingleBase`,
   `rootLocalInstArgSingleBase`,
   the generic `rootIsSchemeAlias && rootBoundIsBaseLike` arm,
   `keepTargetFinal`, and the `rootLocalSchemeAliasBaseLike -> rootFinal`
   continuation.

These anchors show that the accepted local lanes are already split and named.
They do not expose a separate unnamed local successor family after the accepted
`I`, `J`, `K`, and `F` exclusions are applied.

## Exact Case Analysis Over `baseTarget` And `targetC`

### Case 1: Local singleton `rootBoundCandidates`

If a local `TypeRef` root reaches `baseTarget` through a singleton
`rootBoundCandidates` path, `Fallback.hs:367-406` still returns `Just baseC`
through the singleton branch or the generic singleton fallback. But on that
same local lane `Fallback.hs:521-553` still makes `rootLocalSingleBase` true,
and `Fallback.hs:693-720` still consumes that case first via
`Just baseC | rootLocalSingleBase -> baseC`.

That family is already owned by the accepted `I` chain. The generic singleton
fallback inside `baseTarget` does not create a new local slice; for local roots
it still collapses back into the already accepted `rootLocalSingleBase` family.

### Case 2: Local empty-candidate / no-inst-arg scheme-alias / base-like

If a local `TypeRef` root reaches `baseTarget` through empty
`rootBoundCandidates` plus empty `instArgBaseBounds` while scheme-alias /
base-like authority holds, `Fallback.hs:367-381` still returns `Just baseC`
and `Fallback.hs:531-538` still makes
`rootLocalEmptyCandidateSchemeAliasBaseLike` true.
`Fallback.hs:693-720` still consumes that case explicitly via
`Just baseC | rootLocalEmptyCandidateSchemeAliasBaseLike -> baseC`.

That family is already owned by the accepted `K` chain. It cannot be rebound as
fresh `L2` work.

### Case 3: Local inst-arg-only singleton-base

If a local `TypeRef` root reaches `baseTarget` through
`IntSet.null rootBaseBounds && IntSet.size instArgBaseBounds == 1`,
`Fallback.hs:378-387` still returns `Just baseC`,
`Fallback.hs:525-530` still makes `rootLocalInstArgSingleBase` true, and
`Fallback.hs:693-720` still consumes that case explicitly via
`Just baseC | rootLocalInstArgSingleBase -> baseC`.

That family is already owned by the accepted `J` chain. It cannot be rebound as
fresh `L2` work.

### Case 4: Remaining generic scheme-alias / base-like `baseTarget` arm

After the accepted local `I`, `J`, and `K` lanes are removed, the remaining
`Just baseC | rootIsSchemeAlias && rootBoundIsBaseLike -> baseC` arm in
`Fallback.hs:709-710` is not a lawful fresh local slice.

For local roots, every lawful local `baseTarget` path is already captured by one
of the accepted families above:

- singleton local candidate sets collapse to accepted `rootLocalSingleBase`;
- empty local candidate sets plus empty inst-arg base bounds collapse to the
  accepted `rootLocalEmptyCandidateSchemeAliasBaseLike` lane; and
- empty root-base bounds plus singleton inst-arg base bounds collapse to the
  accepted `rootLocalInstArgSingleBase` lane.

What remains after those exclusions is preserved broader continuity only. The
current code keeps the accepted local scheme-alias continuity lane separate:
`keepTargetFinal` still includes `rootLocalSchemeAliasBaseLike`, and when that
accepted lane is used `targetC` still selects `rootFinal`, not a new `baseC`
family. `PipelineSpec.hs` still reflects the same split between
`schemeAliasBaseLikeFallback` and
`localEmptyCandidateSchemeAliasBaseLikeFallback`.

Rebinding the remaining generic scheme-alias / base-like `baseTarget` arm as a
fresh local slice would therefore do one of two forbidden things:

- reopen accepted `I` / `J` / `K` ownership by relabeling an already accepted
  local family; or
- widen into broader or non-local continuity that the current selection and
  inherited boundary keep out of scope.

## Conclusion

`L1` fails closed.

The accepted evidence does not currently support one fresh exact lawful `L2`
slice inside the allowed repaired `URI-R2-C1` boundary. No new local successor
family remains after the accepted `I`, `J`, and `K` `baseTarget` lanes and the
accepted `F2` / `F3` `rootLocalSchemeAliasBaseLike` `targetC -> rootFinal`
continuity lane are applied.

No future implementation ownership is frozen by this artifact, because no fresh
slice is lawfully selected.

## Explicit Non-Selection And Non-Authorization

This `L1` artifact does not reopen or select:

- accepted `F1` / `F2` / `F3` / `F4`;
- accepted `I1` / `I2` / `I3` / `I4`;
- accepted `J1` / `J2` / `J3` / `J4`;
- accepted `K1` / `K2` / `K3` / `K4`;
- `boundVarTarget`;
- `boundTarget`;
- `schemeBodyTarget`;
- `src/MLF/Elab/Run/ResultType/View.hs`;
- replay reopen;
- `MLF.Elab.Inst` or `InstBot`;
- non-local widening; or
- any broader trigger family.

Any future attempt to pursue the broader scheme-alias / base-like `baseTarget`
route requires a separate accepted roadmap/selection change first. `L1` does
not supply that authority.

## Retry-Aware Reviewer Handoff

This artifact is the immutable `attempt-1` bind/selection record for `L1`.
If `round-066` retries later, future attempts must be additive and must not
rewrite this artifact.

Per `orchestrator/retry-subloop.md`, reviewer outcomes supported by this
artifact remain:

- `accepted + finalize`
- `accepted + retry`
- `rejected + retry`
