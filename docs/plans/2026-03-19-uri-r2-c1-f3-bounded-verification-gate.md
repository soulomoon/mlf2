# `F3` Bounded Verification Gate For Repaired `URI-R2-C1`

Date: 2026-03-19
Round: `round-044`
Roadmap item: `F3`
Stage: `implement`
Attempt: `attempt-1`
Retry: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only bounded verification/evidence gate

## Stage Contract Freeze

This artifact records `round-044` `F3` `attempt-1` as a docs-only verification
gate over the accepted `F2` local-binding scheme-alias/base-like
`keepTargetFinal` / `targetC` slice under repaired `URI-R2-C1`.

The inherited boundary remains unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path widening.

This attempt does not reopen `F1` target selection, does not reopen `F2`
implementation, does not preempt `F4`, and does not authorize edits to
`src/`, `test/`, `src-public/`, `app/`, `mlf2.cabal`, controller state,
roadmap history, `Bugs.md`, or prior-round artifacts.

Any verification failure in this stage would be a blocker to record here, not
permission to patch `Fallback.hs`, `PipelineSpec.hs`, or any other production or
test file during `attempt-1`.

## Accepted `F2` Evidence Chain Carried Forward

`round-043` remains the authoritative acceptance point for the bounded `F2`
slice. `orchestrator/rounds/round-043/review-record.json:4-26`
records:

- `stage_id = "F2"`, `attempt = 1`, `attempt_verdict = "accepted"`,
  `stage_action = "finalize"`, `status = "authoritative"`;
- `authoritative_attempt = 1`, `authoritative_result = "pass"`;
- canonical artifact path
  `docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`; and
- a full passing `checks` map including `F2-BOUNDVARTARGET-ABSENT`,
  `F2-OUT-OF-SCOPE-TRIGGERS`, `F2-FOCUSED-COVERAGE`, and `F2-FULL-GATE`.

The accepted `F2` artifact at
`docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md:14-98`
freezes the exact bounded target and exclusions that `F3` rechecks:

- lines `14-34`: one bounded `F2` lane only, with the inherited explicit-only /
  non-equi-recursive / non-cyclic / no-second-interface / no-widening boundary;
- lines `47-52`: `F1` selected exactly the local-binding scheme-alias/base-like
  `keepTargetFinal` / `targetC` lane and left accepted `U2` / `U3` / `U4`
  negative findings binding;
- lines `56-68`: `F2` introduced `rootLocalSchemeAliasBaseLike`, routed
  `keepTargetFinal` through it, and made `targetC` choose `rootFinal` only for
  that local lane;
- lines `72-77`: `boundVarTarget` stayed absent for the selected slice,
  `rootHasMultiInst` / `instArgRootMultiBase` stayed unchanged and out of
  scope, and there was no replay reopen, `MLF.Elab.Inst`, or `InstBot` work;
- lines `81-98`: the focused `ARI-C1` block carried exactly one local positive
  example and one matched non-local fail-closed contrast for this slice.

`Bugs.md:7-22`
still records replay bug `BUG-2026-03-16-001`, but only as replay-lane
continuity context. It does not reopen replay repair authority for this `F3`
verification gate.

## Read-Only Anchor Evidence

### `Fallback.hs`

Read-only inspection of
`src/MLF/Elab/Run/ResultType/Fallback.hs:521-693`
shows the accepted `F2` slice still present without widening:

- lines `521-528` define `rootIsSchemeAlias`, `rootBoundIsBaseLike`, and
  `rootLocalSchemeAliasBaseLike =
  rootBindingIsLocalType && rootIsSchemeAlias && rootBoundIsBaseLike`;
- lines `619-667` keep inherited `boundVarTarget` machinery as context, still
  guarded by the local `TypeRef` lane via `sameLocalTypeLane`, but that branch
  remains contextual rather than the proof for the selected `F2` slice;
- lines `668-674` define `keepTargetFinal` with the required local gate
  `rootBindingIsLocalType` and the unchanged trigger families
  `rootHasMultiInst`, `instArgRootMultiBase`, `rootLocalSchemeAliasBaseLike`,
  and `boundVarTarget`;
- lines `675-693` make `targetC` choose `rootFinal` only when
  `rootLocalSchemeAliasBaseLike` holds, otherwise preserving the inherited
  fail-closed branches through `TyVar`, `boundVarTarget`,
  `schemeBodyTarget targetPresolutionView rootC`, and the outer
  `rootBindingIsLocalType` / `rootFinal` split.

This matches the accepted `F2` contract: `boundVarTarget` still exists as
inherited context, but the selected local scheme-alias/base-like proof does not
depend on it, and `rootHasMultiInst` / `instArgRootMultiBase` remain unchanged
and out of scope.

### `PipelineSpec.hs`

Read-only inspection of
`test/PipelineSpec.hs:1298-1375`
shows the focused `ARI-C1 feasibility characterization (bounded prototype-only)`
block still carries the accepted bounded coverage:

- lines `1298-1308`: the retained-child same-lane baseline still asserts the
  local `TypeRef`-lane lookup discipline inherited from `E2` / `E3`;
- lines `1309-1318`: the nested-`forall` fail-closed retained-child contrast
  remains in place as inherited bounded context;
- lines `1320-1328`: the local scheme-alias/base-like local-`TypeRef` success
  example still expects the quantified fail-closed shell `forall _. Int`;
- lines `1330-1333`: the matched non-local fail-closed contrast still expects
  plain `Int` with `containsMu False`;
- lines `1335-1350`: the corresponding unannotated and non-local proxy controls
  remain fail-closed outside the selected local lane;
- lines `1352-1363`: the source guard still names
  `rootLocalSchemeAliasBaseLike`, keeps the local-binding gate inside
  `keepTargetFinal`, and checks the `targetC` branch that chooses `rootFinal`
  only for that one local proof;
- lines `1365-1375`: the same non-local proxy wrapper remains fail-closed at
  pipeline entrypoints.

Together, these anchors show the accepted `F2` slice is still the same bounded
local lane and that the inherited out-of-scope controls remain fail-closed.

## Fresh Verification Results

Verification ran in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-044`.

### Baseline Controller Checks

- `git diff --check` -> pass (exit `0`, no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass (exit `0`)
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass:
  - items `1` through `12` remain parseable;
  - item `11` (`F3`) is still pending and item `12` (`F4`) is still pending.
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

### Focused `ARI-C1` Rerun

- Command:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Result: pass
- Fresh summary:
  - `11 examples, 0 failures`
  - included the retained-child baseline, the local scheme-alias/base-like
    local-`TypeRef` success example, the matched non-local fail-closed contrast,
    and the local-binding source-guard example.

### Fresh Full Repo Gate

- Command: `cabal build all && cabal test`
- Result: pass
- Fresh summary:
  - `cabal build all` rebuilt the repo targets, including `mlf2`,
    `frozen-parity-gen`, and `mlf2-test`;
  - `cabal test` finished with `1132 examples, 0 failures`.

## Accepted-`F2` Continuity Recheck

- `python3 -m json.tool orchestrator/rounds/round-043/review-record.json >/dev/null`
  -> pass
- `rg -n 'rootLocalSchemeAliasBaseLike|keepTargetFinal|targetC|boundVarTarget|rootHasMultiInst|instArgRootMultiBase' docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`
  -> pass; the accepted `F2` artifact still names the selected proof,
  `boundVarTarget` absence for the slice, and the unchanged out-of-scope
  trigger families.

Reviewer-readable continuity summary:

- the predecessor review record still finalizes `F2` as accepted authoritative
  `attempt-1`;
- the canonical predecessor artifact path still points at the accepted `F2`
  implementation slice;
- the predecessor `checks` map still records the bounded contract, bounded
  coverage, `boundVarTarget` absence, out-of-scope trigger stability, continuity,
  and full-gate pass;
- this `F3` artifact rechecks exactly that accepted slice and nothing broader.

## Docs-Only Diff Evidence

`F3` `attempt-1` is docs-only and did not patch production or test code.

- `git diff --name-only` -> pass:
  - no tracked diffs after writing the canonical `F3` artifact
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> pass:
  - no non-docs diffs under the round worktree
- `git status --short --untracked-files=all` -> bounded reviewer evidence:
  - `?? docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
  - `?? orchestrator/rounds/round-044/plan.md`
  - `?? orchestrator/rounds/round-044/selection.md`

The only new file introduced by this implement-stage attempt is the canonical
`F3` docs artifact. The untracked `plan.md` and `selection.md` files are
pre-existing round-control context and were not edited here.

## Stability Conclusion

The accepted `F2` local-binding scheme-alias/base-like `keepTargetFinal` /
`targetC` slice reverified cleanly in this round:

- read-only code/test anchors still match the accepted bounded proof;
- the focused `ARI-C1` block passed fresh with `11 examples, 0 failures`;
- the fresh full repo gate passed with `1132 examples, 0 failures`; and
- predecessor continuity remains intact against the accepted `F2` artifact and
  `round-043` review record.

No blocker was observed in `attempt-1`.

## Non-Authorization Statement

This artifact does **not** authorize:

- replay reopen;
- `MLF.Elab.Inst`;
- `InstBot`;
- non-local widening;
- use of `boundVarTarget` as new authority for the selected slice;
- widening of `rootHasMultiInst` or `instArgRootMultiBase`;
- equi-recursive reasoning or implicit unfolding;
- cyclic structural encoding;
- multi-SCC or cross-family widening;
- a second executable interface; or
- compatibility, convenience, or default-path widening.

If later review finds a verification problem, the lawful next action is
`accepted + retry` or `rejected + retry` with a recorded blocker, not code/test
patching inside this already-bounded docs-only gate.
