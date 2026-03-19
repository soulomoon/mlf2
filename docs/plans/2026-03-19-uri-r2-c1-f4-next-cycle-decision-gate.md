# `F4` Next-Cycle Decision Gate For Repaired `URI-R2-C1`

Date: 2026-03-19
Round: `round-045`
Roadmap item: `F4`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only aggregate next-cycle decision gate

## Stage Contract Freeze

This artifact records `round-045` `F4` `attempt-1` as a docs-only, aggregate-only
decision gate over the already accepted `F3` verification baseline for the
accepted `F2` local-binding scheme-alias/base-like
`rootLocalSchemeAliasBaseLike` / `targetC -> rootFinal` lane under repaired
`URI-R2-C1`.

The inherited boundary remains unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path widening.

This round records exactly one lawful result token from the closed set
`continue-bounded`, `widen-approved`, `stop-blocked`. It does not reopen `F1`,
`F2`, or `F3`; does not amend the roadmap; does not authorize replay reopen,
`MLF.Elab.Inst`, `InstBot`, `boundVarTarget` widening for this slice, or
`rootHasMultiInst` / `instArgRootMultiBase` widening; and does not authorize
production, test, public API, executable, Cabal, controller-state, roadmap, or
bug-tracker edits.

Any contradiction inside the accepted `F3` evidence chain would be a blocker to
record through `stop-blocked`, not permission to patch code, add tests, reopen
replay, or update `Bugs.md` in this attempt.

Reviewer outcomes for this stage are limited to `accepted + finalize` or
`rejected + retry`. `accepted + retry` is forbidden for `F4`.

## Accepted `F3` Evidence Chain Carried Forward

The decision baseline remains the accepted `F3` artifact plus the authoritative
`round-044` review record, without reopening predecessor stages.

Accepted carry-forward facts:

- the live subject remains repaired `URI-R2-C1`;
- the bounded lane remains the accepted `F2` local-binding scheme-alias/base-like
  `rootLocalSchemeAliasBaseLike` / `targetC -> rootFinal` slice;
- the accepted read-only ownership anchors remain `Fallback.hs` and
  `PipelineSpec.hs`;
- `boundVarTarget` remains absent as authority for the selected slice; and
- `rootHasMultiInst` / `instArgRootMultiBase` remain unchanged and out of scope.

`orchestrator/rounds/round-044/review-record.json` remains authoritative for the
current verification baseline. Fresh docs-only continuity checks confirm:

- `stage_id = "F3"`
- `attempt = 1`
- `attempt_verdict = "accepted"`
- `stage_action = "finalize"`
- `status = "authoritative"`
- `artifact_path = "docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md"`

The accepted `checks` map also remains fully passing for the bounded baseline:

- `F3-CONTRACT`
- `F3-DOCS-ONLY`
- `F3-F2-CARRY-FORWARD`
- `F3-FALLBACK-ANCHOR`
- `F3-BOUNDVARTARGET-ABSENT`
- `F3-OUT-OF-SCOPE-TRIGGERS`
- `F3-FOCUSED-RERUN`
- `F3-FULL-GATE`
- `F3-CANONICAL-ARTIFACT`
- `F3-CONTINUITY`

The accepted `F3` artifact still names the same bounded lane and exclusions:

- `rootLocalSchemeAliasBaseLike` remains the selected local proof;
- `targetC -> rootFinal` remains the bounded admitted route;
- `boundVarTarget` remains inherited context rather than new authority for this
  slice; and
- `rootHasMultiInst` / `instArgRootMultiBase` remain unchanged and out of scope.

`Bugs.md` still carries `BUG-2026-03-16-001`, but only as replay-lane continuity
context. It does not reopen replay repair or widen this round.

## Docs-Only Verification And Continuity Checks

Verification ran in
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-045`.

### Baseline Checks

- `git diff --check` -> pass (exit `0`, no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass (exit `0`)
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass:
  - items `1` through `12` remain parseable;
  - item `12` (`F4`) remains pending.
- required predecessor/design files -> pass:
  - `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  - `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  - `orchestrator/retry-subloop.md`

### Authoritative `F3` Continuity Checks

- `test -f docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
  -> pass
- `python3 -m json.tool orchestrator/rounds/round-044/review-record.json >/dev/null`
  -> pass
- short `python3` assertion over `orchestrator/rounds/round-044/review-record.json`
  -> pass:
  - confirmed `F3` / `attempt-1` / `accepted` / `finalize` / `authoritative`
    metadata;
  - confirmed canonical artifact path
    `docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`;
  - confirmed all required `F3-*` pass keys listed above remained `pass`.
- `rg -n 'rootLocalSchemeAliasBaseLike|targetC -> rootFinal|boundVarTarget|rootHasMultiInst|instArgRootMultiBase' docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
  -> pass; the accepted `F3` artifact still binds this decision gate to the same
  bounded lane and exclusions.

### Docs-Only Diff Evidence

`F4` `attempt-1` remains docs-only and aggregate-only.

- `git diff --name-only` -> pass:
  - no tracked diffs
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> pass:
  - no non-docs diffs under the round worktree
- `git status --short --untracked-files=all` -> bounded reviewer evidence:
  - `?? docs/plans/2026-03-19-uri-r2-c1-f4-next-cycle-decision-gate.md`
  - `?? orchestrator/rounds/round-045/plan.md`
  - `?? orchestrator/rounds/round-045/selection.md`

The only new file introduced by this implement-stage attempt is the canonical
`F4` docs artifact. The untracked `plan.md` and `selection.md` files are
pre-existing round-control context and were not edited here.

## Result Token

Final next-cycle decision token: `continue-bounded`

This is the only lawful result for `attempt-1` because the accepted `F3`
evidence chain remains authoritative, the accepted bounded verification baseline
still stands for the same repaired `URI-R2-C1` local-binding scheme-alias/base-like
lane, no blocker appeared in the docs-only continuity checks, and no
already-accepted widening authority exists in the current repository state.

This token preserves:

- repaired `URI-R2-C1` as the live subject;
- the accepted `Fallback.hs` / `PipelineSpec.hs` ownership anchor for the bounded
  local lane;
- the inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary; and
- the accepted exclusions on replay reopen, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget` widening for this slice, and
  `rootHasMultiInst` / `instArgRootMultiBase` widening.

## Why The Other Tokens Are Not Lawful

### Why `widen-approved` is not lawful

`widen-approved` would require already-accepted repository evidence that amends
the live subject or inherited boundary and clears the still-binding negative
results from `U2`, `U3`, and `U4`. No such accepted artifact or roadmap
amendment exists in the current decision baseline. The accepted `F3` artifact
and the `round-044` authoritative review record preserve the bounded `F2` lane;
they do not convert `authority-narrowed`,
`uniqueness-owner-stable-refuted`, or
`constructor-acyclic-termination-refuted` into widening clearance.

### Why `stop-blocked` is not lawful

`stop-blocked` would require a missing required artifact, a contradictory or
non-authoritative `round-044` review record, or a failure in the docs-level
continuity checks that prevents treating accepted `F3` as the current bounded
verification baseline. None of those blockers appeared:

- the accepted `F3` artifact is present;
- the `round-044` review record remains valid JSON and authoritative;
- the required `F3-*` checks remain `pass`; and
- docs-only continuity checks found no non-doc/code-path widening diff.

## Full-Gate Skip Note

This round intentionally does **not** rerun the focused `ARI-C1` block or
`cabal build all && cabal test`.

Reason: `F4` `attempt-1` is aggregate-only and docs-only, with no authorized
production, test, public API, executable, or Cabal edits. The accepted `F3`
artifact already carries the fresh bounded verification baseline for this exact
lane, including the focused rerun and full repo gate. Reopening those commands in
`F4` would exceed the lawful scope of this decision gate.

## Blockers

None.

## Non-Authorization Statement

This artifact does **not** authorize:

- replay reopen;
- `MLF.Elab.Inst`;
- `InstBot`;
- `boundVarTarget` widening for this slice;
- widening of `rootHasMultiInst` or `instArgRootMultiBase`;
- non-local binding widening;
- equi-recursive reasoning or implicit unfolding;
- cyclic structural encoding;
- multi-SCC widening;
- cross-family widening;
- a second executable interface; or
- compatibility, convenience, or default-path widening.
