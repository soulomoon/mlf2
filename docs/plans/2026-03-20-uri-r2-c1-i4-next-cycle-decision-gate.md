# `I4` Next-Cycle Decision Gate For Repaired `URI-R2-C1`

Date: 2026-03-20
Round: `round-057`
Roadmap item: `I4`
Stage: `implement`
Attempt: `attempt-2`
Retry state: active (`retry.stage_id: "I4"`, `retry.attempt: 2`, `retry.retry_reason: "i4-bug-continuity-authority-mismatch"`)
Live subject: repaired `URI-R2-C1`
Artifact kind: aggregate-only docs-only next-cycle decision gate

## Stage Contract Freeze

This artifact executes only roadmap item `I4` for retry `attempt-2`.

The retry delta is narrow: repair the rejected `attempt-1` bug-authority
continuity mismatch and rerun the same closed-rule `I4` decision using the
live canonical bug tracker plus accepted `I3` / `H4` continuity correctly.
This artifact refreshes the same canonical `I4` file in place; it does not
mint a replacement artifact, does not reopen `I1`, `I2`, or `I3`, and does
not edit `selection.md`, `plan.md`, `review.md`, `/Volumes/src/mlf4/Bugs.md`,
or any production/test file.

`I4` remains aggregate-only and docs-only. It records exactly one bounded
next-step result token over the accepted `I3` evidence chain for the repaired
`URI-R2-C1` local-binding single-base
`rootLocalSingleBase` / `baseTarget -> baseC` / same-lane `targetC` lane:

- `continue-bounded`
- `widen-approved`
- `stop-blocked`

The stale sentence in `orchestrator/rounds/round-057/selection.md` claiming
that `BUG-2026-03-16-001` is still open is not repaired in place here and is
not reused as live decision authority. Under the accepted continuity carried
forward below, that sentence is guider-context drift only.

The inherited boundary remains unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path widening.

Any genuine contradiction in the accepted `I1` / `I2` / `I3` evidence chain or
current authoritative docs-level continuity would still be a blocker to record
through `stop-blocked`, not permission to patch code, add tests, reopen
replay, widen the live subject, or rewrite other round artifacts during this
attempt.

Reviewer outcomes for `I4` remain limited to:

- `accepted + finalize`
- `rejected + retry`

`accepted + retry` is forbidden for `I4`.

## Accepted `I1` / `I2` / `I3` Evidence Chain And Corrected Bug Authority

Only accepted authoritative evidence and the live canonical bug tracker are
carried forward here:

1. `orchestrator/rounds/round-054/review-record.json` remains the
   authoritative acceptance proof that `I1` finalized as `attempt: 1`,
   `attempt_verdict: "accepted"`, `stage_action: "finalize"`,
   `status: "authoritative"`, with canonical artifact path
   `docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md`.
2. `orchestrator/rounds/round-055/review-record.json` remains the
   authoritative acceptance proof that `I2` finalized as `attempt: 1`,
   `attempt_verdict: "accepted"`, `stage_action: "finalize"`,
   `status: "authoritative"`, with canonical artifact path
   `docs/plans/2026-03-20-uri-r2-c1-i2-bounded-implementation-slice.md`.
3. The accepted `I1` artifact still freezes exactly one bounded successor
   slice under repaired `URI-R2-C1`: the local-binding single-base
   `baseTarget -> baseC` fail-closed lane in `Fallback.hs`, together with the
   same-lane `targetC` use, with future ownership limited to
   `Fallback.hs` and `PipelineSpec.hs`.
4. The accepted `I2` artifact still records the landed bounded proof
   `rootLocalSingleBase = rootBindingIsLocalType && IntSet.size rootBoundCandidates == 1 && not rootHasMultiInst && not instArgRootMultiBase`,
   the selected `baseTarget -> baseC` route, the same-lane `targetC` use, and
   the preserved scheme-alias/base-like `baseTarget` consumer outside the
   selected lane.
5. `orchestrator/rounds/round-056/review-record.json` remains the
   authoritative acceptance proof that `I3` finalized as `attempt: 2`,
   `attempt_verdict: "accepted"`, `stage_action: "finalize"`,
   `status: "authoritative"`, with canonical artifact path
   `docs/plans/2026-03-20-uri-r2-c1-i3-bounded-verification-gate.md`.
6. The accepted `I3` checks map remains all-pass for `I3-CONTRACT`,
   `I3-ANCHORS`, `I3-RETRY-HISTORY`, `I3-FRESH-VERIFICATION`,
   `I3-DIFF-BOUNDARY`, and `I3-CONTINUITY`. The accepted `I3` artifact still
   stands as the current bounded verification baseline for this exact lane.
7. `/Volumes/src/mlf4/Bugs.md` is the live canonical bug-status authority for
   this retry. Its `## Open` section is empty and
   `BUG-2026-03-16-001` appears only under `## Resolved`, so the bug is
   resolved continuity context only rather than an open-bug premise.
8. The accepted `I3` artifact already carries that same resolved continuity:
   `/Volumes/src/mlf4/Bugs.md` is continuity context only, and the resolved bug
   does not authorize replay reopen, `MLF.Elab.Inst`, `InstBot`,
   `boundVarTarget` widening, `boundTarget` overlay materialization,
   `View.hs`, `schemeBodyTarget` consolidation, non-local widening, or any
   broader recursive-inference family in this round.
9. The accepted `H4` artifact and the accepted `round-053` `H4` review already
   establish the governing rule for this situation: stale guider text about
   bug status is non-authoritative context drift only when the canonical bug
   tracker and the canonical decision artifact both track the live resolved
   state correctly.
10. Therefore the stale open-bug sentence in
    `orchestrator/rounds/round-057/selection.md` is not a blocker by itself.
    It remains out-of-scope guider drift only, while the bug itself remains
    resolved continuity context only and out of scope for `I4`.
11. Accepted continuity from `U6` and `H4` remains binding: repaired
    `URI-R2-C1` stays the live subject, the inherited explicit-only /
    non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback
    boundary remains fixed, and accepted `U2` / `U3` / `U4` negatives are not
    reinterpreted as widening clearance.

`I4` answers one bounded question only: given that accepted `I3` evidence
chain and the corrected authoritative continuity above, what single lawful
next-step result token applies to the repaired `URI-R2-C1` local single-base
`rootLocalSingleBase` / `baseTarget -> baseC` / same-lane `targetC` lane?

## Decision

Recorded result token: `continue-bounded`

## Decision Rationale

### Why `continue-bounded` Is Lawful

Rerunning the closed-rule `I4` decision over the corrected authoritative
continuity packet yields `continue-bounded`:

- the accepted `I1` / `I2` / `I3` chain remains present, authoritative, and
  internally consistent;
- the accepted `I3` artifact still stands as the current bounded verification
  baseline for the exact local single-base lane selected in `I1` and
  implemented in `I2`;
- the live canonical bug tracker and accepted `I3` / `H4` continuity all
  agree that `BUG-2026-03-16-001` is resolved continuity context only; and
- the accepted `H4` review rule already establishes that the stale guider
  sentence in `selection.md` does not become a blocker when the canonical
  decision artifact uses the live resolved bug state correctly.

Because no genuine blocker remains after using the correct authorities and no
already-accepted widening authority exists, the lawful aggregate result is to
continue under the existing bounded discipline rather than stop or widen.

### Why `stop-blocked` Is Not Lawful

`stop-blocked` is no longer lawful because the rejected `attempt-1` blocker
depended on the wrong authority source:

- no required accepted artifact is missing;
- the `I1`, `I2`, and `I3` review records remain authoritative rather than
  contradictory;
- the live canonical bug tracker does not carry an open `BUG-2026-03-16-001`;
  and
- the stale open-bug sentence in `selection.md` is guider drift only under the
  accepted `H4` review rule, not a live blocker.

The packet therefore no longer contains a genuine blocker that would justify
recording `stop-blocked`.

### Why `widen-approved` Is Not Lawful

`widen-approved` remains unlawful because no already-accepted evidence changes
the live subject or inherited boundary:

- there is no accepted roadmap amendment authorizing widening beyond the
  repaired `URI-R2-C1` local single-base lane;
- there is no accepted artifact clearing the still-binding `U2` / `U3` / `U4`
  negative findings as widening blockers for this slice; and
- accepted continuity still leaves replay reopen, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget`, `boundTarget` overlay materialization, `View.hs`,
  `schemeBodyTarget`, non-local widening, and broader recursive-inference work
  out of scope.

## Continuity Preserved

This decision preserves the exact accepted bounded lane and its exclusions:

- the live subject remains repaired `URI-R2-C1`;
- the selected lane remains the local-binding single-base
  `rootLocalSingleBase` / `baseTarget -> baseC` / same-lane `targetC` slice
  only;
- `Fallback.hs` and `PipelineSpec.hs` remain the accepted read-only ownership
  anchors for the verified lane;
- the already accepted scheme-alias/base-like `baseTarget` route remains
  preserved outside the selected lane;
- replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
  `boundTarget` overlay materialization, `View.hs`, `schemeBodyTarget`,
  non-local widening, and every broader trigger family remain inherited
  context only and out of scope; and
- the inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary remains unchanged.

## Verification And Continuity Notes

Docs/state continuity checks executed for this `I4` retry attempt:

- `git diff --check` -> pass
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass
- `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
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
- `test -f docs/plans/2026-03-20-uri-r2-c1-i3-bounded-verification-gate.md`
  -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
  -> pass
- `test -f orchestrator/rounds/round-053/reviews/attempt-1.md` -> pass
- `test -f /Volumes/src/mlf4/Bugs.md` -> pass
- `python3 -m json.tool orchestrator/rounds/round-054/review-record.json >/dev/null`
  -> pass
- `python3 -m json.tool orchestrator/rounds/round-055/review-record.json >/dev/null`
  -> pass
- `python3 -m json.tool orchestrator/rounds/round-056/review-record.json >/dev/null`
  -> pass
- short `python3` assertion over the `I1` / `I2` / `I3` review records
  -> pass:
  - confirmed expected `stage_id`, `accepted` / `finalize` / `authoritative`
    status, and canonical artifact paths for `I1`, `I2`, and `I3`;
  - confirmed the required all-pass `I3-*` checks map
- `rg -n 'BUG-2026-03-16-001|resolved|continuity context only' /Volumes/src/mlf4/Bugs.md docs/plans/2026-03-20-uri-r2-c1-i3-bounded-verification-gate.md docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md orchestrator/rounds/round-053/reviews/attempt-1.md docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
  -> pass
- `rg -n 'BUG-2026-03-16-001|open' orchestrator/rounds/round-057/selection.md`
  -> pass:
  - the stale guider sentence still exists there, but it is not treated as
    canonical bug-status authority in this artifact
- `python3` token/rationale assertion over
  `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
  -> pass:
  - exactly one decision-token line exists; and
  - the rationale does not derive the result from a stale packet-local
    open-bug contradiction

Skip note for full code-path verification:

- `cabal build all && cabal test` was intentionally not rerun in `I4` because
  this round remains aggregate-only and docs-only and does not authorize edits
  in `src/`, `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
- The accepted `I3` artifact already carries the fresh focused `ARI-C1` rerun
  plus the fresh full repo gate for the exact selected lane, so rerunning the
  full code-path gate here would exceed the bounded `I4` contract rather than
  strengthen it.

Post-edit docs-only diff checks:

- `git status --short --untracked-files=all` -> bounded final diff:
  - ` M orchestrator/state.json`
  - `?? docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
  - `?? orchestrator/rounds/round-057/attempt-log.jsonl`
  - `?? orchestrator/rounds/round-057/plan.md`
  - `?? orchestrator/rounds/round-057/review.md`
  - `?? orchestrator/rounds/round-057/reviews/attempt-1.md`
  - `?? orchestrator/rounds/round-057/selection.md`
- `git diff --name-only` -> existing tracked controller diff only:
  - `orchestrator/state.json`
- `git diff --name-only -- src src-public app test mlf2.cabal` -> no output
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> no output

Those final diff checks confirm this implement-stage retry stayed within the
expected docs/orchestrator boundary. The only implement-stage write is this
refreshed canonical `I4` artifact; the tracked `orchestrator/state.json` diff
and the untracked round packet files pre-existed this attempt and were left
untouched, and no diff was introduced in `src/`, `src-public/`, `app/`,
`test/`, `mlf2.cabal`, `orchestrator/roadmap.md`, or `/Volumes/src/mlf4/Bugs.md`.

## Non-Authorization

This `continue-bounded` `I4` decision does not authorize:

- reopening `I1`, `I2`, or `I3`;
- editing `selection.md`, `plan.md`, `review.md`, `orchestrator/state.json`,
  `orchestrator/roadmap.md`, or `/Volumes/src/mlf4/Bugs.md`;
- replay reopen or any `MLF.Elab.Inst` / `InstBot` work;
- `boundVarTarget`, `boundTarget`, `View.hs`, or `schemeBodyTarget` as new
  target families;
- non-local widening, cross-family widening, or broader recursive-inference
  work;
- equi-recursive reasoning, implicit unfolding, or cyclic structural graph
  encoding; or
- any second interface, compatibility shim, convenience fallback, or
  default-path widening.

Any future successor work after this decision would still require a fresh
bounded exact-target bind under a later accepted cycle. This `continue-bounded`
result may not treat the accepted `I3` local single-base verification as
already clearing replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
`boundTarget`, non-local widening, or any other trigger-family widening.
