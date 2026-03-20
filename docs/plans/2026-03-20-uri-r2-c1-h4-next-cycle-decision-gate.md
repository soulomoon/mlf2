# `H4` Next-Cycle Decision Gate For Repaired `URI-R2-C1`

Date: 2026-03-20
Round: `round-053`
Roadmap item: `H4`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: repaired `URI-R2-C1`
Artifact kind: aggregate-only docs-only next-cycle decision gate

## Stage Contract Freeze

This artifact executes only roadmap item `H4` for `attempt-1`.

`H4` is aggregate-only and docs-only. It records exactly one bounded next-step
result token over the already accepted `H3` evidence chain for the repaired
`URI-R2-C1` local-binding
`rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane:

- `continue-bounded`
- `widen-approved`
- `stop-blocked`

This round does not reopen `H1` target selection, `H2` implementation, or `H3`
verification. It does not amend the roadmap, widen the live subject, rerun
focused or full-repo code-path verification, edit production or test files,
reopen replay work, update `Bugs.md`, or perform controller / review / merge
bookkeeping.

The inherited boundary remains unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path widening.

Any contradiction in the accepted `H3` evidence chain would be a blocker to
record through `stop-blocked`, not permission to patch code, add tests, reopen
replay, reinterpret prior accepted negatives, or widen the round during this
attempt.

Reviewer outcomes for `H4` remain limited to:

- `accepted + finalize`
- `rejected + retry`

`accepted + retry` is forbidden for `H4`.

## Accepted `H1` / `H2` / `H3` Evidence Chain Carried Forward Without Reopening

Only accepted authoritative evidence is carried forward here:

1. `H1` finalized in `round-050` as the authoritative bind that froze exactly
   one remaining bounded family under repaired `URI-R2-C1`: the
   local-binding `instArgRootMultiBase` `keepTargetFinal` / `targetC` slice,
   while leaving `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, and
   `boundVarTarget` as inherited context only.
2. `H2` finalized in `round-051` as the authoritative implementation of only
   that bounded lane, with the reviewer-auditable proof
   `rootLocalInstArgMultiBase = rootBindingIsLocalType && instArgRootMultiBase`,
   with `targetC -> rootFinal` admitted only for that local lane, with one
   local success example plus one matched non-local fail-closed contrast, and
   with `baseTarget` rejection preserved outside the selected lane.
3. `H3` finalized in `round-052` as authoritative `attempt-1`, with
   `stage_id = "H3"`, `attempt = 1`, `attempt_verdict = "accepted"`,
   `stage_action = "finalize"`, `status = "authoritative"`, and canonical
   artifact path
   `docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`.
4. The accepted `H3` checks map remains all-pass for `H3-CONTRACT`,
   `H3-H2-CONTINUITY`, `H3-FALLBACK-ANCHOR`, `H3-PIPELINE-ANCHOR`,
   `H3-FOCUSED-RERUN`, `H3-FULL-GATE`, `H3-CANONICAL-ARTIFACT`,
   `H3-DIFF-BOUNDARY`, and `H3-CONTINUITY`.
5. The accepted `H3` artifact still carries the same bounded lane, the
   read-only `Fallback.hs` / `PipelineSpec.hs` ownership anchor, the matched
   non-local fail-closed contrast, preserved `baseTarget` rejection outside the
   selected lane, and the inherited exclusions that keep
   `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, `boundVarTarget`,
   replay reopen, `MLF.Elab.Inst`, `InstBot`, and non-local widening out of
   scope.
6. `/Volumes/src/mlf4/Bugs.md` remains continuity context only. Its `Open`
   section is empty and `BUG-2026-03-16-001` remains resolved, so the bug
   tracker does not authorize replay reopen, `MLF.Elab.Inst` / `InstBot`,
   `boundVarTarget` widening, non-local widening, or broader recursive
   widening in this round.
7. Accepted negative findings remain binding:
   `U2 = authority-narrowed`,
   `U3 = uniqueness-owner-stable-refuted`,
   `U4 = constructor-acyclic-termination-refuted`.
   `H4` does not reinterpret any of them as widening clearance.

`H4` answers one bounded question only: given that accepted `H3` evidence
chain and the current docs-level continuity, what single lawful next-step
result token applies to the repaired `URI-R2-C1`
`rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane?

## Decision

Recorded result token: `continue-bounded`

## Decision Rationale

### Why `continue-bounded` Is Lawful

`continue-bounded` is the only lawful result because the accepted `H3` evidence
chain still stands as the current authoritative bounded verification baseline
for the same repaired `URI-R2-C1` local
`rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane:

- every required accepted artifact is present;
- `orchestrator/rounds/round-052/review-record.json` remains authoritative and
  internally consistent;
- the required `H3-*` pass checks remain recorded as pass;
- the accepted `H3` artifact still names the same bounded lane, the same
  `Fallback.hs` / `PipelineSpec.hs` anchor ownership, the matched non-local
  fail-closed contrast, and preserved `baseTarget` rejection outside the
  selected lane; and
- docs-level continuity still shows no blocker that invalidates `H3` as the
  current bounded verification baseline.

Because `H3` still stands and no already-accepted widening authority exists,
the lawful aggregate outcome is to continue under the existing bounded
discipline rather than stop or widen.

### Why `stop-blocked` Is Not Lawful

`stop-blocked` is not lawful because the blocker conditions do not match:

- no required accepted artifact is missing;
- the `round-052` review record is authoritative rather than contradictory;
- docs-level continuity checks did not fail; and
- the accepted `H3` verification can still be treated as the current bounded
  verification baseline for the same repaired `URI-R2-C1` local lane,
  including the matched non-local fail-closed contrast and preserved
  `baseTarget` rejection outside that lane.

This attempt therefore has no blocker to record.

### Why `widen-approved` Is Not Lawful

`widen-approved` is not lawful because no already-accepted evidence changes the
live subject or inherited boundary:

- there is no accepted roadmap amendment authorizing widening beyond the
  current repaired `URI-R2-C1` local lane;
- there is no accepted artifact that clears the still-binding `U2` / `U3` /
  `U4` negative findings as widening blockers for this slice; and
- accepted continuity still treats replay reopen, `MLF.Elab.Inst`, `InstBot`,
  `boundVarTarget`, non-local widening, and broader recursive-inference work as
  excluded families rather than approved successors.

Absent already-accepted widening authority, `widen-approved` cannot be
recorded.

## Continuity Preserved

This decision preserves the exact accepted bounded lane and its exclusions:

- the live subject remains repaired `URI-R2-C1`;
- the selected lane remains the local-binding
  `rootLocalInstArgMultiBase` / `targetC -> rootFinal` slice only;
- `Fallback.hs` and `PipelineSpec.hs` remain the accepted read-only ownership
  anchors for the verified lane;
- the matched non-local fail-closed contrast remains preserved;
- `baseTarget` rejection remains preserved outside the selected lane;
- `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, and `boundVarTarget`
  remain inherited context only, not newly selected target families; and
- the inherited explicit-only / non-equi-recursive / non-cyclic-graph /
  no-second-interface / no-fallback boundary remains unchanged.

## Verification And Continuity Notes

Docs/state continuity checks executed for this `H4` attempt:

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
- `test -f docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md` -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
  -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
  -> pass
- `python3 -m json.tool orchestrator/rounds/round-052/review-record.json >/dev/null`
  -> pass
- short `python3` assertion over `orchestrator/rounds/round-052/review-record.json`
  -> pass:
  - confirmed `H3` / `attempt-1` / `accepted` / `finalize` /
    `authoritative`; and
  - confirmed the canonical `H3` artifact path plus the required all-pass
    `H3-*` checks map.
- `rg -n 'rootLocalInstArgMultiBase|targetC -> rootFinal|baseTarget|rootHasMultiInst|rootLocalSchemeAliasBaseLike|boundVarTarget|non-local|Fallback\.hs|PipelineSpec\.hs' docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
  -> pass
- short `python3` continuity check over `/Volumes/src/mlf4/Bugs.md`
  -> pass:
  - `Open` remains empty; and
  - `BUG-2026-03-16-001` remains resolved continuity context only.

These checks confirm the accepted `H3` evidence chain still governs the same
bounded lane and still supports `continue-bounded` rather than `stop-blocked`
or `widen-approved`.

Post-edit docs-only diff checks:

- `git status --short --untracked-files=all` -> docs/orchestrator-only final
  change set plus preexisting controller packet files:
  - ` M orchestrator/state.json`
  - `?? docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
  - `?? orchestrator/rounds/round-053/implementation-notes.md`
  - `?? orchestrator/rounds/round-053/plan.md`
  - `?? orchestrator/rounds/round-053/selection.md`
- `git diff --name-only` -> existing tracked controller diff only:
  - `orchestrator/state.json`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> no output

Those final diff checks confirm this implementer attempt stayed docs-only. The
only new files from this attempt are the canonical `H4` artifact and the paired
round implementation notes. The tracked diff in `orchestrator/state.json` and
the untracked `plan.md` / `selection.md` packet files pre-existed this attempt
and were left untouched.

## Non-Authorization

This `H4` decision does not itself authorize:

- reopening `H1`, `H2`, or `H3`;
- replay reopen or any `MLF.Elab.Inst` / `InstBot` work;
- `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, or `boundVarTarget` as
  separate target families;
- non-local widening, cross-family widening, or broader recursive-inference
  work;
- equi-recursive reasoning, implicit unfolding, or cyclic structural graph
  encoding;
- any second interface, compatibility shim, convenience fallback, or
  default-path widening; or
- roadmap mutation, controller-state edits, review approval, or merge action.

Any future bounded successor work would require a new accepted roadmap update
and a new bounded cycle. This `continue-bounded` result may not treat the
accepted `H3` local `rootLocalInstArgMultiBase` verification as already
clearing replay reopen, `MLF.Elab.Inst` / `InstBot`, `boundVarTarget`,
non-local widening, or any other trigger-family widening.
