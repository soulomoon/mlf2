# `H3` Bounded Verification Gate For Repaired `URI-R2-C1`

Date: 2026-03-20
Round: `round-052`
Roadmap item: `H3`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: repaired `URI-R2-C1`
Artifact kind: bounded reverification + evidence consolidation record

## Stage Contract Freeze

This artifact executes only roadmap item `H3` for `attempt-1`.

`H3` is a docs-only reverification gate. It re-verifies only the accepted `H2`
local-binding `rootLocalInstArgMultiBase` / `targetC -> rootFinal` lane. It
does not reopen `H1` target selection, does not reopen `H2` implementation, and
does not preempt `H4`.

The inherited boundary remains unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path fallback or widening.

This attempt therefore does not authorize production edits, test edits, replay
reopen, `MLF.Elab.Inst`, `InstBot`, non-local widening, `boundVarTarget`
widening, equi-recursive reasoning, cyclic structural encoding, cross-family
widening, or any controller / review / merge bookkeeping.

Any verification failure in `H3` would be a blocker to record, not permission
to patch
`src/MLF/Elab/Run/ResultType/Fallback.hs`,
`test/PipelineSpec.hs`,
or any other production/test file during this attempt.

## Accepted `H1` / `H2` Continuity Carried Forward Without Widening

The authoritative reviewer records still bind this round:

1. `orchestrator/rounds/round-050/review-record.json`
   confirms `H1` finalized as `attempt: 1`, `attempt_verdict: "accepted"`,
   `stage_action: "finalize"`, `status: "authoritative"`, with canonical
   artifact path
   `docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`.
2. `orchestrator/rounds/round-051/review-record.json`
   confirms `H2` finalized as `attempt: 1`, `attempt_verdict: "accepted"`,
   `stage_action: "finalize"`, `status: "authoritative"`, with canonical
   artifact path
   `docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`.

The accepted `H2` slice remains the only lane under reverification:

- `rootLocalInstArgMultiBase = rootBindingIsLocalType && instArgRootMultiBase`
  is still the reviewer-auditable proof for the selected local-binding lane;
- `keepTargetFinal` still retains the final target only behind the local gate;
- `targetC` still chooses `rootFinal` only on the accepted local trigger
  family; and
- the focused `ARI-C1 feasibility characterization (bounded prototype-only)`
  block still contains one local `instArgRootMultiBase` success example and one
  matched non-local fail-closed contrast for that exact lane.

The accepted exclusions remain binding in `H3`:

- `rootHasMultiInst` remains inherited context only;
- `rootLocalSchemeAliasBaseLike` remains inherited context only;
- `boundVarTarget` remains inherited machinery outside the selected lane;
- non-local widening remains out of scope;
- replay reopen remains out of scope;
- `MLF.Elab.Inst` and `InstBot` remain out of scope;
- accepted `U2` / `U3` / `U4` negative findings remain binding;
- equi-recursive reasoning and cyclic structural encoding remain out of scope;
- no second interface is authorized; and
- no compatibility, convenience, or fallback-path widening is authorized.

`/Volumes/src/mlf4/Bugs.md` remains continuity context only. In the current
bug tracker snapshot, the open section is empty and the replay-path
`BUG-2026-03-16-001` appears only in the resolved section, so it provides no
current authority to reopen replay work or widen this round beyond the accepted
`H2` slice.

## Read-Only Anchor Evidence

### `Fallback.hs`

Read-only anchor inspection in
`src/MLF/Elab/Run/ResultType/Fallback.hs`
shows the selected lane is still the live bounded implementation:

- `Fallback.hs:367-402` keeps `baseTarget` fail-closed for the selected family
  by requiring `not instArgRootMultiBase` in every `baseTarget` admission path,
  so multi-base collapse remains rejected outside the selected local lane.
- `Fallback.hs:525-534` still defines three local trigger families. Only
  `rootLocalInstArgMultiBase` at `Fallback.hs:528-530` is the selected `H3`
  authority. `rootLocalMultiInst` at `Fallback.hs:525-527` and
  `rootLocalSchemeAliasBaseLike` at `Fallback.hs:531-534` remain inherited
  context only.
- `Fallback.hs:625-673` keeps `boundVarTarget` as inherited retained-child
  machinery. The local branch still uses `sameLocalTypeLane child`, while the
  non-local branch still falls back to `parentRef == scopeRoot`; `H3` does not
  reinterpret that lane as selected authority.
- `Fallback.hs:674-680` keeps `keepTargetFinal` behind
  `rootBindingIsLocalType`, and the accepted selected proof remains one member
  of that gated disjunction:
  `rootLocalMultiInst || rootLocalInstArgMultiBase || rootLocalSchemeAliasBaseLike || maybe False (const True) boundVarTarget`.
- `Fallback.hs:681-701` keeps the `targetC` decision fail-closed. `rootFinal`
  is still chosen only when one of the local trigger families is present, with
  `rootLocalInstArgMultiBase` still in that exact branch, while the non-selected
  cases still fall back through `boundVarTarget`, `schemeBodyTarget`, or the
  inherited local/non-local split.

### `PipelineSpec.hs`

Read-only anchor inspection in
`test/PipelineSpec.hs`
shows the focused bounded coverage still matches the accepted `H2` slice:

- `PipelineSpec.hs:1364-1479` still keeps the retained-child same-lane baseline
  and fail-closed nested-`forall` contrast in place.
- `PipelineSpec.hs:1481-1494` still carries the local
  scheme-alias/base-like success example and matched fail-closed contrast as
  inherited context only.
- `PipelineSpec.hs:1496-1514` still carries the local multi-inst success
  example and matched non-local fail-closed contrast as inherited context only.
- `PipelineSpec.hs:1516-1524` still defines the selected positive example:
  `keeps local inst-arg multi-base fallback on the local TypeRef lane`.
- `PipelineSpec.hs:1526-1534` still defines the matched negative contrast:
  `keeps the same inst-arg multi-base wrapper fail-closed once it leaves the local TypeRef lane`.
- `PipelineSpec.hs:1553-1585` still names the selected source guard
  `rootLocalInstArgMultiBase`, the inherited context triggers
  `rootLocalMultiInst` and `rootLocalSchemeAliasBaseLike`, and the selected
  `targetC -> rootFinal` branch.
- `PipelineSpec.hs:1587-1597` still keeps the non-local proxy wrapper
  fail-closed at pipeline entrypoints beyond the selected lane.

Taken together, the read-only anchors still answer the one `H3` question in the
affirmative: the accepted `H2` local `rootLocalInstArgMultiBase` /
`targetC -> rootFinal` lane is still the live bounded implementation, while the
matched non-local contrast and out-of-scope fail-closed paths remain intact.

## Verification Notes

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-052`

### Baseline Commands

- `git diff --check` -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass:
  - `2:  "contract_version": 2,`
  - `16:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass:
  - roadmap items remain parseable;
  - item `19` (`H3`) is still `[pending]`; and
  - item `20` (`H4`) is still `[pending]`.
- `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### Focused `H3` Rerun

- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
  -> pass:
  - `15 examples, 0 failures`
  - the selected local `instArgRootMultiBase` success example stayed green;
  - the matched non-local fail-closed contrast stayed green; and
  - the source-guard example naming `rootLocalInstArgMultiBase` stayed green.

### Full Repo Gate

- `cabal build all && cabal test` -> pass:
  - executables `mlf2` and `frozen-parity-gen` built successfully;
  - `mlf2-test` passed; and
  - final test summary was `1136 examples, 0 failures`.

### Continuity Checks

- `python3 -m json.tool orchestrator/rounds/round-050/review-record.json >/dev/null`
  -> pass
- `python3 -m json.tool orchestrator/rounds/round-051/review-record.json >/dev/null`
  -> pass
- short `python3` assertion over both reviewer records -> pass:
  - `round-050/review-record.json` confirmed
    `stage_id: H1`, `attempt: 1`, `attempt_verdict: accepted`,
    `stage_action: finalize`, `status: authoritative`, and canonical artifact
    path `docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`;
  - `round-051/review-record.json` confirmed
    `stage_id: H2`, `attempt: 1`, `attempt_verdict: accepted`,
    `stage_action: finalize`, `status: authoritative`, and canonical artifact
    path `docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`.

### Docs-Only Diff Evidence

Final docs-only diff discipline checks were run after this artifact and the
paired implementation notes were written.

- post-edit `git diff --check` -> pass (no output)
- `git status --short --untracked-files=all` -> docs/orchestrator-only final
  change set plus preexisting controller packet files:
  - ` M orchestrator/state.json`
  - `?? docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
  - `?? orchestrator/rounds/round-052/implementation-notes.md`
  - `?? orchestrator/rounds/round-052/plan.md`
  - `?? orchestrator/rounds/round-052/selection.md`
- `git diff --name-only` -> existing tracked controller diff only:
  - `orchestrator/state.json`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> no output

Those final diff checks confirm `H3` stayed docs-only. The only new files from
this implementer attempt are the canonical `H3` artifact and the paired round
implementation notes. The tracked diff in `orchestrator/state.json` and the
untracked `plan.md` / `selection.md` packet files pre-existed this attempt and
were left untouched.

## Bounded Result

`H3` `attempt-1` found no blocker in the accepted `H2` lane. The exact bounded
`rootLocalInstArgMultiBase` / `targetC -> rootFinal` slice still looks stable
under:

- read-only anchor inspection in `Fallback.hs` and `PipelineSpec.hs`;
- the fresh focused `ARI-C1` rerun;
- the fresh full repo gate; and
- the predecessor continuity rechecks.

This artifact therefore records the accepted `H2` slice as re-verified and
ready for reviewer judgment. It does not authorize `H4`; that decision remains
reviewer-owned and must wait for accepted `H3` review.

## Preserved Non-Authorization

This round still does not authorize:

- any production or test repair;
- replay reopen, `MLF.Elab.Inst`, or `InstBot` work;
- `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, or `boundVarTarget` as
  separate target families;
- non-local widening, cross-family widening, or multi-SCC widening;
- equi-recursive reasoning or cyclic structural graph encoding;
- any second interface, compatibility shim, convenience fallback, or
  default-path widening; or
- controller-state edits, roadmap edits, review approval, or merge action.
