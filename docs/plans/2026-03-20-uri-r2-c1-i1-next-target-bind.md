# `I1` Next-Target Bind For Repaired `URI-R2-C1`

Date: 2026-03-20
Round: `round-054`
Roadmap item: `I1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only bind/selection freeze

## Stage Contract Freeze

This artifact executes only roadmap item `I1` for `attempt-1`.

`I1` is a bind/selection stage only. It freezes exactly one bounded successor
slice under the accepted `H4 = continue-bounded` outcome and does not itself
authorize production edits, test edits, roadmap mutation, controller-state
edits, bug-tracker edits, review/merge bookkeeping, or subject widening.

The inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path widening.

This artifact therefore does not reinterpret accepted `U2`, `U3`, or `U4`
negative findings as clearance for replay reopen, `MLF.Elab.Inst`,
`InstBot`, `boundVarTarget`, non-local widening, or any broader recursive
inference family.

## Corrected Authority And Accepted Predecessor Continuity

Current authority comes from the accepted `H4` decision chain and the canonical
bug tracker, not from the stale open-bug sentence in
`orchestrator/rounds/round-054/selection.md`.

Accepted continuity facts carried forward without reopening prior work:

1. `orchestrator/rounds/round-053/review-record.json` is the authoritative
   acceptance proof that `H4` finalized as `attempt: 1`,
   `attempt_verdict: "accepted"`, `stage_action: "finalize"`,
   `status: "authoritative"`, with canonical artifact path
   `docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`.
2. The accepted `H4` artifact records result token `continue-bounded` and
   explicitly refuses to authorize replay reopen, `MLF.Elab.Inst` / `InstBot`,
   `boundVarTarget`, or non-local widening as automatic successors.
3. `orchestrator/rounds/round-053/review.md` already records that the stale
   bug sentence in `selection.md` is known non-authoritative context drift.
   Guider task/order ownership stays with `selection.md`, but current bug
   authority does not.
4. `/Volumes/src/mlf4/Bugs.md` is the canonical current bug tracker. Its
   `Open` section is empty, and `BUG-2026-03-16-001` appears only under
   `Resolved`. That bug is continuity context only and does not authorize
   replay reopen or any `InstBot` successor slice in `I1`.
5. The accepted `C1` / `C2` / `C3` / `C4` chain established the local
   `rootBindingIsLocalType` / `schemeBodyTarget` / non-local fail-closed
   baseline in `MLF.Elab.Run.ResultType.Fallback` and
   `test/PipelineSpec.hs`. That baseline remains inherited context only.
6. The accepted `E1` / `E2` / `E3` / `E4` chain consumed the same-lane
   retained-child `boundVarTarget` family as predecessor evidence only.
7. The accepted `F1` / `F2` / `F3` / `F4` chain consumed the local
   `rootLocalSchemeAliasBaseLike` family as predecessor evidence only.
8. The accepted `G1` / `G2` / `G3` / `G4` chain consumed the local
   `rootLocalMultiInst` family as predecessor evidence only.
9. The accepted `H1` / `H2` / `H3` / `H4` chain consumed the local
   `rootLocalInstArgMultiBase` family and preserved `baseTarget` rejection
   outside that selected lane.

Taken together, the accepted `E` / `F` / `G` / `H` cycles have already
consumed the currently accepted local `keepTargetFinal` families in
`Fallback.hs`. No still-unopened local `keepTargetFinal` family remains for
`I1` to select next. Replay reopen, non-local widening, and other forbidden
families therefore remain unlawful successors.

## Exact Selected Successor Slice

The only frozen future successor slice is:

local-binding single-base `baseTarget -> baseC` fail-closed hardening in
`src/MLF/Elab/Run/ResultType/Fallback.hs`
centered on `Fallback.hs:367-408`, together with its final target-selection
use at `Fallback.hs:681-701`.

This is the exact semantic boundary of that one slice:

- the future reviewer-auditable proof must stay local-binding-only and be
  derived from the existing ingredients already visible in `Fallback.hs`:
  `rootBindingIsLocalType`, singleton `rootBoundCandidates`,
  `not rootHasMultiInst`, and `not instArgRootMultiBase`;
- future selected behavior may harden only the one local `baseTarget` path
  that yields a single base-like target candidate on the local `TypeRef` lane;
- `targetC` may be hardened only for that same local single-base lane, where
  `baseTarget` currently wins before retained-target and `schemeBodyTarget`
  fallbacks; and
- non-local roots, replay lanes, retained-child `boundVarTarget`, completed
  `rootLocal*` families, and every multi-base or cross-family case must remain
  fail-closed or inherited context only.

### Why This Is The Lawful Next Slice

The current source/test anchors show one adjacent unselected bounded lane and
no lawful broader alternative:

- `Fallback.hs:367-408` already computes `baseTarget` from
  `rootBoundCandidates` and rejects that path whenever `rootHasMultiInst` or
  `instArgRootMultiBase` is true.
- `Fallback.hs:681-701` still checks `baseTarget` before the retained-target
  `keepTargetFinal` lane and before the inherited `schemeBodyTarget`
  fallback, so the single-base branch remains the next adjacent target-choice
  seam in the same subsystem.
- `Fallback.hs:525-534` already shows the three accepted local trigger
  families (`rootLocalMultiInst`, `rootLocalInstArgMultiBase`,
  `rootLocalSchemeAliasBaseLike`) that have been consumed by the accepted
  predecessor chain.
- The focused `ARI-C1 feasibility characterization (bounded prototype-only)`
  block in `test/PipelineSpec.hs` still names retained-child,
  scheme-alias/base-like, multi-inst, inst-arg multi-base, and non-local proxy
  coverage, but does not name `baseTarget`, `boundTarget`, or
  `rootBoundCandidates` as a selected lane.

That makes the local single-base `baseTarget -> baseC` branch the exact next
bounded successor slice. No second implementation family is selected here.

## Frozen Future Ownership

Future ownership is frozen to exactly these files only:

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`

## Frozen Future Focused Verification Intent

Future focused verification remains one bounded `ARI-C1` extension only:

- one local single-base success example that exercises the selected
  `baseTarget` lane on a local `TypeRef` root;
- one matched fail-closed contrast that keeps the same-looking wrapper
  rejected once it becomes non-local or otherwise leaves the selected
  singleton-base lane; and
- one source-guard assertion naming the selected local single-base authority
  while preserving `schemeBodyTarget`, `boundVarTarget`,
  `rootLocalSchemeAliasBaseLike`, `rootLocalMultiInst`, and
  `rootLocalInstArgMultiBase` as inherited context only.

## Explicit Non-Selection And Non-Authorization

This `I1` bind does not authorize:

- replay reopen or any `MLF.Elab.Inst` / `InstBot` work;
- `boundVarTarget` widening or non-local fallback widening;
- reopening the accepted `C`, `E`, `F`, `G`, or `H` chains as live work;
- selection of `boundTarget` overlay materialization or
  `src/MLF/Elab/Run/ResultType/View.hs`;
- `schemeBodyTarget` consolidation or reopening the accepted
  local-vs-non-local baseline;
- any second interface, compatibility shim, convenience fallback, or
  default-path widening; or
- equi-recursive reasoning, implicit unfolding, cyclic structural graph
  encoding, or cross-family widening.

Any broader successor family requires a separate accepted roadmap amendment
before implementation.

## Retry-Aware Reviewer Handoff

This artifact is the immutable `attempt-1` bind/selection record for `I1`.
If `round-054` retries later, future attempts must be additive and must not
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
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-054`

### Baseline Commands

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
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### `I1` Continuity Checks

- `test -f docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md` -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-h2-bounded-implementation-slice.md`
  -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-h3-bounded-verification-gate.md`
  -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
  -> pass
- `python3 -m json.tool orchestrator/rounds/round-053/review-record.json >/dev/null`
  -> pass
- short `python3` assertion over
  `orchestrator/rounds/round-053/review-record.json` -> pass:
  - confirmed `H4` / `attempt-1` / `accepted` / `finalize` /
    `authoritative`; and
  - confirmed canonical artifact path
    `docs/plans/2026-03-20-uri-r2-c1-h4-next-cycle-decision-gate.md`
- `rg -n 'stale guider text|non-authoritative context drift|resolved only' orchestrator/rounds/round-053/review.md`
  -> pass
- short `python3` check over `/Volumes/src/mlf4/Bugs.md` -> pass:
  - `Open` remains empty; and
  - `BUG-2026-03-16-001` remains resolved continuity context only

### Source/Test Anchor Checks

- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '367,408p'`
  -> pass:
  - confirmed the current `baseTarget` branch is still guarded by
    singleton `rootBoundCandidates`, `not rootHasMultiInst`, and
    `not instArgRootMultiBase`
- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '681,701p'`
  -> pass:
  - confirmed `targetC` still checks `baseTarget` before retained-target and
    `schemeBodyTarget` fallbacks
- `sed -n '1102,1597p' test/PipelineSpec.hs | rg -n 'keeps retained-child|scheme-alias/base-like|multi-inst|inst-arg multi-base|non-local proxy'`
  -> pass
- `sed -n '1102,1597p' test/PipelineSpec.hs | rg -n 'baseTarget|boundTarget|rootBoundCandidates'`
  -> no output

### Docs-Only Diff Evidence

- `git status --short --untracked-files=all` -> docs/orchestrator-only final
  change set plus preexisting controller packet files:
  - ` M orchestrator/state.json`
  - `?? docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md`
  - `?? orchestrator/rounds/round-054/implementation-notes.md`
  - `?? orchestrator/rounds/round-054/plan.md`
  - `?? orchestrator/rounds/round-054/selection.md`
- `git diff --name-only` -> existing tracked controller diff only:
  - `orchestrator/state.json`
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> no output

### Full-Gate Skip Note

`cabal build all && cabal test` was intentionally skipped because `I1` is
docs-only and does not edit `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal`.
