# `J1` Next-Target Bind For Repaired `URI-R2-C1`

Date: 2026-03-20
Round: `round-058`
Roadmap item: `J1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only bind/selection freeze

## Stage Contract Freeze

This artifact implements only roadmap item `J1` for `attempt-1` with
`retry: null`.

`J1` is a bind/selection stage only. It carries forward the accepted
`I4 = continue-bounded` outcome and freezes exactly one future bounded `J2`
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
`InstBot`, `boundVarTarget`, `boundTarget` overlay materialization,
`src/MLF/Elab/Run/ResultType/View.hs`, `schemeBodyTarget`, non-local
widening, or any broader recursive-inference family.

## Accepted `I1` / `I4` Continuity And Current Anchor Packet

Only accepted bounded predecessor facts and current read-only anchors are
carried forward here:

1. `orchestrator/rounds/round-057/review-record.json` remains the
   authoritative acceptance proof that `I4` finalized as `attempt: 2`,
   `attempt_verdict: "accepted"`, `stage_action: "finalize"`,
   `status: "authoritative"`, with canonical artifact path
   `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`.
2. The accepted `I4` artifact records result token `continue-bounded` and
   explicitly requires any successor work to begin with a fresh bounded exact
   bind rather than implementation, review, widening, or replay reopen.
3. The accepted `I1` artifact remains the direct target-selection precedent
   for this round: it froze exactly one bounded `I2` successor slice,
   preserved all completed predecessor families as inherited context only, and
   limited future ownership to `Fallback.hs` plus `PipelineSpec.hs`.
4. The accepted `I1` / `I2` / `I3` / `I4` chain already consumed the repaired
   `URI-R2-C1` local single-base
   `rootLocalSingleBase` / `baseTarget -> baseC` / same-lane `targetC` lane.
   In the current source anchor at `Fallback.hs:372-376`, that completed lane
   is still the first singleton `baseTarget` case and is not reopened here.
5. The accepted `F2` / `F3` scheme-alias/base-like packet remains continuity
   context only. In the current source anchor at `Fallback.hs:377-381`, the
   empty-candidate / no-inst-arg `baseTarget` branch remains the preserved
   scheme-alias/base-like `baseTarget` route outside the completed `I` lane,
   not a new `J2` target.
6. The accepted `H1` bind remains the direct earlier template for selecting
   one adjacent local family while keeping all completed families inherited
   only. `J1` follows that same bounded bind discipline and does not reopen
   `H1` or the accepted `G` / `H` chain as live work.
7. The current local-proof cluster in `Fallback.hs:525-539` names
   `rootLocalMultiInst`, `rootLocalInstArgMultiBase`,
   `rootLocalSchemeAliasBaseLike`, and `rootLocalSingleBase`, but no dedicated
   local inst-arg-only singleton-base proof yet.
8. The current `targetC` ordering in `Fallback.hs:686-710` consumes
   `baseTarget` only for the completed `rootLocalSingleBase` lane and the
   preserved scheme-alias/base-like route, while the retained-target lane
   remains driven by `rootLocalSchemeAliasBaseLike`, `rootLocalMultiInst`,
   `rootLocalInstArgMultiBase`, and `boundVarTarget`.
9. The focused helper cluster in `PipelineSpec.hs:1243-1373` currently defines
   `schemeAliasBaseLikeFallback`, `localMultiInstFallback`,
   `localInstArgMultiBaseFallback`, and `localSingleBaseFallback`, but no
   helper isolates the local inst-arg-only singleton-base lane.
10. The focused examples and source-guard block in `PipelineSpec.hs:1518-1607`
    still cover the preserved scheme-alias/base-like route, the completed
    local single-base lane, local multi-inst, local inst-arg multi-base, and
    the current `rootLocalSingleBase` source guard, but no focused example or
    guard names an inst-arg-only singleton-base `baseTarget` / `targetC`
    authority yet.
11. `/Volumes/src/mlf4/Bugs.md` remains continuity context only. Its `Open`
    section is empty, so current bug state does not authorize replay reopen,
    `InstBot`, or any broader successor family here.

Taken together, the accepted `I` chain has already consumed the local
single-base `baseTarget` lane, the accepted `F` chain has already preserved
the empty-candidate scheme-alias/base-like `baseTarget` route, and the
current focused helper/source-guard anchors show one adjacent unselected local
inst-arg-only singleton-base family still lacking a dedicated bounded proof.

## Exact Selected `J2` Target (Exactly One)

The only frozen future `J2` target is:

local-binding inst-arg-only singleton-base `baseTarget -> baseC`
fail-closed hardening in
`src/MLF/Elab/Run/ResultType/Fallback.hs`
limited to the existing `baseTarget` branch at `Fallback.hs:382-387` and the
downstream same-lane `targetC` decision at `Fallback.hs:687-710`.

Required interpretation of that one bounded slice:

- the reviewer-auditable local proof must stay confined to a local-binding
  refinement of the existing ingredients already visible in `Fallback.hs`,
  using all of:
  `rootBindingIsLocalType`,
  `IntSet.null rootBaseBounds`,
  `IntSet.size instArgBaseBounds == 1`,
  `not rootHasMultiInst`,
  `not instArgRootMultiBase`;
- future selected behavior may harden only the one local `baseTarget` path
  where direct root-base candidates are absent but inst-argument evidence
  collapses to exactly one base-like candidate on the local `TypeRef` lane;
- future same-lane `targetC` work may refine only the ordering needed to
  consume that one selected local `baseTarget` path, without changing the
  preserved empty-candidate scheme-alias/base-like `baseTarget` route or the
  already completed `rootLocalSingleBase` lane; and
- `rootLocalSingleBase`, `rootLocalSchemeAliasBaseLike`,
  `rootLocalMultiInst`, `rootLocalInstArgMultiBase`, `boundVarTarget`,
  `boundTarget`, `schemeBodyTarget`, replay reopen, and every non-local or
  broader family remain inherited context only rather than selected `J2`
  authority.

## Why This Is The Only Lawful Next Slice

This is the only lawful next slice because the current bounded anchors expose
one adjacent unselected local `baseTarget` branch and no broader successor
authority:

- `Fallback.hs:372-376` is the completed local single-base lane already
  selected by the accepted `I` chain.
- `Fallback.hs:377-381` is the preserved empty-candidate / no-inst-arg
  scheme-alias/base-like `baseTarget` route already carried as accepted
  continuity from the `F` chain.
- `Fallback.hs:382-387` is the remaining local branch where
  `rootBaseBounds` is empty but `instArgBaseBounds` collapses to exactly one
  base-like candidate while `rootHasMultiInst` and `instArgRootMultiBase`
  remain false.
- `Fallback.hs:525-539` and `Fallback.hs:686-710` show that the surrounding
  proof/selection machinery still lacks a dedicated local inst-arg-only
  singleton-base proof and focused `targetC` consumption path, making this
  the adjacent bounded refinement point in the same subsystem.
- `PipelineSpec.hs:1243-1373` and `PipelineSpec.hs:1518-1607` show focused
  helpers/examples for scheme-alias/base-like, local single-base, local
  multi-inst, and local inst-arg multi-base only. No focused helper/example
  yet isolates the inst-arg-only singleton-base lane, so one bounded `ARI-C1`
  extension is the exact remaining evidence gap.

No replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`,
`boundTarget` overlay materialization, `View.hs`, `schemeBodyTarget`,
non-local widening, or cross-family widening is selected here.

## Future File Ownership

Future `J2` ownership is frozen to exactly these files only:

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`

## Future Focused Verification Intent

Future `J2` evidence remains one bounded extension of the existing
`ARI-C1 feasibility characterization (bounded prototype-only)` block in
`test/PipelineSpec.hs`
only:

- add one local-binding inst-arg-only singleton-base success helper/example
  that keeps the root on the local `TypeRef` lane, makes
  `instArgBaseBounds` collapse to exactly one `Int` candidate while leaving
  `rootBaseBounds` empty, and expects the concrete selected base result;
- add one matched non-local contrast using the same wrapper and same
  singleton inst-arg setup that leaves the local `TypeRef` lane and must stay
  on inherited fail-closed behavior rather than the selected `J2` lane; and
- refresh source-guard assertions only enough to name the selected local proof
  and prove that `targetC` consults the new inst-arg-only singleton
  `baseTarget` lane without changing the preserved scheme-alias/base-like
  `baseTarget` route or the completed `rootLocalSingleBase` lane.

No second implementation family is selected in this artifact.

## Explicit Non-Selection And Non-Authorization

This `J1` bind does not authorize:

- implementation, review approval, merge action, or roadmap mutation;
- any edit to `orchestrator/state.json`, `orchestrator/roadmap.md`, or
  `/Volumes/src/mlf4/Bugs.md`;
- reopening the accepted `I1` / `I2` / `I3` / `I4` chain as live work;
- reopening the preserved scheme-alias/base-like `baseTarget` route as a
  second `J2` family;
- reopening `boundVarTarget`, `boundTarget` overlay materialization,
  `src/MLF/Elab/Run/ResultType/View.hs`,
  or `schemeBodyTarget`;
- replay reopen, `MLF.Elab.Inst`, `InstBot`, or any replay-path repair as
  active `J2` work;
- non-local widening, cross-family widening, equi-recursive reasoning,
  implicit unfolding, cyclic structural graph encoding, or broader recursive
  inference beyond repaired `URI-R2-C1`; or
- any second interface, compatibility shim, convenience fallback, or
  default-path widening.

Any broader successor family requires a separate accepted roadmap change
before implementation.

## Retry-Aware Reviewer Handoff

This artifact is the immutable `attempt-1` bind/selection record for `J1`.
If `round-058` retries later, future attempts must be additive and must not
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
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-058`

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

### `J1` Exact-Target Authority Checks

- `test -f docs/plans/2026-03-20-uri-r2-c1-i1-next-target-bind.md` -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`
  -> pass
- `test -f docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md` -> pass
- `test -f docs/plans/2026-03-19-uri-r2-c1-f2-bounded-implementation-slice.md`
  -> pass
- `test -f docs/plans/2026-03-19-uri-r2-c1-f3-bounded-verification-gate.md`
  -> pass
- `python3 -m json.tool orchestrator/rounds/round-057/review-record.json >/dev/null`
  -> pass
- short `python3` assertion over
  `orchestrator/rounds/round-057/review-record.json` -> pass:
  - confirmed `stage_id = "I4"`;
  - confirmed `attempt = 2`;
  - confirmed `attempt_verdict = "accepted"`;
  - confirmed `stage_action = "finalize"`;
  - confirmed `status = "authoritative"`; and
  - confirmed canonical artifact path
    `docs/plans/2026-03-20-uri-r2-c1-i4-next-cycle-decision-gate.md`

### Source/Test Anchor Checks

- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '367,387p'`
  -> pass:
  - captured the completed local single-base branch at lines `372-376`;
  - captured the preserved empty-candidate / no-inst-arg branch at lines
    `377-381`; and
  - captured the still-unselected inst-arg-only singleton-base branch at
    lines `382-387`
- `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '525,710p'`
  -> pass:
  - confirmed the current local-proof cluster still names
    `rootLocalMultiInst`, `rootLocalInstArgMultiBase`,
    `rootLocalSchemeAliasBaseLike`, and `rootLocalSingleBase`; and
  - confirmed `targetC` still consumes only the completed
    `rootLocalSingleBase` lane and the preserved scheme-alias/base-like
    `baseTarget` route before the retained-target families
- `nl -ba test/PipelineSpec.hs | sed -n '1243,1373p'` -> pass:
  - confirmed the helper cluster currently stops at
    `localSingleBaseFallback`; and
  - confirmed no focused inst-arg-only singleton-base helper exists yet
- `nl -ba test/PipelineSpec.hs | sed -n '1518,1607p'` -> pass:
  - confirmed focused examples already cover the preserved
    scheme-alias/base-like route, the completed local single-base lane,
    local multi-inst, and local inst-arg multi-base; and
  - confirmed the current source guard still names only
    `rootLocalSingleBase` for the `baseTarget` lane

### Docs-Only Diff Evidence

- `git status --short --untracked-files=all` -> docs/orchestrator-only final
  change set plus pre-existing controller packet files:
  - ` M orchestrator/state.json`
  - `?? docs/plans/2026-03-20-uri-r2-c1-j1-next-target-bind.md`
  - `?? orchestrator/rounds/round-058/implementation-notes.md`
  - `?? orchestrator/rounds/round-058/plan.md`
  - `?? orchestrator/rounds/round-058/selection.md`
- `git diff --name-only` -> pass:
  - `orchestrator/state.json`
- `git diff --name-only -- src src-public app test mlf2.cabal` -> pass:
  - no output
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  -> pass:
  - no output

Skip note for full code-path verification:

- `cabal build all && cabal test` was intentionally not run in `J1` because
  this round is docs-only bind/selection work and does not edit `src/`,
  `src-public/`, `app/`, `test/`, or `mlf2.cabal`.
