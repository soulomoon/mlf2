# `F1` Next-Target Bind For Repaired `URI-R2-C1`

Date: 2026-03-19
Round: `round-042`
Roadmap item: `F1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only bind/selection freeze

## Stage Contract Freeze

This artifact implements only roadmap item `F1` for `attempt-1` with `retry: null`.

It carries forward the accepted `E4 = continue-bounded` result and freezes one more
bounded next slice only. `F1` is a bind/selection stage only. It does not perform
production edits, test edits, roadmap mutation, controller-state edits, bug-tracker
updates, replay reopen, or subject widening.

The inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding.

This artifact therefore does not clear authority, uniqueness, owner stability, or
constructor admissibility, and it does not authorize broad automatic recursive
inference beyond repaired `URI-R2-C1`.

## Accepted `E1` / `E2` / `E3` / `E4` Evidence Chain Carried Forward Without Widening

Only accepted bounded predecessor facts are carried forward here:

1. `E1` froze exactly one bounded `E2` target in `ResultType.Fallback` plus focused
   `PipelineSpec` coverage: the same-lane retained-child local-`TypeRef`
   `boundVarTarget` / nested-`forall` lane. `E1` explicitly excluded
   `rootHasMultiInst`, `instArgRootMultiBase`, and
   `rootIsSchemeAlias && rootBoundIsBaseLike` as separate target families for that
   cycle.
2. `E2` authoritative `attempt-2` landed only that bounded same-lane retained-child
   `boundVarTarget` / nested-`forall` fail-closed slice in
   `src/MLF/Elab/Run/ResultType/Fallback.hs`
   and
   `test/PipelineSpec.hs`.
   It did not reopen replay repair, `MLF.Elab.Inst`, or any broader trigger family.
3. `E3` authoritative `attempt-1` reverified that exact same-lane retained-child
   slice under the focused `ARI-C1 feasibility characterization (bounded prototype-only)`
   rerun, a fresh full repo gate, predecessor continuity checks, and docs-only diff
   review. The accepted baseline therefore remains that same-lane retained-child
   lane only.
4. `E4` authoritative `attempt-1` recorded `continue-bounded`, not
   `widen-approved` and not `stop-blocked`. The only lawful successor action is one
   more bounded non-widening cycle from the accepted `E2` / `E3` baseline.
5. Accepted negative findings remain binding: `U2` stays `authority-narrowed`, `U3`
   stays `uniqueness-owner-stable-refuted`, and `U4` stays
   `constructor-acyclic-termination-refuted`. `F1` does not reinterpret any of them
   as implementation clearance.
6. `/Volumes/src/mlf4/Bugs.md` remains repaired-lane continuity context only.
   Resolved replay-path bug `BUG-2026-03-16-001` does not authorize replay reopen,
   `MLF.Elab.Inst`, `MLF.Elab.Inst.applyInstantiation`, `InstBot`, or broader
   recursive-inference work in this round.

## Exact Selected `F2` Target (Exactly One)

The only frozen future `F2` target is:

local-binding scheme-alias/base-like fail-closed hardening in
`src/MLF/Elab/Run/ResultType/Fallback.hs`
limited to the already-computed
`rootIsSchemeAlias && rootBoundIsBaseLike` branch inside `keepTargetFinal` and the
downstream `targetC` selection (`Fallback.hs:521-686`).

Required interpretation of that one bounded slice:

- start from the accepted `E2` / `E3` baseline unchanged:
  `rootBindingIsLocalType`, `scopeRootPost`, `boundHasForallFrom`, `boundVarTarget`,
  and the accepted same-lane retained-child `ARI-C1` examples remain inherited
  starting conditions, not reopened target families;
- keep the selected work confined to the adjacent local-binding case where
  `keepTargetFinal` is carried solely by
  `rootIsSchemeAlias && rootBoundIsBaseLike`;
- treat `boundVarTarget` as absent for this selected slice, so the repaired lane is
  not widened back into retained-child selection work;
- keep `rootHasMultiInst` and `instArgRootMultiBase` out of scope and fail closed if
  the scheme-alias/base-like case would require either trigger family as authority;
- if the selected case would require replay reopen, `MLF.Elab.Inst`, `InstBot`,
  non-local binding, equi-recursive reasoning, cyclic structural encoding, or a
  second interface, the repaired `URI-R2-C1` lane must stay fail-closed and fall
  back to the already-accepted behavior;
- no other `keepTargetFinal` trigger family is selected by this artifact.

### Future File Ownership

Future `F2` ownership is frozen to exactly these files only:

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`

### Future Focused Coverage Intent

Future `F2` coverage remains one bounded extension of the existing
`ARI-C1 feasibility characterization (bounded prototype-only)` block in
`test/PipelineSpec.hs`
only:

- add one local-binding scheme-alias/base-like case that directly exercises the
  selected `keepTargetFinal` / `targetC` lane without reopening retained-child
  widening; and
- add one matched fail-closed contrast where the same-looking wrapper lacks the
  admissible local scheme-alias/base-like proof and therefore must still reject or
  fall back non-recursively.

No second implementation family is selected in this artifact.

## Explicit Non-Selection And Non-Authorization

This `F1` bind does not authorize:

- edits to `src/MLF/Elab/Inst.hs`;
- replay-repair reopen, including `MLF.Elab.Inst.applyInstantiation` / `InstBot`, as
  active `F2` work;
- reopening the completed `E1` / `E2` / `E3` / `E4` cycle;
- `rootHasMultiInst` or `instArgRootMultiBase` as separate `F2` target families in
  this cycle;
- prototype/research entrypoints or a second executable interface;
- equi-recursive reasoning or implicit unfolding;
- cyclic structural graph encoding;
- multi-SCC or cross-family widening;
- heuristic ownership ranking or manufactured authority;
- compatibility shims, convenience fallbacks, or default-path widening; or
- roadmap, controller-state, or bug-tracker edits as part of this bind.

Any broader target family requires a separate accepted roadmap change before
implementation.

## Retry-Aware Reviewer Handoff

This artifact is the immutable `attempt-1` bind/selection record for `F1`.
If `round-042` retries later, future attempts must be additive and must not rewrite
this artifact.

Per `orchestrator/retry-subloop.md`, reviewer outcomes supported by this artifact
remain:

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
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-042`

### Baseline Commands

- `git diff --check` -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md` -> pass:
  - `42:1. [done] Execute the `C1` continue-bounded bind and exact next-slice target selection for the still-bound live subject`
  - `46:2. [done] Execute the `C2` bounded fail-closed local-binding-only result-type target-retention hardening slice frozen by `C1``
  - `50:3. [done] Execute the `C3` bounded verification and evidence consolidation gate for the accepted local-binding-only fail-closed retention slice`
  - `54:4. [done] Execute the bounded `C4` next-cycle decision gate for the verified repaired `URI-R2-C1` local-binding-only fail-closed slice`
  - `58:5. [done] Execute the `E1` continue-bounded bind and exact next-slice target selection for repaired `URI-R2-C1` after the accepted local-binding-only fail-closed retention baseline`
  - `62:6. [done] Execute the `E2` bounded fail-closed retained-child `boundVarTarget` / nested-`forall` implementation slice frozen by `E1``
  - `66:7. [done] Execute the `E3` bounded verification and evidence consolidation gate for the accepted same-lane retained-child `E2` slice`
  - `70:8. [done] Execute the bounded `E4` next-cycle decision gate for the accepted `E3`-reverified same-lane retained-child repaired `URI-R2-C1` slice`
  - `74:9. [pending] Execute the `F1` continue-bounded bind and exact next-slice target selection for repaired `URI-R2-C1` after the accepted same-lane retained-child `E2` / `E3` baseline`
  - `78:10. [pending] Execute the `F2` bounded fail-closed implementation slice frozen by `F1``
  - `82:11. [pending] Execute the `F3` bounded verification and evidence consolidation gate for the accepted `F2` slice`
  - `86:12. [pending] Execute the bounded `F4` next-cycle decision gate for the accepted `F3`-reverified repaired `URI-R2-C1` slice`
- `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  -> pass
- `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  -> pass
- `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md` -> pass
- `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  -> pass
- `test -f orchestrator/retry-subloop.md` -> pass

### `F1`-Specific Checks

- `rg -c '^The only frozen future `F2` target is:$' docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md`
  -> pass:
  - `1`
- `awk 'NR<30 {print}' docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md | rg -n 'Round: `round-042`|Roadmap item: `F1`|Stage: `implement`|Attempt: `attempt-1`|Retry state: `null`|Live subject: repaired `URI-R2-C1`|explicit-only recursive baseline|non-equi-recursive semantics|non-cyclic structural graph encoding'`
  -> pass:
  - `4:Round: `round-042``
  - `5:Roadmap item: `F1``
  - `6:Stage: `implement``
  - `7:Attempt: `attempt-1``
  - `8:Retry state: `null``
  - `9:Live subject: repaired `URI-R2-C1``
  - `23:- explicit-only recursive baseline;`
  - `24:- non-equi-recursive semantics;`
  - `25:- non-cyclic structural graph encoding.`
- `sed -n '66,114p' docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md | rg -n 'The only frozen future `F2` target is:|rootIsSchemeAlias|rootBoundIsBaseLike|keepTargetFinal|targetC|Fallback\\.hs:521-686|boundVarTarget|Future `F2` ownership is frozen|/src/MLF/Elab/Run/ResultType/Fallback.hs|/test/PipelineSpec.hs|No second implementation family is selected'`
  -> pass:
  - `1:The only frozen future `F2` target is:`
  - `4:`src/MLF/Elab/Run/ResultType/Fallback.hs``
  - `6:`rootIsSchemeAlias && rootBoundIsBaseLike` branch inside `keepTargetFinal` and the`
  - `7:downstream `targetC` selection (`Fallback.hs:521-686`).`
  - `12:  `rootBindingIsLocalType`, `scopeRootPost`, `boundHasForallFrom`, `boundVarTarget`,`
  - `16:  `keepTargetFinal` is carried solely by`
  - `17:  `rootIsSchemeAlias && rootBoundIsBaseLike`;`
  - `18:- treat `boundVarTarget` as absent for this selected slice, so the repaired lane is`
  - `30:Future `F2` ownership is frozen to exactly these files only:`
  - `32:- `src/MLF/Elab/Run/ResultType/Fallback.hs``
  - `33:- `test/PipelineSpec.hs``
  - `49:No second implementation family is selected in this artifact.`
- `sed -n '55,132p' docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md | rg -n '`authority-narrowed`|`uniqueness-owner-stable-refuted`|`constructor-acyclic-termination-refuted`|does not reinterpret any of them as implementation clearance|replay reopen|MLF\\.Elab\\.Inst|rootHasMultiInst|instArgRootMultiBase'`
  -> pass:
  - `1:5. Accepted negative findings remain binding: `U2` stays `authority-narrowed`, `U3``
  - `2:   stays `uniqueness-owner-stable-refuted`, and `U4` stays`
  - `3:   `constructor-acyclic-termination-refuted`. `F1` does not reinterpret any of them`
  - `6:   Resolved replay-path bug `BUG-2026-03-16-001` does not authorize replay reopen,`
  - `7:   `MLF.Elab.Inst`, `MLF.Elab.Inst.applyInstantiation`, `InstBot`, or broader`
  - `31:- keep `rootHasMultiInst` and `instArgRootMultiBase` out of scope and fail closed if`
  - `33:- if the selected case would require replay reopen, `MLF.Elab.Inst`, `InstBot`,`
  - `67:- replay-repair reopen, including `MLF.Elab.Inst.applyInstantiation` / `InstBot`, as`
  - `70:- `rootHasMultiInst` or `instArgRootMultiBase` as separate `F2` target families in`

### Docs-Only Diff Evidence

This round remains docs-only. No production, test, public API, executable, Cabal,
roadmap, bug-tracker, or controller-state files were edited.

- `git status --short --untracked-files=all` -> docs/orchestrator-only untracked set:
  - `?? docs/plans/2026-03-19-uri-r2-c1-f1-next-target-bind.md`
  - `?? orchestrator/rounds/round-042/implementation-notes.md`
  - `?? orchestrator/rounds/round-042/plan.md`
  - `?? orchestrator/rounds/round-042/selection.md`
- `git diff --name-only` -> no tracked diffs
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'` ->
  no output

### Full-Gate Skip Note

`cabal build all && cabal test` was intentionally not run in `F1` `attempt-1`
because this round is docs-only and does not edit `src/`, `src-public/`, `app/`,
`test/`, or `mlf2.cabal`.
