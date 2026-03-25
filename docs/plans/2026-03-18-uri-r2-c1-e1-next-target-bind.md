# `E1` Next-Target Bind For Repaired `URI-R2-C1`

Date: 2026-03-18
Round: `round-038`
Roadmap item: `E1`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null`
Live subject: repaired `URI-R2-C1`
Artifact kind: docs-only bind/selection freeze

## Stage Contract Freeze

This artifact implements only roadmap item `E1` for `attempt-1` with `retry: null`.

It carries forward the accepted `C4 = continue-bounded` result and freezes one more
bounded next slice only. `E1` is a bind/selection stage only. It does not perform
production edits, test edits, roadmap mutation, controller-state edits, bug-tracker
updates, replay reopen, or subject widening.

The inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding.

This artifact therefore does not clear authority, uniqueness, owner stability, or
constructor admissibility, and it does not authorize broad automatic recursive
inference beyond repaired `URI-R2-C1`.

## Accepted `C1` / `C2` / `C3` / `C4` Evidence Chain Carried Forward Without Widening

Only accepted bounded predecessor facts are carried forward here:

1. `C1` bound the refreshed cycle to repaired `URI-R2-C1`, preserved the inherited
   explicit-only / non-equi-recursive / non-cyclic-graph boundary, kept accepted
   `U2` / `U3` / `U4` negative findings binding, and froze exactly one bounded `C2`
   target: the local-binding-only `rootBindingIsLocalType` fail-closed retention
   slice in `Fallback.hs` with focused `PipelineSpec.hs` coverage.
2. `C2` authoritative `attempt-2` landed only that bounded local-binding-only
   fail-closed retention slice in
   `src/MLF/Elab/Run/ResultType/Fallback.hs`
   and
   `test/PipelineSpec.hs`.
   It did not reopen replay repair, `MLF.Elab.Inst`, or any broader trigger family.
3. `C3` authoritative `attempt-1` reverified that same accepted `C2` slice with the
   focused `ARI-C1 feasibility characterization (bounded prototype-only)` block plus
   a fresh full repo gate, so the current accepted baseline remains the
   local-binding-only fail-closed retention slice only.
4. `C4` authoritative `attempt-1` recorded `continue-bounded`, not
   `widen-approved` and not `stop-blocked`. The only lawful successor action is one
   more bounded non-widening cycle from the accepted `C2` / `C3` baseline.
5. Accepted negative findings remain binding: `U2` stays `authority-narrowed`, `U3`
   stays `uniqueness-owner-stable-refuted`, and `U4` stays
   `constructor-acyclic-termination-refuted`. `E1` does not reinterpret any of them
   as implementation clearance.
6. `Bugs.md` remains repaired-lane continuity context only. Open replay-path bug
   `BUG-2026-03-16-001` does not authorize replay reopen,
   `MLF.Elab.Inst.applyInstantiation`, `InstBot`, or broader recursive-inference
   work in this round.

## Exact Selected `E2` Target (Exactly One)

The only frozen future `E2` target is:

local-binding-only `boundVarTarget` / nested-`forall` fail-closed retained-child
hardening in
`src/MLF/Elab/Run/ResultType/Fallback.hs`
limited to the retained-child branch around `boundHasForallFrom`, `boundVarTarget`,
`keepTargetFinal`, and `targetC` (`Fallback.hs:530-674`).

Required interpretation of that one bounded slice:

- start from the accepted `C2` / `C3` baseline: `rootBindingIsLocalType` remains the
  mandatory gate before any retained-target behavior is considered;
- keep the selected work confined to the retained-child lane only, where
  `boundVarTarget` may offer a child-derived retained candidate for the already
  accepted local-binding root;
- keep that child-derived retained target only when the retained child stays inside
  the same local `TypeRef` lane and remains free of nested `forall` /
  nested scheme-root ownership as detected by `boundHasForallFrom`;
- if the child-derived candidate crosses a nested `forall` boundary, crosses a
  nested scheme-root owner boundary, or cannot stay inside the same local `TypeRef`
  lane, the repaired `URI-R2-C1` lane must stay fail-closed and fall back to the
  already-accepted non-retained behavior;
- no other `keepTargetFinal` trigger family is selected by this artifact.

### Future File Ownership

Future `E2` ownership is frozen to exactly these files only:

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`

### Future Focused Coverage Intent

Future `E2` coverage remains one bounded extension of the existing
`ARI-C1 feasibility characterization (bounded prototype-only)` block in
`test/PipelineSpec.hs`
only:

- add one local-binding retained-child case that directly exercises the selected
  `boundVarTarget` lane;
- add one matched fail-closed contrast where that same retained-child lane crosses a
  nested `forall` / nested scheme-root boundary and therefore must still reject.

No second implementation family is selected in this artifact.

## Explicit Non-Selection And Non-Authorization

This `E1` bind does not authorize:

- edits to `src/MLF/Elab/Inst.hs`;
- replay-repair reopen, including `MLF.Elab.Inst.applyInstantiation` / `InstBot`, as
  active `E2` work;
- `rootHasMultiInst`, `instArgRootMultiBase`, or
  `rootIsSchemeAlias && rootBoundIsBaseLike` as separate `E2` target families in
  this cycle;
- prototype/research entrypoints or a second executable interface;
- equi-recursive reasoning or implicit unfolding;
- cyclic structural graph encoding;
- multi-SCC or cross-family widening;
- heuristic ownership ranking or manufactured authority;
- compatibility shims, convenience fallbacks, or default-path widening;
- roadmap, controller-state, or bug-tracker edits as part of this bind.

Any broader target family requires a separate accepted roadmap change before
implementation.

## Retry-Aware Reviewer Handoff

This artifact is the immutable `attempt-1` bind/selection record for `E1`.
If `round-038` retries later, future attempts must be additive and must not rewrite
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
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-038`

### Baseline Commands

- `git diff --check` -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null` -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md` -> pass:
  - `38:1. [done] Execute the `C1` continue-bounded bind and exact next-slice target selection for the still-bound live subject`
  - `42:2. [done] Execute the `C2` bounded fail-closed local-binding-only result-type target-retention hardening slice frozen by `C1``
  - `46:3. [done] Execute the `C3` bounded verification and evidence consolidation gate for the accepted local-binding-only fail-closed retention slice`
  - `50:4. [done] Execute the bounded `C4` next-cycle decision gate for the verified repaired `URI-R2-C1` local-binding-only fail-closed slice`
  - `54:5. [pending] Execute the `E1` continue-bounded bind and exact next-slice target selection for repaired `URI-R2-C1` after the accepted local-binding-only fail-closed retention baseline`
  - `58:6. [pending] Execute the `E2` bounded fail-closed implementation slice frozen by `E1``
  - `62:7. [pending] Execute the `E3` bounded verification and evidence consolidation gate for the accepted `E2` slice`
  - `66:8. [pending] Execute the bounded `E4` next-cycle decision gate for the verified `E1`-frozen repaired `URI-R2-C1` slice`
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

### `E1`-Specific Checks

- `rg -c '^The only frozen future `E2` target is:$' docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`
  -> pass:
  - `1`
- `awk 'NR<30 {print}' docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md | rg -n 'Round: `round-038`|Roadmap item: `E1`|Stage: `implement`|Attempt: `attempt-1`|Retry state: `null`|Live subject: repaired `URI-R2-C1`|explicit-only recursive baseline|non-equi-recursive semantics|non-cyclic structural graph encoding'`
  -> pass:
  - `4:Round: `round-038``
  - `5:Roadmap item: `E1``
  - `6:Stage: `implement``
  - `7:Attempt: `attempt-1``
  - `8:Retry state: `null``
  - `9:Live subject: repaired `URI-R2-C1``
  - `23:- explicit-only recursive baseline;`
  - `24:- non-equi-recursive semantics;`
  - `25:- non-cyclic structural graph encoding.`
- `sed -n '62,107p' docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md | rg -n 'The only frozen future `E2` target is:|boundVarTarget|nested-`forall`|boundHasForallFrom|keepTargetFinal|targetC|Fallback\\.hs:530-674|same local `TypeRef` lane|Future `E2` ownership is frozen|/src/MLF/Elab/Run/ResultType/Fallback.hs|/test/PipelineSpec.hs|No second implementation family is selected'`
  -> pass:
  - `3:The only frozen future `E2` target is:`
  - `5:local-binding-only `boundVarTarget` / nested-`forall` fail-closed retained-child`
  - `7:`src/MLF/Elab/Run/ResultType/Fallback.hs``
  - `8:limited to the retained-child branch around `boundHasForallFrom`, `boundVarTarget`,`
  - `9:`keepTargetFinal`, and `targetC` (`Fallback.hs:530-674`).`
  - `16:  `boundVarTarget` may offer a child-derived retained candidate for the already`
  - `19:  the same local `TypeRef` lane and remains free of nested `forall` /`
  - `20:  nested scheme-root ownership as detected by `boundHasForallFrom`;`
  - `25:- no other `keepTargetFinal` trigger family is selected by this artifact.`
  - `29:Future `E2` ownership is frozen to exactly these files only:`
  - `31:- `src/MLF/Elab/Run/ResultType/Fallback.hs``
  - `32:- `test/PipelineSpec.hs``
  - `46:No second implementation family is selected in this artifact.`
- `sed -n '53,60p;111,115p' docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md | rg -n '`authority-narrowed`|`uniqueness-owner-stable-refuted`|`constructor-acyclic-termination-refuted`|does not reinterpret any of them as implementation clearance|does not authorize replay reopen|MLF\\.Elab\\.Inst\\.applyInstantiation|InstBot'`
  -> pass:
  - `1:5. Accepted negative findings remain binding: `U2` stays `authority-narrowed`, `U3``
  - `2:   stays `uniqueness-owner-stable-refuted`, and `U4` stays`
  - `3:   `constructor-acyclic-termination-refuted`. `E1` does not reinterpret any of them`
  - `6:   `BUG-2026-03-16-001` does not authorize replay reopen,`
  - `7:   `MLF.Elab.Inst.applyInstantiation`, `InstBot`, or broader recursive-inference`
  - `12:- replay-repair reopen, including `MLF.Elab.Inst.applyInstantiation` / `InstBot`, as`
- `sed -n '116,125p' docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md | rg -n 'rootHasMultiInst|instArgRootMultiBase|rootIsSchemeAlias && rootBoundIsBaseLike|second executable interface|equi-recursive reasoning|cyclic structural graph encoding|cross-family widening|compatibility shims|default-path widening|roadmap, controller-state, or bug-tracker edits'`
  -> pass:
  - `1:- `rootHasMultiInst`, `instArgRootMultiBase`, or`
  - `2:  `rootIsSchemeAlias && rootBoundIsBaseLike` as separate `E2` target families in`
  - `4:- prototype/research entrypoints or a second executable interface;`
  - `5:- equi-recursive reasoning or implicit unfolding;`
  - `6:- cyclic structural graph encoding;`
  - `7:- multi-SCC or cross-family widening;`
  - `9:- compatibility shims, convenience fallbacks, or default-path widening;`
  - `10:- roadmap, controller-state, or bug-tracker edits as part of this bind.`
- `git status --short --untracked-files=all | rg -n '^(M|A|\\?\\?) (src/|src-public/|app/|test/|mlf2\\.cabal)' || true`
  -> pass (no output)
- `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'` -> pass
  (no output)

### Full-Gate Note

`cabal build all && cabal test` was intentionally not run because this round is
docs-only and does not edit `src/`, `src-public/`, `app/`, `test/`, or
`mlf2.cabal`.
