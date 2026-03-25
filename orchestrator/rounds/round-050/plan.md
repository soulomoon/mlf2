# Round 050 Plan (`H1` Continue-Bounded Next-Target Bind)

## Objective

Execute only roadmap item `H1` and prepare one accepted docs-only bind/selection
artifact at:
`docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`.

This is the initial `H1` plan for `attempt-1` with `retry: null`. The round
must bind the queued next bounded cycle to repaired `URI-R2-C1` under the
accepted `G4` result `continue-bounded`, preserve the inherited
`explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
boundary, and freeze exactly one bounded next-slice target for `H2`.

The selected `H2` target is:

- one remaining local-binding `instArgRootMultiBase` fail-closed hardening slice
  in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`,
  centered on the already-computed inst-argument multi-base aggregation at
  `Fallback.hs:289-359` and the downstream local `keepTargetFinal` / `targetC`
  selection at `Fallback.hs:671-697`;
- focused bounded coverage in
  `test/PipelineSpec.hs`,
  extending the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block around
  the current local multi-inst and source-guard helpers;
- exact semantic intent: admit retained final-target selection only when the
  selected lane is carried by the local-binding `rootBindingIsLocalType` gate
  together with the remaining `instArgRootMultiBase` trigger family, while
  keeping `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, `boundVarTarget`,
  non-local binding, and every broader widening lane as inherited context only
  and fail-closed for this cycle; and
- no reinterpretation of accepted `U2` / `U3` / `U4` negative findings as
  authority, uniqueness, or constructor-feasibility clearance.

`H1` is a docs-only bind/selection round. It does not land the `H2` slice, and
it does not authorize replay reopen, `MLF.Elab.Inst`, `InstBot`, roadmap or
controller-state mutation, `Bugs.md` edits, or any widening beyond repaired
`URI-R2-C1`.

## Locked Round Context

- Round id: `round-050`
- Roadmap item: `H1`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary:
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
- Stage mode: docs-only bind/selection round; no production or test edits in
  `H1`

Accepted carry-forward facts that must remain unchanged throughout `H1`:

- `E2` / `E3` remain completed same-lane retained-child baseline evidence only.
  They are inherited context, not reopened target-selection authority.
- `F2` / `F3` / `F4` remain the accepted local scheme-alias/base-like
  predecessor chain. That lane stays inherited context only.
- `G1` froze exactly one bounded `G2` target, the local-binding
  `rootHasMultiInst` `keepTargetFinal` / `targetC` lane, while leaving
  `instArgRootMultiBase` explicitly unselected.
- `G2` landed only the local `rootLocalMultiInst = rootBindingIsLocalType && rootHasMultiInst`
  proof and corresponding `targetC -> rootFinal` behavior, with one focused
  positive example and one matched non-local fail-closed contrast.
- `G3` reverified only that exact `G2` lane under read-only anchors, a fresh
  focused `ARI-C1` rerun, a fresh full repo gate, and predecessor continuity.
- `G4` finalized `continue-bounded`, not `widen-approved` and not
  `stop-blocked`, so one more bounded non-widening cycle is the only lawful
  successor action.
- Accepted negative findings remain binding:
  `U2 = authority-narrowed`,
  `U3 = uniqueness-owner-stable-refuted`,
  `U4 = constructor-acyclic-termination-refuted`.
- `/Volumes/src/mlf4/Bugs.md` remains continuity context only and does not
  authorize replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget`
  widening, non-local widening, or broad automatic recursive inference here.

Current repository state is already non-pristine (`orchestrator/rounds/round-050/state-snapshot.json`
modified and `orchestrator/rounds/round-050/selection.md` untracked). Respect
those existing changes. Do not revert or "clean up" unrelated work while
preparing the `H1` bind.

## Authoritative Inputs To Preserve

- `AGENTS.md`
- `orchestrator/roles/planner.md`
- `orchestrator/rounds/round-050/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-017/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-017/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-017/roadmap.md`
- `orchestrator/rounds/round-049/review-record.json`
- `docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md`
- `docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md`
- `docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`
- `docs/plans/2026-03-19-uri-r2-c1-g4-next-cycle-decision-gate.md`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `/Volumes/src/mlf4/Bugs.md`

## Files Expected In Scope

Primary writable artifact for the implement stage:

1. `docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
   - canonical `H1` bind/selection record.

Optional bounded note file:

1. `orchestrator/rounds/round-050/implementation-notes.md`
   - optional only if the implementer needs a transcript / reviewer note file.

Read-only target anchors for freezing the `H2` slice:

1. `src/MLF/Elab/Run/ResultType/Fallback.hs`
2. `test/PipelineSpec.hs`
3. `docs/plans/2026-03-19-uri-r2-c1-g1-next-target-bind.md`
4. `docs/plans/2026-03-19-uri-r2-c1-g2-bounded-implementation-slice.md`
5. `docs/plans/2026-03-19-uri-r2-c1-g3-bounded-verification-gate.md`
6. `docs/plans/2026-03-19-uri-r2-c1-g4-next-cycle-decision-gate.md`

Files that must remain untouched by `H1` `attempt-1`:

- `orchestrator/rounds/round-050/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-017/roadmap.md`
- `orchestrator/rounds/round-050/selection.md`
- `/Volumes/src/mlf4/Bugs.md`
- production and test surfaces under
  `src/`,
  `src-public/`,
  `app/`,
  `test/`, and
  `mlf2.cabal`
- `src/MLF/Elab/Inst.hs`
- prior round artifacts under
  `orchestrator/rounds/round-001/`
  through
  `orchestrator/rounds/round-049/`

If the implementer cannot keep the selected target confined to the one
`Fallback.hs` `instArgRootMultiBase` family plus focused `PipelineSpec`
coverage, stop and hand the issue back to review rather than broadening `H2`.

## Sequential Tasks

### Task 1 - Freeze the `H1` `attempt-1` contract as a docs-only bind

- In the canonical `H1` artifact, state explicitly:
  - `Round: round-050`
  - `Roadmap item: H1`
  - `Stage: implement`
  - `Attempt: attempt-1`
  - `Retry state: null`
  - `Live subject: repaired URI-R2-C1`
- Reassert the inherited boundary unchanged:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding;
  - no second executable interface; and
  - no compatibility, convenience, or default-path widening.
- State that `H1` is a bind/selection stage only.
- State that `H1` does not itself authorize production edits, test edits,
  replay reopen, roadmap mutation, controller-state edits, bug-tracker edits,
  or subject widening.

### Task 2 - Restate the controlling accepted evidence chain without reopening it

- Carry forward only the bounded predecessor facts needed for `H1`:
  - accepted `G2` / `G3` establish the current local `rootLocalMultiInst` /
    `targetC -> rootFinal` baseline;
  - accepted `G4` records `continue-bounded` and therefore authorizes only one
    more bounded non-widening cycle;
  - accepted `G1` proves `instArgRootMultiBase` was the one remaining
    explicitly unselected local-binding family after `rootHasMultiInst`;
  - accepted `U2`, `U3`, and `U4` remain binding negative findings; and
  - `Bugs.md` remains repaired-lane continuity context only.
- State explicitly that `H1` starts from the accepted `G2` / `G3` local
  multi-inst baseline but does not reopen `G1`, `G2`, `G3`, or `G4` as live
  work.
- State explicitly that `H1` does not treat any accepted negative finding as
  new implementation clearance and does not reopen replay repair,
  `MLF.Elab.Inst`, or `InstBot`.

### Task 3 - Freeze exactly one bounded `H2` target and reject all broader alternatives

- In the `H1` artifact, record exactly one next-slice target:
  - tighten only the local-binding `instArgRootMultiBase` family in
    `Fallback.hs`, centered on the aggregation and selection path at
    `Fallback.hs:289-359` and `Fallback.hs:671-697`;
  - treat the future implementation intent as one reviewer-auditable local
    proof on the shape
    `rootBindingIsLocalType && instArgRootMultiBase`, carried only through the
    selected `keepTargetFinal` / `targetC` lane;
  - keep `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, and
    `boundVarTarget` as inherited context only rather than selected authority
    for this cycle; and
  - keep the repaired `URI-R2-C1` lane fail-closed whenever the selected case
    would require non-local binding, replay reopen, `MLF.Elab.Inst`, `InstBot`,
    equi-recursive reasoning, cyclic structural encoding, a second interface,
    or any broader widening family.
- Freeze future `H2` ownership to:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`
- Freeze future focused test intent to one bounded extension of the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block:
  - add one local-binding multi-base success example that exercises the
    selected `instArgRootMultiBase` lane while the root stays on the accepted
    local `TypeRef` lane;
  - add one matched fail-closed non-local contrast where the same-looking
    wrapper leaves the local lane and must still reject or fall back
    non-recursively; and
  - refresh the source-guard assertions only as needed to name the selected
    `instArgRootMultiBase` authority without reauthorizing other trigger
    families.
- State explicitly that the selected target does **not** authorize:
  - edits to `src/MLF/Elab/Inst.hs`;
  - replay-repair reopen, prototype/research entrypoints, or a second
    executable interface;
  - `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, or `boundVarTarget` as
    separate `H2` target families in this cycle;
  - non-local widening, equi-recursive reasoning, implicit unfolding, cyclic
    graph encoding, multi-SCC or cross-family widening, heuristic ownership
    ranking, or compatibility / convenience / default-path widening.

### Task 4 - Author the canonical `H1` bind/selection artifact

- Write
  `docs/plans/2026-03-20-uri-r2-c1-h1-next-target-bind.md`
  with reviewer-auditable sections that include:
  - stage metadata (`Date`, `Round`, `Roadmap item`, `Stage`, `Attempt`,
    `Retry state`, `Live subject`, `Artifact kind`);
  - explicit carry-forward from accepted `G4 = continue-bounded`;
  - explicit carry-forward of accepted `G2` / `G3` as the current bounded
    local multi-inst baseline;
  - explicit carry-forward of accepted `U2` / `U3` / `U4` result tokens as
    still-binding negative evidence;
  - the exact selected `H2` target named above, with exact future file
    ownership and explicit non-selection of the other trigger families; and
  - a non-authorization section stating that `H1` does not clear authority,
    uniqueness, owner stability, constructor admissibility, replay-lane repair,
    or broad automatic recursive inference.
- Make the artifact concrete enough that `H2` cannot later widen from the
  selected `instArgRootMultiBase` hardening slice into another trigger family
  without a separate accepted roadmap change.

### Task 5 - Run and record docs-only verification for `H1`

- Run baseline checks from
  `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-017/verification.md`
  applicable to this docs-only slice:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-050/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-050/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-017/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-20-unannotated-iso-recursive-continue-bounded-h-cycle-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  - `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-017/retry-subloop.md`
- Add `H1`-specific checks proving:
  - the artifact records exactly one selected `H2` target;
  - the selected target is confined to the `instArgRootMultiBase`
    aggregation-plus-selection lane in `Fallback.hs:289-359` and
    `Fallback.hs:671-697`;
  - future ownership is frozen only to `Fallback.hs` and `PipelineSpec.hs`;
  - accepted `U2` / `U3` / `U4` result tokens are carried forward as negative
    constraints, not rewritten as clearance;
  - the artifact explicitly excludes replay reopen, `MLF.Elab.Inst`,
    `InstBot`, `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`,
    `boundVarTarget`, non-local widening, equi-recursive reasoning, cyclic
    encoding, second interfaces, and compatibility/default widening; and
  - the accepted `G1` / `G2` / `G3` / `G4` chain remains continuity context
    only rather than reopened work.
- Full Cabal gate handling:
  - do not run `cabal build all && cabal test` unless this round unexpectedly
    edits code-bearing surfaces; and
  - if docs-only scope holds, record the exact full-gate skip note in the
    canonical artifact.

### Task 6 - Prepare reviewer handoff with retry-contract completeness

- Ensure reviewer can lawfully emit one of the allowed `H1` combinations:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
- Ensure reviewer record fields are supported by the artifact and the checks:
  - `Implemented stage result`
  - `Attempt verdict`
  - `Stage action`
  - `Retry reason`
  - `Fix hypothesis`
- Preserve the immutability rule: if `H1` later retries, this `attempt-1`
  bind/selection artifact remains unchanged and later attempts are additive.

## Non-Goals

- No widening beyond repaired `URI-R2-C1`.
- No reinterpretation of accepted `U2` / `U3` / `U4` negative findings as if
  they were already cleared.
- No reopening of the accepted `E2` / `E3`, `F1` / `F2` / `F3` / `F4`, or
  `G1` / `G2` / `G3` / `G4` chain except as inherited context.
- No production-code, test, public API, executable, `mlf2.cabal`, roadmap, or
  bug-tracker edits in `H1`.
- No replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundVarTarget` widening,
  `rootHasMultiInst` reopening, or non-local widening.
- No equi-recursive semantics, implicit unfolding, cyclic structural graph
  encoding, multi-SCC widening, cross-family widening, heuristic owner
  selection, compatibility fallback, convenience shim, or default-on widening
  path.
- No preemption of `H2`, `H3`, or `H4` beyond freezing the one selected `H2`
  target.

## Reviewer Checks

Baseline checks from
`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-017/verification.md`
still apply.

Round-specific checks:

1. `plan.md` and the produced `H1` artifact explicitly name `attempt-1` with
   `retry: null`.
2. The artifact binds the new cycle to repaired `URI-R2-C1` under accepted
   `G4 = continue-bounded` and preserves the inherited
   `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
   boundary.
3. The artifact carries forward `U2 = authority-narrowed`,
   `U3 = uniqueness-owner-stable-refuted`, and
   `U4 = constructor-acyclic-termination-refuted` as still-binding negative
   evidence and does not restate them as clearance.
4. The artifact selects exactly one next bounded target: the local-binding
   `instArgRootMultiBase` `keepTargetFinal` / `targetC` lane confined to
   `Fallback.hs:289-359` and `Fallback.hs:671-697`, with future ownership
   frozen to
   `src/MLF/Elab/Run/ResultType/Fallback.hs`
   plus
   `test/PipelineSpec.hs`.
5. The artifact explicitly excludes replay reopen, `MLF.Elab.Inst`, `InstBot`,
   `rootHasMultiInst`, `rootLocalSchemeAliasBaseLike`, `boundVarTarget`,
   non-local widening, second-interface work, equi-recursive reasoning, cyclic
   encoding, cross-family widening, and compatibility/default widening from
   this cycle bind.
6. Verification evidence includes the required docs-only baseline checks, the
   `H1`-specific target-selection checks, and an explicit full-gate skip
   justification because the round stayed outside code-bearing surfaces.
