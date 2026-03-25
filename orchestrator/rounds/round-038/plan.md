# Round 038 Plan (`E1` Continue-Bounded Next-Target Bind)

## Objective

Execute only roadmap item `E1` and produce one accepted bind/selection artifact at:
`docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`.

This round must bind the queued next bounded cycle to repaired `URI-R2-C1` under
the accepted `C4` result `continue-bounded`, preserve the inherited
`explicit-only / non-equi-recursive / non-cyclic-graph` boundary, and freeze
exactly one bounded next-slice target for `E2`.

The selected `E2` target is:

- one more fail-closed hardening slice in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`
  focused only on the local-binding retained-target branch that derives a child
  candidate through `boundVarTarget` / `boundHasForallFrom`
  (`Fallback.hs:530-674`);
- focused bounded coverage in
  `test/PipelineSpec.hs`, extending the
  existing `ARI-C1 feasibility characterization (bounded prototype-only)` block
  around the already-accepted local-binding-only retention baseline;
- exact semantic intent: keep the retained recursive-looking target available
  only when the child-derived candidate stays inside the same local `TypeRef`
  lane and remains free of nested `forall` / nested scheme-root ownership;
  otherwise remain fail-closed;
- explicit non-selection of every other trigger family currently folded into
  `keepTargetFinal`, especially `rootHasMultiInst`, `instArgRootMultiBase`, and
  `rootIsSchemeAlias && rootBoundIsBaseLike`.

`E1` is a docs-only bind/selection round. It does not land the `E2` slice, and
it does not authorize a broader second interface, replay reopen, `MLF.Elab.Inst`
work, or any reinterpretation of accepted `U2` / `U3` / `U4` negative findings
as if they were already cleared.

## Locked Round Context

- Round id: `round-038`
- Roadmap item: `E1`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: repaired `URI-R2-C1`
- Fixed inherited boundary: `explicit-only / non-equi-recursive / non-cyclic-graph`
- Stage mode: docs-only bind/selection round; no production or test edits in `E1`

Accepted carry-forward facts that must remain unchanged throughout `E1`:

- `C1` froze exactly one bounded `C2` target in `ResultType.Fallback` plus
  focused `PipelineSpec` coverage, and did not reinterpret accepted `U2` /
  `U3` / `U4` negatives as clearance.
- `C2` finalized the local-binding-only `rootBindingIsLocalType` fail-closed
  retention hardening in `Fallback.hs`, with focused same-case coverage in
  `PipelineSpec.hs`.
- `C3` reverified that exact `C2` slice under the bounded `ARI-C1` block plus a
  fresh full repo gate.
- `C4` finalized `continue-bounded`, not `widen-approved` and not
  `stop-blocked`, so the new cycle is authorized only as one more bounded
  non-widening cycle.
- `U2` remains `authority-narrowed`; provenance-stable unannotated authority is
  still not cleared.
- `U3` remains `uniqueness-owner-stable-refuted`; exactly one admissible local
  owner/root cluster is still not established.
- `U4` remains `constructor-acyclic-termination-refuted`; constructor-directed
  admissibility is still fail-closed under the inherited boundary.
- `BUG-2026-03-16-001` in
  `Bugs.md` remains replay-lane
  continuity context only; it is not authority in this round to reopen replay
  repair, `MLF.Elab.Inst`, `applyInstantiation`, or `InstBot`.

Current repository state is already non-pristine
(`orchestrator/rounds/round-038/state-snapshot.json` modified, current round files untracked, task packet
files untracked). Respect those existing changes. Do not revert or "clean up"
unrelated work while preparing the `E1` bind.

## Authoritative Inputs To Preserve

- `orchestrator/rounds/round-038/selection.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-005/verification.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-005/retry-subloop.md`
- `orchestrator/roles/planner.md`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-005/roadmap.md`
- `docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
- `docs/plans/2026-03-18-uri-r2-c1-c1-continue-bounded-target-bind.md`
- `docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
- `docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`
- `docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md`
- `docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `Bugs.md`

## Files Expected In Scope

Primary writable artifact for the implement stage:

1. `docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`
   - canonical `E1` bind/selection record.

Optional bounded note file:

1. `orchestrator/rounds/round-038/implementation-notes.md`
   - optional only if the implementer needs a transcript / reviewer note file.

Read-only target anchors for freezing the `E2` slice:

1. `src/MLF/Elab/Run/ResultType/Fallback.hs`
2. `test/PipelineSpec.hs`
3. `docs/plans/2026-03-18-uri-r2-c1-c2-bounded-fail-closed-implementation-slice.md`
4. `docs/plans/2026-03-18-uri-r2-c1-c3-bounded-verification-gate.md`
5. `docs/plans/2026-03-18-uri-r2-c1-c4-next-cycle-decision-gate.md`

Files that must remain untouched by `E1` `attempt-1`:

- `orchestrator/rounds/round-038/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-005/roadmap.md`
- `orchestrator/rounds/round-038/selection.md`
- `Bugs.md`
- production and test surfaces under
  `src/`,
  `src-public/`,
  `app/`,
  `test/`, and
  `mlf2.cabal`
- `src/MLF/Elab/Inst.hs`
- prior round artifacts under
  `orchestrator/rounds/round-001/`
  through `orchestrator/rounds/round-037/`

If the implementer cannot keep the selected target confined to the one
`Fallback.hs` retained-child branch plus focused `PipelineSpec` coverage, stop
and hand the issue back to review rather than broadening `E2`.

## Sequential Tasks

### Task 1 - Freeze the `E1` `attempt-1` contract as a docs-only bind

- In the canonical `E1` artifact, state explicitly:
  - `Round: round-038`
  - `Roadmap item: E1`
  - `Stage: implement`
  - `Attempt: attempt-1`
  - `Retry state: null`
  - `Live subject: repaired URI-R2-C1`
- Reassert the inherited boundary unchanged:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding.
- State that `E1` is a bind/selection stage only.
- State that `E1` does not itself authorize production edits, test edits,
  replay reopen, roadmap mutation, bug-tracker edits, or subject widening.

### Task 2 - Restate the controlling accepted evidence chain without reopening it

- Carry forward only the bounded predecessor facts needed for `E1`:
  - accepted `C2` / `C3` establish the current local-binding-only fail-closed
    retention baseline;
  - accepted `C4` records `continue-bounded` and therefore authorizes only one
    more bounded non-widening cycle;
  - accepted `U2`, `U3`, and `U4` remain binding negative findings;
  - `Bugs.md` remains continuity-only context.
- State explicitly that `E1` starts from the accepted local-binding-only
  `rootBindingIsLocalType` retention baseline and does not treat any accepted
  negative finding as new implementation clearance.
- State explicitly that `E1` does not reopen replay repair,
  `MLF.Elab.Inst.applyInstantiation`, `InstBot`, or the completed `C1` / `C2` /
  `C3` / `C4` cycle.

### Task 3 - Freeze exactly one bounded `E2` target and reject all broader alternatives

- In the `E1` artifact, record exactly one next-slice target:
  - tighten only the local-binding retained-child path in
    `Fallback.hs:530-674`, centered on `boundHasForallFrom`,
    `boundVarTarget`, `keepTargetFinal`, and the `targetC` selection branch;
  - keep a child-derived retained target only when the root still resolves
    through the accepted local `TypeRef` lane and the child candidate stays free
    of nested `forall` / nested scheme-root ownership;
  - otherwise keep the repaired `URI-R2-C1` lane fail-closed.
- Freeze future `E2` ownership to:
  - `src/MLF/Elab/Run/ResultType/Fallback.hs`
  - `test/PipelineSpec.hs`
- Freeze future focused test intent to one bounded extension of the existing
  `ARI-C1 feasibility characterization (bounded prototype-only)` block:
  - add one local-binding retained-child case that directly exercises the
    selected `boundVarTarget` lane; and
  - add one matched fail-closed contrast where the same lane crosses a nested
    `forall` / nested scheme-root boundary and therefore must still reject.
- State explicitly that the selected target does **not** authorize:
  - edits to `src/MLF/Elab/Inst.hs`;
  - replay-repair reopen, prototype/research entrypoints, or a second
    executable interface;
  - `rootHasMultiInst`, `instArgRootMultiBase`, or `rootIsSchemeAlias` as
    separate `E2` target families in this cycle;
  - equi-recursive reasoning, implicit unfolding, cyclic graph encoding,
    multi-SCC or cross-family widening, heuristic ownership ranking, or
    compatibility / convenience / default-path widening.

### Task 4 - Author the canonical `E1` bind/selection artifact

- Write
  `docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`
  with reviewer-auditable sections that include:
  - stage metadata (`Date`, `Round`, `Roadmap item`, `Stage`, `Attempt`,
    `Retry state`, `Live subject`, `Artifact kind`);
  - explicit carry-forward from accepted `C4 = continue-bounded`;
  - explicit carry-forward of accepted `C2` / `C3` as the local-binding-only
    fail-closed retention baseline;
  - explicit carry-forward of accepted `U2` / `U3` / `U4` result tokens as
    still-binding negative evidence;
  - the exact selected `E2` target named above, with exact future file
    ownership and explicit non-selection of the other `keepTargetFinal`
    trigger families;
  - a non-authorization section stating that `E1` does not clear authority,
    uniqueness, owner stability, constructor admissibility, replay-lane repair,
    or broad automatic recursive inference.
- Make the artifact concrete enough that `E2` cannot later widen from the
  selected `boundVarTarget` hardening slice into another trigger family without
  a separate accepted roadmap change.

### Task 5 - Run and record docs-only verification for `E1`

- Run baseline checks from
  `orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-005/verification.md`
  applicable to this docs-only slice:
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-038/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-038/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-005/roadmap.md`
  - `test -f docs/superpowers/specs/2026-03-18-unannotated-iso-recursive-continue-bounded-cycle-design.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-u6-next-widening-decision-gate.md`
  - `test -f orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-005/retry-subloop.md`
- Add `E1`-specific checks proving:
  - the artifact records exactly one selected `E2` target;
  - the selected target is confined to the `boundVarTarget` /
    nested-`forall` hardening lane inside `Fallback.hs:530-674`;
  - future ownership is frozen only to `Fallback.hs` and `PipelineSpec.hs`;
  - accepted `U2` / `U3` / `U4` result tokens are carried forward as negative
    constraints, not rewritten as clearance;
  - the artifact explicitly excludes replay reopen, `MLF.Elab.Inst`,
    `rootHasMultiInst`, `instArgRootMultiBase`, `rootIsSchemeAlias`,
    equi-recursive reasoning, cyclic encoding, cross-family widening, second
    interfaces, and compatibility/default widening.
- Full Cabal gate handling:
  - do not run `cabal build all && cabal test` unless this round unexpectedly
    edits code-bearing surfaces;
  - if docs-only scope holds, record the exact full-gate skip note in the
    canonical artifact.

### Task 6 - Prepare reviewer handoff with retry-contract completeness

- Ensure reviewer can lawfully emit one of the allowed `E1` combinations:
  - `accepted + finalize`
  - `accepted + retry`
  - `rejected + retry`
- Ensure reviewer record fields are supported by the artifact and the checks:
  - `Implemented stage result`
  - `Attempt verdict`
  - `Stage action`
  - `Retry reason`
  - `Fix hypothesis`
- Preserve immutability rule: if `E1` later retries, this `attempt-1`
  bind/selection artifact remains unchanged and later attempts are additive only.

## Non-Goals

- No widening beyond repaired `URI-R2-C1`.
- No reinterpretation of accepted `U2` / `U3` / `U4` negative findings as if
  they were already cleared.
- No reopening of `BUG-2026-03-16-001`, replay repair, `MLF.Elab.Inst`,
  `applyInstantiation`, or `InstBot`.
- No production-code, test, public API, executable, `mlf2.cabal`, roadmap,
  state, or bug-tracker edits in `E1`.
- No second executable interface, prototype-only side path, compatibility shim,
  convenience fallback, or default-on widening path.
- No preemption of `E2`, `E3`, or `E4` beyond freezing the one selected `E2`
  target.
- No selection of additional `keepTargetFinal` trigger families in this cycle.

## Reviewer Checks

Baseline checks from
`orchestrator/roadmaps/2026-03-18-00-unannotated-iso-recursive-inference-continue-bounded-follow-on-roadmap/rev-005/verification.md` still
apply.

Round-specific checks:

1. `plan.md` and the produced `E1` artifact explicitly name `attempt-1` with
   `retry: null`.
2. The artifact binds the queued cycle to repaired `URI-R2-C1` under accepted
   `C4 = continue-bounded` and preserves the inherited
   `explicit-only / non-equi-recursive / non-cyclic-graph` boundary.
3. The artifact carries forward accepted `C2` / `C3` as the local-binding-only
   fail-closed retention baseline and carries forward `U2 = authority-narrowed`,
   `U3 = uniqueness-owner-stable-refuted`, and
   `U4 = constructor-acyclic-termination-refuted` as still-binding negative
   evidence.
4. The artifact selects exactly one next bounded target: local-binding-only
   `boundVarTarget` / nested-`forall` fail-closed retention hardening in
   `Fallback.hs:530-674`, with future ownership frozen to
   `src/MLF/Elab/Run/ResultType/Fallback.hs`
   plus `test/PipelineSpec.hs`.
5. The artifact explicitly excludes replay reopen, `MLF.Elab.Inst`,
   `rootHasMultiInst`, `instArgRootMultiBase`, `rootIsSchemeAlias`, prototype
   entrypoints, equi-recursive reasoning, cyclic encoding, multi-SCC or
   cross-family widening, second interfaces, and compatibility/default-path
   widening from this cycle bind.
6. Verification evidence includes the required docs-only baseline checks and an
   explicit full-gate skip justification because the round stayed outside
   code-bearing surfaces.
