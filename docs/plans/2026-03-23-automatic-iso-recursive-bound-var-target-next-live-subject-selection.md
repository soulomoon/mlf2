# `N9` Thesis-Backed Bound-Var-Target Next Live-Subject Selection For Automatic Iso-Recursive Inference

Date: 2026-03-23
Round: `round-076`
Roadmap item: `N9`
Stage: `implement`
Attempt: `attempt-1`
Retry state: `null` (`retry: null`)
Live subject: accepted `N8 = reopen-planning-only-successor-lane`
Artifact kind: canonical docs-only next-live-subject selection record

## Stage Contract Freeze

This artifact implements only roadmap item `N9` for `attempt-1` with
`retry: null`.

`N9` is docs-only next-live-subject selection work inside accepted
`N8 = reopen-planning-only-successor-lane`. It may choose exactly one
thesis-backed subject at planning scope only. It does not execute or authorize
exact target binding, implementation, verification, merge action, roadmap
edits, controller-state edits, bug-tracker edits, or predecessor-history
rewrites.

This artifact does not bind an exact target.
This artifact does not authorize implementation.
This artifact does not authorize verification.

The inherited boundary remains fixed and unchanged:

- explicit-only recursive baseline;
- non-equi-recursive semantics;
- non-cyclic structural graph encoding;
- no second executable interface; and
- no compatibility, convenience, or default-path fallback widening.

## Accepted `L1` / `L2` / `N1` Through `N8` Authority And Exact Selection Domain

Only accepted predecessor continuity is carried forward here:

1. `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
   remains the inherited baseline contract: explicit recursive annotations are
   supported, automatic recursive-type inference remains unresolved and
   disabled, and the explicit-only / non-equi-recursive /
   non-cyclic-graph / no-second-interface / no-fallback boundary remains
   mandatory.
2. `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md` remains the
   authoritative `L1` fail-closed record. Inside the repaired `URI-R2-C1`
   queue and the inherited boundary, it proved that no fresh lawful exact
   successor slice remained there.
3. `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
   remains the authoritative `L2` closeout. It finalized that repaired queue
   as closed predecessor evidence only and required separate later authority
   before any broader preserved route could become live work.
4. `docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
   remains the authoritative `N1` record. It reopened work only at planning
   scope and did not authorize implementation or verification.
5. `docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`
   remains the authoritative first reopened `N2` selection record for the
   completed earlier lane only. After accepted `N8`, that earlier subject is
   predecessor evidence only and may not be silently carried forward as the
   next post-`N8` live subject.
6. The accepted `N3` / `N4` / `N5` / `N6` / `N7` safety-contract, exact-bind,
   bounded-implementation, verification, and next-cycle records for the exact
   non-local `baseTarget -> baseC` packet remain predecessor continuity only.
   They do not supply fresh-lane clearance after accepted `N8`.
7. `docs/plans/2026-03-22-automatic-iso-recursive-post-n7-roadmap-amendment-authority-gate.md`
   remains the authoritative `N8` record. Its one bounded outcome reopened
   exactly one fresh planning-only successor lane, treated the exact non-local
   `baseTarget -> baseC` packet as predecessor evidence only, and still did
   not choose the next live subject or authorize implementation or
   verification.
8. `docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`,
   `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`,
   `docs/plans/2026-03-18-uri-r2-c1-e3-bounded-verification-gate.md`, and
   `docs/plans/2026-03-18-uri-r2-c1-e4-next-cycle-decision-gate.md` remain
   accepted predecessor evidence for the repaired-queue retained-child lane
   only. They do not automatically bind a new post-`N8` target, implementation
   slice, or verification slice.
9. `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
   keeps the long-horizon row unresolved and states that the fresh post-`N8`
   successor lane still requires its own bounded next-subject selection before
   any new target or slice can begin.
10. `Bugs.md` remains read-only predecessor context only. Open
    `BUG-2026-03-16-001` does not reopen replay, `MLF.Elab.Inst`, `InstBot`,
    or any other different live subject in this round.

The admissible `N9` domain is therefore narrow:

- choose exactly one post-`N8` planning subject only;
- treat both the prior `N2` selection and the exact non-local
  `baseTarget -> baseC` packet as predecessor evidence only;
- treat the repaired-queue retained-child `E1` / `E2` / `E3` / `E4` packet as
  predecessor evidence only; and
- keep every non-selected route blocked unless a later accepted roadmap item
  reopens it explicitly.

## Thesis Basis For The Selected Subject

The thesis-backed narrowing comes from four section anchors in
`papers/these-finale-english.txt`:

1. `3.3.3.1 Grafting` explains that transformations on graphic types must take
   the binding structure into account, including how free and bound variables
   move when a type is grafted into a larger owner structure. Repo-local
   implication: the next live subject should stay centered on retained-child
   ownership and binder-structure movement rather than on broader fallback
   search.
2. `5.1` says the `MLF` instance relation is adapted to the richer binding
   structure of graphic types. Repo-local implication: a lawful next subject
   should stay inside a binding-structure-led route rather than jumping to a
   flatter cross-family search.
3. `8.3.1 Inlining bounds` says inverse translation is lawful only when the
   binder and the binding flag can be reconstructed unambiguously. Repo-local
   implication: the next live subject should focus on the retained-child route
   where nested-`forall` and owner-boundary checks decide whether that
   reconstruction stays unambiguous.
4. `17.3 Perspectives` says recursive types together with second-order
   polymorphism are already difficult and that the main difficulty likely lies
   in recursion in the binding structure, while cyclic term-graphs were not
   explored. Repo-local implication: the next subject should stay
   binding-structure-led and planning-only, not widen into cyclic encoding,
   equi-recursive reasoning, or implicit unfolding.

These anchors do not authorize code directly. They justify only why the next
live subject should move to one bounded binder-structure route.

## Current Code/Test Predecessor Packet For The Selected Subject

The current read-only source/test anchors identify the selected planning
subject precisely:

1. `src/MLF/Elab/Run/ResultType/Fallback.hs` still computes
   `boundVarTargetRoot` from `schemeBodyTarget targetPresolutionView rootC`,
   uses `boundHasForallFrom` to reject nested-`forall` and nested-owner
   crossings, computes `boundVarTarget` from retained-child candidates, keeps
   `keepTargetFinal` gated by the local-binding lane, and lets `targetC`
   choose the retained child only inside that bounded route.
2. `src/MLF/Elab/Run/Scope.hs` still defines `schemeBodyTarget` as the
   owner-local `S'`-style selector for scheme-body translation. That makes
   `schemeBodyTarget` the neighboring owner boundary used by the retained-child
   route, not the selected `N9` subject itself.
3. `test/PipelineSpec.hs` still carries the bounded retained-child evidence:
   a same-lane local `TypeRef` success case, a source guard that retained-child
   lookup stays on that same local lane, and a nested-`forall` fail-closed
   contrast.

This packet identifies one planning subject only: the preserved retained-child /
nested-`forall` / binding-structure route centered on `boundVarTargetRoot`,
`boundHasForallFrom`, `boundVarTarget`, `keepTargetFinal`, and the final
`targetC` choice. It is not `boundTarget`, not `schemeBodyTarget`, and not the
already-completed non-local `baseTarget` packet.

The accepted repaired-queue `E1` / `E2` / `E3` / `E4` retained-child packet
remains predecessor evidence for boundedness and fail-closed behavior only. It
does not automatically authorize this fresh post-`N8` lane.

## One Bounded `N9` Selection

Authoritative `N9` selection outcome:
`boundVarTarget-planning-subject-selected`

This round selects exactly one subject only: the preserved retained-child /
nested-`forall` / binding-structure `boundVarTarget` route in
`MLF.Elab.Run.ResultType.Fallback`.

Its exact meaning is:

- the next planning artifacts may study the retained-child route centered on
  `boundVarTargetRoot`, `boundHasForallFrom`, `boundVarTarget`,
  `keepTargetFinal`, and `targetC`;
- the selection stays at planning scope only and does not bind an exact next
  target, an implementation slice, or a verification slice;
- the selected subject is the future binder-ownership / nested-owner /
  target-selection planning lane only, not implementation readiness and not
  verification readiness; and
- a later accepted safety/acceptance contract and a later accepted exact-target
  bind are still required before any design or code slice may begin on this
  fresh lane.

## Explicit Deferred Alternatives And Non-Authorization

This artifact keeps the following routes blocked or predecessor-only:

- the prior `N2` selection as still-live subject authority;
- the exact non-local `baseTarget -> baseC` packet as live work;
- automatic carry-forward of the accepted repaired-queue
  `E1` / `E2` / `E3` / `E4` retained-child packet;
- `boundTarget`;
- `schemeBodyTarget`;
- `src/MLF/Elab/Run/ResultType/View.hs`;
- replay reopen;
- `MLF.Elab.Inst` and `InstBot`;
- every other `ResultType.Fallback` family;
- every different solver or pipeline subject;
- cross-family search;
- equi-recursive reasoning;
- implicit unfolding;
- cyclic structural graph encoding;
- multi-SCC support;
- a second executable interface; and
- any compatibility, convenience, or default-path fallback widening.

This artifact also does not authorize:

- exact target binding;
- implementation;
- verification;
- merge action;
- roadmap edits;
- controller-state edits;
- bug-tracker edits; or
- predecessor-history rewrites.

## Current Docs / Continuity / Scope Check Results

Commands executed in:
`/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-076`

### Baseline Checks

- `git branch --show-current`
  -> pass:
  - `codex/round-076-n9-next-live-subject-selection`
- `git status --short --untracked-files=all`
  -> bounded docs-only payload plus existing round inputs:
  - `?? docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-next-live-subject-selection.md`
  - `?? orchestrator/rounds/round-076/plan.md`
  - `?? orchestrator/rounds/round-076/selection.md`
- `git diff --check`
  -> pass (no output)
- `python3 -m json.tool orchestrator/state.json >/dev/null`
  -> pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/state.json`
  -> pass:
  - `2:  "contract_version": 2,`
  - `13:  "retry": null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmap.md`
  -> pass; roadmap remains parseable and item `9` stays pending
- required artifact-presence checks
  -> pass for:
  `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`,
  `docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`,
  `docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`,
  `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`,
  `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`,
  `docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`,
  `docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`,
  `docs/plans/2026-03-22-automatic-iso-recursive-post-n7-roadmap-amendment-authority-gate.md`,
  `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`,
  `orchestrator/retry-subloop.md`, and
  `orchestrator/rounds/round-075/review-record.json`

### `N9`-Specific Continuity Checks

- `rg -n '"stage_id": "N8"|"attempt_verdict": "accepted"|"stage_action": "finalize"|"status": "authoritative"|"final_outcome": "reopen-planning-only-successor-lane"' /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/rounds/round-075/review-record.json`
  -> pass; accepted `N8` finalization fields present
- `rg -n 'reopen-planning-only-successor-lane|predecessor evidence only|does not choose the next live subject|does not authorize implementation|does not authorize verification' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-22-automatic-iso-recursive-post-n7-roadmap-amendment-authority-gate.md`
  -> pass; accepted `N8` still preserves planning-only successor authority and
  still blocks target binding / implementation / verification
- `rg -n 'boundVarTarget|nested-`forall`|same-lane retained-child|continue-bounded' /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md /Users/ares/.codex/worktrees/d432/mlf4/docs/plans/2026-03-18-uri-r2-c1-e4-next-cycle-decision-gate.md`
  -> pass; accepted repaired-queue retained-child packet still exists as
  predecessor evidence
- `rg -n 'boundVarTargetRoot|boundHasForallFrom|boundVarTarget =|keepTargetFinal =|Just v -> v|Nothing -> schemeBodyTarget targetPresolutionView rootC' /Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/ResultType/Fallback.hs`
  -> pass; current retained-child route anchors present
- `rg -n 'keeps retained-child fallback recursive through a same-lane local TypeRef root|keeps retained-child lookup bounded to the same local TypeRef lane|keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary' /Users/ares/.codex/worktrees/d432/mlf4/test/PipelineSpec.hs`
  -> pass; focused retained-child evidence still present
- `rg -n 'schemeBodyTarget :: PresolutionView|S.|generalizeTargetNode' /Users/ares/.codex/worktrees/d432/mlf4/src/MLF/Elab/Run/Scope.hs`
  -> pass; `schemeBodyTarget` remains the neighboring owner-local selector
- `rg -n 'Attempt: `attempt-1`|Retry state: `null`|boundVarTarget-planning-subject-selected|predecessor evidence only|does not authorize implementation|does not authorize verification|does not bind an exact target' docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-next-live-subject-selection.md`
  -> pass; critical `N9` selection constraints present
- `git diff --name-only -- src test src-public app mlf2.cabal`
  -> pass (no output)
- `git diff --name-only -- orchestrator/roadmap.md orchestrator/state.json Bugs.md`
  -> pass (no output)

## Lawful Verification Skip Note

`cabal build all && cabal test` was intentionally not run because this round is
docs-only by contract and does not edit `src/`, `src-public/`, `app/`,
`test/`, or `mlf2.cabal`.

## Reviewer Handoff

Reviewer should confirm that this artifact:

- is explicitly framed as `round-076` / `N9` / `attempt-1` / `retry: null`;
- treats accepted `L1` / `L2` / `N1` through `N8` as binding predecessor
  continuity;
- records exactly one bounded selection outcome,
  `boundVarTarget-planning-subject-selected`;
- treats both the prior `N2` selection and the exact non-local
  `baseTarget -> baseC` packet as predecessor evidence only;
- treats the accepted repaired-queue `E1` / `E2` / `E3` / `E4` retained-child
  packet as predecessor evidence only;
- grounds the selected subject in thesis-backed binding-structure constraints;
- keeps the inherited boundary unchanged;
- does not bind an exact next target or authorize implementation or
  verification; and
- keeps the round docs-only and keeps every deferred route listed above
  blocked.
