# Round 076 Plan (`N9` Thesis-Backed Next Live-Subject Selection Inside Accepted `N8`)

## Objective

Execute only roadmap item `N9` and produce one docs-only selection artifact at:
`docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-next-live-subject-selection.md`.

This is the initial `N9` plan for `attempt-1` with `retry: null`. The round
must keep `N9` docs-only, preserve accepted `L1` / `L2` / `N1` through `N8`
continuity, and choose exactly one thesis-backed next live subject inside the
accepted `N8 = reopen-planning-only-successor-lane`.

Current planning read: the only lawful `N9` selection is the preserved
retained-child / nested-`forall` / binding-structure `boundVarTarget` route,
but only as a planning subject. The intended authoritative `N9` selection
outcome is:

- `boundVarTarget-planning-subject-selected`

The thesis-backed basis for that read is:

- `3.3.3.1 Grafting`: transformations on graphic types must account for the
  binding structure of the graphs, including how free and bound variables move
  across grafted structure.
- `5.1` on the `MLF` instance relation: the inference-oriented relation is
  adapted to the richer binding structure of graphic types rather than to a
  flatter first-order shape.
- `8.3.1 Inlining bounds`: inverse translation is lawful only when the binder
  and binding flag can be reconstructed unambiguously.
- `17.3 Perspectives`: recursive types plus second-order polymorphism remain
  difficult chiefly because of recursion in the binding structure, while cyclic
  term-graphs were not the explored route.

`N9` should therefore select one binder-structure-led planning subject only:
the `boundHasForallFrom` / `boundVarTarget` / `keepTargetFinal` /
`targetC` retained-child route in `MLF.Elab.Run.ResultType.Fallback`, treated
as a future planning lane and not as an already-bound target, an implementation
slice, or a verification-ready packet.

This round must not:

- treat the accepted non-local `baseTarget -> baseC` packet or the accepted
  `N2 = baseTarget-planning-subject-selected` result as automatic carry-forward
  authorization;
- treat the accepted repaired-queue `E1` / `E2` / `E3` / `E4`
  `boundVarTarget` packet as automatic carry-forward authorization; or
- authorize exact target binding, implementation, verification, roadmap edits,
  controller-state edits, bug-tracker edits, review artifacts, merge notes, or
  code changes.

## Locked Round Context

- Round id: `round-076`
- Roadmap item: `N9`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: choose one bounded next live subject only inside the
  accepted `N8` planning-only successor lane
- Active branch: `codex/round-076-n9-next-live-subject-selection`
- Active worktree:
  `.worktrees/round-076`
- Stage mode: docs-only next-live-subject selection only
- Current round review feedback: none yet; this is a full `attempt-1` plan,
  not a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `?? orchestrator/rounds/round-076/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-076/selection.md`
  already fixes this round to roadmap item `N9` only, keeps the round
  docs-only and selection-only, preserves accepted `N8`, and forbids target
  binding, implementation, verification, roadmap edits, state edits, and
  silent widening.
- `orchestrator/rounds/round-076/state-snapshot.json` fixes the
  live controller state at `active_round_id: "round-076"`, `stage: "plan"`,
  `current_task: "N9"`, branch
  `codex/round-076-n9-next-live-subject-selection`, and `retry: null`.
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-009/roadmap.md` makes item
  `9` the first pending item after accepted `N8`; later work cannot begin
  until this round freezes one next live subject.
- `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
  records that the first reopened `N2` selection is now predecessor evidence
  only and that the fresh post-`N8` lane still requires its own bounded next
  subject selection before any new target or slice can start.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains the inherited baseline contract: explicit recursive annotations are
  supported, automatic recursive-type inference remains unresolved and
  disabled, and the
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
  boundary remains mandatory.
- `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
  remains the authoritative `L1` fail-closed record. The repaired
  `URI-R2-C1` queue had no fresh lawful exact successor slice inside the
  inherited boundary.
- `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
  remains the authoritative `L2` closeout. The repaired queue finalized as
  `stop-blocked` and any broader future work required a separate roadmap
  amendment plus fresh selection.
- `docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
  remains the authoritative `N1` record. It reopened only planning scope and
  kept `boundVarTarget`, `boundTarget`, and `schemeBodyTarget` blocked unless a
  later accepted roadmap item explicitly reopened them.
- `docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`
  remains the authoritative first reopened `N2` record. It selected exactly one
  subject, the preserved generic scheme-alias / base-like `baseTarget` route,
  and explicitly deferred `boundVarTarget`, `boundTarget`, `schemeBodyTarget`,
  replay reopen, and every other alternative.
- `docs/plans/2026-03-22-automatic-iso-recursive-post-n7-roadmap-amendment-authority-gate.md`
  remains the authoritative `N8` record. Its one bounded outcome,
  `reopen-planning-only-successor-lane`, keeps the exact accepted non-local
  `baseTarget -> baseC` packet as predecessor evidence only, authorizes exactly
  one fresh docs-first successor-planning lane, and still does not choose the
  next live subject or authorize implementation or verification.
- `orchestrator/rounds/round-075/review-record.json`
  remains the authoritative acceptance proof that `N8` finalized as
  `accepted + finalize` with final outcome
  `reopen-planning-only-successor-lane`.
- `docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`,
  `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`,
  `docs/plans/2026-03-18-uri-r2-c1-e3-bounded-verification-gate.md`,
  and
  `docs/plans/2026-03-18-uri-r2-c1-e4-next-cycle-decision-gate.md`
  remain accepted predecessor evidence for the repaired-queue retained-child
  `boundVarTarget` slice only. They do not automatically bind a new post-`N8`
  target, implementation slice, or verification slice.
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
  still contains the retained-child route anchors:
  `boundVarTargetRoot`, `boundHasForallFrom`, `boundVarTarget`,
  `keepTargetFinal`, and the `targetC` branch that falls back through
  `schemeBodyTarget`.
- `test/PipelineSpec.hs` still carries
  retained-child same-lane positive coverage, same-lane source-guard coverage,
  and nested-`forall` fail-closed contrast coverage.
- `Bugs.md` still carries open
  `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains read-only
  predecessor context only and does not authorize replay reopen, `MLF.Elab.Inst`,
  or any different live subject in this round.
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-009/retry-subloop.md`
  keeps prior attempts immutable. This `attempt-1` plan must therefore create
  one new bounded selection artifact without rewriting any prior attempt or
  review artifact.

## File Map

### Modify

- `docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-next-live-subject-selection.md`
  - Responsibility: canonical `N9` docs-only selection artifact recording the
    bounded thesis-backed subject choice, the predecessor-evidence chain, the
    unchanged blocked-route set, and the explicit non-authorization boundary.

### Read-Only Evidence

- `orchestrator/rounds/round-076/selection.md`
- `orchestrator/rounds/round-076/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-009/roadmap.md`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-009/verification.md`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-009/retry-subloop.md`
- `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
- `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`
- `docs/plans/2026-03-22-automatic-iso-recursive-post-n7-roadmap-amendment-authority-gate.md`
- `orchestrator/rounds/round-075/review-record.json`
- `docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`
- `docs/plans/2026-03-18-uri-r2-c1-e2-bounded-implementation-slice.md`
- `docs/plans/2026-03-18-uri-r2-c1-e3-bounded-verification-gate.md`
- `docs/plans/2026-03-18-uri-r2-c1-e4-next-cycle-decision-gate.md`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/Scope.hs`
- `test/PipelineSpec.hs`
- `papers/these-finale-english.txt`
- `Bugs.md`

### Preserve Unchanged

- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-009/roadmap.md`
- `orchestrator/rounds/round-076/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-009/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-009/verification.md`
- `Bugs.md`
- `src/`
- `test/`
- `src-public/`
- `app/`
- `mlf2.cabal`
- reviewer-owned history under
  `orchestrator/rounds/round-001/`
  through
  `orchestrator/rounds/round-075/`

## Exact Selected `N9` Slice (Exactly One)

The only selected `N9` slice is:

choose exactly one docs-only post-`N8` next live subject, with authoritative
selection outcome `boundVarTarget-planning-subject-selected`.

That selected outcome must mean all of the following, and nothing wider:

- the next live subject is the preserved retained-child / nested-`forall` /
  binding-structure route centered on
  `boundVarTargetRoot`, `boundHasForallFrom`, `boundVarTarget`,
  `keepTargetFinal`, and the `targetC` branch in
  `MLF.Elab.Run.ResultType.Fallback`;
- the accepted repaired-queue `E1` / `E2` / `E3` / `E4` same-lane local
  retained-child slice is predecessor evidence only and may not be treated as
  the already-authorized exact packet for this fresh lane;
- the accepted `N2` selection and the exact accepted non-local
  `baseTarget -> baseC` packet are predecessor evidence only and may not be
  treated as the still-live subject or as carry-forward clearance;
- the selected subject stays at planning scope only and does not bind an exact
  next target, an implementation slice, or a verification slice; and
- replay reopen, `MLF.Elab.Inst`, `InstBot`, `boundTarget`,
  `schemeBodyTarget`, `src/MLF/Elab/Run/ResultType/View.hs`, every other
  fallback family, every different solver/pipeline subject, cross-family
  search, equi-recursive reasoning, implicit unfolding, cyclic structural
  encoding, multi-SCC support, second-interface work, and fallback widening
  remain blocked unless a later accepted roadmap item explicitly reopens them.

Do not switch the selected outcome back to the prior `baseTarget` subject, and
do not silently reopen the accepted repaired-queue `E` packet as live work.

## Sequential Tasks

### Task 1 - Re-establish the exact `N9` stage contract and planning-only boundary

- Write the future canonical `N9` artifact as `round-076` / `N9` /
  `attempt-1` / `retry: null`.
- State explicitly that `N9` is docs-only next-live-subject selection work
  inside accepted `N8 = reopen-planning-only-successor-lane`.
- State explicitly that this round chooses exactly one subject only and does
  not bind the follow-on safety contract, exact target, implementation slice,
  or verification slice for that future lane.
- Preserve accepted `L1` / `L2` / `N1` through `N8` continuity as binding
  predecessor input.
- Preserve the inherited
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
  boundary unchanged.

### Task 2 - Reconstruct the exact admissible post-`N8` selection domain

- Use `selection.md`, `state.json`, `roadmap.md`, `retry-subloop.md`, the
  mechanism table, accepted `L1`, accepted `L2`, accepted `N1`, accepted
  `N2`, accepted `N8`, the accepted repaired-queue `E` chain, and `Bugs.md`
  to restate the exact packet that now constrains `N9`.
- Carry forward these fixed inputs:
  - the repaired `URI-R2-C1` queue remains closed predecessor evidence only;
  - the first reopened `N2 = baseTarget-planning-subject-selected` lane is now
    predecessor evidence only after accepted `N8`;
  - accepted `N8` reopened only a planning lane, not an implementation lane
    and not an exact-target bind;
  - the prior accepted repaired-queue `boundVarTarget` implementation and
    verification packet is predecessor evidence only, not automatic
    carry-forward authority; and
  - every other blocked route remains blocked unless this round selects it
    explicitly at planning scope.
- Do not reinterpret accepted `L1`, accepted `L2`, accepted `N2`, or accepted
  `E1` / `E2` / `E3` / `E4` as implicit permission to skip a fresh subject
  selection.

### Task 3 - Ground the selection in thesis-backed binding-structure constraints

- Extract only the thesis evidence needed to justify why the next live subject
  should move from the completed non-local `baseTarget` packet to one bounded
  binding-structure route:
  - `3.3.3.1 Grafting` for the fact that transformations over graphic types
    must track how binding structure changes;
  - `5.1` for the fact that inference-oriented `MLF` transformations are
    shaped by the richer binding structure of graphic types;
  - `8.3.1 Inlining bounds` for the requirement that binder and binding-flag
    reconstruction stay unambiguous; and
  - `17.3 Perspectives` for the thesis claim that the main difficulty of
    recursive types in this setting lies in recursion in the binding
    structure, not in cyclic-graph experimentation.
- Use those anchors to justify the precise `N9` subject:
  - select the retained-child `boundVarTarget` route;
  - frame it as the next docs-first binder-ownership / nested-owner /
    target-selection planning subject; and
  - keep it outside implementation readiness, exact target binding,
    equi-recursive reasoning, and cyclic-graph experimentation.
- The artifact should paraphrase the thesis into repo-local selection language.
  It should not promise that the thesis already authorizes a code change.

### Task 4 - Reconstruct the exact code/test predecessor packet for the selected subject

- Use current read-only source/test anchors to name the selected planning
  subject precisely:
  - `Fallback.hs` lines covering `boundVarTargetRoot`,
    `boundHasForallFrom`, `boundVarTarget`, `keepTargetFinal`, and the
    `targetC` branch that either selects the retained child or falls back to
    `schemeBodyTarget`;
  - `PipelineSpec.hs` lines covering the retained-child same-lane positive
    case, the retained-child same-lane source guard, and the retained-child
    nested-`forall` fail-closed contrast; and
  - `Scope.hs` lines showing `schemeBodyTarget` remains a neighboring owner
    boundary rather than the selected subject itself.
- State explicitly that the selected planning subject is the preserved
  retained-child / nested-`forall` route only, not the neighboring
  `schemeBodyTarget` fallback, not `boundTarget` overlay materialization, and
  not the already-completed non-local `baseTarget` packet.
- State explicitly that the repaired-queue `E1` / `E2` / `E3` / `E4` packet
  remains predecessor evidence for boundedness and fail-closed behavior only.

### Task 5 - Write the canonical `N9` artifact as one bounded selection

- Create
  `docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-next-live-subject-selection.md`.
- The artifact must include:
  - the stage contract freeze for `round-076` / `N9` / `attempt-1` /
    `retry: null`;
  - accepted `L1` / `L2` / `N1` through `N8` continuity as binding input;
  - the thesis basis from the selected sections of
    `papers/these-finale-english.txt`;
  - the exact one-subject `N9` selection; and
  - an explicit blocked/deferred-alternatives section.
- Current planning read: record exactly one selected subject and one outcome:
  - the preserved retained-child / nested-`forall` / binding-structure
    `boundVarTarget` route;
  - authoritative outcome `boundVarTarget-planning-subject-selected`; and
  - planning-only meaning: the route is selected only as the next live subject
    for later safety-contract and exact-target work.
- State explicitly that a later accepted safety/acceptance contract and a later
  accepted exact-target bind are still required before any design or code slice
  begins on this fresh lane.

### Task 6 - Keep deferred alternatives and non-authorization explicit

- State explicitly that `N9` does not authorize:
  - exact target binding;
  - implementation;
  - verification;
  - merge action;
  - roadmap edits;
  - controller-state edits;
  - bug-tracker edits; or
  - predecessor-history rewrites.
- State explicitly that `N9` defers or rejects, as still blocked:
  - the accepted non-local `baseTarget -> baseC` packet as live work;
  - automatic carry-forward of accepted `N2 = baseTarget-planning-subject-selected`;
  - automatic carry-forward of accepted repaired-queue `E1` / `E2` / `E3` /
    `E4`;
  - `boundTarget`;
  - `schemeBodyTarget`;
  - `src/MLF/Elab/Run/ResultType/View.hs`;
  - replay reopen;
  - `MLF.Elab.Inst` or `InstBot`;
  - every other `ResultType.Fallback` family;
  - every different solver or pipeline subject;
  - cross-family search;
  - equi-recursive reasoning;
  - implicit unfolding;
  - cyclic structural graph encoding;
  - multi-SCC support;
  - a second executable interface; and
  - any compatibility, convenience, or default-path fallback widening.
- State explicitly that selection of the `boundVarTarget` planning subject is
  not exact-target readiness, not implementation readiness, and not authority
  to skip the future gate sequence.

### Task 7 - Run the docs-only continuity and scope checks required for `N9`

All commands run in:
`.worktrees/round-076`

Baseline docs/state checks:

- `git branch --show-current`
  - Expect: `codex/round-076-n9-next-live-subject-selection`
- `git status --short --untracked-files=all`
  - Expect: bounded docs-only round payload only
- `git diff --check`
  - Expect: pass
- `python3 -m json.tool orchestrator/rounds/round-076/state-snapshot.json >/dev/null`
  - Expect: pass
- `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-076/state-snapshot.json`
  - Expect: `contract_version: 2` and `retry: null`
- `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-009/roadmap.md`
  - Expect: parseable roadmap with item `9` pending
- Required artifact presence:
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
  - `test -f docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
  - `test -f docs/plans/2026-03-22-automatic-iso-recursive-post-l2-roadmap-amendment-authority-gate.md`
  - `test -f docs/plans/2026-03-22-automatic-iso-recursive-next-live-subject-selection.md`
  - `test -f docs/plans/2026-03-22-automatic-iso-recursive-post-n7-roadmap-amendment-authority-gate.md`
  - `test -f tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
  - `test -f orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-009/retry-subloop.md`
  - `test -f orchestrator/rounds/round-075/review-record.json`

`N9`-specific continuity checks:

- `rg -n '"stage_id": "N8"|"attempt_verdict": "accepted"|"stage_action": "finalize"|"status": "authoritative"|"final_outcome": "reopen-planning-only-successor-lane"' orchestrator/rounds/round-075/review-record.json`
  - Expect: accepted `N8` finalization fields present
- `rg -n 'reopen-planning-only-successor-lane|predecessor evidence only|does not choose the next live subject|does not authorize implementation|does not authorize verification' docs/plans/2026-03-22-automatic-iso-recursive-post-n7-roadmap-amendment-authority-gate.md`
  - Expect: accepted `N8` still preserves planning-only successor authority and
    still blocks target binding / implementation / verification
- `rg -n 'boundVarTarget|nested-`forall`|same-lane retained-child|continue-bounded' docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md docs/plans/2026-03-18-uri-r2-c1-e4-next-cycle-decision-gate.md`
  - Expect: accepted repaired-queue retained-child packet still exists as
    predecessor evidence and still records bounded same-lane / nested-`forall`
    behavior
- `rg -n 'boundVarTargetRoot|boundHasForallFrom|boundVarTarget =|sameLocalTypeLane|keepTargetFinal =|Just v -> v|Nothing -> schemeBodyTarget targetPresolutionView rootC' src/MLF/Elab/Run/ResultType/Fallback.hs`
  - Expect: current retained-child route anchors present
- `rg -n 'keeps retained-child fallback recursive through a same-lane local TypeRef root|keeps retained-child lookup bounded to the same local TypeRef lane|keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary' test/PipelineSpec.hs`
  - Expect: focused retained-child evidence still present
- `rg -n 'schemeBodyTarget :: PresolutionView|S′|generalizeTargetNode' src/MLF/Elab/Run/Scope.hs`
  - Expect: `schemeBodyTarget` remains the neighboring owner-local target
    selector rather than the selected `N9` subject

Post-write artifact checks:

- `rg -n 'Attempt: `attempt-1`|Retry state: `null`|boundVarTarget-planning-subject-selected|predecessor evidence only|does not authorize implementation|does not authorize verification|does not bind an exact target' docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-next-live-subject-selection.md`
  - Expect: all critical `N9` selection constraints present
- `git diff --name-only -- src test src-public app mlf2.cabal`
  - Expect: no output
- `git diff --name-only -- orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-009/roadmap.md orchestrator/rounds/round-076/state-snapshot.json Bugs.md`
  - Expect: no output

`cabal build all && cabal test` is intentionally out of scope for this round
if the diff remains docs-only and does not touch `src/`, `src-public/`,
`app/`, `test/`, or `mlf2.cabal`. The artifact and reviewer record must state
that skip reason explicitly rather than implying new code-path clearance.

## Reviewer Handoff

Reviewer should confirm that the implemented `N9` artifact:

- is explicitly framed as `round-076` / `N9` / `attempt-1` / `retry: null`;
- treats accepted `L1` / `L2` / `N1` through `N8` as binding predecessor
  continuity;
- records exactly one bounded selection outcome,
  `boundVarTarget-planning-subject-selected`;
- treats both the accepted non-local `baseTarget -> baseC` packet and the
  accepted repaired-queue `E1` / `E2` / `E3` / `E4` retained-child packet as
  predecessor evidence only rather than still-live authorization;
- grounds the selected subject in thesis-backed binding-structure constraints;
- keeps the inherited
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
  boundary unchanged;
- does not bind an exact next target or authorize implementation or
  verification;
- keeps `boundTarget`, `schemeBodyTarget`, replay reopen, `MLF.Elab.Inst`,
  `InstBot`, `src/MLF/Elab/Run/ResultType/View.hs`, other fallback families,
  other solver/pipeline subjects, cross-family search, equi-recursive
  reasoning, implicit unfolding, cyclic encoding, multi-SCC support,
  second-interface work, and fallback widening blocked; and
- keeps the round docs-only, with no roadmap edits, state edits, bug-tracker
  edits, review artifacts, merge notes, or code changes.
