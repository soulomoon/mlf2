# Round 080 Plan (`N13` Verification/Evidence Consolidation For The Accepted `N12` Same-Lane Retained-Child Packet)

## Objective

Execute only roadmap item `N13` and produce one accepted verifier-owned
docs-only artifact at:
`docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-verification-gate.md`.

This is the initial `N13` plan for `attempt-1` with `retry: null`. The round
must consolidate fresh verifier-visible evidence for the exact accepted `N12`
same-lane local `TypeRef` retained-child `boundVarTarget -> targetC` packet
only, under the still-binding accepted `N10` contract and accepted `N11`
exact bind. `N13` is a docs-only verification/evidence gate. It must not widen
into a new target, new implementation work, replay reopen, or `N14`
closure/decision work.

`N13` must re-verify the accepted `N12` slice across all required evidence
surfaces:

1. read-only source anchors in
   `src/MLF/Elab/Run/ResultType/Fallback.hs`
   showing the unchanged `boundVarTargetRoot`, `boundHasForallFrom`, and
   `boundVarTarget` search, plus the explicit
   `sameLaneLocalRetainedChildTarget` proof, the dedicated
   `keepTargetFinal` gate, the retained-child `targetC` routing through
   `Just v -> v`, and the preserved
   `Nothing -> schemeBodyTarget targetPresolutionView rootC` fallback;
2. read-only focused test anchors in
   `test/PipelineSpec.hs`
   showing the `ARI-C1 feasibility characterization (bounded prototype-only)`
   block, the same-lane retained-child positive example, the source guard for
   `sameLaneLocalRetainedChildTarget` / `keepTargetFinal` / retained-child
   `targetC`, the nested-`forall` fail-closed contrast, and the adjacent
   preserved local/non-local continuity examples;
3. a fresh focused rerun of
   `ARI-C1 feasibility characterization (bounded prototype-only)`;
4. a fresh full repo gate via `cabal build all && cabal test`; and
5. predecessor continuity checks against the accepted `N11` / `N12` artifacts
   and accepted `round-078` / `round-079` review chain, while restating that
   accepted `L1`, accepted `L2`, and the inherited baseline contract remain
   binding.

If any verification step fails, record the blocker in the canonical `N13`
artifact and stop. Do not patch `Fallback.hs`, `PipelineSpec.hs`, or any other
production/test file during this round.

## Locked Round Context

- Round id: `round-080`
- Roadmap item: `N13`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Fixed live subject: exact `N11`-frozen / `N12`-implemented same-lane local
  `TypeRef` retained-child `boundVarTarget -> targetC` packet only
- Active branch: `codex/round-080-n13-verification-gate`
- Active worktree:
  `.worktrees/round-080`
- Stage mode: docs-only bounded verification/evidence consolidation only
- Current round review feedback: none yet; this is a full `attempt-1` plan,
  not a retry delta

Current round worktree state is already non-pristine. Respect existing edits
and do not revert unrelated work:

- `M orchestrator/rounds/round-080/state-snapshot.json` (controller-owned; must remain untouched)
- `?? orchestrator/rounds/round-080/selection.md`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-080/selection.md`
  already fixes this round to roadmap item `N13` only, keeps the round
  evidence-only and verifier-owned, requires fresh read-only
  `Fallback.hs` / `PipelineSpec.hs` anchors plus fresh focused/full reruns,
  and forbids new implementation, roadmap/state edits, bug-tracker edits,
  review-history rewrites, or silent widening.
- `orchestrator/rounds/round-080/state-snapshot.json` fixes the
  live controller state at `active_round_id: "round-080"`, `stage: "plan"`,
  `current_task: "N13"`, branch
  `codex/round-080-n13-verification-gate`, and `retry: null`.
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/roadmap.md` makes item
  `13` the first pending item after accepted `N12`; `N14` depends on accepted
  `N13` evidence and cannot run first.
- `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
  still records the long-horizon closure as unresolved. The earlier reopened
  `baseTarget` lane already has predecessor-only `N4` / `N5` / `N6` evidence,
  while the fresh post-`N8` `boundVarTarget` lane now has accepted `N11`
  exact-target and accepted `N12` implementation authority and therefore
  requires one later evidence-only reverification before any next-cycle
  decision can run.
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  remains the inherited baseline contract: explicit recursive annotations are
  supported, automatic recursive-type inference remains unresolved and
  disabled, and the inherited
  `explicit-only / non-equi-recursive / non-cyclic-graph / no-second-interface / no-fallback`
  boundary remains mandatory.
- `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
  remains the authoritative `L1` fail-closed record, and
  `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
  remains the authoritative `L2` closeout. They keep the repaired queue closed
  and do not grant implicit clearance for any preserved route.
- `docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-next-live-subject-selection.md`
  remains the accepted `N9` selection record: exactly one fresh planning
  subject, the retained-child / nested-`forall` / binding-structure
  `boundVarTarget` route.
- `docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-safety-acceptance-contract.md`
  remains the accepted `N10` contract. `N13` must preserve its binding axes:
  retained-child ownership and target-selection anchors stay review-visible,
  nested-`forall` / nested-owner / nested scheme-root crossings stay
  fail-closed, `schemeBodyTarget` stays neighboring boundary context only, and
  the selected packet remains explicit-only, iso-recursive,
  non-equi-recursive, non-cyclic-graph, single-family, and no-fallback.
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-exact-target-bind.md`
  remains the accepted `N11` artifact, and
  `orchestrator/rounds/round-078/review.md`,
  `orchestrator/rounds/round-078/reviews/attempt-1.md`,
  and
  `orchestrator/rounds/round-078/review-record.json`
  remain the accepted `N11` review chain. They freeze exactly one packet only:
  the same-lane local `TypeRef` retained-child `boundVarTarget -> targetC`
  route, with `schemeBodyTarget` neighboring-only and all other routes still
  blocked or predecessor-only.
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-implementation-slice.md`
  remains the accepted `N12` implementation artifact. It states that the
  `boundVarTarget` candidate search stayed unchanged, that the explicit
  `sameLaneLocalRetainedChildTarget` proof was added, and that only the
  retained-child `keepTargetFinal` / `targetC` consumer now routes through
  that proof.
- `orchestrator/rounds/round-079/review.md`,
  `orchestrator/rounds/round-079/reviews/attempt-1.md`,
  and
  `orchestrator/rounds/round-079/review-record.json`
  remain the accepted `N12` review chain. They prove that `N12` finalized as
  `accepted + finalize`, that the exact same-lane retained-child packet stayed
  bounded, and that both the focused `ARI-C1` rerun and the full repo gate
  passed on the accepted implementation slice.
- `docs/plans/2026-03-22-automatic-iso-recursive-base-target-exact-target-bind.md`,
  `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-implementation-slice.md`,
  `docs/plans/2026-03-22-automatic-iso-recursive-base-target-non-local-bounded-verification-gate.md`,
  and
  `docs/plans/2026-03-18-uri-r2-c1-e1-next-target-bind.md`
  through
  `docs/plans/2026-03-18-uri-r2-c1-e4-next-cycle-decision-gate.md`
  remain predecessor evidence for the earlier `baseTarget` lane and repaired
  queue only. They do not authorize current work beyond the exact selected
  `boundVarTarget` packet.
- `Bugs.md` still carries open
  `BUG-2026-03-16-001`, but that replay / `InstBot` defect remains read-only
  predecessor context only and does not authorize replay reopen,
  `MLF.Elab.Inst`, `InstBot`, or any wider live subject in this round.
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/retry-subloop.md`
  keeps prior attempts immutable. This `attempt-1` plan must therefore define
  one bounded verification packet without rewriting any prior attempt or
  review artifact.

## File Map

### Modify

- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-verification-gate.md`
  - Responsibility: canonical `N13` verification/evidence record for the exact
    accepted `N12` slice only, including fresh command evidence, read-only
    anchor evidence, accepted `N11` / `N12` continuity, docs-only diff
    evidence, and explicit non-authorization.

### Read-Only Evidence

- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `test/PipelineSpec.hs`
- `orchestrator/rounds/round-080/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/roadmap.md`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/verification.md`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/retry-subloop.md`
- `tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
- `docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
- `docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-next-live-subject-selection.md`
- `docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-safety-acceptance-contract.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-exact-target-bind.md`
- `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-implementation-slice.md`
- `orchestrator/rounds/round-078/review.md`
- `orchestrator/rounds/round-078/reviews/attempt-1.md`
- `orchestrator/rounds/round-078/review-record.json`
- `orchestrator/rounds/round-079/review.md`
- `orchestrator/rounds/round-079/reviews/attempt-1.md`
- `orchestrator/rounds/round-079/review-record.json`
- `Bugs.md`

### Preserve Unchanged

- `src/`
- `test/`
- `src-public/`
- `app/`
- `mlf2.cabal`
- `orchestrator/rounds/round-080/state-snapshot.json`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/roadmap.md`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/verification.md`
- `Bugs.md`
- `orchestrator/rounds/round-080/selection.md`
- reviewer-owned history under
  `orchestrator/rounds/round-001/`
  through
  `orchestrator/rounds/round-079/`

No edit to `Fallback.hs`, `PipelineSpec.hs`, `src-public/`, `app/`, or
`mlf2.cabal` is authorized during `N13` `attempt-1`. If reverification fails,
record the blocker and stop.

## Exact Selected `N13` Slice (Exactly One)

The only selected `N13` slice is:

record current verifier-visible evidence for the exact accepted `N12`
same-lane local `TypeRef` retained-child packet only, meaning the live
`boundVarTargetRoot` / `boundHasForallFrom` / `boundVarTarget` search,
the explicit `sameLaneLocalRetainedChildTarget` proof, the dedicated
`keepTargetFinal` gate and retained-child `targetC` routing, the focused
`ARI-C1` behavioral/source-guard/fail-closed evidence in `PipelineSpec.hs`,
fresh focused/full gate reruns, and continuity back to the accepted
`L1` / `L2` / `N9` / `N10` / `N11` / `N12` records.

Required interpretation of that one bounded slice:

- `N13` is not another implementation slice. It re-verifies only the already
  accepted `N12` production/test packet.
- The live source anchors to cite are the existing selected-lane logic in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`,
  in particular:
  - `boundVarTargetRoot` at line `558`;
  - `boundHasForallFrom` beginning at line `563`;
  - `boundVarTarget` beginning at line `648`;
  - `sameLaneLocalRetainedChildTarget` at line `697`;
  - `keepTargetFinal` at line `701`; and
  - the retained-child `targetC` routing at lines `708-731`, including
    `Just v -> v` at line `730` and the preserved
    `schemeBodyTarget targetPresolutionView rootC` fallback at line `731`.
- The live focused test anchors to cite are the existing helper/test block in
  `test/PipelineSpec.hs`,
  in particular:
  - the `ARI-C1` block at line `1103`;
  - the same-lane retained-child behavioral success example at line `1495`;
  - the same-lane source guard for
    `sameLaneLocalRetainedChildTarget` / `keepTargetFinal` / retained-child
    `targetC` at line `1572`;
  - the nested-`forall` fail-closed contrast at line `1595`;
  - the preserved local empty-candidate and local scheme-alias/root-final
    continuity examples at lines `1606` and `1610`; and
  - the explicit non-local/local separation guard at line `1710`.
- `N13` must restate that the preserved local empty-candidate lane, the
  preserved local scheme-alias/root-final lane, the local single-base /
  inst-arg lanes, the earlier `baseTarget` packet, the repaired-queue
  retained-child packet, and open `BUG-2026-03-16-001` remain continuity
  context only, not new live work.
- `N13` must not reinterpret fresh green verification as clearance for:
  - `N14`;
  - replay reopen;
  - `MLF.Elab.Inst` or `InstBot`;
  - `boundTarget`, `schemeBodyTarget`, or `src/MLF/Elab/Run/ResultType/View.hs`;
  - any other fallback family;
  - any different solver/pipeline subject;
  - cross-family search;
  - equi-recursive reasoning or implicit unfolding;
  - cyclic structural graph encoding, graph-cycle exceptions, or multi-SCC
    support; or
  - any second interface, compatibility shim, convenience fallback, or
    default-path widening.

## Sequential Tasks

### Task 1 - Freeze the `N13` `attempt-1` contract as a docs-only verification gate

- Write the canonical `N13` artifact as `attempt-1` with `retry: null`.
- State explicitly that `N13` records only current verification/evidence for
  the accepted `N12` same-lane retained-child packet and does not authorize:
  - production/test/public-API/executable/Cabal edits;
  - roadmap edits;
  - `orchestrator/rounds/round-080/state-snapshot.json` edits;
  - bug-tracker edits;
  - predecessor-history rewrites; or
  - `N14` decision work.
- Reassert the inherited boundary unchanged:
  - explicit-only recursive baseline;
  - non-equi-recursive semantics;
  - non-cyclic structural graph encoding;
  - no second executable interface; and
  - no compatibility / convenience / default-path widening.
- State that any verification failure is a blocker to record, not permission to
  patch `Fallback.hs`, `PipelineSpec.hs`, or any other code/test file during
  this attempt.

### Task 2 - Reconfirm the accepted `L1` / `L2` / `N9` / `N10` / `N11` / `N12` authority chain without widening it

- Reconfirm the inherited baseline plus accepted `L1` / `L2` closeout and
  restate that they still bind `N13`.
- Reconfirm the accepted `N10` contract from
  `docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-safety-acceptance-contract.md`
  and restate that it still binds this verification gate.
- Reconfirm the exact `N11` packet from
  `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-exact-target-bind.md`
  and restate that `N13` remains confined to that exact packet only.
- Reconfirm the accepted `N12` implementation artifact and review chain:
  - `python3 -m json.tool orchestrator/rounds/round-078/review-record.json >/dev/null`
  - `python3 -m json.tool orchestrator/rounds/round-079/review-record.json >/dev/null`
  - short `python3` continuity assertion over `round-078` and `round-079`
    review records proving accepted authoritative continuity for `N11` and
    `N12`
  - `test -f docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-implementation-slice.md`
- State explicitly that the accepted `N12` slice still means:
  - the `boundVarTarget` candidate search remained unchanged;
  - explicit `sameLaneLocalRetainedChildTarget` proof present;
  - only the retained-child `keepTargetFinal` / `targetC` consumer routed
    through that proof; and
  - the focused `ARI-C1` block remains the bounded test packet.
- Treat `Bugs.md` only as continuity
  context. Do not reinterpret open `BUG-2026-03-16-001` as current repair or
  widening authority.

### Task 3 - Collect read-only anchor evidence from the live code/test packet

- Inspect the existing selected-lane logic in
  `src/MLF/Elab/Run/ResultType/Fallback.hs`
  without editing it.
- Capture line-referenced evidence for all of the following:
  - `boundVarTargetRoot`;
  - `boundHasForallFrom`;
  - `boundVarTarget`;
  - `sameLaneLocalRetainedChildTarget`;
  - `keepTargetFinal`;
  - the retained-child `targetC` routing through `Just v -> v`; and
  - the preserved `Nothing -> schemeBodyTarget targetPresolutionView rootC`
    fallback.
- Recommended anchor command:
  - `nl -ba src/MLF/Elab/Run/ResultType/Fallback.hs | sed -n '558,731p'`
- Inspect the focused
  `describe "ARI-C1 feasibility characterization (bounded prototype-only)"`
  block in
  `test/PipelineSpec.hs`
  without editing it.
- Capture line-referenced evidence showing:
  - the helper family remains in the same focused block;
  - the same-lane retained-child positive example still exercises the selected
    packet;
  - the source guard still requires the explicit proof name and dedicated
    routing through `keepTargetFinal` / retained-child `targetC`;
  - the nested-`forall` fail-closed contrast still rejects wrapper crossings;
  - preserved local continuity examples remain adjacent continuity only; and
  - the explicit non-local/local separation guard still keeps the earlier
    `baseTarget` packet distinct from the selected packet.
- Recommended anchor commands:
  - `nl -ba test/PipelineSpec.hs | sed -n '1103,1135p'`
  - `nl -ba test/PipelineSpec.hs | sed -n '1495,1785p'`

### Task 4 - Run the required bounded verification suite for `N13`

- Run the baseline checks required by
  `orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/verification.md`:
  - `git branch --show-current`
  - `git status --short --untracked-files=all`
  - `git diff --check`
  - `python3 -m json.tool orchestrator/rounds/round-080/state-snapshot.json >/dev/null`
  - `rg -n '"contract_version": 2|"retry": null|"retry": \{' orchestrator/rounds/round-080/state-snapshot.json`
  - `rg -n '^\d+\. \[(pending|in-progress|done)\]' orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/roadmap.md`
  - `test -f docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
  - `test -f docs/plans/2026-03-14-unannotated-iso-recursive-r5-research-stop-decision.md`
  - `test -f docs/plans/2026-03-17-uri-r2-c1-r4-repair-decision-gate.md`
  - `test -f docs/plans/2026-03-21-uri-r2-c1-l1-next-target-bind.md`
  - `test -f docs/plans/2026-03-21-uri-r2-c1-l2-post-l1-fail-closed-successor-decision-gate.md`
  - `test -f docs/plans/2026-03-23-automatic-iso-recursive-bound-var-target-safety-acceptance-contract.md`
  - `test -f docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-exact-target-bind.md`
  - `test -f docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-implementation-slice.md`
  - `test -f tasks/todo/2026-03-21-automatic-iso-recursive-next-loop/mechanism_table.md`
  - `test -f orchestrator/roadmaps/2026-03-22-00-automatic-iso-recursive-inference-post-l2-successor-roadmap/rev-013/retry-subloop.md`
- Run the focused bounded test block exactly:
  - `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- Require the focused rerun to stay green and to remain bounded to the selected
  same-lane retained-child packet plus its preserved fail-closed and
  predecessor-only contrasts.
- Run the mandatory fresh full repo gate exactly:
  - `cabal build all && cabal test`
- Even though `N13` is docs-only, do not skip the fresh full repo gate. This
  stage exists to record current verification evidence for the accepted `N12`
  slice, not merely restate prior green results.

### Task 5 - Re-run predecessor continuity and docs-only diff checks

- Re-run accepted-predecessor continuity checks against the authoritative
  implementation artifact and authoritative review chain:
  - `python3 -m json.tool orchestrator/rounds/round-078/review-record.json >/dev/null`
  - `python3 -m json.tool orchestrator/rounds/round-079/review-record.json >/dev/null`
  - `rg -n 'sameLaneLocalRetainedChildTarget|cabal build all && cabal test|20 examples, 0 failures|boundVarTarget-same-lane-retained-child-proof-slice-established' docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-implementation-slice.md orchestrator/rounds/round-079/review.md`
  - short reviewer-readable continuity summary over the accepted
    `round-078` / `round-079` review-record fields and `checks` map
- Re-run docs-only diff checks so the reviewer can confirm that `N13` did not
  patch code while reverifying code:
  - `git diff --name-only`
  - `git diff --name-only -- src test src-public app mlf2.cabal`
  - `git diff --name-only -- . ':(exclude)docs/**' ':(exclude)orchestrator/**'`
  - `git status --short --untracked-files=all`
- Reconfirm reviewer-target immutability before reviewer outputs:
  - `find orchestrator/rounds/round-080 -maxdepth 2 -type f | sort`
  - `test ! -f orchestrator/rounds/round-080/review-record.json && test ! -f orchestrator/rounds/round-080/reviews/attempt-1.md`
- If any non-doc diff appears under `src/`, `test/`, `src-public/`, `app/`,
  or `mlf2.cabal`, stop and report a blocker instead of treating the round as
  a clean docs-only verification gate.

### Task 6 - Author the canonical `N13` artifact and reviewer handoff

- Write the canonical verification artifact at:
  `docs/plans/2026-03-24-automatic-iso-recursive-bound-var-target-same-lane-retained-child-bounded-verification-gate.md`.
- Structure the artifact so the reviewer can audit, in order:
  - stage contract freeze for `N13` `attempt-1`;
  - accepted `L1` / `L2` / `N9` / `N10` / `N11` / `N12` authority chain
    carried forward without widening;
  - read-only anchor evidence from `Fallback.hs` and `PipelineSpec.hs`;
  - fresh baseline / focused-block / full-gate results;
  - accepted `N11` / `N12` continuity recheck;
  - docs-only diff evidence;
  - stability conclusion or blocker section; and
  - explicit non-authorization preserving the active boundary and unchanged
    blocked routes.
- Ensure the reviewer can see, without inference, that:
  - this is the initial `N13` plan for `attempt-1` with `retry: null`;
  - the canonical `N13` artifact path is exact;
  - the focused `ARI-C1` rerun and `cabal build all && cabal test` gate are
    both fresh;
  - predecessor continuity was checked against the accepted `N12` artifact and
    accepted `round-079` review chain, plus the accepted `N11` exact bind;
  - `sameLaneLocalRetainedChildTarget` remains the selected explicit proof
    anchor;
  - `keepTargetFinal` and the retained-child `targetC` consumer remain
    dedicated to that proof while `schemeBodyTarget` remains neighboring
    boundary context only; and
  - no implementation/public/Cabal/roadmap/Bugs drift and no new target or
    `N14` authority was inferred.
- Ensure reviewer outputs remain retry-subloop compatible: record
  `Implemented stage result`, `Attempt verdict`, `Stage action`,
  `Retry reason`, and `Fix hypothesis`; any same-round retry must add a new
  attempt artifact rather than rewriting earlier history.

## Reviewer Handoff

Reviewer should record the following concrete checks for this round:

- `N13-CONTRACT`: `selection.md`, `plan.md`, and the canonical artifact align
  on `round-080` / `N13` / `attempt-1` / `retry: null` as a docs-only
  verification/evidence gate for the exact accepted `N12` packet only.
- `N13-L1-L2-N1-N12-CONTINUITY`: the canonical artifact treats accepted `L1`,
  `L2`, and accepted `N1` through `N12` as binding predecessor continuity and
  does not reinterpret predecessor evidence as fresh authority.
- `N13-ROUND079-CONTINUITY`: the artifact explicitly carries forward the
  accepted `round-079` canonical artifact plus
  `review.md` / `reviews/attempt-1.md` / `review-record.json` as the direct
  authority that the exact same-lane retained-child implementation slice
  already landed and previously passed focused/full verification.
- `N13-ANCHORS`: the live read-only `Fallback.hs` and `PipelineSpec.hs` anchors
  still show the unchanged `boundVarTarget` search, the explicit
  `sameLaneLocalRetainedChildTarget` proof, the dedicated
  `keepTargetFinal` / retained-child `targetC` routing, the same-lane positive
  example, the source guard, the nested-`forall` fail-closed contrast, and the
  preserved adjacent continuity routes.
- `N13-FRESH-VERIFICATION`: the focused `ARI-C1` rerun and the fresh full repo
  gate both ran in this round and both passed, or an exact blocker is recorded
  without any compensating code edit.
- `N13-DOCS-ONLY-DIFF-BOUNDARY`: the diff stays bounded to the canonical `N13`
  artifact plus round-local planner/selection artifacts only, with no tracked
  or untracked drift under `src/`, `test/`, `src-public/`, `app/`, or
  `mlf2.cabal` beyond the controller-owned state file.
- `N13-NO-IMPLICIT-CLEARANCE`: the round does not reinterpret fresh green
  verification as authority for `N14`, replay reopen, `MLF.Elab.Inst`,
  `InstBot`, `boundTarget`, `schemeBodyTarget` as a live subject,
  `ResultType.View`, other fallback families, other solver/pipeline subjects,
  or any wider recursive-inference step.
- `N13-IMMUTABILITY`: prior attempts and reviewer-owned history remain
  unchanged, and reviewer outputs for `round-080` are created fresh rather than
  overwriting earlier artifacts.
- `N13-RETRY-SCHEMA`: the review output follows the retry-subloop schema even
  though this is `attempt-1` with `retry: null`.
- `N13-SCOPE-ALIGNMENT`: the implemented artifact matches the intended scope
  only: one verifier-owned bounded evidence consolidation gate for the exact
  accepted `N12` same-lane retained-child packet, without deciding `N14` or
  widening the live subject.
