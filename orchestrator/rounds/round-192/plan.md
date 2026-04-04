# Round 192 Plan

- Round: `round-192`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-6`
- Retry: `null`
- Execution shape: serial, one bounded current-architecture negative-family / termination-pressure campaign only, evidence-led, non-widening, no worker fan-out, no concurrent `cabal` jobs

## Objective

Keep this round on exactly one bounded `item-6` slice: run the
representative current-architecture negative-family / termination-pressure
campaign for `N1`, `N2`, and `N6` on the authoritative current surfaces and
record the aggregate read in:

`docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md`

The campaign is fixed to the already-authoritative evidence surfaces named by
accepted `round-180`:

- `test/Research/P5ClearBoundarySpec.hs` for the clear-boundary control and
  the reject-side `nestedForallContrastExpr` contrast
- `test/PipelineSpec.hs` for disagreement, ambiguity, local-vs-non-local
  fail-closed contrasts, and widened-search pressure checks around
  `runPipelineElab` / `runPipelineElabChecked`

The planning read for this round is intentionally narrow:

- reuse the accepted item-3 route-family and guard contract exactly as-is
- treat `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` only as a read-only
  anchor for
  `rootNonLocalSchemeAliasBaseLike`,
  `sameLaneLocalRetainedChildTarget`,
  `boundHasForallFrom`,
  `keepTargetFinal`, and
  `targetC`
- if the current evidence already makes `N1`, `N2`, and `N6` reviewer-visible,
  stop at the honest aggregate read instead of inventing more work
- if one representative row is not yet reviewer-visible enough, allow only
  the minimum focused `test/Research/P5ClearBoundarySpec.hs` and/or
  `test/PipelineSpec.hs` hardening needed to make that row explicit

This round must not reopen the item-3 route-family or guard contract, must
not edit production source, must not add a new harness or corpus row, must
not widen into `N3` through `N5`, and must not jump ahead to the `item-7`
repo-level readiness / architecture decision.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input:
  `orchestrator/rounds/round-192/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `?? orchestrator/rounds/round-192/` is the round-owned directory. Keep the
  round-local artifact set scoped to this `plan.md`, the one aggregate item-6
  artifact, and an optional `implementation-notes.md` only.
- `orchestrator/rounds/round-192/selection.md` is the round input and must
  remain untouched.

The inherited boundary remains controlling:

- explicit-only
- iso-recursive
- non-equi-recursive
- `non-cyclic-graph = unknown`
- no-fallback

The read-only authority chain that must remain visible in the campaign
artifact is:

- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-case-and-review-ledger.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-path-audit.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-authoritative-collapse-clear-or-confirm.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-end-to-end-revalidation-and-classification.md`
- `docs/plans/2026-03-26-same-lane-retained-child-public-output-continuity-current-architecture-vs-non-cyclic-graph-decision-gate.md`
- `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-predecessor-authority-unresolved-semantic-matrix-family-success-bar-and-first-concrete-deliverable-freeze.md`
- `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`
- `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`
- `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`
- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`

`Bugs.md` remains read-only predecessor context only. It must not be edited
and must not be reinterpreted as item-6 evidence.

## Write Scope

Implementer-owned writes for this round are limited to:

- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md`
- `test/Research/P5ClearBoundarySpec.hs`
  only if step 1 proves the representative `N2` row is not yet explicit
  enough on the authoritative surfaces
- `test/PipelineSpec.hs`
  only if step 1 proves the representative `N1` and/or `N6` row is not yet
  explicit enough on the authoritative surfaces
- `orchestrator/rounds/round-192/implementation-notes.md`
  only if the implementer needs a round-local summary of the exact bounded
  campaign result

Do not modify:

- `orchestrator/rounds/round-192/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-192/review.md`
- `orchestrator/rounds/round-192/merge.md`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- `src/**`
- `src-public/**`
- `app/**`
- `mlf2.cabal`
- `test/Research/C1AuthoritativeSurfaceSpec.hs`
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- any accepted predecessor `docs/plans/**` artifact

In particular, this round does **not** authorize edits to:

- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `src/MLF/Elab/TermClosure.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`

If step 1 reveals that a newly explicit representative row would require a
production change, a route/guard rewrite, a new harness, or a broader corpus
expansion, stop at an exact blocker characterization in the aggregate item-6
artifact rather than widening the round in place.

## Sequential Plan

1. Freeze the exact representative `N1` / `N2` / `N6` evidence ledger before
   drafting or editing anything; modify no files in this step.
   - Read-only inputs:
     `orchestrator/rounds/round-192/selection.md`,
     `orchestrator/state.json`,
     `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md`,
     `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/verification.md`,
     `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/retry-subloop.md`,
     the March / April authority chain listed above,
     `test/Research/P5ClearBoundarySpec.hs`,
     `test/PipelineSpec.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
     `orchestrator/rounds/round-179/review-record.json`,
     `orchestrator/rounds/round-180/review-record.json`, and
     `orchestrator/rounds/round-191/review-record.json`.
   - Reconfirm one fixed three-row representative ledger only:
     `N1 ambiguity-reject`,
     `N2 unsoundness-guard`, and
     `N6 termination-pressure`.
   - For `N1`, reuse only the current authoritative / source-guard evidence in
     `test/PipelineSpec.hs` that keeps named routes explicit, rejects broad
     wildcard selection, and keeps local-vs-non-local contrasts fail closed
     once the wrapper leaves the selected lane.
   - For `N2`, reuse only the current clear-boundary control and
     `nestedForallContrastExpr` contrast from
     `test/Research/P5ClearBoundarySpec.hs`, plus any matching
     authoritative-surface read already present in `test/PipelineSpec.hs`.
   - For `N6`, reuse only the current `test/PipelineSpec.hs` bounded-lane and
     widened-search pressure checks that show no neighboring-route reopening,
     no ranking rescue, and no growth beyond the fixed item-3 anchor set.
   - End this step with one explicit conclusion only:
     either the current tests already make all three rows reviewer-visible on
     the authoritative surfaces, or exactly one missing reviewer-visible row
     remains and must be tightened inside
     `test/Research/P5ClearBoundarySpec.hs` and/or `test/PipelineSpec.hs`
     only.
   - Verification:
     `python3 -m json.tool orchestrator/state.json >/dev/null`
     `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/verification.md" && test -f "$roadmap_dir/retry-subloop.md"`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "fail-closed once it leaves the local TypeRef lane"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the explicit non-local scheme-alias/base-like proof separate from the preserved local lanes"'`
     `rg -n 'rootNonLocalSchemeAliasBaseLike|sameLaneLocalRetainedChildTarget|boundHasForallFrom|keepTargetFinal|targetC' src/MLF/Elab/Run/ResultType/Fallback/Core.hs test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs`

2. Tighten only the missing reviewer-visible representative evidence, and keep
   any edit inside the two already-authoritative test files.
   - If step 1 already proves that all three representative rows are explicit
     enough, skip this step and leave both test files unchanged.
   - If the `N2` row needs tighter visibility, edit only
     `test/Research/P5ClearBoundarySpec.hs` to make the authoritative
     reject-side fact explicit without changing the selected expression pair:
     preserve `sameLaneClearBoundaryExpr`,
     preserve `nestedForallContrastExpr`,
     preserve `runPipelineElab` / `runPipelineElabChecked`, and preserve the
     current reject-side boundary around `PhiTranslatabilityError` and
     `containsMu False`.
   - If the `N1` or `N6` row needs tighter visibility, edit only
     `test/PipelineSpec.hs` to make the existing no-ranking /
     no-neighboring-route / fail-closed current-architecture read more
     reviewer-visible. Reuse the existing local-vs-non-local contrasts and
     source guards; do not add a new helper, a new test module, or a second
     harness.
   - Do not add new subjects, do not reopen `sameLaneAliasFrame*` positive
     packets, do not import `test/Research/C1AuthoritativeSurfaceSpec.hs`, and
     do not broaden into `N3` through `N5`.
   - If a newly explicit assertion exposes unexpected current behavior, stop
     at an exact blocker note for the aggregate artifact. Do not widen into a
     production-source repair in this round.
   - Verification:
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "fail-closed once it leaves the local TypeRef lane"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the explicit non-local scheme-alias/base-like proof separate from the preserved local lanes"'`

3. Draft the one bounded item-6 aggregate artifact and keep it tied to the
   fixed representative ledger only.
   - Create
     `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md`.
   - The document must stay item-6-only, current-architecture-only,
     non-widening, and aggregate-read-only. It must not reopen the item-3
     route/guard contract, reinterpret one pressure row as positive success,
     or make the item-7 readiness / architecture decision early.
   - The document must include, in order:
     a stage-contract freeze;
     an authority ledger pointing back to the baseline contract, March 25
     capability / architecture / reconstruction docs, the March 26 same-lane
     continuity chain, accepted item-1 through item-5 docs, and accepted
     `round-179`, `round-180`, and `round-191` review records;
     one representative evidence ledger fixed to `N1`, `N2`, and `N6` only;
     one row-classification matrix that cites the exact evidence anchors from
     step 1 (and step 2 only if a test edit was needed);
     one bounded conclusion; and
     one non-claims section.
   - In the row-classification matrix:
     `N1` must be grounded in the current no-ranking / no-wildcard-route
     source guards and fail-closed local-vs-non-local contrasts from
     `test/PipelineSpec.hs`;
     `N2` must be grounded in the
     `sameLaneClearBoundaryExpr` control versus
     `nestedForallContrastExpr` reject-side contrast from
     `test/Research/P5ClearBoundarySpec.hs`;
     and `N6` must be grounded in the current bounded-lane / widened-search
     pressure checks from `test/PipelineSpec.hs`.
   - Use the accepted item-4 / March 25 negative-side vocabulary honestly:
     if the representative row remains reject-side, call it
     `fail-closed rejection`;
     if a row is bounded only because the current tests prove that forbidden
     widened search never enters the lawful ledger, explain that boundedness
     as the reason the row remains reject-side rather than inventing a
     positive or blocker-debt bucket.
   - The bounded conclusion must state exactly whether representative `N1`,
     `N2`, and `N6` still fail closed or stay bounded on
     `runPipelineElab` / `runPipelineElabChecked`, must preserve `N3` through
     `N5` as out of scope, and must keep `non-cyclic-graph` unresolved for
     later item `7` rather than deciding it here.
   - Verification:
     `test -f docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md`
     `rg -n 'N1|N2|N6|fail-closed rejection|runPipelineElab|runPipelineElabChecked|P5ClearBoundarySpec|PipelineSpec|non-cyclic-graph|item-7|N3|N4|N5' docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md`

4. Verify exact round scope, diff hygiene, and the conditional full gate.
   - Keep the authored diff limited to the item-6 aggregate artifact, an
     optional `orchestrator/rounds/round-192/implementation-notes.md`, and
     only the two already-authoritative test files if step 2 required them.
   - If `implementation-notes.md` is written, keep it to one bounded summary
     of the exact `N1` / `N2` / `N6` aggregate read only.
   - Run `git diff --check`.
   - If the round diff touches `test/`, run the full verification gate
     required by `verification.md`:
     `cabal build all && cabal test`.
     If the round stays docs-only, do not widen the gate beyond the focused
     checks already listed above.
   - Verification:
     `git diff --check`
     `python3 - <<'PY'
import subprocess
allowed = {
    'docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md',
    'orchestrator/rounds/round-192/implementation-notes.md',
    'orchestrator/rounds/round-192/plan.md',
    'orchestrator/rounds/round-192/selection.md',
    'test/Research/P5ClearBoundarySpec.hs',
    'test/PipelineSpec.hs',
}
tracked = subprocess.check_output(
    ['git', 'diff', '--name-only', '--', 'docs/plans', 'orchestrator/rounds/round-192', 'test', 'src', 'src-public', 'app', 'mlf2.cabal', 'TODO.md', 'implementation_notes.md', 'Bugs.md', 'orchestrator/roadmaps'],
    text=True,
).splitlines()
untracked = subprocess.check_output(
    ['git', 'ls-files', '--others', '--exclude-standard', '--', 'docs/plans', 'orchestrator/rounds/round-192', 'test', 'src', 'src-public', 'app', 'mlf2.cabal', 'TODO.md', 'implementation_notes.md', 'Bugs.md', 'orchestrator/roadmaps'],
    text=True,
).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [p for p in paths if p not in allowed]
if extra:
    raise SystemExit('unexpected round-192 scope escape:\n' + '\n'.join(extra))
print('ROUND_192_SCOPE_OK')
PY`
     `if git diff --name-only -- test/ | grep -q .; then cabal build all && cabal test; fi`

## Review Focus

The reviewer should be left one bounded item-6 result that is easy to approve
against `verification.md`:

- the diff stays inside the authorized docs/test surfaces only
- `N1`, `N2`, and `N6` are the only representative rows consumed
- `test/Research/P5ClearBoundarySpec.hs` and `test/PipelineSpec.hs` remain
  the only authoritative evidence harnesses reused by the round
- the aggregate artifact ties every row back to the accepted item-3 contract
  without editing that contract
- no ambiguous, unsafe, cyclic, multi-SCC, equi-recursive, fallback, or
  second-interface case is counted as success
- if tests changed, `cabal build all && cabal test` passed before review
- the round does not smuggle in an item-7 readiness claim or a
  `non-cyclic-graph` decision
