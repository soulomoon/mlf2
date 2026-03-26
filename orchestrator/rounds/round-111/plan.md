# Round 111 Plan (`item-3` Frozen Same-Pocket Evidence-Surface Validation)

## Objective

Execute only roadmap item `3` and prepare one canonical validation artifact
at:
`docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-bounded-amendment-frozen-same-pocket-evidence-surface-validation.md`.

This round is evidence-only. It must rerun only the exact frozen same-pocket
surface already bound by accepted rev-002 item `4`, now against the accepted
rev-003 item `2` bounded handoff amendment:

- the same exact selected rows `C2`, `C5`, and `C7`;
- the same exact packet and tuple;
- the same exact command set frozen by accepted rev-002 item `4`;
- the same exact six review-visible surfaces frozen by accepted rev-002
  item `4`; and
- the same exact one-interface public-entrypoint chain.

The validation must prove or honestly fail-close whether the bounded
implementation:

- preserves helper-visible/internal recursive continuity;
- changes authoritative public output on the same one public-entrypoint
  chain as intended;
- preserves the same clear-boundary and fail-closed guards; and
- avoids subject drift, family widening, or broader capability narration.

This round must preserve `iso-recursive = keep`,
`non-equi-recursive = keep`, and
`no-fallback = keep`;
keep rev-001 items `6` through `8` blocked;
keep multi-SCC search, second interfaces, fallback widening, helper-route
redesign, hardening, rollout, and repo-level success claims blocked;
and keep the read-only audit anchors read-only.

Because item `3` completion is explicitly bound to the exact rev-002 item-4
command set, the canonical artifact must not widen into extra validation
surfaces. The round-owned diff must stay docs/orchestrator-only, and the
review must record any skipped broader gate as an intentional scope guard
rather than silent omission.

## Locked Round Context

- Round id: `round-111`
- Roadmap item: `item-3`
- Stage: `plan`
- Active attempt: `attempt-1`
- Retry state: `null`
- Active branch: `codex/round-111`
- Active worktree:
  `/Users/ares/.codex/worktrees/d432/mlf4/.worktrees/round-111`
- Fixed live subject: exact-pocket validation for the bounded same-family
  authoritative-handoff amendment on the already-selected same-lane
  `C2` / `C5` / `C7` pocket only
- Fixed inherited boundary:
  `explicit-only / iso-recursive / non-equi-recursive / no-fallback / one-interface / no-multi-SCC`

Current worktree state is already non-pristine at the control-plane level.
Respect existing edits and do not revert unrelated work:

- controller-owned `orchestrator/state.json` is modified and must remain
  untouched by the validation artifact work; and
- existing `orchestrator/rounds/round-111/selection.md` is a round input and
  must remain untouched.

Reviewer outcome constraints for this stage remain:

- `accepted + finalize`
- `accepted + retry`
- `rejected + retry`

## Accepted Continuity That Remains Binding

- `orchestrator/rounds/round-111/selection.md`
  fixes this round to roadmap item `3` only, keeps the selected subject
  exact-pocket-only, and explicitly blocks a second implementation round and
  the later post-amendment decision.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/roadmap.md`
  makes item `3` the lowest-numbered unfinished item whose dependencies are
  now satisfied by accepted rounds `109` and `110`.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/retry-subloop.md`
  allows retry for item `3` and requires `Implemented stage result`,
  `Attempt verdict`, `Stage action`, `Retry reason`, and `Fix hypothesis`
  on every review.
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/verification.md`
  requires same-pocket-validation checks proving that this round reruns only
  the exact rows, packet, command set, and six review-visible surfaces
  frozen by accepted rev-002 item `4`, and that the resulting internal /
  public continuity read stays honest and exact-pocket-only.
- `orchestrator/rounds/round-109/review-record.json`
  is the authoritative rev-003 item-1 acceptance record. It froze the exact
  selected pocket, the exact authoritative-handoff slice, the exact writable
  slice, and the exact read-only audit anchors.
- `orchestrator/rounds/round-110/review-record.json`
  is the authoritative rev-003 item-2 acceptance record. It proves the
  bounded amendment landed only in `Pipeline.hs`, `TermClosure.hs`, and
  `PipelineSpec.hs`, and that the old public-output collapse is no longer
  expected on the exact same packet.
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-architecture-pressure-audit-target-and-evaluation-surface-bind.md`
  is the authoritative rev-002 item-4 bind and therefore the direct scope
  contract for this validation round. It freezes the exact module set, exact
  command set, and exact six review-visible surfaces.
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-architecture-amendment-contract-and-writable-slice-freeze.md`
  is the authoritative rev-003 item-1 freeze and keeps the same exact
  writable/read-only boundary binding in this round.
- `src/MLF/Elab/Run/Pipeline.hs`,
  `src/MLF/Elab/TermClosure.hs`,
  and `test/PipelineSpec.hs`
  already carry the accepted item-2 change and are read-only evidence in
  this round, not writable scope.

## File Map

### Create

- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-bounded-amendment-frozen-same-pocket-evidence-surface-validation.md`
  - Responsibility: canonical item-3 validation record for the accepted
    bounded amendment, using only the frozen exact-pocket command set and
    six review-visible surfaces.

### Read-Only Evidence

- `orchestrator/rounds/round-111/selection.md`
- `orchestrator/rounds/round-109/review-record.json`
- `orchestrator/rounds/round-110/review-record.json`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/roadmap.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/retry-subloop.md`
- `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/verification.md`
- `docs/plans/2026-03-14-automatic-recursive-inference-baseline-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-representative-coverage-and-feasibility-campaign.md`
- `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architecture-decision-and-successor-plan-choice.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-keep-vs-reopen-decision-gate.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-reopened-revision-authority-and-candidate-boundary-freeze.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-reopened-subject-selection.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-safety-and-acceptance-contract.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-architecture-pressure-audit-target-and-evaluation-surface-bind.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-architecture-amendment-lane-open-or-stop-decision.md`
- `docs/plans/2026-03-26-global-non-cyclic-graph-same-lane-retained-child-public-output-continuity-authoritative-handoff-architecture-amendment-contract-and-writable-slice-freeze.md`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/ResultType.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Run.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`
- `src/MLF/Elab/TermClosure.hs`
- `test/PipelineSpec.hs`

### Preserve Unchanged

- `orchestrator/state.json`
- `orchestrator/rounds/round-111/selection.md`
- roadmap files under
  `orchestrator/roadmaps/2026-03-26-01-global-non-cyclic-graph-settlement-and-automatic-iso-recursive-inference-roadmap/rev-003/`
- `Bugs.md`
- `TODO.md`
- `implementation_notes.md`
- `CHANGELOG.md`
- `src/`
- `src-public/`
- `app/`
- `test/`
- `mlf2.cabal`
- every accepted predecessor docs artifact and review record

No round-local `implementation-notes.md` is authorized. This round should
produce only the canonical validation artifact plus the standard round review
packet.

## Exact Frozen Command Set To Rerun

The round must rerun only these exact command surfaces from accepted rev-002
item `4`:

- `cabal repl mlf2-test --repl-options=-ignore-dot-ghci`
- `:module + *PipelineSpec`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child fallback fail-closed when the same wrapper crosses a nested forall boundary"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet clears Phase 6 elaboration"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact packet authoritative public output stays forall identity"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "same-lane retained-child exact edge 3 authoritative instantiation"'`
- `rg -n 'boundVarTargetRoot =|boundHasForallFrom start0 =|sameLaneLocalRetainedChildTarget =|keepTargetFinal =|case sameLaneLocalRetainedChildTarget of|not hasForall' src/MLF/Elab/Run/ResultType/Fallback.hs`
- `rg -n 'computeResultTypeFallback|runPipelineElab =|runPipelineElabChecked =|runPipelineElabWith|termClosed|checkedAuthoritative =|typeCheck termClosed' src/MLF/Elab/Run/ResultType.hs src/MLF/Elab/Run/Pipeline.hs`
- `rg -n 'runPipelineElab|runPipelineElabChecked|runPipelineElabWith' src/MLF/Elab/Run.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs`
- `rg -n 'same-lane retained-child exact packet|same-lane local TypeRef root|same local TypeRef lane|nested forall boundary|edge 3 authoritative instantiation' test/PipelineSpec.hs`

Within the REPL, the replay must stay on the same exact packet and reuse the
same exact helper-visible same-lane rewiring anchored in `PipelineSpec.hs`.
The canonical artifact must record the resulting fallback/internal read,
`containsMu`, and both authoritative public outputs for that same packet
only.

## Expected Validation Questions

The canonical artifact must answer exactly these bounded questions:

1. Does the same helper-visible internal same-lane route still reconstruct
   recursive structure on the selected packet?
2. Do the authoritative public entrypoints now carry bounded recursive
   structure on that same packet, or do they still collapse?
3. Do the same clear-boundary and nested-`forall` fail-closed guards remain
   intact?
4. Does the source/test evidence remain on the same exact route and one
   public-entrypoint chain only?

If any answer is negative or mixed, the artifact must record that honestly as
exact-pocket evidence only. It must not widen into a second packet,
second interface, or broader capability judgment.

## Review Checklist

- `git diff --check`
- `python3 -m json.tool orchestrator/state.json >/dev/null`
- prove the round-owned diff is docs/orchestrator-only apart from the
  controller-owned `orchestrator/state.json`
- prove the canonical artifact and plan both fix the round to item `3`
  exact-pocket validation only
- prove the canonical artifact records only the frozen command set and the
  frozen six review-visible surfaces
- prove the read-only audit anchors remain read-only
- prove no round-local `implementation-notes.md` was created
- if the canonical artifact stays docs-only, record the broader Cabal gate as
  intentionally out of scope for this round because item `3` is bound to the
  exact rev-002 item-4 command set

## Finish Condition

The round is ready for review when:

- the canonical validation artifact exists at the exact path above;
- it records the fresh replay/test/source evidence from only the frozen
  command set;
- it classifies the internal/public continuity outcome honestly without
  subject drift; and
- no file outside the allowed docs/orchestrator packet has changed.
