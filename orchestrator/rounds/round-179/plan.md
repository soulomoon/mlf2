# Round 179 Plan

- Round: `round-179`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-3`
- Retry: `null`
- Execution shape: docs-only, item-3-only, serial, non-widening search-contract

## Objective

Publish one docs-only item-3 search-contract artifact at:

`docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`

The artifact must define, against the accepted item-1 freeze and item-2
mechanism map, a fail-closed contract for the currently named route arms
only: when `rootNonLocalSchemeAliasBaseLike` may introduce or preserve
recursive candidates; when `sameLaneLocalRetainedChildTarget` plus
`boundHasForallFrom`, `keepTargetFinal`, and `targetC` may preserve a
same-lane retained-child candidate; how competing anchors / owners /
binder-side placements are rejected instead of ranked; and why
representative `N1`, `N2`, and `N6` pressure remains bounded without
fallback, heuristic guessing, cyclic search, multi-SCC handling, or
equi-recursive equality.

Current planning read: accepted item-2 already explains the live mechanism
honestly and explicitly leaves candidate-generation, ambiguity rejection, and
bounded termination discipline to item `3`. This round must therefore publish
that search contract itself, not restate item-1, not republish item-2 as the
main deliverable, not define the item-4 reconstruction-visible readiness
contract, not authorize item-5 implementation, and not imply a repo-level
readiness or boundary-revision answer.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input: `orchestrator/rounds/round-179/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `?? orchestrator/rounds/round-179/` is the round-owned directory. Keep the
  round-local artifact set scoped to this plan, the canonical docs artifact,
  and an optional round-local `implementation-notes.md` only.
- `orchestrator/rounds/round-179/selection.md` is the round input and must
  remain untouched.

## Write Scope

Implementer-owned writes for this round are limited to:

- `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`
- `orchestrator/rounds/round-179/implementation-notes.md` only if the
  implementer needs a round-local mirror of the same bounded docs-only result

Do not modify:

- `orchestrator/rounds/round-179/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-179/review.md`
- `orchestrator/rounds/round-179/merge.md`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- any accepted predecessor `docs/plans/**` artifact
- any file under `src/`, `src-public/`, `app/`, or `test/`
- `mlf2.cabal`

This item-3 round stays docs-only and non-widening. The search-contract
artifact may cite source and test anchors read-only, and it may include
bounded pseudo-algorithm or harness-planning detail only if that detail stays
docs-first and leaves runtime semantics unchanged. It may not open a code
writable slice, define the item-4 reconstruction-visible readiness contract,
authorize item-5 implementation, smuggle in fallback or search widening, or
imply that the repo has already earned a full-inference readiness answer.

## Sequential Plan

1. Author the item-3 stage contract, authority ledger, and inherited-boundary
   restatement in
   `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`.
   - Use the same top-level artifact style as the accepted item-1 and item-2
     docs: an explicit header block followed by exact sections
     `## Stage Contract Freeze` and `## Item-3 Authority Ledger`.
   - Mark the artifact explicitly as item `3`, `attempt-1`, `retry: null`,
     docs-only, search-contract-only, current-architecture-only, and
     non-widening.
   - Carry forward these exact authority inputs:
     `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-predecessor-authority-unresolved-semantic-matrix-family-success-bar-and-first-concrete-deliverable-freeze.md`,
     `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`,
     `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`,
     `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`,
     `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`,
     `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-mechanism-map.md`,
     `docs/plans/2026-03-28-same-lane-retained-child-representative-gap-successor-authority-exact-subject-success-bar-and-writable-slice-freeze.md`,
     `docs/plans/2026-03-29-same-lane-alias-frame-representative-gap-post-item-2-settlement-surface-and-repo-impact-read.md`,
     `docs/plans/2026-04-01-general-automatic-iso-recursive-successor-authority-exact-inherited-blocker-lane-and-writable-slice-freeze.md`,
     `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md`,
     `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md`,
     `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-successor-authority-next-exact-representative-gap-packet-current-live-read-success-bar-and-writable-slice-freeze.md`,
     `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-2-settlement-surface-and-exact-repo-impact-read.md`,
     `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md`,
     `orchestrator/rounds/round-177/review-record.json`, and
     `orchestrator/rounds/round-178/review-record.json`.
   - State explicitly that the inherited production boundary remains
     explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph /
     no-fallback, with no second interface, no cyclic search, no multi-SCC
     search, no equi-recursive widening, and no fallback widening authorized
     here.
   - State explicitly that item `3` does not own the item-4
     reconstruction-visible readiness contract, the item-5 implementation
     slice, the item-6 evidence campaign, or the item-7 repo-level decision
     token.

2. Publish the exact candidate-generation and candidate-preservation contract
   in that same artifact.
   - Add an exact section `## Candidate-Generation Contract` followed by one
     concrete route-arm matrix or equivalent reviewer-checkable structure.
   - Populate that contract for the currently named route arms only:
     `rootNonLocalSchemeAliasBaseLike`,
     `sameLaneLocalRetainedChildTarget`, and the retained-child guard cluster
     centered on `boundHasForallFrom`, `keepTargetFinal`, and `targetC`.
   - For each named route arm, define all of the following explicitly:
     the controlling read-only anchors, when a recursive candidate may be
     introduced, when an already-discovered candidate may be preserved, which
     guard facts must already hold, and which conditions force fail-closed
     rejection instead of broadening.
   - Preserve
     `sameLaneAliasFrameClearBoundaryExpr`,
     `sameLaneDoubleAliasFrameClearBoundaryExpr`, and the March 25 non-local
     alias-bound / base-like packet as bounded predecessor evidence only. The
     artifact must say clearly that those packets justify route-specific
     obligations here, not a family-wide search law or a repo-level readiness
     claim.
   - State explicitly that `schemeBodyTarget`, `boundTarget`, extra owners,
     extra binders, and any unnamed route remain neighboring context or
     rejection territory only unless a named item-3 route arm and its guard
     facts already justify them without widening.

3. Publish the fail-closed ambiguity and unsoundness contract in that same
   artifact.
   - Add an exact section `## Ambiguity Rejection And Soundness Guards`.
   - Include one concrete comparison / rejection table or equivalent
     reviewer-checkable structure covering at minimum:
     competing non-local versus same-lane anchors,
     conflicting owners,
     conflicting binder-side placements,
     nested-`forall` crossings,
     retained-child preservation attempts without the full
     `boundHasForallFrom` / `keepTargetFinal` / `targetC` guard story, and
     any route that would require fallback, cyclic search, multi-SCC search,
     equi-recursive equality, or a second interface.
   - State explicitly that multiple surviving anchors, owners, or
     binder-side placements do not trigger ranking, scoring, tie-breaking, or
     heuristic guessing. They remain rejection territory under `N1`.
   - Tie `N2` to the exact guard failures already visible in the current
     mechanism map: if the route cannot preserve owner / binder / retained
     child discipline with the named guard cluster, it must reject instead of
     inventing a broader admissibility rule.
   - Keep the clear-boundary versus nested-`forall` contrast from
     `test/Research/P5ClearBoundarySpec.hs` as fail-closed context only. Do
     not turn that contrast into a general positive `P5` rule.

4. Publish the bounded termination discipline and non-claims in that same
   artifact.
   - Add exact sections `## Bounded Termination Discipline` and
     `## Non-Claims`.
   - Explain why the candidate space remains finite and serial for this item:
     only the two named route families plus the one retained-child guard
     cluster are admitted; checks stay local to the already named anchors;
     `src/MLF/Constraint/Acyclicity.hs` and `src/MLF/Reify/Type.hs` remain
     read-only evidence for the inherited acyclic representation rather than a
     license for cyclic or multi-SCC search; and no repeated fallback retries,
     graph widening, or equi-recursive equality are authorized.
   - If bounded pseudo-algorithm or harness-planning detail is included, keep
     it docs-only and tie it back to read-only anchors only:
     `src/MLF/Constraint/Acyclicity.hs`,
     `src/MLF/Reify/Type.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`,
     `src/MLF/Elab/TermClosure.hs`,
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`,
     `test/Research/P5ClearBoundarySpec.hs`, and
     `test/PipelineSpec.hs`.
   - Close with exact non-claims:
     no change to runtime semantics,
     no item-4 reconstruction-visible readiness definition,
     no item-5 implementation authorization,
     no repo-level readiness claim,
     no boundary revision,
     and no source / test / Cabal / roadmap / controller-state edits.

5. Mirror the same bounded execution in
   `orchestrator/rounds/round-179/implementation-notes.md` only if the
   implementer needs a round-local note.
   - If created, state explicitly that the round is docs-only, item-3-only,
     and non-widening.
   - Record that the canonical output is
     `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`.
   - Record that all source, test, Cabal, roadmap, controller-state, and
     repo-root notes files stayed unchanged, and that any cited seams remained
     read-only in this round.
   - If no round-local note is needed, omit it; the canonical docs artifact
     above remains the only intended non-round-local output.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `python3 - <<'PY'
import pathlib, sys
root = pathlib.Path('.')
artifact_path = root / 'docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md'
artifact = artifact_path.read_text()
required_artifact_tokens = [
    'item-3',
    'attempt-1',
    'retry: null',
    'docs-only',
    'search-contract-only',
    'current-architecture-only',
    'non-widening',
    '## Stage Contract Freeze',
    '## Item-3 Authority Ledger',
    '## Candidate-Generation Contract',
    '## Ambiguity Rejection And Soundness Guards',
    '## Bounded Termination Discipline',
    '## Non-Claims',
    'rootNonLocalSchemeAliasBaseLike',
    'sameLaneLocalRetainedChildTarget',
    'boundHasForallFrom',
    'keepTargetFinal',
    'targetC',
    'sameLaneAliasFrameClearBoundaryExpr',
    'sameLaneDoubleAliasFrameClearBoundaryExpr',
    'candidate generation',
    'ambiguity rejection',
    'bounded termination',
    'N1',
    'N2',
    'N6',
    'heuristic guessing',
    'cyclic search',
    'multi-SCC',
    'equi-recursive equality',
    'reconstruction-visible readiness',
    'repo-level readiness',
    'runtime semantics unchanged',
    'explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback',
    '2026-04-02-general-automatic-iso-recursive-full-inference-predecessor-authority-unresolved-semantic-matrix-family-success-bar-and-first-concrete-deliverable-freeze',
    '2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map',
    '2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus',
    '2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit',
    '2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract',
    '2026-03-25-general-automatic-iso-recursive-inference-mechanism-map',
    'orchestrator/rounds/round-177/review-record.json',
    'orchestrator/rounds/round-178/review-record.json',
    'src/MLF/Constraint/Acyclicity.hs',
    'src/MLF/Reify/Type.hs',
    'src/MLF/Elab/Run/ResultType/Fallback/Core.hs',
    'src/MLF/Elab/TermClosure.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'test/Research/P5ClearBoundarySpec.hs',
    'test/PipelineSpec.hs',
]
for token in required_artifact_tokens:
    if token not in artifact:
        print(f'artifact missing token: {token}')
        sys.exit(1)
notes_path = root / 'orchestrator/rounds/round-179/implementation-notes.md'
if notes_path.exists():
    notes = notes_path.read_text()
    for token in [
        'docs-only',
        'item-3-only',
        'non-widening',
        'docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md',
    ]:
        if token not in notes:
            print(f'implementation-notes missing token: {token}')
            sys.exit(1)
print('ROUND179_ITEM3_SEARCH_CONTRACT_OK')
PY`
- `git diff --check`
- `python3 - <<'PY'
import subprocess, sys
allowed = {
    'docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md',
    'orchestrator/rounds/round-179/implementation-notes.md',
    'orchestrator/rounds/round-179/plan.md',
    'orchestrator/rounds/round-179/selection.md',
}
tracked = subprocess.check_output(['git', 'diff', '--name-only', 'HEAD'], text=True).splitlines()
untracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [p for p in paths if p not in allowed and p != 'orchestrator/state.json']
forbidden = [
    p for p in paths
    if p in {'mlf2.cabal', 'TODO.md', 'implementation_notes.md', 'Bugs.md'}
    or p.startswith('src/')
    or p.startswith('src-public/')
    or p.startswith('app/')
    or p.startswith('test/')
    or p.startswith('orchestrator/roadmaps/')
]
if forbidden or extra:
    if forbidden:
        print('FORBIDDEN_PATHS:')
        print('\n'.join(forbidden))
    if extra:
        print('OUT_OF_SCOPE_PATHS:')
        print('\n'.join(extra))
    sys.exit(1)
print('ROUND179_DOCS_ONLY_SCOPE_OK')
PY`
