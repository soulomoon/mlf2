# Round 180 Plan

- Round: `round-180`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-4`
- Retry: `null`
- Execution shape: docs-only, item-4-only, serial, non-widening readiness contract

## Objective

Publish one docs-only item-4 readiness-contract artifact at:

`docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`

The artifact must define, against the accepted item-2 mechanism map and the
accepted item-3 fail-closed search contract, what counts as
reconstruction-visible recursive success on the authoritative current
surfaces `runPipelineElab`, `runPipelineElabChecked`, and the matching
internal/public pipeline facades; when `TyMu` / `TMu` continuity plus
`ERoll` / `EUnroll` continuity is sufficient evidence for positive `P6`; which
representative positive and negative corpus slices must be rerun before any
broader readiness claim is honest; and what evidence is sufficient versus
insufficient for a later repo-level generality claim.

Current planning read: accepted item-1 already froze the family question and
success bar, accepted item-2 already mapped the live mechanism and named the
authoritative surfaces, and accepted item-3 already froze candidate
generation, ambiguity rejection, and bounded termination to the current named
arms. This round must therefore publish the reconstruction-visible readiness
contract itself. It must not reopen the item-1 freeze, republish item-2 or
item-3 as the deliverable, authorize item-5 implementation, start the item-6
campaign, or imply that the repo has already earned a repo-level readiness or
boundary-revision answer.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input: `orchestrator/rounds/round-180/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `?? orchestrator/rounds/round-180/` is the round-owned directory. Keep the
  round-local artifact set scoped to this plan, the canonical docs artifact,
  and an optional round-local `implementation-notes.md` only.
- `orchestrator/rounds/round-180/selection.md` is the round input and must
  remain untouched.

## Write Scope

Implementer-owned writes for this round are limited to:

- `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`
- `orchestrator/rounds/round-180/implementation-notes.md` only if the
  implementer needs a round-local mirror of the same bounded docs-only result

Do not modify:

- `orchestrator/rounds/round-180/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-180/review.md`
- `orchestrator/rounds/round-180/merge.md`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- any accepted predecessor `docs/plans/**` artifact
- any file under `src/`, `src-public/`, `app/`, or `test/`
- `mlf2.cabal`

This item-4 round stays docs-only and non-widening. The readiness-contract
artifact may cite source and test anchors read-only, but it may not open a
code writable slice, authorize a new interface, reopen cyclic or multi-SCC
search, widen fallback behavior, or imply that one bounded packet already
settles representative `P2` through `P6` or the repo-level readiness
question.

## Sequential Plan

1. Author the item-4 stage contract, authority ledger, and inherited-boundary
   restatement in
   `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`.
   - Use the same top-level artifact style as the accepted item-1 through
     item-3 docs: an explicit header block followed by exact sections
     `## Stage Contract Freeze` and `## Item-4 Authority Ledger`.
   - Mark the artifact explicitly as item `4`, `attempt-1`, `retry: null`,
     docs-only, readiness-contract-only, current-architecture-only, and
     non-widening.
   - Carry forward these exact authority inputs:
     `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-predecessor-authority-unresolved-semantic-matrix-family-success-bar-and-first-concrete-deliverable-freeze.md`,
     `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`,
     `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`,
     `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-capability-contract-and-evaluation-corpus.md`,
     `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-architectural-constraint-audit.md`,
     `docs/plans/2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract.md`,
     `docs/plans/2026-03-27-post-rev-004-repo-scope-refreshed-representative-family-matrix-settlement-surface-and-provenance-validation.md`,
     `docs/plans/2026-04-02-general-automatic-iso-recursive-post-item-2-settlement-surface-and-exact-repo-impact-read.md`,
     `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-2-settlement-surface-and-exact-repo-impact-read.md`,
     `orchestrator/rounds/round-177/review-record.json`,
     `orchestrator/rounds/round-178/review-record.json`, and
     `orchestrator/rounds/round-179/review-record.json`.
   - State explicitly that the inherited production boundary remains
     explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph /
     no-fallback, with no second interface, no cyclic search, no multi-SCC
     search, no equi-recursive widening, and no fallback widening authorized
     here.
   - State explicitly that item `4` does not own the item-5 implementation
     slice, the item-6 negative/termination evidence campaign, or the item-7
     repo-level decision token.

2. Publish the authoritative evaluation-surface map and the actual
   reconstruction-visible readiness contract in that same artifact.
   - Add exact sections
     `## Authoritative Evaluation Surfaces` and
     `## Reconstruction-Visible Readiness Contract`.
   - In `## Authoritative Evaluation Surfaces`, use one reviewer-checkable
     table that distinguishes authoritative output surfaces from supporting
     but non-authoritative continuity evidence.
   - Bind these as the authoritative current surfaces:
     `runPipelineElab`,
     `runPipelineElabChecked`,
     `src/MLF/Elab/Run/Pipeline.hs`,
     `src/MLF/Elab/Pipeline.hs`, and
     `src-public/MLF/Pipeline.hs`.
   - Bind these as read-only supporting seams whose continuity matters but is
     not sufficient by itself:
     `src/MLF/Constraint/Acyclicity.hs`,
     `src/MLF/Reify/Type.hs`,
     `src/MLF/Elab/Elaborate/Algebra.hs`,
     `src/MLF/Elab/TypeCheck.hs`,
     `src/MLF/Elab/Reduce.hs`,
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`, and
     `src/MLF/Elab/TermClosure.hs`.
   - In `## Reconstruction-Visible Readiness Contract`, define the exact
     positive-`P6` bar for the current family: the same item-3-admitted
     candidate family and route must remain identifiable, `TyMu`-side shape
     must survive as reviewable `TMu` output, `ERoll` / `EUnroll` continuity
     must stay consistent with the same recursive story, and both
     authoritative pipeline entrypoints plus the matching public facade must
     expose the recursive structure without disagreement or rescue.
   - State explicitly that solver-only, helper-only, witness-only,
     fallback-only, internal-only, or packet-history-only success is
     insufficient even if `TyMu`, `TMu`, `ERoll`, or `EUnroll` appears in one
     supporting phase.
   - Reuse the March 25 lawful outcome vocabulary explicitly:
     `stable visible persistence`,
     `admitted but not reconstruction-visible / blocker debt`, and
     `fail-closed rejection`.

3. Bind the representative corpus obligations and later-readiness evidence
   thresholds in that same artifact.
   - Add exact sections
     `## Representative Corpus Obligations`,
     `## Evidence Sufficiency And Insufficiency`, and
     `## Non-Claims`.
   - In `## Representative Corpus Obligations`, publish one concrete matrix
     that ties the readiness contract to exact read-only corpus slices and
     family obligations:
     `test/Research/C1AuthoritativeSurfaceSpec.hs` for the non-local
     scheme-alias / base-like packet (`P2` / `P6`);
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` for
     `sameLaneAliasFrameClearBoundaryExpr` and
     `sameLaneDoubleAliasFrameClearBoundaryExpr` (`P3` / `P4` / `P6`);
     `test/Research/P5ClearBoundarySpec.hs` for
     `sameLaneClearBoundaryExpr` and `nestedForallContrastExpr`
     (`P5` / `N2`);
     and `test/PipelineSpec.hs` for the authoritative entrypoint checks around
     automatic `TyMu` / `TMu` introduction and explicit `ERoll` /
     `EUnroll` continuity.
   - Make the corpus obligations concrete enough for reviewers to see which
     representative positive and negative slices must be rerun before any
     broader claim is honest. At minimum, preserve `P2`, `P3`, `P4`, `P5`,
     `P6`, `N1`, `N2`, and `N6` as still-live family obligations rather than
     collapsing them into one packet read.
   - In `## Evidence Sufficiency And Insufficiency`, state exactly what later
     items may count as sufficient evidence for a repo-level readiness claim:
     representative reruns on the authoritative surfaces across the named
     positive and negative slices, with current-architecture continuity that
     matches the item-3 route families and guards. State exactly what remains
     insufficient: one packet only, solver admission only, fallback output
     only, helper-visible continuity only, historical predecessor truth only,
     or any result that would need ambiguity ranking, fallback widening,
     cyclic/multi-SCC search, or a second interface.
   - In `## Non-Claims`, close explicitly with the item-4 boundaries:
     no runtime change,
     no code/test/Cabal edits,
     no item-5 authorization,
     no item-6 or item-7 result,
     no repo-level readiness claim, and
     no boundary revision.

4. Mirror the same bounded execution in
   `orchestrator/rounds/round-180/implementation-notes.md` only if the
   implementer needs a round-local note.
   - If created, state explicitly that the round is docs-only, item-4-only,
     and non-widening.
   - Record that the canonical output is
     `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`.
   - Record that all source, test, Cabal, roadmap, controller-state, and
     repo-root notes files stayed unchanged, and that all cited code/test
     seams remained read-only in this round.
   - If no round-local note is needed, omit it; the canonical docs artifact
     above remains the only intended non-round-local output.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `python3 - <<'PY'
import pathlib, sys
root = pathlib.Path('.')
artifact_path = root / 'docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md'
artifact = artifact_path.read_text()
required_artifact_tokens = [
    'item-4',
    'attempt-1',
    'retry: null',
    'docs-only',
    'readiness-contract-only',
    'current-architecture-only',
    'non-widening',
    '## Stage Contract Freeze',
    '## Item-4 Authority Ledger',
    '## Authoritative Evaluation Surfaces',
    '## Reconstruction-Visible Readiness Contract',
    '## Representative Corpus Obligations',
    '## Evidence Sufficiency And Insufficiency',
    '## Non-Claims',
    'runPipelineElab',
    'runPipelineElabChecked',
    'src/MLF/Elab/Run/Pipeline.hs',
    'src/MLF/Elab/Pipeline.hs',
    'src-public/MLF/Pipeline.hs',
    'src/MLF/Constraint/Acyclicity.hs',
    'src/MLF/Reify/Type.hs',
    'src/MLF/Elab/Elaborate/Algebra.hs',
    'src/MLF/Elab/TypeCheck.hs',
    'src/MLF/Elab/Reduce.hs',
    'src/MLF/Elab/Run/ResultType/Fallback/Core.hs',
    'src/MLF/Elab/TermClosure.hs',
    'TyMu',
    'TMu',
    'ERoll',
    'EUnroll',
    'solver-only',
    'helper-only',
    'witness-only',
    'fallback-only',
    'internal-only',
    'stable visible persistence',
    'admitted but not reconstruction-visible / blocker debt',
    'fail-closed rejection',
    'sameLaneAliasFrameClearBoundaryExpr',
    'sameLaneDoubleAliasFrameClearBoundaryExpr',
    'sameLaneClearBoundaryExpr',
    'nestedForallContrastExpr',
    'test/Research/C1AuthoritativeSurfaceSpec.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'test/Research/P5ClearBoundarySpec.hs',
    'test/PipelineSpec.hs',
    'P2', 'P3', 'P4', 'P5', 'P6',
    'N1', 'N2', 'N6',
    'repo-level readiness',
    '2026-04-02-general-automatic-iso-recursive-full-inference-predecessor-authority-unresolved-semantic-matrix-family-success-bar-and-first-concrete-deliverable-freeze',
    '2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map',
    '2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline',
    '2026-03-25-general-automatic-iso-recursive-inference-full-pipeline-reconstruction-and-validation-contract',
]
for token in required_artifact_tokens:
    if token not in artifact:
        print(f'artifact missing token: {token}')
        sys.exit(1)
notes_path = root / 'orchestrator/rounds/round-180/implementation-notes.md'
if notes_path.exists():
    notes = notes_path.read_text()
    for token in [
        'docs-only',
        'item-4-only',
        'non-widening',
        'canonical output',
        'read-only',
        'docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md',
    ]:
        if token not in notes:
            print(f'implementation-notes missing token: {token}')
            sys.exit(1)
print('ROUND180_ITEM4_READINESS_CONTRACT_OK')
PY`
- `git diff --check`
- `python3 - <<'PY'
import subprocess, sys
allowed = {
    'docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md',
    'orchestrator/rounds/round-180/implementation-notes.md',
    'orchestrator/rounds/round-180/plan.md',
    'orchestrator/rounds/round-180/selection.md',
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
print('ROUND180_DOCS_ONLY_SCOPE_OK')
PY`
