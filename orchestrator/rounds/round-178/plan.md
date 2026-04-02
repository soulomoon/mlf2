# Round 178 Plan

- Round: `round-178`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-2`
- Retry: `null`
- Execution shape: docs-only, item-2-only, serial, non-widening mechanism map

## Objective

Publish one docs-only item-2 mechanism-map artifact at:

`docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`

The artifact must explain, against the live repo state and accepted
predecessor docs, how the current implementation does or does not support
recursive-shape discovery, non-local propagation, owner-sensitive placement,
binder-sensitive placement, polymorphism / nested-`forall` interaction, and
reconstruction visibility inside the inherited explicit-only / iso-recursive /
non-equi-recursive / non-cyclic-graph / no-fallback boundary. It must keep
settled packet truth distinct from still-missing general rules, identify the
smallest lawful code-facing seams for later bounded rounds, and avoid any
repo-level readiness or boundary-revision claim.

Current planning read: accepted item-1 already froze the authority chain, the
still-live `P2`-`P6` plus `N1` / `N2` / `N6` obligations, and the exact
fail-closed writable slice as one docs-only mechanism-map artifact. This round
must therefore publish that mechanism map itself, not a second freeze, not an
implementation plan, not a code campaign, and not a copy of the March 25
mechanism vocabulary divorced from the accepted April packet chain and the
current code-facing seams.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input: `orchestrator/rounds/round-178/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `?? orchestrator/rounds/round-178/` is the round-owned directory. Keep the
  round-local artifact set scoped to this plan, the canonical docs artifact,
  and an optional round-local `implementation-notes.md` only.

## Write Scope

Implementer-owned writes for this round are limited to:

- `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`
- `orchestrator/rounds/round-178/implementation-notes.md` only if the
  implementer needs a round-local mirror of the same bounded docs-only result

Do not modify:

- `orchestrator/rounds/round-178/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-178/review.md`
- `orchestrator/rounds/round-178/merge.md`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- any accepted predecessor `docs/plans/**` artifact
- any file under `src/`, `src-public/`, `app/`, or `test/`
- `mlf2.cabal`

This item-2 round stays docs-only and non-widening. The mechanism map may cite
source and test anchors read-only, but it may not open a code writable slice,
smuggle in an implementation campaign, define item-3 candidate-generation or
termination rules, define the item-4 reconstruction-visible readiness
contract, or imply that the repo has already earned a full-inference
readiness answer.

## Sequential Plan

1. Author the item-2 stage contract, authority ledger, and inherited-boundary
   restatement in
   `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`.
   - Mark the artifact explicitly as item `2`, `attempt-1`, `retry: null`,
     docs-only, mechanism-map-only, current-architecture-only, and
     non-widening.
   - Carry forward the exact authority inputs that control this artifact:
     `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-predecessor-authority-unresolved-semantic-matrix-family-success-bar-and-first-concrete-deliverable-freeze.md`,
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
     `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-2-settlement-surface-and-exact-repo-impact-read.md`, and
     `docs/plans/2026-04-02-general-automatic-iso-recursive-current-architecture-follow-on-post-item-3-successor-decision-and-immediate-handoff-after-bounded-lane.md`.
   - State explicitly that the inherited production boundary remains
     explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph /
     no-fallback, with no second interface, no cyclic search, no multi-SCC
     search, no equi-recursive widening, and no fallback widening authorized
     here.
   - State explicitly that item `2` does not own search policy, ambiguity
     ranking, termination discipline, reconstruction-visible readiness
     criteria, implementation slices, or a repo-level decision token.

2. Publish the actual current-architecture semantic mechanism map in that same
   artifact.
   - Organize the core mechanism read around the required semantic axes:
     recursive-shape discovery,
     non-local propagation,
     owner-sensitive placement,
     binder-sensitive placement,
     polymorphism / nested-`forall` interaction, and
     reconstruction visibility.
   - For each axis, separate three reads explicitly:
     settled predecessor fragments already carried into this family,
     the current mechanism visible in read-only repo anchors today, and
     the still-missing general rule or debt that later items must own.
   - Preserve
     `sameLaneAliasFrameClearBoundaryExpr` and
     `sameLaneDoubleAliasFrameClearBoundaryExpr`
     as bounded `narrow success` predecessor packets only; the artifact must
     say clearly that they do not by themselves settle general `P2`, `P3`,
     `P4`, or `P6`, and they do not answer the repo-level readiness question.
   - Carry forward the March 25 bounded mechanism vocabulary honestly:
     the non-local alias-bound / base-like
     `rootNonLocalSchemeAliasBaseLike` route and the same-lane retained-child
     `sameLaneLocalRetainedChildTarget` route may be used as explanatory
     predecessor fragments, but only if the artifact says where packet-local
     handling still remains and why those named arms are not yet a general
     account.
   - Keep `P2` through `P6` explicitly live at family scope, keep `N1`,
     `N2`, and `N6` present as still-live pressure or fail-closed context, and
     keep `N3`, `N4`, and `N5` explicitly out of scope under the inherited
     boundary unless a later accepted revision changes that.

3. Add one exact "smallest lawful code-facing seams" section to the canonical
   artifact, but keep every seam read-only in this round.
   - Name the exact source anchors that explain the current mechanism shape:
     `src/MLF/Constraint/Acyclicity.hs` for
     `breakCyclesAndCheckAcyclicity` and `TyMu` introduction,
     `src/MLF/Reify/Type.hs` for `TMu` reification,
     `src/MLF/Elab/Elaborate/Algebra.hs` for `ERoll` / `EUnroll` emission,
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` for
     `rootNonLocalSchemeAliasBaseLike` and
     `sameLaneLocalRetainedChildTarget`,
     `src/MLF/Elab/TermClosure.hs` for
     `preserveRetainedChildAuthoritativeResult`,
     `src/MLF/Elab/Run/Pipeline.hs` for
     `runPipelineElab` / `runPipelineElabChecked`,
     `src/MLF/Elab/Pipeline.hs` and `src-public/MLF/Pipeline.hs` for
     authoritative internal/public surface continuity,
     `src/MLF/Elab/TypeCheck.hs` for post-elaboration recursive acceptance,
     and `src/MLF/Elab/Reduce.hs` for roll/unroll reduction continuity.
   - Name the exact read-only evidence anchors that the docs may cite:
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`,
     `test/Research/P5ClearBoundarySpec.hs`, and
     `test/PipelineSpec.hs`.
   - Explain these as the smallest seams later bounded rounds may lawfully
     touch only after later item authorization. The item-2 artifact must not
     relabel them as an opened writable slice, and it must not imply that this
     round authorizes any source/test/Cabal change.

4. Mirror the same bounded execution in
   `orchestrator/rounds/round-178/implementation-notes.md` only if the
   implementer decides a round-local note is needed.
   - If created, state explicitly that the round is docs-only, item-2-only,
     current-architecture-only, and non-widening.
   - Record that the canonical output is
     `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`.
   - Record that all source, test, Cabal, roadmap, controller-state, and
     repo-root notes files stayed unchanged, and that any cited code/test
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
artifact_path = root / 'docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md'
artifact = artifact_path.read_text()
required_artifact_tokens = [
    'item-2',
    'attempt-1',
    'retry: null',
    'sameLaneAliasFrameClearBoundaryExpr',
    'sameLaneDoubleAliasFrameClearBoundaryExpr',
    'rootNonLocalSchemeAliasBaseLike',
    'sameLaneLocalRetainedChildTarget',
    'recursive-shape discovery',
    'non-local propagation',
    'owner-sensitive placement',
    'binder-sensitive placement',
    'polymorphism / nested-`forall`',
    'reconstruction visibility',
    'settled predecessor fragments',
    'still-missing general rules',
    'packet-specific',
    'P2', 'P3', 'P4', 'P5', 'P6',
    'N1', 'N2', 'N3', 'N4', 'N5', 'N6',
    'repo-level readiness',
    'explicit-only / iso-recursive / non-equi-recursive / non-cyclic-graph / no-fallback',
    'src/MLF/Constraint/Acyclicity.hs',
    'src/MLF/Reify/Type.hs',
    'src/MLF/Elab/Elaborate/Algebra.hs',
    'src/MLF/Elab/Run/ResultType/Fallback/Core.hs',
    'src/MLF/Elab/TermClosure.hs',
    'src/MLF/Elab/Run/Pipeline.hs',
    'src/MLF/Elab/Pipeline.hs',
    'src-public/MLF/Pipeline.hs',
    'src/MLF/Elab/TypeCheck.hs',
    'src/MLF/Elab/Reduce.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'test/Research/P5ClearBoundarySpec.hs',
    'test/PipelineSpec.hs',
]
for token in required_artifact_tokens:
    if token not in artifact:
        print(f'artifact missing token: {token}')
        sys.exit(1)
notes_path = root / 'orchestrator/rounds/round-178/implementation-notes.md'
if notes_path.exists():
    notes = notes_path.read_text()
    for token in [
        'docs-only',
        'item-2-only',
        'non-widening',
        'current-architecture',
        'canonical output',
        'no source',
        'read-only',
    ]:
        if token not in notes:
            print(f'implementation-notes missing token: {token}')
            sys.exit(1)
print('ROUND178_ITEM2_DOC_TOKENS_OK')
PY`
- `git diff --check`
- `python3 - <<'PY'
import subprocess, sys
allowed = {
    'docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md',
    'orchestrator/rounds/round-178/implementation-notes.md',
    'orchestrator/rounds/round-178/plan.md',
    'orchestrator/rounds/round-178/selection.md',
}
tracked = subprocess.check_output(['git', 'diff', '--name-only', 'HEAD'], text=True).splitlines()
untracked = subprocess.check_output(['git', 'ls-files', '--others', '--exclude-standard'], text=True).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [
    p for p in paths
    if p not in allowed
    and p != 'orchestrator/state.json'
]
forbidden = [
    p for p in paths
    if p == 'mlf2.cabal'
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
print('ROUND178_DOCS_ONLY_SCOPE_OK')
PY`
