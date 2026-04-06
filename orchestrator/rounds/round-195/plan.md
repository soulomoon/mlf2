# Round 195 Plan

- Round: `round-195`
- Roadmap: `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap` / `rev-001`
- Milestone: `milestone-1`
- Direction: `direction-1b-publish-p5-current-architecture-vs-boundary-gate`
- Extracted item: `publish-p5-current-architecture-vs-boundary-gate`
- Retry: `attempt-2` (`attempt-1` was rejected because the base-branch diff still included `orchestrator/rounds/round-195/implementation-notes.md`)
- Execution shape: serial, same-round retry, docs-only, one extracted item only, no worker fan-out, no concurrent `cabal` jobs, no production/test/Cabal/roadmap/controller-state edits, and no new round-local notes artifact

## Retry Objective

Keep this retry bounded to the already-selected `milestone-1` /
`direction-1b` docs-only gate while repairing the exact scope escape that
caused rejection.

The next lawful attempt is:

1. remove `orchestrator/rounds/round-195/implementation-notes.md` from the
   round result entirely;
2. retain or minimally correct the one canonical docs artifact
   `docs/plans/2026-04-06-post-item-7-p5-current-architecture-vs-boundary-pressure-gate-and-immediate-handoff-decision.md`
   only if a same-round reread finds a concrete milestone-1 mismatch; and
3. leave the reviewer a diff that contains only the selected docs artifact
   plus the existing round-owned `plan.md` / `selection.md`.

Attempt-1 review already says the docs artifact itself is evidence-backed and
milestone-1-correct. The retry therefore exists to repair scope, not to widen
content, open a second artifact, or restate implementation notes elsewhere.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-2`
- Review snapshot:
  `orchestrator/rounds/round-195/reviews/attempt-1.md`
- Retry reason:
  `orchestrator/rounds/round-195/implementation-notes.md` appeared in the
  base-branch diff even though the prior plan did not authorize it
- Review-confirmed safe carry-forward:
  the selected docs artifact already passes the milestone-1 evidence checks,
  keeps the round docs-only, selects one classification token only, and binds
  one next lawful move only
- Active selection input:
  `orchestrator/rounds/round-195/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001`

Read-only existing retry-context files may already be present in the canonical
round worktree and must not be edited during implementation:

- `orchestrator/rounds/round-195/attempt-log.jsonl`
- `orchestrator/rounds/round-195/review.md`
- `orchestrator/rounds/round-195/reviews/attempt-1.md`

The inherited boundary remains controlling:

- explicit-only
- iso-recursive
- non-equi-recursive
- `non-cyclic-graph = unknown`
- no-fallback

The read-only decision chain that still governs this retry is:

- `docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`
- `orchestrator/rounds/round-194/review-record.json`
- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`
- `orchestrator/rounds/round-193/review-record.json`
- `docs/plans/2026-03-28-p5-polymorphism-nested-forall-successor-authority-success-bar-and-writable-slice-freeze.md`
- `docs/plans/2026-03-28-post-implementation-p5-polymorphism-nested-forall-settlement-surface-and-exact-repo-impact-read.md`
- `docs/plans/2026-03-28-post-p5-polymorphism-nested-forall-successor-gate-and-immediate-handoff-decision.md`
- `orchestrator/rounds/round-151/review-record.json`
- `orchestrator/rounds/round-151/implementation-notes.md`
- `implementation_notes.md`
- `TODO.md`
- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`
- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md`
- `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`
- `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`
- `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`
- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `src/MLF/Elab/TermClosure.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`
- `test/Research/P5ClearBoundarySpec.hs`
- `test/PipelineSpec.hs`

## Authorized Write Scope

Authored content for this retry remains limited to exactly one file:

- `docs/plans/2026-04-06-post-item-7-p5-current-architecture-vs-boundary-pressure-gate-and-immediate-handoff-decision.md`

`orchestrator/rounds/round-195/implementation-notes.md` remains outside the
authorized write scope. The only lawful interaction with that path during the
retry is to remove it from the round result so it no longer exists in the
worktree or in the base-branch diff. Do not replace it with new content, a
renamed note, or a second docs artifact.

Do not create or modify:

- `orchestrator/rounds/round-195/selection.md`
- `orchestrator/rounds/round-195/attempt-log.jsonl`
- `orchestrator/rounds/round-195/review.md`
- `orchestrator/rounds/round-195/reviews/**`
- `orchestrator/rounds/round-195/review-record.json`
- `orchestrator/rounds/round-195/merge.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- any accepted predecessor `docs/plans/**` artifact besides the selected
  `round-195` docs artifact above
- any file under `src/`, `src-public/`, `app/`, or `test/`
- `mlf2.cabal`

## Sequential Plan

1. Repair the exact rejected scope escape before reconsidering content.
   - Exact file to remove from the round result:
     `orchestrator/rounds/round-195/implementation-notes.md`
   - Exact file to preserve as the only implementation artifact candidate:
     `docs/plans/2026-04-06-post-item-7-p5-current-architecture-vs-boundary-pressure-gate-and-immediate-handoff-decision.md`
   - Workflow:
     remove `orchestrator/rounds/round-195/implementation-notes.md` from the
     worktree / branch result and keep it absent;
     do not recreate it;
     do not migrate its text into another round-local file;
     do not touch `selection.md`, `review.md`, `attempt-log.jsonl`, or
     `reviews/attempt-1.md`.
   - Exit condition:
     the retry no longer carries any `implementation-notes.md` artifact, and
     the round is back to one-doc-artifact-only scope.
   - Verification:
     `test ! -e orchestrator/rounds/round-195/implementation-notes.md`
     `git diff --name-status codex/automatic-recursive-type-inference...HEAD -- orchestrator/rounds/round-195/implementation-notes.md`
     `git ls-files --others --exclude-standard -- orchestrator/rounds/round-195/implementation-notes.md`

2. Re-verify the selected milestone-1 gate and edit only the canonical docs artifact if a same-round factual mismatch is found.
   - Exact file that may be edited:
     `docs/plans/2026-04-06-post-item-7-p5-current-architecture-vs-boundary-pressure-gate-and-immediate-handoff-decision.md`
   - Exact read-only files that ground the reread:
     `orchestrator/rounds/round-195/selection.md`,
     `orchestrator/state.json`,
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/roadmap.md`,
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/verification.md`,
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/retry-subloop.md`,
     and the decision chain listed above.
   - Workflow:
     treat attempt-1 review as the default carry-forward that the docs artifact
     is already correct;
     leave the file unchanged unless the reread finds a concrete mismatch in
     lineage, milestone-1 evidence, the chosen classification token, or the
     one next lawful move;
     if an edit is needed, keep it confined to this one file only and keep the
     subject bounded to the frozen retained-child guard-cluster lane.
   - Mandatory content constraints if the file is touched:
     preserve exactly one outcome token only
     (`bounded current-architecture continuation` or
     `later explicit boundary-pressure`);
     preserve exactly one next lawful move only;
     keep `nestedForallContrastExpr`, `sameLaneClearBoundaryExpr`, and the
     accepted `round-151` correct-behavior case in predecessor/control roles
     only;
     do not reopen the March 28 packet;
     do not open a fresh `P2` lane;
     do not revise architecture;
     do not add any second artifact or round-local note.
   - Verification:
     `python3 -m json.tool orchestrator/state.json >/dev/null`
     `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/verification.md" && test -f "$roadmap_dir/retry-subloop.md"`
     `test -f docs/plans/2026-04-06-post-item-7-p5-current-architecture-vs-boundary-pressure-gate-and-immediate-handoff-decision.md`
     `rg -n 'round-194|round-193|round-151|current-architecture blockers|N2 unsoundness-guard|runPipelineElab|runPipelineElabChecked|boundHasForallFrom|sameLaneLocalRetainedChildTarget|keepTargetFinal|targetC|preserveRetainedChildAuthoritativeResult' docs/plans/2026-04-06-post-item-7-p5-current-architecture-vs-boundary-pressure-gate-and-immediate-handoff-decision.md docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`
     `rg -n 'boundHasForallFrom|sameLaneLocalRetainedChildTarget|keepTargetFinal|targetC|preserveRetainedChildAuthoritativeResult|runPipelineElab|runPipelineElabChecked|nestedForallContrastExpr|sameLaneClearBoundaryExpr' src/MLF/Elab/Run/ResultType/Fallback/Core.hs src/MLF/Elab/TermClosure.hs src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs test/Research/P5ClearBoundarySpec.hs test/PipelineSpec.hs`
     `rg -n 'bounded current-architecture continuation|later explicit boundary-pressure|next lawful move|immediate handoff|milestone-2|## Non-Claims' docs/plans/2026-04-06-post-item-7-p5-current-architecture-vs-boundary-pressure-gate-and-immediate-handoff-decision.md`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "P5 clear-boundary retained-child probes"'`
     `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps retained-child lookup bounded to the same local TypeRef lane"'`

3. Run the final same-round scope gate with the retry-era round artifacts handled explicitly.
   - Expected round result after this step:
     the selected docs artifact is the only implementation-owned content file;
     `plan.md` and `selection.md` remain as the existing round-owned planning
     artifacts;
     `attempt-log.jsonl`, `review.md`, and `reviews/**` may exist as
     controller/reviewer context but are not implementer outputs;
     `implementation-notes.md` is absent.
   - Workflow:
     verify diff hygiene;
     verify the round result contains no extra tracked or untracked file under
     the watched paths except the allowed docs artifact, the existing
     `plan.md` / `selection.md`, and the read-only retry-context files named
     above;
     stop if any other path appears rather than widening the retry.
   - Verification:
     `git diff --check`
     `python3 - <<'PY'
import subprocess

allowed_exact = {
    'docs/plans/2026-04-06-post-item-7-p5-current-architecture-vs-boundary-pressure-gate-and-immediate-handoff-decision.md',
    'orchestrator/rounds/round-195/plan.md',
    'orchestrator/rounds/round-195/selection.md',
    'orchestrator/rounds/round-195/attempt-log.jsonl',
    'orchestrator/rounds/round-195/review.md',
}
allowed_prefixes = (
    'orchestrator/rounds/round-195/reviews/',
)
tracked = subprocess.check_output(
    [
        'git',
        'diff',
        '--name-only',
        'codex/automatic-recursive-type-inference...HEAD',
        '--',
        'docs/plans',
        'orchestrator/rounds/round-195',
        'TODO.md',
        'implementation_notes.md',
        'Bugs.md',
        'orchestrator/roadmaps',
        'orchestrator/state.json',
        'src',
        'src-public',
        'app',
        'test',
        'mlf2.cabal',
    ],
    text=True,
).splitlines()
untracked = subprocess.check_output(
    [
        'git',
        'ls-files',
        '--others',
        '--exclude-standard',
        '--',
        'docs/plans',
        'orchestrator/rounds/round-195',
        'TODO.md',
        'implementation_notes.md',
        'Bugs.md',
        'orchestrator/roadmaps',
        'orchestrator/state.json',
        'src',
        'src-public',
        'app',
        'test',
        'mlf2.cabal',
    ],
    text=True,
).splitlines()
paths = sorted({p for p in tracked + untracked if p})
extra = []
for path in paths:
    if path in allowed_exact:
        continue
    if any(path.startswith(prefix) for prefix in allowed_prefixes):
        continue
    extra.append(path)
if extra:
    raise SystemExit('unexpected round-195 retry scope escape:\\n' + '\\n'.join(extra))
print('ROUND_195_RETRY_SCOPE_OK')
PY`
     `git status --short -- orchestrator/state.json orchestrator/rounds/round-195 docs/plans/2026-04-06-post-item-7-p5-current-architecture-vs-boundary-pressure-gate-and-immediate-handoff-decision.md`

## Review Focus

The reviewer should see one unambiguous same-round retry result:

- `orchestrator/rounds/round-195/implementation-notes.md` is gone from the
  worktree and from the base-branch diff
- the selected docs artifact remains milestone-1 / direction-1b-only,
  evidence-backed, and non-widening
- the round stays docs-only and bounded to the frozen retained-child
  guard-cluster lane plus one next lawful move only
- existing retry-context files
  (`attempt-log.jsonl`, `review.md`, `reviews/**`) are not counted as new
  implementer-owned outputs
- no source, test, Cabal, roadmap, controller-state, `TODO.md`,
  `implementation_notes.md`, or `Bugs.md` change appears
