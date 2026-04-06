# Round 194 Plan

- Round: `round-194`
- Roadmap: `2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap` / `rev-001`
- Milestone: `milestone-1`
- Direction: `direction-1a-freeze-p5-authority-and-success-bar`
- Extracted item: `post-item-7-p5-successor-gate-freeze`
- Retry: `attempt-2` (`scope-conformance repair for round-194/implementation-notes.md`)
- Execution shape: serial, docs-only, same-round retry, one selected
  milestone-1 / direction-1a extraction only, no worker fan-out, no
  source/test/Cabal/roadmap/controller-state edits, no concurrent `cabal`
  jobs

## Objective

Preserve the already-lawful milestone-1 freeze content and repair only the
rejected review mismatch: the branch diff against
`codex/automatic-recursive-type-inference` must no longer include
`orchestrator/rounds/round-194/implementation-notes.md`.

This retry is not a redesign of the selected extraction. The accepted-content
read from the rejected review remains binding:

- the docs artifact already cites accepted `round-193`, the March 28 `P5`
  freeze / settlement / successor-gate chain, and accepted `round-151`
  honestly;
- it already keeps `nestedForallContrastExpr` closed as settled predecessor
  truth only;
- it already freezes one exact post-item-7 `P5` lane, one
  authoritative-surface success bar, and one writable slice only; and
- it already keeps the current-architecture versus boundary-pressure read
  docs-only and non-authorizing.

The only blocker is scope. The rejected attempt's base-branch diff contained:

- `docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`
- `orchestrator/rounds/round-194/implementation-notes.md`
- `orchestrator/rounds/round-194/plan.md`
- `orchestrator/rounds/round-194/selection.md`

The next lawful attempt must reduce that branch diff to:

- `docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`
- `orchestrator/rounds/round-194/plan.md`
- `orchestrator/rounds/round-194/selection.md`

and nothing else in the selected extraction.

## Locked Retry Context

- Stage: `plan`
- Attempt: `attempt-2`
- Latest attempt verdict: `rejected`
- Latest stage action: `retry`
- Retry reason: the committed round diff against
  `codex/automatic-recursive-type-inference` includes
  `orchestrator/rounds/round-194/implementation-notes.md`, which exceeds the
  write scope frozen by the prior `plan.md`
- Fix hypothesis: remove
  `orchestrator/rounds/round-194/implementation-notes.md` from the round diff
  and keep the branch limited to the authorized docs artifact plus the
  round-owned `selection.md` / `plan.md`, then rerun review

The rejected review authority that this retry must satisfy remains:

- `orchestrator/rounds/round-194/review.md`
- `orchestrator/rounds/round-194/reviews/attempt-1.md`
- `orchestrator/rounds/round-194/attempt-log.jsonl`

The canonical selected extraction that must remain bounded remains:

- `docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`

The read-only authority chain that still controls that artifact remains:

- `orchestrator/rounds/round-194/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/roadmap.md`
- `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/verification.md`
- `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/retry-subloop.md`
- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-repo-level-readiness-and-architecture-decision.md`
- `orchestrator/rounds/round-193/review-record.json`
- `docs/plans/2026-03-28-p5-polymorphism-nested-forall-successor-authority-success-bar-and-writable-slice-freeze.md`
- `docs/plans/2026-03-28-post-implementation-p5-polymorphism-nested-forall-settlement-surface-and-exact-repo-impact-read.md`
- `docs/plans/2026-03-28-post-p5-polymorphism-nested-forall-successor-gate-and-immediate-handoff-decision.md`
- `orchestrator/rounds/round-151/implementation-notes.md`
- `implementation_notes.md`
- `TODO.md`
- `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-reconstruction-visible-readiness-contract-and-authoritative-evaluation-surfaces.md`
- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-positive-family-aggregate-classification.md`
- `docs/plans/2026-04-05-general-automatic-iso-recursive-full-inference-negative-family-and-termination-pressure-aggregate-classification.md`
- `docs/plans/2026-04-02-general-automatic-iso-recursive-full-inference-current-architecture-semantic-mechanism-map.md`
- `docs/plans/2026-04-03-general-automatic-iso-recursive-full-inference-fail-closed-candidate-generation-ambiguity-rejection-and-bounded-termination-discipline.md`
- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `src/MLF/Elab/TermClosure.hs`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`
- `test/Research/P5ClearBoundarySpec.hs`

Current worktree state is already non-pristine. Respect unrelated edits and do
not revert them. In particular:

- `M orchestrator/state.json` is controller-owned and must remain untouched.
- `orchestrator/rounds/round-194/review.md`,
  `orchestrator/rounds/round-194/reviews/attempt-1.md`, and
  `orchestrator/rounds/round-194/attempt-log.jsonl` are reviewer-owned retry
  history and must remain untouched.

The inherited boundary remains controlling:

- explicit-only
- iso-recursive
- non-equi-recursive
- `non-cyclic-graph = unknown`
- no-fallback

## Write Scope

Implementer-owned writes for retry attempt `2` are limited to:

- `docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`
  only if a wording repair is strictly needed to preserve the already-accepted
  milestone-1 freeze content

`orchestrator/rounds/round-194/implementation-notes.md` remains outside the
authorized write scope for this retry. Do not author it, refresh it, or keep
it in the final branch diff. The only lawful scope outcome for that path is
absence from `git diff --name-only codex/automatic-recursive-type-inference...HEAD`.

Do not modify:

- `orchestrator/rounds/round-194/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-194/review.md`
- `orchestrator/rounds/round-194/reviews/attempt-1.md`
- `orchestrator/rounds/round-194/attempt-log.jsonl`
- `orchestrator/rounds/round-194/review-record.json`
- `orchestrator/rounds/round-194/merge.md`
- `orchestrator/rounds/round-194/implementation-notes.md`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- any accepted predecessor `docs/plans/**` artifact
- any file under `src/`, `src-public/`, `app/`, or `test/`
- `mlf2.cabal`
- `worker-plan.json`

No round-local implementation-notes handoff is authorized in this retry. No
fresh implementation, corpus expansion, roadmap mutation, retry-log rewrite,
or boundary-decision artifact is authorized either.

## Sequential Plan

1. Reconfirm that retry attempt `2` is scope-repair only and that the
   milestone-1 freeze content remains the same selected extraction.
   - Modify no files in this step.
   - Read-only files:
     `orchestrator/rounds/round-194/review.md`,
     `orchestrator/rounds/round-194/reviews/attempt-1.md`,
     `orchestrator/rounds/round-194/attempt-log.jsonl`,
     `orchestrator/rounds/round-194/selection.md`,
     `orchestrator/state.json`,
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/roadmap.md`,
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/verification.md`,
     `orchestrator/roadmaps/2026-04-05-00-general-automatic-iso-recursive-p5-and-p2-follow-on-roadmap/rev-001/retry-subloop.md`,
     and
     `docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`.
   - Reconfirm explicitly that the rejected review found no milestone-1
     content defect in the docs artifact; it found only the
     `orchestrator/rounds/round-194/implementation-notes.md` scope escape.
   - Verification:
     `python3 -m json.tool orchestrator/state.json >/dev/null`
     `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/verification.md" && test -f "$roadmap_dir/retry-subloop.md"`
     `rg -n 'Retry reason|Fix hypothesis|implementation-notes\\.md|authorized docs artifact plus the round-owned' orchestrator/rounds/round-194/review.md orchestrator/rounds/round-194/reviews/attempt-1.md orchestrator/rounds/round-194/attempt-log.jsonl`
     `rg -n 'round-193|round-151|sameLaneClearBoundaryExpr|nestedForallContrastExpr|runPipelineElab|runPipelineElabChecked|writable slice|boundary-pressure|polymorphic mediation|## Non-Claims' docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`

2. Keep the selected extraction bounded to the one docs artifact and do not
   reopen round-local notes.
   - Modify only
     `docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`,
     and only if a wording repair is strictly needed to preserve the already
     accepted milestone-1 ledger, exact lane, success bar, writable slice,
     and non-claims. The rejected review does not require such a repair, so
     the preferred outcome is to leave this file unchanged.
   - Do not create or update
     `orchestrator/rounds/round-194/implementation-notes.md`.
   - Keep the artifact docs-only, same-round, milestone-1-only,
     direction-1a-only, and below any milestone-2 implementation or
     boundary-pressure decision.
   - Verification:
     `test -f docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`
     `rg -n 'round-193|round-151|sameLaneClearBoundaryExpr|nestedForallContrastExpr|runPipelineElab|runPipelineElabChecked|writable slice|boundary-pressure|polymorphic mediation|## Non-Claims' docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`
     `rg -n 'Fallback/Core.hs|TermClosure.hs|test/Research/P5ClearBoundarySpec.hs|test/PipelineSpec.hs|src-public/MLF/Pipeline.hs' docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`

3. Prove the final branch diff is now inside the retry-authorized scope and
   that `implementation-notes.md` is gone from the base-branch diff.
   - Modify no files in this step.
   - Treat
     `orchestrator/rounds/round-194/implementation-notes.md`
     as rejected-attempt residue only; it must not survive in
     `git diff --name-only codex/automatic-recursive-type-inference...HEAD`.
   - The only allowed tracked paths in that base-branch diff are:
     `docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`,
     `orchestrator/rounds/round-194/plan.md`,
     and
     `orchestrator/rounds/round-194/selection.md`.
   - Verification:
     `git diff --check codex/automatic-recursive-type-inference...HEAD`
     `printf '%s\n' '--- name-only diff ---' && git diff --name-only codex/automatic-recursive-type-inference...HEAD`
     `if git diff --name-only codex/automatic-recursive-type-inference...HEAD | rg -n '^orchestrator/rounds/round-194/implementation-notes\\.md$'; then printf 'ROUND_194_BASE_DIFF_SCOPE_ESCAPE\n'; exit 1; else printf 'ROUND_194_BASE_DIFF_SCOPE_OK\n'; fi`
     `python3 - <<'PY'
import subprocess, sys
allowed = {
    'docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md',
    'orchestrator/rounds/round-194/plan.md',
    'orchestrator/rounds/round-194/selection.md',
}
paths = [
    p for p in subprocess.check_output(
        ['git', 'diff', '--name-only', 'codex/automatic-recursive-type-inference...HEAD'],
        text=True,
    ).splitlines()
    if p
]
extra = [p for p in paths if p not in allowed]
if extra:
    print('ROUND_194_BASE_DIFF_UNEXPECTED_PATHS:')
    print('\n'.join(extra))
    sys.exit(1)
print('ROUND_194_BASE_DIFF_ALLOWED_SET_OK')
for p in paths:
    print(p)
PY`
     `git status --short -- orchestrator/state.json orchestrator/rounds/round-194 docs/plans/2026-04-06-post-item-7-p5-successor-authority-success-bar-and-writable-slice-freeze.md`

## Review Focus

The implementer should leave the reviewer one easy approval path only:

- the docs artifact remains the same milestone-1 freeze that already passed
  the content checks in the rejected review;
- `orchestrator/rounds/round-194/implementation-notes.md` is absent from the
  branch diff against `codex/automatic-recursive-type-inference`;
- the allowed base-branch diff is limited to the docs artifact plus the
  existing round-owned `plan.md` / `selection.md`; and
- no fresh round-local notes, no production/test/Cabal changes, and no
  boundary-decision widening appear in the retry.
