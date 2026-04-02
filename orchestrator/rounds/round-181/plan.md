# Round 181 Plan

- Round: `round-181`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-5`
- Retry: `null`
- Execution shape: serial, `C1`-only, systematic-debugging-first, TDD-first, non-widening

## Objective

Keep the round on exactly one item-5 slice: the `P2` non-local `C1`
authoritative-surface packet anchored at
`rootNonLocalSchemeAliasBaseLike` / `baseTarget -> baseC`.

The current tree contains a post-acceptance divergence that must be treated as
the root-cause question for this slice, not as already-earned family support:
accepted March 25 docs still classify `C1` as
`admitted but not reconstruction-visible / blocker debt`, while commit
`98af0ff` added `preserveC1AuthoritativeRecursiveAlias` in
`src/MLF/Elab/Run/Pipeline.hs` and the current `C1` tests now assert
recursive authoritative output. This round must first determine whether the
current `runPipelineElab` / `runPipelineElabChecked` recursive `C1` read is an
honest continuation of the admitted non-local route or packet-local folklore
caused by that shortcut.

If production changes are still justified after that investigation, they must
start with a failing focused test, keep the fallback surface honestly
non-recursive as supporting context only, and land only the minimum code/test
correction needed to make the authoritative surfaces truthful for this exact
packet. No same-lane, nested-`forall`, negative-family, cyclic, multi-SCC,
equi-recursive, fallback, or second-interface widening is authorized.

## Locked Round Context

- Stage: `plan`
- Attempt: `attempt-1`
- Current review feedback: none yet
- Active selection input:
  `orchestrator/rounds/round-181/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `?? orchestrator/rounds/round-181/` is the round-owned directory. Keep the
  round-local artifact set scoped to this plan and an optional
  `implementation-notes.md` only.
- `orchestrator/rounds/round-181/selection.md` is the round input and must
  remain untouched.

The frozen packet for this round is only:

- `test/Research/C1AuthoritativeSurfaceSpec.hs`
  `c1Expr = ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (EVar "k")`

The inherited boundary remains controlling:

- explicit-only
- iso-recursive
- non-equi-recursive
- non-cyclic-graph
- no-fallback

The round must not reinterpret one packet as general `P2` closure or repo-level
readiness.

## Write Scope

Implementer-owned writes for this round are limited to:

- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/TermClosure.hs` only if the honest fix requires moving
  authoritative-preservation logic into the existing shared closure owner
  instead of leaving it packet-local in `Run/Pipeline`
- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` only if root-cause evidence
  shows the admitted `rootNonLocalSchemeAliasBaseLike` / `baseTarget -> baseC`
  route itself must carry the same `C1` recursive story and that this can be
  done without opening a new route family
- `src/MLF/Elab/Pipeline.hs` and `src-public/MLF/Pipeline.hs` only if an
  actual authoritative entrypoint/export contract changes in lockstep with
  `src/MLF/Elab/Run/Pipeline.hs`; otherwise leave them untouched
- `test/Research/C1AuthoritativeSurfaceSpec.hs`
- `test/PipelineSpec.hs`
- `orchestrator/rounds/round-181/implementation-notes.md` only if the
  implementer needs a round-local record of the exact bounded result

Do not modify:

- `orchestrator/rounds/round-181/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `orchestrator/rounds/round-181/review.md`
- `orchestrator/rounds/round-181/merge.md`
- `docs/plans/**`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- `test/Research/P5ClearBoundarySpec.hs`
- `mlf2.cabal`

This round must stay inside the exact `C1` non-local packet only. Do not
reopen same-lane retained-child families, nested-`forall` families, or any
aggregate item-5 classification artifact.

## Sequential Plan

1. Reproduce and localize the `C1` divergence before proposing any fix; modify
   no files in this step.
   - Re-run the exact `C1` focused evidence in
     `test/Research/C1AuthoritativeSurfaceSpec.hs` and the matching
     non-local assertions in `test/PipelineSpec.hs`.
   - Inspect commit `98af0ff` together with
     `src/MLF/Elab/Run/Pipeline.hs`,
     `src/MLF/Elab/TermClosure.hs`, and
     `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`.
   - Identify the first point where the exact admitted non-local packet stops
     matching the accepted March 25 blocker-debt read and starts returning a
     recursive authoritative result.
   - End the investigation with one explicit technical conclusion only:
     either the current authoritative `C1` result is genuinely carried by the
     admitted `rootNonLocalSchemeAliasBaseLike` route, or it depends on the
     packet-local `preserveC1AuthoritativeRecursiveAlias` shortcut.
   - Do not code a fix before that root-cause conclusion is established.

2. Add the failing focused `C1` test surface first; keep it bounded to the
   same exact packet.
   - If step 1 shows the recursive authoritative read should remain, tighten
     `test/Research/C1AuthoritativeSurfaceSpec.hs` and the matching `C1`
     assertions in `test/PipelineSpec.hs` so they demand the exact honest
     `C1` outcome and a route-backed explanation, not merely
     `containsMu True` after a packet-local rescue.
   - In that same positive-path case, add a focused guard in
     `test/PipelineSpec.hs` that fails while
     `preserveC1AuthoritativeRecursiveAlias` /
     `isBlockedC1AliasScheme` remains the reason the `C1` packet stays
     recursive.
   - If step 1 shows the current recursive authoritative read is packet-local
     folklore, update only those same two `C1` test surfaces back to the
     honest blocker-debt expectation for the exact packet.
   - Run the focused `C1` commands immediately and watch the intended red
     failure before changing production code.

3. Apply the smallest lawful production correction only after the red run
   fails.
   - Start in `src/MLF/Elab/Run/Pipeline.hs`, because that is where the
     current packet-local helper lives.
   - Remove or replace `preserveC1AuthoritativeRecursiveAlias`; do not layer a
     second packet-local rescue on top of it.
   - If the honest fix keeps positive authoritative `C1` output, move that
     support to the existing owner of the real mechanism only:
     `src/MLF/Elab/TermClosure.hs` for shared authoritative-preservation logic
     or `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` for the admitted
     non-local route, but only if step 1 proved that those seams already carry
     the same recursive story.
   - If the honest fix is blocker debt, remove the packet-local helper and do
     not replace it with a new rescue path.
   - Do not add new route arms, new candidate ranking, new search loops,
     same-lane retained-child logic, nested-`forall` handling, cyclic or
     multi-SCC behavior, equi-recursive reasoning, fallback widening, or a
     second interface.

4. Re-green the bounded `C1` slice and keep the outcome honest.
   - Make only the `C1`-focused tests in
     `test/Research/C1AuthoritativeSurfaceSpec.hs` and
     `test/PipelineSpec.hs` green.
   - Touch `src/MLF/Elab/Pipeline.hs` or `src-public/MLF/Pipeline.hs` only if
     step 3 genuinely changes the authoritative entrypoint/export contract;
     otherwise confirm they stayed unchanged.
   - Update `orchestrator/rounds/round-181/implementation-notes.md` only if
     needed, and if written, record one packet-bounded result only:
     either `C1 route-backed authoritative continuation landed` or
     `C1 remains admitted but not reconstruction-visible / blocker debt after
     shortcut removal`.
   - Do not extrapolate this one packet into same-lane readiness, positive
     `P5`, aggregate `P2`, or repo-level readiness.

5. Run the focused and full verification gates.
   - Re-run the focused `C1` commands.
   - Recheck that no packet-local `C1` shortcut remains in
     `src/MLF/Elab/Run/Pipeline.hs`.
   - Recheck that the diff stayed inside the authorized `C1` writable slice.
   - Re-run `cabal build all && cabal test` because source/test files are in
     scope for this round.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "C1 authoritative-surface harness"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "keeps the selected non-local scheme-alias/base-like packet"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "ARI-C1 feasibility characterization (bounded prototype-only)"'`
- `python3 - <<'PY'
from pathlib import Path
text = Path('src/MLF/Elab/Run/Pipeline.hs').read_text()
for token in ('preserveC1AuthoritativeRecursiveAlias', 'isBlockedC1AliasScheme'):
    if token in text:
        raise SystemExit(f'forbidden packet-local C1 shortcut still present: {token}')
print('C1_SHORTCUT_REMOVED')
PY`
- `rg -n 'rootNonLocalSchemeAliasBaseLike|baseTarget|targetC' src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `python3 - <<'PY'
import subprocess, sys
allowed = {
    'src/MLF/Elab/Run/Pipeline.hs',
    'src/MLF/Elab/TermClosure.hs',
    'src/MLF/Elab/Run/ResultType/Fallback/Core.hs',
    'src/MLF/Elab/Pipeline.hs',
    'src-public/MLF/Pipeline.hs',
    'test/Research/C1AuthoritativeSurfaceSpec.hs',
    'test/PipelineSpec.hs',
    'orchestrator/rounds/round-181/implementation-notes.md',
}
tracked = subprocess.check_output(
    ['git', 'diff', '--name-only', '--', 'src', 'src-public', 'test', 'orchestrator/rounds/round-181', 'mlf2.cabal'],
    text=True,
).splitlines()
untracked = subprocess.check_output(
    ['git', 'ls-files', '--others', '--exclude-standard', '--', 'src', 'src-public', 'test', 'orchestrator/rounds/round-181', 'mlf2.cabal'],
    text=True,
).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [
    p for p in paths
    if p not in allowed
    and p not in {
        'orchestrator/rounds/round-181/plan.md',
        'orchestrator/rounds/round-181/selection.md',
    }
]
forbidden = [p for p in paths if p == 'mlf2.cabal']
if extra or forbidden:
    if forbidden:
        print('FORBIDDEN_PATHS:')
        print('\n'.join(forbidden))
    if extra:
        print('OUT_OF_SCOPE_PATHS:')
        print('\n'.join(extra))
    sys.exit(1)
print('ROUND181_C1_WRITABLE_SLICE_OK')
PY`
- `git diff --check`
- `cabal build all && cabal test`
