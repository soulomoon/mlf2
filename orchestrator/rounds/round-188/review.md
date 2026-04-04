# Round 188 Review

- Round: `round-188`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-5`
- Reviewer decision: **APPROVED**

## Retry Subloop Record

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: the exact `sameLaneSeptupleAliasFrameClearBoundaryExpr` packet is honestly supported by raising only the terminal `hasRetainedChildClearBoundaryWithAliasBudget source term` entry from `2` to `3`, while keeping the outer alias-boundary budget at `2`; the fresh octuple control still fails closed on both authoritative entrypoints and remains out of scope except as a boundedness check

## Commands Run

1. `python3 -m json.tool orchestrator/state.json >/dev/null`
   - Exit: `0`
2. `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
   - Exit: `0`
3. `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
   - Exit: `0`
4. `rg -n 'roadmap_id|roadmap_revision|roadmap_dir' orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md orchestrator/rounds/round-188/selection.md`
   - Exit: `0`
5. `git diff --check`
   - Exit: `0`
6. `python3 - <<'PY'
import subprocess
allowed = {
    'src/MLF/Elab/TermClosure.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'test/PipelineSpec.hs',
    'orchestrator/rounds/round-188/implementation-notes.md',
    'orchestrator/rounds/round-188/plan.md',
}
changed = {
    line.strip()
    for line in subprocess.check_output(['git', 'diff', '--name-only'], text=True).splitlines()
    if line.strip() and line.strip() != 'orchestrator/state.json'
}
extra = sorted(changed - allowed)
if extra:
    raise SystemExit('forbidden diff drift: ' + ', '.join(extra))
print('ROUND_188_DIFF_SCOPE_OK')
PY`
   - Exit: `0`
   - Output summary: `ROUND_188_DIFF_SCOPE_OK`
7. `rg -n 'sameLaneSeptupleAliasFrameClearBoundaryExpr|sameLaneSextupleAliasFrameClearBoundaryExpr|hasRetainedChildAliasBoundary v body [234] =|hasRetainedChildClearBoundaryWithAliasBudget source term [234]' test/PipelineSpec.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs src/MLF/Elab/TermClosure.hs`
   - Exit: `0`
8. `cabal repl mlf2-test <<'EOF'
import qualified Data.Set as Set
import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabChecked)
import MLF.Frontend.Syntax
import SpecUtil (unsafeNormalizeExpr)
let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
let sextuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "leaf" (EVar "tail") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "leaf")) (EVar "u")))))))))
let septuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "leaf" (EVar "tail") (ELet "tip" (EVar "leaf") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "tip")) (EVar "u"))))))))))
let octuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "leaf" (EVar "tail") (ELet "tip" (EVar "leaf") (ELet "bud" (EVar "tip") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "bud")) (EVar "u")))))))))))
print (runPipelineElab Set.empty sextuple)
print (runPipelineElabChecked Set.empty sextuple)
print (runPipelineElab Set.empty septuple)
print (runPipelineElabChecked Set.empty septuple)
print (runPipelineElab Set.empty octuple)
print (runPipelineElabChecked Set.empty octuple)
:quit
EOF`
   - Exit: `0`
   - Output summary: sextuple `Right` on `runPipelineElab` and `runPipelineElabChecked`; septuple `Right` on `runPipelineElab` and `runPipelineElabChecked`; fresh octuple control `Left (PipelineTypeCheckError (TCLetTypeMismatch ...))` on both authoritative entrypoints
9. `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSeptupleAliasFrameClearBoundaryExpr"'`
   - Exit: `0`
   - Output summary: `4 examples, 0 failures`
10. `for name in sameLaneSextupleAliasFrameClearBoundaryExpr sameLaneQuintupleAliasFrameClearBoundaryExpr sameLaneQuadrupleAliasFrameClearBoundaryExpr sameLaneTripleAliasFrameClearBoundaryExpr sameLaneDoubleAliasFrameClearBoundaryExpr sameLaneAliasFrameClearBoundaryExpr; do cabal test mlf2-test --test-show-details=direct --test-options="--match $name" || exit $?; done`
    - Exit: `0`
    - Output summary: sextuple `4 examples, 0 failures`; quintuple `4 examples, 0 failures`; quadruple `4 examples, 0 failures`; triple `4 examples, 0 failures`; double `3 examples, 0 failures`; alias `3 examples, 0 failures`
11. `tmpdir=$(mktemp -d /tmp/round188-review-base.XXXXXX) && git -C /Users/ares/.codex/worktrees/d432/mlf4 worktree add --detach "$tmpdir" 965c135 >/dev/null && printf '%s\n' "$tmpdir"`
    - Exit: `0`
    - Output summary: created detached verification worktree at `/tmp/round188-review-base.2obkxP`
12. `cabal repl mlf2-test <<'EOF'
import qualified Data.Set as Set
import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabChecked)
import MLF.Frontend.Syntax
import SpecUtil (unsafeNormalizeExpr)
let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
let sextuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "leaf" (EVar "tail") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "leaf")) (EVar "u")))))))))
let septuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "leaf" (EVar "tail") (ELet "tip" (EVar "leaf") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "tip")) (EVar "u"))))))))))
let octuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "leaf" (EVar "tail") (ELet "tip" (EVar "leaf") (ELet "bud" (EVar "tip") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "bud")) (EVar "u")))))))))))
print (runPipelineElab Set.empty sextuple)
print (runPipelineElabChecked Set.empty sextuple)
print (runPipelineElab Set.empty septuple)
print (runPipelineElabChecked Set.empty septuple)
print (runPipelineElab Set.empty octuple)
print (runPipelineElabChecked Set.empty octuple)
:quit
EOF`
    - Exit: `0`
    - Output summary: detached baseline at commit `965c135` returned sextuple `Right` on both authoritative entrypoints while the selected septuple packet and fresh octuple control both returned `Left (PipelineTypeCheckError (TCLetTypeMismatch ...))`
13. `git -C /Users/ares/.codex/worktrees/d432/mlf4/orchestrator/worktrees/round-188 diff -- test/PipelineSpec.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs | git -C /tmp/round188-review-base.2obkxP apply`
    - Exit: `0`
14. `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSeptupleAliasFrameClearBoundaryExpr"'`
    - Exit: `1`
    - Output summary: expected red-phase confirmation in the detached baseline worktree, with failures limited to the selected septuple packet assertions plus the new `hasRetainedChildClearBoundaryWithAliasBudget source term 3` mechanism guard
15. `git -C /Users/ares/.codex/worktrees/d432/mlf4 worktree remove /tmp/round188-review-base.2obkxP --force`
    - Exit: `0`
16. `cabal build all && cabal test`
    - Exit: `0`
    - Output summary: full gate passed with `1327 examples, 0 failures`

## Check Results

- Baseline 1, roadmap identity/pointer consistency: PASS. `orchestrator/state.json`, the pointer stubs, and `orchestrator/rounds/round-188/selection.md` resolve the same `roadmap_id`, `roadmap_revision`, and `roadmap_dir`. The active roadmap bundle exists, and no roadmap-family files are in the implementation diff. This review writes `review-record.json` with the same identity tuple.
- Baseline 2, diff hygiene: PASS. `git diff --check` returned clean.
- Baseline 3, roadmap metadata integrity: PASS. Every roadmap item still carries `Item id`, `Depends on`, `Parallel safe`, `Parallel group`, and `Merge after`.
- Baseline 4, build/test gate: PASS. The round touches `src/` and `test/`, so `cabal build all && cabal test` was required and passed.
- Baseline 5, thesis gate: N/A. No thesis-facing files changed.
- Baseline 6, worker-plan integrity: N/A. No worker fan-out is recorded.
- Item-5 writable-slice check: PASS. The implementation diff stays inside `src/MLF/Elab/TermClosure.hs`, `test/PipelineSpec.hs`, and `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`. `orchestrator/state.json` remains the pre-existing controller-owned diff described in selection/plan; no pipeline facade, fallback, scope, Cabal, `test/Main.hs`, or adjacent control file drift appeared.
- Item-5 representative evidence check: PASS. The new septuple research assertions cover both authoritative entrypoints, the matching pipeline regression and mechanism guard pass, and alias-through-sextuple predecessor packets remain green as read-only predecessor evidence.
- Item-5 boundedness check: PASS. The fresh octuple control still fails closed on `runPipelineElab` and `runPipelineElabChecked` after the fix, and detached-baseline replay confirms the selected septuple packet and octuple control were both still closed before the production edit.
- Item-5 aggregate-read honesty: PASS. The diff supports one exact packet only and does not claim broader `P3`/`P4`/`P6` closure or repo-level readiness.

## Diff vs Plan

1. Step 1 matched. Detached-baseline replay at commit `965c135` reproduced the plan’s localization claim exactly: sextuple stayed `Right`, the selected septuple packet stayed `Left`, and the fresh octuple control stayed `Left` on both authoritative entrypoints before any production edit.
2. Step 2 matched. `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` adds exactly the selected septuple packet plus two authoritative-entrypoint assertions. `test/PipelineSpec.hs` adds the selected authoritative regression and the focused `TermClosure` mechanism guard, while narrowing the predecessor sextuple guard back to the shared seam. Applying only those test-file changes onto the detached baseline produced the expected red phase for the selected packet and new `source term 3` guard.
3. Step 3 matched. The only production edit is the plan-authorized helper adjustment in `src/MLF/Elab/TermClosure.hs`: `hasRetainedChildClearBoundary` now delegates to `hasRetainedChildClearBoundaryWithAliasBudget source term 3`, while `hasRetainedChildAliasBoundary v body 2 =` stays unchanged. Grep evidence confirms no `hasRetainedChildClearBoundaryWithAliasBudget source term 4` and no `hasRetainedChildAliasBoundary v body 3 =`.
4. Step 4 matched. Post-fix replay in the round worktree shows sextuple and septuple `Right` on both authoritative entrypoints while the fresh octuple control remains `Left`; focused predecessor matches alias through sextuple all stay green.
5. Step 5 matched. The roadmap checks, diff-scope guard, focused same-lane evidence, detached-baseline verification, `git diff --check`, and the full repo gate all passed.

## Evidence Summary

- `src/MLF/Elab/TermClosure.hs` changes only one production line in the selected seam: `hasRetainedChildClearBoundary` now delegates to `hasRetainedChildClearBoundaryWithAliasBudget source term 3`, while the outer alias-boundary entry budget remains `2`.
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` adds only the septuple packet and two exact authoritative-output assertions.
- `test/PipelineSpec.hs` adds only the septuple authoritative regression plus the selected mechanism guard; it also keeps the sextuple guard on the shared seam so the new `source term 3` ownership belongs only to the selected packet.
- The fresh octuple packet still fails closed on both authoritative entrypoints, so the landed change did not silently widen beyond the selected septuple slice.
- The implementation-notes verification claims are supported: detached-baseline replay reproduced the pre-fix `Left` outcomes, tests-only replay reproduced the red phase, and the round worktree replay plus the full gate reproduced the post-fix green state.

## Decision

**APPROVED**
