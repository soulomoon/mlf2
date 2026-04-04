# Round 187 Review

- Round: `round-187`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-5`
- Reviewer decision: **APPROVED**

## Retry Subloop Record

- Implemented stage result: `accepted`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: the exact `sameLaneSextupleAliasFrameClearBoundaryExpr` packet is honestly supported by raising only the terminal `hasRetainedChildClearBoundaryWithAliasBudget source term` entry from `1` to `2`, while keeping the outer alias-boundary budget at `2`; the fresh septuple control still fails closed on both authoritative entrypoints and remains out of scope except as a boundedness check

## Commands Run

1. `python3 -m json.tool orchestrator/state.json >/dev/null`
   - Exit: `0`
2. `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
   - Exit: `0`
3. `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
   - Exit: `0`
4. `rg -n 'roadmap_id|roadmap_revision|roadmap_dir' orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md orchestrator/rounds/round-187/selection.md`
   - Exit: `0`
5. `git diff --check codex/automatic-recursive-type-inference --`
   - Exit: `0`
6. `python3 - <<'PY'
import subprocess
allowed = {
    'src/MLF/Elab/TermClosure.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'test/PipelineSpec.hs',
    'orchestrator/rounds/round-187/implementation-notes.md',
    'orchestrator/rounds/round-187/plan.md',
}
changed = {
    line.strip()
    for line in subprocess.check_output(['git', 'diff', '--name-only'], text=True).splitlines()
    if line.strip() and line.strip() != 'orchestrator/state.json'
}
extra = sorted(changed - allowed)
if extra:
    raise SystemExit('forbidden diff drift: ' + ', '.join(extra))
print('ROUND_187_DIFF_SCOPE_OK')
PY`
   - Exit: `0`
   - Output summary: `ROUND_187_DIFF_SCOPE_OK`
7. `rg -n 'sameLaneSextupleAliasFrameClearBoundaryExpr|sameLaneQuintupleAliasFrameClearBoundaryExpr|hasRetainedChildAliasBoundary v body [234] =|hasRetainedChildClearBoundaryWithAliasBudget source term [123]' test/PipelineSpec.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs src/MLF/Elab/TermClosure.hs`
   - Exit: `0`
8. `cabal repl mlf2-test <<'EOF'
import qualified Data.Set as Set
import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabChecked)
import MLF.Frontend.Syntax
import SpecUtil (unsafeNormalizeExpr)
let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
let quintuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "tail")) (EVar "u"))))))))
let sextuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "leaf" (EVar "tail") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "leaf")) (EVar "u")))))))))
let septuple = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "tail" (EVar "deep") (ELet "leaf" (EVar "tail") (ELet "tip" (EVar "leaf") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "tip")) (EVar "u"))))))))))
print (runPipelineElab Set.empty quintuple)
print (runPipelineElabChecked Set.empty quintuple)
print (runPipelineElab Set.empty sextuple)
print (runPipelineElabChecked Set.empty sextuple)
print (runPipelineElab Set.empty septuple)
print (runPipelineElabChecked Set.empty septuple)
:quit
EOF`
   - Exit: `0`
   - Output summary: quintuple `Right` on `runPipelineElab` and `runPipelineElabChecked`; sextuple `Right` on `runPipelineElab` and `runPipelineElabChecked`; fresh septuple control `Left (PipelineTypeCheckError (TCLetTypeMismatch ...))` on both authoritative entrypoints
9. `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneSextupleAliasFrameClearBoundaryExpr"'`
   - Exit: `0`
   - Output summary: `4 examples, 0 failures`
10. `for pattern in 'sameLaneQuintupleAliasFrameClearBoundaryExpr' 'sameLaneQuadrupleAliasFrameClearBoundaryExpr' 'sameLaneTripleAliasFrameClearBoundaryExpr' 'sameLaneDoubleAliasFrameClearBoundaryExpr' 'sameLaneAliasFrameClearBoundaryExpr'; do cabal test mlf2-test --test-show-details=direct --test-options="--match \"$pattern\"" || exit $?; done`
    - Exit: `0`
    - Output summary: quintuple `4 examples, 0 failures`; quadruple `4 examples, 0 failures`; triple `4 examples, 0 failures`; double `3 examples, 0 failures`; alias `3 examples, 0 failures`
11. `cabal build all && cabal test`
    - Exit: `0`
    - Output summary: full gate passed with `1323 examples, 0 failures`

## Check Results

- Baseline 1, roadmap identity/pointer consistency: PASS. `orchestrator/state.json`, the pointer stubs, and `orchestrator/rounds/round-187/selection.md` all resolve the same `roadmap_id`, `roadmap_revision`, and `roadmap_dir`. The active roadmap bundle exists, and no roadmap-family files are in the implementation diff.
- Baseline 2, diff hygiene: PASS. `git diff --check` against the round diff returned clean.
- Baseline 3, roadmap metadata integrity: PASS. Every roadmap item still carries `Item id`, `Depends on`, `Parallel safe`, `Parallel group`, and `Merge after`.
- Baseline 4, build/test gate: PASS. The round touches `src/` and `test/`, so `cabal build all && cabal test` was required and passed.
- Baseline 5, thesis gate: N/A. No thesis-facing files changed.
- Baseline 6, worker-plan integrity: N/A. No worker fan-out is recorded.
- Item-5 writable-slice check: PASS. The only implementation diff is in `src/MLF/Elab/TermClosure.hs`, `test/PipelineSpec.hs`, and `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`. `orchestrator/state.json` is the pre-existing controller-owned round-state diff described in selection/plan; no pipeline facade, fallback, scope, Cabal, `test/Main.hs`, or adjacent control file drift appeared.
- Item-5 representative evidence check: PASS. The new sextuple research assertions cover both authoritative entrypoints, the matching pipeline regression and mechanism guard pass, and the alias through quintuple predecessor packets remain green as read-only predecessor evidence.
- Item-5 boundedness check: PASS. The fresh septuple probe still fails closed on `runPipelineElab` and `runPipelineElabChecked`, and it is recorded only as control evidence that the change did not widen beyond the selected sextuple packet.
- Item-5 aggregate-read honesty: PASS. The diff supports one exact packet only and does not claim broader `P3`/`P4`/`P6` closure or repo-level readiness.

## Diff vs Plan

1. Step 1 matched. The review-time probe reproduced the selected lane faithfully: quintuple stayed `Right`, sextuple is now `Right`, and the fresh septuple control remains `Left` on both authoritative entrypoints. Read-only authority flow and fallback files stayed untouched.
2. Step 2 matched. `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` adds exactly the sextuple packet plus two authoritative-entrypoint assertions reusing the existing exact recursive-output expectation. `test/PipelineSpec.hs` adds the selected sextuple authoritative regression plus the focused bounded-helper guard. The quintuple mechanism guard was narrowed to the shared seam so the selected `source term 2` ownership lives only on the sextuple packet, which is consistent with the plan’s ownership rule for step 4.
3. Step 3 matched. The only production edit is the plan-authorized helper adjustment in `src/MLF/Elab/TermClosure.hs`: `hasRetainedChildClearBoundaryWithAliasBudget source term` now starts at `2`, while `hasRetainedChildAliasBoundary v body 2 =` stays unchanged. Grep evidence confirms no `source term 3`, no `hasRetainedChildAliasBoundary v body 3 =`, and no fallback/search widening markers.
4. Step 4 matched. The selected sextuple slice is green, predecessor packets alias through quintuple remain green, and the fresh septuple control still fails closed, so the round stays packet-bounded.
5. Step 5 matched. The roadmap checks, diff-scope guard, focused same-lane evidence, `git diff --check`, and the full repo gate all passed.

## Evidence Summary

- `src/MLF/Elab/TermClosure.hs` changes only one line in the selected seam: `hasRetainedChildClearBoundary` now delegates to `hasRetainedChildClearBoundaryWithAliasBudget source term 2`, while the outer alias-boundary entry budget remains `2`.
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` adds only the sextuple packet and two exact authoritative-output assertions.
- `test/PipelineSpec.hs` adds only the sextuple authoritative regression plus the selected mechanism guard; it also keeps the quintuple guard at the shared seam instead of letting that predecessor claim the new `source term 2` ownership.
- The deeper septuple packet still fails closed on both authoritative entrypoints, so the landed change did not silently widen past the selected slice.

## Decision

**APPROVED**
