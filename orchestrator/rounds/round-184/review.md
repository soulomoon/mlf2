# Round 184 Review

Decision: **APPROVED**

Merge-ready: **yes**

## Scope Analyzed

- Item `item-5`, exact `sameLaneTripleAliasFrameClearBoundaryExpr` slice only.
- Writable implementation/test payload reviewed:
  - `src/MLF/Elab/TermClosure.hs`
  - `test/PipelineSpec.hs`
  - `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  - `orchestrator/rounds/round-184/selection.md`
  - `orchestrator/rounds/round-184/plan.md`
  - `orchestrator/rounds/round-184/implementation-notes.md`
- `orchestrator/state.json` was validated for roadmap-pointer consistency only and treated as controller-owned / out of scope for approval.

## Findings

- No blocking findings.
- Residual risk: the red-first ordering from plan step 2 is evidenced by `implementation-notes.md`; the current green worktree state does not independently preserve that temporal sequence.

## Commands Run

- Exit 0: `python3 -m json.tool orchestrator/state.json >/dev/null`
- Exit 0:
  `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md" && for f in orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md; do rg -n "2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap|rev-001|orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001" "$f"; done`
- Exit 0:
  `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- Exit 0:
  `rg -n 'preserveRetainedChildAuthoritativeResult|preserveRetainedChildAliasBoundary|hasRetainedChildAliasBoundary|isClearBoundaryRetainedChildRhs|isIdentityBoundaryLambda' src/MLF/Elab/TermClosure.hs`
- Exit 0:
  `rg -n 'runPipelineElabWith|preserveRetainedChildAuthoritativeResult' src/MLF/Elab/Run/Pipeline.hs`
- Exit 0:
  `rg -n 'sameLaneLocalRetainedChildTarget|boundHasForallFrom|keepTargetFinal|targetC' src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- Exit 0:
  `rg -n 'sameLaneTripleAliasFrameClearBoundaryExpr|sameLaneDoubleAliasFrameClearBoundaryExpr|sameLaneAliasFrameClearBoundaryExpr' test/PipelineSpec.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- Exit 1:
  reviewer note only, non-blocking:
  `python3 - <<'PY'
import subprocess, sys
allowed = {
    'src/MLF/Elab/TermClosure.hs',
    'test/PipelineSpec.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'orchestrator/rounds/round-184/implementation-notes.md',
}
tracked = subprocess.check_output(
    ['git', 'diff', '--name-only', '--', 'src', 'src-public', 'test', 'orchestrator/rounds/round-184', 'mlf2.cabal'],
    text=True,
).splitlines()
untracked = subprocess.check_output(
    ['git', 'ls-files', '--others', '--exclude-standard', '--', 'src', 'src-public', 'test', 'orchestrator/rounds/round-184', 'mlf2.cabal'],
    text=True,
).splitlines()
paths = [p for p in tracked + untracked if p]
extra = [
    p for p in paths
    if p not in allowed
    and p not in {
        'orchestrator/rounds/round-184/plan.md',
        'orchestrator/rounds/round-184/selection.md',
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
print('ROUND184_WRITABLE_SLICE_OK')
PY`
  output summary: reported only pre-existing reviewer artifacts `orchestrator/rounds/round-184/review.md` and `orchestrator/rounds/round-184/review-record.json` as out-of-scope paths.
- Exit 0:
  reviewer-aware pathset confirmation:
  `python3 - <<'PY'
import subprocess, sys
allowed = {
    'src/MLF/Elab/TermClosure.hs',
    'test/PipelineSpec.hs',
    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',
    'orchestrator/rounds/round-184/implementation-notes.md',
    'orchestrator/rounds/round-184/review.md',
    'orchestrator/rounds/round-184/review-record.json',
}
tracked = subprocess.check_output(
    ['git', 'diff', '--name-only', '--', 'src', 'src-public', 'test', 'orchestrator/rounds/round-184', 'mlf2.cabal'],
    text=True,
).splitlines()
untracked = subprocess.check_output(
    ['git', 'ls-files', '--others', '--exclude-standard', '--', 'src', 'src-public', 'test', 'orchestrator/rounds/round-184', 'mlf2.cabal'],
    text=True,
).splitlines()
paths = [p for p in tracked + untracked if p]
impl_extra = [
    p for p in paths
    if p not in allowed
    and p not in {
        'orchestrator/rounds/round-184/plan.md',
        'orchestrator/rounds/round-184/selection.md',
    }
]
print('\n'.join(paths) or '(none)')
if impl_extra:
    print('IMPL_EXTRA:')
    print('\n'.join(impl_extra))
    sys.exit(1)
print('ROUND184_IMPLEMENTATION_AND_REVIEW_PATHSET_OK')
PY`
- Exit 0: `git diff --check`
- Exit 0: `git diff --name-only`
- Exit 0: `git diff --name-only -- orchestrator/roadmaps`
- Exit 0:
  `git diff -- src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/Scope.hs test/Research/C1AuthoritativeSurfaceSpec.hs test/Research/P5ClearBoundarySpec.hs test/Main.hs mlf2.cabal`
- Exit 0:
  `cabal repl mlf2-test <<'EOF'
import qualified Data.Set as Set
import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabChecked)
import MLF.Frontend.Syntax
import SpecUtil (unsafeNormalizeExpr)
:{
let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
    expr =
      ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
          (ELet "keep" (EVar "hold")
            (ELet "more" (EVar "keep")
              (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "more")) (EVar "u")))))
:}
print (runPipelineElab Set.empty (unsafeNormalizeExpr expr))
print (runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr))
:q
EOF`
- Exit 0:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'`
- Exit 0:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
- Exit 0:
  `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'`
- Exit 0: `cabal build all && cabal test`

## Baseline Checks

- Roadmap identity, pointer, and preserved-history consistency: passed.
  - `orchestrator/state.json`, `orchestrator/roadmap.md`, `orchestrator/verification.md`, and `orchestrator/retry-subloop.md` all point to roadmap `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`, revision `rev-001`, directory `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`.
  - `git diff --name-only -- orchestrator/roadmaps` returned no output, so prior roadmap families/revisions remain unchanged.
- Diff hygiene: passed.
  - `git diff --check` returned no output.
- Roadmap metadata integrity: passed.
  - Active `roadmap.md` still contains `Item id:`, `Depends on:`, `Parallel safe:`, `Parallel group:`, and `Merge after:` for every roadmap item (`item-1` through `item-7`).
- Build/test gate for code/test changes: passed.
  - `cabal build all && cabal test` finished with `1311 examples, 0 failures`.
- Thesis conformance gate: not applicable.
  - No thesis-facing paths changed.
- Worker-plan integrity: not applicable.
  - No planner-authored worker fan-out artifacts exist for this round.

## Item-5 Checks

- Writable slice respected: passed.
  - `git diff --name-only` shows tracked modifications only in `orchestrator/state.json`, `src/MLF/Elab/TermClosure.hs`, `test/PipelineSpec.hs`, and `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`.
  - No forbidden diff appears in `Run/Pipeline`, fallback, scope, public pipeline, `test/Main.hs`, `mlf2.cabal`, or the adjacent control specs.
  - The raw implementer slice script now fails only because reviewer-owned `review.md` and `review-record.json` already exist in the round directory. After accounting for reviewer artifacts, the pathset check passed and the implementation payload stayed within the authorized slice.
- Representative `P2`-`P6` evidence remains honest: passed.
  - The diff adds only the selected `sameLaneTripleAliasFrameClearBoundaryExpr` research and authoritative-entrypoint coverage.
  - The settled alias and double-alias predecessor assertions remain read-only and were replayed as controls.
- Full gate for code/test work: passed.
  - `cabal build all && cabal test` succeeded.
- Aggregate positive-family read stays bounded: passed.
  - The notes and diff claim only that the triple-alias packet is now preserved via an exact `TermClosure` extension.
  - No general `P3` / `P4` / `P6` readiness or repo-level readiness claim was added.

## Plan Comparison

1. Reproduce and localize before widening scope: satisfied.
   - Current review reran the packet probe in `cabal repl mlf2-test` and confirmed both authoritative entrypoints now return the recursive output shape.
   - `src/MLF/Elab/Run/Pipeline.hs` still routes through `preserveRetainedChildAuthoritativeResult`, and the item-3 route/guard cluster in `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` stayed read-only.
   - The initial failing baseline described in `implementation-notes.md` is consistent with the production/test delta.
2. Add failing focused tests first and keep them bounded: satisfied on diff shape; temporal ordering evidenced only by notes.
   - `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` adds exactly the two selected-packet authoritative assertions.
   - `test/PipelineSpec.hs` adds exactly one selected-packet authoritative regression plus one exact mechanism-boundary guard.
   - No adjacent control assertions were edited.
3. Apply the smallest lawful production correction: satisfied.
   - `src/MLF/Elab/TermClosure.hs` adds one exhausted-budget alias-shell branch and a helper `hasRetainedChildClearBoundary`; no out-of-slice production file changed.
4. Re-green the exact packet and keep the outcome honest: satisfied.
   - Triple-alias focused tests pass.
   - Alias and double-alias predecessor controls still pass unchanged.
   - `implementation-notes.md` keeps the claim bounded to the exact triple-alias packet.
5. Run focused and full verification gates serially: satisfied.
   - Focused triple-alias, double-alias, and alias checks all passed.
   - Full `cabal build all && cabal test` passed.

## Evidence Summary

- `src/MLF/Elab/TermClosure.hs:79-105` still enters the alias-boundary path only through `hasRetainedChildAliasBoundary v body 1 =`; the new logic adds exactly one exhausted-budget alias shell when it immediately leads to the pre-existing clear-boundary retained-child shape.
- `test/PipelineSpec.hs:2194-2236` adds one selected-packet authoritative regression and one exact source/mechanism guard that still forbids a depth-`2` explicit entrypoint marker while requiring the new bounded exhausted-budget branch.
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs:40-48` adds the exact selected-packet research assertions on `runPipelineElab` and `runPipelineElabChecked`.
- The corrected REPL probe returned recursive output on both authoritative entrypoints for `sameLaneTripleAliasFrameClearBoundaryExpr`.
- Focused controls passed:
  - triple-alias: `4 examples, 0 failures`
  - double-alias: `3 examples, 0 failures`
  - alias: `3 examples, 0 failures`
- Full suite passed: `1311 examples, 0 failures`.

## Decision

**APPROVED**
