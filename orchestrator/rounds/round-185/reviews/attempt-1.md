# Round 185 Review

Decision: **REJECTED: predecessor control diff diverges from the approved round plan**

Merge-ready: **no**

## Scope Analyzed

- Item `item-5`, exact `sameLaneQuadrupleAliasFrameClearBoundaryExpr` slice only.
- Writable implementation/test payload reviewed:
  - `src/MLF/Elab/TermClosure.hs`
  - `test/PipelineSpec.hs`
  - `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  - `orchestrator/rounds/round-185/implementation-notes.md`
- Read-only review inputs followed:
  - `orchestrator/roles/reviewer.md`
  - `AGENTS.md`
  - `orchestrator/state.json`
  - `orchestrator/rounds/round-185/selection.md`
  - `orchestrator/rounds/round-185/plan.md`
  - `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/verification.md`
  - `orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/retry-subloop.md`
- `orchestrator/state.json` was validated for roadmap-pointer consistency only and treated as controller-owned / out of approval scope.

## Findings

1. **Blocking:** `test/PipelineSpec.hs:2158-2161` rewrites the double-alias predecessor source/mechanism guard from `1 / not 2` to `2 / not 3`. That conflicts with the approved plan, which requires alias, double-alias, and triple-alias predecessor controls to be replayed as read-only evidence (`orchestrator/rounds/round-185/plan.md:182-183`) and allows only the adjacent triple-alias source/mechanism guard to change if the shared budget marker must move (`orchestrator/rounds/round-185/plan.md:223-225`). The implementation notes acknowledge the same out-of-plan edit (`orchestrator/rounds/round-185/implementation-notes.md:10-13`). This is a plan divergence inside predecessor evidence, so the round cannot be approved even though the selected packet and the full test gate are green.

## Commands Run

- Exit 0: `jq -r '[.roadmap_id,.roadmap_revision,.roadmap_dir,.active_round_id,.stage,.retry] | @tsv' orchestrator/state.json`
- Exit 0: `rg -n 'roadmap_id|roadmap_revision|roadmap_dir' orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md orchestrator/rounds/round-185/selection.md`
- Exit 0: `for p in '^   Item id:' '^   Depends on:' '^   Parallel safe:' '^   Parallel group:' '^   Merge after:'; do printf '%s ' "$p"; rg -c "$p" orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md; done`
- Exit 0: `git diff --check`
- Exit 0: `git diff --name-only`
- Exit 0: `git ls-files --others --exclude-standard orchestrator/rounds/round-185`
- Exit 0: `git diff --name-only -- src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/Scope.hs test/Research/C1AuthoritativeSurfaceSpec.hs test/Research/P5ClearBoundarySpec.hs test/Main.hs mlf2.cabal`
- Exit 0: `git diff -- src/MLF/Elab/TermClosure.hs`
- Exit 0: `git diff -- test/PipelineSpec.hs`
- Exit 0: `git diff -- test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- Exit 0: `rg -n 'hasRetainedChildAliasBoundary v body 2 =|hasRetainedChildAliasBoundary v body 3 =|hasRetainedChildClearBoundary' src/MLF/Elab/TermClosure.hs test/PipelineSpec.hs`
- Exit 0: `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuadrupleAliasFrameClearBoundaryExpr"'`
- Exit 0: `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'`
- Exit 0: `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'`
- Exit 0: `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'`
- Exit 0:
  `cabal repl mlf2-test <<'EOF'
  import qualified Data.Set as Set
  import MLF.Elab.Pipeline (runPipelineElab, runPipelineElabChecked)
  import MLF.Frontend.Syntax
  import SpecUtil (unsafeNormalizeExpr)
  let recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))
  let expr = unsafeNormalizeExpr (ELet "k" (ELamAnn "x" recursiveAnn (EVar "x")) (ELet "hold" (EVar "k") (ELet "keep" (EVar "hold") (ELet "more" (EVar "keep") (ELet "deep" (EVar "more") (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "deep")) (EVar "u")))))))
  print (runPipelineElab Set.empty expr)
  print (runPipelineElabChecked Set.empty expr)
  :quit
  EOF`
- Exit 0: `cabal build all && cabal test`

## Pass/Fail Results

- **Baseline 1: roadmap identity / pointer consistency:** pass.
  - `state.json`, the live pointer stubs, and `selection.md` all resolve `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`, `roadmap_revision = rev-001`, and the matching `roadmap_dir`.
- **Baseline 2: diff hygiene:** pass.
  - `git diff --check` reported no whitespace or merge-marker problems.
- **Baseline 3: roadmap metadata integrity:** pass.
  - The active roadmap still contains all five required metadata fields for all seven items.
- **Baseline 4: build and test gate for production/test changes:** pass.
  - `cabal build all && cabal test` passed with `1315 examples, 0 failures`.
- **Item-5 writable-slice and forbidden-file guard:** pass.
  - The active implementation diff is limited to `src/MLF/Elab/TermClosure.hs`, `test/PipelineSpec.hs`, `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`, plus controller-owned `orchestrator/state.json`; no forbidden pipeline, fallback, scope, Cabal, or test harness files drifted.
- **Item-5 focused evidence replay:** pass.
  - Focused packet checks passed:
    - quadruple-alias: `4 examples, 0 failures`
    - triple-alias: `4 examples, 0 failures`
    - double-alias: `3 examples, 0 failures`
    - alias: `3 examples, 0 failures`
  - The direct `cabal repl` probe returned matching `Right (...)` recursive authoritative outputs from both `runPipelineElab` and `runPipelineElabChecked`.
- **Plan conformance:** fail.
  - The round changed double-alias predecessor source assertions even though the plan froze predecessor controls as read-only and only authorized a possible triple-alias adjacent source-guard update.

## Plan Comparison

1. **Keep the round on the exact quadruple-alias slice:** mostly satisfied.
   - `src/MLF/Elab/TermClosure.hs` changes only the retained-child alias-boundary entry budget from `1` to `2`.
   - `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` adds exactly the two selected-packet authoritative assertions.
   - `test/PipelineSpec.hs` adds the selected-packet regression and selected-packet source/mechanism guard.
2. **Replay predecessor controls as read-only evidence:** not satisfied.
   - `test/PipelineSpec.hs:2158-2161` rewrites the double-alias predecessor mechanism assertions instead of replaying them unchanged.
3. **Keep the result bounded and non-widening:** satisfied on code shape.
   - `hasRetainedChildClearBoundary` remains the terminal bounded rule.
   - No pipeline, fallback, scope, Cabal, or public-surface file changed.
4. **Run focused and full verification gates:** satisfied.
   - All required focused checks and the full `cabal build all && cabal test` gate passed.

## Evidence Summary

- `src/MLF/Elab/TermClosure.hs:66-95` applies the smallest production-side change: the alias-boundary entry budget moves from `1` to `2`, and the exhausted-budget terminal rule remains `hasRetainedChildClearBoundary`.
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs:50-58,137-144` adds the exact quadruple-alias packet and the two required authoritative entrypoint assertions.
- `test/PipelineSpec.hs:2224-2283` adds the selected-packet regression and the selected-packet bounded source/mechanism guard.
- `test/PipelineSpec.hs:2158-2161` also edits the double-alias predecessor source assertions, which exceeds the plan's authorized predecessor-evidence handling.
- `orchestrator/rounds/round-185/implementation-notes.md:10-13` explicitly records that predecessor source assertions were updated, matching the observed divergence.
- The selected packet itself is green and bounded, but approval requires plan conformance as well as passing tests.

## Retry Subloop Record

- Implemented stage result: `rejected`
- Attempt verdict: `retry`
- Stage action: `retry`
- Retry reason: predecessor double-alias source/mechanism evidence was edited outside the approved round plan.
- Fix hypothesis: restore the double-alias predecessor control to read-only evidence and confine any budget-text update to the selected quadruple-alias guard plus, if unavoidable, the adjacent triple-alias source/mechanism guard only.

## Decision

**REJECTED: predecessor control diff diverges from the approved round plan**
