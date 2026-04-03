# Round 185 Review

Decision: **APPROVED**

Merge-ready: **yes**

## Scope Analyzed

- Item `item-5`, exact `sameLaneQuadrupleAliasFrameClearBoundaryExpr` slice only.
- Retry context: `attempt-2` after attempt-1 rejection for predecessor
  double-alias budget-marker ownership drift.
- Retry-writable surfaces audited against the locked plan:
  - `test/PipelineSpec.hs`
  - `orchestrator/rounds/round-185/implementation-notes.md`
- Frozen read-only baselines revalidated:
  - `src/MLF/Elab/TermClosure.hs`
  - `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- `orchestrator/state.json` was checked for roadmap-pointer consistency only and
  remained controller-owned / out of approval scope.

## Commands Run

- Exit 0: `jq -r '[.roadmap_id,.roadmap_revision,.roadmap_dir,.active_round_id,.stage,.retry.attempt] | @tsv' orchestrator/state.json`
- Exit 0: `rg -n 'roadmap_id|roadmap_revision|roadmap_dir' orchestrator/roadmap.md orchestrator/verification.md orchestrator/retry-subloop.md orchestrator/rounds/round-185/selection.md`
- Exit 0: `for p in '^   Item id:' '^   Depends on:' '^   Parallel safe:' '^   Parallel group:' '^   Merge after:'; do printf '%s ' "$p"; rg -c "$p" orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001/roadmap.md; done`
- Exit 0: `git merge-base HEAD codex/automatic-recursive-type-inference`
- Exit 0: `git diff --check`
- Exit 0: `git diff --name-only`
- Exit 0: `git diff 2f02c72355b30b75ff587588e0e8b2e0fe45f99b --name-only`
- Exit 0: `git diff --name-only -- orchestrator/roadmaps`
- Exit 0: `git diff --name-only -- src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/Scope.hs test/Research/C1AuthoritativeSurfaceSpec.hs test/Research/P5ClearBoundarySpec.hs test/Main.hs mlf2.cabal`
- Exit 0: `git ls-files --others --exclude-standard orchestrator/rounds/round-185`
- Exit 0: `shasum src/MLF/Elab/TermClosure.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- Exit 0: `git diff -- src/MLF/Elab/TermClosure.hs test/PipelineSpec.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs orchestrator/rounds/round-185/implementation-notes.md`
- Exit 0: `rg -n 'sameLaneDoubleAliasFrameClearBoundaryExpr|sameLaneTripleAliasFrameClearBoundaryExpr keeps alias-shell preservation|sameLaneQuadrupleAliasFrameClearBoundaryExpr keeps alias-shell preservation|hasRetainedChildAliasBoundary v body [123] =|hasRetainedChildClearBoundary' test/PipelineSpec.hs src/MLF/Elab/TermClosure.hs`
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
- Exit 0: `nl -ba src/MLF/Elab/TermClosure.hs | sed -n '64,101p'`
- Exit 0: `nl -ba test/PipelineSpec.hs | sed -n '2154,2284p'`
- Exit 0: `nl -ba test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs | sed -n '44,150p'`
- Exit 0: `nl -ba orchestrator/rounds/round-185/implementation-notes.md`
- Exit 0: `shasum src/MLF/Elab/TermClosure.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- Exit 0: `git diff --check`
- Exit 0: `git diff --name-only -- src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/Scope.hs test/Research/C1AuthoritativeSurfaceSpec.hs test/Research/P5ClearBoundarySpec.hs test/Main.hs mlf2.cabal`

## Pass/Fail Results

- **Baseline 1: roadmap identity, pointer, and preserved-history consistency:** pass.
  - `orchestrator/state.json`, `orchestrator/roadmap.md`,
    `orchestrator/verification.md`, `orchestrator/retry-subloop.md`, and
    `selection.md` all resolve the same roadmap bundle:
    `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
    `roadmap_revision = rev-001`, and the matching `roadmap_dir`.
  - `git diff --name-only -- orchestrator/roadmaps` returned nothing, so prior
    roadmap families/revisions did not drift.
- **Baseline 2: diff hygiene:** pass.
  - `git diff --check` reported no whitespace or merge-marker problems before
    and after the full gate.
- **Baseline 3: roadmap metadata integrity:** pass.
  - The active roadmap still carries all five required metadata fields for all
    seven items.
- **Baseline 4: build and test gate for production/test changes:** pass.
  - `cabal build all && cabal test` passed with `1315 examples, 0 failures`.
- **Baseline 5: thesis conformance gate:** not applicable.
  - No thesis-facing files changed.
- **Baseline 6: worker-plan integrity:** not applicable.
  - This round does not use planner-authored worker fan-out.
- **Item-5 writable-slice, frozen-hash, and forbidden-file guard:** pass.
  - The code/test diff against base stays limited to
    `src/MLF/Elab/TermClosure.hs`, `test/PipelineSpec.hs`, and
    `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`, plus the
    pre-existing controller-owned `orchestrator/state.json`.
  - No forbidden pipeline, fallback, scope, Cabal, or adjacent research files
    drifted.
  - The frozen retry hashes still match the plan-locked values:
    `e4606fccbcb50c3e9229c56bc15c0b8bab152c65` for
    `src/MLF/Elab/TermClosure.hs` and
    `840761c0df2150b58d7f2246c8302a89fadbcb94` for
    `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`.
- **Item-5 representative evidence replay:** pass.
  - Focused packet checks passed:
    - quadruple-alias: `4 examples, 0 failures`
    - triple-alias: `4 examples, 0 failures`
    - double-alias: `3 examples, 0 failures`
    - alias: `3 examples, 0 failures`
  - The direct `cabal repl` probe returned matching `Right (...)` recursive
    authoritative outputs from both `runPipelineElab` and
    `runPipelineElabChecked` for the selected quadruple-alias packet.
- **Item-5 aggregate positive-family read honesty:** pass.
  - The retry keeps the read exact and packet-bounded: predecessor alias,
    double-alias, and triple-alias slices are replayed as focused evidence
    only; the selected quadruple-alias packet is the only fresh slice; no
    broader `P3`/`P4`/`P6` or repo-readiness claim appears.
- **Plan conformance:** pass.
  - The attempt-1 blocker is gone. The double-alias predecessor block no longer
    owns the moved numeric budget text, while the shared `v body 2` narration
    is confined to the adjacent triple guard and the exact depth-3 read stays
    owned by the selected quadruple guard.

## Plan Comparison

1. **Reconfirm the exact retry baseline before editing `test/PipelineSpec.hs`: satisfied.**
   - The frozen read-only seams still hash-match the values locked in
     `plan.md`, so the preserved attempt-1 production/research baseline stayed
     byte-identical through retry review.
   - The remaining live code delta is exactly the predecessor-guard cleanup in
     `test/PipelineSpec.hs`; `implementation-notes.md` narrates that narrowed
     state honestly.
2. **Repair only the `PipelineSpec` guard ownership that caused the rejection: satisfied.**
   - `test/PipelineSpec.hs:2158-2166` now keeps only invariant shared mechanism
     strings in the double-alias predecessor block; the moved numeric budget
     assertions are gone.
   - `test/PipelineSpec.hs:2256-2267` is the only predecessor-adjacent guard
     that still narrates the shared `hasRetainedChildAliasBoundary v body 2 =`
     truth.
   - `test/PipelineSpec.hs:2269-2281` remains the owner of the selected exact
     boundary read: it asserts `hasRetainedChildAliasBoundary v body 2 =`,
     forbids `hasRetainedChildAliasBoundary v body 3 =`, and keeps
     `hasRetainedChildClearBoundary` as the terminal bounded rule.
3. **Align the round notes with the narrowed retry diff: satisfied.**
   - `orchestrator/rounds/round-185/implementation-notes.md:12-18` now says
     the double-alias predecessor block was restored to read-only evidence and
     keeps any surviving budget-text ownership confined to the triple/selected
     guards only.
4. **Re-run focused checks and the full repo gate, then re-check the frozen seams: satisfied.**
   - All four focused same-lane checks passed, the direct quadruple-alias probe
     passed on both authoritative entrypoints, the post-gate hashes still match
     the locked values, `git diff --check` stayed clean, and the full
     `cabal build all && cabal test` gate passed.

## Evidence Summary

- `src/MLF/Elab/TermClosure.hs:66-95` remains the preserved frozen attempt-1
  baseline: `preserveRetainedChildAliasBoundary` uses
  `hasRetainedChildAliasBoundary v body 2 =`, the recursive alias-shell walk
  still decrements via `remainingAliasFrames - 1`, and
  `hasRetainedChildClearBoundary` remains the terminal bounded rule.
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs:50-58,137-144`
  still provides the exact frozen selected packet and both authoritative
  entrypoint assertions for
  `sameLaneQuadrupleAliasFrameClearBoundaryExpr`.
- `test/PipelineSpec.hs:2158-2190` now replays the double-alias predecessor
  block as read-only evidence only: no `hasRetainedChildAliasBoundary v body 1
  =`, `v body 2 =`, or `v body 3 =` ownership remains there.
- `test/PipelineSpec.hs:2256-2267` carries the shared triple-alias bounded-entry
  narration, while `test/PipelineSpec.hs:2269-2281` keeps the selected
  quadruple-alias exact depth-3 boundary read.
- `orchestrator/rounds/round-185/implementation-notes.md:3-18` records the
  selected-packet budget move, exact selected coverage, predecessor guard
  restoration, and focused verification without broadening the family claim.
- The selected packet remains green and bounded: both authoritative entrypoints
  return matching recursive `Right (...)` outputs, the focused tests are green,
  and no pipeline, fallback, scope, or interface surface widened.

## Retry Subloop Record

- Implemented stage result: `pass`
- Attempt verdict: `accepted`
- Stage action: `finalize`
- Retry reason: `none`
- Fix hypothesis: `none`

## Decision

**APPROVED**
