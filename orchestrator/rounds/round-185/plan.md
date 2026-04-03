# Round 185 Plan

- Round: `round-185`
- Roadmap: `2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap` / `rev-001`
- Item: `item-5`
- Retry: `attempt-2` (`rejected -> retry`)
- Execution shape: serial, review-driven conformance retry, exact `sameLaneQuadrupleAliasFrameClearBoundaryExpr` slice only, no concurrent `cabal` jobs

## Objective

Keep this round on exactly one bounded same-lane retained-child packet:
`sameLaneQuadrupleAliasFrameClearBoundaryExpr`.

Attempt 1 already produced the selected packet as green in the canonical round
worktree: `src/MLF/Elab/TermClosure.hs` carries the bounded entry-budget move
from `hasRetainedChildAliasBoundary v body 1 =` to
`hasRetainedChildAliasBoundary v body 2 =`, the research spec adds exact
quadruple-alias authoritative-entrypoint coverage, and `test/PipelineSpec.hs`
adds the selected quadruple-alias regression plus the exact depth-3 guard.
Review rejected the round anyway because the double-alias predecessor
source/mechanism guard was also rewritten from `1 / not 2` to `2 / not 3`,
which exceeds the approved predecessor-evidence handling.

This retry must therefore preserve the already-green selected packet while
removing the plan divergence. Budget-marker ownership now belongs only to the
selected quadruple-alias guard and, if honest narration of the shared source
truth requires it, the adjacent triple-alias source/mechanism guard. The
double-alias predecessor block returns to read-only predecessor evidence and
must not own the moved entry-budget text.

No new route-family widening, fallback-core edit, pipeline-facade edit, cyclic
or multi-SCC behavior, equi-recursive reasoning, fallback widening, or
second-interface work is authorized.

## Locked Retry Context

- Stage: `plan`
- Attempt: `attempt-2`
- Current review feedback:
  - retry reason: predecessor double-alias source/mechanism evidence was
    edited outside the approved round plan
  - fix hypothesis: restore the double-alias predecessor control to read-only
    evidence and confine any budget-text update to the selected quadruple-alias
    guard plus, if unavoidable, the adjacent triple-alias source/mechanism
    guard only
- Active selection input:
  `orchestrator/rounds/round-185/selection.md`
- Active controller pointer:
  `orchestrator/state.json` resolves
  `roadmap_id = 2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap`,
  `roadmap_revision = rev-001`, and
  `roadmap_dir = orchestrator/roadmaps/2026-04-02-01-general-automatic-iso-recursive-full-inference-roadmap/rev-001`

Current worktree state is already non-pristine. Respect existing edits and do
not revert unrelated work:

- `M orchestrator/state.json` is pre-existing controller-owned state and must
  remain untouched.
- `M src/MLF/Elab/TermClosure.hs` is the selected-packet production baseline
  from attempt 1 and is now read-only for this retry.
- `M test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` is the
  selected-packet research-spec baseline from attempt 1 and is now read-only
  for this retry.
- `M test/PipelineSpec.hs` contains both the selected-packet additions and the
  blocking predecessor-plan divergence; only the exact guard ownership cleanup
  is writable now.
- `?? orchestrator/rounds/round-185/` is the round-owned directory. The
  implementer may update `implementation-notes.md` only if needed to keep the
  retry artifact honest.

Read-only authoritative and supporting seams relevant to this retry:

- `src/MLF/Elab/Run/Pipeline.hs`
  - `runPipelineElab`
  - `runPipelineElabChecked`
  - `runPipelineElabWith`
  - the `preserveRetainedChildAuthoritativeResult` call site
- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
  - `boundVarTargetRoot`
  - `boundHasForallFrom`
  - `sameLaneLocalRetainedChildTarget`
  - `keepTargetFinal`
  - `targetC`
  - `schemeBodyTarget`
  - `rootFinalInvolvesMu`

Those files remain read-only context only. The live route family and guard
cluster stay fixed to the accepted item-3 boundary:

- `sameLaneLocalRetainedChildTarget`
- `boundHasForallFrom`
- `keepTargetFinal`
- `targetC`

## Frozen Read-Only Baseline

The selected-packet production and research seams are already in the state this
retry is meant to preserve. Treat them as byte-frozen:

- `src/MLF/Elab/TermClosure.hs`
  - expected hash during this retry:
    `e4606fccbcb50c3e9229c56bc15c0b8bab152c65`
  - selected-packet truth to preserve:
    `preserveRetainedChildAliasBoundary` calls
    `hasRetainedChildAliasBoundary v body 2 =`
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
  - expected hash during this retry:
    `840761c0df2150b58d7f2246c8302a89fadbcb94`
  - selected-packet truth to preserve:
    exact `sameLaneQuadrupleAliasFrameClearBoundaryExpr` plus its two
    authoritative-entrypoint assertions

Accepted predecessor packet behavior remains read-only evidence only:

- `sameLaneAliasFrameClearBoundaryExpr`
- `sameLaneDoubleAliasFrameClearBoundaryExpr`
- `sameLaneTripleAliasFrameClearBoundaryExpr`

Do not translate one exact packet into general `P3`, `P4`, or `P6` closure or
into a repo-level readiness claim.

## Write Scope

Implementer-owned writes for retry attempt 2 are narrowed to:

- `test/PipelineSpec.hs`
  - the predecessor block named
    `sameLaneDoubleAliasFrameClearBoundaryExpr double-alias clear-boundary packet preserves recursive output on both authoritative entrypoints`
  - the adjacent guard named
    `sameLaneTripleAliasFrameClearBoundaryExpr keeps alias-shell preservation within the bounded entry budget`
    only if the shared `v body 2` source truth cannot be narrated honestly
    without it
  - the selected-packet guard named
    `sameLaneQuadrupleAliasFrameClearBoundaryExpr keeps alias-shell preservation exact at depth 3`
    only if a tiny wording realignment is needed after the predecessor cleanup
- `orchestrator/rounds/round-185/implementation-notes.md`
  - revise the round notes so they describe the retained selected-packet
    baseline plus the predecessor-guard restoration honestly

Do not modify:

- `src/MLF/Elab/TermClosure.hs`
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`
- `orchestrator/rounds/round-185/selection.md`
- `orchestrator/state.json`
- `orchestrator/roadmaps/**`
- `docs/plans/**`
- `TODO.md`
- `implementation_notes.md`
- `Bugs.md`
- `src/MLF/Elab/Run/Pipeline.hs`
- `src/MLF/Elab/Pipeline.hs`
- `src-public/MLF/Pipeline.hs`
- `src/MLF/Elab/Run/ResultType/Fallback/Core.hs`
- `src/MLF/Elab/Run/ResultType/Fallback.hs`
- `src/MLF/Elab/Run/Scope.hs`
- `test/Research/C1AuthoritativeSurfaceSpec.hs`
- `test/Research/P5ClearBoundarySpec.hs`
- `test/Main.hs`
- `mlf2.cabal`

Keep the round packet-bounded to
`sameLaneQuadrupleAliasFrameClearBoundaryExpr` only.

## Sequential Plan

1. Reconfirm the exact retry baseline before editing `test/PipelineSpec.hs`.
   - Inspect the current diff in the four round-local files only:
     `src/MLF/Elab/TermClosure.hs`,
     `test/PipelineSpec.hs`,
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`, and
     `orchestrator/rounds/round-185/implementation-notes.md`.
   - Reconfirm that the selected quadruple-alias packet and the predecessor
     alias / double-alias / triple-alias slices are already green on the
     current attempt-1 diff.
   - Record the frozen hashes for `src/MLF/Elab/TermClosure.hs` and
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs`; those
     hashes must match again at the end of the retry.
   - End this step with one explicit diagnosis only: the remaining blocker is
     the ownership of the moved entry-budget text inside
     `test/PipelineSpec.hs`, not the selected production baseline.

   Verification commands:

   ```bash
   git diff -- src/MLF/Elab/TermClosure.hs test/PipelineSpec.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs orchestrator/rounds/round-185/implementation-notes.md
   shasum src/MLF/Elab/TermClosure.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs
   cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuadrupleAliasFrameClearBoundaryExpr"'
   cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'
   cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'
   cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'
   ```

2. Repair only the `PipelineSpec` guard ownership that caused the rejection.
   - In the double-alias predecessor block, keep the behavioral authoritative
     output assertions unchanged.
   - Remove the moved budget-marker ownership from that predecessor block:
     delete the exact numeric entry-budget assertions there instead of moving
     them further forward again. The double-alias block may keep only invariant
     retained-child mechanism strings that remain honest under the current
     `TermClosure` source.
   - Leave the selected quadruple-alias behavioral regression unchanged.
   - Prefer leaving the adjacent triple-alias guard exactly as it is now. Edit
     it only if the current shared `v body 2` source truth would otherwise be
     described dishonestly after the double-alias cleanup.
   - Keep the selected quadruple-alias guard as the owner of the exact boundary
     read: it must still assert
     `hasRetainedChildAliasBoundary v body 2 =`,
     still forbid `hasRetainedChildAliasBoundary v body 3 =`, and still show
     `hasRetainedChildClearBoundary` as the terminal bounded rule.
   - Do not touch `src/MLF/Elab/TermClosure.hs` or the research spec while
     making this cleanup.

   Verification commands:

   ```bash
   rg -n 'sameLaneDoubleAliasFrameClearBoundaryExpr|sameLaneTripleAliasFrameClearBoundaryExpr keeps alias-shell preservation|sameLaneQuadrupleAliasFrameClearBoundaryExpr keeps alias-shell preservation|hasRetainedChildAliasBoundary v body [123] =' test/PipelineSpec.hs
   cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'
   cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'
   cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuadrupleAliasFrameClearBoundaryExpr"'
   ```

3. Align the round notes with the narrowed retry diff.
   - Update `orchestrator/rounds/round-185/implementation-notes.md` so it
     no longer claims that predecessor source assertions were updated.
   - Record the honest retry result instead:
     the attempt keeps the selected `TermClosure` budget move and exact
     quadruple-alias evidence, restores the double-alias predecessor block to
     read-only evidence, and leaves any budget-text ownership limited to the
     selected quadruple guard plus the adjacent triple guard only if that
     guard still needs to narrate the shared source truth.
   - Do not add any broader readiness, family, or architecture claim.

   Verification command:

   ```bash
   git diff -- orchestrator/rounds/round-185/implementation-notes.md
   ```

4. Re-run focused checks and the full repo gate, then re-check the frozen
   seams.
   - Re-run the exact selected packet plus the alias / double-alias /
     triple-alias predecessor slices.
   - Re-run the frozen-file hashes and confirm they still match the values
     locked above for `src/MLF/Elab/TermClosure.hs` and the research spec.
   - Check diff hygiene.
   - Confirm no forbidden file drift into pipeline facades, fallback files,
     scope files, adjacent research controls, `test/Main.hs`, or Cabal wiring.
   - Because the round diff still touches `src/` and `test/`, finish with the
     full repo gate.

   Verification commands:

   ```bash
   shasum src/MLF/Elab/TermClosure.hs test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs
   git diff --check
   git diff --name-only -- src/MLF/Elab/Run/Pipeline.hs src/MLF/Elab/Pipeline.hs src-public/MLF/Pipeline.hs src/MLF/Elab/Run/ResultType/Fallback/Core.hs src/MLF/Elab/Run/ResultType/Fallback.hs src/MLF/Elab/Run/Scope.hs test/Research/C1AuthoritativeSurfaceSpec.hs test/Research/P5ClearBoundarySpec.hs test/Main.hs mlf2.cabal
   cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneQuadrupleAliasFrameClearBoundaryExpr"'
   cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneTripleAliasFrameClearBoundaryExpr"'
   cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneDoubleAliasFrameClearBoundaryExpr"'
   cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'
   cabal build all && cabal test
   ```

## Planned End-State

This retry is complete only when all of the following are true:

- the selected packet
  `sameLaneQuadrupleAliasFrameClearBoundaryExpr`
  stays green on `runPipelineElab` and `runPipelineElabChecked`
- the double-alias predecessor block in `test/PipelineSpec.hs` no longer owns
  the moved entry-budget text
- any surviving `v body 2` budget-marker narration lives only in the selected
  quadruple guard and, if still necessary for honesty, the adjacent triple
  guard
- `src/MLF/Elab/TermClosure.hs` still hashes to
  `e4606fccbcb50c3e9229c56bc15c0b8bab152c65`
- `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` still hashes to
  `840761c0df2150b58d7f2246c8302a89fadbcb94`
- `cabal build all && cabal test` passes

The retry remains packet-bounded, non-widening, and item-5-only throughout.
