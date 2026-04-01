# Round 170 Plan

- Round: `round-170`
- Roadmap: `2026-04-01-00-general-automatic-iso-recursive-inference-successor-roadmap` / `rev-001`
- Item: `item-2`
- Retry: `null`
- Execution shape: serial, systematic-debugging-first, TDD-first

## Objective

Land one exact item-2 slice for the frozen packet
`sameLaneAliasFrameClearBoundaryExpr` without touching
`src/MLF/Elab/TypeCheck.hs`. The plan must first explain why the packet still
reaches `PipelineTypeCheckError (TCLetTypeMismatch ...)` after the existing
post-elaboration preservation passes, then add one focused RED regression,
then apply only the smallest lawful fix inside the frozen slice. If the
investigation proves the real fix lives only in an out-of-slice file, stop at
an exact narrower-blocker characterization rather than widening scope.

## Sequential Plan

1. Root-cause investigation first; modify no files in this step.
   - Replay the frozen packet from
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` on both
     authoritative entrypoints and compare it with the already-working
     recursive continuity case in `test/PipelineSpec.hs`.
   - Inspect the authoritative closure/preservation path in
     `src/MLF/Elab/Run/Pipeline.hs` and
     `src/MLF/Elab/TermClosure.hs` to answer one concrete question:
     does the mismatch survive because the retained-child preservation hooks
     stop at the current trivial shape
     `ELet v sch rhs (EVar v)` with an identity scheme, leaving the inherited
     packet's one-extra-`hold` same-lane alias binder unpreserved, or because
     the earlier closure/generalization target is already wrong before those
     hooks run?
   - Inspect `src/MLF/Elab/Run/Scope.hs` and
     `src/MLF/Elab/Run/ResultType/Fallback.hs` only to determine whether the
     in-slice `schemeBodyTarget` / result-type wrapper behavior feeds the same
     non-authoritative quantified scheme into closure.
   - Treat `src/MLF/Elab/Run/ResultType/Fallback/Core.hs` and
     `src/MLF/Elab/TypeCheck.hs` as read-only evidence only. They may explain
     the failure, but they are not lawful edit targets for this round.

2. Add the focused RED test before any production edit in `test/PipelineSpec.hs`.
   - Add one exact packet regression for
     `sameLaneAliasFrameClearBoundaryExpr`, named so it is easy to run in
     isolation, and assert the intended packet-level outcome on both
     `runPipelineElab` and `runPipelineElabChecked`.
   - Reuse the existing `expectAlignedPipelineSuccessType` / recursive-type
     helpers in `test/PipelineSpec.hs` so the test proves the packet should
     return one aligned recursive `mu`-bearing result rather than a
     `TCLetTypeMismatch` blocker.
   - Run that new example alone and observe the RED failure before touching
     any production file.

3. Apply the smallest lawful production change only inside the preserved
   authoritative path.
   - Primary edit target:
     `src/MLF/Elab/TermClosure.hs`.
     Extend `preserveRetainedChildAuthoritativeResult` only far enough to
     preserve the recursive rhs through this exact same-lane alias-frame
     clear-boundary shell if step 1 shows the current hook misses the
     one-extra-`hold` retained-child alias shape. Keep the extension packet-
     specific and structurally minimal; do not turn it into a general
     let-normalization pass.
   - Secondary edit target only if hook ordering or existing helper plumbing
     must change, and only for this packet:
     `src/MLF/Elab/Run/Pipeline.hs`.
   - Alternate in-slice fix path only if step 1 proves the blocked scheme is
     already chosen before the preservation pass:
     `src/MLF/Elab/Run/Scope.hs`,
     and, only if wrapper-level synchronization is required for the same
     packet, `src/MLF/Elab/Run/ResultType/Fallback.hs`.
   - Do not modify `src/MLF/Elab/Pipeline.hs`,
     `src-public/MLF/Pipeline.hs`,
     `test/Main.hs`, or `mlf2.cabal` unless a strictly necessary test-support
     seam is proven during step 1. Do not add fallback widening, new exports,
     generalized normalization, or any non-packet refactor.

4. Refresh the exact lane anchor honestly after the bounded fix attempt.
   - If step 3 makes the RED example pass, update
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` so the
     frozen packet no longer accepts broad blocker constructors blindly and
     instead asserts the exact success continuity now observed on both
     authoritative entrypoints.
   - If step 1 proves the only truthful fix lives outside the writable slice,
     do not fake a green result. Tighten
     `test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs` to the
     exact narrower blocker localized by the investigation and stop the round
     at `narrower current-architecture blocker`.
   - Keep the test surface packet-bounded. Do not add new test modules, and
     keep `test/Main.hs` and `mlf2.cabal` unchanged unless a new module is
     truly unavoidable.

## Verification Commands

- `python3 -m json.tool orchestrator/state.json >/dev/null`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && test -f "$roadmap_dir/roadmap.md" && test -f "$roadmap_dir/retry-subloop.md" && test -f "$roadmap_dir/verification.md"`
- `roadmap_dir="$(jq -r '.roadmap_dir' orchestrator/state.json)" && rg -n 'Item id:|Depends on:|Parallel safe:|Parallel group:|Merge after:' "$roadmap_dir/roadmap.md"`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "sameLaneAliasFrameClearBoundaryExpr"'`
- `cabal test mlf2-test --test-show-details=direct --test-options='--match "alias-frame clear-boundary packet"'`
- `python3 - <<'PY'\nimport subprocess, sys\nallowed = {\n    'src/MLF/Elab/Run/Pipeline.hs',\n    'src/MLF/Elab/TermClosure.hs',\n    'src/MLF/Elab/Run/Scope.hs',\n    'src/MLF/Elab/Run/ResultType/Fallback.hs',\n    'test/PipelineSpec.hs',\n    'test/Research/SameLaneRetainedChildRepresentativeGapSpec.hs',\n}\nout = subprocess.check_output(\n    ['git', 'diff', '--name-only', '--', 'src', 'src-public', 'test', 'mlf2.cabal'],\n    text=True,\n).splitlines()\nextra = [path for path in out if path and path not in allowed]\nif extra:\n    print('\\n'.join(extra))\n    sys.exit(1)\nprint('ITEM2_WRITABLE_SLICE_OK')\nPY`
- `git diff --check`
- `cabal build all && cabal test`
