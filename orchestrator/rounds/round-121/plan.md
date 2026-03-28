# Round 121 Plan (`item-2` Bounded `C1` / `P2` Implementation Slice)

## Objective

Execute roadmap item `2` only:
implement and validate one bounded current-architecture `C1`
authoritative-surface continuation slice.

This round is `attempt-1` with `retry: null`.

## Frozen Inputs

- the March 14 baseline contract;
- the March 25 capability contract;
- the March 25 full-pipeline validation contract;
- the accepted March 27 refreshed matrix and narrowed successor gate; and
- the accepted March 28 item-1 freeze
  `docs/plans/2026-03-28-c1-p2-authoritative-surface-successor-authority-success-bar-and-writable-slice-freeze.md`.

## Exact Success Target

For the exact admitted `C1` packet on `baseTarget -> baseC -> targetC`:

- keep the fallback surface evidence honest;
- make both authoritative public pipeline entrypoints preserve recursive
  structure (`containsMu True`);
- keep the behavior inside the inherited current architecture; and
- prove the result with focused C1 regressions plus the full Cabal gate.

## Writable Slice

This round may write only:

- `src/MLF/Elab/Run/Pipeline.hs`
- `test/Research/C1AuthoritativeSurfaceSpec.hs`
- `test/PipelineSpec.hs`
- `orchestrator/rounds/round-121/*`

The round may narrow further. It may not widen beyond the item-1 frozen slice.

## Implementation Hypothesis

The likely public-surface loss point is still the trivial let-alias closure
path, but the accepted freeze only permits a pipeline-surface intervention for
item `2`. The lawful move is therefore to keep `TermClosure.hs` unchanged and
add one bounded authoritative-result preservation step in
`runPipelineElab*` for the exact blocked `C1` alias shape, only when the
right-hand side already typechecks to a closed recursive result.

## Task List

1. Narrow the authoritative-result preservation logic so a trivial
   `let v = rhs in v` may return `rhs` for the exact blocked `C1` alias shape
   when `rhs` already has a closed recursive type, while preserving the
   existing retained-child identity case unchanged.
2. Update the focused `C1` harness to require recursive authoritative output.
3. Add or refresh one production-path regression in `PipelineSpec` for the
   exact non-local scheme-alias/base-like packet.
4. Record the implementation summary and verification evidence.

## Verification Commands

- `cabal test mlf2-test --builddir=dist-newstyle-round121-c1 --test-show-details=direct --test-options='--match "C1 authoritative-surface harness"'`
- `cabal test mlf2-test --builddir=dist-newstyle-round121-pipeline --test-show-details=direct --test-options='--match "selected non-local scheme-alias/base-like packet recursive on both authoritative pipeline entrypoints"'`
- `cabal build all && cabal test`
- `git diff --check`

## Exit Criteria

This round is complete only when:

- the exact `C1` packet stays inside the frozen slice;
- both authoritative public entrypoints become recursive on the focused C1
  evidence;
- the focused regressions pass;
- the full Cabal gate passes; and
- `implementation-notes.md` records the bounded change summary and evidence.
