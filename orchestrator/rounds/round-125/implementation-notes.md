# Round 125 Implementation Notes

Date: 2026-03-28
Round: `round-125`
Roadmap item: `item-2`
Attempt: `attempt-1`
Outcome branch: `no lawful recursive carrier found`

## Lawfulness Probe

- Audited the writable production slice in `src/MLF/Elab/Run/ResultType/Fallback.hs`,
  `src/MLF/Elab/Run/Pipeline.hs`, `src/MLF/Elab/Pipeline.hs`, and
  `src-public/MLF/Pipeline.hs`.
- The retained recursive continuity logic is gated by the local-`TypeRef`
  / retained-child path in `computeResultTypeFallbackCore`; the exact frozen
  packet `ELam "x" (EVar "x")` does not satisfy those predicates.
- Strengthened the existing exact-packet regression in `test/PipelineSpec.hs`
  so the round now checks:
  - the internal fallback route from `computeResultTypeFallback`; and
  - both authoritative entrypoints `runPipelineElab` and
    `runPipelineElabChecked`.
- All three routes remain `containsMu False` for the exact frozen packet.

## Chosen Branch

- `no lawful recursive carrier found`
- No production code change was made.
- The round is fail-closed: preserve inherited semantics for the exact packet
  and refresh focused evidence only.

## Files Changed

- `test/PipelineSpec.hs`
- `orchestrator/rounds/round-125/implementation-notes.md`

## Commands

- `cabal test mlf2-test --builddir=dist-newstyle-round125-control --test-show-details=direct --test-options='--match "keeps retained-child fallback recursive through a same-lane local TypeRef root"'`
  - control rerun completed successfully.
- `cabal test mlf2-test --builddir=dist-newstyle-round125-p1 --test-show-details=direct --test-options='--match "does not infer recursive shape for the corresponding unannotated variant"'`
  - focused exact-packet rerun completed successfully with the strengthened
    fail-closed evidence.
- `cabal build all && cabal test`
  - passed; required because `test/PipelineSpec.hs` changed.
- `git diff --check`
  - no whitespace or conflict-marker damage.
