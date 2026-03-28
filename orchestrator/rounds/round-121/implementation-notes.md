# Round 121 Implementation Notes

## Change Summary

- restored `TermClosure.hs` to the accepted item-1 writable boundary and moved
  the bounded `C1` preservation step onto the allowed pipeline surface in
  `src/MLF/Elab/Run/Pipeline.hs`;
- extended the authoritative-result preservation path for the exact blocked
  `C1` trivial let-alias shape when the right-hand side is already closed and
  recursive;
- updated the focused `C1` authoritative-surface harness to require recursive
  public output; and
- added a matching production-path regression for the exact non-local
  scheme-alias/base-like packet.

## Verification Log

- `cabal test mlf2-test --builddir=dist-newstyle-round121-c1 --test-show-details=direct --test-options='--match "C1 authoritative-surface harness"'`
  - Result: pass
- `cabal test mlf2-test --builddir=dist-newstyle-round121-pipeline --test-show-details=direct --test-options='--match "selected non-local scheme-alias/base-like packet recursive on both authoritative pipeline entrypoints"'`
  - Result: pass
- `cabal build all && cabal test`
  - Result: pass (`1149 examples, 0 failures`)
- `git diff --check`
  - Result: pass

## Boundary Note

- The live code diff stays inside the accepted item-1 writable slice:
  `src/MLF/Elab/Run/Pipeline.hs`,
  `test/Research/C1AuthoritativeSurfaceSpec.hs`,
  `test/PipelineSpec.hs`,
  and round-owned `round-121` artifacts.
- `TermClosure.hs` was restored to the inherited same-lane behavior and is not
  part of the final round diff.
