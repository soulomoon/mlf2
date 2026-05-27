# GHC Parser-Shape Reference

This directory keeps the GHC comparison used by
`bench/parser-library-performance.md` reproducible.

The closest real GHC artifact to our generated `.mlfp` parser library is not a
testsuite fixture. It is `GHC.Parser`, generated from GHC's
`compiler/GHC/Parser.y`. GHC's `testsuite/tests/parser/*` tree is a collection
of parser correctness cases, so it is useful context but not a scale-equivalent
single module.

`ParserShape914.hs` is the controlled local reference: a synthetic Haskell
module with 914 parser-shaped top-level definitions. It is intentionally simpler
than GHC's real parser so it measures the lower bound for processing many
ordinary top-level parser definitions with shared module state.

Regenerate the checked-in module:

```bash
bench/ghc-reference/generate-parser-shape.sh \
  --defs 914 \
  --output bench/ghc-reference/ParserShape914.hs \
  --module ParserShape914
```

Refresh the timing artifact:

```bash
bench/run-ghc-parser-reference.sh \
  --runs 1 \
  --output bench/results/ghc-per-def-reference.tsv
```
