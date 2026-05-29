# many-defs performance table

Active synthetic fixture:

```text
bench/benchmarks/many-defs-1000
bench/ghc-reference/ManyDefs1000.hs
```

The generated shape is mixed: 1000 annotated top-level value definitions, two
top-level helper definitions, one exported/checkable result definition, and two
small ADTs. The value definitions cycle through aliases, local polymorphic lets,
global polymorphic instantiation, higher-order function arguments, ADT
construction, and case analysis. This measures ordinary many-definition module
overhead across several checker paths, not parser-combinator or typeclass-heavy
behavior.

Generation and benchmark command:

```bash
bench/run-many-defs-comparison.sh \
  --defs 1000 \
  --runs 1 \
  --no-build \
  --output bench/results/many-defs-comparison.tsv
```

`bench/run-many-defs-comparison.sh` defaults this fixture to
`MLF_MODULE_DEF_BATCH_SIZE=160`, the best one-run batch setting from the latest
batch sweep.

The `.mlfp` side is checked with:

```text
MLF_PROGRAM_TIMING_DETAIL=1 mlf2 +RTS -A64m -RTS check-program bench/benchmarks/many-defs-1000
```

The Haskell side is checked with:

```text
ghc -fforce-recomp -fno-code -O0 -v0 bench/ghc-reference/ManyDefs1000.hs +RTS -A64m
```

Source artifacts:

```text
bench/results/many-defs-comparison.tsv
bench/results/many-defs-comparison-batch8.tsv
bench/results/many-defs-comparison-detail-current.tsv
```

## Headline

Current fair comparison uses `MLF_PROGRAM_TIMING_DETAIL=1` without per-operation
logging. The older operation-timed rows are still useful for diagnosis, but they
emit many per-binding metrics and are not comparable to GHC wall-clock time.

| Tool / metric | Batch setting | Checked top-level defs | Total | Per def | Ratio vs GHC |
| --- | --- | ---: | ---: | ---: | ---: |
| GHC 9.12.2 wall-clock `real` | `+RTS -A64m` | 1003 | 250.000ms | 0.249ms | 1.00x |
| `mlf2` `program.check.module.Main.def-bindings` | `MLF_MODULE_DEF_BATCH_SIZE=160`, `+RTS -A64m` | 1003 | 248.211ms | 0.247ms | 0.99x |
| `mlf2` `program.check.module.Main` | `MLF_MODULE_DEF_BATCH_SIZE=160`, `+RTS -A64m` | 1003 | 348.635ms | 0.348ms | 1.40x |
| `mlf2` `program.check.modules` | `MLF_MODULE_DEF_BATCH_SIZE=160`, `+RTS -A64m` | 1003 | 910.849ms | 0.908ms | 3.65x |

## GHC Comparison

This table compares per-definition build/check time directly against GHC's
`-fforce-recomp -fno-code -O0` wall-clock time on the matched Haskell module.

| Compiler / slice | Batch setting | Total | Per def | Ratio vs GHC per def |
| --- | --- | ---: | ---: | ---: |
| GHC 9.12.2 wall-clock `real` | `+RTS -A64m` | 250.000ms | 0.249ms | 1.00x |
| `mlf2` `Main.def-bindings` | `MLF_MODULE_DEF_BATCH_SIZE=160`, `+RTS -A64m` | 248.211ms | 0.247ms | 0.99x |
| `mlf2` `Main` module check | `MLF_MODULE_DEF_BATCH_SIZE=160`, `+RTS -A64m` | 348.635ms | 0.348ms | 1.40x |
| `mlf2` package check including Prelude | `MLF_MODULE_DEF_BATCH_SIZE=160`, `+RTS -A64m` | 910.849ms | 0.908ms | 3.65x |

## Reading

- The isolated `Main.def-bindings` lane is now faster than GHC in the latest
  one-run fair comparison: `0.247ms/def` versus GHC's `0.249ms/def`.
- The full package number is still dominated by checking the built-in Prelude
  from source in each compiler process (`~560ms` in this run). GHC is using
  installed interfaces for its Prelude/base environment.
- Operation-timed benchmark files remain useful for finding hot paths, but their
  per-binding logging inflates wall-clock time and should not be used as the
  direct GHC comparison.
