# Benchmarks

This directory holds focused frontend and typechecker performance artifacts.
The active benchmark runner uses the canonical repo fixture:

```text
test/programs/packages/cross-module-let
```

The generated `.mlfp` package fixtures are kept as historical/ad hoc inputs,
but they are not part of the default repeated benchmark because they exercise
the same shape.

Regenerate the checked-in baseline fixture:

```bash
./bench/generate-benchmarks.sh
```

Generate a larger cross-module let fixture:

```bash
./bench/generate-benchmarks.sh --name cross-module-let-depth-25 --let-depth 25
```

Run the canonical fixture and write the timing summary:

```bash
./bench/run-benchmarks.sh --runs 1 --output bench/results/latest.tsv
```

The benchmark runner applies `+RTS -A64m -RTS` to `mlf2`, matching the nursery
setting used by the GHC reference benchmarks.

That command writes:

```text
bench/results/latest.tsv
bench/results/latest.runs.tsv
```

Run a specific package fixture through the same harness:

```bash
./bench/run-benchmarks.sh \
  --runs 1 \
  --benchmark parser-library test/programs/compiler-parser-parity/parser-library \
  --output bench/results/parser-library-latest.tsv
```

For library-only package roots that intentionally have no `main`, allow the
expected CLI status while still collecting module-check timings:

```bash
./bench/run-benchmarks.sh \
  --runs 1 \
  --allow-status 1 \
  --benchmark parser-library test/programs/compiler-parser-parity/parser-library \
  --output bench/results/parser-library-latest.tsv
```

Run the canonical fixture through the same CLI path used by normal package checks:

```bash
cabal build exe:mlf2
MLF_PROGRAM_TIMING_DETAIL=1 \
  "$(cabal list-bin exe:mlf2)" check-program test/programs/packages/cross-module-let
```

Operation-level and per-definition nested pipeline timing are opt-in because
they add substantial measurement overhead on generated libraries:

```bash
./bench/run-benchmarks.sh \
  --runs 1 \
  --operations \
  --benchmark parser-library test/programs/compiler-parser-parity/parser-library \
  --output bench/results/parser-library-operation-latest.tsv

MLF_PROGRAM_TIMING_DETAIL=1 MLF_PROGRAM_TIMING_OPERATIONS=1 MLF_PROGRAM_TIMING_DEF_DETAILS=1 \
  "$(cabal list-bin exe:mlf2)" check-program test/programs/compiler-parser-parity/parser-library
```

The committed before/after records for the group-finalizer run are stored in:

```text
bench/baselines/cross-module-let-baseline.before-group-finalizer.tsv
bench/baselines/cross-module-let-baseline.after-group-finalizer.tsv
```

A standalone comparison table for later optimization rounds is stored in:

```text
bench/cross-module-let-baseline-performance.md
```

The parser-library exact module read-context comparison is stored in:

```text
bench/parser-library-performance.md
```

The matched synthetic many-definition `.mlfp`/Haskell comparison is stored in:

```text
bench/many-defs-performance.md
```

The persisted GHC parser-shape scale reference is stored in:

```text
bench/ghc-reference/ParserShape914.hs
bench/results/ghc-per-def-reference.tsv
```

Refresh it with:

```bash
bench/run-ghc-parser-reference.sh \
  --runs 1 \
  --output bench/results/ghc-per-def-reference.tsv
```

Generate and compare a matched synthetic mixed many-definition module in
`.mlfp` and Haskell:

```bash
bench/run-many-defs-comparison.sh \
  --defs 1000 \
  --runs 1 \
  --output bench/results/many-defs-comparison.tsv
```

That command writes the generated sources and per-tool timing artifacts:

```text
bench/benchmarks/many-defs-1000/Main.mlfp
bench/ghc-reference/ManyDefs1000.hs
bench/results/many-defs-comparison.mlfp.tsv
bench/results/many-defs-comparison.ghc.tsv
bench/results/many-defs-comparison.tsv
```

To compare a future run, capture stderr and read the same top-level timing:

```bash
MLF_PROGRAM_TIMING_DETAIL=1 \
  "$(cabal list-bin exe:mlf2)" check-program test/programs/packages/cross-module-let \
  >/tmp/cross-module-let-baseline.stdout \
  2>/tmp/cross-module-let-baseline.stderr
rg "program\.check\.modules |program\.check\.module\.Prelude\.(constructor|instance|def)-bindings|real |user |sys " \
  /tmp/cross-module-let-baseline.stderr
```

For cost-center profiles, build the profiled executable once and run the same
package root:

```bash
cabal build exe:mlf2 --enable-profiling --enable-library-profiling --ghc-options='-fprof-auto -rtsopts'
"$(cabal list-bin exe:mlf2 --enable-profiling --enable-library-profiling --ghc-options='-fprof-auto -rtsopts')" \
  check-program test/programs/packages/cross-module-let +RTS -p -s -RTS
```
