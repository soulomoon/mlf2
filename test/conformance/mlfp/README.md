# `.mlfp` Shared Conformance Corpus

This tree contains file-based `.mlfp` compiler behavior fixtures that can be
run unchanged by the current Haskell compiler and a future `.mlfp` compiler.
Expected outputs are committed oracle artifacts. Ordinary tests compare against
them exactly and must not regenerate, bless, or dynamically accept new outputs.

## Tracer Metadata Contract

Tracer fixtures use a plain line-oriented metadata file:

```text
fixture-id: cross-module-let-run-program
package-root: src
search-paths: none
command: run-program
expect: pass
normalization: none
stage-applicability: all
tags: package,public,cross-module,let-polymorphism
expected-stdout: expected/run-program.stdout
```

Fields are `key: value` lines. For these tracers, `package-root`,
`search-paths`, `expected-stdout`, and `expected-stderr` are resolved relative
to the directory containing `fixture.meta`. Use `search-paths: none` for
fixtures with no extra roots. Otherwise, `search-paths` is a comma-separated
ordered list of roots; the initial search-path tracer uses exactly one root.
The only recognized commands are `run-program` and `check-program`, the
recognized expected statuses are `pass` and `fail`, and the only normalization
profile is `none`. Passing fixtures name committed stdout with
`expected-stdout`; failing fixtures name committed stderr with
`expected-stderr`.

Any expected-output update is a reviewed source change. A test run may write
actual outputs only to an explicitly selected actual-output root in a later
round; this tracer does not provide regeneration or blessing tooling.

## Parser Parity Projections

`parser-parity/` contains committed parser-program projection oracles for
bounded canonical-parser parity tracers. These artifacts are compared exactly
against both the current Haskell canonical parser projection and the matching
`.mlfp` parser-owned package output. The carried `.mlfp` fixture packages now
route through the shared parser library at
`test/programs/compiler-parser-parity/parser-library/` via `--search-path`
instead of owning per-fixture parser modules. They are not generated or blessed
during test runs, and they do not imply full `.mlfp` parser parity. Current
bounded projection fixtures cover the basic Bool module, one import-exposing
Bool module, one value-definition-list Int/reference module, one let/lambda/app
Int expression module, one typed-annotation Int expression module, and one
data-declaration Nat module with constructor spans, plus two case-expression
Nat modules covering constructor, wildcard, and nested constructor patterns.
