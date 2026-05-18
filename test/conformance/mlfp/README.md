# `.mlfp` Shared Conformance Corpus

This tree contains file-based `.mlfp` compiler behavior fixtures that can be
run unchanged by the current Haskell compiler and a future `.mlfp` compiler.
Expected outputs are committed oracle artifacts. Ordinary tests compare against
them exactly and must not regenerate, bless, or dynamically accept new outputs.

## Tracer Metadata Contract

The initial tracer fixture uses a plain line-oriented metadata file:

```text
fixture-id: cross-module-let-run-program
package-root: src
command: run-program
expect: pass
normalization: none
stage-applicability: all
tags: package,public,cross-module,let-polymorphism
expected-stdout: expected/run-program.stdout
```

Fields are `key: value` lines. For this tracer, `package-root` and
`expected-stdout` are resolved relative to the directory containing
`fixture.meta`. The only recognized command is `run-program`, the only expected
status is `pass`, and the only normalization profile is `none`.

Any expected-output update is a reviewed source change. A test run may write
actual outputs only to an explicitly selected actual-output root in a later
round; this tracer does not provide regeneration or blessing tooling.
