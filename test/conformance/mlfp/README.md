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
against the current Haskell canonical parser projection and one generated
`.mlfp` public CLI driver. That driver embeds the carried fixture source texts,
imports the shared parser library at
`test/programs/compiler-parser-parity/parser-library/` via `--search-path`,
and renders all positive, negative, and retry evidence sections in one run so
the shared parser library is checked once per spec run rather than once per
fixture. The committed oracles are not generated or blessed during test runs,
and they do not imply full `.mlfp` parser parity. Current bounded projection
fixtures cover the basic Bool module, one import-exposing
Bool module, one value-definition-list Int/reference module, one let/lambda/app
Int expression module, one typed-annotation Int expression module, and one
data-declaration Nat module with constructor spans, plus two case-expression
Nat modules covering constructor, wildcard, and nested constructor patterns,
plus two typeclass/instance modules covering class method signatures,
`deriving`, instance method definitions, and one malformed instance-method
diagnostic path, plus two higher-kinded/constrained class modules covering
kinded declaration parameters, variable-headed type applications, superclass
constraints, multi-parameter class heads, Unicode functional dependencies,
empty instance bodies, and one malformed functional-dependency arrow
diagnostic path, plus two closed type-family/type-level modules covering
kind-variable result kinds, kinded/plain family parameters, type-level
constructor and variable patterns, type-level lambda/application RHS syntax,
source type-family application annotations, and one malformed type-family
equation diagnostic path, plus two GADT/existential constructor modules
covering parameterized data heads, GADT-style constructor result heads,
constructor-local Unicode `∀`, nested source-type applications in constructor
fields/results, related constructor/case patterns, and one malformed
constructor-local forall-dot diagnostic path, plus two qualified
import/reference modules covering import aliases, alias-only imports, exposed
classes/types/constructors/values/methods, qualified
value/type/constructor/class/method references, and one malformed import-alias
diagnostic path.
