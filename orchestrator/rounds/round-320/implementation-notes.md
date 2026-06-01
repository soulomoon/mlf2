### Changes Made
- `test/conformance/mlfp/parser-parity/first-class-polymorphism-source-types/`: added the canonical source fixture and expected parser projection for first-class polymorphic source types.
- `test/programs/compiler-parser-parity/first-class-polymorphism-source-types/`: added the thin public harness that calls the shared parser library.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: added parser-owned source type, definition row, and expression parsing for the selected fixture surface, deriving type text, expression text, and spans from consumed tokens.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`: added a current-token diagnostic helper for the existing `ExpectedConstructorForallDot` diagnostic.
- `test/ProgramParserParitySpec.hs`: added the focused matcher, batch positive case, malformed source-type negative case, and round-320 shortcut guards.
- `test/conformance/mlfp/README.md`: updated the bounded parser-parity fixture inventory.

### Tests
- RED: `timeout 3600 cabal test mlf2-test --test-options='--match "first-class polymorphic source types" --fail-on=empty'` failed 1 example before implementation with `Right "parser-error\n"` instead of the expected projection.
- GREEN: `timeout 900 cabal run mlf2 -- check-program test/programs/compiler-parser-parity/text-literal-char-string --search-path test/programs/compiler-parser-parity/parser-library` passed.
- GREEN: `timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/first-class-polymorphism-source-types --search-path test/programs/compiler-parser-parity/parser-library` printed the expected projection.
- GREEN: `timeout 3600 cabal test mlf2-test --test-options='--match "first-class polymorphic source types" --fail-on=empty'` passed 1 example.
- GREEN: `timeout 3600 cabal test mlf2-test --test-options='--match "malformed first-class polymorphic source-type diagnostics" --fail-on=empty'` passed 1 example.
- GREEN: `timeout 300 cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints" --fail-on=empty'` passed 1 example.
- GREEN: `timeout 3600 cabal test mlf2-test --test-options='--match "MLF.Program parser parity" --fail-on=empty'` passed 13 examples.
- GREEN: direct smoke/diff loop over every `test/programs/compiler-parser-parity/*` fixture, excluding `parser-library`, passed with no diffs.
- GREEN: `rg -n 'parseFirstClassPolymorphism|completeModuleKey "first-class-polymorphism-source-types"|moduleKey "first-class-polymorphism-source-types"|programKey "first-class-polymorphism-source-types"|FirstClassPolymorphismTokens|LexerOk firstClassPolymorphismTokens|first-class-polymorphism-source-types tokens|defRows sourceFile "usePoly"|def usePoly type=\(∀a\. a -> a\) -> Bool|def id type=∀a\. a -> a' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs` returned no matches.
- GREEN: `git diff --check` passed.
- GREEN: `timeout 3600 cabal build all` passed.
- GREEN: `timeout 7200 cabal test` passed 2660 examples.
- GREEN: `timeout 7200 ./scripts/thesis-conformance-gate.sh` passed.

### Notes
- The malformed source-type path reuses the existing parser-owned `expected-constructor-forall-dot@...` label because the underlying missing-token shape is the same forall-dot diagnostic class.
- A bounded three-definition parser is used for this thin parser-parity fixture instead of an optional more-or-close chain; the optional chain triggered presolution failure in `ParserParityParser.def-bindings`.
