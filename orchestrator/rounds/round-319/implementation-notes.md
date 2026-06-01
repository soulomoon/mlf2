# Round 319 Implementation Notes

## Item

- `item-319-parser-library-text-literal-extension`

## Changes

- Added the positive `text-literal-char-string` conformance fixture with exported `sampleChar : Char = 'λ'` and `sampleString : String = "hello λ"` source forms.
- Added the thin public harness under `test/programs/compiler-parser-parity/text-literal-char-string/`.
- Extended the parser-owned shared `.mlfp` parser library:
  - `ParserParityLexer.mlfp` now scans generic identifiers plus `char-literal:` and `string-literal:` token text from source.
  - `ParserParityParserCombinator.mlfp` now exposes `expectTokenPrefix`.
  - `ParserParityParser.mlfp` now consumes Char/String literal tokens through parser state and renders definition rows from consumed tokens.
- Extended `test/ProgramParserParitySpec.hs`:
  - focused matcher for Char/String literal parity,
  - canonical projection rendering for `LChar` and `LString`,
  - batch registration for the positive fixture,
  - malformed text literal negative case through `renderParserNegativeEvidenceFromSourceText`,
  - round-319 shortcut/static guards for fixture keys, fixture token streams, and pre-rendered literal rows.

## TDD Evidence

- RED: `timeout 3600 cabal test mlf2-test --test-options='--match "Char and String literals"'`
  - Result: 1 example, 1 failure.
  - Failure: shared parser returned `Right "parser-error\n"` instead of the expected `text-literal-char-string` projection.
- GREEN: `timeout 3600 cabal test mlf2-test --test-options='--match "Char and String literals"'`
  - Result: 1 example, 0 failures.

## Verification

- Negative literal diagnostic matcher:
  - `timeout 3600 cabal test mlf2-test --test-options='--match "malformed text literal"'`
  - Result: 1 example, 0 failures.
- Shortcut/static guard:
  - `timeout 1800 cabal test mlf2-test --test-options='--match "expanded grammar paths"'`
  - Result: 1 example, 0 failures.
- Direct smoke/diff:
  - Ran every `test/programs/compiler-parser-parity/<fixture>` harness against `test/programs/compiler-parser-parity/parser-library`.
  - Result: all 21 parser-parity fixtures printed `PASS`, including `text-literal-char-string`.
- Full parser-parity group:
  - `timeout 3600 cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
  - Result: 11 examples, 0 failures.
- Whitespace:
  - `git diff --check`
  - Result: pass.
- Build:
  - `timeout 7200 cabal build all`
  - Result: pass.
- Full test suite:
  - `timeout 7200 cabal test`
  - Result: 2658 examples, 0 failures.
- Thesis gate:
  - `timeout 7200 ./scripts/thesis-conformance-gate.sh`
  - Result: `[thesis-gate] PASS: thesis conformance anchors are green`.

## Blockers

- None.
