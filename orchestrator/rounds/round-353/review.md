### Checks Run
- Command: `git diff --check`
  Result: pass; no whitespace errors reported.

- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares parser-value source-span extraction substrate"'`
  Result: pass; 1 example, 0 failures.

- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
  Result: pass; 80 examples, 0 failures; Hspec finished in 8532.4838 seconds.

- Command: `bash -lc 'set -euo pipefail; p="test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp"; s="test/ProgramParserParitySpec.hs"; for x in "def parserValueTokenTextOrUnknown : ParserValue -> String" "def parserValueDroppedTokenTextOrUnknown : Int -> ParserValue -> String" "def parserValueProjectionOrTokenTextOrUnknown : ParserValue -> String" "def parserValueConstructorRowsOrEmpty : ParserValue -> String" "def parserValueTokenStartCoordinateOrUnexpected : ParserValue -> String" "def parserValueTokenEndCoordinateOrUnexpected : ParserValue -> String" "def parserValueModuleKeyOrTokenStartCoordinateOrUnexpected : ParserValue -> String" "def parserValueTokenEndOrModuleKeyCoordinateOrUnexpected : ParserValue -> String" "def parserValueTokenSpanOrUnexpected : ParserValue -> String" "def parserValueTokenBoundsSpanOrUnexpected : ParserValue -> ParserValue -> String" "def parserValueTokenStartToStartSpanOrUnexpected : ParserValue -> ParserValue -> String" "parserValueDroppedTokenTextOrUnknown 11 value" "parserValueDroppedTokenTextOrUnknown 13 value" "parserValueDroppedTokenTextOrUnknown 15 value" "parserValueDroppedTokenTextOrUnknown 4 value" "parserValueProjectionOrTokenTextOrUnknown value" "parserValueConstructorRowsOrEmpty value" "parserValueModuleKeyOrTokenStartCoordinateOrUnexpected value" "parserValueTokenEndOrModuleKeyCoordinateOrUnexpected value" "parserValueTokenBoundsSpanOrUnexpected startValue endValue" "parserValueTokenStartToStartSpanOrUnexpected startValue endValue" "parserValueTokenSpanOrUnexpected value" "constructorRows sourceFile (identifierNameFromValue constructorToken) (parserTextFromValue typeValue) coordinates"; do rg -Fq "$x" "$p"; done; for x in "shared parser-owned .mlfp parser shares parser-value source-span extraction substrate" "sharedParserValueSourceSpanSubstratePhrases" "sharedParserValueSourceSpanUsePhrases" "sharedParserRemovedParserValueSourceSpanFallbackPhrases"; do rg -Fq "$x" "$s"; done; ! rg -U "def (identifierNameFromValue|charLiteralTextFromValue|stringLiteralTextFromValue|intLiteralTextFromValue|parserTextFromValue|coordinateFromValue|tokenStartCoordinate|tokenEndCoordinate|constructorRowsFromValue|moduleNameFromValue) : ParserValue -> String =\n    .*case value of" "$p"; ! rg -U "def (spanFromTokenBounds|spanFromTokenStartToTokenStart) : ParserValue -> ParserValue -> String =\n    .*case startValue of" "$p"; echo "static helper/call-site/duplicate-fallback-removal guard passed"'`
  Result: pass; reviewer rerun found the required parser-value extraction helpers, migrated wrapper/call-site phrases, spec guard phrases, and no old direct fallback case blocks on migrated helper names.

- Command: `bash -lc 'set -euo pipefail; if git diff -U0 -- test/ProgramParserParitySpec.hs test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp | awk '\''/^\+[^+]/ { print }'\'' | rg -i "fixture-name shortcut|pre-rendered projection|canonical-parser bypass|canonical parser bypass|static negative evidence|retired syntax alias|compiler-package|compiler package|platform hook|proof hook|native completion|backend completion|package-manager|package manager|linker|self-boot completion|self boot completion|full parser parity claim|full parser parity"; then exit 1; else rc=$?; if [ "$rc" -eq 1 ]; then echo "changed-line shortcut/overclaim guard passed"; else exit "$rc"; fi; fi'`
  Result: pass; no changed implementation/spec lines introduced shortcut or overclaim phrases.

### Plan Compliance
- Step 1: met; the diff targets the existing token text, coordinate, span, constructor-row, module-name, and projection-token helper surface in `ParserParityParser.mlfp`.
- Step 2: met; `ParserParityParser.mlfp` now defines the owner-local parser-value helper surface for token text, dropped token text, projection-or-token text, constructor rows, start/end coordinates, module-key coordinate fallback, token span, and token-bound spans.
- Step 3: met; `identifierNameFromValue`, literal text helpers, `parserTextFromValue`, coordinate helpers, token-bound span helpers, and start-to-coordinate helpers route through the new helper surface.
- Step 4: met; constructor-row append and projection-token code use the migrated helper surface without widening into grammar sequencing or diagnostics redesign.
- Step 5: met; static guard and diff inspection show the migrated wrappers no longer carry their old direct hand-rolled fallback case blocks.
- Step 6: met; `test/ProgramParserParitySpec.hs` adds the focused static guard for substrate phrases, representative use phrases, and removed fallback phrases.
- Step 7: met; no `CHANGELOG.md` or durable docs update was needed because the behavior stayed inside the focused parser-library/spec substrate slice. `implementation-notes.md` records bounded non-claim language.
- Step 8: met; all focused verification checks required by the plan and active `verification.md` were rerun by reviewer.

### Findings
- Blocking: no
  Problem: No blocking findings.
  Evidence: `git diff --check`, focused Hspec selector, aggregate parser parity group, static helper guard, changed-line overclaim guard, and fallback inspection all passed.
  Suggested fix: none.

### Decision
**APPROVED**

### Retry
- Retry target: none
- Required changes: none

### Roadmap Closeout
- Mode: none
- Status changes: none
- Completion pointers: none
- History entries: none
- Semantic update reason: none

### Evidence
The implementation is confined to `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp` and `test/ProgramParserParitySpec.hs`. It does not touch production parser, checker, resolver, backend, package, platform, proof, native, or roadmap files.

Fallback semantics were preserved. Token text helpers still return `"unknown"` for non-token/non-projection payloads. Coordinate helpers still return `basicUnexpectedSpan` for non-coordinate payloads. `coordinateFromValue` and `tokenStartCoordinate` preserve `ValueModuleKey` as the current-token start-coordinate fallback, and `tokenEndCoordinate` preserves the end-or-module-key coordinate fallback. Constructor-row extraction still returns the empty string for non-constructor-row values. `projectionLineFromToken` still fails non-token values with `ExpectedCompleteModule basicUnexpectedSpan`.

The aggregate parser-parity Hspec group is the focused owner-surface gate for this substrate slice. Full closeout gates, `cabal build all && cabal test`, and `./scripts/thesis-conformance-gate.sh` were not run because the approved verification profile is focused, the diff stays inside parser-library/spec/round-artifact scope, and the round makes no thesis-facing semantic, package/platform/proof/native/backend, milestone-closeout, self-boot, compiler-package, package-manager, linker, or full-parser-parity claim.
