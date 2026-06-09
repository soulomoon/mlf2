### Checks Run
- Command: `git diff --check`
  Result: PASS; no whitespace or conflict-marker output.
- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares constructor row accumulation"'`
  Result: PASS; 1 example, 0 failures.
- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
  Result: PASS on serial rerun; 75 examples, 0 failures, finished in 8371.4375 seconds. An earlier parallel attempt of the same command failed before test execution with a `ghcup` temporary symlink collision, then this serial rerun completed successfully.
- Command: `rg -n "parseExact(Four|Five|Nine)ConstructorDataRows(Constructor[0-9]+Continue|Finish)|ValueConstructorRows|emptyConstructorRows|appendConstructorRow|dataRowsWithConstructorRows" test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`
  Result: PASS; constructor-row payload and helpers are present, exact four/five/nine parsers use the helper path, and migrated `Continue`/`Finish` helper aliases are absent from the parser library.
- Command: `git diff -- test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp test/ProgramParserParitySpec.hs | rg '^\+.*(fixture-name shortcut|pre-rendered projection|canonical-parser bypass|static negative evidence|compiler-seed/package/platform/proof hook|native/backend claim|package-manager/linker claim|self-boot claim|full parser parity claim|retired syntax alias)'`
  Result: PASS; no forbidden shortcut or overclaim matches. `rg` exited 1 because there were no matches.
- Command: `git status --short -- runtime/mlfp_io/target/release/libmlfp_io.d orchestrator/state.json`
  Result: PASS; no output, so the generated dependency file and controller-owned state are clean.
- Command: `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`
  Result: Not rerun by reviewer. Implementer recorded PASS with 2722 examples, 0 failures in `implementation-notes.md`; reviewer reran the focused constructor-row guard and aggregate parser-parity owner-surface gate, and the aggregate rerun alone took 8371.4375 seconds. No source-inspection or focused-rerun issue justified another duplicate full gate before review.

### Plan Compliance
- Step 1, inspect `ParserValue` and destructors: met. `ValueConstructorRows` was added as a narrow `ParserValue` payload, and existing destructor contexts return the same fallback families as non-owned payloads: `"unknown"`, `basicUnexpectedSpan`, or parser failure.
- Step 2, preserve non-constructor fallback behavior: met. Token, module-key, projection-row, and unit behavior are unchanged aside from exhaustive handling of the new constructor-row case.
- Step 3, add focused constructor-row accumulator helpers: met. `emptyConstructorRows`, `constructorRowsFromValue`, `appendConstructorRowText`, `appendConstructorRow`, and `dataRowsWithConstructorRows` are owner-local in `ParserParityParser.mlfp`.
- Step 4, migrate exact four-constructor rows: met. The parser starts with `emptyConstructorRows`, appends each constructor after its separator/final semicolon is parsed, and renders final data rows through `dataRowsWithConstructorRows`.
- Step 5, migrate exact five- and nine-constructor rows without compatibility aliases: met. The same accumulator pattern is used for all selected constructors, and migrated tuple-threading `Continue`/`Finish` aliases are absent from the parser library.
- Step 6, add focused static coverage: met. The new Hspec guard checks payload/helper phrases, representative exact four/five/nine use, and absence of migrated aliases.
- Step 7, run focused and standard validation: met with reviewer reruns for `git diff --check`, focused static guard, and aggregate parser parity. Full gate was not duplicated by reviewer for the recorded long-gate reason above; implementer recorded full-gate PASS.
- Step 8, record implementation evidence and non-claims: met. `implementation-notes.md` records the standard commands, full-gate evidence, generated-file restoration, and explicit non-claims.

### Findings
- Blocking: no
  Problem: No blocking findings
  Evidence: Source inspection plus rerun evidence above.
  Suggested fix: none

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
The actual diff is limited to `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`, `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`, and `test/ProgramParserParitySpec.hs`.

`appendConstructorRowText` preserves earlier constructor rows. `stringIndexOf` is used as `haystack -> needle`; `stringIndexOf "" existingRows` returns `Some` only when `existingRows` is empty and `None` for non-empty accumulated rows, so the helper emits `nextRows` for the first row and `appendLine existingRows nextRows` after that. The runtime implementation in `src/MLF/Frontend/Program/Run.hs` uses `findIndex (needle isPrefixOf) (tails haystack)`, and existing backend tests expect an empty needle to return index 0.

The migrated exact four/five/nine constructor parsers preserve the old parse order: `data`, data name, `=`, constructor name, `:`, source type, `|` for non-final constructors, and final `;`. Non-final constructor spans still end at the following separator token start through `spanFromStartToTokenStart (tokenStartCoordinate cNToken) separatorToken`; final constructor spans still end at the semicolon token start; data-row spans still use `parserCurrentTokenStartOr (tokenEndCoordinate semicolonToken)` after the semicolon.

No roadmap closeout is justified. This is bounded milestone-4 parser ergonomics substrate under `direction-4b-compiler-seed-parser-ergonomics-substrate`; it does not close parser parity, change milestone status, or require a semantic roadmap update.
