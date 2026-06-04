### Checks Run
- Command: `git diff --check`
  Result: pass; exited 0 with no whitespace errors.
- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
  Result: pass; `MLF.Program parser parity` finished in 3030.7539 seconds with 36 examples, 0 failures, and `Test suite mlf2-test: PASS`.
- Command: `cmp -s test/programs/recursive-adt/typeclass-integration.mlfp test/conformance/mlfp/parser-parity/typeclass-integration/src/Main.mlfp && echo 'source fixture matches recursive-adt/typeclass-integration.mlfp'`
  Result: pass; the new canonical fixture source matches the selected recursive ADT/typeclass integration source.
- Command: `if rg -n 'typeclass-integration|TypeclassIntegration|instanceRows sourceFile "Eq"|methodDefinitionRows sourceFile "eq"|def same type=|def main type=Bool expr=same|typeclass-integration parser negative' test/programs/compiler-parser-parity/parser-library; then exit 1; else echo 'no typeclass-integration fixture-specific/static rows in parser-library'; fi`
  Result: pass; no fixture-name, whole-source, pre-rendered row, or static negative-evidence strings for this slice were found in the shared parser library.
- Command: `if find orchestrator/rounds/round-331 -maxdepth 2 -name '*.json' -print | rg .; then exit 1; else echo 'no paired JSON round artifacts under round-331'; fi`
  Result: pass; no paired JSON round artifacts were present.
- Command: `rg -n 'positive:typeclass-integration|negative:typeclass-integration-nested-case|renderParserNegativeEvidenceFromSourceText|sharedParserRound331ShortcutPhrases|typeclassIntegrationNegativeEvidenceProjection|ParserParityPositiveCase "positive:typeclass-integration"|ParserParityNegativeCase "negative:typeclass-integration-nested-case"' test/ProgramParserParitySpec.hs`
  Result: pass by inspection; the Hspec aggregate registration, negative-evidence path, dynamic renderer, and round-331 shortcut/static guard coverage are present.
- Command: `git diff -- CHANGELOG.md docs/mlfp-self-boot-readiness.md implementation_notes.md | rg -n '^\+.*(Round 331|not full parser parity|does not claim full parser parity|resolver/checker/backend|compiler-package|driver|platform|proof|self-boot)'`
  Result: pass by inspection; added docs use bounded parser-parity language and explicit non-claims.
- Command: `if git diff -- CHANGELOG.md docs/mlfp-self-boot-readiness.md implementation_notes.md | rg -n '^\+.*(milestone closeout|milestone-4|full parser parity is complete|parser parity is done)'; then exit 1; else echo 'no added docs milestone-closeout/full-completion claim'; fi`
  Result: pass; no added milestone closeout or parser-parity completion claim was found.

### Plan Compliance
- Step 1: met. Added `test/conformance/mlfp/parser-parity/typeclass-integration/src/Main.mlfp`, matching `test/programs/recursive-adt/typeclass-integration.mlfp`, and added `expected/parser-program.txt` with the canonical projection.
- Step 2: met. Added `test/programs/compiler-parser-parity/typeclass-integration/Main.mlfp` and `ParserParityFixture.mlfp`; the root exposes `sourceFile` and `sourceText` and calls `renderParserParityProjectionFromSourceText`.
- Step 3: met. `test/ProgramParserParitySpec.hs` registers the positive fixture, the aggregate public CLI section, the direct shared-parser assertion, and `negative:typeclass-integration-nested-case`.
- Step 4: met. `ParserParityParser.mlfp` extends shared parser-library paths for the selected `Eq` class, recursive `Nat`, explicit `Eq Nat` instance method, nested case expressions, `same`, and `main` without adding fixture-specific source recognition.
- Step 5: met. Round-331 shortcut/static guard phrases cover fixture keys, whole-source recognition, fixture token streams, pre-rendered instance/method/definition rows, and static negative evidence; the focused Hspec selector passed those guard examples.
- Step 6: met. `implementation_notes.md`, `CHANGELOG.md`, and `docs/mlfp-self-boot-readiness.md` were updated with bounded parser-parity evidence and explicit non-claims.

### Findings
- Blocking: no
  Problem: No blocking findings.
  Evidence: Required focused verification passed; manual audits found no round-331 parser-library shortcuts, no paired JSON round artifact, and no documentation overclaim.
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
The implemented diff matches the selected non-closeout parser-parity slice. The new conformance fixture copies the selected recursive ADT/typeclass integration source, the thin `.mlfp` parser root feeds source text through the shared parser-library entrypoint, and the expected projection is checked against the Haskell canonical parser by the focused Hspec selector.

The shared parser-library changes build typeclass-integration output from parsed tokens and parsed expression text. The parser-library audit found no `typeclass-integration` fixture keys, no `TypeclassIntegration` whole-source recognizer, no static `method-definition eq` / `def same` / `def main` row strings, and no static `typeclass-integration parser negative` evidence in `test/programs/compiler-parser-parity/parser-library`.

The generated aggregate public CLI driver covers the new positive section and the malformed nested-case negative section through `renderParserNegativeEvidenceFromSourceText`. The focused verification profile is sufficient because the plan explicitly scopes this as a non-closeout parser-parity slice with no production parser replacement, checker/resolver/backend/compiler-package/platform/proof/self-boot claim, and no milestone closeout request.
