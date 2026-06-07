### Checks Run
- Command: `git status --short --branch --untracked-files=all`
  Result: pass. Worktree is on `orchestrator/round-341-ergonomics-substrate`; tracked changes are limited to `test/ProgramParserParitySpec.hs`, `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`, and `test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`, with round plan/implementation artifacts untracked before this review.
- Command: `git status --short -- orchestrator/state.json`
  Result: pass. No output; reviewer did not edit controller-owned state.
- Command: `git diff --name-only`
  Result: pass. Tracked diff is limited to the parser-library/spec files selected by the plan.
- Command: `git diff --check`
  Result: pass. No whitespace or patch format errors.
- Command: `rg -n "data ParserExpectation|def parserDiagnosticForExpectation|def parserFailExpected :|def parserFailExpectedAtCurrent|def labelExpected|labelExpected ParserExpect|parserFailExpectedAtCurrent ParserExpect|parserFailExpected ParserExpect" test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp test/ProgramParserParitySpec.hs`
  Result: pass. Found `ParserExpectation`, `parserDiagnosticForExpectation`, `parserFailExpected`, `parserFailExpectedAtCurrent`, and `labelExpected` in the combinator module, plus migrated call sites and Hspec guard phrases.
- Command: `if rg -n "(parserFailExpected(ImportSemicolon|DefSemicolon|ImportExposingSeparator|CaseBranchArrow|ConstructorForallDot|ExpressionCloseParen)AtCurrent|label(UnexpectedSource|Equals|ImportSemicolon|ImportAlias|DefSemicolon|LetIn|LetAnnotationType|ConstructorColon|CaseBranchArrow|InstanceMethodEquals|FunctionalDependencyArrow|TypeFamilyEquationEquals|ConstructorForallDot))" test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp; then exit 1; else printf 'no removed expectation aliases in parser-library files\n'; fi`
  Result: pass. Output: `no removed expectation aliases in parser-library files`.
- Command: `rg -n "parserChoice :|UnexpectedSourceText _ -> runParser second state|Expected(Equals|ImportSemicolon|ImportAlias|ImportExposingSeparator|DefSemicolon|LetIn|LetAnnotationType|ConstructorColon|CaseBranchArrow|InstanceMethodEquals|FunctionalDependencyArrow|TypeFamilyEquationEquals|ConstructorForallDot|ExpressionCloseParen) span -> ParserStepError|UnexpectedSourceText span -> ParserStepError \(parserDiagnosticForExpectation expectation span\)" test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp`
  Result: pass. `parserChoice` backtracks retryable `UnexpectedSourceText` and `ExpectedCompleteModule`; committed expected diagnostics remain returned unchanged. `labelExpected` relabels only `UnexpectedSourceText` through `parserDiagnosticForExpectation`.
- Command: `if git diff --unified=0 -- test/ProgramParserParitySpec.hs test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp test/programs/compiler-parser-parity/parser-library/ParserParityParserCombinator.mlfp | rg -n "^\+.*(fixture-name shortcut|pre-rendered|static negative evidence|canonical-parser bypass|canonical parser bypass|compiler-seed shortcut|compiler-package|platform/proof|platform|proof|native/backend|package-manager|linker|self-boot|full parser parity|full-parser-parity|moduleKey \"Seed|ParserTextMatched -> moduleKey|stringSlice source|renderParserParityProjectionFromSourceText .*->|renderParserNegativeEvidenceFromSourceText .*->)"; then exit 1; else printf 'no shortcut or overclaim additions in changed parser-library/spec lines\n'; fi`
  Result: pass. Output: `no shortcut or overclaim additions in changed parser-library/spec lines`.
- Command: `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`
  Result: pass. `68 examples, 0 failures`; finished in `9740.8990 seconds`. Log: `dist-newstyle/build/aarch64-osx/ghc-9.14.1/mlf2-0.2.0.0/t/mlf2-test/test/mlf2-0.2.0.0-mlf2-test.log`.

### Plan Compliance
- Step 1: met. The diff removes repeated current-token expected-failure helpers and expectation-specific relabel helpers from `ParserParityParserCombinator.mlfp`.
- Step 2: met. `ParserParityParserCombinator.mlfp` now owns `ParserExpectation` and maps each expectation to a `ParserDiagnostic` with `parserDiagnosticForExpectation`.
- Step 3: met. `parserFailExpected`, `parserFailExpectedAtCurrent`, and `labelExpected` provide the generic substrate. `parserChoice` behavior remains plausibly unchanged for the round claim: retryable unexpected-source failures can be relabeled, while committed expected diagnostics are not silently backtracked.
- Step 4: met. `ParserParityParser.mlfp` imports `ParserExpectation(..)`, `parserFailExpected`, `parserFailExpectedAtCurrent`, and `labelExpected`, and its migrated call sites use the generic expectation constructors instead of the removed helper names.
- Step 5: met. `test/ProgramParserParitySpec.hs` adds static coverage requiring the expectation substrate/use phrases and rejecting reintroduced compatibility aliases in parser-library source.
- Step 6: met. No repo-facing docs were changed, and the changed lines make no full parser parity, compiler-package, platform/proof, native/backend, package-manager/linker, or self-boot claim.

### Findings
- Blocking: no
  Problem: No blocking findings.
  Evidence: Scope guard, alias-removal guard, shortcut/overclaim guard, `git diff --check`, and the focused parser-parity Hspec command all passed.
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
The implementation stays inside the rev-007 parser ergonomics substrate slice: the tracked code diff touches only the shared parser-library combinator, parser-library parser, and parser-parity spec. The round does not edit production parser, checker, resolver, backend, package, platform, proof, or roadmap files, and `orchestrator/state.json` remains untouched.

The new substrate is used rather than merely defined. `ParserParityParserCombinator.mlfp` exports `ParserExpectation(..)`, `parserFailExpected`, `parserFailExpectedAtCurrent`, and `labelExpected`; `ParserParityParser.mlfp` imports those names and uses `ParserExpect...` constructors at current-token failures and relabeling call sites.

The removed helper names are absent from parser-library source. The Hspec static guard intentionally stores those removed names as strings, then checks the concatenated parser-library source does not contain them.

The focused profile is sufficient under rev-007 verification because the slice is confined to parser-library/spec owner surface, preserves parser-choice behavior, and makes no milestone closeout or thesis-facing self-boot/package/platform/proof claim. Full closeout gates, `cabal build all && cabal test`, and `./scripts/thesis-conformance-gate.sh` are not required for this focused non-closeout substrate slice.
