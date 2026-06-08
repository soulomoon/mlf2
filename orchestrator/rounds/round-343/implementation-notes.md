### Changes Made
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: added one owner-local bounded case-branch row helper family, `parseBoundedCaseBranchRows`, and routed ordinary source cases, non-depth nested cases, and nested depth 4 through 1 branch lists through it. The helper preserves the existing semicolon branch budget, `appendSourceCaseBranchText` ordering, `finishSourceCaseExpression` close behavior, and the final close-only path through `parseSourceCaseClose`.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: removed the migrated numbered source and nested case-branch continuation ladders instead of leaving compatibility aliases.
- `test/ProgramParserParitySpec.hs`: added focused static coverage proving the bounded case-branch row helper exists, source/nested/depth case parsers use it, and migrated numbered case-branch helper names are absent from the parser-library sources.

### Tests
- `test/ProgramParserParitySpec.hs`: verifies helper presence/use and absence of migrated numbered case-branch helper aliases.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser shares bounded case branch rows"'`: passed, 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser parses recursive ADT typeclass integration"'`: passed, 1 example, 0 failures, 291.0471 seconds.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: passed, 70 examples, 0 failures, 8735.8719 seconds.
- `git diff --check`: passed.
- focused static guard over parser-library/spec: passed; helper/call-site/spec evidence present and migrated case-branch aliases absent from parser-library source.
- shortcut/overclaim guard over changed parser-library/spec/docs lines: passed.

### Notes
The plan preferred first-order selector data based on round 342. A first pass with a `CaseBranchParser` dispatcher typechecked, but runtime verification exposed a recursive top-level binding lookup when source case parsing reached nested case parsing and re-entered the same dispatcher. The final helper passes the branch parser function directly; this typechecks in the `.mlfp` parser-library path, removes the dispatcher cycle, and keeps one shared bounded accumulation substrate.
