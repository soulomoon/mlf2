### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-319-parser-library-text-literal-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-004`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-004`

### Goal
Extend the existing shared parser-owned source-text `.mlfp` parser library so it parses canonical `Char` and `String` literal syntax and emits the same parser projection rows and token-derived spans as the current Haskell canonical parser for a thin parser-parity fixture.

The selected public behavior is a new positive fixture, `text-literal-char-string`, containing one module that imports `Char` and `String`, exports two values, and defines one `Char` literal value plus one `String` literal value using a non-ASCII Unicode scalar. Add one malformed literal negative path through the public parser-parity batch so malformed literal syntax is rejected by the same shared lexer/parser route.

This is parser parity only. Do not include checker, resolver, backend, platform, driver, proof, package-manager, full parser parity, or self-boot scope.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Work in vertical RED -> GREEN -> refactor cycles, starting from one public-interface parser-parity matcher that fails before parser-library changes.

Keep all grammar work inside `test/programs/compiler-parser-parity/parser-library/`. The new fixture package should be a thin harness that provides `sourceFile` and `sourceText`, calls `renderParserParityProjectionFromSourceText`, and contributes to the generated public CLI batch. Do not add a fixture-owned parser package, do not recognize the whole fixture source text, and do not return a prebuilt token stream for this fixture.

Extend the shared parser-owned lexer/token layer for literal tokens, the shared parser combinators/grammar for literal expressions, and the shared projection rendering so the parser library derives definition expression text and spans from consumed tokens. If `ProgramParserParitySpec` currently renders canonical `Char` or `String` literals only through fallback `Show`, update the test-support projection renderer to make the expected public parser projection explicit and stable.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED matcher:
   `MLF.Program parser parity / shared parser-owned .mlfp parser parses Char and String literals`
   The matcher should compare the Haskell canonical parser projection and the shared `.mlfp` parser-library projection for `text-literal-char-string`.
2. Add the committed positive fixture under `test/conformance/mlfp/parser-parity/text-literal-char-string/` with `src/Main.mlfp` and `expected/parser-program.txt`. The source should include two exported definitions, one `Char` literal and one `String` literal, and use at least one non-ASCII Unicode scalar in the literal text.
3. Add the thin parser-parity harness under `test/programs/compiler-parser-parity/text-literal-char-string/`. It should mirror the carried fixture pattern: `ParserParityFixture.mlfp` owns only `sourceFile` and `sourceText`, while `Main.mlfp` calls the shared parser-library entrypoint.
4. Register the new positive case in `test/ProgramParserParitySpec.hs` and the parser-parity batch. Add or extend canonical projection rendering for `Char` and `String` literals so expected rows are stable and explicit.
5. Extend `ParserParityLexer.mlfp`, `ParserParityToken.mlfp` if needed, and `ParserParityParser.mlfp` so literal tokens are scanned from source text, consumed through parser-owned combinators, and rendered as expression text from parser state. The success path must not use `stringIndexOf source <entire fixture source>` or a fixture-specific token-stream constructor.
6. Add one malformed literal negative source path through the generated public parser-parity batch, such as a missing closing quote. Route it through `renderParserNegativeEvidenceFromSourceText` and a parser-owned diagnostic category/span rather than Megaparsec prose.
7. Extend static guards in `ProgramParserParitySpec.hs` to reject round-319 shortcut shapes: fixture-specific token-stream names, `moduleKey`/`completeModuleKey` success keys for `text-literal-char-string`, whole-source literal recognition, and pre-rendered `Char`/`String` projection rows for the new fixture.
8. Keep docs and notes scoped to bounded parser parity if any wording changes are necessary. Do not update roadmap semantics or claim milestone-4 completion.

### Verification
- Focused RED before implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses Char and String literals/"'`
- Focused GREEN after implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses Char and String literals/"'`
- Negative literal diagnostic matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser rejects malformed text literal syntax through public run-program/"'`
- Parser-library shortcut/static guard:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
- Full parser-parity group:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Direct smoke/diff check for every parser-parity fixture, including `text-literal-char-string`, by running each `test/programs/compiler-parser-parity/<fixture>` through `cabal run mlf2 -- run-program ... --search-path test/programs/compiler-parser-parity/parser-library` and diffing against `test/conformance/mlfp/parser-parity/<fixture>/expected/parser-program.txt`.
- Diff and full closeout gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also write `selection-record.json` and `round-plan-record.json` beside this `plan.md`. They are the machine authority for lineage and worker scheduling. This round uses `worker_mode: "none"`.
