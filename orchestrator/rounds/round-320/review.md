### Checks Run

- Command: `git fetch origin master && git rev-parse HEAD && git rev-parse origin/master && git merge-base HEAD origin/master && git merge-base --is-ancestor origin/master HEAD`
  Result: pass. `HEAD`, `origin/master`, and merge-base are all `48a26d928062a9a8429f1e74dc9975c3b3e284af`; `origin/master` is an ancestor of the assigned branch head.

- Command: `rg -n '^(<<<<<<<|=======|>>>>>>>)' . -g '!dist-newstyle/**' -g '!runtime/mlfp_io/target/**'`
  Result: pass. No conflict-marker matches.

- Command: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses first-class polymorphic source types/"'`
  Result: pass. 1 example, 0 failures; finished in 163.1016s.

- Command: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed first-class polymorphic source-type diagnostics through public run-program/"'`
  Result: pass. 1 example, 0 failures; finished in 242.9028s.

- Command: `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
  Result: pass. 1 example, 0 failures; finished in 0.4237s.

- Command: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  Result: pass. 13 examples, 0 failures; finished in 573.0751s.

- Command:
  ```sh
  for fixture in test/programs/compiler-parser-parity/*; do
    name=$(basename "$fixture")
    if [ "$name" = parser-library ]; then
      continue
    fi
    expected="test/conformance/mlfp/parser-parity/$name/expected/parser-program.txt"
    actual=$(mktemp)
    printf 'fixture %s\n' "$name"
    timeout 900 cabal run mlf2 -- run-program "$fixture" --search-path test/programs/compiler-parser-parity/parser-library > "$actual"
    diff -u "$expected" "$actual"
    rm -f "$actual"
  done
  ```
  Result: pass. All 22 parser-parity fixtures, including `first-class-polymorphism-source-types`, ran with no diffs.

- Command: `rg -n 'parseFirstClassPolymorphism|completeModuleKey "first-class-polymorphism-source-types"|moduleKey "first-class-polymorphism-source-types"|programKey "first-class-polymorphism-source-types"|FirstClassPolymorphismTokens|LexerOk firstClassPolymorphismTokens|first-class-polymorphism-source-types tokens|defRows sourceFile "usePoly"|def usePoly type=\(∀a\. a -> a\) -> Bool|def id type=∀a\. a -> a' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  Result: pass. No shortcut matches.

- Command: `git diff --check --cached && git diff --check`
  Result: pass. No whitespace errors.

- Command: `timeout 3600 cabal build all`
  Result: pass.

- Command: `timeout 7200 cabal test`
  Result: pass. 2660 examples, 0 failures; finished in 938.4282s.

- Command: `timeout 7200 ./scripts/thesis-conformance-gate.sh`
  Result: pass. Final result: `[thesis-gate] PASS: thesis conformance anchors are green`.

### Plan Compliance

- Shared parser-owned library parses first-class polymorphic source types: met. The new fixture projection matches the canonical parser projection, and the shared parser path consumes source text through `tokenizeCompleteModule`, `runParser`, `parserBind`, and `parserChoice`.
- Thin fixture/harness only: met. The new parser-parity package has only `Main.mlfp` and `ParserParityFixture.mlfp`; it provides `sourceFile`/`sourceText` and calls `renderParserParityProjectionFromSourceText`.
- Malformed source-type negative path through public parser-parity batch: met. The batch includes `negative:first-class-polymorphism-source-type`, and the matcher passes with `expected-constructor-forall-dot@...`.
- No round-320 shortcuts: met. Static guard and explicit `rg` audit found no fixture-specific parser entrypoint, fixture success key, prebuilt token stream, exact-source token path, or pre-rendered `usePoly`/`id`/`main` rows.
- Parser-owned combinator/Parser state architecture remains intact: met. The new source-type, definition, and expression parsing is implemented as parser-library grammar functions over the existing `Parser` state and combinators. The bounded three-definition body branch is token-driven and derives names, type text, expression text, and spans from consumed tokens rather than matching the fixture key or source text.
- Scope boundaries: met. The diff stays in parser-parity tests, fixtures, the parser-library fixture modules, and the conformance README inventory. No checker, resolver, backend, platform, driver, proof, full parser parity, or self-boot scope is introduced.
- Registration/doc hygiene: met. No new Haskell spec module or production module was added, so `mlf2.cabal` and `test/Main.hs` registration were not required. README wording is bounded to parser-parity fixture inventory and does not overclaim broader compiler support.
- TDD evidence: met from `implementation-notes.md`. It records the focused RED failure before implementation (`Right "parser-error\n"` instead of expected projection) and the subsequent focused GREEN checks; reviewer reran the green gates listed above.

### Decision

**APPROVED**

### Evidence

The assigned branch is fresh against `origin/master` at `48a26d928062a9a8429f1e74dc9975c3b3e284af`, conflict-marker and whitespace checks are clean, focused positive/negative/static tests passed, the full parser-parity group passed, all 22 direct parser-parity fixture smokes diffed cleanly, `cabal build all` passed, `cabal test` passed with 2660 examples, and the thesis conformance gate passed.
