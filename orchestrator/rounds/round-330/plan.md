### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-330-parser-library-recursive-tree-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-006`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-006`

### Goal
Extend the existing shared parser-owned source-text `.mlfp` parser library so it parses two adjacent recursive-tree parser-parity fixtures:

- `recursive-tree-first-order`, copied from `test/programs/recursive-adt/recursive-tree-first-order.mlfp`, covering `data Tree = Leaf : Tree | Branch : Tree -> Tree -> Tree`, recursive `mirror`, `isBranch`, constructor-pattern case branches, wildcard patterns, and nested constructor/application expressions.
- `recursive-tree-deriving`, copied from `test/programs/recursive-adt/recursive-tree-deriving.mlfp`, covering `class Eq`, a recursive `Tree` declaration with `deriving Eq`, and `eq (Branch Leaf Leaf) (Branch Leaf Leaf)`.

The public GREEN behavior is that the shared `.mlfp` parser-library projection for each new fixture exactly matches the Haskell canonical parser projection committed under `test/conformance/mlfp/parser-parity/<fixture>/expected/parser-program.txt`.

This round is parser parity only. Do not include checker, resolver, backend, platform, driver, proof, package-manager, full parser parity, milestone-4 closeout, or self-boot scope.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Proceed as vertical public-interface RED -> GREEN -> refactor cycles, not as a horizontal batch.

Start with this first public-interface RED matcher:

`MLF.Program parser parity / shared parser-owned .mlfp parser parses recursive tree first-order programs`

After that first slice is GREEN, add the adjacent deriving fixture through a second public-interface matcher:

`MLF.Program parser parity / shared parser-owned .mlfp parser parses recursive tree deriving programs`

Both fixture roots under `test/programs/compiler-parser-parity/` should remain thin harnesses: `ParserParityFixture.mlfp` owns `sourceFile` and `sourceText`, and `Main.mlfp` calls `renderParserParityProjectionFromSourceText` from `test/programs/compiler-parser-parity/parser-library/`.

Grow only the shared parser-owned lexer/parser-combinator path. Reuse and generalize existing data-declaration, constructor type, source-definition, annotated-lambda, case-expression, constructor-pattern, wildcard-pattern, deriving, and nested application helpers. Do not add fixture-owned parser packages, do not key success on either recursive-tree fixture name, do not recognize either whole fixture source text, and do not return prebuilt token streams or pre-rendered projection rows for these fixtures.

Because parser-parity public evidence runs through a checker-facing `.mlfp` program harness, preserve the rev-006 shared-context run constraint. The broad parser-parity regression must use the existing generated aggregate public CLI driver with labelled per-case sections. A standalone smoke/diff for each new fixture is allowed only as package-root evidence for that fixture, not as the broad regression strategy.

### Execution Profile
- Complexity: `standard`
- Verification profile: `standard`
- Reason: this is behavior-changing parser-library work over two same-owner recursive-tree fixtures. It is bounded and does not need closeout verification, but it needs implementer TDD evidence, reviewer judgment, focused parser checks, the aggregate parser-parity regression, full Cabal gates, and the thesis gate.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec matcher for `recursive-tree-first-order` in `test/ProgramParserParitySpec.hs`. It should compare the Haskell canonical parser projection and the shared `.mlfp` parser-library projection for the fixture.
2. Add committed fixture files under `test/conformance/mlfp/parser-parity/recursive-tree-first-order/`: `src/Main.mlfp` and `expected/parser-program.txt`. The expected projection should expose the module/export spans, `data Tree` span, `Leaf`/`Branch` constructor spans, and `mirror`, `isBranch`, and `main` definition spans with canonical expression text.
3. Add the thin public harness under `test/programs/compiler-parser-parity/recursive-tree-first-order/`. Keep it source/evidence only and route through `renderParserParityProjectionFromSourceText`.
4. Extend `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp` so the shared parser-owned grammar parses the first-order recursive tree surface from consumed tokens: two-field `Branch` constructor types, `Branch left right` and `Branch _ _` case patterns, recursive branch bodies such as `Branch (mirror right) (mirror left)`, and nested applications such as `isBranch (mirror (Branch Leaf Leaf))`.
5. Rerun the first focused matcher until GREEN, then refactor only while it stays green.
6. Add the second focused RED Hspec matcher for `recursive-tree-deriving`. Add committed fixture files under `test/conformance/mlfp/parser-parity/recursive-tree-deriving/` and the thin harness under `test/programs/compiler-parser-parity/recursive-tree-deriving/`.
7. Reuse the existing typeclass/deriving parser path and generalize only where needed so `data Tree ... deriving Eq` and `eq (Branch Leaf Leaf) (Branch Leaf Leaf)` render token-derived projection rows. Keep the Tree deriving support in shared grammar helpers, not a fixture-specific parser.
8. Register both positive fixtures in `parserParityPositiveCases` and in the generated aggregate parser-parity batch. The batch should include labelled `positive:recursive-tree-first-order` and `positive:recursive-tree-deriving` sections.
9. Add one public malformed recursive-tree negative path through the generated parser-parity batch, such as omitting the case branch arrow in a `Branch left right` branch. Render it through `renderParserNegativeEvidenceFromSourceText` with the existing stable parser-owned `expected-case-branch-arrow@...` diagnostic if that is the honest failure category.
10. Extend static guards in `test/ProgramParserParitySpec.hs` to reject round-330 shortcut shapes: fixture-specific token stream names, `moduleKey`/`completeModuleKey`/`programKey` success keys for either recursive-tree fixture, whole-source recognition, `parseRecursiveTree...` fixture entrypoints, pre-rendered `Tree`/`Branch`/`mirror`/`isBranch`/`main` rows, and static negative evidence strings for the new negative path.
11. Update bounded progress docs that already enumerate parser-parity fixtures when needed, such as `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md`. Keep wording scoped to bounded parser parity and do not claim checker/resolver/backend/platform/driver/proof, full parser parity, milestone-4 completion, or self-boot completion.
12. Run the focused RED/GREEN matchers, the new negative matcher, shortcut/static guards, the shared-context aggregate parser-parity batch, optional standalone fixture smoke/diffs, `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED before first implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses recursive tree first-order programs/"'`
- Focused GREEN after first implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses recursive tree first-order programs/"'`
- Focused RED/GREEN for the deriving slice:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses recursive tree deriving programs/"'`
- Negative recursive-tree diagnostic matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed recursive tree diagnostics through public run-program/"'`
- Parser-library shortcut/static guard:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
- Static negative-evidence guard:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser reaches success only after complete syntax and dynamic diagnostics/"'`
- Shared-context aggregate parser-parity batch:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/runs all .mlfp parser parity fixtures through one generated public CLI driver/"'`
- Full parser-parity group:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Optional standalone new-fixture public-interface smoke/diffs, justified only as package-root evidence:
  ```sh
  actual=$(mktemp)
  timeout 900 cabal -v0 run mlf2 -- run-program test/programs/compiler-parser-parity/recursive-tree-first-order --search-path test/programs/compiler-parser-parity/parser-library > "$actual"
  diff -u test/conformance/mlfp/parser-parity/recursive-tree-first-order/expected/parser-program.txt "$actual"
  rm -f "$actual"

  actual=$(mktemp)
  timeout 900 cabal -v0 run mlf2 -- run-program test/programs/compiler-parser-parity/recursive-tree-deriving --search-path test/programs/compiler-parser-parity/parser-library > "$actual"
  diff -u test/conformance/mlfp/parser-parity/recursive-tree-deriving/expected/parser-program.txt "$actual"
  rm -f "$actual"
  ```
- New-fixture shortcut audit:
  `rg -n 'parseRecursiveTree|completeModuleKey "recursive-tree-first-order"|completeModuleKey "recursive-tree-deriving"|moduleKey "recursive-tree-first-order"|moduleKey "recursive-tree-deriving"|programKey "recursive-tree-first-order"|programKey "recursive-tree-deriving"|RecursiveTreeFirstOrderTokens|RecursiveTreeDerivingTokens|LexerOk recursiveTreeFirstOrderTokens|LexerOk recursiveTreeDerivingTokens|recursive-tree-first-order tokens|recursive-tree-deriving tokens|stringIndexOf sourceText "module RecursiveTree"|stringIndexOf "module RecursiveTree" sourceText|defRows sourceFile "mirror"|defRows sourceFile "isBranch"|defRows sourceFile "main"|dataRows sourceFile "Tree"|constructorRows sourceFile "Branch"|recursive-tree parser negative expected-case-branch-arrow@' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Existing fixture/exact-source audits should remain green, and the new fixtures should be included in their banned-shape lists.
- Diff and full standard gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Scheduler
- Depends on round ids: none
- Merge after item ids: none
- Parallel group: none

### Worker Fan-Out
- Worker mode: none
- Workers: none
- Integration: none
