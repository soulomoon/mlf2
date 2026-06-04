### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-329-parser-library-recursive-list-tail-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-005`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-005`

### Goal
Extend the existing shared parser-owned source-text `.mlfp` parser library so it parses one bounded frozen recursive-list source surface through a thin parser-parity fixture:

- `module RecursiveList export (Nat(..), List(..), tailOrNil, isNil, main) { ... }`;
- `data Nat = Zero : Nat | Succ : Nat -> Nat;`;
- `data List = Nil : List | Cons : Nat -> List -> List;`;
- `def tailOrNil : List -> List = λ(xs : List) case xs of { Nil -> Nil; Cons _ rest -> rest };`;
- `def isNil : List -> Bool = λ(xs : List) case xs of { Nil -> true; Cons _ _ -> false };`;
- `def main : Bool = isNil (tailOrNil (Cons Zero Nil));`.

The public GREEN behavior is that the shared `.mlfp` parser-library projection for `recursive-list-tail` exactly matches the Haskell canonical parser projection committed under `test/conformance/mlfp/parser-parity/recursive-list-tail/expected/parser-program.txt`.

This round is parser parity only. Do not include checker, resolver, backend, platform, driver, proof, package-manager, full parser parity, or self-boot scope.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Start with one public-interface RED matcher:

`MLF.Program parser parity / shared parser-owned .mlfp parser parses recursive list tail`

Add a new conformance fixture and thin public parser-parity harness named `recursive-list-tail`. The source should be copied from `test/programs/recursive-adt/recursive-list-tail.mlfp` unless the implementer finds a smaller equivalent source that still covers the selected syntax. The fixture root under `test/programs/compiler-parser-parity/recursive-list-tail/` should contain only `Main.mlfp` plus `ParserParityFixture.mlfp`; it must provide `sourceFile` and `sourceText` and call `renderParserParityProjectionFromSourceText` from `test/programs/compiler-parser-parity/parser-library/`.

Grow the shared parser-owned lexer/parser-combinator path for this syntax. The expected parser work is to make declaration-led modules compose two recursive data declarations followed by three generic source definitions, then render token-derived rows for `tailOrNil`, `isNil`, and `main`. Reuse and generalize the existing parser-owned export-list, data declaration, constructor type, source-definition, annotated-lambda, case-expression, constructor-pattern, wildcard-pattern, identifier-pattern, parenthesized application, and nested constructor/application helpers. Do not add a fixture-owned parser package, do not key success on `recursive-list-tail`, do not recognize the whole fixture source text, and do not return a prebuilt token stream or pre-rendered projection rows for this fixture.

Because this is parser-parity public evidence that runs through a checker-facing `.mlfp` program harness, preserve the rev-005 shared-context run constraint. The broad parser-parity regression must use the existing generated aggregate public CLI driver with labelled per-case sections. Do not add or require a repeated loop of separate `run-program` invocations for every parser-parity fixture. A single standalone smoke/diff for only the new fixture is allowed as public-interface package-root evidence; if used, it must be labelled as that reason rather than as the broad regression strategy.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec matcher named above in `test/ProgramParserParitySpec.hs`. The matcher should compare the Haskell canonical parser projection and the shared `.mlfp` parser-library projection for `recursive-list-tail`.
2. Add committed fixture files under `test/conformance/mlfp/parser-parity/recursive-list-tail/`: `src/Main.mlfp` and `expected/parser-program.txt`. The expected projection should expose the `RecursiveList` module span, `Nat(..)`/`List(..)`/`tailOrNil`/`isNil`/`main` export spans, `data Nat` and `data List` rows, `Zero`/`Succ`/`Nil`/`Cons` constructor rows, and the three definition spans with canonical expression text for the two annotated-lambda case expressions and the nested `isNil (tailOrNil (Cons Zero Nil))` application.
3. Add the thin public harness under `test/programs/compiler-parser-parity/recursive-list-tail/`. Keep it source/evidence only: `ParserParityFixture.mlfp` owns `sourceFile` and `sourceText`, and `Main.mlfp` calls the shared parser-library entrypoint.
4. Register the positive fixture in `test/ProgramParserParitySpec.hs` and in the generated parser-parity batch. After registration, the batch should include the carried 29 positive parser-parity fixtures plus `positive:recursive-list-tail`.
5. Extend `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp` so declaration-led modules can parse the bounded `Nat` plus `List` data-declaration body through parser-owned combinators. Prefer shared helpers that parse constructor rows from consumed tokens and compose them with `appendProjectionValues` and `finishModuleBodyRows`; avoid naming helper APIs around the whole fixture.
6. Ensure constructor type rendering handles the selected multi-field constructor type `Nat -> List -> List`, and constructor pattern rendering handles `Cons _ rest` and `Cons _ _` without static rows. Reuse existing nested source-arrow and case-pattern helpers where possible.
7. Ensure `parseSourceExpression` and its helpers parse the selected definition expressions from consumed tokens: annotated lambdas whose bodies are `case` expressions, branch bodies that are constructors, identifiers, booleans, or applications, and the nested parenthesized application `isNil (tailOrNil (Cons Zero Nil))`. Extend only shared expression/case/application helpers if a gap appears.
8. Add one public malformed recursive-list negative path through the generated parser-parity batch, such as omitting the branch arrow in `Cons _ rest rest`. Render it through `renderParserNegativeEvidenceFromSourceText` with the stable parser-owned `expected-case-branch-arrow@...` diagnostic if that is the honest failure category; if the implementer finds a more precise shared parser-owned category, use that category and keep the committed evidence public and labelled.
9. Extend static guards in `test/ProgramParserParitySpec.hs` to reject round-329 shortcut shapes, including fixture-specific token stream names, `moduleKey`/`completeModuleKey`/`programKey` success keys for `recursive-list-tail`, whole-source recognition, `parseRecursiveListTail...` entrypoints, pre-rendered `tailOrNil`/`isNil`/`main` rows for this fixture, static `data List`/`constructor Cons` rows, and static negative evidence strings for the new negative path.
10. Update bounded progress docs that already enumerate parser-parity fixtures when needed, such as `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md`. Keep wording scoped to bounded parser parity and do not claim checker/resolver/backend/platform/driver/proof, full parser parity, milestone-4 completion, or self-boot completion.
11. Run the focused RED/GREEN matcher, the new negative matcher, shortcut/static guards, the shared-context aggregate parser-parity batch, one optional public-interface standalone smoke/diff for the new fixture only, `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED before implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses recursive list tail/"'`
- Focused GREEN after implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses recursive list tail/"'`
- Negative recursive-list diagnostic matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed recursive list tail diagnostics through public run-program/"'`
- Parser-library shortcut/static guard:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
- Static negative-evidence guard:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser reaches success only after complete syntax and dynamic diagnostics/"'`
- Shared-context aggregate parser-parity batch:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/runs all .mlfp parser parity fixtures through one generated public CLI driver/"'`
- Full parser-parity group:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Optional standalone new-fixture public-interface smoke/diff, justified only as package-root evidence for the new fixture:
  ```sh
  actual=$(mktemp)
  timeout 900 cabal -v0 run mlf2 -- run-program test/programs/compiler-parser-parity/recursive-list-tail --search-path test/programs/compiler-parser-parity/parser-library > "$actual"
  diff -u test/conformance/mlfp/parser-parity/recursive-list-tail/expected/parser-program.txt "$actual"
  rm -f "$actual"
  ```
- New-fixture shortcut audit:
  `rg -n 'parseRecursiveListTail|completeModuleKey "recursive-list-tail"|moduleKey "recursive-list-tail"|programKey "recursive-list-tail"|RecursiveListTailTokens|LexerOk recursiveListTailTokens|recursive-list-tail tokens|stringIndexOf sourceText "module RecursiveList export"|stringIndexOf "module RecursiveList export" sourceText|defRows sourceFile "tailOrNil"|defRows sourceFile "isNil"|defRows sourceFile "main"|dataRows sourceFile "List"|constructorRows sourceFile "Cons"|def tailOrNil type=List -> List expr=λ\\(xs : List\\) case xs of|def isNil type=List -> Bool expr=λ\\(xs : List\\) case xs of|def main type=Bool expr=isNil \\(tailOrNil \\(Cons Zero Nil\\)\\)|recursive-list-tail parser negative expected-case-branch-arrow@' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Existing fixture/exact-source audits should remain green, and the new fixture should be included in their banned-shape lists.
- Diff and full closeout gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-329/selection-record.json`
- `orchestrator/rounds/round-329/round-plan-record.json`
