### Selected Extraction
- Milestone: Full Canonical `.mlfp` Parser Parity
- Milestone id: `milestone-4`
- Direction id: `direction-4a-canonical-parser-parity`
- Extracted item id: `item-328-parser-library-recursive-adt-plain-nat-extension`
- Roadmap id: `2026-05-18-00-full-self-boot-end-to-end-roadmap`
- Roadmap revision: `rev-005`
- Roadmap dir: `orchestrator/roadmaps/2026-05-18-00-full-self-boot-end-to-end-roadmap/rev-005`

### Goal
Extend the existing shared parser-owned source-text `.mlfp` parser library so it parses one bounded frozen recursive-ADT source surface through a thin parser-parity fixture:

- `module NatPlain export (Nat(..), isZero, peel, main) { ... }`;
- `data Nat = Zero : Nat | Succ : Nat -> Nat;`;
- `def isZero : Nat -> Bool = λ(n : Nat) case n of { Zero -> true; Succ _ -> false };`;
- `def peel : Nat -> Nat = λ(n : Nat) case n of { Zero -> Zero; Succ inner -> inner };`;
- `def main : Bool = isZero (peel (Succ Zero));`.

The public GREEN behavior is that the shared `.mlfp` parser-library projection for `recursive-adt-plain-nat` exactly matches the Haskell canonical parser projection committed under `test/conformance/mlfp/parser-parity/recursive-adt-plain-nat/expected/parser-program.txt`.

This round is parser parity only. Do not include checker, resolver, backend, platform, driver, proof, package-manager, full parser parity, or self-boot scope.

### Approach
Use the `tdd` skill at `/Users/ares/.agents/skills/tdd/SKILL.md`. Start with one public-interface RED matcher:

`MLF.Program parser parity / shared parser-owned .mlfp parser parses recursive ADT plain Nat`

Add a new conformance fixture and thin public parser-parity harness named `recursive-adt-plain-nat`. The source should be copied from `test/programs/recursive-adt/plain-recursive-nat.mlfp` unless the implementer finds a smaller equivalent source that still covers the selected syntax. The fixture root under `test/programs/compiler-parser-parity/recursive-adt-plain-nat/` should contain only `Main.mlfp` plus `ParserParityFixture.mlfp`; it must provide `sourceFile` and `sourceText` and call `renderParserParityProjectionFromSourceText` from `test/programs/compiler-parser-parity/parser-library/`.

Grow the shared parser-owned lexer/parser-combinator path for this syntax. The expected parser work is to make a declaration-led module body compose a `Nat` data declaration with three generic source definitions, then render token-derived rows for `isZero`, `peel`, and `main`. Reuse the existing parser-owned export-list, `Nat` data declaration, source-definition, annotated-lambda, case-expression, constructor-pattern, wildcard-pattern, identifier-pattern, parenthesized application, and nested constructor/application helpers. Do not add a fixture-owned parser package, do not key success on `recursive-adt-plain-nat`, do not recognize the whole fixture source text, and do not return a prebuilt token stream or pre-rendered projection rows for this fixture.

Because this is parser-parity public evidence that runs through a checker-facing `.mlfp` program harness, preserve the rev-005 shared-context run constraint. The broad parser-parity regression must use the existing generated aggregate public CLI driver with labelled per-case sections. Do not add or require a repeated loop of separate `run-program` invocations for every parser-parity fixture. A single standalone smoke/diff for only the new fixture is allowed as public-interface package-root evidence; if used, it must be labelled as that reason rather than as the broad regression strategy.

### Steps
1. Load `/Users/ares/.agents/skills/tdd/SKILL.md`. Add the focused RED Hspec matcher named above in `test/ProgramParserParitySpec.hs`. The matcher should compare the Haskell canonical parser projection and the shared `.mlfp` parser-library projection for `recursive-adt-plain-nat`.
2. Add committed fixture files under `test/conformance/mlfp/parser-parity/recursive-adt-plain-nat/`: `src/Main.mlfp` and `expected/parser-program.txt`. The expected projection should expose the `NatPlain` module span, `Nat(..)`/`isZero`/`peel`/`main` export spans, `data Nat` and `Zero`/`Succ` constructor spans, and the three definition spans with canonical expression text for the two annotated-lambda case expressions and the nested `isZero (peel (Succ Zero))` application.
3. Add the thin public harness under `test/programs/compiler-parser-parity/recursive-adt-plain-nat/`. Keep it source/evidence only: `ParserParityFixture.mlfp` owns `sourceFile` and `sourceText`, and `Main.mlfp` calls the shared parser-library entrypoint.
4. Register the positive fixture in `test/ProgramParserParitySpec.hs` and in the generated parser-parity batch. After registration, the batch should include the carried 28 positive parser-parity fixtures plus `positive:recursive-adt-plain-nat`.
5. Extend `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp` so declaration-led `Nat` data modules can parse a bounded data-plus-three-source-definitions body through parser-owned combinators. Prefer a shared helper such as `parseNatDataThreeDefinitionRows` that composes `parseNatRecursiveDataRows`, `parseSourceDefinitionRows`, `appendProjectionValues`, and `finishModuleBodyRows`; keep existing one-definition, two-definition, three-definition, class/data, and multi-module paths green.
6. Ensure `parseSourceExpression` and its helpers parse the selected definition expressions from consumed tokens: annotated lambdas whose bodies are `case` expressions, case branches with `Zero`, `Succ _`, and `Succ inner` patterns, branch bodies that are literals, constructors, identifiers, or applications, and the nested parenthesized application `isZero (peel (Succ Zero))`. Extend only shared expression/case/application helpers if a gap appears.
7. Add one public malformed recursive-ADT plain Nat negative path through the generated parser-parity batch, such as omitting the branch arrow in the `peel` branch `Succ inner inner`. Render it through `renderParserNegativeEvidenceFromSourceText` with the stable parser-owned `expected-case-branch-arrow@...` diagnostic if that is the honest failure category; if the implementer finds a more precise shared parser-owned category, use that category and keep the committed evidence public and labelled.
8. Extend static guards in `test/ProgramParserParitySpec.hs` to reject round-328 shortcut shapes, including fixture-specific token stream names, `moduleKey`/`completeModuleKey`/`programKey` success keys for `recursive-adt-plain-nat`, whole-source recognition, `parseRecursiveAdtPlainNat...` or `parsePlainRecursiveNat...` entrypoints, pre-rendered `isZero`/`peel`/`main` rows for this fixture, and static negative evidence strings for the new negative path.
9. Update bounded progress docs that already enumerate parser-parity fixtures when needed, such as `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md`. Keep wording scoped to bounded parser parity and do not claim checker/resolver/backend/platform/driver/proof, full parser parity, milestone-4 completion, or self-boot completion.
10. Run the focused RED/GREEN matcher, the new negative matcher, shortcut/static guards, the shared-context aggregate parser-parity batch, one optional public-interface standalone smoke/diff for the new fixture only, `git diff --check`, `cabal build all`, `cabal test`, and `./scripts/thesis-conformance-gate.sh`.

### Verification
- Focused RED before implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses recursive ADT plain Nat/"'`
- Focused GREEN after implementation:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser parses recursive ADT plain Nat/"'`
- Negative recursive-ADT plain Nat diagnostic matcher:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/parser-owned .mlfp parser reports malformed recursive ADT plain Nat diagnostics through public run-program/"'`
- Parser-library shortcut/static guard:
  `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser keeps expanded grammar paths instead of shortcut entrypoints/"'`
- Shared-context aggregate parser-parity batch:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/runs all .mlfp parser parity fixtures through one generated public CLI driver/"'`
- Full parser-parity group:
  `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
- Optional standalone new-fixture public-interface smoke/diff, justified only as package-root evidence for the new fixture:
  ```sh
  actual=$(mktemp)
  timeout 900 cabal run mlf2 -- run-program test/programs/compiler-parser-parity/recursive-adt-plain-nat --search-path test/programs/compiler-parser-parity/parser-library > "$actual"
  diff -u test/conformance/mlfp/parser-parity/recursive-adt-plain-nat/expected/parser-program.txt "$actual"
  rm -f "$actual"
  ```
- New-fixture shortcut audit:
  `rg -n 'parseRecursiveAdtPlainNat|parsePlainRecursiveNat|completeModuleKey "recursive-adt-plain-nat"|moduleKey "recursive-adt-plain-nat"|programKey "recursive-adt-plain-nat"|RecursiveAdtPlainNatTokens|PlainRecursiveNatTokens|LexerOk recursiveAdtPlainNatTokens|LexerOk plainRecursiveNatTokens|recursive-adt-plain-nat tokens|plain-recursive-nat tokens|stringIndexOf sourceText "module NatPlain export"|stringIndexOf "module NatPlain export" sourceText|defRows sourceFile "isZero"|defRows sourceFile "peel"|defRows sourceFile "main"|def isZero type=Nat -> Bool expr=λ(n : Nat) case n of|def peel type=Nat -> Nat expr=λ(n : Nat) case n of|def main type=Bool expr=isZero \\(peel \\(Succ Zero\\)\\)|recursive-adt-plain-nat parser negative expected-case-branch-arrow@' test/programs/compiler-parser-parity/parser-library test/ProgramParserParitySpec.hs`
  should produce no shortcut matches.
- Existing fixture/exact-source audits should remain green, and the new fixture should be included in their banned-shape lists.
- Diff and full closeout gates:
  `git diff --check`
  `cabal build all`
  `cabal test`
  `./scripts/thesis-conformance-gate.sh`

### Round Plan Record
Also written beside this plan:

- `orchestrator/rounds/round-328/selection-record.json`
- `orchestrator/rounds/round-328/round-plan-record.json`
