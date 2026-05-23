### Changes Made
- `test/ProgramParserParitySpec.hs`: Added the public round-315 matcher for closed type-family and type-level source-text grammar, canonical projection rendering for type-family declarations/equations/type-level lambdas/applications, malformed type-family equation evidence, and shortcut guard assertions for rejected fixture-key/parser-branch shapes.
- `test/conformance/mlfp/parser-parity/type-family-kind-lambda/`: Added the canonical source and expected parser projection for a closed `Normalize` family with kind-variable result kind, kinded parameter, constructor/variable equation patterns, and type-level lambda/application RHS syntax.
- `test/conformance/mlfp/parser-parity/type-family-apply-annotation/`: Added the canonical source and expected parser projection for optional module exports, a closed `Apply` family, and a type-family-style source type annotation.
- `test/programs/compiler-parser-parity/type-family-kind-lambda/` and `test/programs/compiler-parser-parity/type-family-apply-annotation/`: Added thin fixture roots exposing only `sourceFile`/`sourceText` and calling the shared parser-library `renderParserParityProjectionFromSourceText` entrypoint.
- `test/programs/compiler-parser-parity/parser-library/ParserParityLexer.mlfp`: Extended the shared source-text lexer for the selected type-family/type-level token set, including `type`, `family`, `where`, `::`, source kind/type identifiers, and the type-lambda token.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: Extended the shared parser-combinator grammar for optional module export lists, closed type-family declarations, kinded/plain family parameters, source kind variables, family equation lists, constructor/variable type-level patterns, type-level lambda/application RHS syntax, type-family-style annotations, and the selected malformed equation diagnostic. The implementation uses reusable grammar helpers and internal projection keys rather than fixture-named success branches.
- `test/programs/compiler-parser-parity/parser-library/ParserParityAst.mlfp`, `ParserParitySource.mlfp`, `ParserParityDiagnostic.mlfp`, and `ParserParityParserCombinator.mlfp`: Added projection rendering, stable source-span labels, the `ExpectedTypeFamilyEquationEquals` diagnostic, and parser-combinator labeling for the new syntax slice.
- `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md`: Documented bounded parser-parity progress without claiming full parser parity, type-family checker/reducer completion, backend/platform/driver/proof work, or self-boot completion.

### Tests
- `test/ProgramParserParitySpec.hs`: Verifies the public source-text parser-library behavior for both new positive fixtures, the malformed type-family equation negative path, and guards against fixture-key shortcuts such as `parseTypeFamilyKindLambdaModule` and `completeModuleKey "type-family-kind-lambda"`.
- Focused RED evidence:
  - `timeout 240 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to closed type-family and type-level syntax/"'` timed out during the cold rebuild before behavioral evidence.
  - `timeout 300 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to closed type-family and type-level syntax/"'` then failed as expected with `parser-error` for the new fixture path before implementation.
- Focused GREEN evidence:
  - `timeout 800 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to closed type-family and type-level syntax/"'` passed, 1 example, 0 failures.
- Direct fixture smokes:
  - Ran `timeout 300 cabal run mlf2 -- run-program <fixture> --search-path test/programs/compiler-parser-parity/parser-library` for `basic-module-def-bool`, `import-exposing-def-bool`, `value-def-list-int-ref`, `let-lambda-application`, `typed-annotation-types`, `data-declaration-constructor-spans`, `case-expression-constructor-patterns`, `case-expression-nested-patterns`, `typeclass-deriving-method`, `typeclass-instance-nullary-method`, `higher-kinded-class-data-params`, `multiparam-superclass-fundep`, `type-family-kind-lambda`, and `type-family-apply-annotation`; all passed.
- Static audits:
  - Thin fixture audit from the plan returned no matches.
  - Exact-source/static-token audit from the plan returned no matches.
  - Round-315 rejected-shape audit returned no matches.
  - Shared parser architecture audit returned no matches.
- Broader validation:
  - `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'` timed out after passing through malformed data declarations; no failure was reported before timeout.
  - `git diff --check` passed.
  - `timeout 2400 cabal build all` passed.
  - `timeout 2400 ./scripts/thesis-conformance-gate.sh` passed.
  - `timeout 7200 cabal test` passed: 2637 examples, 0 failures. This full-suite run completed the full parser-parity section, including the examples not reached by the earlier parser-parity-group timeout.

### Notes
The parser-library extension is intentionally bounded to parser-parity projection evidence. It does not add or claim type-family checker/reducer work beyond existing compiler behavior, backend/native support for the new parser fixtures, driver/platform/proof work, full parser parity, or self-boot completion.

`runtime/mlfp_io/target/release/libmlfp_io.d` was regenerated by validation with the round worktree path and was restored before handoff. `orchestrator/state.json` was already modified in the worktree and was left untouched.
