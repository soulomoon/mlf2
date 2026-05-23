### Checks Run
- Command: `cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/shared parser-owned .mlfp parser extends source-text grammar to closed type-family and type-level syntax/"'`
  Result: pass. The focused round-315 matcher completed in 471.3455 seconds with 1 example and 0 failures.

- Command: `timeout 3600 cabal test mlf2-test --test-options='--match "/MLF.Program parser parity/"'`
  Result: timeout, not a behavioral failure. The group-level run progressed through and passed the carried lexer, case-expression, typeclass/instance, higher-kinded/constrained, and round-315 closed type-family/type-level parser examples, plus several later carried examples, before `timeout` exited 124. Full `cabal test` below completed the entire parser-parity section and is the accepted full-suite evidence for this slow group.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/basic-module-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/basic-module-def-bool/src/Main.mlfp:1:1-4:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/import-exposing-def-bool --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/import-exposing-def-bool/src/Main.mlfp:1:1-5:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/value-def-list-int-ref --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/value-def-list-int-ref/src/Main.mlfp:1:1-6:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/let-lambda-application --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/let-lambda-application/src/Main.mlfp:1:1-5:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typed-annotation-types --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/typed-annotation-types/src/Main.mlfp:1:1-5:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/data-declaration-constructor-spans --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/data-declaration-constructor-spans/src/Main.mlfp:1:1-8:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/case-expression-constructor-patterns --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/case-expression-constructor-patterns/src/Main.mlfp:1:1-11:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/case-expression-nested-patterns --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/case-expression-nested-patterns/src/Main.mlfp:1:1-12:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typeclass-deriving-method --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/typeclass-deriving-method/src/Main.mlfp:1:1-13:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/typeclass-instance-nullary-method --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/typeclass-instance-nullary-method/src/Main.mlfp:1:1-18:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/higher-kinded-class-data-params --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/higher-kinded-class-data-params/src/Main.mlfp:1:1-9:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/multiparam-superclass-fundep --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/multiparam-superclass-fundep/src/Main.mlfp:1:1-9:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/type-family-kind-lambda --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/type-family-kind-lambda/src/Main.mlfp:1:1-8:1`.

- Command: `cabal run mlf2 -- run-program test/programs/compiler-parser-parity/type-family-apply-annotation --search-path test/programs/compiler-parser-parity/parser-library`
  Result: pass; output began with `module Main span=test/conformance/mlfp/parser-parity/type-family-apply-annotation/src/Main.mlfp:1:1-8:1`.

- Command: `find test/programs/compiler-parser-parity -mindepth 2 -maxdepth 2 -name ParserParityFixture.mlfp -print0 | xargs -0 rg -n 'ParserSourceInput|ParserSourceSymbol|SourceSymbol|SourceInputCons|sourceInputCons|basicModuleKeywordSpan|dataDeclSpan|case.*Span|class.*Span|instance.*Span|deriving.*Span|higherKinded.*Span|fundep.*Span|constraint.*Span|typeFamily.*Span|family.*Span|typeLevel.*Span'`
  Result: pass; no matches in fixture roots.

- Command: `rg -n 'stringSlice source [0-9]|stringIndexOf source .*SourceText|renderParserParityEvidence|TokenStream :|BasicModuleTokens|ImportBoolTokens|ValueDefListTokens|LetLambdaApplicationTokens|TypedAnnotationTypesTokens|DataDeclarationTokens|CaseExpressionTokens|TypeclassTokens|InstanceTokens|HigherKindedTokens|ConstraintTokens|FundepTokens|TypeFamilyTokens|FamilyTokens|LexerOk (basicModuleTokens|importBoolTokens|valueDefListTokens|letLambdaApplicationTokens|typedAnnotationTypesTokens|dataDeclarationTokens|caseExpressionTokens|typeclassTokens|instanceTokens|higherKindedTokens|constraintTokens|fundepTokens|typeFamilyTokens|familyTokens)|case tokens|class tokens|instance tokens|higher-kinded tokens|constraint tokens|fundep tokens|type-family tokens|family tokens' test/programs/compiler-parser-parity/parser-library test/programs/compiler-parser-parity test/ProgramParserParitySpec.hs`
  Result: pass; no exact-source or static-token shortcut matches.

- Command: `rg -n 'parseTypeFamilyKindLambdaModule|parseTypeFamilyApplyAnnotationModule|completeModuleKey "type-family-kind-lambda"|completeModuleKey "type-family-apply-annotation"|moduleKey "type-family-kind-lambda"|moduleKey "type-family-apply-annotation"' test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`
  Result: pass; no round-315 rejected-shape matches.

- Command: `find test/programs/compiler-parser-parity -path '*/ParserParityParser.mlfp' -print`
  Result: pass; only `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp` exists.

- Command: `rg -n 'renderParserParityProjection.*ParserSourceInput|parseCompleteModule : ParserSourceInput|tokenizeCompleteModule : ParserSourceInput' test/programs/compiler-parser-parity`
  Result: pass; no public parser entrypoint accepts fixture-authored `ParserSourceInput`.

- Command: `find test/programs/compiler-parser-parity/type-family-kind-lambda test/programs/compiler-parser-parity/type-family-apply-annotation -maxdepth 1 -type f -print`
  Result: pass; each new fixture root contains only `Main.mlfp` and `ParserParityFixture.mlfp`.

- Command: `find test/conformance/mlfp/parser-parity/type-family-kind-lambda test/conformance/mlfp/parser-parity/type-family-apply-annotation -maxdepth 2 -type f -print`
  Result: pass; each new conformance fixture contains only `src/Main.mlfp` and `expected/parser-program.txt`.

- Command: `git diff -- mlf2.cabal test/Main.hs`
  Result: pass; no new Haskell modules or spec modules require cabal/test-main registration.

- Command: `rg -n 'full parser parity|type-family checker|type family checker|reducer|backend|platform|driver|proof|self-boot|self boot|self-hosting|self hosting' CHANGELOG.md implementation_notes.md docs/mlfp-self-boot-readiness.md test/conformance/mlfp/README.md orchestrator/rounds/round-315/implementation-notes.md`
  Result: pass after manual audit; changed docs and notes explicitly bound the round to parser parity and avoid claiming full parser parity, type-family checker/reducer support, backend/platform/driver/proof work, or self-boot completion.

- Command: `git diff --check`
  Result: pass before and after validation.

- Command: `timeout 2400 cabal build all`
  Result: pass.

- Command: `timeout 7200 cabal test`
  Result: pass. The full suite completed in 5289.0502 seconds with 2637 examples and 0 failures. The output shows the `MLF.Program parser parity` section completed, including the round-315 closed type-family/type-level example and the parser architecture guard examples, before later sections ran.

- Command: `timeout 2400 ./scripts/thesis-conformance-gate.sh`
  Result: pass; final line reported `[thesis-gate] PASS: thesis conformance anchors are green`.

### Plan Compliance
- Step 1: met. `test/ProgramParserParitySpec.hs` contains the focused public matcher and shortcut guard assertions. Implementation notes record RED and GREEN evidence; reviewer reran the focused GREEN matcher successfully.
- Step 2: met. New committed conformance source and expected parser projection files exist under `test/conformance/mlfp/parser-parity/type-family-kind-lambda/` and `test/conformance/mlfp/parser-parity/type-family-apply-annotation/`.
- Step 3: met. New fixture roots under `test/programs/compiler-parser-parity/` are thin harnesses with only `Main.mlfp` and `ParserParityFixture.mlfp`, exposing `sourceFile` and `sourceText` and calling `renderParserParityProjectionFromSourceText`.
- Step 4: met. The shared parser-library lexer handles the selected type-family/type-level token set, including `type`, `family`, `where`, `::`, `*`, `->`, selected identifiers, and the type-lambda token. No fixture-owned lexer/token-stream shortcut was found.
- Step 5: met. `ParserParityParser.mlfp` extends the shared parser-combinator grammar through `parserBind`, `parserChoice`, and parser-state sequencing, not fixture-root parsing.
- Step 6: met. Shared parser-library AST/projection/span rendering was extended for the two new fixtures. The Haskell canonical projections and `.mlfp` parser projections match the committed expected projection files.
- Step 7: met. The focused matcher exercises malformed type-family equation evidence through `renderParserNegativeEvidenceFromSourceText` with `expected-type-family-equation-equals`.
- Step 8: met. Static and Hspec guard coverage rejects the named round-315 shortcut shapes. Reviewer static audit found no rejected-shape matches.
- Step 9: met. `CHANGELOG.md`, `implementation_notes.md`, `docs/mlfp-self-boot-readiness.md`, and `test/conformance/mlfp/README.md` document bounded parser-parity progress without overclaiming checker/reducer, backend, platform, driver, proof, full parser parity, or self-boot support.
- Step 10: met with one timeout caveat. Focused matcher passed; all direct smokes passed; static audits passed; `git diff --check`, `cabal build all`, `cabal test`, and thesis gate passed. The standalone parser-parity group timed out at 3600 seconds after passing the round-315 example, but full `cabal test` completed the entire parser-parity section, so the required parser-parity evidence is sufficient.

### Decision
**APPROVED**

### Evidence
The integrated diff stays within `milestone-4` / `direction-4a-canonical-parser-parity` / `item-315-parser-library-type-family-type-level-extension`. The new syntax goes through the shared source-text lexer/parser-combinator front door, fixture roots stay thin, and static audits found no exact-source/static-token shortcuts or rejected round-315 fixture-key branches.

The docs and implementation notes keep the scope layered: parser-parity projection evidence only. They do not claim type-family checker/reducer support beyond existing compiler behavior, backend/native/platform/driver/proof work, full parser parity, or self-boot completion.

Closeout classification: status-only. This round should add a compact milestone-4 completion pointer and keep `milestone-4` in progress; it does not change future coordination, sequencing, extraction scope, retry policy, milestone meaning, or verification meaning.

Validation side effect: reviewer validation regenerated `runtime/mlfp_io/target/release/libmlfp_io.d` with the round worktree path. This is the known tracked depfile path rewrite from local native validation, not part of the reviewed implementation payload. I did not restore it because reviewer scope is limited to reviewer-owned artifacts.
