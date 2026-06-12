### Changes Made
- `src/MLF/Constraint/Presolution/WitnessNorm.hs`: keeps the widened support that filters rewritten binder arguments down to finalized live node keys before widening no-replay normalization interiors or populating `OmegaNormalizeEnv.binderArgs`.
- `src/MLF/Frontend/Program/Run.hs`: added the optional delayed-recursion runtime fix authorized by the plan after the refreshed focused method-row gate reached `run-program IO runtime encountered recursive top-level binding lookup`. Lambda closures now clear the active top-level lookup stack so recursion through the delayed lambda body is not treated as recursive RHS forcing.
- `test/Presolution/WitnessSpec.hs`: keeps the regression `normalization does not widen no-replay interiors with dead rewritten binder copies`.
- `test/ProgramSpec.hs`: adds the focused regression `allows delayed top-level recursion through lambda closures` for the runtime surface that became necessary.
- `test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: refreshed onto current parent parser drift, preserving recursive module-body and data-constructor row sequencing, then reapplied the round-355 recursive Eq class and Eq Nat instance method-row substrate. The stale one-method continuations and exact-count module-body/data-constructor helpers remain absent.
- `test/ProgramParserParitySpec.hs`: refreshed onto current parent spec drift, preserving recursive module-body/data-constructor dynamic and static guards, then reapplied the two-method class/instance dynamic check and method-row helper/call-site/alias-removal guards.

### Tests
- `git diff --check`: passed.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "normalization does not widen no-replay interiors with dead rewritten binder copies"'`: passed, 2 matching examples, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser guards recursive class and instance method row substrate"'`: passed, 1 example, 0 failures.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "shared parser-owned .mlfp parser recursively sequences class and instance method rows"'`: first failed after the drift refresh with `run-program IO runtime encountered recursive top-level binding lookup: ParserParityParser__parseModuleBodyRowsMoreOrClose -> ParserParityParser__parseModuleBodyRowsMoreOrClose`; after the minimal `Run.hs`/`ProgramSpec.hs` change, passed, 1 example, 0 failures, `Finished in 238.3133 seconds`.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "allows delayed top-level recursion through lambda closures"'`: passed, 1 example, 0 failures.
- Static parser helper/call-site/alias-removal guard:
  ```sh
  ruby -e '
  parser = File.read("test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp")
  spec = File.read("test/ProgramParserParitySpec.hs")
  combined = parser + "\n" + spec
  required_parser = [
    "def parseModuleBodyRowsFirst : String -> String -> String -> String -> ParserValue -> Parser ParserValue",
    "def parseModuleBodyRowsMoreOrClose : String -> String -> String -> String -> ParserValue -> Parser ParserValue",
    "def appendModuleBodyDeclarationRowsAndContinue : (ParserValue -> Parser ParserValue) -> ParserValue -> ParserValue -> Parser ParserValue",
    "def parseEqClassMethodRowsFirst : String -> String -> ParserValue -> Parser ParserValue",
    "def parseEqClassMethodRowsMoreOrClose3 : String -> String -> ParserValue -> Parser ParserValue",
    "def appendClassMethodRowsAndContinue2 : String -> String -> ParserValue -> ParserValue -> Parser ParserValue",
    "def parseClassMethodSignatureRow : String -> ParserValue -> Parser ParserValue",
    "def parseEqNatInstanceMethodRowsFirst : String -> ParserValue -> ParserValue -> ParserValue -> Parser ParserValue",
    "def parseEqNatInstanceMethodRowsMoreOrClose3 : String -> ParserValue -> ParserValue -> ParserValue -> Parser ParserValue",
    "def appendInstanceMethodRowsAndContinue2 : String -> ParserValue -> ParserValue -> ParserValue -> ParserValue -> Parser ParserValue",
    "def parseInstanceMethodDefinitionRow : String -> ParserValue -> Parser ParserValue",
    "parserBind (appendProjectionValues existingRows nextRows)",
    "parserBind (parseClassMethodSignatureRow sourceFile ValueUnit)",
    "parserBind (parseInstanceMethodDefinitionRow sourceFile ValueUnit)",
    "parseTwoConstructorDerivedDataRows sourceFile ValueUnit",
    "parseRecursiveConstructorDataRows sourceFile ValueUnit"
  ]
  required_combined = [
    "shared parser-owned .mlfp parser recursively sequences class and instance method rows",
    "shared parser-owned .mlfp parser guards recursive class and instance method row substrate",
    "shared parser-owned .mlfp parser recursively sequences module-body declarations",
    "sharedParserRecursiveMethodRowSubstratePhrases",
    "sharedParserRecursiveMethodRowUsePhrases",
    "sharedParserRemovedMethodRowContinuationAliases",
    "sharedParserRecursiveModuleBodySequenceSubstratePhrases",
    "sharedParserRetiredExactModuleBodySequencePhrases",
    "recursiveMethodRowsSourceText",
    "writeRecursiveMethodRowsParserPackage"
  ]
  retired_parser = [
    "def parseEqMethodName :",
    "def parseEqMethodColon :",
    "def parseEqMethodFirstArg :",
    "def parseEqMethodFirstArrow :",
    "def parseEqMethodSecondArg :",
    "def parseEqMethodSecondArrow :",
    "def parseEqMethodResult :",
    "def parseEqMethodSemicolon :",
    "def parseEqClassClose :",
    "def parseEqNatInstanceMethodName :",
    "def parseEqNatInstanceMethodEquals :",
    "def parseEqNatInstanceMethodExpression :",
    "def parseEqNatInstanceMethodSemicolon :",
    "def captureEqNatInstanceMethodEnd :",
    "def parseEqNatInstanceClose :",
    "def parseDataLedSourceDefinitionSuffixRows :",
    "def parseBoundedSourceDefinitionRows :",
    "def parseBoundedSourceDefinitionRowsRemaining",
    "def parseSixDataFourDefinition",
    "def finishSixDataFourDefinitionRows :",
    "def parseExactFourConstructorDataRows",
    "def parseExactFiveConstructorDataRows",
    "def parseExactNineConstructorDataRows"
  ]
  missing = required_parser.reject { |s| parser.include?(s) } + required_combined.reject { |s| combined.include?(s) }
  present_retired = retired_parser.select { |s| parser.include?(s) }
  abort("missing required recursive substrate phrases: #{missing.inspect}") unless missing.empty?
  abort("retired helper aliases present in parser: #{present_retired.inspect}") unless present_retired.empty?
  puts "static parser helper/call-site/alias guard passed: #{required_parser.length + required_combined.length} required phrases, #{retired_parser.length} retired phrases checked"
  '
  ```
  Result: passed, `static parser helper/call-site/alias guard passed: 26 required phrases, 23 retired phrases checked`.
- Changed-line shortcut/overclaim guard:
  ```sh
  ruby -e 'diff=`git diff --unified=0 -- src/MLF/Constraint/Presolution/WitnessNorm.hs test/Presolution/WitnessSpec.hs test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp test/ProgramParserParitySpec.hs src/MLF/Frontend/Program/Run.hs test/ProgramSpec.hs`; added=diff.each_line.select{|l| l.start_with?("+") && !l.start_with?("+++")}.map{|l| l[1..]}; patterns={"fixture-name shortcut"=>/fixture[-_ ]?name shortcut|fixture shortcut|fixtureName|fixture_name/i,"pre-rendered projection"=>/pre[-_ ]?rendered|golden projection|projection cache|expected projection bypass/i,"canonical-parser bypass"=>/bypass.*canonical|canonical.*bypass|parseCompleteProgramFixture|shortcut entrypoint/i,"static-negative-only evidence"=>/static negative evidence|static negative|negative evidence only/i,"retired syntax alias"=>/legacy syntax|retired syntax|compatibility alias|alias for removed/i,"compiler-package hook"=>/compiler[-_ ]?package|compiler package/i,"platform-proof hook"=>/platform|proof|stage[-_ ]?proof/i,"native-backend claim"=>/native|backend/i,"package-manager-linker claim"=>/package[-_ ]?manager|linker/i,"self-boot claim"=>/self[-_ ]?boot/i,"full parser parity claim"=>/full parser parity/i}; offenders=[]; added.each_with_index{|line,i| patterns.each{|name,re| offenders << [i+1,name,line.strip] if line.match?(re)}}; abort("changed-line guard failed: #{offenders.inspect}") unless offenders.empty?; puts "changed-line guard passed: #{added.length} added source/spec lines checked"'
  ```
  Result: passed, `changed-line guard passed: 809 added source/spec lines checked`.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "MLF.Program parser parity"'`: passed, 84 examples, 0 failures, `Finished in 7362.3916 seconds`.
- `ghcup run --ghc 9.14.1 -- cabal build all && ghcup run --ghc 9.14.1 -- cabal test`: passed. `cabal build all` completed for `mlf2`, `mlf2-test`, and `frozen-parity-gen`; `cabal test` passed, 2734 examples, 0 failures, `Finished in 7353.2402 seconds`.
- `./scripts/thesis-conformance-gate.sh`: passed, ending with `[thesis-gate] PASS: thesis conformance anchors are green`.

### Notes
The base/drift refresh used the parent checkout only as evidence and recreated the needed state in the assigned worktree. Parent-driven recursive module-body and constructor-row guards were preserved, and stale helpers such as `parseDataLedSourceDefinitionSuffixRows`, `parseBoundedSourceDefinitionRows*`, and exact-count constructor-data helpers were not revived.

The optional runtime surface became necessary only after the parser/checker work advanced the focused method-row package to a runtime delayed-recursion failure. No broader runtime/checker changes were made.

Generated `runtime/mlfp_io/target` build noise produced by validation was removed/restored after the gates. No parent checkout files, `orchestrator/state.json`, merge, review, active roadmap, `CHANGELOG.md`, or root `implementation_notes.md` were touched.

### Finalization Drift-Preservation Refresh
- `test/ProgramSpec.hs`: reintroduced the parent regression `rejects constructor pattern fields passed where the wrapper type is expected` beside the existing pattern-constructor mismatch coverage, while preserving the round-355 `allows delayed top-level recursion through lambda closures` regression.
- `src/MLF/Frontend/Program/Elaborate.hs`: reintroduced the parent checker support needed by that regression. The first focused run of the reintroduced test failed with `ProgramPipelineError "Phase 4 (presolution): ExecError (OccursCheckPresolution ...)"`; after bringing over the parent source-type compatibility predicate split, the focused regression passed with the intended `ProgramTypeMismatch`.

### Finalization Drift-Preservation Tests
- Parent-drift plus round-355 guard:
  ```sh
  ruby -e '<checked WitnessNorm, Elaborate, Run, WitnessSpec, ProgramSpec, ParserParityParser.mlfp, and ProgramParserParitySpec.hs for parent drift plus round-355 required phrases; checked retired parser helpers absent>'
  ```
  Result: passed, `parent-drift plus round-355 guard passed: 17 required phrases checked; 5 retired phrases absent`.
- `diff -u src/MLF/Frontend/Program/Elaborate.hs /Volumes/src/mlf4/src/MLF/Frontend/Program/Elaborate.hs | sed -n '1,120p'`: passed with empty output after the parent checker hunk was restored locally.
- `rg -n "allows delayed top-level recursion through lambda closures|rejects constructor pattern fields passed where the wrapper type is expected" test/ProgramSpec.hs`: passed, both regressions present.
- `rg -n "parseModuleBodyRowsFirst|parseRecursiveConstructorDataRows|parseEqClassMethodRowsFirst|parseEqNatInstanceMethodRowsFirst|parseDataLedSourceDefinitionSuffixRows|parseBoundedSourceDefinitionRows|parseSixDataFourDefinition" test/programs/compiler-parser-parity/parser-library/ParserParityParser.mlfp`: passed; recursive module-body/data-constructor and method-row helpers are present, retired exact-count helper names absent.
- `rg -n "shared parser-owned .mlfp parser recursively sequences module-body declarations|shared parser-owned .mlfp parser recursively sequences class and instance method rows|sharedParserConstructorRowAccumulatorSubstratePhrases|sharedParserRecursiveMethodRowSubstratePhrases" test/ProgramParserParitySpec.hs`: passed.
- `ghcup run --ghc 9.14.1 -- cabal test mlf2-test --test-options='--match "rejects constructor pattern fields passed where the wrapper type is expected"'`: first failed before the `Elaborate.hs` parent hunk was restored; passed after, 1 example, 0 failures.
- `git diff --check`: passed.

No full standard gates were rerun for this finalization-only preservation refresh. The edit was limited to reintroducing parent drift plus the smallest parent checker support required by the focused preserved regression.
