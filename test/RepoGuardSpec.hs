{-# LANGUAGE DataKinds #-}
module RepoGuardSpec (spec) where

import Control.Monad (forM_)
import Data.Char (isAlphaNum)
import Data.List (intercalate, isInfixOf, isSuffixOf, sort)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (dropExtension, makeRelative, splitDirectories, takeExtension, (</>))
import Test.Hspec

spec :: Spec
spec = describe "Repository guardrails" $ do
  it "all spec modules are wired into Cabal and test/Main.hs" $ do
    specModules <- sort <$> discoverSpecModules "test"
    mainEntryModules <- sort <$> discoverMainEntrySpecModules "test"
    cabalModules <- sort <$> discoverCabalSpecModules
    mainImports <- sort <$> discoverMainImports
    mainCalls <- sort <$> discoverMainCalls

    assertSet "Cabal other-modules" specModules cabalModules
    assertSet "test/Main imports" mainEntryModules mainImports
    assertSet "test/Main registrations" mainEntryModules mainCalls

  it "legacy MyLib surface is removed" $ do
    doesFileExist "src-public/MyLib.hs" >>= (`shouldBe` False)
    cabalSrc <- readFile "mlf2.cabal"
    cabalSrc `shouldSatisfy` (not . isInfixOf "MyLib")
    offenders <- findImportOffenders ["src", "src-public", "test", "app"]
    offenders `shouldBe` []

  it "production code keeps castConstraint quarantined to the defining module" $ do
    offenders <- findCastConstraintOffenders ["src", "src-public", "app"]
    offenders `shouldBe` []

  it "castConstraint guard ignores non-call mentions" $ do
    let guardedSource =
          unlines
            [ "-- castConstraint is mentioned in a comment",
              "{- castConstraint is mentioned in a block comment -}",
              "message = \"castConstraint is mentioned in a string\"",
              "castConstraint :: Constraint p -> Constraint q",
              "import MLF.Constraint.Types.Graph (castConstraint)"
            ]
        multilineSignature =
          unlines
            [ "castConstraint",
              "  :: Constraint p -> Constraint q"
            ]
        multilineImport =
          unlines
            [ "import MLF.Constraint.Types.Graph",
              "  (",
              "    castConstraint",
              "  )"
            ]
        multilineExport =
          unlines
            [ "module MLF.Constraint.Types.Graph",
              "  (",
              "    castConstraint",
              "  ) where"
            ]
        classSignature =
          unlines
            [ "class Legacy c where",
              "  castConstraint",
              "    :: Constraint p -> Constraint q"
            ]
    containsCastConstraintCall guardedSource `shouldBe` False
    containsCastConstraintCall multilineSignature `shouldBe` False
    containsCastConstraintCall multilineImport `shouldBe` False
    containsCastConstraintCall multilineExport `shouldBe` False
    containsCastConstraintCall classSignature `shouldBe` False
    containsCastConstraintCall "legacy = castConstraint c" `shouldBe` True
    containsCastConstraintCall "x' = castConstraint c" `shouldBe` True
    containsCastConstraintCall (unlines ["legacy =", "  castConstraint", "    c"])
      `shouldBe` True
    containsCastConstraintCall (unlines ["legacy =", "  (castConstraint)", "    c"])
      `shouldBe` True
    containsCastConstraintCall (unlines ["legacy =", "  castConstraint", "  :: Constraint p -> Constraint q"])
      `shouldBe` True
    containsCastConstraintCall (unlines ["legacy =", "  id", "    castConstraint", "    :: Constraint p -> Constraint q"])
      `shouldBe` True
    containsCastConstraintCall "legacy = Graph.castConstraint c" `shouldBe` True

  it "MLF.API no longer exports pipeline/runtime helpers and MLF.Pipeline owns them" $ do
    apiSrc <- readFile "src-public/MLF/API.hs"
    pipelineSrc <- readFile "src-public/MLF/Pipeline.hs"
    forM_
      [ "inferConstraintGraph",
        "PipelineConfig(..)",
        "defaultPipelineConfig",
        "TraceConfig(..)",
        "defaultTraceConfig",
        "PipelineError(..)",
        "renderPipelineError",
        "runPipelineElab",
        "runPipelineElabChecked",
        "runPipelineElabWithConfig",
        "runPipelineElabCheckedWithConfig",
        "typeCheck",
        "step",
        "\n    , normalize\n",
        "isValue",
        "checkProgram",
        "runProgram",
        "prettyValue"
      ]
      $ \marker -> do
        apiSrc `shouldSatisfy` (not . isInfixOf marker)
        pipelineSrc `shouldSatisfy` isInfixOf marker

  it "README and architecture docs agree on the current public topology" $ do
    readmeSrc <- readFile "README.md"
    architectureSrc <- readFile "docs/architecture.md"
    forM_
      [ "- `MLF.API` — surface syntax plus eMLF / `.mlfp` parsing, pretty-printing, and normalization helpers",
        "- `MLF.Pipeline` — canonical public constraint-generation / elaboration / runtime API, including `.mlfp` elaboration/checking on the shared eMLF/xMLF path",
        "- `MLF.Program` — thin compatibility re-export for the same unified `.mlfp` surface",
        "- `MLF.XMLF` — xMLF syntax, parser, and pretty-printer"
      ]
      $ \marker ->
        readmeSrc `shouldSatisfy` isInfixOf marker
    forM_
      [ "- `MLF.API` — umbrella frontend module (surface syntax + eMLF / `.mlfp` parse/pretty + normalization helpers)",
        "- `MLF.Pipeline` — canonical pipeline/runtime module (e.g. `inferConstraintGraph`, `runPipelineElab`, `typeCheck`, `step`, `normalize`, `.mlfp` elaboration/checking/runtime)",
        "- `MLF.Program` — compatibility shim re-exporting the same unified `.mlfp` surface",
        "- `MLF.XMLF` — explicit xMLF syntax, parser, and pretty-printing helpers"
      ]
      $ \marker ->
        architectureSrc `shouldSatisfy` isInfixOf marker

  it "`.mlfp` elaboration reuses the existing eMLF/typecheck boundary without private authority facades" $ do
    authorityExists <- doesFileExist "src/MLF/Frontend/Program/Authority.hs"
    typecheckProgramExists <- doesFileExist "src/MLF/Elab/TypeCheck/Program.hs"
    checkSrc <- readFile "src/MLF/Frontend/Program/Check.hs"
    elaborateSrc <- readFile "src/MLF/Frontend/Program/Elaborate.hs"
    finalizeSrc <- readFile "src/MLF/Frontend/Program/Finalize.hs"
    typesSrc <- readFile "src/MLF/Frontend/Program/Types.hs"
    runSrc <- readFile "src/MLF/Frontend/Program/Run.hs"
    pipelineSrc <- readFile "src-public/MLF/Pipeline.hs"
    pipelineRunSrc <- readFile "src/MLF/Elab/Run/Pipeline.hs"
    syntaxSrc <- readFile "src/MLF/Frontend/Syntax.hs"
    readmeSrc <- readFile "README.md"
    architectureSrc <- readFile "docs/architecture.md"
    notesSrc <- readFile "implementation_notes.md"
    authorityExists `shouldBe` False
    typecheckProgramExists `shouldBe` False
    forM_
      [ "isPipelineLowerable",
        "resolveLowerableChecked",
        "inferLowerableExpr",
        "inferExprTypeViaPipeline",
        "expression is not lowerable to the authoritative pipeline",
        "resolveLowerableChecked received a non-lowerable case expression",
        "authoritative `.mlfp` typing needs an explicit parameter type for non-lowerable lambda",
        "data ExprScope",
        "inferMethodApp ::",
        "checkAlt ::",
        "Program.Authority",
        "MLF.Elab.TypeCheck.Program",
        "constructorTerm",
        "import MLF.Elab.TypeCheck"
      ]
      $ \marker -> do
        checkSrc `shouldSatisfy` (not . isInfixOf marker)
        elaborateSrc `shouldSatisfy` (not . isInfixOf marker)
        runSrc `shouldSatisfy` (not . isInfixOf marker)
        pipelineSrc `shouldSatisfy` (not . isInfixOf marker)
    forM_
      [ "MLF.Elab.",
        "ElabTerm",
        "ElabType",
        "runPipelineElab",
        "typeCheck",
        "CheckedBinding",
        "checkedBindingTerm",
        "checkedBindingType",
        ".ETyAbs",
        ".ELam",
        ".EApp",
        ".ERoll",
        ".EUnroll",
        ".ELet",
        ".ETyInst"
      ]
      $ \marker ->
        elaborateSrc `shouldSatisfy` (not . isInfixOf marker)
    forM_
      [ "EUnfold ::",
        "EUnfoldSurfaceF",
        "unfold marker for case scrutinees"
      ]
      $ \marker ->
        syntaxSrc `shouldSatisfy` (not . isInfixOf marker)
    checkSrc `shouldSatisfy` isInfixOf "import MLF.Frontend.Program.Elaborate"
    checkSrc `shouldSatisfy` isInfixOf "import MLF.Frontend.Program.Finalize"
    typesSrc `shouldSatisfy` isInfixOf "loweredBindingSurfaceExpr :: SurfaceExpr"
    typesSrc `shouldSatisfy` (not . isInfixOf "loweredBindingTerm :: ElabTerm")
    finalizeSrc `shouldSatisfy` isInfixOf "runPipelineElabDetailedWithExternalBindings"
    finalizeSrc `shouldSatisfy` isInfixOf "runPipelineElabDetailedUncheckedWithExternalBindings"
    finalizeSrc `shouldSatisfy` isInfixOf "normalizeExpr surfaceExpr"
    finalizeSrc `shouldSatisfy` isInfixOf "typeCheckWithEnv caseRewriteEnv rewritten"
    pipelineRunSrc `shouldSatisfy` isInfixOf "Compatibility alias"
    pipelineRunSrc `shouldSatisfy` isInfixOf "runPipelineElabCheckedWithConfig = runPipelineElabWithConfig"
    forM_
      [ "where possible and only emits direct",
        "direct `ElabTerm`s for constructs",
        "direct `ElabTerm`s only when"
      ]
      $ \marker -> do
        readmeSrc `shouldSatisfy` (not . isInfixOf marker)
        architectureSrc `shouldSatisfy` (not . isInfixOf marker)
        notesSrc `shouldSatisfy` (not . isInfixOf marker)
    runSrc `shouldSatisfy` isInfixOf "normalize"

  it "`.mlfp` checking consumes resolved syntax without unresolving whole programs" $ do
    checkSrc <- readFile "src/MLF/Frontend/Program/Check.hs"
    forM_
      [ "P.unresolveModule",
        "P.unresolveProgram",
        "resolvedExprForEnv",
        "qualifyInstanceHeadOnly",
        "qualifyInstance ::",
        "qualifyInstance alias"
      ]
      $ \marker ->
        checkSrc `shouldSatisfy` (not . isInfixOf marker)
    finalizeSrc <- readFile "src/MLF/Frontend/Program/Finalize.hs"
    elaborateSrc <- readFile "src/MLF/Frontend/Program/Elaborate.hs"
    forM_
      [ "resolveInstanceInfoWithSubst",
        "resolveMethodInstanceInfoWithSubst"
      ]
      $ \marker ->
        finalizeSrc `shouldSatisfy` (not . isInfixOf marker)
    forM_
      [ "(P.constraintClassName constraint, show (P.constraintType constraint))",
        "Set (P.ClassName, String) -> [P.ClassConstraint]",
        "resolvedExprForScope",
        "resolvedPatternForScope",
        "resolvedTypeForScope",
        "resolvedConstrainedTypeForScope",
        "resolvedClassConstraintForScope"
      ]
      $ \marker ->
        elaborateSrc `shouldSatisfy` (not . isInfixOf marker)

  it "split facades stay thin and child-owned" $ do
    forM_ splitFacadeGuards $ \(path, maxLines, requiredMarkers) -> do
      src <- readFile path
      length (lines src) `shouldSatisfy` (<= maxLines)
      forM_ requiredMarkers $ \marker ->
        src `shouldSatisfy` isInfixOf marker

  it "split child modules stay implementation-only in Cabal" $ do
    cabalSrc <- readFile "mlf2.cabal"
    let publicLibrarySrc = extractPublicLibraryStanza cabalSrc
    forM_ splitChildModules $ \moduleName -> do
      countModuleEntries moduleName cabalSrc `shouldBe` 1
      publicLibrarySrc `shouldSatisfy` (not . isInfixOf moduleName)

  it "one-backend-IR contract stays explicit and no public lower IR leaks" $ do
    architectureSrc <- readFile "docs/architecture.md"
    backendIRSrc <- readFile "src/MLF/Backend/IR.hs"
    backendConvertSrc <- readFile "src/MLF/Backend/Convert.hs"
    backendLowerSrc <- readFile "src/MLF/Backend/LLVM/Lower.hs"
    nativePipelineSrc <- readFile "docs/backend-native-pipeline.md"
    cabalSrc <- readFile "mlf2.cabal"
    let publicLibrarySrc = extractPublicLibraryStanza cabalSrc
    publicLibrarySrc `shouldSatisfy` (not . isInfixOf "MLF.Backend.")
    publicLibrarySrc `shouldSatisfy` (not . isInfixOf "LowerableBackend.")
    forM_
      [ ("docs/architecture.md", architectureSrc, architectureContractMarkers ++ futureLowerIRCriteriaMarkers),
        ("src/MLF/Backend/IR.hs", backendIRSrc, backendIRContractMarkers ++ futureLowerIRCriteriaMarkers),
        ("src/MLF/Backend/Convert.hs", backendConvertSrc, backendConvertContractMarkers ++ futureLowerIRCriteriaMarkers),
        ("src/MLF/Backend/LLVM/Lower.hs", backendLowerSrc, backendLowerContractMarkers ++ futureLowerIRCriteriaMarkers),
        ("docs/backend-native-pipeline.md", nativePipelineSrc, nativePipelineContractMarkers ++ futureLowerIRCriteriaMarkers)
      ]
      $ \(path, src, markers) ->
        assertMarkersPresent path src markers

  it "eager-runtime lowering contract stays explicit and lazy STG machinery stays out of scope" $ do
    architectureSrc <- readFile "docs/architecture.md"
    backendIRSrc <- readFile "src/MLF/Backend/IR.hs"
    backendConvertSrc <- readFile "src/MLF/Backend/Convert.hs"
    backendLowerSrc <- readFile "src/MLF/Backend/LLVM/Lower.hs"
    nativePipelineSrc <- readFile "docs/backend-native-pipeline.md"
    forM_
      [ ("docs/architecture.md", architectureSrc, architectureEagerRuntimeMarkers ++ eagerRuntimeExclusionMarkers),
        ("src/MLF/Backend/IR.hs", backendIRSrc, backendIREagerRuntimeMarkers ++ eagerRuntimeExclusionMarkers),
        ("src/MLF/Backend/Convert.hs", backendConvertSrc, backendConvertEagerRuntimeMarkers ++ eagerRuntimeExclusionMarkers),
        ("src/MLF/Backend/LLVM/Lower.hs", backendLowerSrc, backendLowerEagerRuntimeMarkers ++ eagerRuntimeExclusionMarkers),
        ("docs/backend-native-pipeline.md", nativePipelineSrc, nativePipelineEagerRuntimeMarkers ++ eagerRuntimeExclusionMarkers)
      ]
      $ \(path, src, markers) ->
        assertMarkersPresent path src markers

  it "callable-shape contract stays explicit and direct-vs-closure call heads stay unambiguous" $ do
    architectureSrc <- readFile "docs/architecture.md"
    backendIRSrc <- readFile "src/MLF/Backend/IR.hs"
    backendConvertSrc <- readFile "src/MLF/Backend/Convert.hs"
    backendLowerSrc <- readFile "src/MLF/Backend/LLVM/Lower.hs"
    forM_
      [ ("docs/architecture.md", architectureSrc, architectureCallableShapeMarkers),
        ("src/MLF/Backend/IR.hs", backendIRSrc, backendIRCallableShapeMarkers),
        ("src/MLF/Backend/Convert.hs", backendConvertSrc, backendConvertCallableShapeMarkers),
        ("src/MLF/Backend/LLVM/Lower.hs", backendLowerSrc, backendLowerCallableShapeMarkers)
      ]
      $ \(path, src, markers) ->
        assertMarkersPresent path src markers

  it "ADT and case semantic boundary stays explicit while lowerer-owned layout policy stays private and frozen" $ do
    architectureSrc <- readFile "docs/architecture.md"
    nativePipelineSrc <- readFile "docs/backend-native-pipeline.md"
    backendIRSrc <- readFile "src/MLF/Backend/IR.hs"
    backendConvertSrc <- readFile "src/MLF/Backend/Convert.hs"
    backendLowerSrc <- readFile "src/MLF/Backend/LLVM/Lower.hs"
    forM_
      [ ("docs/architecture.md", architectureSrc, backendADTCaseOwnershipMarkers),
        ("docs/backend-native-pipeline.md", nativePipelineSrc, backendADTCaseOwnershipMarkers ++ nativePipelineADTCaseLayoutMarkers),
        ("src/MLF/Backend/IR.hs", backendIRSrc, backendADTCaseOwnershipMarkers),
        ("src/MLF/Backend/Convert.hs", backendConvertSrc, backendADTCaseOwnershipMarkers),
        ("src/MLF/Backend/LLVM/Lower.hs", backendLowerSrc, backendADTCaseOwnershipMarkers ++ backendLowerADTCaseLayoutMarkers)
      ]
      $ \(path, src, markers) ->
        assertMarkersPresent path src markers

  it "primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary" $ do
    architectureSrc <- readFile "docs/architecture.md"
    nativePipelineSrc <- readFile "docs/backend-native-pipeline.md"
    backendIRSrc <- readFile "src/MLF/Backend/IR.hs"
    backendConvertSrc <- readFile "src/MLF/Backend/Convert.hs"
    backendLowerSrc <- readFile "src/MLF/Backend/LLVM/Lower.hs"
    forM_
      [ ("docs/architecture.md", architectureSrc, primitiveOperationEagerOrderMarkers),
        ("docs/backend-native-pipeline.md", nativePipelineSrc, primitiveOperationEagerOrderMarkers),
        ("src/MLF/Backend/IR.hs", backendIRSrc, primitiveOperationEagerOrderMarkers),
        ("src/MLF/Backend/Convert.hs", backendConvertSrc, primitiveOperationEagerOrderMarkers),
        ("src/MLF/Backend/LLVM/Lower.hs", backendLowerSrc, primitiveOperationEagerOrderMarkers)
      ]
      $ \(path, src, markers) ->
        assertMarkersPresent path src markers

  it "polymorphism-erasure and lowerability contract stays explicit without widening the backend boundary" $ do
    architectureSrc <- readFile "docs/architecture.md"
    nativePipelineSrc <- readFile "docs/backend-native-pipeline.md"
    backendIRSrc <- readFile "src/MLF/Backend/IR.hs"
    backendConvertSrc <- readFile "src/MLF/Backend/Convert.hs"
    backendLowerSrc <- readFile "src/MLF/Backend/LLVM/Lower.hs"
    forM_
      [ ("docs/architecture.md", architectureSrc, polymorphismLowerabilityMarkers),
        ("docs/backend-native-pipeline.md", nativePipelineSrc, polymorphismLowerabilityMarkers),
        ("src/MLF/Backend/IR.hs", backendIRSrc, polymorphismLowerabilityMarkers),
        ("src/MLF/Backend/Convert.hs", backendConvertSrc, polymorphismLowerabilityMarkers),
        ("src/MLF/Backend/LLVM/Lower.hs", backendLowerSrc, polymorphismLowerabilityMarkers)
      ]
      $ \(path, src, markers) ->
        assertMarkersPresent path src markers

  it "backend-boundary mechanism table and closeout ledger stay synchronized" $ do
    repoGuardSrc <- readFile "test/RepoGuardSpec.hs"
    tableSrc <- readFile "docs/plans/2026-05-02-backend-ir-executable-boundary-mechanism-table.md"
    todoSrc <- readFile "TODO.md"
    notesSrc <- readFile "implementation_notes.md"
    changelogSrc <- readFile "CHANGELOG.md"

    rows <-
      case parseBackendBoundaryMechanismTable tableSrc of
        Left err -> expectationFailure err >> pure []
        Right parsedRows -> pure parsedRows

    map mechanismTableRowName rows `shouldBe` backendBoundaryMechanismNames
    map mechanismTableRowGate rows `shouldSatisfy` all (`elem` ["YES", "NO"])
    map mechanismTableRowGate rows `shouldBe` replicate 7 "YES"

    case rows of
      [_, _, _, _, _, _, row7] -> do
        mechanismTableRowGapSummary row7
          `shouldSatisfy` (not . isInfixOf "Some goals are currently design intent rather than verified mechanism.")
        mechanismTableRowEvidence row7
          `shouldSatisfy` (not . isInfixOf "backend tests referenced by cabal/test stanzas")
        mechanismTableRowNextAction row7
          `shouldSatisfy` (not . isInfixOf "Add or update focused tests and docs with each mechanism improvement")
        assertMarkersPresent "row-7 evidence" (mechanismTableRowEvidence row7) backendBoundaryRow7EvidenceMarkers
        mechanismTableRowNextAction row7
          `shouldSatisfy` isInfixOf "later accepted roadmap revision"
      _ ->
        expectationFailure
          ( "expected 7 backend-boundary mechanism-table rows, found "
              ++ show (length rows)
          )

    assertMarkersPresent "test/RepoGuardSpec.hs" repoGuardSrc backendBoundaryFocusedGuardNames
    assertMarkersPresent "TODO.md" todoSrc backendBoundaryCloseoutTodoMarkers
    assertMarkersPresent "implementation_notes.md" notesSrc backendBoundaryCloseoutImplementationNoteMarkers
    assertMarkersPresent "CHANGELOG.md" changelogSrc backendBoundaryCloseoutChangelogMarkers

discoverSpecModules :: FilePath -> IO [String]
discoverSpecModules root = do
  hsFiles <- collectHsFiles root
  pure
    [ pathToModule root path
    | path <- hsFiles,
      "Spec.hs" `isSuffixOf` path
    ]

discoverMainEntrySpecModules :: FilePath -> IO [String]
discoverMainEntrySpecModules root = do
  hsFiles <- collectHsFiles root
  pure
    [ pathToModule root path
    | path <- hsFiles,
      "Spec.hs" `isSuffixOf` path,
      isMainEntryPath root path
    ]

discoverCabalSpecModules :: IO [String]
discoverCabalSpecModules = do
  src <- lines <$> readFile "mlf2.cabal"
  pure
    [ moduleName
    | line <- src,
      let moduleName = normalizeModuleToken (dropFieldPrefix (trim line)),
      "Spec" `isSuffixOf` moduleName
    ]

discoverMainImports :: IO [String]
discoverMainImports = do
  src <- lines <$> readFile "test/Main.hs"
  pure
    [ moduleName
    | line <- src,
      Just moduleName <- [parseMainImport (trim line)],
      "Spec" `isSuffixOf` moduleName
    ]
  where
    -- Handle both prefix ("import qualified Foo") and postfix ("import Foo qualified") syntax
    parseMainImport line
      | Just rest <- stripPrefix "import qualified " line =
          let modName = takeWhile (\c -> c /= ' ' && c /= '(') rest
           in if null modName then Nothing else Just modName
      | Just rest <- stripPrefix "import " line =
          case words rest of
            (modName : "qualified" : _)
              | not (null modName) -> Just modName
            _ -> Nothing
      | otherwise = Nothing

discoverMainCalls :: IO [String]
discoverMainCalls = do
  src <- lines <$> readFile "test/Main.hs"
  pure
    [ reverse (drop 5 (reverse trimmed))
    | line <- src,
      let trimmed = trim line,
      ".spec" `isSuffixOf` trimmed
    ]

findImportOffenders :: [FilePath] -> IO [FilePath]
findImportOffenders roots = do
  hsFiles <- concat <$> mapM collectHsFiles roots
  offenders <- mapM hasMyLibImport hsFiles
  pure [path | (path, True) <- offenders]

hasMyLibImport :: FilePath -> IO (FilePath, Bool)
hasMyLibImport path = do
  src <- readFile path
  pure (path, any (== "import MyLib") (map trimImport (lines src)))

findCastConstraintOffenders :: [FilePath] -> IO [FilePath]
findCastConstraintOffenders roots = do
  hsFiles <- concat <$> mapM collectHsFiles roots
  offenders <- mapM hasCastConstraintUse hsFiles
  pure [path | (path, True) <- offenders]

hasCastConstraintUse :: FilePath -> IO (FilePath, Bool)
hasCastConstraintUse path = do
  src <- readFile path
  let definingModule = path == "src/MLF/Constraint/Types/Graph.hs"
  pure (path, not definingModule && containsCastConstraintCall src)

data SourceMode
  = SourceCode
  | SourceLineComment
  | SourceBlockComment Int
  | SourceString

data SymbolListContext
  = OutsideSymbolList
  | PendingSymbolList
  | InsideSymbolList Int

stripHaskellCommentsAndLiterals :: String -> String
stripHaskellCommentsAndLiterals = go SourceCode
  where
    go _ [] = []
    go SourceCode ('-' : '-' : rest) = ' ' : ' ' : go SourceLineComment rest
    go SourceCode ('{' : '-' : rest) = ' ' : ' ' : go (SourceBlockComment 1) rest
    go SourceCode ('"' : rest) = ' ' : go SourceString rest
    go SourceCode (ch : rest) = ch : go SourceCode rest
    go SourceLineComment ('\n' : rest) = '\n' : go SourceCode rest
    go SourceLineComment (_ : rest) = ' ' : go SourceLineComment rest
    go (SourceBlockComment depth) ('{' : '-' : rest) =
      ' ' : ' ' : go (SourceBlockComment (depth + 1)) rest
    go (SourceBlockComment 1) ('-' : '}' : rest) = ' ' : ' ' : go SourceCode rest
    go (SourceBlockComment depth) ('-' : '}' : rest) =
      ' ' : ' ' : go (SourceBlockComment (depth - 1)) rest
    go mode@(SourceBlockComment _) ('\n' : rest) = '\n' : go mode rest
    go mode@(SourceBlockComment _) (_ : rest) = ' ' : go mode rest
    go SourceString ('\\' : _ : rest) = ' ' : ' ' : go SourceString rest
    go SourceString ('"' : rest) = ' ' : go SourceCode rest
    go SourceString ('\n' : rest) = '\n' : go SourceCode rest
    go SourceString (_ : rest) = ' ' : go SourceString rest

containsCastConstraintCall :: String -> Bool
containsCastConstraintCall source =
  any lineHasCastConstraintCall (zip3 [0 ..] symbolListLines sourceLines)
  where
    sourceLines = lines (stripHaskellCommentsAndLiterals source)
    symbolListLines = importExportSymbolListLines sourceLines
    lineHasCastConstraintCall (lineIndex, inImportExportList, line)
      | isImportLine line = False
      | isModuleHeaderLine line = False
      | otherwise =
          any
            (isCallAt lineIndex inImportExportList line)
            (identifierTokenOffsets "castConstraint" line)
    isCallAt lineIndex inImportExportList line offset =
      let (prefix, tokenAndSuffix) = splitAt offset line
          suffix = drop (length "castConstraint") tokenAndSuffix
       in isCastConstraintCallContext
            inImportExportList
            (nextLineStartsSignature lineIndex)
            (previousLinesContinueExpression lineIndex)
            prefix
            suffix
    nextLineStartsSignature lineIndex =
      case firstNonEmpty (drop (lineIndex + 1) sourceLines) of
        Just nextLine -> "::" `isPrefixOf` trim nextLine
        Nothing -> False
    previousLinesContinueExpression lineIndex =
      case drop lineIndex sourceLines of
        currentLine : _ ->
          priorLinesContinueExpression (lineIndent currentLine) (reverse (take lineIndex sourceLines))
        [] -> False

isImportLine :: String -> Bool
isImportLine line = "import " `isPrefixOf` trim line

isModuleHeaderLine :: String -> Bool
isModuleHeaderLine line = "module " `isPrefixOf` trim line

importExportSymbolListLines :: [String] -> [Bool]
importExportSymbolListLines = go OutsideSymbolList
  where
    go _ [] = []
    go listContext (line : rest) =
      let startsPendingList =
            case listContext of
              PendingSymbolList -> "(" `isPrefixOf` trim line
              _ -> False
          inSymbolList =
            startsPendingList
              || case listContext of
                InsideSymbolList _ -> True
                _ -> False
          nextContext = advanceSymbolListContext listContext line
       in inSymbolList : go nextContext rest

advanceSymbolListContext :: SymbolListContext -> String -> SymbolListContext
advanceSymbolListContext listContext line =
  case listContext of
    InsideSymbolList depth -> continueList (depth + parenDelta line)
    PendingSymbolList
      | null (trim line) -> PendingSymbolList
      | "(" `isPrefixOf` trim line -> continueList (parenDelta line)
      | otherwise -> OutsideSymbolList
    OutsideSymbolList
      | startsSymbolListOwner line ->
          let depth = parenDelta line
           in if depth > 0
                then InsideSymbolList depth
                else
                  if hasOpeningParen line
                    then OutsideSymbolList
                    else PendingSymbolList
      | otherwise -> OutsideSymbolList
  where
    continueList depth
      | depth > 0 = InsideSymbolList depth
      | otherwise = OutsideSymbolList

startsSymbolListOwner :: String -> Bool
startsSymbolListOwner line =
  isImportLine line
    || (isModuleHeaderLine line && not (" where" `isInfixOf` trim line))

parenDelta :: String -> Int
parenDelta = go 0
  where
    go depth [] = depth
    go depth ('(' : rest) = go (depth + 1) rest
    go depth (')' : rest) = go (depth - 1) rest
    go depth (_ : rest) = go depth rest

hasOpeningParen :: String -> Bool
hasOpeningParen [] = False
hasOpeningParen ('(' : _) = True
hasOpeningParen (_ : rest) = hasOpeningParen rest

isCastConstraintCallContext :: Bool -> Bool -> Bool -> String -> String -> Bool
isCastConstraintCallContext inImportExportList nextLineStartsSignature previousLineContinuesExpression prefix suffix =
  not (isSignatureEntry || isImportExportListEntry)
  where
    prefixTrimmed = trim prefix
    suffixTrimmed = trim suffix
    onlyImportExportPunctuation = all (`elem` "(),")
    onlySignaturePunctuation = all (== ',')
    isSignatureEntry =
      onlySignaturePunctuation prefixTrimmed
        && ( "::" `isPrefixOf` suffixTrimmed
              || ( nextLineStartsSignature
                     && not previousLineContinuesExpression
                     && onlySignaturePunctuation suffixTrimmed
                 )
           )
    isImportExportListEntry =
      inImportExportList
        && onlyImportExportPunctuation prefixTrimmed
        && onlyImportExportPunctuation suffixTrimmed

identifierTokenOffsets :: String -> String -> [Int]
identifierTokenOffsets needle haystack = go 0 haystack
  where
    go _ [] = []
    go offset rest@(_ : tailRest)
      | Just suffix <- stripPrefix needle rest,
        hasIdentifierBoundary offset suffix =
          offset : go (offset + 1) tailRest
      | otherwise = go (offset + 1) tailRest
    hasIdentifierBoundary offset suffix =
      let beforeOk =
            offset == 0
              || not (isIdentifierChar (haystack !! (offset - 1)))
          afterOk =
            case suffix of
              ch : _ -> not (isIdentifierChar ch)
              [] -> True
       in beforeOk && afterOk

isIdentifierChar :: Char -> Bool
isIdentifierChar ch = isAlphaNum ch || ch == '_' || ch == '\''

firstNonEmpty :: [String] -> Maybe String
firstNonEmpty [] = Nothing
firstNonEmpty (line : rest)
  | null (trim line) = firstNonEmpty rest
  | otherwise = Just line

lineContinuesExpression :: String -> Bool
lineContinuesExpression line =
  any (`isSuffixOf` trimmed) ["=", "(", "$", "\\", "->"]
  where
    trimmed = trim line

priorLinesContinueExpression :: Int -> [String] -> Bool
priorLinesContinueExpression currentIndent = go
  where
    go [] = False
    go (line : rest)
      | null (trim line) = go rest
      | lineIndent line < currentIndent = lineContinuesExpression line
          || (not (lineStartsDeclarationBoundary line) && go rest)
      | currentIndent == 0 = lineContinuesExpression line
      | otherwise = go rest

lineIndent :: String -> Int
lineIndent = length . takeWhile (== ' ')

lineStartsDeclarationBoundary :: String -> Bool
lineStartsDeclarationBoundary line =
  any
    (`isPrefixOf` trimmed)
    ["class ", "data ", "instance ", "module ", "import "]
    || " where" `isSuffixOf` trimmed
  where
    trimmed = trim line

collectHsFiles :: FilePath -> IO [FilePath]
collectHsFiles root = do
  entries <- listDirectory root
  fmap concat $ mapM (go root) entries
  where
    go dir entry = do
      let path = dir </> entry
      isFile <- doesFileExist path
      if isFile
        then pure [path | takeExtension path == ".hs"]
        else collectHsFiles path

normalizeModuleToken :: String -> String
normalizeModuleToken = reverse . dropWhile (`elem` ", ") . reverse . trim

dropFieldPrefix :: String -> String
dropFieldPrefix line =
  case break (== ':') line of
    (_field, ':' : rest) -> rest
    _ -> line

trimImport :: String -> String
trimImport = unwords . take 2 . words

pathToModule :: FilePath -> FilePath -> String
pathToModule root path =
  intercalate "." (splitDirectories (dropExtension (makeRelative root path)))

isMainEntryPath :: FilePath -> FilePath -> Bool
isMainEntryPath root path =
  case splitDirectories (makeRelative root path) of
    [_file] -> True
    [dir, _file] -> dir `elem` ["Constraint", "Phi", "Presolution", "Property", "Reify", "Research", "Thesis", "Util"]
    _ -> False

assertSet :: String -> [String] -> [String] -> Expectation
assertSet label expected actual =
  if expected == actual
    then pure ()
    else
      expectationFailure $
        label ++ " mismatch\nexpected: " ++ show expected ++ "\nactual: " ++ show actual

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x : xs) (y : ys)
  | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

splitFacadeGuards :: [(FilePath, Int, [String])]
splitFacadeGuards =
  [ ( "src/MLF/Elab/Phi/Omega.hs",
      30,
      [ "import MLF.Elab.Phi.Omega.Domain",
        "import MLF.Elab.Phi.Omega.Interpret",
        "import MLF.Elab.Phi.Omega.Normalize"
      ]
    ),
    ( "src/MLF/Constraint/Presolution/EdgeUnify.hs",
      95,
      [ "import MLF.Constraint.Presolution.EdgeUnify.State",
        "import qualified MLF.Constraint.Presolution.EdgeUnify.Omega as Omega",
        "import MLF.Constraint.Presolution.EdgeUnify.Unify"
      ]
    ),
    ( "src/MLF/Reify/Core.hs",
      95,
      [ "import qualified MLF.Reify.Bound as Bound",
        "import qualified MLF.Reify.Named as Named",
        "import qualified MLF.Reify.Type as Type"
      ]
    ),
    ( "src/MLF/Constraint/Solve.hs",
      130,
      [ "import MLF.Constraint.Solve.Finalize",
        "import MLF.Constraint.Solve.Internal",
        "import MLF.Constraint.Solve.Worklist"
      ]
    ),
    ( "src/MLF/Elab/Elaborate.hs",
      120,
      [ "import MLF.Elab.Elaborate.Algebra",
        "import MLF.Elab.Elaborate.Annotation",
        "import MLF.Elab.Elaborate.Scope"
      ]
    )
  ]

splitChildModules :: [String]
splitChildModules =
  [ "MLF.Constraint.Presolution.EdgeUnify.State",
    "MLF.Constraint.Presolution.EdgeUnify.Omega",
    "MLF.Constraint.Presolution.EdgeUnify.Unify",
    "MLF.Constraint.Solve.Worklist",
    "MLF.Constraint.Solve.Harmonize",
    "MLF.Constraint.Solve.Finalize",
    "MLF.Reify.Cache",
    "MLF.Reify.Named",
    "MLF.Reify.Type",
    "MLF.Reify.Bound",
    "MLF.Elab.Elaborate.Algebra",
    "MLF.Elab.Elaborate.Scope",
    "MLF.Elab.Elaborate.Annotation",
    "MLF.Elab.Phi.Omega.Domain",
    "MLF.Elab.Phi.Omega.Interpret",
    "MLF.Elab.Phi.Omega.Normalize"
  ]

backendBoundaryMechanismNames :: [String]
backendBoundaryMechanismNames =
  [ "IR role separation and non-duplication",
    "Eager runtime lowering contract",
    "Direct calls, closure values, and callable shapes",
    "ADT/case semantics versus layout",
    "Primitive operations and eager evaluation order",
    "Polymorphism erasure and lowerability",
    "Validation, evidence, and guidance synchronization"
  ]

backendBoundaryFocusedGuardNames :: [String]
backendBoundaryFocusedGuardNames =
  [ "one-backend-IR contract stays explicit and no public lower IR leaks",
    "eager-runtime lowering contract stays explicit and lazy STG machinery stays out of scope",
    "callable-shape contract stays explicit and direct-vs-closure call heads stay unambiguous",
    "ADT and case semantic boundary stays explicit while lowerer-owned layout policy stays private and frozen",
    "primitive-operation and eager-evaluation-order contract stays explicit without widening the backend boundary",
    "polymorphism-erasure and lowerability contract stays explicit without widening the backend boundary",
    "backend-boundary mechanism table and closeout ledger stay synchronized"
  ]

backendBoundaryRow7EvidenceMarkers :: [String]
backendBoundaryRow7EvidenceMarkers =
  [ "docs/architecture.md",
    "docs/backend-native-pipeline.md",
    "src/MLF/Backend/IR.hs",
    "src/MLF/Backend/Convert.hs",
    "src/MLF/Backend/LLVM/Lower.hs",
    "test/BackendIRSpec.hs",
    "test/BackendConvertSpec.hs",
    "test/BackendLLVMSpec.hs",
    "test/RepoGuardSpec.hs",
    "backend-boundary mechanism table and closeout ledger stay synchronized"
  ]

backendBoundaryCloseoutTodoMarkers :: [String]
backendBoundaryCloseoutTodoMarkers =
  [ "## Task 110 backend IR executable-boundary family closeout (completed 2026-05-03)",
    "rows 1 through 6 of the backend IR executable-boundary",
    "`710c92eb`",
    "Row 7 now closes the mechanism table and guidance ledger",
    "one executable eager backend IR",
    "no public `LowerableBackend.IR`",
    "no lazy STG machinery",
    "No new backend implementation feature was added by this closeout."
  ]

backendBoundaryCloseoutImplementationNoteMarkers :: [String]
backendBoundaryCloseoutImplementationNoteMarkers =
  [ "## 2026-05-03 - Backend IR executable-boundary family closed on the merged 710c92eb baseline",
    "rows 1 through 7 closed on the merged `710c92eb` baseline",
    "one executable eager backend IR",
    "eager runtime lowering only",
    "no lazy STG machinery",
    "no public `LowerableBackend.IR`",
    "no fallback/runtime-rescue widening",
    "no new backend implementation feature"
  ]

backendBoundaryCloseoutChangelogMarkers :: [String]
backendBoundaryCloseoutChangelogMarkers =
  [ "Closed the backend IR executable-boundary family on merged `710c92eb`",
    "seven backend-boundary mechanism-table rows are now explicitly settled",
    "`backend-boundary mechanism table and closeout ledger stay synchronized`",
    "repo-facing note sync",
    "one executable eager backend IR",
    "no public `LowerableBackend.IR`",
    "no lazy STG",
    "no new backend feature or public boundary was introduced"
  ]

architectureContractMarkers :: [String]
architectureContractMarkers =
  [ "xMLF remains the thesis-faithful typed elaboration IR.",
    "`MLF.Backend.IR` is the single executable eager backend IR in the current",
    "`MLF.Backend.Convert` is the only checked-program to backend-IR conversion",
    "layout-only structure, or lowerability-only",
    "public `LowerableBackend.IR`"
  ]

backendIRContractMarkers :: [String]
backendIRContractMarkers =
  [ "xMLF remains the thesis-faithful typed elaboration IR;",
    "`MLF.Backend.IR` is the single executable eager backend IR;",
    "no second executable backend IR, no public `LowerableBackend.IR`, and no",
    "layout-only structure, or lowerability-only"
  ]

backendConvertContractMarkers :: [String]
backendConvertContractMarkers =
  [ "xMLF remains the thesis-faithful typed",
    "`MLF.Backend.IR` is the single executable eager backend",
    "Checked-program conversion stops at `MLF.Backend.IR`;",
    "unsupported checked",
    "must fail here instead of being rerouted through a second IR layer.",
    "public `LowerableBackend.IR`"
  ]

backendLowerContractMarkers :: [String]
backendLowerContractMarkers =
  [ "xMLF remains the thesis-faithful typed elaboration IR, and `MLF.Backend.IR`",
    "Both 'lowerBackendProgram' and",
    "'lowerBackendProgramNative' lower the same `MLF.Backend.IR` program.",
    "public `LowerableBackend.IR`"
  ]

nativePipelineContractMarkers :: [String]
nativePipelineContractMarkers =
  [ "xMLF remains the thesis-faithful typed elaboration IR, and",
    "`MLF.Backend.IR` is the single executable eager backend IR.",
    "`emit-backend` and `emit-native` consume the same `MLF.Backend.IR` program",
    "it is not a second executable IR,",
    "becoming a public `LowerableBackend.IR`"
  ]

architectureEagerRuntimeMarkers :: [String]
architectureEagerRuntimeMarkers =
  [ "`MLF.Backend.IR` owns the eager executable",
    "typed direct application, explicit closures and",
    "`BackendClosureCall`, ADT construction and case analysis, lets, lambdas, type",
    "LLVM/native lowering owns only downstream private lowering/runtime details",
    "Both raw and native emission still",
    "consume the same backend IR program."
  ]

backendIREagerRuntimeMarkers :: [String]
backendIREagerRuntimeMarkers =
  [ "`MLF.Backend.IR` owns the eager executable representation consumed by the",
    "typed direct application, explicit closures and",
    "`BackendClosureCall`, ADT construction and case analysis, lets, lambdas,",
    "validation-visible invariants for those executable shapes live at this",
    "closure-record layout, native process entrypoints, renderer helpers, native"
  ]

backendConvertEagerRuntimeMarkers :: [String]
backendConvertEagerRuntimeMarkers =
  [ "Checked-program conversion publishes that eager executable representation",
    "direct application, explicit closures and",
    "`BackendClosureCall`, ADT construction and case analysis, lets, lambdas, type",
    "Unsupported checked shapes fail here",
    "lazy runtime artifacts, lowerer-private",
    "native-wrapper-specific machinery."
  ]

backendLowerEagerRuntimeMarkers :: [String]
backendLowerEagerRuntimeMarkers =
  [ "Both 'lowerBackendProgram' and",
    "'lowerBackendProgramNative' lower the same `MLF.Backend.IR` program.",
    "LLVM lowering and native emission own only the downstream private",
    "wrapper/runtime symbol emission, and executable rendering support.",
    "Raw LLVM emission and native emission",
    "both start from the same `MLF.Backend.IR` program"
  ]

nativePipelineEagerRuntimeMarkers :: [String]
nativePipelineEagerRuntimeMarkers =
  [ "`emit-backend` and `emit-native` consume the same `MLF.Backend.IR` program",
    "`emit-backend` is the raw inspection/lowering output from that same",
    "`emit-native` is that same eager IR plus private",
    "native-entrypoint/runtime support only.",
    "The added support",
    "does not create a second executable IR or a lazy runtime."
  ]

eagerRuntimeExclusionMarkers :: [String]
eagerRuntimeExclusionMarkers =
  [ "no thunks",
    "no update frames",
    "no CAF update semantics",
    "no graph reduction",
    "no implicit laziness rescue"
  ]

architectureCallableShapeMarkers :: [String]
architectureCallableShapeMarkers =
  [ "That callable contract is explicit. `BackendApp` is the direct first-order",
    "`BackendClosureCall` is the indirect closure-call node, so closure-valued",
    "`BackendApp` heads must stay on the direct-call path",
    "`BackendClosureCall` heads must remain closure"
  ]

backendIRCallableShapeMarkers :: [String]
backendIRCallableShapeMarkers =
  [ "`BackendApp` is the direct first-order call node",
    "closure-valued heads violate a",
    "`BackendClosureCall` is the indirect closure-call node",
    "heads are rejected with explicit callable diagnostics."
  ]

backendConvertCallableShapeMarkers :: [String]
backendConvertCallableShapeMarkers =
  [ "`BackendApp` is reserved for direct first-order callable heads",
    "closure-valued aliases, captured closures, and case/let-selected closure",
    "values are emitted as `BackendClosureCall`"
  ]

backendLowerCallableShapeMarkers :: [String]
backendLowerCallableShapeMarkers =
  [ "indirect closure calls must use",
    "the explicit `BackendClosureCall` node.",
    "Lowering consumes that same callable",
    "`BackendApp` remains the direct first-order call path",
    "case/let-selected closure values must"
  ]

backendADTCaseOwnershipMarkers :: [String]
backendADTCaseOwnershipMarkers =
  [ "Row-4 ADT/case ownership",
    "semantic constructor/case nodes",
    "`MLF.Backend.IR`",
    "field slots, closure-record storage for",
    "nullary tag-only representation stay private to"
  ]

nativePipelineADTCaseLayoutMarkers :: [String]
nativePipelineADTCaseLayoutMarkers =
  [ "declaration-order",
    "zero-based constructor tags drive emitted `switch` targets",
    "object offset `0`",
    "function-like constructor fields store explicit closure records",
    "nullary constructors",
    "tag-only heap objects"
  ]

backendLowerADTCaseLayoutMarkers :: [String]
backendLowerADTCaseLayoutMarkers =
  [ "declaration-order zero-based constructor tags",
    "the tag word is stored at object offset `0`",
    "field slots start after that tag word",
    "function-like constructor fields are stored as explicit closure records",
    "nullary constructors use tag-only heap objects"
  ]

primitiveOperationEagerOrderMarkers :: [String]
primitiveOperationEagerOrderMarkers =
  [ "closed reserved runtime-binding set",
    "__mlfp_and",
    "__io_pure",
    "__io_bind",
    "__io_putStrLn",
    "`BackendVar`, `BackendApp`, and `BackendTyApp`",
    "no new `BackendPrim`",
    "let RHS before body",
    "case scrutinee before branch selection",
    "direct/primitive call arguments in written order",
    "effect sequencing remains explicit through `__io_bind`"
  ]

polymorphismLowerabilityMarkers :: [String]
polymorphismLowerabilityMarkers =
  [ "checked `Backend.IR` may still carry `BackendTyAbs` and `BackendTyApp`",
    "LLVM/native lowering owns only the specialization-based lowerable subset",
    "type applications may specialize privately inside the lowerer",
    "runtime polymorphism remains unsupported and must fail with explicit diagnostics without widening the backend boundary"
  ]

futureLowerIRCriteriaMarkers :: [String]
futureLowerIRCriteriaMarkers =
  [ "distinct backend-owned executable invariants that cannot live in",
    "`MLF.Backend.IR` or a private lowering helper",
    "a dedicated validation/evidence owner for that new boundary",
    "a later accepted roadmap revision before any new durable or public surface",
    "is added."
  ]

assertMarkersPresent :: FilePath -> String -> [String] -> Expectation
assertMarkersPresent path src markers =
  forM_ markers assertMarker
  where
    assertMarker marker =
      if marker `isInfixOf` src
        then pure ()
        else expectationFailure (path ++ " missing marker: " ++ show marker)

countModuleEntries :: String -> String -> Int
countModuleEntries moduleName src =
  length
    [ ()
    | line <- lines src,
      normalizeModuleToken (dropFieldPrefix (trim line)) == moduleName
    ]

data MechanismTableRow = MechanismTableRow
  { mechanismTableRowName :: String,
    mechanismTableRowGapSummary :: String,
    mechanismTableRowEvidence :: String,
    mechanismTableRowGate :: String,
    mechanismTableRowNextAction :: String
  }

parseBackendBoundaryMechanismTable :: String -> Either String [MechanismTableRow]
parseBackendBoundaryMechanismTable src =
  traverse parseMechanismTableRow rowLines
  where
    rowLines =
      [ line
      | line <- lines src,
        isPrefixOf "| " line,
        not (isPrefixOf "|---" line),
        not (isPrefixOf "| Mechanism " line)
      ]

parseMechanismTableRow :: String -> Either String MechanismTableRow
parseMechanismTableRow line =
  case map trim (stripTableEdgeCells (splitOn '|' line)) of
    [name, _currentBehavior, _targetBehavior, gapSummary, evidence, gate, nextAction] ->
      Right
        MechanismTableRow
          { mechanismTableRowName = name,
            mechanismTableRowGapSummary = gapSummary,
            mechanismTableRowEvidence = evidence,
            mechanismTableRowGate = gate,
            mechanismTableRowNextAction = nextAction
          }
    parts ->
      Left
        ( "could not parse backend-boundary mechanism-table row: "
            ++ show line
            ++ "\nparsed cells: "
            ++ show parts
        )

stripTableEdgeCells :: [String] -> [String]
stripTableEdgeCells cells =
  case cells of
    [] -> []
    (_ : rest) -> reverse (drop 1 (reverse rest))

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delimiter = go []
  where
    go acc [] = [reverse acc]
    go acc (x : xs)
      | x == delimiter = reverse acc : go [] xs
      | otherwise = go (x : acc) xs

extractPublicLibraryStanza :: String -> String
extractPublicLibraryStanza src =
  unlines (takeWhile (not . isNextStanza) (drop 1 afterPublicLibrary))
  where
    ls = lines src
    afterPublicLibrary = dropWhile (/= "library") ls
    isNextStanza line =
      any
        (`isPrefixOf` trim line)
        [ "library ",
          "executable ",
          "test-suite ",
          "benchmark ",
          "foreign-library "
        ]

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys
