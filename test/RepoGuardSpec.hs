module RepoGuardSpec (spec) where

import Control.Monad (forM_)
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

  it "modules do not import both MLF.Constraint.Types and .Graph" $ do
    offenders <- findDualImportOffenders ["src", "src-public", "test", "app"]
    offenders `shouldBe` []

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

findDualImportOffenders :: [FilePath] -> IO [FilePath]
findDualImportOffenders roots = do
  hsFiles <- concat <$> mapM collectHsFiles roots
  offenders <- mapM hasDualImports hsFiles
  pure [path | (path, True) <- offenders]

hasMyLibImport :: FilePath -> IO (FilePath, Bool)
hasMyLibImport path = do
  src <- readFile path
  pure (path, any (== "import MyLib") (map trimImport (lines src)))

hasDualImports :: FilePath -> IO (FilePath, Bool)
hasDualImports path = do
  src <- readFile path
  let imports = [moduleName | line <- lines src, Just moduleName <- [parseImportModule line]]
  pure (path, "MLF.Constraint.Types" `elem` imports && "MLF.Constraint.Types.Graph" `elem` imports)

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

parseImportModule :: String -> Maybe String
parseImportModule line =
  case words (trim line) of
    ("import" : "qualified" : modName : _) -> Just modName
    ("import" : modName : _) -> Just modName
    _ -> Nothing

trimImport :: String -> String
trimImport = unwords . take 2 . words

pathToModule :: FilePath -> FilePath -> String
pathToModule root path =
  intercalate "." (splitDirectories (dropExtension (makeRelative root path)))

isMainEntryPath :: FilePath -> FilePath -> Bool
isMainEntryPath root path =
  case splitDirectories (makeRelative root path) of
    [_file] -> True
    ["Presolution", "UnificationClosureSpec.hs"] -> True
    [dir, _file] -> dir `elem` ["Constraint", "Phi", "Property", "Reify", "Research", "Thesis", "Util"]
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
