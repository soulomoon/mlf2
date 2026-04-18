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
    runSrc <- readFile "src/MLF/Frontend/Program/Run.hs"
    pipelineSrc <- readFile "src-public/MLF/Pipeline.hs"
    syntaxSrc <- readFile "src/MLF/Frontend/Syntax.hs"
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
        "import MLF.Elab.TypeCheck",
        "Elab.ETyAbs",
        "Elab.ELam",
        "Elab.EApp",
        "Elab.ERoll",
        "Elab.EUnroll"
      ]
      $ \marker -> do
        checkSrc `shouldSatisfy` (not . isInfixOf marker)
        elaborateSrc `shouldSatisfy` (not . isInfixOf marker)
        runSrc `shouldSatisfy` (not . isInfixOf marker)
        pipelineSrc `shouldSatisfy` (not . isInfixOf marker)
    forM_
      [ "EUnfold ::",
        "EUnfoldSurfaceF",
        "unfold marker for case scrutinees"
      ]
      $ \marker ->
        syntaxSrc `shouldSatisfy` (not . isInfixOf marker)
    checkSrc `shouldSatisfy` isInfixOf "import MLF.Frontend.Program.Elaborate"
    elaborateSrc `shouldSatisfy` isInfixOf "runPipelineElabWithEnv"
    runSrc `shouldSatisfy` isInfixOf "normalize"

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
    [dir, _file] -> dir `elem` ["Constraint", "Phi", "Property", "Reify", "Research", "Util"]
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
