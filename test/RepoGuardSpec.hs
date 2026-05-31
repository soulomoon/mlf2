module RepoGuardSpec (spec) where

import Control.Monad (forM, forM_)
import Data.List (intercalate, isSuffixOf, sort)
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
    countModuleEntries "MyLib" cabalSrc `shouldBe` 0
    offenders <- findImportOffenders ["MyLib"] ["src", "src-public", "test", "app"]
    offenders `shouldBe` []

  it "legacy MLF.Program compatibility shim is removed" $ do
    doesFileExist "src-public/MLF/Program.hs" >>= (`shouldBe` False)
    cabalSrc <- readFile "mlf2.cabal"
    countModuleEntries "MLF.Program" cabalSrc `shouldBe` 0
    offenders <- findImportOffenders ["MLF.Program"] ["src", "src-public", "test", "app"]
    offenders `shouldBe` []

  it "solver and solved internals stay behind owner and test-support seams" $ do
    offenders <- findConstraintBoundaryImportOffenders
    offenders `shouldBe` []

  it "snapshot finalization internals stay behind Finalize and test-support seams" $ do
    offenders <- findFinalizeInternalImportOffenders
    offenders `shouldBe` []

  it "split child modules stay implementation-only in Cabal" $ do
    cabalSrc <- readFile "mlf2.cabal"
    let publicLibrarySrc = extractPublicLibraryStanza cabalSrc
        publicLibraryModules = listedModules publicLibrarySrc
    forM_ splitChildModules $ \moduleName -> do
      countModuleEntries moduleName cabalSrc `shouldBe` 1
      publicLibraryModules `shouldSatisfy` notElem moduleName

  it "backend implementation modules stay out of the public library" $ do
    cabalSrc <- readFile "mlf2.cabal"
    let publicLibraryModules = listedModules (extractPublicLibraryStanza cabalSrc)
    publicLibraryModules `shouldSatisfy` all (not . hasModulePrefix "MLF.Backend.")
    publicLibraryModules `shouldSatisfy` all (not . hasModulePrefix "LowerableBackend.")

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
    [ moduleName
    | line <- src,
      let trimmed = trim line,
      Just moduleName <- [parseMainCall trimmed]
    ]
  where
    parseMainCall line
      | ".spec" `isSuffixOf` line =
          case reverse (words (reverse (drop 5 (reverse line)))) of
            moduleName : _ -> Just moduleName
            [] -> Nothing
      | otherwise = Nothing

findImportOffenders :: [String] -> [FilePath] -> IO [FilePath]
findImportOffenders moduleNames roots = do
  hsFiles <- concat <$> mapM collectHsFiles roots
  offenders <- mapM hasTargetImport hsFiles
  pure [path | (path, True) <- offenders]
  where
    hasTargetImport path = do
      src <- readFile path
      pure (path, any (`elem` moduleNames) (importedModules src))

findConstraintBoundaryImportOffenders :: IO [String]
findConstraintBoundaryImportOffenders = do
  hsFiles <- concat <$> mapM collectHsFiles ["src", "src-public", "app"]
  fmap sort . fmap concat $
    forM hsFiles $ \path -> do
      src <- readFile path
      pure
        [ path ++ " imports " ++ moduleName
        | moduleName <- importedModules src,
          not (isAllowedConstraintBoundaryImport path moduleName)
        ]

isAllowedConstraintBoundaryImport :: FilePath -> String -> Bool
isAllowedConstraintBoundaryImport path moduleName =
  case moduleName of
    "MLF.Constraint.Solve.Internal" ->
      path
        `elem` [ "src/MLF/Constraint/Finalize.hs",
                 "src/MLF/Constraint/Solve.hs",
                 "src/MLF/Constraint/Solved/Internal.hs"
               ]
        || "src/MLF/Constraint/Solve/" `isPathPrefixOf` path
    "MLF.Constraint.Solved.Internal" ->
      path
        `elem` [ "src/MLF/Constraint/Finalize.hs",
                 "src/MLF/Constraint/Finalize/Internal.hs",
                 "src/MLF/Constraint/Solved.hs",
                 "src/MLF/Constraint/Solved/TestSupport.hs"
               ]
        || "src/MLF/Constraint/Solved/" `isPathPrefixOf` path
    "MLF.Constraint.Solve.TestSupport" -> False
    "MLF.Constraint.Solved.TestSupport" -> False
    "MLF.Constraint.Finalize.TestSupport" -> False
    "MLF.Constraint.Finalize.Internal" ->
      path
        `elem` [ "src/MLF/Constraint/Finalize.hs",
                 "src/MLF/Constraint/Finalize/TestSupport.hs"
               ]
    "MLF.Constraint.Types.Witness.TestSupport" -> False
    _ -> True

findFinalizeInternalImportOffenders :: IO [String]
findFinalizeInternalImportOffenders = do
  hsFiles <- concat <$> mapM collectHsFiles ["src", "src-public", "app", "test"]
  fmap sort . fmap concat $
    forM hsFiles $ \path -> do
      src <- readFile path
      pure
        [ path ++ " imports MLF.Constraint.Finalize.Internal"
        | moduleName <- importedModules src,
          moduleName == "MLF.Constraint.Finalize.Internal",
          path
            `notElem` [ "src/MLF/Constraint/Finalize.hs",
                        "src/MLF/Constraint/Finalize/TestSupport.hs"
                      ]
        ]

importedModules :: String -> [String]
importedModules src =
  [ moduleName
  | line <- lines src,
    Just moduleName <- [parseImportModule line]
  ]

parseImportModule :: String -> Maybe String
parseImportModule line =
  case words (stripLineComment line) of
    "import" : rest -> firstModuleToken rest
    _ -> Nothing
  where
    firstModuleToken [] = Nothing
    firstModuleToken (tok : toks)
      | tok `elem` ["qualified", "safe", "as", "hiding"] = firstModuleToken toks
      | "\"" `isPrefixOfToken` tok = firstModuleToken toks
      | otherwise = Just (normalizeModuleToken tok)

stripLineComment :: String -> String
stripLineComment [] = []
stripLineComment ('-' : '-' : _) = []
stripLineComment (c : cs) = c : stripLineComment cs

isPrefixOfToken :: String -> String -> Bool
isPrefixOfToken prefix token =
  take (length prefix) token == prefix

isPathPrefixOf :: FilePath -> FilePath -> Bool
isPathPrefixOf prefix path =
  take (length prefix) path == prefix

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

pathToModule :: FilePath -> FilePath -> String
pathToModule root path =
  intercalate "." (splitDirectories (dropExtension (makeRelative root path)))

isMainEntryPath :: FilePath -> FilePath -> Bool
isMainEntryPath root path =
  case splitDirectories (makeRelative root path) of
    [_file] -> True
    [dir, _file] -> dir `elem` ["Constraint", "Elab", "Phi", "Presolution", "Property", "Reify", "Research", "Thesis", "Util"]
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
listedModules :: String -> [String]
listedModules src =
  [ moduleName
  | line <- lines src,
    let moduleName = normalizeModuleToken (dropFieldPrefix (trim line)),
    looksLikeModuleName moduleName
  ]

looksLikeModuleName :: String -> Bool
looksLikeModuleName moduleName =
  case moduleName of
    [] -> False
    c : _ -> c `elem` ['A' .. 'Z']

hasModulePrefix :: String -> String -> Bool
hasModulePrefix prefix moduleName =
  prefix `isPrefixOf` moduleName

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
