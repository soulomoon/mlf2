module RepoGuardSpec (spec) where

import Data.List (intercalate, isInfixOf, isSuffixOf, sort)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>), dropExtension, makeRelative, splitDirectories, takeExtension)
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

discoverSpecModules :: FilePath -> IO [String]
discoverSpecModules root = do
    hsFiles <- collectHsFiles root
    pure
        [ pathToModule root path
        | path <- hsFiles
        , "Spec.hs" `isSuffixOf` path
        ]

discoverMainEntrySpecModules :: FilePath -> IO [String]
discoverMainEntrySpecModules root = do
    hsFiles <- collectHsFiles root
    pure
        [ pathToModule root path
        | path <- hsFiles
        , "Spec.hs" `isSuffixOf` path
        , isMainEntryPath root path
        ]

discoverCabalSpecModules :: IO [String]
discoverCabalSpecModules = do
    src <- lines <$> readFile "mlf2.cabal"
    pure
        [ moduleName
        | line <- src
        , let moduleName = normalizeModuleToken (dropFieldPrefix (trim line))
        , "Spec" `isSuffixOf` moduleName
        ]

discoverMainImports :: IO [String]
discoverMainImports = do
    src <- lines <$> readFile "test/Main.hs"
    pure
        [ moduleName
        | line <- src
        , Just moduleName <- [stripPrefix "import qualified " (trim line)]
        , ".spec" `notElem` words moduleName
        , "Spec" `isSuffixOf` moduleName
        ]

discoverMainCalls :: IO [String]
discoverMainCalls = do
    src <- lines <$> readFile "test/Main.hs"
    pure
        [ reverse (drop 5 (reverse trimmed))
        | line <- src
        , let trimmed = trim line
        , ".spec" `isSuffixOf` trimmed
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
        ("import":"qualified":modName:_) -> Just modName
        ("import":modName:_) -> Just modName
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
        [dir, _file] -> dir `elem` ["Constraint", "Phi"]
        _ -> False

assertSet :: String -> [String] -> [String] -> Expectation
assertSet label expected actual =
    if expected == actual
        then pure ()
        else expectationFailure $
            label ++ " mismatch\nexpected: " ++ show expected ++ "\nactual: " ++ show actual

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
    | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing
