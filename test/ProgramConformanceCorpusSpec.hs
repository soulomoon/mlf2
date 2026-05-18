module ProgramConformanceCorpusSpec (spec) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory)
import Test.Hspec

import MLF.Program.CLI (checkProgramArgs, runProgramArgs)

spec :: Spec
spec =
    describe "MLF.Program shared conformance corpus" $ do
        it "shared conformance corpus validates run-program package fixture" $ do
            fixture <- loadFixture crossModuleLetFixture
            actual <- runFixture fixture

            actual `shouldMatchFixture` fixture

        it "shared conformance corpus validates run-program search-path fixture" $ do
            fixture <- loadFixture searchPathFixture
            actual <- runFixture fixture

            actual `shouldMatchFixture` fixture

        it "shared conformance corpus validates check-program package fixture" $ do
            fixture <- loadFixture checkProgramCrossModuleLetFixture
            actual <- runFixture fixture

            actual `shouldMatchFixture` fixture

        it "shared conformance corpus validates check-program search-path fixture" $ do
            fixture <- loadFixture checkProgramSearchPathFixture
            actual <- runFixture fixture

            actual `shouldMatchFixture` fixture

        it "shared conformance corpus validates check-program missing-import failure fixture" $ do
            fixture <- loadFixture checkProgramMissingImportFixture
            actual <- runFixture fixture

            actual `shouldMatchFixture` fixture

data ConformanceFixture = ConformanceFixture
    { fixtureCommand :: String
    , fixtureArgs :: [FilePath]
    , fixtureExpectedOutput :: FixtureExpectedOutput
    }

data FixtureExpectedOutput
    = ExpectStdout FilePath
    | ExpectStderr FilePath

data FixtureExpectation = FixtureExpectation
    { expectationMetaPath :: FilePath
    , expectationFixtureId :: String
    , expectationCommand :: String
    , expectationStatus :: String
    , expectationSearchPaths :: String
    , expectationTags :: String
    }

crossModuleLetFixture :: FixtureExpectation
crossModuleLetFixture =
    FixtureExpectation
        { expectationMetaPath =
            "test/conformance/mlfp/run-program/cross-module-let/fixture.meta"
        , expectationFixtureId = "cross-module-let-run-program"
        , expectationCommand = "run-program"
        , expectationStatus = "pass"
        , expectationSearchPaths = "none"
        , expectationTags = "package,public,cross-module,let-polymorphism"
        }

searchPathFixture :: FixtureExpectation
searchPathFixture =
    FixtureExpectation
        { expectationMetaPath =
            "test/conformance/mlfp/run-program/search-path-package/fixture.meta"
        , expectationFixtureId = "search-path-run-program"
        , expectationCommand = "run-program"
        , expectationStatus = "pass"
        , expectationSearchPaths = "roots/lib"
        , expectationTags = "package,public,search-path,cross-root-import"
        }

checkProgramCrossModuleLetFixture :: FixtureExpectation
checkProgramCrossModuleLetFixture =
    FixtureExpectation
        { expectationMetaPath =
            "test/conformance/mlfp/check-program/cross-module-let/fixture.meta"
        , expectationFixtureId = "cross-module-let-check-program"
        , expectationCommand = "check-program"
        , expectationStatus = "pass"
        , expectationSearchPaths = "none"
        , expectationTags = "package,public,cross-module,let-polymorphism,check"
        }

checkProgramSearchPathFixture :: FixtureExpectation
checkProgramSearchPathFixture =
    FixtureExpectation
        { expectationMetaPath =
            "test/conformance/mlfp/check-program/search-path-package/fixture.meta"
        , expectationFixtureId = "search-path-check-program"
        , expectationCommand = "check-program"
        , expectationStatus = "pass"
        , expectationSearchPaths = "roots/lib"
        , expectationTags = "package,public,search-path,cross-root-import,check"
        }

checkProgramMissingImportFixture :: FixtureExpectation
checkProgramMissingImportFixture =
    FixtureExpectation
        { expectationMetaPath =
            "test/conformance/mlfp/check-program/missing-import/fixture.meta"
        , expectationFixtureId = "missing-import-check-program"
        , expectationCommand = "check-program"
        , expectationStatus = "fail"
        , expectationSearchPaths = "none"
        , expectationTags = "package,public,diagnostic,missing-import,check"
        }

loadFixture :: FixtureExpectation -> IO ConformanceFixture
loadFixture expectation = do
    let metaPath = expectationMetaPath expectation
    exists <- doesFileExist metaPath
    exists `shouldBe` True
    contents <- readFile metaPath
    let fields = parseFields contents
        fixtureRoot = takeDirectory metaPath
    fixtureId <- requireField "fixture-id" fields
    command <- requireField "command" fields
    expect <- requireField "expect" fields
    normalization <- requireField "normalization" fields
    stageApplicability <- requireField "stage-applicability" fields
    tags <- requireField "tags" fields
    packageRoot <- requireField "package-root" fields
    searchPaths <- requireField "search-paths" fields
    expectedOutput <- requireExpectedOutputField fixtureRoot expect fields

    fixtureId `shouldBe` expectationFixtureId expectation
    command `shouldBe` expectationCommand expectation
    expect `shouldBe` expectationStatus expectation
    normalization `shouldBe` "none"
    stageApplicability `shouldBe` "all"
    searchPaths `shouldBe` expectationSearchPaths expectation
    tags `shouldBe` expectationTags expectation

    let packageArg = fixtureRoot </> packageRoot
        searchPathArgs = concatMap searchPathArg (parseSearchPaths fixtureRoot searchPaths)

    pure
        ConformanceFixture
            { fixtureCommand = command
            , fixtureArgs = packageArg : searchPathArgs
            , fixtureExpectedOutput = expectedOutput
            }

shouldMatchFixture :: Either String String -> ConformanceFixture -> IO ()
shouldMatchFixture actual fixture =
    case fixtureExpectedOutput fixture of
        ExpectStdout expectedPath -> do
            expected <- readFile expectedPath
            actual `shouldBe` Right expected
        ExpectStderr expectedPath -> do
            expected <- readFile expectedPath
            actual `shouldBe` Left expected

runFixture :: ConformanceFixture -> IO (Either String String)
runFixture fixture =
    case fixtureCommand fixture of
        "run-program" ->
            runProgramArgs (fixtureArgs fixture)
        "check-program" ->
            checkProgramArgs (fixtureArgs fixture)
        command ->
            expectationFailure ("unsupported conformance command: " ++ command)
                >> fail ("unsupported conformance command: " ++ command)

searchPathArg :: FilePath -> [FilePath]
searchPathArg path =
    ["--search-path", path]

requireExpectedOutputField ::
    FilePath ->
    String ->
    [(String, String)] ->
    IO FixtureExpectedOutput
requireExpectedOutputField fixtureRoot expect fields =
    case expect of
        "pass" ->
            ExpectStdout . (fixtureRoot </>) <$> requireField "expected-stdout" fields
        "fail" ->
            ExpectStderr . (fixtureRoot </>) <$> requireField "expected-stderr" fields
        other ->
            expectationFailure ("unsupported conformance expectation: " ++ other)
                >> fail ("unsupported conformance expectation: " ++ other)

parseSearchPaths :: FilePath -> String -> [FilePath]
parseSearchPaths fixtureRoot rawSearchPaths =
    case trim rawSearchPaths of
        "none" ->
            []
        paths ->
            map ((fixtureRoot </>) . trim) (splitOnComma paths)

splitOnComma :: String -> [String]
splitOnComma input =
    case break (== ',') input of
        (chunk, []) ->
            [chunk]
        (chunk, _comma : rest) ->
            chunk : splitOnComma rest

parseFields :: String -> [(String, String)]
parseFields =
    map parseFieldLine
        . filter (not . null)
        . filter (not . isComment)
        . map trim
        . lines

parseFieldLine :: String -> (String, String)
parseFieldLine line =
    let (key, rest) = break (== ':') line
     in (trim key, trim (drop 1 rest))

requireField :: HasCallStack => String -> [(String, String)] -> IO String
requireField key fields =
    case lookup key fields of
        Just value | not (null value) ->
            pure value
        _ ->
            expectationFailure ("missing conformance metadata field: " ++ key)
                >> fail ("missing conformance metadata field: " ++ key)

isComment :: String -> Bool
isComment =
    isPrefixOf "#"

trim :: String -> String
trim =
    dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate =
    reverse . dropWhile predicate . reverse
