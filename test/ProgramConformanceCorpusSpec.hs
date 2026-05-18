module ProgramConformanceCorpusSpec (spec) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory)
import Test.Hspec

import MLF.Program.CLI (runProgramArgs)

spec :: Spec
spec =
    describe "MLF.Program shared conformance corpus" $ do
        it "shared conformance corpus validates run-program package fixture" $ do
            fixture <- loadFixture crossModuleLetFixture
            actual <- runProgramArgs (fixtureRunProgramArgs fixture)
            expected <- readFile (fixtureExpectedStdout fixture)

            actual `shouldBe` Right expected

        it "shared conformance corpus validates run-program search-path fixture" $ do
            fixture <- loadFixture searchPathFixture
            actual <- runProgramArgs (fixtureRunProgramArgs fixture)
            expected <- readFile (fixtureExpectedStdout fixture)

            actual `shouldBe` Right expected

data ConformanceFixture = ConformanceFixture
    { fixtureRunProgramArgs :: [FilePath]
    , fixtureExpectedStdout :: FilePath
    }

data FixtureExpectation = FixtureExpectation
    { expectationMetaPath :: FilePath
    , expectationFixtureId :: String
    , expectationSearchPaths :: String
    , expectationTags :: String
    }

crossModuleLetFixture :: FixtureExpectation
crossModuleLetFixture =
    FixtureExpectation
        { expectationMetaPath =
            "test/conformance/mlfp/run-program/cross-module-let/fixture.meta"
        , expectationFixtureId = "cross-module-let-run-program"
        , expectationSearchPaths = "none"
        , expectationTags = "package,public,cross-module,let-polymorphism"
        }

searchPathFixture :: FixtureExpectation
searchPathFixture =
    FixtureExpectation
        { expectationMetaPath =
            "test/conformance/mlfp/run-program/search-path-package/fixture.meta"
        , expectationFixtureId = "search-path-run-program"
        , expectationSearchPaths = "roots/lib"
        , expectationTags = "package,public,search-path,cross-root-import"
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
    expectedStdout <- requireField "expected-stdout" fields

    fixtureId `shouldBe` expectationFixtureId expectation
    command `shouldBe` "run-program"
    expect `shouldBe` "pass"
    normalization `shouldBe` "none"
    stageApplicability `shouldBe` "all"
    searchPaths `shouldBe` expectationSearchPaths expectation
    tags `shouldBe` expectationTags expectation

    let packageArg = fixtureRoot </> packageRoot
        searchPathArgs = concatMap searchPathArg (parseSearchPaths fixtureRoot searchPaths)

    pure
        ConformanceFixture
            { fixtureRunProgramArgs = packageArg : searchPathArgs
            , fixtureExpectedStdout = fixtureRoot </> expectedStdout
            }

searchPathArg :: FilePath -> [FilePath]
searchPathArg path =
    ["--search-path", path]

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
