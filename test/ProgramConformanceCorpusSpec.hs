module ProgramConformanceCorpusSpec (spec) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory)
import Test.Hspec

import MLF.Program.CLI (runProgramArgs)

spec :: Spec
spec =
    describe "MLF.Program shared conformance corpus" $
        it "shared conformance corpus validates run-program package fixture" $ do
            fixture <- loadFixture conformanceFixtureMeta
            actual <- runProgramArgs [fixturePackageRoot fixture]
            expected <- readFile (fixtureExpectedStdout fixture)

            actual `shouldBe` Right expected

data ConformanceFixture = ConformanceFixture
    { fixturePackageRoot :: FilePath
    , fixtureExpectedStdout :: FilePath
    }

conformanceFixtureMeta :: FilePath
conformanceFixtureMeta =
    "test/conformance/mlfp/run-program/cross-module-let/fixture.meta"

loadFixture :: FilePath -> IO ConformanceFixture
loadFixture metaPath = do
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
    expectedStdout <- requireField "expected-stdout" fields

    fixtureId `shouldBe` "cross-module-let-run-program"
    command `shouldBe` "run-program"
    expect `shouldBe` "pass"
    normalization `shouldBe` "none"
    stageApplicability `shouldBe` "all"
    tags `shouldBe` "package,public,cross-module,let-polymorphism"

    pure
        ConformanceFixture
            { fixturePackageRoot = fixtureRoot </> packageRoot
            , fixtureExpectedStdout = fixtureRoot </> expectedStdout
            }

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
