module Research.UriR2C1PrototypeP1Spec (spec) where

import Control.Exception (catch)
import Data.Char (isDigit)
import Data.List (isInfixOf)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Test.Hspec

import MLF.Research.URI.R2.C1.Prototype.Entrypoint
    ( AppRun(..)
    , runApp
    , runResearchPrototype
    )
import MLF.Research.URI.R2.C1.Prototype.Types
    ( PrototypeError(..)
    , PrototypeReport
    , PrototypeRequest(..)
    , attemptEvidenceRelativeDir
    , boundedSubjectId
    , candidateInventoryRelativePath
    , prototypeStageResult
    , prototypeSubjectId
    , researchEntrypointId
    , scenarioIdUriR2C1OnlyV1
    , stageSelectorP1
    )

spec :: Spec
spec = do
    describe "URI-R2-C1 P1 prototype entrypoint" $ do
        it "accepts only the locked tuple and attempt ids 1 through 3" $ do
            root <- freshRoot "accepts-locked-tuple"
            mapM_ (runAccepted root) [1 .. 3]
            badResult <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = "wrong-entrypoint"
                    , prStageSelector = stageSelectorP1
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 1
                    }
            badResult `shouldBe` Left (UnsupportedResearchEntrypoint "wrong-entrypoint")

        it "rejects a wrong scenario before writing any evidence files" $ do
            root <- freshRoot "rejects-wrong-scenario"
            result <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = researchEntrypointId
                    , prStageSelector = stageSelectorP1
                    , prScenarioId = "wrong-scenario"
                    , prAttemptId = 1
                    }
            result `shouldBe` Left (UnsupportedScenario "wrong-scenario")
            doesDirectoryExist (attemptDir root 1) >>= (`shouldBe` False)

        it "rejects a wrong stage selector before writing any evidence files" $ do
            root <- freshRoot "rejects-wrong-stage"
            result <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = researchEntrypointId
                    , prStageSelector = "P2-provenance-preservation"
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 1
                    }
            result `shouldBe` Left (UnsupportedStageSelector "P2-provenance-preservation")
            doesDirectoryExist (attemptDir root 1) >>= (`shouldBe` False)

        it "writes the mandatory P1 evidence files" $ do
            root <- freshRoot "writes-mandatory-files"
            report <- requirePrototypePass root 1
            mapM_ (\path -> doesFileExist path >>= (`shouldBe` True))
                [ attemptDir root 1 </> "trace-bundle.json"
                , attemptDir root 1 </> "candidate-inventory.json"
                , attemptDir root 1 </> "candidate-selection-rule.json"
                , attemptDir root 1 </> "check-P1-C.json"
                , attemptDir root 1 </> "check-P1-N.json"
                , attemptDir root 1 </> "check-P1-U.json"
                , attemptDir root 1 </> "stage-verdict.json"
                ]
            prototypeStageResult report `shouldBe` "pass"

        it "emits subject-token.json if and only if the stage result is pass" $ do
            root <- freshRoot "subject-token-iff-pass"
            report <- requirePrototypePass root 1
            let subjectTokenPath = attemptDir root 1 </> "subject-token.json"
            doesFileExist subjectTokenPath >>= (`shouldBe` (prototypeStageResult report == "pass"))

        it "writes bounded identity fields and only allowed rejection triggers" $ do
            root <- freshRoot "writes-bounded-identity"
            report <- requirePrototypePass root 1
            inventory <- readFile (attemptDir root 1 </> "candidate-inventory.json")
            inventory `shouldSatisfy` isInfixOf ("\"scenario_id\": \"" ++ scenarioIdUriR2C1OnlyV1 ++ "\"")
            inventory `shouldSatisfy` isInfixOf "\"candidate_id\""
            inventory `shouldSatisfy` isInfixOf "\"candidate_kind\""
            inventory `shouldSatisfy` isInfixOf "\"normalization_basis\""
            inventory `shouldSatisfy` isInfixOf "\"admissibility_verdict\""
            inventory `shouldSatisfy` isInfixOf "\"rejection_trigger\": \"none\""
            subjectToken <- readFile (attemptDir root 1 </> "subject-token.json")
            subjectToken `shouldSatisfy` isInfixOf ("\"subject_id\": \"" ++ prototypeSubjectId report ++ "\"")
            subjectToken `shouldSatisfy` isInfixOf ("\"bounded_subject\": \"" ++ boundedSubjectId ++ "\"")
            subjectToken `shouldSatisfy` isInfixOf "\"origin_stage\": \"P1\""
            subjectToken `shouldSatisfy`
                isInfixOf ("\"candidate_inventory_ref\": \"" ++ candidateInventoryRelativePath 1 ++ "\"")
            mapM_
                (`shouldSatisfy` (`elem` allowedRejectionTriggers))
                (extractStringValues "rejection_trigger" inventory)

        it "repeats invocation metadata on every machine-readable P1 output" $ do
            root <- freshRoot "repeats-invocation-metadata"
            _ <- requirePrototypePass root 2
            mapM_ (assertInvocationMetadata root 2)
                [ "trace-bundle.json"
                , "candidate-inventory.json"
                , "candidate-selection-rule.json"
                , "check-P1-C.json"
                , "check-P1-N.json"
                , "check-P1-U.json"
                , "stage-verdict.json"
                , "subject-token.json"
                ]

        it "writes the full P1 trace, checker, stage-verdict, and subject-token schema" $ do
            root <- freshRoot "writes-full-p1-schema"
            report <- requirePrototypePass root 1

            traceBundle <- readFile (attemptDir root 1 </> "trace-bundle.json")
            extractStringField "correlation_id" traceBundle `shouldBe` Just "uri-r2-c1-only-v1-attempt-1"
            extractStringField "subject_id" traceBundle `shouldBe` Just (prototypeSubjectId report)
            traceBundle `shouldSatisfy` isInfixOf "\"trace_refs\""

            mapM_
                (assertCheckerSchema root 1 (prototypeSubjectId report))
                [ "check-P1-C.json"
                , "check-P1-N.json"
                , "check-P1-U.json"
                ]

            stageVerdict <- readFile (attemptDir root 1 </> "stage-verdict.json")
            extractIntField "attempt_id" stageVerdict `shouldBe` Just 1
            extractStringField "subject_token_ref" stageVerdict
                `shouldBe` Just (attemptEvidenceRelativeDir 1 </> "subject-token.json")
            stageVerdict `shouldSatisfy` isInfixOf "\"checker_results\""
            extractStringField "terminal_reason" stageVerdict `shouldBe` Just "none"

            subjectToken <- readFile (attemptDir root 1 </> "subject-token.json")
            subjectToken `shouldSatisfy` isInfixOf "\"owner_family_status\""
            subjectToken `shouldSatisfy` isInfixOf "\"trace_handles\""
            extractStringField "candidate_id" subjectToken `shouldBe` Just "cluster-1"
            extractStringField "normalization_basis" subjectToken `shouldBe` Just "cluster-equivalence-v1"
            extractStringField "discovery_trace_ref" subjectToken
                `shouldBe` Just "trace://uri-r2-c1/p1/discovery-cluster-1"

        it "keeps the default no-argument mlf2 path available without prototype flags" $ do
            root <- freshRoot "default-no-arg-path"
            result <- runApp root []
            case result of
                Left err ->
                    expectationFailure ("Expected default no-argument path to succeed, got: " ++ err)
                Right (AppDefaultDemo output) ->
                    output `shouldSatisfy` isInfixOf "Type:"
                Right other ->
                    expectationFailure ("Expected default app mode, got: " ++ show other)

runAccepted :: FilePath -> Int -> Expectation
runAccepted root attemptId = do
    result <- runResearchPrototype $
        PrototypeRequest
            { prRepoRoot = root
            , prResearchEntrypointId = researchEntrypointId
            , prStageSelector = stageSelectorP1
            , prScenarioId = scenarioIdUriR2C1OnlyV1
            , prAttemptId = attemptId
            }
    case result of
        Left err ->
            expectationFailure ("Expected accepted request, got: " ++ show err)
        Right report ->
            prototypeStageResult report `shouldBe` "pass"

requirePrototypePass :: FilePath -> Int -> IO PrototypeReport
requirePrototypePass root attemptId = do
    result <- runResearchPrototype $
        PrototypeRequest
            { prRepoRoot = root
            , prResearchEntrypointId = researchEntrypointId
            , prStageSelector = stageSelectorP1
            , prScenarioId = scenarioIdUriR2C1OnlyV1
            , prAttemptId = attemptId
            }
    case result of
        Left err -> expectationFailure ("Expected prototype success, got: " ++ show err) >> fail "prototype failed"
        Right report -> pure report

freshRoot :: FilePath -> IO FilePath
freshRoot label = do
    tmp <- getTemporaryDirectory
    let root = tmp </> ("mlf2-uri-r2-c1-prototype-" ++ label)
    removePathForcibly root `catch` swallowMissing
    createDirectoryIfMissing True root
    pure root

attemptDir :: FilePath -> Int -> FilePath
attemptDir root attemptId =
    root </> "orchestrator" </> "rounds" </> "round-016" </> "evidence" </> "P1" </> ("attempt-" ++ show attemptId)

swallowMissing :: IOError -> IO ()
swallowMissing err
    | isDoesNotExistError err = pure ()
    | otherwise = ioError err

allowedRejectionTriggers :: [String]
allowedRejectionTriggers =
    [ "none"
    , "widened-search"
    , "multi-scc"
    , "cross-family"
    , "heuristic-choice"
    , "late-repair"
    , "manufactured-provenance"
    , "surrogate-substitution"
    , "replay-domain-widening"
    , "non-local-salvage"
    , "structural-cycle"
    , "implicit-unfolding"
    , "equi-recursive-reasoning"
    , "termination-weakening"
    , "nondeterministic-output"
    , "inconsistent-trace"
    , "partial-replay"
    , "production-path-dependence"
    , "blocking-stop-condition"
    ]

assertInvocationMetadata :: FilePath -> Int -> FilePath -> Expectation
assertInvocationMetadata root attemptId fileName = do
    content <- readFile (attemptDir root attemptId </> fileName)
    extractStringField "research_entrypoint_id" content `shouldBe` Just researchEntrypointId
    extractStringField "stage_selector" content `shouldBe` Just stageSelectorP1
    extractStringField "scenario_id" content `shouldBe` Just scenarioIdUriR2C1OnlyV1
    extractIntField "attempt_id" content `shouldBe` Just attemptId
    if fileName `elem` ["trace-bundle.json", "stage-verdict.json"]
        then extractStringField "stage" content `shouldBe` Just "P1"
        else pure ()

assertCheckerSchema :: FilePath -> Int -> String -> FilePath -> Expectation
assertCheckerSchema root attemptId subjectId fileName = do
    content <- readFile (attemptDir root attemptId </> fileName)
    extractStringField "subject_id" content `shouldBe` Just subjectId
    extractStringField "evidence_ref" content
        `shouldSatisfy` maybeAttemptEvidenceRef attemptId
    extractStringField "verdict" content `shouldBe` Just "pass"
    extractStringField "rejection_trigger" content `shouldSatisfy` maybeAllowedTrigger

maybeAllowedTrigger :: Maybe String -> Bool
maybeAllowedTrigger = maybe False (`elem` allowedRejectionTriggers)

maybeAttemptEvidenceRef :: Int -> Maybe FilePath -> Bool
maybeAttemptEvidenceRef attemptId =
    maybe False (isInfixOf (attemptEvidenceRelativeDir attemptId))

extractStringField :: String -> String -> Maybe String
extractStringField key text =
    case findAfter ("\"" ++ key ++ "\": \"") text of
        Nothing -> Nothing
        Just rest -> Just (takeWhile (/= '"') rest)

extractIntField :: String -> String -> Maybe Int
extractIntField key text =
    case findAfter ("\"" ++ key ++ "\": ") text of
        Nothing -> Nothing
        Just rest ->
            let digits = takeWhile isDigit rest
            in if null digits then Nothing else Just (read digits)

extractStringValues :: String -> String -> [String]
extractStringValues key = go
  where
    needle = "\"" ++ key ++ "\": \""

    go text =
        case findAfter needle text of
            Nothing -> []
            Just rest ->
                let value = takeWhile (/= '"') rest
                    remaining = drop (length value + 1) rest
                in value : go remaining

findAfter :: String -> String -> Maybe String
findAfter needle = go
  where
    go haystack
        | needle `isInfixOf` haystack =
            case breakOn needle haystack of
                Just (_, rest) -> Just rest
                Nothing -> Nothing
        | otherwise = Nothing

breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go []
  where
    go _ [] = Nothing
    go prefixAcc haystack@(ch : rest) =
        if take (length needle) haystack == needle
            then Just (reverse prefixAcc, drop (length needle) haystack)
            else go (ch : prefixAcc) rest
