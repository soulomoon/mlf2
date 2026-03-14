module Research.UriR2C1PrototypeP1Spec (spec) where

import Control.Exception (catch)
import Data.Char (isDigit)
import Data.List (isInfixOf, sort)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , getTemporaryDirectory
    , listDirectory
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
    , p2AttemptEvidenceRelativeDir
    , prototypeStageResult
    , prototypeSubjectId
    , researchEntrypointId
    , scenarioIdUriR2C1OnlyV1
    , stageSelectorP1
    , stageSelectorP2
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
                    , prStageSelector = "P3-safety-validation"
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 1
                    }
            result `shouldBe` Left (UnsupportedStageSelector "P3-safety-validation")
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

    describe "URI-R2-C1 P2 prototype entrypoint" $ do
        it "allows attempt-2 reruns and records replay diagnostics as bounded non-pass" $ do
            root <- freshRoot "p2-attempt-2-bounded-non-pass"
            _ <- seedAuthoritativeP1Token root
            report <- requirePrototypeReportFor stageSelectorP2 root 2
            files <- listDirectory (attemptDirP2 root 2)
            sort files `shouldBe`
                [ "check-P2-G.json"
                , "check-P2-R.json"
                , "check-P2-S.json"
                , "check-P2-W.json"
                , "stage-verdict.json"
                , "trace-bundle.json"
                ]
            prototypeStageResult report `shouldBe` "semantic-negative"
            doesFileExist (attemptDirP2 root 2 </> "subject-token.json") >>= (`shouldBe` False)
            doesDirectoryExist (attemptDirP2 root 1) >>= (`shouldBe` False)

            checkW <- readFile (attemptDirP2 root 2 </> "check-P2-W.json")
            extractStringField "verdict" checkW `shouldBe` Just "semantic-negative"
            extractStringField "rejection_trigger" checkW `shouldBe` Just "partial-replay"
            checkW `shouldSatisfy` isInfixOf "applyInstantiation diagnostic failed"

            stageVerdict <- readFile (attemptDirP2 root 2 </> "stage-verdict.json")
            extractStringField "subject_token_ref" stageVerdict `shouldBe` Nothing
            extractStringField "stage_result" stageVerdict `shouldBe` Just "semantic-negative"

        it "repeats invocation metadata on attempt-2 and renders a non-pass artifact without handoff" $ do
            root <- freshRoot "p2-attempt-2-artifact"
            seeded <- seedAuthoritativeP1Token root
            _ <- requirePrototypeReportFor stageSelectorP2 root 2
            mapM_ (assertInvocationMetadataFor root attemptDirP2 2 stageSelectorP2 (Just "P2"))
                [ "trace-bundle.json"
                , "check-P2-G.json"
                , "check-P2-S.json"
                , "check-P2-R.json"
                , "check-P2-W.json"
                , "stage-verdict.json"
                ]

            traceBundle <- readFile (attemptDirP2 root 2 </> "trace-bundle.json")
            extractStringField "correlation_id" traceBundle `shouldBe` Just "uri-r2-c1-only-v1-p2-attempt-2"
            extractStringField "subject_id" traceBundle `shouldBe` Just (prototypeSubjectId seeded)
            mapM_
                (\needle -> traceBundle `shouldSatisfy` isInfixOf needle)
                [ "trace://uri-r2-c1/p2/generalize/"
                , "trace://uri-r2-c1/p2/scheme-to-type/"
                , "trace://uri-r2-c1/p2/reify-no-fallback/"
                , "trace://uri-r2-c1/p2/witness-replay/"
                ]

            mapM_
                (assertP2CheckerSchema root 2 (prototypeSubjectId seeded))
                [ "check-P2-G.json"
                , "check-P2-S.json"
                , "check-P2-R.json"
                , "check-P2-W.json"
                ]

            stageVerdict <- readFile (attemptDirP2 root 2 </> "stage-verdict.json")
            extractStringField "subject_token_ref" stageVerdict `shouldBe` Nothing
            extractStringField "stage_result" stageVerdict `shouldBe` Just "semantic-negative"

            inheritedToken <- readFile (attemptDir root 2 </> "subject-token.json")
            extractStringField "subject_id" inheritedToken `shouldBe` Just (prototypeSubjectId seeded)
            extractStringField "subject_id" traceBundle `shouldBe` extractStringField "subject_id" inheritedToken

            artifact <- readFile (root </> "docs" </> "plans" </> "2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md")
            artifact `shouldSatisfy` isInfixOf "Attempt: 2"
            artifact `shouldSatisfy` isInfixOf "attempt_id: 2"
            artifact `shouldSatisfy` isInfixOf "`semantic-negative`"
            artifact `shouldSatisfy` (not . isInfixOf "## Next-Stage Handoff")

runAccepted :: FilePath -> Int -> Expectation
runAccepted root attemptId = do
    result <- runPrototype stageSelectorP1 root attemptId
    case result of
        Left err ->
            expectationFailure ("Expected accepted request, got: " ++ show err)
        Right report ->
            prototypeStageResult report `shouldBe` "pass"

requirePrototypePass :: FilePath -> Int -> IO PrototypeReport
requirePrototypePass = requirePrototypePassFor stageSelectorP1

requirePrototypePassFor :: String -> FilePath -> Int -> IO PrototypeReport
requirePrototypePassFor stageSelector root attemptId = do
    result <- runPrototype stageSelector root attemptId
    case result of
        Left err -> expectationFailure ("Expected prototype success, got: " ++ show err) >> fail "prototype failed"
        Right report -> pure report

requirePrototypeReportFor :: String -> FilePath -> Int -> IO PrototypeReport
requirePrototypeReportFor stageSelector root attemptId = do
    result <- runPrototype stageSelector root attemptId
    case result of
        Left err -> expectationFailure ("Expected prototype execution, got: " ++ show err) >> fail "prototype failed"
        Right report -> pure report

runPrototype :: String -> FilePath -> Int -> IO (Either PrototypeError PrototypeReport)
runPrototype stageSelector root attemptId =
    runResearchPrototype $
        PrototypeRequest
            { prRepoRoot = root
            , prResearchEntrypointId = researchEntrypointId
            , prStageSelector = stageSelector
            , prScenarioId = scenarioIdUriR2C1OnlyV1
            , prAttemptId = attemptId
            }

seedAuthoritativeP1Token :: FilePath -> IO PrototypeReport
seedAuthoritativeP1Token root = requirePrototypePass root 2

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

attemptDirP2 :: FilePath -> Int -> FilePath
attemptDirP2 root attemptId =
    root </> p2AttemptEvidenceRelativeDir attemptId

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
assertInvocationMetadata root attemptId =
    assertInvocationMetadataFor root attemptDir attemptId stageSelectorP1 (Just "P1")

assertInvocationMetadataFor
    :: FilePath
    -> (FilePath -> Int -> FilePath)
    -> Int
    -> String
    -> Maybe String
    -> FilePath
    -> Expectation
assertInvocationMetadataFor root attemptDirFn attemptId stageSelector expectedStage fileName = do
    content <- readFile (attemptDirFn root attemptId </> fileName)
    extractStringField "research_entrypoint_id" content `shouldBe` Just researchEntrypointId
    extractStringField "stage_selector" content `shouldBe` Just stageSelector
    extractStringField "scenario_id" content `shouldBe` Just scenarioIdUriR2C1OnlyV1
    extractIntField "attempt_id" content `shouldBe` Just attemptId
    case (expectedStage, fileName `elem` ["trace-bundle.json", "stage-verdict.json"]) of
        (Just stageName, True) -> extractStringField "stage" content `shouldBe` Just stageName
        _ -> pure ()

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

maybeAllowedVerdict :: Maybe String -> Bool
maybeAllowedVerdict = maybe False (`elem` ["pass", "semantic-negative", "inconclusive"])

maybeAttemptEvidenceRef :: Int -> Maybe FilePath -> Bool
maybeAttemptEvidenceRef attemptId =
    maybe False (isInfixOf (attemptEvidenceRelativeDir attemptId))

maybeAttemptEvidenceRefFor :: FilePath -> Maybe FilePath -> Bool
maybeAttemptEvidenceRefFor expectedPath =
    maybe False (== expectedPath)

assertP2CheckerSchema :: FilePath -> Int -> String -> FilePath -> Expectation
assertP2CheckerSchema root attemptId subjectId fileName = do
    content <- readFile (attemptDirP2 root attemptId </> fileName)
    extractStringField "correlation_id" content
        `shouldBe` Just ("uri-r2-c1-only-v1-p2-attempt-" ++ show attemptId)
    extractStringField "subject_id" content `shouldBe` Just subjectId
    extractStringField "evidence_ref" content
        `shouldSatisfy` maybeAttemptEvidenceRefFor (p2AttemptEvidenceRelativeDir attemptId </> "trace-bundle.json")
    extractStringField "verdict" content `shouldSatisfy` maybeAllowedVerdict
    extractStringField "rejection_trigger" content `shouldSatisfy` maybeAllowedTrigger

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
