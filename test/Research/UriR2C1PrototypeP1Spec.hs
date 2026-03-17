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
    , d1AttemptEvidenceRelativeDir
    , d2AttemptEvidenceRelativeDir
    , d3AttemptEvidenceRelativeDir
    , p2AttemptEvidenceRelativeDir
    , p3AttemptEvidenceRelativeDir
    , p4AttemptEvidenceRelativeDir
    , prototypeStageResult
    , prototypeSubjectId
    , replayRootCauseEntrypointId
    , researchEntrypointId
    , scenarioIdUriR2C1OnlyV1
    , stageSelectorD1
    , stageSelectorD2
    , stageSelectorD3
    , stageSelectorP1
    , stageSelectorP2
    , stageSelectorP3
    , stageSelectorP4
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
                    , prStageSelector = "wrong-stage"
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 1
                    }
            result `shouldBe` Left (UnsupportedStageSelector "wrong-stage")
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
        it "allows attempt-2 reruns and records live replay widening as bounded non-pass" $ do
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
            extractStringField "rejection_trigger" checkW `shouldBe` Just "replay-domain-widening"
            checkW `shouldSatisfy` isInfixOf "witness replay produced t9 -> t9 but reification preserved t5 -> t5"

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

    describe "URI-R2-C1 P3 prototype entrypoint" $ do
        it "runs P3 from authoritative P2 no-token continuity and emits bounded non-pass evidence" $ do
            root <- freshRoot "p3-bounded-non-pass"
            seedAuthoritativeP2ReviewRecord root
            report <- requirePrototypeReportFor stageSelectorP3 root 1
            files <- listDirectory (attemptDirP3 root 1)
            sort files `shouldBe`
                [ "check-P3-A.json"
                , "check-P3-B.json"
                , "check-P3-C.json"
                , "check-P3-S.json"
                , "stage-verdict.json"
                , "trace-bundle.json"
                ]
            prototypeStageResult report `shouldBe` "semantic-negative"
            doesFileExist (attemptDirP3 root 1 </> "subject-token.json") >>= (`shouldBe` False)

            traceBundle <- readFile (attemptDirP3 root 1 </> "trace-bundle.json")
            extractStringField "correlation_id" traceBundle `shouldBe` Just "uri-r2-c1-only-v1-p3-attempt-1"
            traceBundle `shouldSatisfy` isInfixOf "\"subject_id\": null"

            mapM_
                (assertP3CheckerSchema root 1 Nothing)
                [ "check-P3-S.json"
                , "check-P3-A.json"
                , "check-P3-B.json"
                , "check-P3-C.json"
                ]

            checkS <- readFile (attemptDirP3 root 1 </> "check-P3-S.json")
            extractStringField "verdict" checkS `shouldBe` Just "semantic-negative"
            extractStringField "rejection_trigger" checkS `shouldBe` Just "blocking-stop-condition"

            stageVerdict <- readFile (attemptDirP3 root 1 </> "stage-verdict.json")
            extractStringField "subject_token_ref" stageVerdict `shouldBe` Nothing
            extractStringField "stage_result" stageVerdict `shouldBe` Just "semantic-negative"
            extractStringField "terminal_reason" stageVerdict `shouldBe` Just "blocking-stop-condition"

        it "rejects wrong-scenario P3 runs without writing attempt-local evidence" $ do
            root <- freshRoot "p3-wrong-scenario"
            seedAuthoritativeP2ReviewRecord root
            result <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = researchEntrypointId
                    , prStageSelector = stageSelectorP3
                    , prScenarioId = "wrong-scenario"
                    , prAttemptId = 1
                    }
            result `shouldBe` Left (UnsupportedScenario "wrong-scenario")
            doesDirectoryExist (attemptDirP3 root 1) >>= (`shouldBe` False)

    describe "URI-R2-C1 P4 prototype entrypoint" $ do
        it "runs P4 from authoritative P1/P2/P3 review records and emits hard-stop evidence" $ do
            root <- freshRoot "p4-hard-stop"
            seedAuthoritativeP1ReviewRecord root
            seedAuthoritativeP2ReviewRecord root
            seedAuthoritativeP3ReviewRecord root
            report <- requirePrototypeReportFor stageSelectorP4 root 1
            files <- listDirectory (attemptDirP4 root 1)
            sort files `shouldBe`
                [ "decision-verdict.json"
                , "stage-consumption.json"
                , "trace-bundle.json"
                ]
            prototypeStageResult report `shouldBe` "hard-stop"

            mapM_ (assertInvocationMetadataFor root attemptDirP4 1 stageSelectorP4 (Just "P4"))
                [ "trace-bundle.json"
                , "stage-consumption.json"
                , "decision-verdict.json"
                ]

            decision <- readFile (attemptDirP4 root 1 </> "decision-verdict.json")
            extractStringField "final_decision" decision `shouldBe` Just "hard-stop"
            extractStringField "terminal_reason" decision `shouldBe` Just "blocking-stop-condition"
            decision `shouldSatisfy` isInfixOf "\"P1\": \"pass\""
            decision `shouldSatisfy` isInfixOf "\"P2\": \"semantic-negative\""
            decision `shouldSatisfy` isInfixOf "\"P3\": \"semantic-negative\""

            stageConsumption <- readFile (attemptDirP4 root 1 </> "stage-consumption.json")
            stageConsumption `shouldSatisfy` isInfixOf "\"stage\": \"P1\""
            stageConsumption `shouldSatisfy` isInfixOf "\"stage\": \"P2\""
            stageConsumption `shouldSatisfy` isInfixOf "\"stage\": \"P3\""
            stageConsumption `shouldSatisfy` isInfixOf "\"authoritative_attempt\": 2"

        it "rejects wrong-scenario P4 runs without writing attempt-local evidence" $ do
            root <- freshRoot "p4-wrong-scenario"
            seedAuthoritativeP3ReviewRecord root
            result <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = researchEntrypointId
                    , prStageSelector = stageSelectorP4
                    , prScenarioId = "wrong-scenario"
                    , prAttemptId = 1
                    }
            result `shouldBe` Left (UnsupportedScenario "wrong-scenario")
            doesDirectoryExist (attemptDirP4 root 1) >>= (`shouldBe` False)

    describe "URI-R2-C1 D1 replay root-cause entrypoint" $ do
        it "runs D1 attempt-1 via the root-cause tuple and records continuity drift against the historical replay boundary" $ do
            root <- freshRoot "d1-attempt-1-pass"
            _ <- seedAuthoritativeReplayBoundary root
            report <- requireD1Report root 1
            files <- listDirectory (attemptDirD1 root 1)
            sort files `shouldBe`
                [ "check-D1-I.json"
                , "check-D1-M.json"
                , "check-D1-R.json"
                , "stage-verdict.json"
                , "trace-bundle.json"
                ]
            prototypeStageResult report `shouldBe` "semantic-negative"
            doesFileExist (attemptDirD1 root 1 </> "subject-token.json") >>= (`shouldBe` False)

            mapM_ (assertInvocationMetadataForEntrypoint root attemptDirD1 1 replayRootCauseEntrypointId stageSelectorD1 (Just "D1"))
                [ "trace-bundle.json"
                , "check-D1-I.json"
                , "check-D1-R.json"
                , "check-D1-M.json"
                , "stage-verdict.json"
                ]

            checkI <- readFile (attemptDirD1 root 1 </> "check-D1-I.json")
            extractStringField "verdict" checkI `shouldBe` Just "semantic-negative"
            extractStringField "rejection_trigger" checkI `shouldBe` Just "inconsistent-trace"
            checkI `shouldSatisfy` isInfixOf "inherited P2-W trigger must remain partial-replay"
            checkI `shouldSatisfy` isInfixOf "InstBot expects"

            checkR <- readFile (attemptDirD1 root 1 </> "check-D1-R.json")
            extractStringField "verdict" checkR `shouldBe` Just "semantic-negative"
            extractStringField "rejection_trigger" checkR `shouldBe` Just "replay-domain-widening"
            checkR `shouldSatisfy` isInfixOf "classification=bounded-diagnostic-drift"
            checkR `shouldSatisfy` isInfixOf "witness replay produced t9 -> t9 but reification preserved t5 -> t5"

            checkM <- readFile (attemptDirD1 root 1 </> "check-D1-M.json")
            extractStringField "verdict" checkM `shouldBe` Just "semantic-negative"
            checkM `shouldSatisfy` isInfixOf "target trigger=replay-domain-widening"
            checkM `shouldSatisfy` isInfixOf "InstBot expects"
            checkM `shouldSatisfy` isInfixOf "continuity=inherited P2-W trigger must remain partial-replay"

            stageVerdict <- readFile (attemptDirD1 root 1 </> "stage-verdict.json")
            extractStringField "subject_token_ref" stageVerdict `shouldBe` Nothing
            extractStringField "stage_result" stageVerdict `shouldBe` Just "semantic-negative"
            extractStringField "terminal_reason" stageVerdict `shouldBe` Just "none"

            artifact <- readFile (root </> "docs" </> "plans" </> "2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md")
            artifact `shouldSatisfy` isInfixOf "Stage: `D1`"
            artifact `shouldSatisfy` isInfixOf "Attempt: 1"
            artifact `shouldSatisfy` isInfixOf "`D1-I`"
            artifact `shouldSatisfy` isInfixOf "`D1-R`"
            artifact `shouldSatisfy` isInfixOf "`D1-M`"

        it "rejects wrong scenario, stage, entrypoint, and out-of-range attempt before writing D1 evidence" $ do
            root <- freshRoot "d1-invalid-tuple"
            _ <- seedAuthoritativeReplayBoundary root

            wrongScenario <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = replayRootCauseEntrypointId
                    , prStageSelector = stageSelectorD1
                    , prScenarioId = "wrong-scenario"
                    , prAttemptId = 1
                    }
            wrongScenario `shouldBe` Left (UnsupportedScenario "wrong-scenario")

            wrongStage <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = replayRootCauseEntrypointId
                    , prStageSelector = "wrong-stage"
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 1
                    }
            wrongStage `shouldBe` Left (UnsupportedStageSelector "wrong-stage")

            wrongEntrypoint <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = "wrong-entrypoint"
                    , prStageSelector = stageSelectorD1
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 1
                    }
            wrongEntrypoint `shouldBe` Left (UnsupportedResearchEntrypoint "wrong-entrypoint")

            wrongAttempt <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = replayRootCauseEntrypointId
                    , prStageSelector = stageSelectorD1
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 0
                    }
            wrongAttempt `shouldBe` Left (UnsupportedAttemptId 0)

            doesDirectoryExist (attemptDirD1 root 1) >>= (`shouldBe` False)

        it "does not mutate existing D1 attempt-1 evidence on rejected reruns" $ do
            root <- freshRoot "d1-rejected-no-mutation"
            _ <- seedAuthoritativeReplayBoundary root
            _ <- requireD1Report root 1
            baselineStageVerdict <- readFile (attemptDirD1 root 1 </> "stage-verdict.json")
            baselineFiles <- fmap sort (listDirectory (attemptDirD1 root 1))

            _ <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = replayRootCauseEntrypointId
                    , prStageSelector = "wrong-stage"
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 1
                    }
            _ <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = replayRootCauseEntrypointId
                    , prStageSelector = stageSelectorD1
                    , prScenarioId = "wrong-scenario"
                    , prAttemptId = 1
                    }

            afterStageVerdict <- readFile (attemptDirD1 root 1 </> "stage-verdict.json")
            afterFiles <- fmap sort (listDirectory (attemptDirD1 root 1))
            afterStageVerdict `shouldBe` baselineStageVerdict
            afterFiles `shouldBe` baselineFiles

    describe "URI-R2-C1 D2 replay root-cause entrypoint" $ do
        it "runs D2 attempt-1 via the root-cause tuple and reports continuity-blocked localization on live reruns" $ do
            root <- freshRoot "d2-attempt-1-pass"
            _ <- seedAuthoritativeReplayBoundary root
            _ <- requireD1Report root 1
            seedAuthoritativeD1ReviewRecord root
            report <- requireD2Report root 1
            files <- listDirectory (attemptDirD2 root 1)
            sort files `shouldBe`
                [ "check-D2-L.json"
                , "check-D2-O.json"
                , "check-D2-T.json"
                , "stage-verdict.json"
                , "trace-bundle.json"
                ]
            prototypeStageResult report `shouldBe` "semantic-negative"
            doesFileExist (attemptDirD2 root 1 </> "subject-token.json") >>= (`shouldBe` False)

            mapM_ (assertInvocationMetadataForEntrypoint root attemptDirD2 1 replayRootCauseEntrypointId stageSelectorD2 (Just "D2"))
                [ "trace-bundle.json"
                , "check-D2-T.json"
                , "check-D2-L.json"
                , "check-D2-O.json"
                , "stage-verdict.json"
                ]
            historicalD1 <- readFile (root </> "orchestrator" </> "rounds" </> "round-020" </> "review-record.json")
            extractStringField "stage_result" historicalD1 `shouldBe` Just "pass"
            extractStringField "authoritative_result" historicalD1 `shouldBe` Just "pass"

            checkT <- readFile (attemptDirD2 root 1 </> "check-D2-T.json")
            extractStringField "verdict" checkT `shouldBe` Just "semantic-negative"
            extractStringField "rejection_trigger" checkT `shouldBe` Just "inconsistent-trace"
            checkT `shouldSatisfy` isInfixOf "P2-W trigger must remain partial-replay"
            checkT `shouldSatisfy` isInfixOf "D1 stage verdict must remain pass"

            checkL <- readFile (attemptDirD2 root 1 </> "check-D2-L.json")
            extractStringField "verdict" checkL `shouldBe` Just "semantic-negative"
            extractStringField "rejection_trigger" checkL `shouldBe` Just "inconsistent-trace"
            checkL `shouldSatisfy` isInfixOf "cannot localize divergence while continuity mismatches remain"

            checkO <- readFile (attemptDirD2 root 1 </> "check-D2-O.json")
            extractStringField "verdict" checkO `shouldBe` Just "semantic-negative"
            extractStringField "rejection_trigger" checkO `shouldBe` Just "inconsistent-trace"
            checkO `shouldSatisfy` isInfixOf "cannot assign single owner while continuity mismatches remain"

            stageVerdict <- readFile (attemptDirD2 root 1 </> "stage-verdict.json")
            extractStringField "subject_token_ref" stageVerdict `shouldBe` Nothing
            extractStringField "stage_result" stageVerdict `shouldBe` Just "semantic-negative"
            extractStringField "terminal_reason" stageVerdict `shouldBe` Just "none"

            traceBundle <- readFile (attemptDirD2 root 1 </> "trace-bundle.json")
            extractStringField "divergence_boundary" traceBundle
                `shouldBe` Just "unresolved-continuity-blocked"
            extractStringField "owner_account" traceBundle
                `shouldBe` Just "unresolved-owner-continuity-blocked"

            artifact <- readFile (root </> "docs" </> "plans" </> "2026-03-16-uri-r2-c1-d2-replay-mismatch-localization.md")
            artifact `shouldSatisfy` isInfixOf "Stage: `D2`"
            artifact `shouldSatisfy` isInfixOf "Attempt: 1"
            artifact `shouldSatisfy` isInfixOf "`D2-T`"
            artifact `shouldSatisfy` isInfixOf "`D2-L`"
            artifact `shouldSatisfy` isInfixOf "`D2-O`"

        it "rejects wrong scenario, stage, entrypoint, and out-of-range attempt before writing D2 evidence" $ do
            root <- freshRoot "d2-invalid-tuple"
            _ <- seedAuthoritativeReplayBoundary root
            _ <- requireD1Report root 1
            seedAuthoritativeD1ReviewRecord root

            wrongScenario <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = replayRootCauseEntrypointId
                    , prStageSelector = stageSelectorD2
                    , prScenarioId = "wrong-scenario"
                    , prAttemptId = 1
                    }
            wrongScenario `shouldBe` Left (UnsupportedScenario "wrong-scenario")

            wrongStage <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = replayRootCauseEntrypointId
                    , prStageSelector = "wrong-stage"
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 1
                    }
            wrongStage `shouldBe` Left (UnsupportedStageSelector "wrong-stage")

            wrongEntrypoint <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = "wrong-entrypoint"
                    , prStageSelector = stageSelectorD2
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 1
                    }
            wrongEntrypoint `shouldBe` Left (UnsupportedResearchEntrypoint "wrong-entrypoint")

            wrongAttempt <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = replayRootCauseEntrypointId
                    , prStageSelector = stageSelectorD2
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 0
                    }
            wrongAttempt `shouldBe` Left (UnsupportedAttemptId 0)

            doesDirectoryExist (attemptDirD2 root 1) >>= (`shouldBe` False)

    describe "URI-R2-C1 D3 replay root-cause entrypoint" $ do
        it "runs D3 attempt-1 via the root-cause tuple and reports bounded-negative continuity drift on live reruns" $ do
            root <- freshRoot "d3-attempt-1-pass"
            _ <- seedAuthoritativeReplayBoundary root
            _ <- requireD1Report root 1
            seedAuthoritativeD1ReviewRecord root
            _ <- requireD2Report root 1
            seedAuthoritativeD2ReviewRecord root
            report <- requireD3Report root 1
            files <- listDirectory (attemptDirD3 root 1)
            sort files `shouldBe`
                [ "check-D3-B.json"
                , "check-D3-H.json"
                , "check-D3-V.json"
                , "stage-verdict.json"
                , "trace-bundle.json"
                ]
            prototypeStageResult report `shouldBe` "semantic-negative"
            doesFileExist (attemptDirD3 root 1 </> "subject-token.json") >>= (`shouldBe` False)

            mapM_ (assertInvocationMetadataForEntrypoint root attemptDirD3 1 replayRootCauseEntrypointId stageSelectorD3 (Just "D3"))
                [ "trace-bundle.json"
                , "check-D3-H.json"
                , "check-D3-B.json"
                , "check-D3-V.json"
                , "stage-verdict.json"
                ]
            historicalD2 <- readFile (root </> "orchestrator" </> "rounds" </> "round-021" </> "review-record.json")
            extractStringField "stage_result" historicalD2 `shouldBe` Just "pass"
            extractStringField "authoritative_result" historicalD2 `shouldBe` Just "pass"

            checkH <- readFile (attemptDirD3 root 1 </> "check-D3-H.json")
            extractStringField "verdict" checkH `shouldBe` Just "semantic-negative"
            extractStringField "rejection_trigger" checkH `shouldBe` Just "inconsistent-trace"
            checkH `shouldSatisfy` isInfixOf "continuity mismatch blocks D3-H"
            checkH `shouldSatisfy` isInfixOf "D2-L verdict must remain pass"

            checkB <- readFile (attemptDirD3 root 1 </> "check-D3-B.json")
            extractStringField "verdict" checkB `shouldBe` Just "semantic-negative"
            extractStringField "rejection_trigger" checkB `shouldBe` Just "inconsistent-trace"
            checkB `shouldSatisfy` isInfixOf "continuity mismatch blocks D3-B"

            checkV <- readFile (attemptDirD3 root 1 </> "check-D3-V.json")
            extractStringField "verdict" checkV `shouldBe` Just "semantic-negative"
            extractStringField "rejection_trigger" checkV `shouldBe` Just "inconsistent-trace"
            checkV `shouldSatisfy` isInfixOf "bounded-negative"

            stageVerdict <- readFile (attemptDirD3 root 1 </> "stage-verdict.json")
            extractStringField "subject_token_ref" stageVerdict `shouldBe` Nothing
            extractStringField "attempt_verdict" stageVerdict `shouldBe` Just "bounded-negative"
            extractStringField "stage_result" stageVerdict `shouldBe` Just "semantic-negative"
            extractStringField "terminal_reason" stageVerdict `shouldBe` Just "none"

            traceBundle <- readFile (attemptDirD3 root 1 </> "trace-bundle.json")
            extractStringField "attempt_verdict" traceBundle `shouldBe` Just "bounded-negative"
            extractStringField "fix_hypothesis" traceBundle
                `shouldSatisfy` maybe False (isInfixOf "H1:")
            extractStringField "divergence_boundary" traceBundle
                `shouldBe` Just "witness-replay/applyInstantiation-instbot-precondition"
            extractStringField "owner_account" traceBundle
                `shouldSatisfy` maybe False (isInfixOf "MLF.Elab.Inst.applyInstantiation")
            traceBundle `shouldSatisfy` isInfixOf "no second executable interface"

            artifact <- readFile (root </> "docs" </> "plans" </> "2026-03-16-uri-r2-c1-d3-bounded-fixability-probe.md")
            artifact `shouldSatisfy` isInfixOf "Stage: `D3`"
            artifact `shouldSatisfy` isInfixOf "Attempt: 1"
            artifact `shouldSatisfy` isInfixOf "`D3-H`"
            artifact `shouldSatisfy` isInfixOf "`D3-B`"
            artifact `shouldSatisfy` isInfixOf "`D3-V`"
            artifact `shouldSatisfy` isInfixOf "bounded-negative"

        it "rejects wrong scenario, stage, entrypoint, and out-of-range attempt before writing D3 evidence" $ do
            root <- freshRoot "d3-invalid-tuple"
            _ <- seedAuthoritativeReplayBoundary root
            _ <- requireD1Report root 1
            seedAuthoritativeD1ReviewRecord root
            _ <- requireD2Report root 1
            seedAuthoritativeD2ReviewRecord root

            wrongScenario <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = replayRootCauseEntrypointId
                    , prStageSelector = stageSelectorD3
                    , prScenarioId = "wrong-scenario"
                    , prAttemptId = 1
                    }
            wrongScenario `shouldBe` Left (UnsupportedScenario "wrong-scenario")

            wrongStage <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = replayRootCauseEntrypointId
                    , prStageSelector = "wrong-stage"
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 1
                    }
            wrongStage `shouldBe` Left (UnsupportedStageSelector "wrong-stage")

            wrongEntrypoint <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = "wrong-entrypoint"
                    , prStageSelector = stageSelectorD3
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 1
                    }
            wrongEntrypoint `shouldBe` Left (UnsupportedResearchEntrypoint "wrong-entrypoint")

            wrongAttempt <- runResearchPrototype $
                PrototypeRequest
                    { prRepoRoot = root
                    , prResearchEntrypointId = replayRootCauseEntrypointId
                    , prStageSelector = stageSelectorD3
                    , prScenarioId = scenarioIdUriR2C1OnlyV1
                    , prAttemptId = 0
                    }
            wrongAttempt `shouldBe` Left (UnsupportedAttemptId 0)

            doesDirectoryExist (attemptDirD3 root 1) >>= (`shouldBe` False)

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

requireD1Report :: FilePath -> Int -> IO PrototypeReport
requireD1Report root attemptId = do
    result <- runD1 root attemptId
    case result of
        Left err -> expectationFailure ("Expected D1 execution, got: " ++ show err) >> fail "d1 failed"
        Right report -> pure report

requireD2Report :: FilePath -> Int -> IO PrototypeReport
requireD2Report root attemptId = do
    result <- runD2 root attemptId
    case result of
        Left err -> expectationFailure ("Expected D2 execution, got: " ++ show err) >> fail "d2 failed"
        Right report -> pure report

requireD3Report :: FilePath -> Int -> IO PrototypeReport
requireD3Report root attemptId = do
    result <- runD3 root attemptId
    case result of
        Left err -> expectationFailure ("Expected D3 execution, got: " ++ show err) >> fail "d3 failed"
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

runD1 :: FilePath -> Int -> IO (Either PrototypeError PrototypeReport)
runD1 root attemptId =
    runResearchPrototype $
        PrototypeRequest
            { prRepoRoot = root
            , prResearchEntrypointId = replayRootCauseEntrypointId
            , prStageSelector = stageSelectorD1
            , prScenarioId = scenarioIdUriR2C1OnlyV1
            , prAttemptId = attemptId
            }

runD2 :: FilePath -> Int -> IO (Either PrototypeError PrototypeReport)
runD2 root attemptId =
    runResearchPrototype $
        PrototypeRequest
            { prRepoRoot = root
            , prResearchEntrypointId = replayRootCauseEntrypointId
            , prStageSelector = stageSelectorD2
            , prScenarioId = scenarioIdUriR2C1OnlyV1
            , prAttemptId = attemptId
            }

runD3 :: FilePath -> Int -> IO (Either PrototypeError PrototypeReport)
runD3 root attemptId =
    runResearchPrototype $
        PrototypeRequest
            { prRepoRoot = root
            , prResearchEntrypointId = replayRootCauseEntrypointId
            , prStageSelector = stageSelectorD3
            , prScenarioId = scenarioIdUriR2C1OnlyV1
            , prAttemptId = attemptId
            }

seedAuthoritativeP1Token :: FilePath -> IO PrototypeReport
seedAuthoritativeP1Token root = requirePrototypePass root 2

seedAuthoritativeReplayBoundary :: FilePath -> IO PrototypeReport
seedAuthoritativeReplayBoundary root = do
    _ <- seedAuthoritativeP1Token root
    requirePrototypeReportFor stageSelectorP2 root 2

seedAuthoritativeD1ReviewRecord :: FilePath -> IO ()
seedAuthoritativeD1ReviewRecord root = do
    let reviewRecordPath =
            root
                </> "orchestrator"
                </> "rounds"
                </> "round-020"
                </> "review-record.json"
        reviewRecordContent =
            unlines
                [ "{"
                , "  \"research_entrypoint_id\": \"uri-r2-c1-p2-replay-root-cause-v1\","
                , "  \"scenario_id\": \"uri-r2-c1-only-v1\","
                , "  \"stage_id\": \"D1\","
                , "  \"attempt\": 1,"
                , "  \"attempt_verdict\": \"accepted\","
                , "  \"stage_result\": \"pass\","
                , "  \"stage_action\": \"finalize\","
                , "  \"retry_reason\": \"none\","
                , "  \"fix_hypothesis\": \"none\","
                , "  \"status\": \"authoritative\","
                , "  \"authoritative_attempt\": 1,"
                , "  \"authoritative_result\": \"pass\","
                , "  \"artifact_path\": \"docs/plans/2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md\","
                , "  \"evidence_dir\": \"orchestrator/rounds/round-020/evidence/D1/attempt-1/\","
                , "  \"terminal_reason\": \"none\""
                , "}"
                ]
    createDirectoryIfMissing True (root </> "orchestrator" </> "rounds" </> "round-020")
    writeFile reviewRecordPath reviewRecordContent

seedAuthoritativeD2ReviewRecord :: FilePath -> IO ()
seedAuthoritativeD2ReviewRecord root = do
    let reviewRecordPath =
            root
                </> "orchestrator"
                </> "rounds"
                </> "round-021"
                </> "review-record.json"
        reviewRecordContent =
            unlines
                [ "{"
                , "  \"research_entrypoint_id\": \"uri-r2-c1-p2-replay-root-cause-v1\","
                , "  \"scenario_id\": \"uri-r2-c1-only-v1\","
                , "  \"stage_id\": \"D2\","
                , "  \"attempt\": 1,"
                , "  \"attempt_verdict\": \"accepted\","
                , "  \"stage_result\": \"pass\","
                , "  \"stage_action\": \"finalize\","
                , "  \"retry_reason\": \"none\","
                , "  \"fix_hypothesis\": \"none\","
                , "  \"status\": \"authoritative\","
                , "  \"authoritative_attempt\": 1,"
                , "  \"authoritative_result\": \"pass\","
                , "  \"artifact_path\": \"docs/plans/2026-03-16-uri-r2-c1-d2-replay-mismatch-localization.md\","
                , "  \"evidence_dir\": \"orchestrator/rounds/round-021/evidence/D2/attempt-1/\","
                , "  \"terminal_reason\": \"none\""
                , "}"
                ]
    createDirectoryIfMissing True (root </> "orchestrator" </> "rounds" </> "round-021")
    writeFile reviewRecordPath reviewRecordContent

seedAuthoritativeP1ReviewRecord :: FilePath -> IO ()
seedAuthoritativeP1ReviewRecord root = do
    let reviewRecordPath =
            root
                </> "orchestrator"
                </> "rounds"
                </> "round-016"
                </> "review-record.json"
        reviewRecordContent =
            unlines
                [ "{"
                , "  \"research_entrypoint_id\": \"uri-r2-c1-prototype-entrypoint-v1\","
                , "  \"scenario_id\": \"uri-r2-c1-only-v1\","
                , "  \"stages\": {"
                , "    \"P1\": {"
                , "      \"status\": \"authoritative\","
                , "      \"authoritative_attempt\": 2,"
                , "      \"authoritative_result\": \"pass\","
                , "      \"terminal_reason\": \"none\""
                , "    },"
                , "    \"P2\": {"
                , "      \"status\": \"not-yet-run\","
                , "      \"authoritative_attempt\": null,"
                , "      \"authoritative_result\": null,"
                , "      \"terminal_reason\": \"none\""
                , "    },"
                , "    \"P3\": {"
                , "      \"status\": \"not-yet-run\","
                , "      \"authoritative_attempt\": null,"
                , "      \"authoritative_result\": null,"
                , "      \"terminal_reason\": \"none\""
                , "    },"
                , "    \"P4\": {"
                , "      \"status\": \"not-yet-run\","
                , "      \"authoritative_attempt\": null,"
                , "      \"authoritative_result\": null,"
                , "      \"consumed_stage_results\": null,"
                , "      \"terminal_reason\": \"none\""
                , "    }"
                , "  }"
                , "}"
                ]
    createDirectoryIfMissing True (root </> "orchestrator" </> "rounds" </> "round-016")
    writeFile reviewRecordPath reviewRecordContent

seedAuthoritativeP2ReviewRecord :: FilePath -> IO ()
seedAuthoritativeP2ReviewRecord root = do
    let reviewRecordPath =
            root
                </> "orchestrator"
                </> "rounds"
                </> "round-017"
                </> "review-record.json"
        reviewRecordContent =
            unlines
                [ "{"
                , "  \"research_entrypoint_id\": \"uri-r2-c1-prototype-entrypoint-v1\","
                , "  \"scenario_id\": \"uri-r2-c1-only-v1\","
                , "  \"stages\": {"
                , "    \"P1\": {"
                , "      \"status\": \"authoritative\","
                , "      \"authoritative_attempt\": 2,"
                , "      \"authoritative_result\": \"pass\""
                , "    },"
                , "    \"P2\": {"
                , "      \"status\": \"authoritative\","
                , "      \"authoritative_attempt\": 2,"
                , "      \"authoritative_result\": \"semantic-negative\""
                , "    },"
                , "    \"P3\": {"
                , "      \"status\": \"not-yet-run\","
                , "      \"authoritative_attempt\": null,"
                , "      \"authoritative_result\": null"
                , "    }"
                , "  }"
                , "}"
                ]
    createDirectoryIfMissing True (root </> "orchestrator" </> "rounds" </> "round-017")
    writeFile reviewRecordPath reviewRecordContent

seedAuthoritativeP3ReviewRecord :: FilePath -> IO ()
seedAuthoritativeP3ReviewRecord root = do
    let reviewRecordPath =
            root
                </> "orchestrator"
                </> "rounds"
                </> "round-018"
                </> "review-record.json"
        reviewRecordContent =
            unlines
                [ "{"
                , "  \"research_entrypoint_id\": \"uri-r2-c1-prototype-entrypoint-v1\","
                , "  \"scenario_id\": \"uri-r2-c1-only-v1\","
                , "  \"stages\": {"
                , "    \"P1\": {"
                , "      \"status\": \"authoritative\","
                , "      \"authoritative_attempt\": 2,"
                , "      \"authoritative_result\": \"pass\","
                , "      \"terminal_reason\": \"none\""
                , "    },"
                , "    \"P2\": {"
                , "      \"status\": \"authoritative\","
                , "      \"authoritative_attempt\": 2,"
                , "      \"authoritative_result\": \"semantic-negative\","
                , "      \"terminal_reason\": \"none\""
                , "    },"
                , "    \"P3\": {"
                , "      \"status\": \"authoritative\","
                , "      \"authoritative_attempt\": 2,"
                , "      \"authoritative_result\": \"semantic-negative\","
                , "      \"terminal_reason\": \"none\""
                , "    },"
                , "    \"P4\": {"
                , "      \"status\": \"not-yet-run\","
                , "      \"authoritative_attempt\": null,"
                , "      \"authoritative_result\": null,"
                , "      \"consumed_stage_results\": null,"
                , "      \"terminal_reason\": \"none\""
                , "    }"
                , "  }"
                , "}"
                ]
    createDirectoryIfMissing True (root </> "orchestrator" </> "rounds" </> "round-018")
    writeFile reviewRecordPath reviewRecordContent

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

attemptDirP3 :: FilePath -> Int -> FilePath
attemptDirP3 root attemptId =
    root </> p3AttemptEvidenceRelativeDir attemptId

attemptDirP4 :: FilePath -> Int -> FilePath
attemptDirP4 root attemptId =
    root </> p4AttemptEvidenceRelativeDir attemptId

attemptDirD1 :: FilePath -> Int -> FilePath
attemptDirD1 root attemptId =
    root </> d1AttemptEvidenceRelativeDir attemptId

attemptDirD2 :: FilePath -> Int -> FilePath
attemptDirD2 root attemptId =
    root </> d2AttemptEvidenceRelativeDir attemptId

attemptDirD3 :: FilePath -> Int -> FilePath
attemptDirD3 root attemptId =
    root </> d3AttemptEvidenceRelativeDir attemptId

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

assertInvocationMetadataForEntrypoint
    :: FilePath
    -> (FilePath -> Int -> FilePath)
    -> Int
    -> String
    -> String
    -> Maybe String
    -> FilePath
    -> Expectation
assertInvocationMetadataForEntrypoint root attemptDirFn attemptId expectedEntrypoint stageSelector expectedStage fileName = do
    content <- readFile (attemptDirFn root attemptId </> fileName)
    extractStringField "research_entrypoint_id" content `shouldBe` Just expectedEntrypoint
    extractStringField "stage_selector" content `shouldBe` Just stageSelector
    extractStringField "scenario_id" content `shouldBe` Just scenarioIdUriR2C1OnlyV1
    extractIntField "attempt_id" content `shouldBe` Just attemptId
    case (expectedStage, fileName `elem` ["trace-bundle.json", "stage-verdict.json"]) of
        (Just stageName, True) -> extractStringField "stage" content `shouldBe` Just stageName
        _ -> pure ()

assertInvocationMetadataFor
    :: FilePath
    -> (FilePath -> Int -> FilePath)
    -> Int
    -> String
    -> Maybe String
    -> FilePath
    -> Expectation
assertInvocationMetadataFor root attemptDirFn attemptId stageSelector expectedStage fileName = do
    assertInvocationMetadataForEntrypoint
        root
        attemptDirFn
        attemptId
        researchEntrypointId
        stageSelector
        expectedStage
        fileName

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

assertP3CheckerSchema :: FilePath -> Int -> Maybe String -> FilePath -> Expectation
assertP3CheckerSchema root attemptId mSubjectId fileName = do
    content <- readFile (attemptDirP3 root attemptId </> fileName)
    extractStringField "correlation_id" content
        `shouldBe` Just ("uri-r2-c1-only-v1-p3-attempt-" ++ show attemptId)
    case mSubjectId of
        Nothing -> content `shouldSatisfy` isInfixOf "\"subject_id\": null"
        Just subjectId -> extractStringField "subject_id" content `shouldBe` Just subjectId
    extractStringField "evidence_ref" content
        `shouldSatisfy` maybeAttemptEvidenceRefFor (p3AttemptEvidenceRelativeDir attemptId </> "trace-bundle.json")
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
