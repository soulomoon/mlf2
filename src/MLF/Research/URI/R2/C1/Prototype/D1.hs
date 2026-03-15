module MLF.Research.URI.R2.C1.Prototype.D1 (
    D1CheckArtifact(..),
    D1Execution(..),
    executeD1
) where

import Control.Exception (catch)
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (find, isInfixOf)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , removePathForcibly
    )
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (isDoesNotExistError)

import MLF.Research.URI.R2.C1.Prototype.P2 (P2CheckArtifact(..), P2Execution(..), executeP2)
import MLF.Research.URI.R2.C1.Prototype.Types

data D1CheckArtifact = D1CheckArtifact
    { d1caResult :: CheckResult
    , d1caCorrelationId :: String
    , d1caSubjectId :: String
    , d1caEvidenceRef :: FilePath
    }
    deriving (Eq, Show)

data D1Execution = D1Execution
    { d1AppReport :: PrototypeReport
    , d1InheritedSubjectTokenPath :: FilePath
    , d1InheritedCheckWPath :: FilePath
    , d1InheritedStageVerdictPath :: FilePath
    , d1InheritedTraceBundlePath :: FilePath
    , d1CorrelationId :: String
    , d1TraceRefs :: [String]
    , d1Checks :: [D1CheckArtifact]
    , d1StageResult :: String
    , d1ReplayClassification :: String
    , d1ReplayMismatch :: Maybe String
    , d1AcceptedMismatch :: String
    , d1TraceSummary :: [String]
    }
    deriving (Eq, Show)

data InheritedP2Boundary = InheritedP2Boundary
    { ip2bScenarioId :: String
    , ip2bSubjectId :: String
    , ip2bRejectionTrigger :: String
    , ip2bDetails :: String
    }
    deriving (Eq, Show)

data InheritedP2StageVerdict = InheritedP2StageVerdict
    { ip2svScenarioId :: String
    , ip2svStageResult :: String
    }
    deriving (Eq, Show)

data InheritedP2TraceBundle = InheritedP2TraceBundle
    { ip2tbScenarioId :: String
    , ip2tbSubjectId :: String
    }
    deriving (Eq, Show)

data ReplayObservation = ReplayObservation
    { roClassification :: String
    , roCheckVerdict :: String
    , roRejectionTrigger :: String
    , roDetails :: String
    , roMismatch :: Maybe String
    , roTraceRefs :: [String]
    }
    deriving (Eq, Show)

authoritativeSubjectId :: String
authoritativeSubjectId = "uri-r2-c1/cluster-1"

authoritativeP2Trigger :: String
authoritativeP2Trigger = "partial-replay"

authoritativeMismatch :: String
authoritativeMismatch = "InstBot expects \8869, got: t9 -> t9"

executeD1 :: PrototypeRequest -> IO (Either PrototypeError D1Execution)
executeD1 req = do
    let inheritedSubjectTokenRelPath = p1AuthoritativeSubjectTokenRelativePath
        inheritedCheckWRelPath = p2AuthoritativeCheckWRelativePath
        inheritedStageVerdictRelPath = p2AuthoritativeStageVerdictRelativePath
        inheritedTraceBundleRelPath = p2AuthoritativeTraceBundleRelativePath
        subjectTokenPath = prRepoRoot req </> inheritedSubjectTokenRelPath
        checkWPath = prRepoRoot req </> inheritedCheckWRelPath
        stageVerdictPath = prRepoRoot req </> inheritedStageVerdictRelPath
        traceBundlePath = prRepoRoot req </> inheritedTraceBundleRelPath
    allExist <- and <$> mapM doesFileExist [subjectTokenPath, checkWPath, stageVerdictPath, traceBundlePath]
    if not allExist
        then
            firstMissing
                [ (inheritedSubjectTokenRelPath, subjectTokenPath)
                , (inheritedCheckWRelPath, checkWPath)
                , (inheritedStageVerdictRelPath, stageVerdictPath)
                , (inheritedTraceBundleRelPath, traceBundlePath)
                ]
        else do
            subjectTokenText <- readFile subjectTokenPath
            checkWText <- readFile checkWPath
            stageVerdictText <- readFile stageVerdictPath
            traceBundleText <- readFile traceBundlePath
            case decodeD1Inputs inheritedSubjectTokenRelPath subjectTokenText inheritedCheckWRelPath checkWText inheritedStageVerdictRelPath stageVerdictText inheritedTraceBundleRelPath traceBundleText of
                Left err -> pure (Left err)
                Right (subjectToken, inheritedBoundary, inheritedStageVerdict, inheritedTraceBundle) -> do
                    let paths = prototypePaths req
                    cleanDir (ppEvidenceDir paths)
                    replay <- runBoundedReplayLane req subjectTokenText
                    createDirectoryIfMissing True (ppEvidenceDir paths)
                    let execution =
                            finalizeExecution
                                req
                                inheritedSubjectTokenRelPath
                                inheritedCheckWRelPath
                                inheritedStageVerdictRelPath
                                inheritedTraceBundleRelPath
                                subjectToken
                                inheritedBoundary
                                inheritedStageVerdict
                                inheritedTraceBundle
                                replay
                    writeEvidence paths execution
                    pure (Right execution)
  where
    firstMissing [] = pure (Left (MissingStageInputToken ""))
    firstMissing ((relPath, fullPath) : rest) = do
        exists <- doesFileExist fullPath
        if exists
            then firstMissing rest
            else pure (Left (MissingStageInputToken relPath))

decodeD1Inputs
    :: FilePath
    -> String
    -> FilePath
    -> String
    -> FilePath
    -> String
    -> FilePath
    -> String
    -> Either PrototypeError (SubjectToken, InheritedP2Boundary, InheritedP2StageVerdict, InheritedP2TraceBundle)
decodeD1Inputs
    subjectTokenPath
    subjectTokenText
    checkWPath
    checkWText
    stageVerdictPath
    stageVerdictText
    traceBundlePath
    traceBundleText = do
        subjectToken <- decodeSubjectToken subjectTokenPath subjectTokenText
        boundary <- decodeInheritedP2Boundary checkWPath checkWText
        verdict <- decodeInheritedP2StageVerdict stageVerdictPath stageVerdictText
        traceBundle <- decodeInheritedP2TraceBundle traceBundlePath traceBundleText
        pure (subjectToken, boundary, verdict, traceBundle)

runBoundedReplayLane :: PrototypeRequest -> String -> IO ReplayObservation
runBoundedReplayLane req subjectTokenText = do
    let attemptId = prAttemptId req
        sandboxRoot =
            prRepoRoot req
                </> d1AttemptEvidenceRelativeDir attemptId
                </> ".bounded-replay-sandbox"
        sandboxTokenPath = sandboxRoot </> p1AuthoritativeSubjectTokenRelativePath
        sandboxRequest =
            PrototypeRequest
                { prRepoRoot = sandboxRoot
                , prResearchEntrypointId = researchEntrypointId
                , prStageSelector = stageSelectorP2
                , prScenarioId = scenarioIdUriR2C1OnlyV1
                , prAttemptId = 1
                }
    cleanDir sandboxRoot
    createDirectoryIfMissing True (takeDirectory sandboxTokenPath)
    writeFile sandboxTokenPath subjectTokenText
    p2Result <- executeP2 sandboxRequest
    cleanDir sandboxRoot
    pure (toReplayObservation p2Result)

toReplayObservation :: Either PrototypeError P2Execution -> ReplayObservation
toReplayObservation result = case result of
    Left err ->
        ReplayObservation
            { roClassification = "bounded-unable-to-reproduce"
            , roCheckVerdict = "inconclusive"
            , roRejectionTrigger = "blocking-stop-condition"
            , roDetails = "bounded replay lane failed before witness replay: " ++ show err
            , roMismatch = Nothing
            , roTraceRefs = []
            }
    Right execution ->
        let checkW =
                find
                    (\check -> ckCheckId (p2caResult check) == "P2-W")
                    (p2Checks execution)
            rewrittenTraceRefs = map rewriteReplayTraceRef (p2TraceRefs execution)
        in case checkW of
            Nothing ->
                ReplayObservation
                    { roClassification = "bounded-unable-to-reproduce"
                    , roCheckVerdict = "inconclusive"
                    , roRejectionTrigger = "blocking-stop-condition"
                    , roDetails = "bounded replay lane did not emit P2-W witness replay diagnostics"
                    , roMismatch = Nothing
                    , roTraceRefs = rewrittenTraceRefs
                    }
            Just artifact ->
                let resultW = p2caResult artifact
                    details = ckDetails resultW
                    trigger = ckRejectionTrigger resultW
                    verdict = ckStatus resultW
                    mismatch = extractObservedMismatch details
                    classification
                        | verdict == "semantic-negative"
                            && trigger == authoritativeP2Trigger
                            && detailsHasAuthoritativeMismatch details =
                                "exact-bounded-replay-failure"
                        | verdict == "pass" || verdict == "semantic-negative" =
                            "bounded-diagnostic-drift"
                        | otherwise =
                            "bounded-unable-to-reproduce"
                in ReplayObservation
                    { roClassification = classification
                    , roCheckVerdict =
                        case classification of
                            "exact-bounded-replay-failure" -> "pass"
                            "bounded-diagnostic-drift" -> "semantic-negative"
                            _ -> "inconclusive"
                    , roRejectionTrigger =
                        case classification of
                            "exact-bounded-replay-failure" -> authoritativeP2Trigger
                            "bounded-diagnostic-drift" ->
                                if trigger == "none" then "inconsistent-trace" else trigger
                            _ -> "blocking-stop-condition"
                    , roDetails = details
                    , roMismatch = mismatch
                    , roTraceRefs = rewrittenTraceRefs
                    }

rewriteReplayTraceRef :: String -> String
rewriteReplayTraceRef ref =
    case stripPrefix "trace://uri-r2-c1/p2/" ref of
        Just rest -> "trace://uri-r2-c1/d1/replay/" ++ rest
        Nothing ->
            "trace://uri-r2-c1/d1/replay/external/" ++ slugify ref

finalizeExecution
    :: PrototypeRequest
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> SubjectToken
    -> InheritedP2Boundary
    -> InheritedP2StageVerdict
    -> InheritedP2TraceBundle
    -> ReplayObservation
    -> D1Execution
finalizeExecution
    req
    subjectTokenPath
    checkWPath
    stageVerdictPath
    traceBundlePath
    subjectToken
    inheritedBoundary
    inheritedStageVerdict
    inheritedTraceBundle
    replay =
        let correlationId = mkCorrelationId req
            subjectId = stSubjectId subjectToken
            traceBundleRef = d1TraceBundleRelativePath (prAttemptId req)
            continuityIssues = continuityMismatches subjectToken inheritedBoundary inheritedStageVerdict inheritedTraceBundle
            checkI =
                mkCheckArtifact
                    subjectId
                    traceBundleRef
                    correlationId
                    "D1-I"
                    (if null continuityIssues then "pass" else "semantic-negative")
                    (if null continuityIssues then "none" else "inconsistent-trace")
                    (if null continuityIssues then continuityPassDetail else continuityFailDetail continuityIssues)
            checkR =
                mkCheckArtifact
                    subjectId
                    traceBundleRef
                    correlationId
                    "D1-R"
                    (roCheckVerdict replay)
                    (roRejectionTrigger replay)
                    (renderReplayDetail replay)
            checkM =
                mkCheckArtifact
                    subjectId
                    traceBundleRef
                    correlationId
                    "D1-M"
                    (mismatchVerdict continuityIssues replay)
                    (mismatchTrigger continuityIssues replay)
                    (renderMismatchDetail continuityIssues inheritedBoundary replay)
            checks = [checkI, checkR, checkM]
            stageResult = stageResultFromStatuses (map (ckStatus . d1caResult) checks)
            summary =
                [ "D1-I consumed only authoritative inherited inputs from round-016/P1 attempt-2 and round-017/P2 attempt-2."
                , "D1-R reran the bounded replay lane (`generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback -> witness replay`) via the root-cause entrypoint."
                , "D1-M compared the new replay outcome against the accepted P2-W boundary (`partial-replay`, `InstBot expects \8869, got: t9 -> t9`)."
                , "D1 replay classification: " ++ roClassification replay ++ "."
                ]
                    ++ map (\check -> ckCheckId (d1caResult check) ++ ": " ++ ckDetails (d1caResult check)) checks
            report =
                PrototypeReport
                    { prototypeRequest = req
                    , prototypeCandidates = []
                    , prototypeP1C = d1caResult checkI
                    , prototypeP1N = d1caResult checkR
                    , prototypeP1U = d1caResult checkM
                    , prototypeStageResult = stageResult
                    , prototypeSubjectToken = Nothing
                    , prototypeTraceSummary = summary
                    , prototypeRawObservationCount = length checks
                    }
        in D1Execution
            { d1AppReport = report
            , d1InheritedSubjectTokenPath = subjectTokenPath
            , d1InheritedCheckWPath = checkWPath
            , d1InheritedStageVerdictPath = stageVerdictPath
            , d1InheritedTraceBundlePath = traceBundlePath
            , d1CorrelationId = correlationId
            , d1TraceRefs = roTraceRefs replay
            , d1Checks = checks
            , d1StageResult = stageResult
            , d1ReplayClassification = roClassification replay
            , d1ReplayMismatch = roMismatch replay
            , d1AcceptedMismatch = authoritativeMismatch
            , d1TraceSummary = summary
            }
  where
    continuityPassDetail =
        "Inherited continuity confirmed: subject_id uri-r2-c1/cluster-1, scenario uri-r2-c1-only-v1, P2-W trigger partial-replay, mismatch InstBot expects \\8869, got: t9 -> t9."

    continuityFailDetail issues =
        "Inherited continuity mismatch: " ++ joinWith "; " issues

mismatchVerdict :: [String] -> ReplayObservation -> String
mismatchVerdict continuityIssues replay
    | not (null continuityIssues) = "semantic-negative"
    | roClassification replay == "exact-bounded-replay-failure" = "pass"
    | roClassification replay == "bounded-diagnostic-drift" = "semantic-negative"
    | otherwise = "inconclusive"

mismatchTrigger :: [String] -> ReplayObservation -> String
mismatchTrigger continuityIssues replay
    | not (null continuityIssues) = "inconsistent-trace"
    | roClassification replay == "exact-bounded-replay-failure" = "none"
    | roClassification replay == "bounded-diagnostic-drift" =
        if roRejectionTrigger replay == "none" then "inconsistent-trace" else roRejectionTrigger replay
    | otherwise = "blocking-stop-condition"

renderReplayDetail :: ReplayObservation -> String
renderReplayDetail replay =
    "classification="
        ++ roClassification replay
        ++ "; witness replay verdict="
        ++ roCheckVerdict replay
        ++ "; trigger="
        ++ roRejectionTrigger replay
        ++ "; details="
        ++ roDetails replay

renderMismatchDetail :: [String] -> InheritedP2Boundary -> ReplayObservation -> String
renderMismatchDetail continuityIssues inheritedBoundary replay =
    "target trigger="
        ++ ip2bRejectionTrigger inheritedBoundary
        ++ "; target mismatch="
        ++ authoritativeMismatch
        ++ "; observed trigger="
        ++ roRejectionTrigger replay
        ++ "; observed mismatch="
        ++ maybe "none" id (roMismatch replay)
        ++ "; continuity="
        ++ (if null continuityIssues then "ok" else joinWith "|" continuityIssues)

continuityMismatches
    :: SubjectToken
    -> InheritedP2Boundary
    -> InheritedP2StageVerdict
    -> InheritedP2TraceBundle
    -> [String]
continuityMismatches token boundary stageVerdict traceBundle =
    concat
        [ [ "subject_id drifted from authoritative uri-r2-c1/cluster-1"
          | stSubjectId token /= authoritativeSubjectId
          ]
        , [ "subject scope scenario must remain uri-r2-c1-only-v1"
          | ssScenarioId (stSubjectScope token) /= scenarioIdUriR2C1OnlyV1
          ]
        , [ "subject scope bounded_subject must remain URI-R2-C1"
          | ssBoundedSubject (stSubjectScope token) /= boundedSubjectId
          ]
        , [ "inherited P2-W trigger must remain partial-replay"
          | ip2bRejectionTrigger boundary /= authoritativeP2Trigger
          ]
        , [ "inherited P2-W mismatch must include InstBot expects \\8869, got: t9 -> t9"
          | not (detailsHasAuthoritativeMismatch (ip2bDetails boundary))
          ]
        , [ "inherited P2-W scenario must remain uri-r2-c1-only-v1"
          | ip2bScenarioId boundary /= scenarioIdUriR2C1OnlyV1
          ]
        , [ "inherited P2-W subject_id must match authoritative subject"
          | ip2bSubjectId boundary /= authoritativeSubjectId
          ]
        , [ "inherited P2 stage result must remain semantic-negative"
          | ip2svStageResult stageVerdict /= "semantic-negative"
          ]
        , [ "inherited P2 stage verdict scenario must remain uri-r2-c1-only-v1"
          | ip2svScenarioId stageVerdict /= scenarioIdUriR2C1OnlyV1
          ]
        , [ "inherited P2 trace bundle scenario must remain uri-r2-c1-only-v1"
          | ip2tbScenarioId traceBundle /= scenarioIdUriR2C1OnlyV1
          ]
        , [ "inherited P2 trace bundle subject_id must remain uri-r2-c1/cluster-1"
          | ip2tbSubjectId traceBundle /= authoritativeSubjectId
          ]
        ]

detailsHasAuthoritativeMismatch :: String -> Bool
detailsHasAuthoritativeMismatch details =
    "InstBot expects \8869, got: t9 -> t9" `isInfixOf` details
        || "InstBot expects \\8869, got: t9 -> t9" `isInfixOf` details

extractObservedMismatch :: String -> Maybe String
extractObservedMismatch details
    | detailsHasAuthoritativeMismatch details = Just authoritativeMismatch
    | "InstBot expects" `isInfixOf` details && ", got:" `isInfixOf` details =
        Just (extractBetween "InstBot expects" ")" details)
    | otherwise = Nothing

extractBetween :: String -> String -> String -> String
extractBetween startNeedle endNeedle text =
    case findAfter startNeedle text of
        Nothing -> ""
        Just rest ->
            let candidate = startNeedle ++ takeWhileNotNeedle endNeedle rest
            in trim candidate

takeWhileNotNeedle :: String -> String -> String
takeWhileNotNeedle _ [] = []
takeWhileNotNeedle needle input
    | needle `isPrefixOf` input = []
takeWhileNotNeedle needle (ch : rest) = ch : takeWhileNotNeedle needle rest

isPrefixOf :: String -> String -> Bool
isPrefixOf prefix haystack = take (length prefix) haystack == prefix

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

mkCorrelationId :: PrototypeRequest -> String
mkCorrelationId req =
    prScenarioId req ++ "-d1-attempt-" ++ show (prAttemptId req)

mkCheckArtifact
    :: String
    -> FilePath
    -> String
    -> String
    -> String
    -> String
    -> String
    -> D1CheckArtifact
mkCheckArtifact subjectId evidenceRef correlationId checkId status rejectionTrigger details =
    D1CheckArtifact
        { d1caResult =
            CheckResult
                { ckCheckId = checkId
                , ckStatus = status
                , ckRejectionTrigger = rejectionTrigger
                , ckDetails = details
                }
        , d1caCorrelationId = correlationId
        , d1caSubjectId = subjectId
        , d1caEvidenceRef = evidenceRef
        }

stageResultFromStatuses :: [String] -> String
stageResultFromStatuses statuses
    | any (== "semantic-negative") statuses = "semantic-negative"
    | any (== "inconclusive") statuses = "inconclusive"
    | otherwise = "pass"

metadataFor :: PrototypeRequest -> Bool -> EvidenceMetadata
metadataFor req includeStage =
    EvidenceMetadata
        { emResearchEntrypointId = prResearchEntrypointId req
        , emStageSelector = prStageSelector req
        , emScenarioId = prScenarioId req
        , emAttemptId = prAttemptId req
        , emStage = if includeStage then Just "D1" else Nothing
        }

writeEvidence :: PrototypePaths -> D1Execution -> IO ()
writeEvidence paths execution = do
    let req = prototypeRequest (d1AppReport execution)
        attemptId = prAttemptId req
    writeJsonFile
        (ppEvidenceDir paths </> "trace-bundle.json")
        (traceBundleJson req execution)
    mapM_
        (\check ->
            writeJsonFile
                (ppEvidenceDir paths </> ("check-" ++ ckCheckId (d1caResult check) ++ ".json"))
                (d1CheckJson req check)
        )
        (d1Checks execution)
    writeJsonFile
        (ppEvidenceDir paths </> "stage-verdict.json")
        (stageVerdictJson req execution attemptId)

traceBundleJson :: PrototypeRequest -> D1Execution -> JValue
traceBundleJson req execution =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("correlation_id", JString (d1CorrelationId execution))
               , ("subject_id", JString authoritativeSubjectId)
               , ("trace_refs", JArray (map JString (d1TraceRefs execution)))
               , ("trace_summary", JArray (map JString (d1TraceSummary execution)))
               , ("replay_classification", JString (d1ReplayClassification execution))
               ]
        )

d1CheckJson :: PrototypeRequest -> D1CheckArtifact -> JValue
d1CheckJson req check =
    let result = d1caResult check
    in JObject
        ( metadataPairs (metadataFor req False)
            ++ [ ("check_id", JString (ckCheckId result))
               , ("correlation_id", JString (d1caCorrelationId check))
               , ("subject_id", JString (d1caSubjectId check))
               , ("evidence_ref", JString (d1caEvidenceRef check))
               , ("verdict", JString (ckStatus result))
               , ("rejection_trigger", JString (ckRejectionTrigger result))
               , ("details", JString (ckDetails result))
               ]
        )

stageVerdictJson :: PrototypeRequest -> D1Execution -> Int -> JValue
stageVerdictJson req execution attemptId =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("subject_token_ref", JNull)
               , ("checker_results", JArray (map (JString . d1CheckResultRelativePath attemptId . ckCheckId . d1caResult) (d1Checks execution)))
               , ("stage_result", JString (d1StageResult execution))
               , ("terminal_reason", JString (stageTerminalReason (d1StageResult execution)))
               ]
        )

stageTerminalReason :: String -> String
stageTerminalReason stageResult
    | stageResult == "inconclusive" = "blocking-stop-condition"
    | otherwise = "none"

decodeInheritedP2Boundary :: FilePath -> String -> Either PrototypeError InheritedP2Boundary
decodeInheritedP2Boundary sourcePath contents = do
    scenarioId <- requiredStringField sourcePath "scenario_id" contents
    subjectId <- requiredStringField sourcePath "subject_id" contents
    rejectionTrigger <- requiredStringField sourcePath "rejection_trigger" contents
    details <- requiredStringField sourcePath "details" contents
    pure
        InheritedP2Boundary
            { ip2bScenarioId = scenarioId
            , ip2bSubjectId = subjectId
            , ip2bRejectionTrigger = rejectionTrigger
            , ip2bDetails = details
            }

decodeInheritedP2StageVerdict :: FilePath -> String -> Either PrototypeError InheritedP2StageVerdict
decodeInheritedP2StageVerdict sourcePath contents = do
    scenarioId <- requiredStringField sourcePath "scenario_id" contents
    stageResult <- requiredStringField sourcePath "stage_result" contents
    pure
        InheritedP2StageVerdict
            { ip2svScenarioId = scenarioId
            , ip2svStageResult = stageResult
            }

decodeInheritedP2TraceBundle :: FilePath -> String -> Either PrototypeError InheritedP2TraceBundle
decodeInheritedP2TraceBundle sourcePath contents = do
    scenarioId <- requiredStringField sourcePath "scenario_id" contents
    subjectId <- requiredStringField sourcePath "subject_id" contents
    pure
        InheritedP2TraceBundle
            { ip2tbScenarioId = scenarioId
            , ip2tbSubjectId = subjectId
            }

decodeSubjectToken :: FilePath -> String -> Either PrototypeError SubjectToken
decodeSubjectToken tokenPath contents = do
    subjectId <- requiredStringField tokenPath "subject_id" contents
    subjectKindText <- requiredStringField tokenPath "subject_kind" contents
    subjectKind <- parseCandidateKind tokenPath subjectKindText
    scenarioId <- requiredStringField tokenPath "scenario_id" contents
    boundedSubject <- requiredStringField tokenPath "bounded_subject" contents
    originStage <- requiredStringField tokenPath "origin_stage" contents
    candidateId <- requiredStringField tokenPath "candidate_id" contents
    candidateInventoryRef <- requiredStringField tokenPath "candidate_inventory_ref" contents
    normalizationBasis <- requiredStringField tokenPath "normalization_basis" contents
    discoveryTraceRef <- optionalStringField "discovery_trace_ref" contents
    ownerKind <- requiredStringField tokenPath "kind" contents
    familyId <- optionalStringField "family_id" contents
    traceHandles <- requiredStringArrayField tokenPath "trace_handles" contents
    pure
        SubjectToken
            { stSubjectId = subjectId
            , stSubjectKind = subjectKind
            , stSubjectScope =
                SubjectScope
                    { ssScenarioId = scenarioId
                    , ssBoundedSubject = boundedSubject
                    }
            , stProvenanceAnchor =
                ProvenanceAnchor
                    { paOriginStage = originStage
                    , paCandidateId = candidateId
                    , paCandidateInventoryRef = candidateInventoryRef
                    , paNormalizationBasis = normalizationBasis
                    , paDiscoveryTraceRef = discoveryTraceRef
                    }
            , stOwnerFamilyStatus =
                OwnerFamilyStatus
                    { ofsKind = ownerKind
                    , ofsFamilyId = familyId
                    }
            , stTraceHandles = traceHandles
            }

parseCandidateKind :: FilePath -> String -> Either PrototypeError CandidateKind
parseCandidateKind tokenPath raw = case raw of
    "local-root" -> Right CandidateLocalRoot
    "equivalent-local-cluster" -> Right CandidateEquivalentLocalCluster
    _ -> Left (MalformedStageInputToken tokenPath ("unsupported subject_kind: " ++ raw))

requiredStringField :: FilePath -> String -> String -> Either PrototypeError String
requiredStringField tokenPath fieldName contents =
    case extractStringField fieldName contents of
        Just value -> Right value
        Nothing -> Left (MalformedStageInputToken tokenPath ("missing string field: " ++ fieldName))

optionalStringField :: String -> String -> Either PrototypeError (Maybe String)
optionalStringField fieldName contents =
    case findAfter ("\"" ++ fieldName ++ "\": ") contents of
        Nothing -> Right Nothing
        Just rest ->
            case dropWhile isSpace rest of
                ('n' : 'u' : 'l' : 'l' : _) -> Right Nothing
                ('"' : xs) -> Right (Just (takeWhile (/= '"') xs))
                _ -> Right Nothing

requiredStringArrayField :: FilePath -> String -> String -> Either PrototypeError [String]
requiredStringArrayField tokenPath fieldName contents =
    case extractStringArrayField fieldName contents of
        Just values -> Right values
        Nothing -> Left (MalformedStageInputToken tokenPath ("missing string array field: " ++ fieldName))

extractStringField :: String -> String -> Maybe String
extractStringField key text =
    case findAfter ("\"" ++ key ++ "\": \"") text of
        Nothing -> Nothing
        Just rest -> Just (parseJsonString rest)

parseJsonString :: String -> String
parseJsonString = go []
  where
    go acc [] = reverse acc
    go acc ('"' : _) = reverse acc
    go acc ('\\' : '"' : rest) = go ('"' : acc) rest
    go acc ('\\' : '\\' : rest) = go ('\\' : acc) rest
    go acc ('\\' : 'n' : rest) = go ('\n' : acc) rest
    go acc ('\\' : 'r' : rest) = go ('\r' : acc) rest
    go acc ('\\' : 't' : rest) = go ('\t' : acc) rest
    go acc ('\\' : ch : rest) = go (ch : acc) rest
    go acc (ch : rest) = go (ch : acc) rest

extractStringArrayField :: String -> String -> Maybe [String]
extractStringArrayField key contents =
    case findAfter ("\"" ++ key ++ "\": [") contents of
        Nothing -> Nothing
        Just rest -> Just (go rest)
  where
    go txt =
        case dropWhile (\ch -> isSpace ch || ch == ',') txt of
            (']' : _) -> []
            ('"' : more) ->
                let value = takeWhile (/= '"') more
                    afterValue = drop (length value + 1) more
                in value : go afterValue
            _ -> []

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

cleanDir :: FilePath -> IO ()
cleanDir path = do
    exists <- doesDirectoryExist path
    if exists
        then removePathForcibly path `catch` swallowMissing
        else pure ()

swallowMissing :: IOError -> IO ()
swallowMissing err
    | isDoesNotExistError err = pure ()
    | otherwise = ioError err

data JValue
    = JObject [(String, JValue)]
    | JArray [JValue]
    | JString String
    | JNumber Int
    | JNull

writeJsonFile :: FilePath -> JValue -> IO ()
writeJsonFile path value = writeFile path (renderJValue 0 value ++ "\n")

metadataPairs :: EvidenceMetadata -> [(String, JValue)]
metadataPairs metadata =
    [ ("research_entrypoint_id", JString (emResearchEntrypointId metadata))
    , ("stage_selector", JString (emStageSelector metadata))
    , ("scenario_id", JString (emScenarioId metadata))
    , ("attempt_id", JNumber (emAttemptId metadata))
    ]
        ++ case emStage metadata of
            Nothing -> []
            Just stageName -> [("stage", JString stageName)]

renderJValue :: Int -> JValue -> String
renderJValue indentLevel value = case value of
    JObject fields -> renderObject indentLevel fields
    JArray values -> renderArray indentLevel values
    JString txt -> string txt
    JNumber n -> show n
    JNull -> "null"

renderObject :: Int -> [(String, JValue)] -> String
renderObject indentLevel fields =
    case fields of
        [] -> "{}"
        _ ->
            let innerIndent = replicate (indentLevel + 2) ' '
                closingIndent = replicate indentLevel ' '
                renderField (key, value) =
                    innerIndent ++ string key ++ ": " ++ renderJValue (indentLevel + 2) value
            in "{\n"
                ++ joinWith ",\n" (map renderField fields)
                ++ "\n"
                ++ closingIndent
                ++ "}"

renderArray :: Int -> [JValue] -> String
renderArray indentLevel values =
    case values of
        [] -> "[]"
        _ ->
            let innerIndent = replicate (indentLevel + 2) ' '
                closingIndent = replicate indentLevel ' '
                renderValue value = innerIndent ++ renderJValue (indentLevel + 2) value
            in "[\n"
                ++ joinWith ",\n" (map renderValue values)
                ++ "\n"
                ++ closingIndent
                ++ "]"

string :: String -> String
string value = "\"" ++ concatMap escapeChar value ++ "\""

escapeChar :: Char -> String
escapeChar ch = case ch of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> [ch]

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [value] = value
joinWith sep (value : rest) = value ++ sep ++ joinWith sep rest

stripPrefix :: String -> String -> Maybe String
stripPrefix prefix value
    | prefix `isPrefixOf` value = Just (drop (length prefix) value)
    | otherwise = Nothing

slugify :: String -> String
slugify =
    trimHyphen
        . take 80
        . collapseHyphen
        . map normalizeChar
  where
    normalizeChar ch
        | isAlphaNum ch = toLower ch
        | otherwise = '-'

    collapseHyphen [] = []
    collapseHyphen ('-' : '-' : rest) = collapseHyphen ('-' : rest)
    collapseHyphen (ch : rest) = ch : collapseHyphen rest

    trimHyphen = reverse . dropWhile (== '-') . reverse . dropWhile (== '-')
