module MLF.Research.URI.R2.C1.Prototype.P3 (
    P3CheckArtifact(..),
    P3Execution(..),
    executeP3
) where

import Control.Exception (catch)
import Data.Char (isSpace)
import Data.List (isInfixOf)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , removePathForcibly
    )
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

import MLF.Research.URI.R2.C1.Prototype.Types

data P3CheckArtifact = P3CheckArtifact
    { p3caResult :: CheckResult
    , p3caCorrelationId :: String
    , p3caSubjectId :: Maybe String
    , p3caEvidenceRef :: FilePath
    }
    deriving (Eq, Show)

data P3Execution = P3Execution
    { p3AppReport :: PrototypeReport
    , p3P2TokenPath :: FilePath
    , p3P2TokenPresent :: Bool
    , p3SubjectId :: Maybe String
    , p3ReviewRecordPath :: FilePath
    , p3CorrelationId :: String
    , p3TraceRefs :: [String]
    , p3Checks :: [P3CheckArtifact]
    , p3StageResult :: String
    , p3OutputToken :: Maybe SubjectToken
    , p3TraceSummary :: [String]
    }
    deriving (Eq, Show)

executeP3 :: PrototypeRequest -> IO (Either PrototypeError P3Execution)
executeP3 req = do
    let tokenRelativePath = p2SubjectTokenRelativePath 2
        tokenPath = prRepoRoot req </> tokenRelativePath
        reviewRecordRelativePath = p2AuthoritativeReviewRecordRelativePath
        reviewRecordPath = prRepoRoot req </> reviewRecordRelativePath
    reviewExists <- doesFileExist reviewRecordPath
    if not reviewExists
        then pure (Left (MissingStageInputToken reviewRecordRelativePath))
        else do
            reviewText <- readFile reviewRecordPath
            case validateP2ReviewRecord reviewRecordRelativePath reviewText of
                Left err -> pure (Left err)
                Right () -> do
                    tokenExists <- doesFileExist tokenPath
                    if tokenExists
                        then do
                            tokenText <- readFile tokenPath
                            case decodeSubjectToken tokenRelativePath tokenText of
                                Left err -> pure (Left err)
                                Right _ ->
                                    pure
                                        ( Left
                                            ( MalformedStageInputToken
                                                reviewRecordRelativePath
                                                "Authoritative P2 semantic-negative must not emit a P2->P3 handoff token."
                                            )
                                        )
                        else do
                            let paths = prototypePaths req
                            cleanEvidenceDir (ppEvidenceDir paths)
                            let execution =
                                    runP3
                                        req
                                        tokenRelativePath
                                        tokenExists
                                        reviewRecordRelativePath
                            createDirectoryIfMissing True (ppEvidenceDir paths)
                            writeEvidence paths execution
                            pure (Right execution)

runP3 :: PrototypeRequest -> FilePath -> Bool -> FilePath -> P3Execution
runP3 req tokenPath tokenPresent reviewRecordPath =
    finalizeExecution req tokenPath tokenPresent subjectId reviewRecordPath correlationId traceRefs checks
  where
    correlationId = mkCorrelationId req
    subjectId = Nothing
    traceBundleRef = p3TraceBundleRelativePath (prAttemptId req)
    subjectTraceRef = maybe "no-p2-token-handoff" id subjectId
    traceRefs =
        [ traceRef "precondition" subjectTraceRef "authoritative-p2-semantic-negative-no-token"
        , traceRef "scc-check" subjectTraceRef "blocked-by-p2-non-pass"
        , traceRef "acyclicity-check" subjectTraceRef "blocked-by-p2-non-pass"
        , traceRef "ownership-check" subjectTraceRef "blocked-by-p2-non-pass"
        , traceRef "constructor-check" subjectTraceRef "blocked-by-p2-non-pass"
        ]
    checks =
        [ mkCheckArtifact subjectId traceBundleRef correlationId "P3-S" "semantic-negative" "blocking-stop-condition"
            "Local obligation-SCC check is blocked because authoritative P2 is semantic-negative and emits no handoff token for P3."
        , mkCheckArtifact subjectId traceBundleRef correlationId "P3-A" "inconclusive" "blocking-stop-condition"
            "Structural-acyclicity check was not executed because P3 is blocked by authoritative P2 non-pass and no-token continuity."
        , mkCheckArtifact subjectId traceBundleRef correlationId "P3-B" "inconclusive" "blocking-stop-condition"
            "Single-family ownership check was not executed because P3 is blocked by authoritative P2 non-pass and no-token continuity."
        , mkCheckArtifact subjectId traceBundleRef correlationId "P3-C" "inconclusive" "blocking-stop-condition"
            "Constructor-directed reasoning check was not executed because P3 is blocked by authoritative P2 non-pass and no-token continuity."
        ]

validateP2ReviewRecord :: FilePath -> String -> Either PrototypeError ()
validateP2ReviewRecord reviewPath reviewRecordText =
    case p2Section reviewRecordText of
        Nothing ->
            Left (MalformedStageInputToken reviewPath "missing P2 stage section in review-record.json")
        Just section
            | "\"status\": \"authoritative\"" `isInfixOf` section
                && "\"authoritative_attempt\": 2" `isInfixOf` section
                && "\"authoritative_result\": \"semantic-negative\"" `isInfixOf` section ->
                    Right ()
            | otherwise ->
                Left
                    ( MalformedStageInputToken
                        reviewPath
                        "P2 must be authoritative attempt 2 with authoritative_result semantic-negative for bounded P3 safety validation."
                    )

mkCorrelationId :: PrototypeRequest -> String
mkCorrelationId req =
    prScenarioId req ++ "-p3-attempt-" ++ show (prAttemptId req)

mkCheckArtifact
    :: Maybe String
    -> FilePath
    -> String
    -> String
    -> String
    -> String
    -> String
    -> P3CheckArtifact
mkCheckArtifact subjectId evidenceRef correlationId checkId status rejectionTrigger details =
    P3CheckArtifact
        { p3caResult =
            CheckResult
                { ckCheckId = checkId
                , ckStatus = status
                , ckRejectionTrigger = rejectionTrigger
                , ckDetails = details
                }
        , p3caCorrelationId = correlationId
        , p3caSubjectId = subjectId
        , p3caEvidenceRef = evidenceRef
        }

finalizeExecution
    :: PrototypeRequest
    -> FilePath
    -> Bool
    -> Maybe String
    -> FilePath
    -> String
    -> [String]
    -> [P3CheckArtifact]
    -> P3Execution
finalizeExecution req tokenPath tokenPresent subjectId reviewRecordPath correlationId traceRefs checks =
    let stageResult = stageResultFromStatuses (map (ckStatus . p3caResult) checks)
        outputToken = Nothing
        p2TokenObservation =
            if tokenPresent
                then "Unexpected: a P2 token file was present despite authoritative semantic-negative P2."
                else "Observed no P2->P3 handoff token, as required by authoritative semantic-negative P2."
        summary =
            [ "P3 consumed only authoritative P2 handoff continuity for stage input."
            , p2TokenObservation
            , "No widened search, no surrogate substitution, and no production-path behavior changes were introduced."
            , "All attempt-local check files use a shared correlation_id and a null subject_id because no P2 handoff token exists."
            ]
                ++ map (\check -> ckCheckId (p3caResult check) ++ ": " ++ ckDetails (p3caResult check)) checks
        report =
            PrototypeReport
                { prototypeRequest = req
                , prototypeCandidates = []
                , prototypeP1C = reportCheckAt 0
                , prototypeP1N = reportCheckAt 1
                , prototypeP1U = reportCheckAt 3
                , prototypeStageResult = stageResult
                , prototypeSubjectToken = outputToken
                , prototypeTraceSummary = summary
                , prototypeRawObservationCount = length checks
                }
    in P3Execution
        { p3AppReport = report
        , p3P2TokenPath = tokenPath
        , p3P2TokenPresent = tokenPresent
        , p3SubjectId = subjectId
        , p3ReviewRecordPath = reviewRecordPath
        , p3CorrelationId = correlationId
        , p3TraceRefs = traceRefs
        , p3Checks = checks
        , p3StageResult = stageResult
        , p3OutputToken = outputToken
        , p3TraceSummary = summary
        }
  where
    reportCheckAt ix =
        case drop ix checks of
            (check : _) -> p3caResult check
            [] ->
                CheckResult
                    { ckCheckId = "P3-missing"
                    , ckStatus = "inconclusive"
                    , ckRejectionTrigger = "blocking-stop-condition"
                    , ckDetails = "Missing P3 check artifact."
                    }

stageResultFromStatuses :: [String] -> String
stageResultFromStatuses statuses
    | any (== "semantic-negative") statuses = "semantic-negative"
    | any (== "inconclusive") statuses = "inconclusive"
    | otherwise = "pass"

traceRef :: String -> String -> String -> String
traceRef operation subjectId detail =
    "trace://uri-r2-c1/p3/"
        ++ operation
        ++ "/"
        ++ slugify subjectId
        ++ "/"
        ++ slugify detail

slugify :: String -> String
slugify =
    trimHyphen
        . take 80
        . collapseHyphen
        . map normalizeChar
  where
    normalizeChar ch
        | 'A' <= ch && ch <= 'Z' = toEnum (fromEnum ch + 32)
        | 'a' <= ch && ch <= 'z' = ch
        | '0' <= ch && ch <= '9' = ch
        | otherwise = '-'

    collapseHyphen [] = []
    collapseHyphen ('-' : '-' : rest) = collapseHyphen ('-' : rest)
    collapseHyphen (ch : rest) = ch : collapseHyphen rest

    trimHyphen = reverse . dropWhile (== '-') . reverse . dropWhile (== '-')

cleanEvidenceDir :: FilePath -> IO ()
cleanEvidenceDir path = do
    exists <- doesDirectoryExist path
    if exists
        then removePathForcibly path `catch` swallowMissing
        else pure ()

swallowMissing :: IOError -> IO ()
swallowMissing err
    | isDoesNotExistError err = pure ()
    | otherwise = ioError err

metadataFor :: PrototypeRequest -> Bool -> EvidenceMetadata
metadataFor req includeStage =
    EvidenceMetadata
        { emResearchEntrypointId = prResearchEntrypointId req
        , emStageSelector = prStageSelector req
        , emScenarioId = prScenarioId req
        , emAttemptId = prAttemptId req
        , emStage = if includeStage then Just "P3" else Nothing
        }

writeEvidence :: PrototypePaths -> P3Execution -> IO ()
writeEvidence paths execution = do
    let req = prototypeRequest (p3AppReport execution)
        attemptId = prAttemptId req
    writeJsonFile
        (ppEvidenceDir paths </> "trace-bundle.json")
        (traceBundleJson req execution)
    mapM_
        (\check ->
            writeJsonFile
                (ppEvidenceDir paths </> ("check-" ++ ckCheckId (p3caResult check) ++ ".json"))
                (p3CheckJson req check)
        )
        (p3Checks execution)
    writeJsonFile
        (ppEvidenceDir paths </> "stage-verdict.json")
        (stageVerdictJson req execution attemptId)
    case p3OutputToken execution of
        Nothing -> pure ()
        Just token ->
            writeJsonFile
                (ppEvidenceDir paths </> "subject-token.json")
                (subjectTokenJson req token)

traceBundleJson :: PrototypeRequest -> P3Execution -> JValue
traceBundleJson req execution =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("correlation_id", JString (p3CorrelationId execution))
               , ("subject_id", maybe JNull JString (p3SubjectId execution))
               , ("trace_refs", JArray (map JString (p3TraceRefs execution)))
               , ("trace_summary", JArray (map JString (p3TraceSummary execution)))
               ]
        )

p3CheckJson :: PrototypeRequest -> P3CheckArtifact -> JValue
p3CheckJson req check =
    let result = p3caResult check
    in JObject
        ( metadataPairs (metadataFor req False)
            ++ [ ("check_id", JString (ckCheckId result))
               , ("correlation_id", JString (p3caCorrelationId check))
               , ("subject_id", maybe JNull JString (p3caSubjectId check))
               , ("evidence_ref", JString (p3caEvidenceRef check))
               , ("verdict", JString (ckStatus result))
               , ("rejection_trigger", JString (ckRejectionTrigger result))
               , ("details", JString (ckDetails result))
               ]
        )

stageVerdictJson :: PrototypeRequest -> P3Execution -> Int -> JValue
stageVerdictJson req execution attemptId =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("subject_token_ref", maybe JNull JString (fmap (const (p3SubjectTokenRelativePath attemptId)) (p3OutputToken execution)))
               , ("checker_results", JArray (map (JString . p3CheckResultRelativePath attemptId . ckCheckId . p3caResult) (p3Checks execution)))
               , ("stage_result", JString (p3StageResult execution))
               , ("terminal_reason", JString (stageTerminalReason (p3StageResult execution)))
               ]
        )

stageTerminalReason :: String -> String
stageTerminalReason stageResult
    | stageResult == "pass" = "none"
    | otherwise = "blocking-stop-condition"

subjectTokenJson :: PrototypeRequest -> SubjectToken -> JValue
subjectTokenJson req token =
    let provenance = stProvenanceAnchor token
        scope = stSubjectScope token
        owner = stOwnerFamilyStatus token
    in JObject
        ( metadataPairs (metadataFor req False)
            ++ [ ("subject_id", JString (stSubjectId token))
               , ("subject_kind", JString (candidateKindText (stSubjectKind token)))
               , ( "subject_scope"
                 , JObject
                    [ ("scenario_id", JString (ssScenarioId scope))
                    , ("bounded_subject", JString (ssBoundedSubject scope))
                    ]
                 )
               , ( "provenance_anchor"
                 , JObject
                    [ ("origin_stage", JString (paOriginStage provenance))
                    , ("candidate_id", JString (paCandidateId provenance))
                    , ("candidate_inventory_ref", JString (paCandidateInventoryRef provenance))
                    , ("normalization_basis", JString (paNormalizationBasis provenance))
                    , ("discovery_trace_ref", maybe JNull JString (paDiscoveryTraceRef provenance))
                    ]
                 )
               , ( "owner_family_status"
                 , JObject
                    [ ("kind", JString (ofsKind owner))
                    , ("family_id", maybe JNull JString (ofsFamilyId owner))
                    ]
                 )
               , ("trace_handles", JArray (map JString (stTraceHandles token)))
               ]
        )

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
        Just rest -> Just (takeWhile (/= '"') rest)

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

p2Section :: String -> Maybe String
p2Section contents = do
    rest <- findAfter "\"P2\": {" contents
    case breakOn "\"P3\": {" rest of
        Just (section, _) -> Just section
        Nothing -> Just rest

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

data JValue
    = JObject [(String, JValue)]
    | JArray [JValue]
    | JString String
    | JNumber Int
    | JNull

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

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [value] = value
joinWith sep (value : rest) = value ++ sep ++ joinWith sep rest

escapeChar :: Char -> String
escapeChar ch = case ch of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> [ch]
