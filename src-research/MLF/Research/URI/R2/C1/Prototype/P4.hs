module MLF.Research.URI.R2.C1.Prototype.P4 (
    P4CheckArtifact(..),
    P4StageAuthority(..),
    P4Execution(..),
    executeP4
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

data P4CheckArtifact = P4CheckArtifact
    { p4caResult :: CheckResult
    , p4caCorrelationId :: String
    , p4caEvidenceRef :: FilePath
    }
    deriving (Eq, Show)

data P4StageAuthority = P4StageAuthority
    { p4saStage :: String
    , p4saAttempt :: Int
    , p4saResult :: String
    , p4saTerminalReason :: String
    , p4saSourceReviewRecord :: FilePath
    }
    deriving (Eq, Show)

data P4Execution = P4Execution
    { p4AppReport :: PrototypeReport
    , p4ReviewRecordPaths :: [FilePath]
    , p4ConsumedStages :: [P4StageAuthority]
    , p4Decision :: String
    , p4TerminalReason :: String
    , p4CorrelationId :: String
    , p4TraceRefs :: [String]
    , p4Checks :: [P4CheckArtifact]
    , p4TraceSummary :: [String]
    }
    deriving (Eq, Show)

executeP4 :: PrototypeRequest -> IO (Either PrototypeError P4Execution)
executeP4 req = do
    let records =
            [ (p1AuthoritativeReviewRecordRelativePath, "P1", 2)
            , (p2AuthoritativeReviewRecordRelativePath, "P2", 2)
            , (p3AuthoritativeReviewRecordRelativePath, "P3", 2)
            ]
    parsed <- traverse (loadStageAuthority req) records
    case collectAuthorities parsed of
        Left err -> pure (Left err)
        Right consumedStages -> do
            let paths = prototypePaths req
            cleanEvidenceDir (ppEvidenceDir paths)
            let execution = runP4 req consumedStages
            createDirectoryIfMissing True (ppEvidenceDir paths)
            writeEvidence paths execution
            pure (Right execution)

collectAuthorities :: [Either PrototypeError P4StageAuthority] -> Either PrototypeError [P4StageAuthority]
collectAuthorities = foldr step (Right [])
  where
    step next acc = case (next, acc) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right authority, Right authorities) -> Right (authority : authorities)

loadStageAuthority
    :: PrototypeRequest
    -> (FilePath, String, Int)
    -> IO (Either PrototypeError P4StageAuthority)
loadStageAuthority req (relativePath, stageName, expectedAttempt) = do
    let fullPath = prRepoRoot req </> relativePath
    exists <- doesFileExist fullPath
    if not exists
        then pure (Left (MissingStageInputToken relativePath))
        else do
            contents <- readFile fullPath
            pure (decodeStageAuthority relativePath stageName expectedAttempt contents)

runP4 :: PrototypeRequest -> [P4StageAuthority] -> P4Execution
runP4 req consumedStages =
    finalizeExecution req consumedStages correlationId traceRefs checks decision terminalReason
  where
    correlationId = mkCorrelationId req
    decision = finalDecision consumedStages
    terminalReason = decisionTerminalReason consumedStages decision
    traceRefs =
        [ traceRef "consume" ("p1-" ++ stageResultTag "P1")
        , traceRef "consume" ("p2-" ++ stageResultTag "P2")
        , traceRef "consume" ("p3-" ++ stageResultTag "P3")
        , traceRef "decision" (decision ++ "-" ++ terminalReason)
        ]
    checks =
        [ mkCheckArtifact correlationId (p4StageConsumptionRelativePath (prAttemptId req))
            "P4-CONSUME"
            "pass"
            "none"
            "P4 consumed authoritative P1/P2/P3 review records without widening scope."
        , mkCheckArtifact correlationId (p4DecisionVerdictRelativePath (prAttemptId req))
            "P4-DECISION"
            (if decision == "reopen-handoff-track" then "pass" else "semantic-negative")
            (if decision == "reopen-handoff-track" then "none" else terminalReason)
            ("P4 terminal decision: " ++ decision ++ ".")
        ]

    stageResultTag :: String -> String
    stageResultTag stageName =
        case filter (\authority -> p4saStage authority == stageName) consumedStages of
            (authority : _) -> slugify (p4saResult authority)
            [] -> "missing"

decodeStageAuthority :: FilePath -> String -> Int -> String -> Either PrototypeError P4StageAuthority
decodeStageAuthority reviewPath stageName expectedAttempt contents = do
    section <-
        case extractStageSection stageName contents of
            Nothing ->
                Left
                    (MalformedStageInputToken reviewPath ("missing " ++ stageName ++ " stage section in review-record.json"))
            Just stageSection -> Right stageSection
    status <- requiredStringField reviewPath "status" section
    if status /= "authoritative"
        then
            Left
                ( MalformedStageInputToken
                    reviewPath
                    (stageName ++ " must have status authoritative.")
                )
        else pure ()
    attempt <- requiredIntField reviewPath "authoritative_attempt" section
    if attempt /= expectedAttempt
        then
            Left
                ( MalformedStageInputToken
                    reviewPath
                    ( stageName
                        ++ " authoritative_attempt must be "
                        ++ show expectedAttempt
                        ++ " for bounded P4."
                    )
                )
        else pure ()
    result <- requiredStringField reviewPath "authoritative_result" section
    terminalReason <- optionalStringField "terminal_reason" section
    pure
        P4StageAuthority
            { p4saStage = stageName
            , p4saAttempt = attempt
            , p4saResult = result
            , p4saTerminalReason = maybe "none" id terminalReason
            , p4saSourceReviewRecord = reviewPath
            }

finalDecision :: [P4StageAuthority] -> String
finalDecision consumedStages
    | all stagePass consumedStages
        && all ((== "none") . p4saTerminalReason) consumedStages =
            "reopen-handoff-track"
    | otherwise = "hard-stop"
  where
    stagePass authority = p4saResult authority == "pass"

decisionTerminalReason :: [P4StageAuthority] -> String -> String
decisionTerminalReason consumedStages decision
    | decision == "reopen-handoff-track" = "none"
    | any isBudgetExhausted consumedStages = "budget-exhausted-inconclusive"
    | otherwise = "blocking-stop-condition"
  where
    isBudgetExhausted authority =
        p4saTerminalReason authority == "budget-exhausted-inconclusive"
            || p4saResult authority == "inconclusive"

mkCorrelationId :: PrototypeRequest -> String
mkCorrelationId req =
    prScenarioId req ++ "-p4-attempt-" ++ show (prAttemptId req)

mkCheckArtifact :: String -> FilePath -> String -> String -> String -> String -> P4CheckArtifact
mkCheckArtifact correlationId evidenceRef checkId status rejectionTrigger details =
    P4CheckArtifact
        { p4caResult =
            CheckResult
                { ckCheckId = checkId
                , ckStatus = status
                , ckRejectionTrigger = rejectionTrigger
                , ckDetails = details
                }
        , p4caCorrelationId = correlationId
        , p4caEvidenceRef = evidenceRef
        }

finalizeExecution
    :: PrototypeRequest
    -> [P4StageAuthority]
    -> String
    -> [String]
    -> [P4CheckArtifact]
    -> String
    -> String
    -> P4Execution
finalizeExecution req consumedStages correlationId traceRefs checks decision terminalReason =
    let summary =
            [ "P4 consumed authoritative stage outcomes from round-016, round-017, and round-018 review records."
            , "Decision threshold respected: reopen requires all pass and no unresolved caveat."
            , "Given inherited results, terminal decision is " ++ decision ++ "."
            ]
                ++ map describeConsumed consumedStages
        report =
            PrototypeReport
                { prototypeRequest = req
                , prototypeCandidates = []
                , prototypeP1C = reportCheckAt 0
                , prototypeP1N = reportCheckAt 1
                , prototypeP1U = reportCheckAt 1
                , prototypeStageResult = decision
                , prototypeSubjectToken = Nothing
                , prototypeTraceSummary = summary
                , prototypeRawObservationCount = length checks
                }
    in P4Execution
        { p4AppReport = report
        , p4ReviewRecordPaths = map p4saSourceReviewRecord consumedStages
        , p4ConsumedStages = consumedStages
        , p4Decision = decision
        , p4TerminalReason = terminalReason
        , p4CorrelationId = correlationId
        , p4TraceRefs = traceRefs
        , p4Checks = checks
        , p4TraceSummary = summary
        }
  where
    reportCheckAt ix =
        case drop ix checks of
            (check : _) -> p4caResult check
            [] ->
                CheckResult
                    { ckCheckId = "P4-missing"
                    , ckStatus = "inconclusive"
                    , ckRejectionTrigger = "blocking-stop-condition"
                    , ckDetails = "Missing P4 check artifact."
                    }

    describeConsumed authority =
        p4saStage authority
            ++ " authoritative attempt "
            ++ show (p4saAttempt authority)
            ++ " => "
            ++ p4saResult authority
            ++ " (terminal_reason: "
            ++ p4saTerminalReason authority
            ++ ")"

traceRef :: String -> String -> String
traceRef operation detail =
    "trace://uri-r2-c1/p4/"
        ++ operation
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
        , emStage = if includeStage then Just "P4" else Nothing
        }

writeEvidence :: PrototypePaths -> P4Execution -> IO ()
writeEvidence paths execution = do
    let req = prototypeRequest (p4AppReport execution)
        attemptId = prAttemptId req
    writeJsonFile
        (ppEvidenceDir paths </> "trace-bundle.json")
        (traceBundleJson req execution)
    writeJsonFile
        (ppEvidenceDir paths </> "stage-consumption.json")
        (stageConsumptionJson req execution)
    writeJsonFile
        (ppEvidenceDir paths </> "decision-verdict.json")
        (decisionVerdictJson req execution attemptId)

traceBundleJson :: PrototypeRequest -> P4Execution -> JValue
traceBundleJson req execution =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("correlation_id", JString (p4CorrelationId execution))
               , ("subject_id", JNull)
               , ("trace_refs", JArray (map JString (p4TraceRefs execution)))
               , ("trace_summary", JArray (map JString (p4TraceSummary execution)))
               ]
        )

stageConsumptionJson :: PrototypeRequest -> P4Execution -> JValue
stageConsumptionJson req execution =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("consumed_stages", JArray (map consumedStageJson (p4ConsumedStages execution)))
               , ("consumed_stage_results", consumedStageResultsObject (p4ConsumedStages execution))
               ]
        )

decisionVerdictJson :: PrototypeRequest -> P4Execution -> Int -> JValue
decisionVerdictJson req execution attemptId =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("correlation_id", JString (p4CorrelationId execution))
               , ("decision_evidence_ref", JString (p4DecisionVerdictRelativePath attemptId))
               , ("stage_consumption_ref", JString (p4StageConsumptionRelativePath attemptId))
               , ("final_decision", JString (p4Decision execution))
               , ("terminal_reason", JString (p4TerminalReason execution))
               , ("consumed_stage_results", consumedStageResultsObject (p4ConsumedStages execution))
               , ("decision_rationale", JString (decisionRationale execution))
               , ("allowed_terminal_decisions", JArray [JString "reopen-handoff-track", JString "hard-stop"])
               ]
        )

decisionRationale :: P4Execution -> String
decisionRationale execution =
    case p4Decision execution of
        "reopen-handoff-track" ->
            "All inherited stages are pass with no unresolved terminal caveat."
        _ ->
            "At least one inherited stage is semantic-negative or terminally blocked, so bounded P4 resolves to hard-stop."

consumedStageJson :: P4StageAuthority -> JValue
consumedStageJson authority =
    JObject
        [ ("stage", JString (p4saStage authority))
        , ("authoritative_attempt", JNumber (p4saAttempt authority))
        , ("authoritative_result", JString (p4saResult authority))
        , ("terminal_reason", JString (p4saTerminalReason authority))
        , ("source_review_record", JString (p4saSourceReviewRecord authority))
        ]

consumedStageResultsObject :: [P4StageAuthority] -> JValue
consumedStageResultsObject authorities =
    JObject
        ( map
            (\authority -> (p4saStage authority, JString (p4saResult authority)))
            authorities
        )

extractStageSection :: String -> String -> Maybe String
extractStageSection stageName contents = do
    rest <- findAfter ("\"" ++ stageName ++ "\":") contents
    let trimmed = dropWhile isSpace rest
    case trimmed of
        ('{' : xs) -> extractBalancedObject 1 "{" xs
        _ -> Nothing

extractBalancedObject :: Int -> String -> String -> Maybe String
extractBalancedObject _ _ [] = Nothing
extractBalancedObject depth acc (ch : rest)
    | ch == '{' =
        extractBalancedObject (depth + 1) (acc ++ [ch]) rest
    | ch == '}' =
        let acc' = acc ++ [ch]
        in if depth == 1
            then Just acc'
            else extractBalancedObject (depth - 1) acc' rest
    | otherwise =
        extractBalancedObject depth (acc ++ [ch]) rest

requiredStringField :: FilePath -> String -> String -> Either PrototypeError String
requiredStringField tokenPath fieldName contents =
    case extractStringField fieldName contents of
        Just value -> Right value
        Nothing -> Left (MalformedStageInputToken tokenPath ("missing string field: " ++ fieldName))

requiredIntField :: FilePath -> String -> String -> Either PrototypeError Int
requiredIntField tokenPath fieldName contents =
    case extractIntField fieldName contents of
        Just value -> Right value
        Nothing -> Left (MalformedStageInputToken tokenPath ("missing int field: " ++ fieldName))

optionalStringField :: String -> String -> Either PrototypeError (Maybe String)
optionalStringField fieldName contents =
    case findAfter ("\"" ++ fieldName ++ "\": ") contents of
        Nothing -> Right Nothing
        Just rest ->
            case dropWhile isSpace rest of
                ('n' : 'u' : 'l' : 'l' : _) -> Right Nothing
                ('"' : xs) -> Right (Just (takeWhile (/= '"') xs))
                _ -> Right Nothing

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
            let digits = takeWhile (\ch -> '0' <= ch && ch <= '9') rest
            in if null digits then Nothing else Just (read digits)

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
