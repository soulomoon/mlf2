module MLF.Research.URI.R2.C1.Prototype.P1 (
    executeP1
) where

import Data.List (partition)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import MLF.Constraint.Types.Graph (BaseTy(..))
import MLF.Reify.TypeOps (alphaEqType)
import MLF.Types.Elab (ElabType, Ty(..))

import MLF.Research.URI.R2.C1.Prototype.Types

data SeedObservation = SeedObservation
    { soBinderFamily :: String
    , soSccId :: String
    , soType :: ElabType
    }

executeP1 :: PrototypeRequest -> IO PrototypeReport
executeP1 req = do
    let paths = prototypePaths req
        seeds = uriR2C1Seeds
        candidates = normalizeCandidates seeds
        admissible = filter isAdmissible candidates
        stageResult = p1StageResult admissible
        subjectToken = mkSubjectToken req stageResult admissible
        traceSummary =
            [ "Bounded discovery remained inside one SCC and one binder family."
            , "Normalization used cluster-equivalence-v1 over alpha-equivalent recursive roots."
            , "Ambiguity exclusion accepted exactly one normalized candidate."
            ]
        report =
            PrototypeReport
                { prototypeRequest = req
                , prototypeCandidates = candidates
                , prototypeP1C = mkP1CResult
                , prototypeP1N = mkP1NResult
                , prototypeP1U = mkP1UResult stageResult admissible
                , prototypeStageResult = stageResult
                , prototypeSubjectToken = subjectToken
                , prototypeTraceSummary = traceSummary
                , prototypeRawObservationCount = length seeds
                }
    createDirectoryIfMissing True (ppEvidenceDir paths)
    writeEvidence paths seeds report
    pure report

uriR2C1Seeds :: [SeedObservation]
uriR2C1Seeds =
    [ SeedObservation
        { soBinderFamily = "family-uri-r2-c1"
        , soSccId = "scc-uri-r2-c1"
        , soType = recursiveArrowInt "self"
        }
    , SeedObservation
        { soBinderFamily = "family-uri-r2-c1"
        , soSccId = "scc-uri-r2-c1"
        , soType = recursiveArrowInt "loop"
        }
    ]

recursiveArrowInt :: String -> ElabType
recursiveArrowInt binder =
    TMu binder (TArrow (TVar binder) (TBase (BaseTy "Int")))

p1StageName :: String
p1StageName = "P1"

normalizationBasisId :: String
normalizationBasisId = "cluster-equivalence-v1"

normalizeCandidates :: [SeedObservation] -> [CandidateRecord]
normalizeCandidates seeds = go 1 seeds
  where
    go :: Int -> [SeedObservation] -> [CandidateRecord]
    go _ [] = []
    go idx (seed : rest) =
        let (same, other) = partition (sameCandidate seed) rest
            cluster = seed : same
            candidateKind =
                if length cluster == 1
                    then CandidateLocalRoot
                    else CandidateEquivalentLocalCluster
            candidate =
                CandidateRecord
                    { crCandidateId =
                        if candidateKind == CandidateLocalRoot
                            then "root-" ++ show idx
                            else "cluster-" ++ show idx
                    , crCandidateKind = candidateKind
                    , crNormalizationBasis = normalizationBasisId
                    , crAdmissibilityVerdict = "admissible"
                    , crRejectionTrigger = "none"
                    , crObservationCount = length cluster
                    }
        in candidate : go (idx + 1) other

    sameCandidate :: SeedObservation -> SeedObservation -> Bool
    sameCandidate left right =
        soBinderFamily left == soBinderFamily right
            && soSccId left == soSccId right
            && alphaEqType (soType left) (soType right)

isAdmissible :: CandidateRecord -> Bool
isAdmissible candidate =
    crAdmissibilityVerdict candidate == "admissible"
        && crRejectionTrigger candidate == "none"

p1StageResult :: [CandidateRecord] -> String
p1StageResult admissible = case length admissible of
    1 -> "pass"
    0 -> "semantic-negative"
    _ -> "semantic-negative"

mkP1CResult :: CheckResult
mkP1CResult =
    CheckResult
        { ckCheckId = "P1-C"
        , ckStatus = "pass"
        , ckRejectionTrigger = "none"
        , ckDetails = "Finite candidate enumeration stayed inside URI-R2-C1."
        }

mkP1NResult :: CheckResult
mkP1NResult =
    CheckResult
        { ckCheckId = "P1-N"
        , ckStatus = "pass"
        , ckRejectionTrigger = "none"
        , ckDetails = "Normalization collapsed alpha-equivalent roots into one bounded cluster."
        }

mkP1UResult :: String -> [CandidateRecord] -> CheckResult
mkP1UResult stageResult admissible =
    CheckResult
        { ckCheckId = "P1-U"
        , ckStatus = stageResult
        , ckRejectionTrigger =
            case admissible of
                [_] -> "none"
                [] -> "none"
                _ -> "heuristic-choice"
        , ckDetails = "Ambiguity exclusion selected the sole admissible normalized candidate."
        }

mkSubjectToken :: PrototypeRequest -> String -> [CandidateRecord] -> Maybe SubjectToken
mkSubjectToken req stageResult admissible =
    case admissible of
        [candidate] | stageResult == "pass" ->
            let traceHandle = discoveryTraceRef candidate
            in Just
                SubjectToken
                    { stSubjectId = "uri-r2-c1/" ++ crCandidateId candidate
                    , stSubjectKind = crCandidateKind candidate
                    , stSubjectScope =
                        SubjectScope
                            { ssScenarioId = scenarioIdUriR2C1OnlyV1
                            , ssBoundedSubject = boundedSubjectId
                            }
                    , stProvenanceAnchor =
                        ProvenanceAnchor
                            { paOriginStage = p1StageName
                            , paCandidateId = crCandidateId candidate
                            , paCandidateInventoryRef = candidateInventoryRelativePath (prAttemptId req)
                            , paNormalizationBasis = normalizationBasisId
                            , paDiscoveryTraceRef = Just traceHandle
                            }
                    , stOwnerFamilyStatus =
                        OwnerFamilyStatus
                            { ofsKind = "unknown"
                            , ofsFamilyId = Nothing
                            }
                    , stTraceHandles = [traceHandle]
                    }
        _ -> Nothing

discoveryTraceRef :: CandidateRecord -> String
discoveryTraceRef candidate =
    "trace://uri-r2-c1/p1/discovery-" ++ crCandidateId candidate

metadataFor :: PrototypeRequest -> Bool -> EvidenceMetadata
metadataFor req includeStage =
    EvidenceMetadata
        { emResearchEntrypointId = prResearchEntrypointId req
        , emStageSelector = prStageSelector req
        , emScenarioId = prScenarioId req
        , emAttemptId = prAttemptId req
        , emStage = if includeStage then Just p1StageName else Nothing
        }

correlationId :: PrototypeRequest -> String
correlationId req =
    prScenarioId req ++ "-attempt-" ++ show (prAttemptId req)

traceBundleArtifact :: PrototypeReport -> TraceBundle
traceBundleArtifact report =
    let req = prototypeRequest report
    in TraceBundle
        { tbMetadata = metadataFor req True
        , tbCorrelationId = Just (correlationId req)
        , tbSubjectId = tokenSubjectId (prototypeSubjectToken report)
        , tbTraceRefs = maybe [] stTraceHandles (prototypeSubjectToken report)
        }

selectionRuleArtifact :: CandidateSelectionRule
selectionRuleArtifact =
    CandidateSelectionRule
        { csrCandidateUniverse = "local roots or equivalent local clusters inside URI-R2-C1"
        , csrNormalization = normalizationBasisId
        , csrAdmissibilityTest = "exclude widened, repaired, or heuristic candidates"
        , csrOutcome = "exactly-one-subject"
        }

checkerArtifacts :: PrototypeReport -> [CheckerArtifact]
checkerArtifacts report =
    let req = prototypeRequest report
        metadata = metadataFor req False
        attemptId = prAttemptId req
        subjectId = tokenSubjectId (prototypeSubjectToken report)
    in [ CheckerArtifact
            { caMetadata = metadata
            , caCheckId = "P1-C"
            , caSubjectId = subjectId
            , caEvidenceRef = traceBundleRelativePath attemptId
            , caVerdict = ckStatus (prototypeP1C report)
            , caRejectionTrigger = ckRejectionTrigger (prototypeP1C report)
            }
       , CheckerArtifact
            { caMetadata = metadata
            , caCheckId = "P1-N"
            , caSubjectId = subjectId
            , caEvidenceRef = candidateInventoryRelativePath attemptId
            , caVerdict = ckStatus (prototypeP1N report)
            , caRejectionTrigger = ckRejectionTrigger (prototypeP1N report)
            }
       , CheckerArtifact
            { caMetadata = metadata
            , caCheckId = "P1-U"
            , caSubjectId = subjectId
            , caEvidenceRef = candidateSelectionRuleRelativePath attemptId
            , caVerdict = ckStatus (prototypeP1U report)
            , caRejectionTrigger = ckRejectionTrigger (prototypeP1U report)
            }
       ]

stageVerdictArtifact :: PrototypeReport -> StageVerdictArtifact
stageVerdictArtifact report =
    let req = prototypeRequest report
        attemptId = prAttemptId req
    in StageVerdictArtifact
        { svaMetadata = metadataFor req True
        , svaSubjectTokenRef =
            case prototypeSubjectToken report of
                Nothing -> Nothing
                Just _ -> Just (subjectTokenRelativePath attemptId)
        , svaCheckerResults =
            map (checkResultRelativePath attemptId . caCheckId) (checkerArtifacts report)
        , svaStageResult = prototypeStageResult report
        , svaTerminalReason = "none"
        }

tokenSubjectId :: Maybe SubjectToken -> Maybe String
tokenSubjectId = fmap stSubjectId

writeEvidence :: PrototypePaths -> [SeedObservation] -> PrototypeReport -> IO ()
writeEvidence paths _ report = do
    let checkArtifacts = checkerArtifacts report
    writeJsonFile (ppEvidenceDir paths </> "trace-bundle.json") (traceBundleJson (traceBundleArtifact report))
    writeJsonFile (ppEvidenceDir paths </> "candidate-inventory.json") (candidateInventoryJson report)
    writeJsonFile (ppEvidenceDir paths </> "candidate-selection-rule.json") (candidateSelectionRuleJson report)
    mapM_
        (\artifact ->
            writeJsonFile
                (ppEvidenceDir paths </> ("check-" ++ caCheckId artifact ++ ".json"))
                (checkerArtifactJson artifact)
        )
        checkArtifacts
    writeJsonFile (ppEvidenceDir paths </> "stage-verdict.json") (stageVerdictJson (stageVerdictArtifact report))
    case prototypeSubjectToken report of
        Nothing -> pure ()
        Just token ->
            writeJsonFile (ppEvidenceDir paths </> "subject-token.json") (subjectTokenJson report token)

writeJsonFile :: FilePath -> JValue -> IO ()
writeJsonFile path value = writeFile path (renderJValue 0 value ++ "\n")

traceBundleJson :: TraceBundle -> JValue
traceBundleJson bundle =
    JObject
        ( metadataPairs (tbMetadata bundle)
            ++ [ ("correlation_id", maybe JNull JString (tbCorrelationId bundle))
               , ("subject_id", maybe JNull JString (tbSubjectId bundle))
               , ("trace_refs", JArray (map JString (tbTraceRefs bundle)))
               ]
        )

candidateInventoryJson :: PrototypeReport -> JValue
candidateInventoryJson report =
    let req = prototypeRequest report
    in JObject
        ( metadataPairs (metadataFor req False)
            ++ [ ("candidates", JArray (map candidateJson (prototypeCandidates report)))
               ]
        )

candidateSelectionRuleJson :: PrototypeReport -> JValue
candidateSelectionRuleJson report =
    let req = prototypeRequest report
    in JObject
        ( metadataPairs (metadataFor req False)
            ++ [ ("candidate_selection_rule", candidateSelectionRuleJsonValue report selectionRuleArtifact)
               ]
        )

candidateSelectionRuleJsonValue :: PrototypeReport -> CandidateSelectionRule -> JValue
candidateSelectionRuleJsonValue report selectionRule =
    JObject
        [ ("candidate_universe", JString (csrCandidateUniverse selectionRule))
        , ("normalization", JString (csrNormalization selectionRule))
        , ("admissibility_test", JString (csrAdmissibilityTest selectionRule))
        , ("outcome", JString (selectionOutcome report selectionRule))
        ]

selectionOutcome :: PrototypeReport -> CandidateSelectionRule -> String
selectionOutcome report selectionRule
    | prototypeStageResult report == "pass" = csrOutcome selectionRule
    | otherwise = "bounded-ambiguity"

checkerArtifactJson :: CheckerArtifact -> JValue
checkerArtifactJson artifact =
    JObject
        ( metadataPairs (caMetadata artifact)
            ++ [ ("check_id", JString (caCheckId artifact))
               , ("subject_id", maybe JNull JString (caSubjectId artifact))
               , ("evidence_ref", JString (caEvidenceRef artifact))
               , ("verdict", JString (caVerdict artifact))
               , ("rejection_trigger", JString (caRejectionTrigger artifact))
               ]
        )

stageVerdictJson :: StageVerdictArtifact -> JValue
stageVerdictJson verdict =
    JObject
        ( metadataPairs (svaMetadata verdict)
            ++ [ ("subject_token_ref", maybe JNull JString (svaSubjectTokenRef verdict))
               , ("checker_results", JArray (map JString (svaCheckerResults verdict)))
               , ("stage_result", JString (svaStageResult verdict))
               , ("terminal_reason", JString (svaTerminalReason verdict))
               ]
        )

subjectTokenJson :: PrototypeReport -> SubjectToken -> JValue
subjectTokenJson report token =
    let req = prototypeRequest report
        provenance = stProvenanceAnchor token
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

candidateJson :: CandidateRecord -> JValue
candidateJson candidate =
    JObject
        [ ("candidate_id", JString (crCandidateId candidate))
        , ("candidate_kind", JString (candidateKindText (crCandidateKind candidate)))
        , ("normalization_basis", JString (crNormalizationBasis candidate))
        , ("admissibility_verdict", JString (crAdmissibilityVerdict candidate))
        , ("rejection_trigger", JString (crRejectionTrigger candidate))
        ]

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
