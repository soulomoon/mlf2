module MLF.Research.URI.R2.C1.Prototype.D3 (
    D3CheckArtifact(..),
    D3Execution(..),
    executeD3
) where

import Control.Exception (catch)
import Data.Char (isDigit, isSpace)
import Data.List (intercalate, isInfixOf)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , removePathForcibly
    )
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

import MLF.Research.URI.R2.C1.Prototype.Types

data D3CheckArtifact = D3CheckArtifact
    { d3caResult :: CheckResult
    , d3caCorrelationId :: String
    , d3caSubjectId :: String
    , d3caEvidenceRef :: FilePath
    }
    deriving (Eq, Show)

data D3Execution = D3Execution
    { d3AppReport :: PrototypeReport
    , d3InheritedSubjectTokenPath :: FilePath
    , d3InheritedP2CheckWPath :: FilePath
    , d3InheritedP2StageVerdictPath :: FilePath
    , d3InheritedD1StageVerdictPath :: FilePath
    , d3InheritedD1ReviewRecordPath :: FilePath
    , d3InheritedD2CheckLPath :: FilePath
    , d3InheritedD2CheckOPath :: FilePath
    , d3InheritedD2StageVerdictPath :: FilePath
    , d3InheritedD2TraceBundlePath :: FilePath
    , d3InheritedD2ReviewRecordPath :: FilePath
    , d3CorrelationId :: String
    , d3TraceRefs :: [String]
    , d3Checks :: [D3CheckArtifact]
    , d3StageResult :: String
    , d3AttemptVerdict :: String
    , d3FixHypothesis :: String
    , d3RepairDirection :: String
    , d3DivergenceBoundary :: String
    , d3OwnerAccount :: String
    , d3TraceSummary :: [String]
    }
    deriving (Eq, Show)

data InheritedCheck = InheritedCheck
    { icScenarioId :: String
    , icSubjectId :: String
    , icVerdict :: String
    , icRejectionTrigger :: String
    , icDetails :: String
    }
    deriving (Eq, Show)

data InheritedStageVerdict = InheritedStageVerdict
    { isvScenarioId :: String
    , isvStageResult :: String
    }
    deriving (Eq, Show)

data InheritedTraceBundle = InheritedTraceBundle
    { itbScenarioId :: String
    , itbSubjectId :: String
    , itbTraceRefs :: [String]
    , itbDivergenceBoundary :: String
    , itbOwnerAccount :: String
    }
    deriving (Eq, Show)

data InheritedReviewRecord = InheritedReviewRecord
    { irrScenarioId :: String
    , irrStageId :: String
    , irrAttemptVerdict :: String
    , irrStageResult :: String
    , irrStageAction :: String
    , irrStatus :: String
    , irrAuthoritativeAttempt :: Int
    , irrAuthoritativeResult :: String
    }
    deriving (Eq, Show)

data HypothesisProbe = HypothesisProbe
    { hpStatus :: String
    , hpTrigger :: String
    , hpDetails :: String
    }
    deriving (Eq, Show)

data BoundaryProbe = BoundaryProbe
    { bpStatus :: String
    , bpTrigger :: String
    , bpDetails :: String
    }
    deriving (Eq, Show)

data VerdictProbe = VerdictProbe
    { vpStatus :: String
    , vpTrigger :: String
    , vpDetails :: String
    , vpAttemptVerdict :: String
    , vpStageResult :: String
    }
    deriving (Eq, Show)

authoritativeSubjectId :: String
authoritativeSubjectId = "uri-r2-c1/cluster-1"

authoritativeTrigger :: String
authoritativeTrigger = "partial-replay"

authoritativeMismatch :: String
authoritativeMismatch = "InstBot expects \8869, got: t9 -> t9"

authoritativeBoundary :: String
authoritativeBoundary = "witness-replay/applyInstantiation-instbot-precondition"

authoritativeOwner :: String
authoritativeOwner =
    "applyInstantiation semantics (`MLF.Elab.Inst.applyInstantiation`, InstBot branch)"

fixHypothesisH1 :: String
fixHypothesisH1 =
    "H1: the localized InstBot precondition mismatch supports one bounded paper-faithful repair direction at applyInstantiation (InstBot) without scope widening or default-path behavior changes."

repairDirectionH1 :: String
repairDirectionH1 =
    "Bounded direction: align InstBot precondition handling with the no-fallback replay shape (`t5 -> t5`) at the localized applyInstantiation boundary, preserving URI-R2-C1 and uri-r2-c1-only-v1 scope."

executeD3 :: PrototypeRequest -> IO (Either PrototypeError D3Execution)
executeD3 req = do
    let inheritedSubjectTokenRelPath = p1AuthoritativeSubjectTokenRelativePath
        inheritedP2CheckWRelPath = p2AuthoritativeCheckWRelativePath
        inheritedP2StageVerdictRelPath = p2AuthoritativeStageVerdictRelativePath
        inheritedD1StageVerdictRelPath = d1StageVerdictRelativePath 1
        inheritedD1ReviewRecordRelPath = d1AuthoritativeReviewRecordRelativePath
        inheritedD2CheckLRelPath = d2CheckResultRelativePath 1 "D2-L"
        inheritedD2CheckORelPath = d2CheckResultRelativePath 1 "D2-O"
        inheritedD2StageVerdictRelPath = d2StageVerdictRelativePath 1
        inheritedD2TraceBundleRelPath = d2TraceBundleRelativePath 1
        inheritedD2ReviewRecordRelPath = d2AuthoritativeReviewRecordRelativePath
        subjectTokenPath = prRepoRoot req </> inheritedSubjectTokenRelPath
        p2CheckWPath = prRepoRoot req </> inheritedP2CheckWRelPath
        p2StageVerdictPath = prRepoRoot req </> inheritedP2StageVerdictRelPath
        d1StageVerdictPath = prRepoRoot req </> inheritedD1StageVerdictRelPath
        d1ReviewRecordPath = prRepoRoot req </> inheritedD1ReviewRecordRelPath
        d2CheckLPath = prRepoRoot req </> inheritedD2CheckLRelPath
        d2CheckOPath = prRepoRoot req </> inheritedD2CheckORelPath
        d2StageVerdictPath = prRepoRoot req </> inheritedD2StageVerdictRelPath
        d2TraceBundlePath = prRepoRoot req </> inheritedD2TraceBundleRelPath
        d2ReviewRecordPath = prRepoRoot req </> inheritedD2ReviewRecordRelPath
        requiredInputs =
            [ (inheritedSubjectTokenRelPath, subjectTokenPath)
            , (inheritedP2CheckWRelPath, p2CheckWPath)
            , (inheritedP2StageVerdictRelPath, p2StageVerdictPath)
            , (inheritedD1StageVerdictRelPath, d1StageVerdictPath)
            , (inheritedD1ReviewRecordRelPath, d1ReviewRecordPath)
            , (inheritedD2CheckLRelPath, d2CheckLPath)
            , (inheritedD2CheckORelPath, d2CheckOPath)
            , (inheritedD2StageVerdictRelPath, d2StageVerdictPath)
            , (inheritedD2TraceBundleRelPath, d2TraceBundlePath)
            , (inheritedD2ReviewRecordRelPath, d2ReviewRecordPath)
            ]
    allExist <- and <$> mapM (doesFileExist . snd) requiredInputs
    if not allExist
        then firstMissing requiredInputs
        else do
            subjectTokenText <- readFile subjectTokenPath
            p2CheckWText <- readFile p2CheckWPath
            p2StageVerdictText <- readFile p2StageVerdictPath
            d1StageVerdictText <- readFile d1StageVerdictPath
            d1ReviewRecordText <- readFile d1ReviewRecordPath
            d2CheckLText <- readFile d2CheckLPath
            d2CheckOText <- readFile d2CheckOPath
            d2StageVerdictText <- readFile d2StageVerdictPath
            d2TraceBundleText <- readFile d2TraceBundlePath
            d2ReviewRecordText <- readFile d2ReviewRecordPath
            case
                    decodeD3Inputs
                        inheritedSubjectTokenRelPath
                        subjectTokenText
                        inheritedP2CheckWRelPath
                        p2CheckWText
                        inheritedP2StageVerdictRelPath
                        p2StageVerdictText
                        inheritedD1StageVerdictRelPath
                        d1StageVerdictText
                        inheritedD1ReviewRecordRelPath
                        d1ReviewRecordText
                        inheritedD2CheckLRelPath
                        d2CheckLText
                        inheritedD2CheckORelPath
                        d2CheckOText
                        inheritedD2StageVerdictRelPath
                        d2StageVerdictText
                        inheritedD2TraceBundleRelPath
                        d2TraceBundleText
                        inheritedD2ReviewRecordRelPath
                        d2ReviewRecordText
                of
                Left err -> pure (Left err)
                Right
                    ( subjectToken
                    , p2CheckW
                    , p2StageVerdict
                    , d1StageVerdict
                    , d1ReviewRecord
                    , d2CheckL
                    , d2CheckO
                    , d2StageVerdict
                    , d2TraceBundle
                    , d2ReviewRecord
                    ) -> do
                        let paths = prototypePaths req
                        cleanDir (ppEvidenceDir paths)
                        createDirectoryIfMissing True (ppEvidenceDir paths)
                        let execution =
                                finalizeExecution
                                    req
                                    inheritedSubjectTokenRelPath
                                    inheritedP2CheckWRelPath
                                    inheritedP2StageVerdictRelPath
                                    inheritedD1StageVerdictRelPath
                                    inheritedD1ReviewRecordRelPath
                                    inheritedD2CheckLRelPath
                                    inheritedD2CheckORelPath
                                    inheritedD2StageVerdictRelPath
                                    inheritedD2TraceBundleRelPath
                                    inheritedD2ReviewRecordRelPath
                                    subjectToken
                                    p2CheckW
                                    p2StageVerdict
                                    d1StageVerdict
                                    d1ReviewRecord
                                    d2CheckL
                                    d2CheckO
                                    d2StageVerdict
                                    d2TraceBundle
                                    d2ReviewRecord
                        writeEvidence paths execution
                        pure (Right execution)
  where
    firstMissing [] = pure (Left (MissingStageInputToken ""))
    firstMissing ((relPath, fullPath) : rest) = do
        exists <- doesFileExist fullPath
        if exists
            then firstMissing rest
            else pure (Left (MissingStageInputToken relPath))

decodeD3Inputs
    :: FilePath
    -> String
    -> FilePath
    -> String
    -> FilePath
    -> String
    -> FilePath
    -> String
    -> FilePath
    -> String
    -> FilePath
    -> String
    -> FilePath
    -> String
    -> FilePath
    -> String
    -> FilePath
    -> String
    -> FilePath
    -> String
    -> Either
        PrototypeError
        ( SubjectToken
        , InheritedCheck
        , InheritedStageVerdict
        , InheritedStageVerdict
        , InheritedReviewRecord
        , InheritedCheck
        , InheritedCheck
        , InheritedStageVerdict
        , InheritedTraceBundle
        , InheritedReviewRecord
        )
decodeD3Inputs
    subjectTokenPath
    subjectTokenText
    p2CheckWPath
    p2CheckWText
    p2StageVerdictPath
    p2StageVerdictText
    d1StageVerdictPath
    d1StageVerdictText
    d1ReviewRecordPath
    d1ReviewRecordText
    d2CheckLPath
    d2CheckLText
    d2CheckOPath
    d2CheckOText
    d2StageVerdictPath
    d2StageVerdictText
    d2TraceBundlePath
    d2TraceBundleText
    d2ReviewRecordPath
    d2ReviewRecordText = do
        subjectToken <- decodeSubjectToken subjectTokenPath subjectTokenText
        p2CheckW <- decodeCheck p2CheckWPath p2CheckWText
        p2StageVerdict <- decodeStageVerdict p2StageVerdictPath p2StageVerdictText
        d1StageVerdict <- decodeStageVerdict d1StageVerdictPath d1StageVerdictText
        d1ReviewRecord <- decodeReviewRecord d1ReviewRecordPath d1ReviewRecordText
        d2CheckL <- decodeCheck d2CheckLPath d2CheckLText
        d2CheckO <- decodeCheck d2CheckOPath d2CheckOText
        d2StageVerdict <- decodeStageVerdict d2StageVerdictPath d2StageVerdictText
        d2TraceBundle <- decodeTraceBundle d2TraceBundlePath d2TraceBundleText
        d2ReviewRecord <- decodeReviewRecord d2ReviewRecordPath d2ReviewRecordText
        pure
            ( subjectToken
            , p2CheckW
            , p2StageVerdict
            , d1StageVerdict
            , d1ReviewRecord
            , d2CheckL
            , d2CheckO
            , d2StageVerdict
            , d2TraceBundle
            , d2ReviewRecord
            )

finalizeExecution
    :: PrototypeRequest
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> FilePath
    -> SubjectToken
    -> InheritedCheck
    -> InheritedStageVerdict
    -> InheritedStageVerdict
    -> InheritedReviewRecord
    -> InheritedCheck
    -> InheritedCheck
    -> InheritedStageVerdict
    -> InheritedTraceBundle
    -> InheritedReviewRecord
    -> D3Execution
finalizeExecution
    req
    subjectTokenPath
    p2CheckWPath
    p2StageVerdictPath
    d1StageVerdictPath
    d1ReviewRecordPath
    d2CheckLPath
    d2CheckOPath
    d2StageVerdictPath
    d2TraceBundlePath
    d2ReviewRecordPath
    subjectToken
    p2CheckW
    p2StageVerdict
    d1StageVerdict
    d1ReviewRecord
    d2CheckL
    d2CheckO
    d2StageVerdict
    d2TraceBundle
    d2ReviewRecord =
        let correlationId = mkCorrelationId req
            subjectId = stSubjectId subjectToken
            traceBundleRef = d3TraceBundleRelativePath (prAttemptId req)
            continuityIssues =
                continuityMismatches
                    token
                    p2CheckW
                    p2StageVerdict
                    d1StageVerdict
                    d1ReviewRecord
                    d2CheckL
                    d2CheckO
                    d2StageVerdict
                    d2TraceBundle
                    d2ReviewRecord
            token = subjectToken
            hypothesis = evaluateHypothesis continuityIssues d2CheckL d2CheckO d2TraceBundle
            boundary = evaluateBoundary req continuityIssues
            verdict = evaluateVerdict continuityIssues hypothesis boundary
            checkH =
                mkCheckArtifact
                    subjectId
                    traceBundleRef
                    correlationId
                    "D3-H"
                    (hpStatus hypothesis)
                    (hpTrigger hypothesis)
                    (hpDetails hypothesis)
            checkB =
                mkCheckArtifact
                    subjectId
                    traceBundleRef
                    correlationId
                    "D3-B"
                    (bpStatus boundary)
                    (bpTrigger boundary)
                    (bpDetails boundary)
            checkV =
                mkCheckArtifact
                    subjectId
                    traceBundleRef
                    correlationId
                    "D3-V"
                    (vpStatus verdict)
                    (vpTrigger verdict)
                    (vpDetails verdict)
            checks = [checkH, checkB, checkV]
            summary =
                [ "D3-H tested one bounded hypothesis (`H1`) anchored to the D2-localized boundary and owner account."
                , "D3-B verified scope preservation: URI-R2-C1, uri-r2-c1-only-v1, inherited P1/P2/D1/D2 authorities, and no second executable interface."
                , "D3-V classified the bounded probe attempt as `" ++ vpAttemptVerdict verdict ++ "`."
                , "Fix hypothesis: " ++ fixHypothesisH1
                , "Repair direction (probe-only): " ++ repairDirectionH1
                ]
                    ++ map (\check -> ckCheckId (d3caResult check) ++ ": " ++ ckDetails (d3caResult check)) checks
            report =
                PrototypeReport
                    { prototypeRequest = req
                    , prototypeCandidates = []
                    , prototypeP1C = d3caResult checkH
                    , prototypeP1N = d3caResult checkB
                    , prototypeP1U = d3caResult checkV
                    , prototypeStageResult = vpStageResult verdict
                    , prototypeSubjectToken = Nothing
                    , prototypeTraceSummary = summary
                    , prototypeRawObservationCount = length checks
                    }
        in D3Execution
            { d3AppReport = report
            , d3InheritedSubjectTokenPath = subjectTokenPath
            , d3InheritedP2CheckWPath = p2CheckWPath
            , d3InheritedP2StageVerdictPath = p2StageVerdictPath
            , d3InheritedD1StageVerdictPath = d1StageVerdictPath
            , d3InheritedD1ReviewRecordPath = d1ReviewRecordPath
            , d3InheritedD2CheckLPath = d2CheckLPath
            , d3InheritedD2CheckOPath = d2CheckOPath
            , d3InheritedD2StageVerdictPath = d2StageVerdictPath
            , d3InheritedD2TraceBundlePath = d2TraceBundlePath
            , d3InheritedD2ReviewRecordPath = d2ReviewRecordPath
            , d3CorrelationId = correlationId
            , d3TraceRefs = rewriteD3Refs (itbTraceRefs d2TraceBundle)
            , d3Checks = checks
            , d3StageResult = vpStageResult verdict
            , d3AttemptVerdict = vpAttemptVerdict verdict
            , d3FixHypothesis = fixHypothesisH1
            , d3RepairDirection = repairDirectionH1
            , d3DivergenceBoundary = authoritativeBoundary
            , d3OwnerAccount = authoritativeOwner
            , d3TraceSummary = summary
            }

evaluateHypothesis
    :: [String]
    -> InheritedCheck
    -> InheritedCheck
    -> InheritedTraceBundle
    -> HypothesisProbe
evaluateHypothesis continuityIssues d2CheckL d2CheckO d2TraceBundle
    | not (null continuityIssues) =
        HypothesisProbe
            { hpStatus = "semantic-negative"
            , hpTrigger = "inconsistent-trace"
            , hpDetails = "continuity mismatch blocks D3-H: " ++ intercalate "; " continuityIssues
            }
    | localizationPass && ownerPass && traceBoundaryPass && traceOwnerPass =
        HypothesisProbe
            { hpStatus = "pass"
            , hpTrigger = "none"
            , hpDetails =
                "H1 support established at "
                    ++ authoritativeBoundary
                    ++ ": localized boundary and owner are stable, and one bounded paper-faithful repair direction exists without widening. direction="
                    ++ repairDirectionH1
            }
    | icVerdict d2CheckL == "inconclusive" || icVerdict d2CheckO == "inconclusive" =
        HypothesisProbe
            { hpStatus = "inconclusive"
            , hpTrigger = "blocking-stop-condition"
            , hpDetails =
                "cannot stabilize H1 because D2 localization/owner evidence is inconclusive"
            }
    | otherwise =
        HypothesisProbe
            { hpStatus = "semantic-negative"
            , hpTrigger = "inconsistent-trace"
            , hpDetails =
                "H1 unsupported under bounded evidence: D2-L verdict="
                    ++ icVerdict d2CheckL
                    ++ ", D2-O verdict="
                    ++ icVerdict d2CheckO
                    ++ ", boundary_pass="
                    ++ boolText traceBoundaryPass
                    ++ ", owner_pass="
                    ++ boolText traceOwnerPass
            }
  where
    localizationPass =
        icVerdict d2CheckL == "pass"
            && icRejectionTrigger d2CheckL == "none"
            && authoritativeBoundary `isInfixOf` icDetails d2CheckL
            && authoritativeMismatch `isInfixOf` icDetails d2CheckL
    ownerPass =
        icVerdict d2CheckO == "pass"
            && icRejectionTrigger d2CheckO == "none"
            && authoritativeOwner `isInfixOf` icDetails d2CheckO
    traceBoundaryPass = itbDivergenceBoundary d2TraceBundle == authoritativeBoundary
    traceOwnerPass = authoritativeOwner `isInfixOf` itbOwnerAccount d2TraceBundle

evaluateBoundary :: PrototypeRequest -> [String] -> BoundaryProbe
evaluateBoundary req continuityIssues
    | not (null continuityIssues) =
        BoundaryProbe
            { bpStatus = "semantic-negative"
            , bpTrigger = "inconsistent-trace"
            , bpDetails = "continuity mismatch blocks D3-B: " ++ intercalate "; " continuityIssues
            }
    | tupleLocked =
        BoundaryProbe
            { bpStatus = "pass"
            , bpTrigger = "none"
            , bpDetails =
                "bounded scope preserved: tuple locked to { research_entrypoint_id="
                    ++ replayRootCauseEntrypointId
                    ++ ", stage_selector="
                    ++ stageSelectorD3
                    ++ ", scenario_id="
                    ++ scenarioIdUriR2C1OnlyV1
                    ++ " }, evidence written only under "
                    ++ d3AttemptEvidenceRelativeDir (prAttemptId req)
                    ++ ", no second executable interface, no default-path production change"
            }
    | otherwise =
        BoundaryProbe
            { bpStatus = "semantic-negative"
            , bpTrigger = "inconsistent-trace"
            , bpDetails =
                "tuple mismatch blocked D3-B: entrypoint="
                    ++ prResearchEntrypointId req
                    ++ ", stage="
                    ++ prStageSelector req
                    ++ ", scenario="
                    ++ prScenarioId req
            }
  where
    tupleLocked =
        prResearchEntrypointId req == replayRootCauseEntrypointId
            && prStageSelector req == stageSelectorD3
            && prScenarioId req == scenarioIdUriR2C1OnlyV1

evaluateVerdict :: [String] -> HypothesisProbe -> BoundaryProbe -> VerdictProbe
evaluateVerdict continuityIssues hypothesis boundary
    | not (null continuityIssues) =
        VerdictProbe
            { vpStatus = "semantic-negative"
            , vpTrigger = "inconsistent-trace"
            , vpDetails = "probe classified bounded-negative because continuity mismatches remain unresolved"
            , vpAttemptVerdict = "bounded-negative"
            , vpStageResult = "semantic-negative"
            }
    | hpStatus hypothesis == "pass" && bpStatus boundary == "pass" =
        VerdictProbe
            { vpStatus = "pass"
            , vpTrigger = "none"
            , vpDetails =
                "attempt classified repair-supporting: one bounded repair direction is justified and remains within D3 scope"
            , vpAttemptVerdict = "repair-supporting"
            , vpStageResult = "pass"
            }
    | hpStatus hypothesis == "inconclusive" || bpStatus boundary == "inconclusive" =
        VerdictProbe
            { vpStatus = "inconclusive"
            , vpTrigger = "blocking-stop-condition"
            , vpDetails =
                "attempt classified inconclusive: bounded evidence cannot stabilize H1 and boundary proof together"
            , vpAttemptVerdict = "inconclusive"
            , vpStageResult = "inconclusive"
            }
    | otherwise =
        VerdictProbe
            { vpStatus = "semantic-negative"
            , vpTrigger = "inconsistent-trace"
            , vpDetails =
                "attempt classified bounded-negative: no bounded support for reopening repair direction in this attempt"
            , vpAttemptVerdict = "bounded-negative"
            , vpStageResult = "semantic-negative"
            }

continuityMismatches
    :: SubjectToken
    -> InheritedCheck
    -> InheritedStageVerdict
    -> InheritedStageVerdict
    -> InheritedReviewRecord
    -> InheritedCheck
    -> InheritedCheck
    -> InheritedStageVerdict
    -> InheritedTraceBundle
    -> InheritedReviewRecord
    -> [String]
continuityMismatches
    token
    p2CheckW
    p2StageVerdict
    d1StageVerdict
    d1ReviewRecord
    d2CheckL
    d2CheckO
    d2StageVerdict
    d2TraceBundle
    d2ReviewRecord =
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
            , [ "P2-W scenario must remain uri-r2-c1-only-v1"
              | icScenarioId p2CheckW /= scenarioIdUriR2C1OnlyV1
              ]
            , [ "P2-W subject_id must remain uri-r2-c1/cluster-1"
              | icSubjectId p2CheckW /= authoritativeSubjectId
              ]
            , [ "P2-W trigger must remain partial-replay"
              | icRejectionTrigger p2CheckW /= authoritativeTrigger
              ]
            , [ "P2-W mismatch must include InstBot expects \\8869, got: t9 -> t9"
              | not (detailsHasAuthoritativeMismatch (icDetails p2CheckW))
              ]
            , [ "P2 stage verdict must remain semantic-negative"
              | isvStageResult p2StageVerdict /= "semantic-negative"
              ]
            , [ "P2 stage verdict scenario must remain uri-r2-c1-only-v1"
              | isvScenarioId p2StageVerdict /= scenarioIdUriR2C1OnlyV1
              ]
            , [ "D1 stage verdict must remain pass"
              | isvStageResult d1StageVerdict /= "pass"
              ]
            , [ "D1 stage verdict scenario must remain uri-r2-c1-only-v1"
              | isvScenarioId d1StageVerdict /= scenarioIdUriR2C1OnlyV1
              ]
            , [ "D1 review record stage_id must remain D1"
              | irrStageId d1ReviewRecord /= "D1"
              ]
            , [ "D1 review record attempt_verdict must remain accepted"
              | irrAttemptVerdict d1ReviewRecord /= "accepted"
              ]
            , [ "D1 review record stage_action must remain finalize"
              | irrStageAction d1ReviewRecord /= "finalize"
              ]
            , [ "D1 review record stage_result must remain pass"
              | irrStageResult d1ReviewRecord /= "pass"
              ]
            , [ "D1 review record status must remain authoritative"
              | irrStatus d1ReviewRecord /= "authoritative"
              ]
            , [ "D1 review record authoritative_attempt must remain 1"
              | irrAuthoritativeAttempt d1ReviewRecord /= 1
              ]
            , [ "D1 review record authoritative_result must remain pass"
              | irrAuthoritativeResult d1ReviewRecord /= "pass"
              ]
            , [ "D1 review record scenario must remain uri-r2-c1-only-v1"
              | irrScenarioId d1ReviewRecord /= scenarioIdUriR2C1OnlyV1
              ]
            , [ "D2-L verdict must remain pass"
              | icVerdict d2CheckL /= "pass"
              ]
            , [ "D2-L scenario must remain uri-r2-c1-only-v1"
              | icScenarioId d2CheckL /= scenarioIdUriR2C1OnlyV1
              ]
            , [ "D2-L subject_id must remain uri-r2-c1/cluster-1"
              | icSubjectId d2CheckL /= authoritativeSubjectId
              ]
            , [ "D2-L boundary must include witness-replay/applyInstantiation-instbot-precondition"
              | authoritativeBoundary `isNotInfixOf` icDetails d2CheckL
              ]
            , [ "D2-O verdict must remain pass"
              | icVerdict d2CheckO /= "pass"
              ]
            , [ "D2-O scenario must remain uri-r2-c1-only-v1"
              | icScenarioId d2CheckO /= scenarioIdUriR2C1OnlyV1
              ]
            , [ "D2-O subject_id must remain uri-r2-c1/cluster-1"
              | icSubjectId d2CheckO /= authoritativeSubjectId
              ]
            , [ "D2-O owner must include applyInstantiation semantics"
              | authoritativeOwner `isNotInfixOf` icDetails d2CheckO
              ]
            , [ "D2 stage verdict must remain pass"
              | isvStageResult d2StageVerdict /= "pass"
              ]
            , [ "D2 stage verdict scenario must remain uri-r2-c1-only-v1"
              | isvScenarioId d2StageVerdict /= scenarioIdUriR2C1OnlyV1
              ]
            , [ "D2 trace bundle scenario must remain uri-r2-c1-only-v1"
              | itbScenarioId d2TraceBundle /= scenarioIdUriR2C1OnlyV1
              ]
            , [ "D2 trace bundle subject_id must remain uri-r2-c1/cluster-1"
              | itbSubjectId d2TraceBundle /= authoritativeSubjectId
              ]
            , [ "D2 trace bundle divergence_boundary must remain witness-replay/applyInstantiation-instbot-precondition"
              | itbDivergenceBoundary d2TraceBundle /= authoritativeBoundary
              ]
            , [ "D2 trace bundle owner_account must remain applyInstantiation semantics"
              | authoritativeOwner `isNotInfixOf` itbOwnerAccount d2TraceBundle
              ]
            , [ "D2 review record stage_id must remain D2"
              | irrStageId d2ReviewRecord /= "D2"
              ]
            , [ "D2 review record attempt_verdict must remain accepted"
              | irrAttemptVerdict d2ReviewRecord /= "accepted"
              ]
            , [ "D2 review record stage_action must remain finalize"
              | irrStageAction d2ReviewRecord /= "finalize"
              ]
            , [ "D2 review record stage_result must remain pass"
              | irrStageResult d2ReviewRecord /= "pass"
              ]
            , [ "D2 review record status must remain authoritative"
              | irrStatus d2ReviewRecord /= "authoritative"
              ]
            , [ "D2 review record authoritative_attempt must remain 1"
              | irrAuthoritativeAttempt d2ReviewRecord /= 1
              ]
            , [ "D2 review record authoritative_result must remain pass"
              | irrAuthoritativeResult d2ReviewRecord /= "pass"
              ]
            , [ "D2 review record scenario must remain uri-r2-c1-only-v1"
              | irrScenarioId d2ReviewRecord /= scenarioIdUriR2C1OnlyV1
              ]
            ]
  where
    isNotInfixOf needle haystack = not (needle `isInfixOf` haystack)

rewriteD3Refs :: [String] -> [String]
rewriteD3Refs refs = map rewrite refs
  where
    rewrite ref =
        case stripPrefix "trace://uri-r2-c1/d2/mismatch-localization/" ref of
            Just suffix -> "trace://uri-r2-c1/d3/fixability-probe/" ++ suffix
            Nothing -> "trace://uri-r2-c1/d3/fixability-probe/external/" ++ slugify ref

mkCorrelationId :: PrototypeRequest -> String
mkCorrelationId req =
    prScenarioId req ++ "-d3-attempt-" ++ show (prAttemptId req)

mkCheckArtifact
    :: String
    -> FilePath
    -> String
    -> String
    -> String
    -> String
    -> String
    -> D3CheckArtifact
mkCheckArtifact subjectId evidenceRef correlationId checkId status rejectionTrigger details =
    D3CheckArtifact
        { d3caResult =
            CheckResult
                { ckCheckId = checkId
                , ckStatus = status
                , ckRejectionTrigger = rejectionTrigger
                , ckDetails = details
                }
        , d3caCorrelationId = correlationId
        , d3caSubjectId = subjectId
        , d3caEvidenceRef = evidenceRef
        }

metadataFor :: PrototypeRequest -> Bool -> EvidenceMetadata
metadataFor req includeStage =
    EvidenceMetadata
        { emResearchEntrypointId = prResearchEntrypointId req
        , emStageSelector = prStageSelector req
        , emScenarioId = prScenarioId req
        , emAttemptId = prAttemptId req
        , emStage = if includeStage then Just "D3" else Nothing
        }

writeEvidence :: PrototypePaths -> D3Execution -> IO ()
writeEvidence paths execution = do
    let req = prototypeRequest (d3AppReport execution)
        attemptId = prAttemptId req
    writeJsonFile
        (ppEvidenceDir paths </> "trace-bundle.json")
        (traceBundleJson req execution)
    mapM_
        (\check ->
            writeJsonFile
                (ppEvidenceDir paths </> ("check-" ++ ckCheckId (d3caResult check) ++ ".json"))
                (d3CheckJson req check)
        )
        (d3Checks execution)
    writeJsonFile
        (ppEvidenceDir paths </> "stage-verdict.json")
        (stageVerdictJson req execution attemptId)

traceBundleJson :: PrototypeRequest -> D3Execution -> JValue
traceBundleJson req execution =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("correlation_id", JString (d3CorrelationId execution))
               , ("subject_id", JString authoritativeSubjectId)
               , ("trace_refs", JArray (map JString (d3TraceRefs execution)))
               , ("trace_summary", JArray (map JString (d3TraceSummary execution)))
               , ("fix_hypothesis", JString (d3FixHypothesis execution))
               , ("attempt_verdict", JString (d3AttemptVerdict execution))
               , ("repair_direction", JString (d3RepairDirection execution))
               , ("divergence_boundary", JString (d3DivergenceBoundary execution))
               , ("owner_account", JString (d3OwnerAccount execution))
               ]
        )

d3CheckJson :: PrototypeRequest -> D3CheckArtifact -> JValue
d3CheckJson req check =
    let result = d3caResult check
    in JObject
        ( metadataPairs (metadataFor req False)
            ++ [ ("check_id", JString (ckCheckId result))
               , ("correlation_id", JString (d3caCorrelationId check))
               , ("subject_id", JString (d3caSubjectId check))
               , ("evidence_ref", JString (d3caEvidenceRef check))
               , ("verdict", JString (ckStatus result))
               , ("rejection_trigger", JString (ckRejectionTrigger result))
               , ("details", JString (ckDetails result))
               ]
        )

stageVerdictJson :: PrototypeRequest -> D3Execution -> Int -> JValue
stageVerdictJson req execution attemptId =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("subject_token_ref", JNull)
               , ("checker_results", JArray (map (JString . d3CheckResultRelativePath attemptId . ckCheckId . d3caResult) (d3Checks execution)))
               , ("attempt_verdict", JString (d3AttemptVerdict execution))
               , ("stage_result", JString (d3StageResult execution))
               , ("terminal_reason", JString (stageTerminalReason (d3StageResult execution)))
               ]
        )

stageTerminalReason :: String -> String
stageTerminalReason stageResult
    | stageResult == "inconclusive" = "blocking-stop-condition"
    | otherwise = "none"

decodeCheck :: FilePath -> String -> Either PrototypeError InheritedCheck
decodeCheck sourcePath contents = do
    scenarioId <- requiredStringField sourcePath "scenario_id" contents
    subjectId <- requiredStringField sourcePath "subject_id" contents
    verdict <- requiredStringField sourcePath "verdict" contents
    rejectionTrigger <- requiredStringField sourcePath "rejection_trigger" contents
    details <- requiredStringField sourcePath "details" contents
    pure
        InheritedCheck
            { icScenarioId = scenarioId
            , icSubjectId = subjectId
            , icVerdict = verdict
            , icRejectionTrigger = rejectionTrigger
            , icDetails = details
            }

decodeStageVerdict :: FilePath -> String -> Either PrototypeError InheritedStageVerdict
decodeStageVerdict sourcePath contents = do
    scenarioId <- requiredStringField sourcePath "scenario_id" contents
    stageResult <- requiredStringField sourcePath "stage_result" contents
    pure
        InheritedStageVerdict
            { isvScenarioId = scenarioId
            , isvStageResult = stageResult
            }

decodeTraceBundle :: FilePath -> String -> Either PrototypeError InheritedTraceBundle
decodeTraceBundle sourcePath contents = do
    scenarioId <- requiredStringField sourcePath "scenario_id" contents
    subjectId <- requiredStringField sourcePath "subject_id" contents
    traceRefs <- requiredStringArrayField sourcePath "trace_refs" contents
    divergenceBoundary <- requiredStringField sourcePath "divergence_boundary" contents
    ownerAccount <- requiredStringField sourcePath "owner_account" contents
    pure
        InheritedTraceBundle
            { itbScenarioId = scenarioId
            , itbSubjectId = subjectId
            , itbTraceRefs = traceRefs
            , itbDivergenceBoundary = divergenceBoundary
            , itbOwnerAccount = ownerAccount
            }

decodeReviewRecord :: FilePath -> String -> Either PrototypeError InheritedReviewRecord
decodeReviewRecord sourcePath contents = do
    scenarioId <- requiredStringField sourcePath "scenario_id" contents
    stageId <- requiredStringField sourcePath "stage_id" contents
    attemptVerdict <- requiredStringField sourcePath "attempt_verdict" contents
    stageResult <- requiredStringField sourcePath "stage_result" contents
    stageAction <- requiredStringField sourcePath "stage_action" contents
    status <- requiredStringField sourcePath "status" contents
    authoritativeAttempt <- requiredIntField sourcePath "authoritative_attempt" contents
    authoritativeResult <- requiredStringField sourcePath "authoritative_result" contents
    pure
        InheritedReviewRecord
            { irrScenarioId = scenarioId
            , irrStageId = stageId
            , irrAttemptVerdict = attemptVerdict
            , irrStageResult = stageResult
            , irrStageAction = stageAction
            , irrStatus = status
            , irrAuthoritativeAttempt = authoritativeAttempt
            , irrAuthoritativeResult = authoritativeResult
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

detailsHasAuthoritativeMismatch :: String -> Bool
detailsHasAuthoritativeMismatch details =
    "InstBot expects \8869, got: t9 -> t9" `isInfixOf` details
        || "InstBot expects \\8869, got: t9 -> t9" `isInfixOf` details

requiredStringField :: FilePath -> String -> String -> Either PrototypeError String
requiredStringField sourcePath fieldName contents =
    case extractStringField fieldName contents of
        Just value -> Right value
        Nothing -> Left (MalformedStageInputToken sourcePath ("missing string field: " ++ fieldName))

requiredIntField :: FilePath -> String -> String -> Either PrototypeError Int
requiredIntField sourcePath fieldName contents =
    case extractIntField fieldName contents of
        Just value -> Right value
        Nothing -> Left (MalformedStageInputToken sourcePath ("missing int field: " ++ fieldName))

optionalStringField :: String -> String -> Either PrototypeError (Maybe String)
optionalStringField fieldName contents =
    case findAfter ("\"" ++ fieldName ++ "\": ") contents of
        Nothing -> Right Nothing
        Just rest ->
            case dropWhile isSpace rest of
                ('n' : 'u' : 'l' : 'l' : _) -> Right Nothing
                ('"' : xs) -> Right (Just (parseJsonString xs))
                _ -> Right Nothing

requiredStringArrayField :: FilePath -> String -> String -> Either PrototypeError [String]
requiredStringArrayField sourcePath fieldName contents =
    case extractStringArrayField fieldName contents of
        Just values -> Right values
        Nothing -> Left (MalformedStageInputToken sourcePath ("missing string array field: " ++ fieldName))

extractStringField :: String -> String -> Maybe String
extractStringField key text =
    case findAfter ("\"" ++ key ++ "\": \"") text of
        Nothing -> Nothing
        Just rest -> Just (parseJsonString rest)

extractIntField :: String -> String -> Maybe Int
extractIntField key text =
    case findAfter ("\"" ++ key ++ "\": ") text of
        Nothing -> Nothing
        Just rest ->
            let digits = takeWhile isDigit rest
            in if null digits then Nothing else Just (read digits)

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
                let value = parseJsonString more
                    afterValue = drop (jsonStringTokenLength more) more
                in value : go afterValue
            _ -> []

jsonStringTokenLength :: String -> Int
jsonStringTokenLength = go 0
  where
    go n [] = n
    go n ('"' : _) = n + 1
    go n ('\\' : _ : rest) = go (n + 2) rest
    go n (_ : rest) = go (n + 1) rest

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

stripPrefix :: String -> String -> Maybe String
stripPrefix prefix value
    | take (length prefix) value == prefix = Just (drop (length prefix) value)
    | otherwise = Nothing

slugify :: String -> String
slugify = reverse . dropWhile (== '-') . reverse . go False . map toDash . map toLowerAscii
  where
    go _ [] = []
    go prevDash (ch : rest)
        | ch == '-' && prevDash = go True rest
        | ch == '-' = ch : go True rest
        | otherwise = ch : go False rest

    toDash ch
        | isAlphaNumAscii ch = ch
        | otherwise = '-'

    isAlphaNumAscii ch =
        (ch >= 'a' && ch <= 'z')
            || (ch >= '0' && ch <= '9')

    toLowerAscii ch
        | ch >= 'A' && ch <= 'Z' = toEnum (fromEnum ch + 32)
        | otherwise = ch

boolText :: Bool -> String
boolText flag = if flag then "yes" else "no"

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
