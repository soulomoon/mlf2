module MLF.Research.URI.R2.C1.Prototype.D2 (
    D2CheckArtifact(..),
    D2Execution(..),
    executeD2
) where

import Control.Exception (catch)
import Data.Char (isDigit, isSpace)
import Data.List (intercalate, isInfixOf)
import Data.Maybe (fromMaybe)
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , removePathForcibly
    )
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

import MLF.Research.URI.R2.C1.Prototype.Types

data D2CheckArtifact = D2CheckArtifact
    { d2caResult :: CheckResult
    , d2caCorrelationId :: String
    , d2caSubjectId :: String
    , d2caEvidenceRef :: FilePath
    }
    deriving (Eq, Show)

data D2Execution = D2Execution
    { d2AppReport :: PrototypeReport
    , d2InheritedSubjectTokenPath :: FilePath
    , d2InheritedP2CheckRPath :: FilePath
    , d2InheritedP2CheckWPath :: FilePath
    , d2InheritedP2StageVerdictPath :: FilePath
    , d2InheritedP2TraceBundlePath :: FilePath
    , d2InheritedD1CheckRPath :: FilePath
    , d2InheritedD1StageVerdictPath :: FilePath
    , d2InheritedD1TraceBundlePath :: FilePath
    , d2InheritedD1ReviewRecordPath :: FilePath
    , d2CorrelationId :: String
    , d2TraceRefs :: [String]
    , d2Checks :: [D2CheckArtifact]
    , d2StageResult :: String
    , d2DivergenceBoundary :: String
    , d2OwnerAccount :: String
    , d2TraceSummary :: [String]
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
    , itbCorrelationId :: String
    , itbTraceRefs :: [String]
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

data TraceAlignment = TraceAlignment
    { taStatus :: String
    , taTrigger :: String
    , taDetails :: String
    , taRefs :: [String]
    }
    deriving (Eq, Show)

data Localization = Localization
    { locStatus :: String
    , locTrigger :: String
    , locBoundary :: String
    , locDetails :: String
    }
    deriving (Eq, Show)

data OwnerAccount = OwnerAccount
    { ownerStatus :: String
    , ownerTrigger :: String
    , ownerName :: String
    , ownerDetails :: String
    }
    deriving (Eq, Show)

authoritativeSubjectId :: String
authoritativeSubjectId = "uri-r2-c1/cluster-1"

authoritativeMismatch :: String
authoritativeMismatch = "InstBot expects \8869, got: t9 -> t9"

authoritativeTrigger :: String
authoritativeTrigger = "partial-replay"

expectedTraceSuffixes :: [String]
expectedTraceSuffixes =
    [ "generalize/uri-r2-c1-cluster-1/a-b-a-a-b"
    , "scheme-to-type/uri-r2-c1-cluster-1/a-b-a-a-b"
    , "reify-no-fallback/uri-r2-c1-cluster-1/t5-t5"
    , "witness-replay/uri-r2-c1-cluster-1/t9-n-a-a-n"
    ]

executeD2 :: PrototypeRequest -> IO (Either PrototypeError D2Execution)
executeD2 req = do
    let inheritedSubjectTokenRelPath = p1AuthoritativeSubjectTokenRelativePath
        inheritedP2CheckRRelPath = p2AuthoritativeCheckRRelativePath
        inheritedP2CheckWRelPath = p2AuthoritativeCheckWRelativePath
        inheritedP2StageVerdictRelPath = p2AuthoritativeStageVerdictRelativePath
        inheritedP2TraceBundleRelPath = p2AuthoritativeTraceBundleRelativePath
        inheritedD1CheckRRelPath = d1CheckResultRelativePath 1 "D1-R"
        inheritedD1StageVerdictRelPath = d1StageVerdictRelativePath 1
        inheritedD1TraceBundleRelPath = d1TraceBundleRelativePath 1
        inheritedD1ReviewRecordRelPath = d1AuthoritativeReviewRecordRelativePath
        subjectTokenPath = prRepoRoot req </> inheritedSubjectTokenRelPath
        p2CheckRPath = prRepoRoot req </> inheritedP2CheckRRelPath
        p2CheckWPath = prRepoRoot req </> inheritedP2CheckWRelPath
        p2StageVerdictPath = prRepoRoot req </> inheritedP2StageVerdictRelPath
        p2TraceBundlePath = prRepoRoot req </> inheritedP2TraceBundleRelPath
        d1CheckRPath = prRepoRoot req </> inheritedD1CheckRRelPath
        d1StageVerdictPath = prRepoRoot req </> inheritedD1StageVerdictRelPath
        d1TraceBundlePath = prRepoRoot req </> inheritedD1TraceBundleRelPath
        d1ReviewRecordPath = prRepoRoot req </> inheritedD1ReviewRecordRelPath
        requiredInputs =
            [ (inheritedSubjectTokenRelPath, subjectTokenPath)
            , (inheritedP2CheckRRelPath, p2CheckRPath)
            , (inheritedP2CheckWRelPath, p2CheckWPath)
            , (inheritedP2StageVerdictRelPath, p2StageVerdictPath)
            , (inheritedP2TraceBundleRelPath, p2TraceBundlePath)
            , (inheritedD1CheckRRelPath, d1CheckRPath)
            , (inheritedD1StageVerdictRelPath, d1StageVerdictPath)
            , (inheritedD1TraceBundleRelPath, d1TraceBundlePath)
            , (inheritedD1ReviewRecordRelPath, d1ReviewRecordPath)
            ]
    allExist <- and <$> mapM (doesFileExist . snd) requiredInputs
    if not allExist
        then firstMissing requiredInputs
        else do
            subjectTokenText <- readFile subjectTokenPath
            p2CheckRText <- readFile p2CheckRPath
            p2CheckWText <- readFile p2CheckWPath
            p2StageVerdictText <- readFile p2StageVerdictPath
            p2TraceBundleText <- readFile p2TraceBundlePath
            d1CheckRText <- readFile d1CheckRPath
            d1StageVerdictText <- readFile d1StageVerdictPath
            d1TraceBundleText <- readFile d1TraceBundlePath
            d1ReviewRecordText <- readFile d1ReviewRecordPath
            case
                    decodeD2Inputs
                        inheritedSubjectTokenRelPath
                        subjectTokenText
                        inheritedP2CheckRRelPath
                        p2CheckRText
                        inheritedP2CheckWRelPath
                        p2CheckWText
                        inheritedP2StageVerdictRelPath
                        p2StageVerdictText
                        inheritedP2TraceBundleRelPath
                        p2TraceBundleText
                        inheritedD1CheckRRelPath
                        d1CheckRText
                        inheritedD1StageVerdictRelPath
                        d1StageVerdictText
                        inheritedD1TraceBundleRelPath
                        d1TraceBundleText
                        inheritedD1ReviewRecordRelPath
                        d1ReviewRecordText
                of
                Left err -> pure (Left err)
                Right
                    ( subjectToken
                    , p2CheckR
                    , p2CheckW
                    , p2StageVerdict
                    , p2TraceBundle
                    , d1CheckR
                    , d1StageVerdict
                    , d1TraceBundle
                    , d1ReviewRecord
                    ) -> do
                        let paths = prototypePaths req
                        cleanDir (ppEvidenceDir paths)
                        createDirectoryIfMissing True (ppEvidenceDir paths)
                        let execution =
                                finalizeExecution
                                    req
                                    inheritedSubjectTokenRelPath
                                    inheritedP2CheckRRelPath
                                    inheritedP2CheckWRelPath
                                    inheritedP2StageVerdictRelPath
                                    inheritedP2TraceBundleRelPath
                                    inheritedD1CheckRRelPath
                                    inheritedD1StageVerdictRelPath
                                    inheritedD1TraceBundleRelPath
                                    inheritedD1ReviewRecordRelPath
                                    subjectToken
                                    p2CheckR
                                    p2CheckW
                                    p2StageVerdict
                                    p2TraceBundle
                                    d1CheckR
                                    d1StageVerdict
                                    d1TraceBundle
                                    d1ReviewRecord
                        writeEvidence paths execution
                        pure (Right execution)
  where
    firstMissing [] = pure (Left (MissingStageInputToken ""))
    firstMissing ((relPath, fullPath) : rest) = do
        exists <- doesFileExist fullPath
        if exists
            then firstMissing rest
            else pure (Left (MissingStageInputToken relPath))

decodeD2Inputs
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
    -> Either
        PrototypeError
        ( SubjectToken
        , InheritedCheck
        , InheritedCheck
        , InheritedStageVerdict
        , InheritedTraceBundle
        , InheritedCheck
        , InheritedStageVerdict
        , InheritedTraceBundle
        , InheritedReviewRecord
        )
decodeD2Inputs
    subjectTokenPath
    subjectTokenText
    p2CheckRPath
    p2CheckRText
    p2CheckWPath
    p2CheckWText
    p2StageVerdictPath
    p2StageVerdictText
    p2TraceBundlePath
    p2TraceBundleText
    d1CheckRPath
    d1CheckRText
    d1StageVerdictPath
    d1StageVerdictText
    d1TraceBundlePath
    d1TraceBundleText
    d1ReviewRecordPath
    d1ReviewRecordText = do
        subjectToken <- decodeSubjectToken subjectTokenPath subjectTokenText
        p2CheckR <- decodeCheck p2CheckRPath p2CheckRText
        p2CheckW <- decodeCheck p2CheckWPath p2CheckWText
        p2StageVerdict <- decodeStageVerdict p2StageVerdictPath p2StageVerdictText
        p2TraceBundle <- decodeTraceBundle p2TraceBundlePath p2TraceBundleText
        d1CheckR <- decodeCheck d1CheckRPath d1CheckRText
        d1StageVerdict <- decodeStageVerdict d1StageVerdictPath d1StageVerdictText
        d1TraceBundle <- decodeTraceBundle d1TraceBundlePath d1TraceBundleText
        d1ReviewRecord <- decodeReviewRecord d1ReviewRecordPath d1ReviewRecordText
        pure
            ( subjectToken
            , p2CheckR
            , p2CheckW
            , p2StageVerdict
            , p2TraceBundle
            , d1CheckR
            , d1StageVerdict
            , d1TraceBundle
            , d1ReviewRecord
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
    -> SubjectToken
    -> InheritedCheck
    -> InheritedCheck
    -> InheritedStageVerdict
    -> InheritedTraceBundle
    -> InheritedCheck
    -> InheritedStageVerdict
    -> InheritedTraceBundle
    -> InheritedReviewRecord
    -> D2Execution
finalizeExecution
    req
    subjectTokenPath
    p2CheckRPath
    p2CheckWPath
    p2StageVerdictPath
    p2TraceBundlePath
    d1CheckRPath
    d1StageVerdictPath
    d1TraceBundlePath
    d1ReviewRecordPath
    subjectToken
    p2CheckR
    p2CheckW
    p2StageVerdict
    p2TraceBundle
    d1CheckR
    d1StageVerdict
    d1TraceBundle
    d1ReviewRecord =
        let correlationId = mkCorrelationId req
            subjectId = stSubjectId subjectToken
            traceBundleRef = d2TraceBundleRelativePath (prAttemptId req)
            continuityIssues =
                continuityMismatches
                    subjectToken
                    p2CheckR
                    p2CheckW
                    p2StageVerdict
                    p2TraceBundle
                    d1CheckR
                    d1StageVerdict
                    d1TraceBundle
                    d1ReviewRecord
            alignment = evaluateTraceAlignment continuityIssues p2TraceBundle d1TraceBundle
            localization =
                evaluateLocalization continuityIssues p2CheckR p2CheckW d1CheckR alignment
            owner =
                evaluateOwner continuityIssues d1CheckR localization
            checkT =
                mkCheckArtifact
                    subjectId
                    traceBundleRef
                    correlationId
                    "D2-T"
                    (taStatus alignment)
                    (taTrigger alignment)
                    (taDetails alignment)
            checkL =
                mkCheckArtifact
                    subjectId
                    traceBundleRef
                    correlationId
                    "D2-L"
                    (locStatus localization)
                    (locTrigger localization)
                    (locDetails localization)
            checkO =
                mkCheckArtifact
                    subjectId
                    traceBundleRef
                    correlationId
                    "D2-O"
                    (ownerStatus owner)
                    (ownerTrigger owner)
                    (ownerDetails owner)
            checks = [checkT, checkL, checkO]
            stageResult = stageResultFromStatuses (map (ckStatus . d2caResult) checks)
            summary =
                [ "D2-T aligned P2 and D1 replay trace segments under one correlation id for `generalize -> scheme-to-type -> reify-no-fallback -> witness-replay`."
                , "D2-L localized the earliest divergence boundary where replay identity first splits from the observed mismatch."
                , "D2-O assigned one bounded owner account and excluded adjacent non-owner categories for this attempt."
                , "Divergence boundary: " ++ locBoundary localization ++ "."
                , "Owner account: " ++ ownerName owner ++ "."
                ]
                    ++ map (\check -> ckCheckId (d2caResult check) ++ ": " ++ ckDetails (d2caResult check)) checks
            report =
                PrototypeReport
                    { prototypeRequest = req
                    , prototypeCandidates = []
                    , prototypeP1C = d2caResult checkT
                    , prototypeP1N = d2caResult checkL
                    , prototypeP1U = d2caResult checkO
                    , prototypeStageResult = stageResult
                    , prototypeSubjectToken = Nothing
                    , prototypeTraceSummary = summary
                    , prototypeRawObservationCount = length checks
                    }
        in D2Execution
            { d2AppReport = report
            , d2InheritedSubjectTokenPath = subjectTokenPath
            , d2InheritedP2CheckRPath = p2CheckRPath
            , d2InheritedP2CheckWPath = p2CheckWPath
            , d2InheritedP2StageVerdictPath = p2StageVerdictPath
            , d2InheritedP2TraceBundlePath = p2TraceBundlePath
            , d2InheritedD1CheckRPath = d1CheckRPath
            , d2InheritedD1StageVerdictPath = d1StageVerdictPath
            , d2InheritedD1TraceBundlePath = d1TraceBundlePath
            , d2InheritedD1ReviewRecordPath = d1ReviewRecordPath
            , d2CorrelationId = correlationId
            , d2TraceRefs = taRefs alignment
            , d2Checks = checks
            , d2StageResult = stageResult
            , d2DivergenceBoundary = locBoundary localization
            , d2OwnerAccount = ownerName owner
            , d2TraceSummary = summary
            }

evaluateTraceAlignment
    :: [String]
    -> InheritedTraceBundle
    -> InheritedTraceBundle
    -> TraceAlignment
evaluateTraceAlignment continuityIssues p2TraceBundle d1TraceBundle
    | not (null continuityIssues) =
        TraceAlignment
            { taStatus = "semantic-negative"
            , taTrigger = "inconsistent-trace"
            , taDetails = "continuity mismatch blocks D2-T: " ++ intercalate "; " continuityIssues
            , taRefs = mappedD2Refs expectedTraceSuffixes
            }
    | otherwise =
        let p2Suffixes =
                mapMaybeStripPrefix "trace://uri-r2-c1/p2/" (itbTraceRefs p2TraceBundle)
            d1Suffixes =
                mapMaybeStripPrefix "trace://uri-r2-c1/d1/replay/" (itbTraceRefs d1TraceBundle)
            missingRequired =
                [ suffix
                | suffix <- expectedTraceSuffixes
                , suffix `notElem` p2Suffixes
                ]
            suffixesAligned = p2Suffixes == d1Suffixes
        in if suffixesAligned && null missingRequired
            then
                TraceAlignment
                    { taStatus = "pass"
                    , taTrigger = "none"
                    , taDetails =
                        "aligned trace suffixes: "
                            ++ intercalate " -> " p2Suffixes
                            ++ "; inherited correlations: "
                            ++ itbCorrelationId p2TraceBundle
                            ++ " => "
                            ++ itbCorrelationId d1TraceBundle
                    , taRefs = mappedD2Refs p2Suffixes
                    }
            else
                TraceAlignment
                    { taStatus = "semantic-negative"
                    , taTrigger = "inconsistent-trace"
                    , taDetails =
                        "trace alignment mismatch: p2_suffixes="
                            ++ show p2Suffixes
                            ++ "; d1_suffixes="
                            ++ show d1Suffixes
                            ++ "; missing_required="
                            ++ show missingRequired
                    , taRefs = mappedD2Refs (if null p2Suffixes then expectedTraceSuffixes else p2Suffixes)
                    }

evaluateLocalization
    :: [String]
    -> InheritedCheck
    -> InheritedCheck
    -> InheritedCheck
    -> TraceAlignment
    -> Localization
evaluateLocalization continuityIssues p2CheckR p2CheckW d1CheckR alignment
    | not (null continuityIssues) =
        Localization
            { locStatus = "semantic-negative"
            , locTrigger = "inconsistent-trace"
            , locBoundary = "unresolved-continuity-blocked"
            , locDetails = "cannot localize divergence while continuity mismatches remain"
            }
    | taStatus alignment /= "pass" =
        Localization
            { locStatus = "inconclusive"
            , locTrigger = "blocking-stop-condition"
            , locBoundary = "unresolved-trace-alignment-blocked"
            , locDetails = "cannot localize first divergence without D2-T alignment pass"
            }
    | otherwise =
        let p2ReifyShape = extractAfterNeedle "reifyTypeWithNamedSetNoFallback => " (icDetails p2CheckR)
            p2HasMismatch = detailsHasAuthoritativeMismatch (icDetails p2CheckW)
            d1HasMismatch = detailsHasAuthoritativeMismatch (icDetails d1CheckR)
            d1HasApplyInst = "applyInstantiation diagnostic failed" `isInfixOf` icDetails d1CheckR
            boundaryId = "witness-replay/applyInstantiation-instbot-precondition"
            expectedShape = fromMaybe "unknown" p2ReifyShape
        in if icVerdict p2CheckR == "pass"
            && icRejectionTrigger p2CheckR == "none"
            && p2HasMismatch
            && d1HasMismatch
            && d1HasApplyInst
            then
                Localization
                    { locStatus = "pass"
                    , locTrigger = "none"
                    , locBoundary = boundaryId
                    , locDetails =
                        "first divergence boundary="
                            ++ boundaryId
                            ++ "; expected replay lane shape from no-fallback reification="
                            ++ expectedShape
                            ++ "; observed witness replay path ends at applyInstantiation mismatch="
                            ++ authoritativeMismatch
                    }
            else
                Localization
                    { locStatus = "semantic-negative"
                    , locTrigger = "inconsistent-trace"
                    , locBoundary = "unresolved-mismatch-boundary"
                    , locDetails =
                        "localization prerequisites failed: p2_reify_verdict="
                            ++ icVerdict p2CheckR
                            ++ "; p2_reify_trigger="
                            ++ icRejectionTrigger p2CheckR
                            ++ "; p2_has_mismatch="
                            ++ boolText p2HasMismatch
                            ++ "; d1_has_mismatch="
                            ++ boolText d1HasMismatch
                            ++ "; d1_has_applyInstantiation_diagnostic="
                            ++ boolText d1HasApplyInst
                    }

evaluateOwner :: [String] -> InheritedCheck -> Localization -> OwnerAccount
evaluateOwner continuityIssues d1CheckR localization
    | not (null continuityIssues) =
        OwnerAccount
            { ownerStatus = "semantic-negative"
            , ownerTrigger = "inconsistent-trace"
            , ownerName = "unresolved-owner-continuity-blocked"
            , ownerDetails = "cannot assign single owner while continuity mismatches remain"
            }
    | locStatus localization /= "pass" =
        OwnerAccount
            { ownerStatus = "inconclusive"
            , ownerTrigger = "blocking-stop-condition"
            , ownerName = "unresolved-owner-localization-blocked"
            , ownerDetails = "cannot assign single owner without a localized divergence boundary"
            }
    | "applyInstantiation diagnostic failed" `isInfixOf` icDetails d1CheckR =
        OwnerAccount
            { ownerStatus = "pass"
            , ownerTrigger = "none"
            , ownerName = "applyInstantiation semantics (`MLF.Elab.Inst.applyInstantiation`, InstBot branch)"
            , ownerDetails =
                "owner=applyInstantiation semantics (`MLF.Elab.Inst.applyInstantiation`, InstBot branch); boundary="
                    ++ locBoundary localization
                    ++ "; non-owners: witness construction preserved the same witness text across P2 and D1, replay-domain reconstruction preserved aligned trace suffixes, no-fallback reification output remained `t5 -> t5` before witness replay"
            }
    | otherwise =
        OwnerAccount
            { ownerStatus = "semantic-negative"
            , ownerTrigger = "inconsistent-trace"
            , ownerName = "unresolved-owner-applyInstantiation-not-observed"
            , ownerDetails = "D1-R did not retain the applyInstantiation diagnostic needed for owner assignment"
            }

mappedD2Refs :: [String] -> [String]
mappedD2Refs suffixes =
    map (\suffix -> "trace://uri-r2-c1/d2/mismatch-localization/" ++ suffix) suffixes

continuityMismatches
    :: SubjectToken
    -> InheritedCheck
    -> InheritedCheck
    -> InheritedStageVerdict
    -> InheritedTraceBundle
    -> InheritedCheck
    -> InheritedStageVerdict
    -> InheritedTraceBundle
    -> InheritedReviewRecord
    -> [String]
continuityMismatches
    token
    p2CheckR
    p2CheckW
    p2StageVerdict
    p2TraceBundle
    d1CheckR
    d1StageVerdict
    d1TraceBundle
    d1ReviewRecord =
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
            , [ "P2-R scenario must remain uri-r2-c1-only-v1"
              | icScenarioId p2CheckR /= scenarioIdUriR2C1OnlyV1
              ]
            , [ "P2-W scenario must remain uri-r2-c1-only-v1"
              | icScenarioId p2CheckW /= scenarioIdUriR2C1OnlyV1
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
            , [ "P2 trace bundle scenario must remain uri-r2-c1-only-v1"
              | itbScenarioId p2TraceBundle /= scenarioIdUriR2C1OnlyV1
              ]
            , [ "P2 trace bundle subject_id must remain uri-r2-c1/cluster-1"
              | itbSubjectId p2TraceBundle /= authoritativeSubjectId
              ]
            , [ "D1-R scenario must remain uri-r2-c1-only-v1"
              | icScenarioId d1CheckR /= scenarioIdUriR2C1OnlyV1
              ]
            , [ "D1-R details must preserve exact-bounded-replay-failure classification"
              | "classification=exact-bounded-replay-failure" `isNotInfixOf` icDetails d1CheckR
              ]
            , [ "D1 stage verdict must remain pass"
              | isvStageResult d1StageVerdict /= "pass"
              ]
            , [ "D1 trace bundle scenario must remain uri-r2-c1-only-v1"
              | itbScenarioId d1TraceBundle /= scenarioIdUriR2C1OnlyV1
              ]
            , [ "D1 trace bundle subject_id must remain uri-r2-c1/cluster-1"
              | itbSubjectId d1TraceBundle /= authoritativeSubjectId
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
            ]
  where
    isNotInfixOf needle haystack = not (needle `isInfixOf` haystack)

mkCorrelationId :: PrototypeRequest -> String
mkCorrelationId req =
    prScenarioId req ++ "-d2-attempt-" ++ show (prAttemptId req)

mkCheckArtifact
    :: String
    -> FilePath
    -> String
    -> String
    -> String
    -> String
    -> String
    -> D2CheckArtifact
mkCheckArtifact subjectId evidenceRef correlationId checkId status rejectionTrigger details =
    D2CheckArtifact
        { d2caResult =
            CheckResult
                { ckCheckId = checkId
                , ckStatus = status
                , ckRejectionTrigger = rejectionTrigger
                , ckDetails = details
                }
        , d2caCorrelationId = correlationId
        , d2caSubjectId = subjectId
        , d2caEvidenceRef = evidenceRef
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
        , emStage = if includeStage then Just "D2" else Nothing
        }

writeEvidence :: PrototypePaths -> D2Execution -> IO ()
writeEvidence paths execution = do
    let req = prototypeRequest (d2AppReport execution)
        attemptId = prAttemptId req
    writeJsonFile
        (ppEvidenceDir paths </> "trace-bundle.json")
        (traceBundleJson req execution)
    mapM_
        (\check ->
            writeJsonFile
                (ppEvidenceDir paths </> ("check-" ++ ckCheckId (d2caResult check) ++ ".json"))
                (d2CheckJson req check)
        )
        (d2Checks execution)
    writeJsonFile
        (ppEvidenceDir paths </> "stage-verdict.json")
        (stageVerdictJson req execution attemptId)

traceBundleJson :: PrototypeRequest -> D2Execution -> JValue
traceBundleJson req execution =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("correlation_id", JString (d2CorrelationId execution))
               , ("subject_id", JString authoritativeSubjectId)
               , ("trace_refs", JArray (map JString (d2TraceRefs execution)))
               , ("trace_summary", JArray (map JString (d2TraceSummary execution)))
               , ("divergence_boundary", JString (d2DivergenceBoundary execution))
               , ("owner_account", JString (d2OwnerAccount execution))
               ]
        )

d2CheckJson :: PrototypeRequest -> D2CheckArtifact -> JValue
d2CheckJson req check =
    let result = d2caResult check
    in JObject
        ( metadataPairs (metadataFor req False)
            ++ [ ("check_id", JString (ckCheckId result))
               , ("correlation_id", JString (d2caCorrelationId check))
               , ("subject_id", JString (d2caSubjectId check))
               , ("evidence_ref", JString (d2caEvidenceRef check))
               , ("verdict", JString (ckStatus result))
               , ("rejection_trigger", JString (ckRejectionTrigger result))
               , ("details", JString (ckDetails result))
               ]
        )

stageVerdictJson :: PrototypeRequest -> D2Execution -> Int -> JValue
stageVerdictJson req execution attemptId =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("subject_token_ref", JNull)
               , ("checker_results", JArray (map (JString . d2CheckResultRelativePath attemptId . ckCheckId . d2caResult) (d2Checks execution)))
               , ("stage_result", JString (d2StageResult execution))
               , ("terminal_reason", JString (stageTerminalReason (d2StageResult execution)))
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
    correlationId <- requiredStringField sourcePath "correlation_id" contents
    traceRefs <- requiredStringArrayField sourcePath "trace_refs" contents
    pure
        InheritedTraceBundle
            { itbScenarioId = scenarioId
            , itbSubjectId = subjectId
            , itbCorrelationId = correlationId
            , itbTraceRefs = traceRefs
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

detailsHasAuthoritativeMismatch :: String -> Bool
detailsHasAuthoritativeMismatch details =
    "InstBot expects \8869, got: t9 -> t9" `isInfixOf` details
        || "InstBot expects \\8869, got: t9 -> t9" `isInfixOf` details

extractAfterNeedle :: String -> String -> Maybe String
extractAfterNeedle needle text =
    case findAfter needle text of
        Nothing -> Nothing
        Just rest -> Just (trim rest)

mapMaybeStripPrefix :: String -> [String] -> [String]
mapMaybeStripPrefix _ [] = []
mapMaybeStripPrefix prefix (value : rest) =
    case stripPrefix prefix value of
        Nothing -> mapMaybeStripPrefix prefix rest
        Just suffix -> suffix : mapMaybeStripPrefix prefix rest

boolText :: Bool -> String
boolText flag = if flag then "yes" else "no"

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

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

stripPrefix :: String -> String -> Maybe String
stripPrefix prefix value
    | take (length prefix) value == prefix = Just (drop (length prefix) value)
    | otherwise = Nothing

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
