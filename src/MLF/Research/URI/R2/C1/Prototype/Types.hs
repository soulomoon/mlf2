module MLF.Research.URI.R2.C1.Prototype.Types (
    PrototypeRequest(..),
    PrototypeError(..),
    CandidateKind(..),
    CandidateRecord(..),
    CheckResult(..),
    EvidenceMetadata(..),
    CandidateSelectionRule(..),
    SubjectScope(..),
    ProvenanceAnchor(..),
    OwnerFamilyStatus(..),
    SubjectToken(..),
    TraceBundle(..),
    CheckerArtifact(..),
    StageVerdictArtifact(..),
    PrototypeReport(..),
    PrototypePaths(..),
    researchEntrypointId,
    replayRootCauseEntrypointId,
    stageSelectorD1,
    stageSelectorD2,
    stageSelectorP1,
    stageSelectorP2,
    stageSelectorP3,
    stageSelectorP4,
    scenarioIdUriR2C1OnlyV1,
    boundedSubjectId,
    d1ArtifactRelativePath,
    d2ArtifactRelativePath,
    artifactRelativePath,
    p2ArtifactRelativePath,
    p3ArtifactRelativePath,
    p4ArtifactRelativePath,
    d1AttemptEvidenceRelativeDir,
    d2AttemptEvidenceRelativeDir,
    attemptEvidenceRelativeDir,
    p2AttemptEvidenceRelativeDir,
    p3AttemptEvidenceRelativeDir,
    p4AttemptEvidenceRelativeDir,
    d1AttemptEvidenceFileRelativePath,
    d2AttemptEvidenceFileRelativePath,
    attemptEvidenceFileRelativePath,
    p2AttemptEvidenceFileRelativePath,
    p3AttemptEvidenceFileRelativePath,
    p4AttemptEvidenceFileRelativePath,
    candidateInventoryRelativePath,
    candidateSelectionRuleRelativePath,
    d1CheckResultRelativePath,
    d2CheckResultRelativePath,
    checkResultRelativePath,
    p2CheckResultRelativePath,
    p3CheckResultRelativePath,
    p4CheckResultRelativePath,
    d1StageVerdictRelativePath,
    d2StageVerdictRelativePath,
    stageVerdictRelativePath,
    p2StageVerdictRelativePath,
    p3StageVerdictRelativePath,
    p4DecisionVerdictRelativePath,
    p4StageConsumptionRelativePath,
    subjectTokenRelativePath,
    p2SubjectTokenRelativePath,
    p3SubjectTokenRelativePath,
    d1TraceBundleRelativePath,
    d2TraceBundleRelativePath,
    traceBundleRelativePath,
    p2TraceBundleRelativePath,
    p3TraceBundleRelativePath,
    p4TraceBundleRelativePath,
    p1AuthoritativeSubjectTokenRelativePath,
    p2AuthoritativeCheckRRelativePath,
    p2AuthoritativeCheckWRelativePath,
    p2AuthoritativeStageVerdictRelativePath,
    p2AuthoritativeTraceBundleRelativePath,
    d1AuthoritativeReviewRecordRelativePath,
    p2AuthoritativeReviewRecordRelativePath,
    p1AuthoritativeReviewRecordRelativePath,
    p3AuthoritativeReviewRecordRelativePath,
    prototypePaths,
    candidateKindText,
    stageResultText,
    prototypeSubjectId,
    normalizedRejectionTriggers
) where

import System.FilePath ((</>))

data PrototypeRequest = PrototypeRequest
    { prRepoRoot :: FilePath
    , prResearchEntrypointId :: String
    , prStageSelector :: String
    , prScenarioId :: String
    , prAttemptId :: Int
    }
    deriving (Eq, Show)

data PrototypeError
    = UnsupportedResearchEntrypoint String
    | UnsupportedStageSelector String
    | UnsupportedScenario String
    | UnsupportedAttemptId Int
    | MissingStageInputToken FilePath
    | MalformedStageInputToken FilePath String
    | UnsupportedBoundedSubject String
    | TokenScenarioMismatch String
    deriving (Eq, Show)

data CandidateKind
    = CandidateLocalRoot
    | CandidateEquivalentLocalCluster
    deriving (Eq, Show)

data CandidateRecord = CandidateRecord
    { crCandidateId :: String
    , crCandidateKind :: CandidateKind
    , crNormalizationBasis :: String
    , crAdmissibilityVerdict :: String
    , crRejectionTrigger :: String
    , crObservationCount :: Int
    }
    deriving (Eq, Show)

data CheckResult = CheckResult
    { ckCheckId :: String
    , ckStatus :: String
    , ckRejectionTrigger :: String
    , ckDetails :: String
    }
    deriving (Eq, Show)

data EvidenceMetadata = EvidenceMetadata
    { emResearchEntrypointId :: String
    , emStageSelector :: String
    , emScenarioId :: String
    , emAttemptId :: Int
    , emStage :: Maybe String
    }
    deriving (Eq, Show)

data CandidateSelectionRule = CandidateSelectionRule
    { csrCandidateUniverse :: String
    , csrNormalization :: String
    , csrAdmissibilityTest :: String
    , csrOutcome :: String
    }
    deriving (Eq, Show)

data SubjectScope = SubjectScope
    { ssScenarioId :: String
    , ssBoundedSubject :: String
    }
    deriving (Eq, Show)

data ProvenanceAnchor = ProvenanceAnchor
    { paOriginStage :: String
    , paCandidateId :: String
    , paCandidateInventoryRef :: FilePath
    , paNormalizationBasis :: String
    , paDiscoveryTraceRef :: Maybe String
    }
    deriving (Eq, Show)

data OwnerFamilyStatus = OwnerFamilyStatus
    { ofsKind :: String
    , ofsFamilyId :: Maybe String
    }
    deriving (Eq, Show)

data SubjectToken = SubjectToken
    { stSubjectId :: String
    , stSubjectKind :: CandidateKind
    , stSubjectScope :: SubjectScope
    , stProvenanceAnchor :: ProvenanceAnchor
    , stOwnerFamilyStatus :: OwnerFamilyStatus
    , stTraceHandles :: [String]
    }
    deriving (Eq, Show)

data TraceBundle = TraceBundle
    { tbMetadata :: EvidenceMetadata
    , tbCorrelationId :: Maybe String
    , tbSubjectId :: Maybe String
    , tbTraceRefs :: [String]
    }
    deriving (Eq, Show)

data CheckerArtifact = CheckerArtifact
    { caMetadata :: EvidenceMetadata
    , caCheckId :: String
    , caSubjectId :: Maybe String
    , caEvidenceRef :: FilePath
    , caVerdict :: String
    , caRejectionTrigger :: String
    }
    deriving (Eq, Show)

data StageVerdictArtifact = StageVerdictArtifact
    { svaMetadata :: EvidenceMetadata
    , svaSubjectTokenRef :: Maybe FilePath
    , svaCheckerResults :: [FilePath]
    , svaStageResult :: String
    , svaTerminalReason :: String
    }
    deriving (Eq, Show)

data PrototypeReport = PrototypeReport
    { prototypeRequest :: PrototypeRequest
    , prototypeCandidates :: [CandidateRecord]
    , prototypeP1C :: CheckResult
    , prototypeP1N :: CheckResult
    , prototypeP1U :: CheckResult
    , prototypeStageResult :: String
    , prototypeSubjectToken :: Maybe SubjectToken
    , prototypeTraceSummary :: [String]
    , prototypeRawObservationCount :: Int
    }
    deriving (Eq, Show)

data PrototypePaths = PrototypePaths
    { ppEvidenceDir :: FilePath
    , ppArtifactPath :: FilePath
    }
    deriving (Eq, Show)

researchEntrypointId :: String
researchEntrypointId = "uri-r2-c1-prototype-entrypoint-v1"

replayRootCauseEntrypointId :: String
replayRootCauseEntrypointId = "uri-r2-c1-p2-replay-root-cause-v1"

stageSelectorD1 :: String
stageSelectorD1 = "D1-replay-reproduction"

stageSelectorD2 :: String
stageSelectorD2 = "D2-mismatch-localization"

stageSelectorP1 :: String
stageSelectorP1 = "P1-subject-discovery"

stageSelectorP2 :: String
stageSelectorP2 = "P2-provenance-preservation"

stageSelectorP3 :: String
stageSelectorP3 = "P3-safety-validation"

stageSelectorP4 :: String
stageSelectorP4 = "P4-prototype-decision-gate"

scenarioIdUriR2C1OnlyV1 :: String
scenarioIdUriR2C1OnlyV1 = "uri-r2-c1-only-v1"

boundedSubjectId :: String
boundedSubjectId = "URI-R2-C1"

d1ArtifactRelativePath :: FilePath
d1ArtifactRelativePath =
    "docs"
        </> "plans"
        </> "2026-03-16-uri-r2-c1-d1-replay-reproduction-contract.md"

d2ArtifactRelativePath :: FilePath
d2ArtifactRelativePath =
    "docs"
        </> "plans"
        </> "2026-03-16-uri-r2-c1-d2-replay-mismatch-localization.md"

artifactRelativePath :: FilePath
artifactRelativePath =
    "docs"
        </> "plans"
        </> "2026-03-15-uri-r2-c1-p1-subject-discovery-prototype.md"

p2ArtifactRelativePath :: FilePath
p2ArtifactRelativePath =
    "docs"
        </> "plans"
        </> "2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md"

p3ArtifactRelativePath :: FilePath
p3ArtifactRelativePath =
    "docs"
        </> "plans"
        </> "2026-03-15-uri-r2-c1-p3-safety-validation-prototype.md"

p4ArtifactRelativePath :: FilePath
p4ArtifactRelativePath =
    "docs"
        </> "plans"
        </> "2026-03-15-uri-r2-c1-p4-prototype-decision-gate.md"

d1AttemptEvidenceRelativeDir :: Int -> FilePath
d1AttemptEvidenceRelativeDir attemptId =
    "orchestrator"
        </> "rounds"
        </> "round-020"
        </> "evidence"
        </> "D1"
        </> ("attempt-" ++ show attemptId)

d2AttemptEvidenceRelativeDir :: Int -> FilePath
d2AttemptEvidenceRelativeDir attemptId =
    "orchestrator"
        </> "rounds"
        </> "round-021"
        </> "evidence"
        </> "D2"
        </> ("attempt-" ++ show attemptId)

attemptEvidenceRelativeDir :: Int -> FilePath
attemptEvidenceRelativeDir attemptId =
    "orchestrator"
        </> "rounds"
        </> "round-016"
        </> "evidence"
        </> "P1"
        </> ("attempt-" ++ show attemptId)

p2AttemptEvidenceRelativeDir :: Int -> FilePath
p2AttemptEvidenceRelativeDir attemptId =
    "orchestrator"
        </> "rounds"
        </> "round-017"
        </> "evidence"
        </> "P2"
        </> ("attempt-" ++ show attemptId)

p3AttemptEvidenceRelativeDir :: Int -> FilePath
p3AttemptEvidenceRelativeDir attemptId =
    "orchestrator"
        </> "rounds"
        </> "round-018"
        </> "evidence"
        </> "P3"
        </> ("attempt-" ++ show attemptId)

p4AttemptEvidenceRelativeDir :: Int -> FilePath
p4AttemptEvidenceRelativeDir attemptId =
    "orchestrator"
        </> "rounds"
        </> "round-019"
        </> "evidence"
        </> "P4"
        </> ("attempt-" ++ show attemptId)

candidateInventoryRelativePath :: Int -> FilePath
candidateInventoryRelativePath attemptId =
    attemptEvidenceRelativeDir attemptId </> "candidate-inventory.json"

attemptEvidenceFileRelativePath :: Int -> FilePath -> FilePath
attemptEvidenceFileRelativePath attemptId fileName =
    attemptEvidenceRelativeDir attemptId </> fileName

p2AttemptEvidenceFileRelativePath :: Int -> FilePath -> FilePath
p2AttemptEvidenceFileRelativePath attemptId fileName =
    p2AttemptEvidenceRelativeDir attemptId </> fileName

p3AttemptEvidenceFileRelativePath :: Int -> FilePath -> FilePath
p3AttemptEvidenceFileRelativePath attemptId fileName =
    p3AttemptEvidenceRelativeDir attemptId </> fileName

p4AttemptEvidenceFileRelativePath :: Int -> FilePath -> FilePath
p4AttemptEvidenceFileRelativePath attemptId fileName =
    p4AttemptEvidenceRelativeDir attemptId </> fileName

d1AttemptEvidenceFileRelativePath :: Int -> FilePath -> FilePath
d1AttemptEvidenceFileRelativePath attemptId fileName =
    d1AttemptEvidenceRelativeDir attemptId </> fileName

d2AttemptEvidenceFileRelativePath :: Int -> FilePath -> FilePath
d2AttemptEvidenceFileRelativePath attemptId fileName =
    d2AttemptEvidenceRelativeDir attemptId </> fileName

candidateSelectionRuleRelativePath :: Int -> FilePath
candidateSelectionRuleRelativePath attemptId =
    attemptEvidenceFileRelativePath attemptId "candidate-selection-rule.json"

d1CheckResultRelativePath :: Int -> String -> FilePath
d1CheckResultRelativePath attemptId checkId =
    d1AttemptEvidenceFileRelativePath attemptId ("check-" ++ checkId ++ ".json")

d2CheckResultRelativePath :: Int -> String -> FilePath
d2CheckResultRelativePath attemptId checkId =
    d2AttemptEvidenceFileRelativePath attemptId ("check-" ++ checkId ++ ".json")

checkResultRelativePath :: Int -> String -> FilePath
checkResultRelativePath attemptId checkId =
    attemptEvidenceFileRelativePath attemptId ("check-" ++ checkId ++ ".json")

p2CheckResultRelativePath :: Int -> String -> FilePath
p2CheckResultRelativePath attemptId checkId =
    p2AttemptEvidenceFileRelativePath attemptId ("check-" ++ checkId ++ ".json")

p3CheckResultRelativePath :: Int -> String -> FilePath
p3CheckResultRelativePath attemptId checkId =
    p3AttemptEvidenceFileRelativePath attemptId ("check-" ++ checkId ++ ".json")

p4CheckResultRelativePath :: Int -> String -> FilePath
p4CheckResultRelativePath attemptId checkId =
    p4AttemptEvidenceFileRelativePath attemptId ("check-" ++ checkId ++ ".json")

d1StageVerdictRelativePath :: Int -> FilePath
d1StageVerdictRelativePath attemptId =
    d1AttemptEvidenceFileRelativePath attemptId "stage-verdict.json"

d2StageVerdictRelativePath :: Int -> FilePath
d2StageVerdictRelativePath attemptId =
    d2AttemptEvidenceFileRelativePath attemptId "stage-verdict.json"

stageVerdictRelativePath :: Int -> FilePath
stageVerdictRelativePath attemptId =
    attemptEvidenceFileRelativePath attemptId "stage-verdict.json"

p2StageVerdictRelativePath :: Int -> FilePath
p2StageVerdictRelativePath attemptId =
    p2AttemptEvidenceFileRelativePath attemptId "stage-verdict.json"

p3StageVerdictRelativePath :: Int -> FilePath
p3StageVerdictRelativePath attemptId =
    p3AttemptEvidenceFileRelativePath attemptId "stage-verdict.json"

p4DecisionVerdictRelativePath :: Int -> FilePath
p4DecisionVerdictRelativePath attemptId =
    p4AttemptEvidenceFileRelativePath attemptId "decision-verdict.json"

p4StageConsumptionRelativePath :: Int -> FilePath
p4StageConsumptionRelativePath attemptId =
    p4AttemptEvidenceFileRelativePath attemptId "stage-consumption.json"

subjectTokenRelativePath :: Int -> FilePath
subjectTokenRelativePath attemptId =
    attemptEvidenceFileRelativePath attemptId "subject-token.json"

p2SubjectTokenRelativePath :: Int -> FilePath
p2SubjectTokenRelativePath attemptId =
    p2AttemptEvidenceFileRelativePath attemptId "subject-token.json"

p3SubjectTokenRelativePath :: Int -> FilePath
p3SubjectTokenRelativePath attemptId =
    p3AttemptEvidenceFileRelativePath attemptId "subject-token.json"

d1TraceBundleRelativePath :: Int -> FilePath
d1TraceBundleRelativePath attemptId =
    d1AttemptEvidenceFileRelativePath attemptId "trace-bundle.json"

d2TraceBundleRelativePath :: Int -> FilePath
d2TraceBundleRelativePath attemptId =
    d2AttemptEvidenceFileRelativePath attemptId "trace-bundle.json"

traceBundleRelativePath :: Int -> FilePath
traceBundleRelativePath attemptId =
    attemptEvidenceFileRelativePath attemptId "trace-bundle.json"

p2TraceBundleRelativePath :: Int -> FilePath
p2TraceBundleRelativePath attemptId =
    p2AttemptEvidenceFileRelativePath attemptId "trace-bundle.json"

p3TraceBundleRelativePath :: Int -> FilePath
p3TraceBundleRelativePath attemptId =
    p3AttemptEvidenceFileRelativePath attemptId "trace-bundle.json"

p4TraceBundleRelativePath :: Int -> FilePath
p4TraceBundleRelativePath attemptId =
    p4AttemptEvidenceFileRelativePath attemptId "trace-bundle.json"

p1AuthoritativeSubjectTokenRelativePath :: FilePath
p1AuthoritativeSubjectTokenRelativePath = subjectTokenRelativePath 2

p2AuthoritativeCheckWRelativePath :: FilePath
p2AuthoritativeCheckWRelativePath =
    p2AttemptEvidenceFileRelativePath 2 "check-P2-W.json"

p2AuthoritativeCheckRRelativePath :: FilePath
p2AuthoritativeCheckRRelativePath =
    p2AttemptEvidenceFileRelativePath 2 "check-P2-R.json"

p2AuthoritativeStageVerdictRelativePath :: FilePath
p2AuthoritativeStageVerdictRelativePath =
    p2AttemptEvidenceFileRelativePath 2 "stage-verdict.json"

p2AuthoritativeTraceBundleRelativePath :: FilePath
p2AuthoritativeTraceBundleRelativePath =
    p2AttemptEvidenceFileRelativePath 2 "trace-bundle.json"

p1AuthoritativeReviewRecordRelativePath :: FilePath
p1AuthoritativeReviewRecordRelativePath =
    "orchestrator"
        </> "rounds"
        </> "round-016"
        </> "review-record.json"

p2AuthoritativeReviewRecordRelativePath :: FilePath
p2AuthoritativeReviewRecordRelativePath =
    "orchestrator"
        </> "rounds"
        </> "round-017"
        </> "review-record.json"

d1AuthoritativeReviewRecordRelativePath :: FilePath
d1AuthoritativeReviewRecordRelativePath =
    "orchestrator"
        </> "rounds"
        </> "round-020"
        </> "review-record.json"

p3AuthoritativeReviewRecordRelativePath :: FilePath
p3AuthoritativeReviewRecordRelativePath =
    "orchestrator"
        </> "rounds"
        </> "round-018"
        </> "review-record.json"

prototypePaths :: PrototypeRequest -> PrototypePaths
prototypePaths req =
    PrototypePaths
        { ppEvidenceDir = prRepoRoot req </> evidenceDirForSelector (prStageSelector req) (prAttemptId req)
        , ppArtifactPath = prRepoRoot req </> artifactPathForSelector (prStageSelector req)
        }

evidenceDirForSelector :: String -> Int -> FilePath
evidenceDirForSelector stageSelector attemptId
    | stageSelector == stageSelectorD1 = d1AttemptEvidenceRelativeDir attemptId
    | stageSelector == stageSelectorD2 = d2AttemptEvidenceRelativeDir attemptId
    | stageSelector == stageSelectorP1 = attemptEvidenceRelativeDir attemptId
    | stageSelector == stageSelectorP2 = p2AttemptEvidenceRelativeDir attemptId
    | stageSelector == stageSelectorP3 = p3AttemptEvidenceRelativeDir attemptId
    | stageSelector == stageSelectorP4 = p4AttemptEvidenceRelativeDir attemptId
    | otherwise = attemptEvidenceRelativeDir attemptId

artifactPathForSelector :: String -> FilePath
artifactPathForSelector stageSelector
    | stageSelector == stageSelectorD1 = d1ArtifactRelativePath
    | stageSelector == stageSelectorD2 = d2ArtifactRelativePath
    | stageSelector == stageSelectorP1 = artifactRelativePath
    | stageSelector == stageSelectorP2 = p2ArtifactRelativePath
    | stageSelector == stageSelectorP3 = p3ArtifactRelativePath
    | stageSelector == stageSelectorP4 = p4ArtifactRelativePath
    | otherwise = artifactRelativePath

candidateKindText :: CandidateKind -> String
candidateKindText kind = case kind of
    CandidateLocalRoot -> "local-root"
    CandidateEquivalentLocalCluster -> "equivalent-local-cluster"

stageResultText :: String -> String
stageResultText = id

prototypeSubjectId :: PrototypeReport -> String
prototypeSubjectId report =
    case prototypeSubjectToken report of
        Nothing -> ""
        Just token -> stSubjectId token

normalizedRejectionTriggers :: [String]
normalizedRejectionTriggers =
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
