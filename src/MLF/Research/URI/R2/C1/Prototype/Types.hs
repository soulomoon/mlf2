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
    stageSelectorP1,
    scenarioIdUriR2C1OnlyV1,
    boundedSubjectId,
    artifactRelativePath,
    attemptEvidenceRelativeDir,
    attemptEvidenceFileRelativePath,
    candidateInventoryRelativePath,
    candidateSelectionRuleRelativePath,
    checkResultRelativePath,
    stageVerdictRelativePath,
    subjectTokenRelativePath,
    traceBundleRelativePath,
    prototypePaths,
    candidateKindText,
    stageResultText,
    prototypeSubjectId
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

stageSelectorP1 :: String
stageSelectorP1 = "P1-subject-discovery"

scenarioIdUriR2C1OnlyV1 :: String
scenarioIdUriR2C1OnlyV1 = "uri-r2-c1-only-v1"

boundedSubjectId :: String
boundedSubjectId = "URI-R2-C1"

artifactRelativePath :: FilePath
artifactRelativePath =
    "docs"
        </> "plans"
        </> "2026-03-15-uri-r2-c1-p1-subject-discovery-prototype.md"

attemptEvidenceRelativeDir :: Int -> FilePath
attemptEvidenceRelativeDir attemptId =
    "orchestrator"
        </> "rounds"
        </> "round-016"
        </> "evidence"
        </> "P1"
        </> ("attempt-" ++ show attemptId)

candidateInventoryRelativePath :: Int -> FilePath
candidateInventoryRelativePath attemptId =
    attemptEvidenceRelativeDir attemptId </> "candidate-inventory.json"

attemptEvidenceFileRelativePath :: Int -> FilePath -> FilePath
attemptEvidenceFileRelativePath attemptId fileName =
    attemptEvidenceRelativeDir attemptId </> fileName

candidateSelectionRuleRelativePath :: Int -> FilePath
candidateSelectionRuleRelativePath attemptId =
    attemptEvidenceFileRelativePath attemptId "candidate-selection-rule.json"

checkResultRelativePath :: Int -> String -> FilePath
checkResultRelativePath attemptId checkId =
    attemptEvidenceFileRelativePath attemptId ("check-" ++ checkId ++ ".json")

stageVerdictRelativePath :: Int -> FilePath
stageVerdictRelativePath attemptId =
    attemptEvidenceFileRelativePath attemptId "stage-verdict.json"

subjectTokenRelativePath :: Int -> FilePath
subjectTokenRelativePath attemptId =
    attemptEvidenceFileRelativePath attemptId "subject-token.json"

traceBundleRelativePath :: Int -> FilePath
traceBundleRelativePath attemptId =
    attemptEvidenceFileRelativePath attemptId "trace-bundle.json"

prototypePaths :: PrototypeRequest -> PrototypePaths
prototypePaths req =
    PrototypePaths
        { ppEvidenceDir = prRepoRoot req </> attemptEvidenceRelativeDir (prAttemptId req)
        , ppArtifactPath = prRepoRoot req </> artifactRelativePath
        }

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
