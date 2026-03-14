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
    stageSelectorP2,
    scenarioIdUriR2C1OnlyV1,
    boundedSubjectId,
    artifactRelativePath,
    p2ArtifactRelativePath,
    attemptEvidenceRelativeDir,
    p2AttemptEvidenceRelativeDir,
    attemptEvidenceFileRelativePath,
    p2AttemptEvidenceFileRelativePath,
    candidateInventoryRelativePath,
    candidateSelectionRuleRelativePath,
    checkResultRelativePath,
    p2CheckResultRelativePath,
    stageVerdictRelativePath,
    p2StageVerdictRelativePath,
    subjectTokenRelativePath,
    p2SubjectTokenRelativePath,
    traceBundleRelativePath,
    p2TraceBundleRelativePath,
    p1AuthoritativeSubjectTokenRelativePath,
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

stageSelectorP1 :: String
stageSelectorP1 = "P1-subject-discovery"

stageSelectorP2 :: String
stageSelectorP2 = "P2-provenance-preservation"

scenarioIdUriR2C1OnlyV1 :: String
scenarioIdUriR2C1OnlyV1 = "uri-r2-c1-only-v1"

boundedSubjectId :: String
boundedSubjectId = "URI-R2-C1"

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

candidateInventoryRelativePath :: Int -> FilePath
candidateInventoryRelativePath attemptId =
    attemptEvidenceRelativeDir attemptId </> "candidate-inventory.json"

attemptEvidenceFileRelativePath :: Int -> FilePath -> FilePath
attemptEvidenceFileRelativePath attemptId fileName =
    attemptEvidenceRelativeDir attemptId </> fileName

p2AttemptEvidenceFileRelativePath :: Int -> FilePath -> FilePath
p2AttemptEvidenceFileRelativePath attemptId fileName =
    p2AttemptEvidenceRelativeDir attemptId </> fileName

candidateSelectionRuleRelativePath :: Int -> FilePath
candidateSelectionRuleRelativePath attemptId =
    attemptEvidenceFileRelativePath attemptId "candidate-selection-rule.json"

checkResultRelativePath :: Int -> String -> FilePath
checkResultRelativePath attemptId checkId =
    attemptEvidenceFileRelativePath attemptId ("check-" ++ checkId ++ ".json")

p2CheckResultRelativePath :: Int -> String -> FilePath
p2CheckResultRelativePath attemptId checkId =
    p2AttemptEvidenceFileRelativePath attemptId ("check-" ++ checkId ++ ".json")

stageVerdictRelativePath :: Int -> FilePath
stageVerdictRelativePath attemptId =
    attemptEvidenceFileRelativePath attemptId "stage-verdict.json"

p2StageVerdictRelativePath :: Int -> FilePath
p2StageVerdictRelativePath attemptId =
    p2AttemptEvidenceFileRelativePath attemptId "stage-verdict.json"

subjectTokenRelativePath :: Int -> FilePath
subjectTokenRelativePath attemptId =
    attemptEvidenceFileRelativePath attemptId "subject-token.json"

p2SubjectTokenRelativePath :: Int -> FilePath
p2SubjectTokenRelativePath attemptId =
    p2AttemptEvidenceFileRelativePath attemptId "subject-token.json"

traceBundleRelativePath :: Int -> FilePath
traceBundleRelativePath attemptId =
    attemptEvidenceFileRelativePath attemptId "trace-bundle.json"

p2TraceBundleRelativePath :: Int -> FilePath
p2TraceBundleRelativePath attemptId =
    p2AttemptEvidenceFileRelativePath attemptId "trace-bundle.json"

p1AuthoritativeSubjectTokenRelativePath :: FilePath
p1AuthoritativeSubjectTokenRelativePath = subjectTokenRelativePath 2

prototypePaths :: PrototypeRequest -> PrototypePaths
prototypePaths req =
    PrototypePaths
        { ppEvidenceDir = prRepoRoot req </> evidenceDirForSelector (prStageSelector req) (prAttemptId req)
        , ppArtifactPath = prRepoRoot req </> artifactPathForSelector (prStageSelector req)
        }

evidenceDirForSelector :: String -> Int -> FilePath
evidenceDirForSelector stageSelector attemptId
    | stageSelector == stageSelectorP1 = attemptEvidenceRelativeDir attemptId
    | stageSelector == stageSelectorP2 = p2AttemptEvidenceRelativeDir attemptId
    | otherwise = attemptEvidenceRelativeDir attemptId

artifactPathForSelector :: String -> FilePath
artifactPathForSelector stageSelector
    | stageSelector == stageSelectorP1 = artifactRelativePath
    | stageSelector == stageSelectorP2 = p2ArtifactRelativePath
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
