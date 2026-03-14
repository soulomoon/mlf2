module MLF.Research.URI.R2.C1.Prototype.Artifact (
    writeP1Artifact,
    writeP2Artifact,
    writeP3Artifact
) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import MLF.Research.URI.R2.C1.Prototype.P2 (P2Execution(..), P2CheckArtifact(..))
import MLF.Research.URI.R2.C1.Prototype.P3 (P3Execution(..), P3CheckArtifact(..))
import MLF.Research.URI.R2.C1.Prototype.Types

writeP1Artifact :: PrototypeReport -> IO FilePath
writeP1Artifact report = do
    let path = ppArtifactPath (prototypePaths (prototypeRequest report))
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path (renderP1Artifact report)
    pure path

writeP2Artifact :: P2Execution -> IO FilePath
writeP2Artifact execution = do
    let path = ppArtifactPath (prototypePaths (prototypeRequest (p2AppReport execution)))
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path (renderP2Artifact execution)
    pure path

writeP3Artifact :: P3Execution -> IO FilePath
writeP3Artifact execution = do
    let path = ppArtifactPath (prototypePaths (prototypeRequest (p3AppReport execution)))
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path (renderP3Artifact execution)
    pure path

renderP1Artifact :: PrototypeReport -> String
renderP1Artifact report =
    unlines $
        [ "# `P1` Subject-Discovery Prototype For `URI-R2-C1`"
        , ""
        , "Date: 2026-03-15"
        , "Roadmap item: 1"
        , "Stage: `P1`"
        , "Attempt: " ++ show (prAttemptId (prototypeRequest report))
        , "Active subject: `URI-R2-C1`"
        , "Active scenario: `uri-r2-c1-only-v1`"
        , "Artifact kind: subject-discovery prototype"
        , ""
        , "## Inherited Inputs"
        , ""
        , "- `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`"
        , "- `docs/plans/2026-03-14-uri-r2-c1-re5-final-successor-recommendation.md`"
        , "- `orchestrator/rounds/round-016/selection.md`"
        , ""
        , "## Stage Input Interface"
        , ""
        , "- Fixed inherited subject boundary: `URI-R2-C1`, `single-SCC`, `single-binder-family`, non-equi-recursive, non-cyclic structural graph."
        , "- `research_entrypoint_id`: `uri-r2-c1-prototype-entrypoint-v1`"
        , "- `stage_selector`: `P1-subject-discovery`"
        , "- `scenario_id`: `uri-r2-c1-only-v1`"
        , "- Candidate universe: bounded local recursive roots or equivalent local clusters inside `URI-R2-C1`, one SCC, one binder family, normalized by `cluster-equivalence-v1`."
        , ""
        , "## Method"
        , ""
        , "- Shared research entrypoint: `uri-r2-c1-prototype-entrypoint-v1`."
        , "- Bounded scenario: `uri-r2-c1-only-v1` only."
        , "- Check sequence: `P1-C`, `P1-N`, `P1-U`."
        , "- Evidence directory: `" ++ attemptEvidenceRelativeDir (prAttemptId (prototypeRequest report)) ++ "`."
        , ""
        , "## Evidence"
        , ""
        , "Normalized candidate set:"
        ]
            ++ map renderCandidate (prototypeCandidates report)
            ++ [ ""
               , "Every inadmissible candidate would fail inside the fixed boundary. This accepted attempt observed no non-`none` rejection trigger, so no inadmissible normalized candidate remained after bounded discovery."
               , ""
               , "## Rejection Triggers"
               , ""
               , renderP1RejectionTriggers report
               , ""
               , "## Stage Result"
               , ""
               , "`" ++ prototypeStageResult report ++ "`"
               , ""
               , "## Next-Stage Handoff"
               , ""
               , renderP1Handoff report
               ]

renderP2Artifact :: P2Execution -> String
renderP2Artifact execution =
    let report = p2AppReport execution
        req = prototypeRequest report
        attemptId = prAttemptId req
    in unlines $
        [ "# `P2` Provenance-Preservation Prototype For `URI-R2-C1`"
        , ""
        , "Date: 2026-03-15"
        , "Roadmap item: 2"
        , "Stage: `P2`"
        , "Attempt: " ++ show attemptId
        , "Active subject: `URI-R2-C1`"
        , "Active scenario: `uri-r2-c1-only-v1`"
        , "Artifact kind: provenance-preservation prototype"
        , ""
        , "## Inherited Inputs"
        , ""
        , "- `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`"
        , "- `docs/plans/2026-03-15-uri-r2-c1-p1-subject-discovery-prototype.md`"
        , "- `" ++ p2InheritedTokenPath execution ++ "`"
        , "- `orchestrator/rounds/round-017/selection.md`"
        , ""
        , "## Stage Input Interface"
        , ""
        , "- Inherited token path: `" ++ p2InheritedTokenPath execution ++ "`."
        , "- Shared entrypoint tuple: `{ research_entrypoint_id: uri-r2-c1-prototype-entrypoint-v1, stage_selector: P2-provenance-preservation, scenario_id: uri-r2-c1-only-v1, attempt_id: "
            ++ show attemptId
            ++ " }`."
        , ""
        , "## Method"
        , ""
        , "- Ordered checks: `P2-G`, `P2-S`, `P2-R`, `P2-W`."
        , "- The bounded fixture executes `generalizeWithPlan -> schemeToType -> reifyTypeWithNamedSetNoFallback -> witness replay` under the existing shared entrypoint."
        , "- Shared `correlation_id`: `" ++ p2CorrelationId execution ++ "`."
        , ""
        , "## Evidence"
        , ""
        , "- Attempt-local evidence directory: `" ++ p2AttemptEvidenceRelativeDir attemptId ++ "`."
        , "- Trace bundle: `" ++ p2TraceBundleRelativePath attemptId ++ "`."
        ]
            ++ map renderP2CheckLine (p2Checks execution)
            ++ [ "- Trace refs: " ++ unwords (map (\ref -> "`" ++ ref ++ "`") (p2TraceRefs execution))
               , "- Observations:"
               ]
            ++ map ("- " ++) (p2TraceSummary execution)
            ++ [ ""
               , "## Rejection Triggers"
               , ""
               , renderP2RejectionTriggers execution
               , ""
               , "## Stage Result"
               , ""
               , "`" ++ prototypeStageResult report ++ "`"
               ]
            ++ if prototypeStageResult report == "pass"
                then
                    [ ""
                    , "## Next-Stage Handoff"
                    , ""
                    , "Reaffirmed subject token: `" ++ p2AttemptEvidenceRelativeDir attemptId ++ "/subject-token.json`."
                    ]
                else []

renderP3Artifact :: P3Execution -> String
renderP3Artifact execution =
    let report = p3AppReport execution
        req = prototypeRequest report
        attemptId = prAttemptId req
        tokenObservation =
            if p3P2TokenPresent execution
                then "present (unexpected for authoritative semantic-negative P2)"
                else "absent (required for authoritative semantic-negative P2)"
    in unlines $
        [ "# `P3` Safety-Validation Prototype For `URI-R2-C1`"
        , ""
        , "Date: 2026-03-15"
        , "Roadmap item: 3"
        , "Stage: `P3`"
        , "Attempt: " ++ show attemptId
        , "Active subject: `URI-R2-C1`"
        , "Active scenario: `uri-r2-c1-only-v1`"
        , "Artifact kind: safety-validation prototype"
        , ""
        , "## Inherited Inputs"
        , ""
        , "- `docs/superpowers/specs/2026-03-15-uri-r2-c1-prototype-evidence-roadmap-design.md`"
        , "- `docs/plans/2026-03-15-uri-r2-c1-p2-provenance-preservation-prototype.md`"
        , "- `" ++ p3ReviewRecordPath execution ++ "`"
        , "- `orchestrator/rounds/round-018/selection.md`"
        , ""
        , "## Stage Input Interface"
        , ""
        , "- Expected `P2 -> P3` handoff token path: `" ++ p3P2TokenPath execution ++ "`."
        , "- Observed `P2 -> P3` handoff token state: " ++ tokenObservation ++ "."
        , "- Inherited review-record path: `" ++ p3ReviewRecordPath execution ++ "`."
        , "- Shared entrypoint tuple: `{ research_entrypoint_id: uri-r2-c1-prototype-entrypoint-v1, stage_selector: P3-safety-validation, scenario_id: uri-r2-c1-only-v1, attempt_id: "
            ++ show attemptId
            ++ " }`."
        , ""
        , "## Method"
        , ""
        , "- Ordered checks: `P3-S`, `P3-A`, `P3-B`, `P3-C`."
        , "- The bounded stage consumes only authoritative `P2` handoff continuity; when `P2` is non-pass and emits no token, `P3` records bounded non-pass without fallback to `P1`."
        , "- Shared `correlation_id`: `" ++ p3CorrelationId execution ++ "`."
        , ""
        , "## Evidence"
        , ""
        , "- Attempt-local evidence directory: `" ++ p3AttemptEvidenceRelativeDir attemptId ++ "`."
        , "- Trace bundle: `" ++ p3TraceBundleRelativePath attemptId ++ "`."
        ]
            ++ map renderP3CheckLine (p3Checks execution)
            ++ [ "- Trace refs: " ++ unwords (map (\ref -> "`" ++ ref ++ "`") (p3TraceRefs execution))
               , "- Observations:"
               ]
            ++ map ("- " ++) (p3TraceSummary execution)
            ++ [ ""
               , "## Rejection Triggers"
               , ""
               , renderP3RejectionTriggers execution
               , ""
               , "## Stage Result"
               , ""
               , "`" ++ prototypeStageResult report ++ "`"
               ]
            ++ if prototypeStageResult report == "pass"
                then
                    [ ""
                    , "## Next-Stage Handoff"
                    , ""
                    , "Reaffirmed subject token: `" ++ p3AttemptEvidenceRelativeDir attemptId ++ "/subject-token.json`."
                    ]
                else []

renderCandidate :: CandidateRecord -> String
renderCandidate candidate =
    "- `"
        ++ crCandidateId candidate
        ++ "` (`"
        ++ candidateKindText (crCandidateKind candidate)
        ++ "`), normalization basis `"
        ++ crNormalizationBasis candidate
        ++ "`, verdict `"
        ++ crAdmissibilityVerdict candidate
        ++ "`, rejection trigger `"
        ++ crRejectionTrigger candidate
        ++ "`."

renderP1RejectionTriggers :: PrototypeReport -> String
renderP1RejectionTriggers report =
    let triggers =
            filter (/= "none")
                [ ckRejectionTrigger (prototypeP1C report)
                , ckRejectionTrigger (prototypeP1N report)
                , ckRejectionTrigger (prototypeP1U report)
                ]
    in case triggers of
        [] -> "No non-`none` rejection triggers were observed."
        _ -> unwords (map (\trigger -> "`" ++ trigger ++ "`") triggers)

renderP1Handoff :: PrototypeReport -> String
renderP1Handoff report =
    case prototypeSubjectToken report of
        Nothing ->
            "No handoff token exists because the stage did not pass."
        Just _ ->
            "Canonical subject token: `"
                ++ attemptEvidenceRelativeDir (prAttemptId (prototypeRequest report))
                ++ "/subject-token.json`."

renderP2CheckLine :: P2CheckArtifact -> String
renderP2CheckLine check =
    let result = p2caResult check
    in "- `"
        ++ ckCheckId result
        ++ "`: `"
        ++ ckStatus result
        ++ "` via `"
        ++ p2caEvidenceRef check
        ++ "`."

renderP2RejectionTriggers :: P2Execution -> String
renderP2RejectionTriggers execution =
    let observed =
            filter (/= "none")
                (map (ckRejectionTrigger . p2caResult) (p2Checks execution))
        observedLine =
            case observed of
                [] -> "Observed triggers: `none`."
                _ -> "Observed triggers: " ++ unwords (map (\trigger -> "`" ++ trigger ++ "`") observed) ++ "."
        detailsLine =
            unlines (map (\check -> "`" ++ ckCheckId (p2caResult check) ++ "`: " ++ ckDetails (p2caResult check)) (p2Checks execution))
        vocabularyLine =
            "Normalized vocabulary: " ++ unwords (map (\trigger -> "`" ++ trigger ++ "`") normalizedRejectionTriggers) ++ "."
    in observedLine ++ "\n\n" ++ detailsLine ++ "\n" ++ vocabularyLine

renderP3CheckLine :: P3CheckArtifact -> String
renderP3CheckLine check =
    let result = p3caResult check
    in "- `"
        ++ ckCheckId result
        ++ "`: `"
        ++ ckStatus result
        ++ "` via `"
        ++ p3caEvidenceRef check
        ++ "`."

renderP3RejectionTriggers :: P3Execution -> String
renderP3RejectionTriggers execution =
    let observed =
            filter (/= "none")
                (map (ckRejectionTrigger . p3caResult) (p3Checks execution))
        observedLine =
            case observed of
                [] -> "Observed triggers: `none`."
                _ -> "Observed triggers: " ++ unwords (map (\trigger -> "`" ++ trigger ++ "`") observed) ++ "."
        detailsLine =
            unlines (map (\check -> "`" ++ ckCheckId (p3caResult check) ++ "`: " ++ ckDetails (p3caResult check)) (p3Checks execution))
        vocabularyLine =
            "Normalized vocabulary: " ++ unwords (map (\trigger -> "`" ++ trigger ++ "`") normalizedRejectionTriggers) ++ "."
    in observedLine ++ "\n\n" ++ detailsLine ++ "\n" ++ vocabularyLine
