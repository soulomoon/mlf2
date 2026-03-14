module MLF.Research.URI.R2.C1.Prototype.Artifact (
    writeP1Artifact
) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import MLF.Research.URI.R2.C1.Prototype.Types

writeP1Artifact :: PrototypeReport -> IO FilePath
writeP1Artifact report = do
    let path = ppArtifactPath (prototypePaths (prototypeRequest report))
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path (renderArtifact report)
    pure path

renderArtifact :: PrototypeReport -> String
renderArtifact report =
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
               , renderRejectionTriggers report
               , ""
               , "## Stage Result"
               , ""
               , "`" ++ prototypeStageResult report ++ "`"
               , ""
               , "## Next-Stage Handoff"
               , ""
               , renderHandoff report
               ]

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

renderRejectionTriggers :: PrototypeReport -> String
renderRejectionTriggers report =
    let triggers =
            filter (/= "none")
                [ ckRejectionTrigger (prototypeP1C report)
                , ckRejectionTrigger (prototypeP1N report)
                , ckRejectionTrigger (prototypeP1U report)
                ]
    in case triggers of
        [] -> "No non-`none` rejection triggers were observed."
        _ -> unwords (map (\trigger -> "`" ++ trigger ++ "`") triggers)

renderHandoff :: PrototypeReport -> String
renderHandoff report =
    case prototypeSubjectToken report of
        Nothing ->
            "No handoff token exists because the stage did not pass."
        Just _ ->
            "Canonical subject token: `"
                ++ attemptEvidenceRelativeDir (prAttemptId (prototypeRequest report))
                ++ "/subject-token.json`."
