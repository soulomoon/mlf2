module MLF.Research.URI.R2.C1.Prototype.P2 (
    P2CheckArtifact(..),
    P2Execution(..),
    executeP2
) where

import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (isInfixOf)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , removePathForcibly
    )
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch)

import MLF.Constraint.Acyclicity (checkAcyclicity)
import qualified MLF.Constraint.Finalize as Finalize
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionResult(..)
    , computePresolution
    )
import MLF.Constraint.Presolution.Base (EdgeArtifacts(..))
import MLF.Constraint.Presolution.View (toRawPresolutionViewForLegacy)
import MLF.Constraint.Types.Graph
    ( EdgeId
    , NodeId(..)
    , NodeRef
    , cNodes
    , getEdgeId
    , getNodeId
    , lookupNodeIn
    , toRawConstraintForLegacy
    )
import MLF.Constraint.Types.Presolution (snapshotConstraint, snapshotUnionFind)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Canonicalizer (canonicalizeNode)
import MLF.Constraint.Normalize (normalize)
import MLF.Elab.Pipeline
    ( Pretty(..)
    , SchemeInfo(..)
    , applyInstantiation
    , defaultTraceConfig
    , phiFromEdgeWitnessWithTrace
    , schemeToType
    )
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Run.Annotation (annNode, redirectAndCanonicalizeAnn)
import MLF.Elab.Run.Generalize
    ( constraintForGeneralization
    , generalizeAtWithBuilder
    , instantiationCopyNodes
    )
import MLF.Elab.Run.Provenance (buildTraceCopyMap, collectBaseNamedKeys)
import MLF.Elab.Run.ResultType
    ( ResultTypeInputs(..)
    , generalizeWithPlan
    , mkResultTypeInputs
    , rtcEdgeTraces
    , rtcEdgeWitnesses
    )
import MLF.Elab.Run.Scope (resolveCanonicalScope, schemeBodyTarget)
import MLF.Elab.Run.Util
    ( canonicalizeExpansion
    , canonicalizeTrace
    , canonicalizeWitness
    , makeCanonicalizer
    )
import MLF.Elab.Types (bindingToElab)
import MLF.Frontend.ConstraintGen (AnnExpr(..), ConstraintResult(..), generateConstraints)
import MLF.Frontend.Normalize (normalizeExpr)
import MLF.Frontend.Syntax (Expr(..), SurfaceExpr, SrcTy(..))
import MLF.Reify.Core (namedNodes, reifyTypeWithNamedSetNoFallback)
import MLF.Reify.TypeOps (alphaEqType)

import MLF.Research.URI.R2.C1.Prototype.Types

data P2CheckArtifact = P2CheckArtifact
    { p2caResult :: CheckResult
    , p2caCorrelationId :: String
    , p2caSubjectId :: String
    , p2caEvidenceRef :: FilePath
    }
    deriving (Eq, Show)

data P2Execution = P2Execution
    { p2AppReport :: PrototypeReport
    , p2InheritedTokenPath :: FilePath
    , p2InheritedToken :: SubjectToken
    , p2CorrelationId :: String
    , p2TraceRefs :: [String]
    , p2Checks :: [P2CheckArtifact]
    , p2StageResult :: String
    , p2OutputToken :: Maybe SubjectToken
    , p2TraceSummary :: [String]
    }
    deriving (Eq, Show)

data ScenarioContext = ScenarioContext
    { scInputs :: ResultTypeInputs
    , scInner :: AnnExpr
    , scAnnNodeId :: NodeId
    , scEdgeId :: EdgeId
    , scScopeRoot :: NodeRef
    , scTargetNode :: NodeId
    , scWitnessTrace :: Maybe EdgeTrace
    }

executeP2 :: PrototypeRequest -> IO (Either PrototypeError P2Execution)
executeP2 req = do
    let tokenRelativePath = p1AuthoritativeSubjectTokenRelativePath
        tokenPath = prRepoRoot req </> tokenRelativePath
    tokenExists <- doesFileExist tokenPath
    if not tokenExists
        then pure (Left (MissingStageInputToken tokenRelativePath))
        else do
            tokenText <- readFile tokenPath
            case decodeSubjectToken tokenRelativePath tokenText of
                Left err -> pure (Left err)
                Right inheritedToken ->
                    case validateInheritedToken inheritedToken of
                        Left err -> pure (Left err)
                        Right canonicalToken -> do
                            let paths = prototypePaths req
                            cleanEvidenceDir (ppEvidenceDir paths)
                            let execution = runP2 req tokenRelativePath canonicalToken
                            createDirectoryIfMissing True (ppEvidenceDir paths)
                            writeEvidence paths execution
                            pure (Right execution)

runP2 :: PrototypeRequest -> FilePath -> SubjectToken -> P2Execution
runP2 req tokenPath inheritedToken =
    let correlationId = mkCorrelationId req
        p2SubjectId = stSubjectId inheritedToken
        p2TraceBundleRef = p2TraceBundleRelativePath (prAttemptId req)
    in case prepareScenarioContext of
        Left err ->
            finalizeExecution
                req
                tokenPath
                inheritedToken
                correlationId
                []
                [ blockingFailure "P2-G" err
                , skippedCheck "P2-S" "generalizeWithPlan did not produce a scheme."
                , skippedCheck "P2-R" "No reconstructed type was available for no-fallback reification."
                , skippedCheck "P2-W" "Witness replay was not attempted because the bounded scenario setup failed."
                ]
        Right ctx ->
            case generalizeWithPlan
                (rtcPlanBuilder (scInputs ctx))
                (rtcBindParentsGa (scInputs ctx))
                (rtcPresolutionView (scInputs ctx))
                (scScopeRoot ctx)
                (scTargetNode ctx) of
                Left err ->
                    finalizeExecution
                        req
                        tokenPath
                        inheritedToken
                        correlationId
                        []
                        [ mkCheckArtifact p2SubjectId p2TraceBundleRef correlationId "P2-G" "semantic-negative" "blocking-stop-condition"
                            ("generalizeWithPlan failed: " ++ show err)
                        , skippedCheck "P2-S" "schemeToType was not executed because generalization failed."
                        , skippedCheck "P2-R" "reifyTypeWithNamedSetNoFallback was not executed because generalization failed."
                        , skippedCheck "P2-W" "Witness replay was not executed because generalization failed."
                        ]
                Right (scheme, subst) ->
                    let schemeText = pretty scheme
                        traceG = traceRef "generalize" subjectId schemeText
                        checkG =
                            mkCheckArtifact subjectId traceBundleRef correlationId "P2-G" "pass" "none"
                                ("generalizeWithPlan => " ++ schemeText)
                        schemeTy = schemeToType scheme
                        schemeTyText = pretty schemeTy
                        traceS = traceRef "scheme-to-type" subjectId schemeTyText
                        checkS =
                            mkCheckArtifact subjectId traceBundleRef correlationId "P2-S" "pass" "none"
                                ("schemeToType => " ++ schemeTyText)
                    in case namedNodes (rtcPresolutionView (scInputs ctx)) of
                        Left err ->
                            finalizeExecution
                                req
                                tokenPath
                                inheritedToken
                                correlationId
                                [traceG, traceS]
                                [ checkG
                                , checkS
                                , mkCheckArtifact p2SubjectId p2TraceBundleRef correlationId "P2-R" "semantic-negative" "blocking-stop-condition"
                                    ("namedNodes failed before no-fallback reification: " ++ show err)
                                , skippedCheck "P2-W" "Witness replay was not executed because named-node discovery failed."
                                ]
                        Right namedSet ->
                            let reifyNode = schemeBodyTarget (rtcPresolutionView (scInputs ctx)) (scAnnNodeId ctx)
                            in case reifyTypeWithNamedSetNoFallback
                                (rtcPresolutionView (scInputs ctx))
                                IntMap.empty
                                namedSet
                                reifyNode of
                                Left err ->
                                    finalizeExecution
                                        req
                                        tokenPath
                                        inheritedToken
                                        correlationId
                                        [traceG, traceS]
                                        [ checkG
                                        , checkS
                                        , mkCheckArtifact p2SubjectId p2TraceBundleRef correlationId "P2-R" "semantic-negative" "blocking-stop-condition"
                                            ("reifyTypeWithNamedSetNoFallback failed: " ++ show err)
                                        , skippedCheck "P2-W" "Witness replay was not executed because reification failed."
                                        ]
                                Right reifiedTy ->
                                    let reifiedText = pretty reifiedTy
                                        traceR = traceRef "reify-no-fallback" subjectId reifiedText
                                        checkR =
                                            mkCheckArtifact subjectId traceBundleRef correlationId "P2-R" "pass" "none"
                                                ("reifyTypeWithNamedSetNoFallback => " ++ reifiedText)
                                        generalizeAtWith mbGa =
                                            generalizeAtWithBuilder
                                                (rtcPlanBuilder (scInputs ctx))
                                                mbGa
                                                (rtcPresolutionView (scInputs ctx))
                                    in case scWitnessTrace ctx of
                                        Nothing ->
                                            finalizeExecution
                                                req
                                                tokenPath
                                                inheritedToken
                                                correlationId
                                                [traceG, traceS, traceR]
                                                [ checkG
                                                , checkS
                                                , checkR
                                                , mkCheckArtifact subjectId traceBundleRef correlationId "P2-W" "inconclusive" "partial-replay"
                                                    "Witness replay trace was missing for the bounded annotation edge."
                                                ]
                                        Just trace0 ->
                                            let witnesses = rtcEdgeWitnesses (scInputs ctx)
                                                edgeTraces = rtcEdgeTraces (scInputs ctx)
                                            in case IntMap.lookup (edgeKey (scEdgeId ctx)) witnesses of
                                                Nothing ->
                                                    finalizeExecution
                                                        req
                                                        tokenPath
                                                        inheritedToken
                                                        correlationId
                                                        [traceG, traceS, traceR]
                                                        [ checkG
                                                        , checkS
                                                        , checkR
                                                        , mkCheckArtifact p2SubjectId p2TraceBundleRef correlationId "P2-W" "inconclusive" "partial-replay"
                                                            "Witness replay could not start because the bounded annotation edge had no witness."
                                                        ]
                                                Just witness ->
                                                    let schemeInfo =
                                                            SchemeInfo
                                                                { siScheme = scheme
                                                                , siSubst = subst
                                                                }
                                                        traceForReplay =
                                                            IntMap.lookup (edgeKey (scEdgeId ctx)) edgeTraces
                                                                `orElse` Just trace0
                                                    in case phiFromEdgeWitnessWithTrace
                                                        (rtcTraceConfig (scInputs ctx))
                                                        generalizeAtWith
                                                        (rtcPresolutionView (scInputs ctx))
                                                        (Just (rtcBindParentsGa (scInputs ctx)))
                                                        (Just schemeInfo)
                                                        traceForReplay
                                                        witness of
                                                        Left err ->
                                                            finalizeExecution
                                                                req
                                                                tokenPath
                                                                inheritedToken
                                                                correlationId
                                                                [traceG, traceS, traceR]
                                                                [ checkG
                                                                , checkS
                                                                , checkR
                                                                , mkCheckArtifact p2SubjectId p2TraceBundleRef correlationId "P2-W" "semantic-negative" "partial-replay"
                                                                    ("phiFromEdgeWitnessWithTrace failed: " ++ show err)
                                                                ]
                                                        Right phi ->
                                                            let phiText = pretty phi
                                                                (checkW, traceW) =
                                                                    case applyInstantiation schemeTy phi of
                                                                        Right replayedTy ->
                                                                            let replayedText = pretty replayedTy
                                                                                checkResult
                                                                                    | alphaEqType replayedTy reifiedTy =
                                                                                        mkCheckArtifact subjectId traceBundleRef correlationId "P2-W" "pass" "none"
                                                                                            ("witness replay => " ++ phiText ++ " => " ++ replayedText)
                                                                                    | otherwise =
                                                                                        mkCheckArtifact subjectId traceBundleRef correlationId "P2-W" "semantic-negative" "replay-domain-widening"
                                                                                            ("witness replay produced " ++ replayedText ++ " but reification preserved " ++ reifiedText)
                                                                            in ( checkResult
                                                                               , traceRef "witness-replay" subjectId (phiText ++ "-" ++ replayedText)
                                                                               )
                                                                        Left err ->
                                                                            ( mkCheckArtifact subjectId traceBundleRef correlationId "P2-W" "semantic-negative" "partial-replay"
                                                                                ("witness replay => " ++ phiText ++ " (applyInstantiation diagnostic failed: " ++ show err ++ ")")
                                                                            , traceRef "witness-replay" subjectId phiText
                                                                            )
                                                            in finalizeExecution
                                                                req
                                                                tokenPath
                                                                inheritedToken
                                                                correlationId
                                                                [traceG, traceS, traceR, traceW]
                                                                [checkG, checkS, checkR, checkW]
  where
    skippedCheck checkId details =
        mkCheckArtifact subjectId traceBundleRef helperCorrelationId checkId "inconclusive" "blocking-stop-condition" details

    blockingFailure checkId details =
        mkCheckArtifact subjectId traceBundleRef helperCorrelationId checkId "semantic-negative" "blocking-stop-condition" details

    subjectId = stSubjectId inheritedToken
    traceBundleRef = p2TraceBundleRelativePath (prAttemptId req)
    helperCorrelationId = mkCorrelationId req

validateInheritedToken :: SubjectToken -> Either PrototypeError SubjectToken
validateInheritedToken token
    | ssScenarioId (stSubjectScope token) /= scenarioIdUriR2C1OnlyV1 =
        Left (TokenScenarioMismatch (ssScenarioId (stSubjectScope token)))
    | ssBoundedSubject (stSubjectScope token) /= boundedSubjectId =
        Left (UnsupportedBoundedSubject (ssBoundedSubject (stSubjectScope token)))
    | paOriginStage (stProvenanceAnchor token) /= "P1" =
        Left (MalformedStageInputToken p1AuthoritativeSubjectTokenRelativePath "provenance_anchor.origin_stage must be P1")
    | "uri-r2-c1/" `isInfixOf` stSubjectId token = Right token
    | otherwise =
        Left (UnsupportedBoundedSubject (stSubjectId token))

prepareScenarioContext :: Either String ScenarioContext
prepareScenarioContext = do
    let expr = uriR2C1FixtureExpr
    normExpr <- firstShow (normalizeExpr expr)
    ConstraintResult { crConstraint = c0, crAnnotated = ann } <- firstShow (generateConstraints Set.empty normExpr)
    let c1 = normalize c0
    (cAcyclic, acyclic) <- firstShow (checkAcyclicity c1)
    pres <- firstShow (computePresolution defaultTraceConfig acyclic cAcyclic)
    solvedClean <- firstShow (Finalize.finalizeSolvedFromSnapshot (snapshotConstraint pres) (snapshotUnionFind pres))
    presolutionViewClean <- toRawPresolutionViewForLegacy <$> firstShow (Finalize.finalizePresolutionViewFromSnapshot (snapshotConstraint pres) (snapshotUnionFind pres))
    let canonNode = makeCanonicalizer (Solved.canonicalMap solvedClean) (prRedirects pres)
        canonical = canonicalizeNode canonNode
        cBaseRaw = toRawConstraintForLegacy cAcyclic
        baseNamedKeysAll = collectBaseNamedKeys cBaseRaw
        edgeTracesForCopy =
            IntMap.filter
                (\tr ->
                    case lookupNodeIn (cNodes cBaseRaw) (etRoot tr) of
                        Just _ -> True
                        Nothing -> False
                )
                (prEdgeTraces pres)
        instCopyNodes =
            instantiationCopyNodes presolutionViewClean (prRedirects pres) edgeTracesForCopy
        traceMaps =
            map
                (buildTraceCopyMap cBaseRaw baseNamedKeysAll canonical)
                (IntMap.elems edgeTracesForCopy)
        instCopyMapFull = foldl' IntMap.union IntMap.empty traceMaps
        (constraintForGen, bindParentsGa) =
            constraintForGeneralization
                defaultTraceConfig
                presolutionViewClean
                (prRedirects pres)
                instCopyNodes
                instCopyMapFull
                cBaseRaw
                ann
    presolutionViewForGen <-
        firstShow
            (Finalize.finalizePresolutionViewFromSnapshot constraintForGen (Solved.canonicalMap solvedClean))
    let annCanon =
            redirectAndCanonicalizeAnn canonical (prRedirects pres) ann
        edgeArtifacts =
            EdgeArtifacts
                { eaEdgeExpansions = IntMap.map (canonicalizeExpansion canonNode) (prEdgeExpansions pres)
                , eaEdgeWitnesses = IntMap.map (canonicalizeWitness canonNode) (prEdgeWitnesses pres)
                , eaEdgeTraces = IntMap.map (canonicalizeTrace canonNode) (prEdgeTraces pres)
                }
        inputs =
            mkResultTypeInputs
                canonical
                edgeArtifacts
                presolutionViewForGen
                bindParentsGa
                (prPlanBuilder pres)
                cBaseRaw
                (prRedirects pres)
                defaultTraceConfig
    case annCanon of
        AAnn inner annNodeId edgeId -> do
            let rootC = rtcCanonical inputs (annNode inner)
                targetNode = schemeBodyTarget (rtcPresolutionView inputs) rootC
                scopeRootNodePre0 = annNode inner
                scopeRootNodePre =
                    IntMap.findWithDefault
                        scopeRootNodePre0
                        (getNodeId targetNode)
                        (gaSolvedToBase (rtcBindParentsGa inputs))
            scopeRoot <-
                firstShow
                    (bindingToElab (resolveCanonicalScope cBaseRaw (rtcPresolutionView inputs) (rtcRedirects inputs) scopeRootNodePre))
            pure
                ScenarioContext
                    { scInputs = inputs
                    , scInner = inner
                    , scAnnNodeId = annNodeId
                    , scEdgeId = edgeId
                    , scScopeRoot = scopeRoot
                    , scTargetNode = targetNode
                    , scWitnessTrace = IntMap.lookup (edgeKey edgeId) (rtcEdgeTraces inputs)
                    }
        other ->
            Left ("expected top-level annotation in bounded P2 fixture, got: " ++ show other)

uriR2C1FixtureExpr :: SurfaceExpr
uriR2C1FixtureExpr =
    EAnn
        (ELam "x" (EVar "x"))
        (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))

mkCorrelationId :: PrototypeRequest -> String
mkCorrelationId req =
    prScenarioId req ++ "-p2-attempt-" ++ show (prAttemptId req)

mkCheckArtifact
    :: String
    -> FilePath
    -> String
    -> String
    -> String
    -> String
    -> String
    -> P2CheckArtifact
mkCheckArtifact subjectId evidenceRef correlationId checkId status rejectionTrigger details =
    P2CheckArtifact
        { p2caResult =
            CheckResult
                { ckCheckId = checkId
                , ckStatus = status
                , ckRejectionTrigger = rejectionTrigger
                , ckDetails = details
                }
        , p2caCorrelationId = correlationId
        , p2caSubjectId = subjectId
        , p2caEvidenceRef = evidenceRef
        }

finalizeExecution
    :: PrototypeRequest
    -> FilePath
    -> SubjectToken
    -> String
    -> [String]
    -> [P2CheckArtifact]
    -> P2Execution
finalizeExecution req tokenPath inheritedToken correlationId traceRefs checks =
    let stageResult = stageResultFromStatuses (map (ckStatus . p2caResult) checks)
        outputToken =
            if stageResult == "pass"
                then Just (inheritedToken { stTraceHandles = stTraceHandles inheritedToken ++ traceRefs })
                else Nothing
        summary =
            [ "P2 consumed only the authoritative P1 subject token from round-016."
            , "The bounded fixture executed generalization, reconstruction, no-fallback reification, and witness replay through the shared prototype entrypoint."
            , "All attempt-local check files repeat the inherited subject_id and shared correlation_id."
            ]
                ++ map (\check -> ckCheckId (p2caResult check) ++ ": " ++ ckDetails (p2caResult check)) checks
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
    in P2Execution
        { p2AppReport = report
        , p2InheritedTokenPath = tokenPath
        , p2InheritedToken = inheritedToken
        , p2CorrelationId = correlationId
        , p2TraceRefs = traceRefs
        , p2Checks = checks
        , p2StageResult = stageResult
        , p2OutputToken = outputToken
        , p2TraceSummary = summary
        }
  where
    reportCheckAt ix =
        case drop ix checks of
            (check : _) -> p2caResult check
            [] ->
                CheckResult
                    { ckCheckId = "P2-missing"
                    , ckStatus = "inconclusive"
                    , ckRejectionTrigger = "blocking-stop-condition"
                    , ckDetails = "Missing P2 check artifact."
                    }

stageResultFromStatuses :: [String] -> String
stageResultFromStatuses statuses
    | any (== "semantic-negative") statuses = "semantic-negative"
    | any (== "inconclusive") statuses = "inconclusive"
    | otherwise = "pass"

edgeKey :: EdgeId -> Int
edgeKey = getEdgeId

traceRef :: String -> String -> String -> String
traceRef operation subjectId detail =
    "trace://uri-r2-c1/p2/"
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
        | isAlphaNum ch = toLower ch
        | otherwise = '-'

    collapseHyphen [] = []
    collapseHyphen ('-' : '-' : rest) = collapseHyphen ('-' : rest)
    collapseHyphen (ch : rest) = ch : collapseHyphen rest

    trimHyphen = reverse . dropWhile (== '-') . reverse . dropWhile (== '-')

orElse :: Maybe a -> Maybe a -> Maybe a
orElse left right = case left of
    Just _ -> left
    Nothing -> right

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
        , emStage = if includeStage then Just "P2" else Nothing
        }

writeEvidence :: PrototypePaths -> P2Execution -> IO ()
writeEvidence paths execution = do
    let req = prototypeRequest (p2AppReport execution)
        attemptId = prAttemptId req
    writeJsonFile
        (ppEvidenceDir paths </> "trace-bundle.json")
        (traceBundleJson req execution)
    mapM_
        (\check ->
            writeJsonFile
                (ppEvidenceDir paths </> ("check-" ++ ckCheckId (p2caResult check) ++ ".json"))
                (p2CheckJson req check)
        )
        (p2Checks execution)
    writeJsonFile
        (ppEvidenceDir paths </> "stage-verdict.json")
        (stageVerdictJson req execution attemptId)
    case p2OutputToken execution of
        Nothing -> pure ()
        Just token ->
            writeJsonFile
                (ppEvidenceDir paths </> "subject-token.json")
                (subjectTokenJson req token)

traceBundleJson :: PrototypeRequest -> P2Execution -> JValue
traceBundleJson req execution =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("correlation_id", JString (p2CorrelationId execution))
               , ("subject_id", JString (stSubjectId (p2InheritedToken execution)))
               , ("trace_refs", JArray (map JString (p2TraceRefs execution)))
               , ("trace_summary", JArray (map JString (p2TraceSummary execution)))
               ]
        )

p2CheckJson :: PrototypeRequest -> P2CheckArtifact -> JValue
p2CheckJson req check =
    let result = p2caResult check
    in JObject
        ( metadataPairs (metadataFor req False)
            ++ [ ("check_id", JString (ckCheckId result))
               , ("correlation_id", JString (p2caCorrelationId check))
               , ("subject_id", JString (p2caSubjectId check))
               , ("evidence_ref", JString (p2caEvidenceRef check))
               , ("verdict", JString (ckStatus result))
               , ("rejection_trigger", JString (ckRejectionTrigger result))
               , ("details", JString (ckDetails result))
               ]
        )

stageVerdictJson :: PrototypeRequest -> P2Execution -> Int -> JValue
stageVerdictJson req execution attemptId =
    JObject
        ( metadataPairs (metadataFor req True)
            ++ [ ("subject_token_ref", maybe JNull JString (fmap (const (p2SubjectTokenRelativePath attemptId)) (p2OutputToken execution)))
               , ("checker_results", JArray (map (JString . p2CheckResultRelativePath attemptId . ckCheckId . p2caResult) (p2Checks execution)))
               , ("stage_result", JString (p2StageResult execution))
               , ("terminal_reason", JString "none")
               ]
        )

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

firstShow :: Show e => Either e a -> Either String a
firstShow = either (Left . show) Right
