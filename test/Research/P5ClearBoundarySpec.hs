module Research.P5ClearBoundarySpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Test.Hspec

import qualified MLF.Constraint.Finalize as Finalize
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Presolution.View as PresolutionViewBoundary
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Canonicalizer (canonicalizeNode)
import MLF.Constraint.Presolution
import MLF.Constraint.Presolution.Plan.Context
    ( GaBindParents(..)
    )
import MLF.Constraint.Presolution.TestSupport
    ( EdgeArtifacts(..)
    , defaultPlanBuilder
    )
import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , NodeId(..)
    , TyNode(..)
    , cBindParents
    , cNodes
    , fromListNode
    , getNodeId
    , nodeRefKey
    , toListNode
    , typeRef
    )
import MLF.Elab.Pipeline
    ( applyRedirectsToAnn
    , canonicalizeAnn
    , renderPipelineError
    , runPipelineElab
    , runPipelineElabChecked
    )
import MLF.Elab.Run.ResultType
    ( ResultTypeInputs(..)
    , computeResultTypeFallback
    , mkResultTypeInputs
    )
import MLF.Elab.Run.Util
    ( canonicalizeExpansion
    , canonicalizeTrace
    , canonicalizeWitness
    , makeCanonicalizer
    )
import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Frontend.Syntax
import MLF.Types.Elab
    ( BoundType
    , ElabType
    , Ty(..)
    )
import SpecUtil
    ( PipelineArtifacts(..)
    , defaultTraceConfig
    , requireRight
    , runPipelineArtifactsDefault
    , unsafeNormalizeExpr
    )

spec :: Spec
spec =
    describe "P5 clear-boundary retained-child probes" $ do
        it "sameLaneClearBoundaryExpr stays recursive as the first milestone-3 representative broader-positive clear-boundary anchor while the quantified boundary stays clear" $ do
            fallbackTy <- sameLaneClearBoundaryFallbackType
            containsMu fallbackTy `shouldBe` True

        it "sameLaneAliasFrameClearBoundaryExpr stays recursive as preserved predecessor alias-frame truth while the quantified boundary stays clear" $ do
            fallbackTy <- sameLaneAliasFrameClearBoundaryFallbackType
            containsMu fallbackTy `shouldBe` True

        it "sameLaneDoubleAliasFrameClearBoundaryExpr stays recursive as the next explicit milestone-3 representative broader-positive clear-boundary anchor while the quantified boundary stays clear" $ do
            fallbackTy <- sameLaneDoubleAliasFrameClearBoundaryFallbackType
            containsMu fallbackTy `shouldBe` True

        it "sameLaneTripleAliasFrameClearBoundaryExpr stays recursive as the next milestone-3 representative broader-positive clear-boundary packet after the merged double-alias anchor while the quantified boundary stays clear" $ do
            fallbackTy <- sameLaneTripleAliasFrameClearBoundaryFallbackType
            containsMu fallbackTy `shouldBe` True

        it "nestedForallContrastExpr stays recursive as preserved merged-baseline same-wrapper nested-forall success across a nested forall boundary" $ do
            fallbackTy <- fallbackType nestedForallContrastExpr
            containsMu fallbackTy `shouldBe` True

        it "sameLaneClearBoundaryExpr is the first explicit milestone-3 representative broader-positive clear-boundary packet on both authoritative entrypoints" $ do
            let pipelineRuns =
                    [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneClearBoundaryExpr))
                    , ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneClearBoundaryExpr))
                    ]
            mapM_
                (\(label, result) -> case result of
                    Left err ->
                        expectationFailure (label ++ ": " ++ renderPipelineError err)
                    Right (_term, ty) ->
                        containsMu ty `shouldBe` True
                )
                pipelineRuns

        it "sameLaneAliasFrameClearBoundaryExpr alias-frame clear-boundary packet preserves recursive output on both authoritative entrypoints" $ do
            let pipelineRuns =
                    [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneAliasFrameClearBoundaryExpr))
                    , ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneAliasFrameClearBoundaryExpr))
                    ]
            mapM_
                (\(label, result) -> case result of
                    Left err ->
                        expectationFailure (label ++ ": " ++ renderPipelineError err)
                    Right (_term, ty) ->
                        containsMu ty `shouldBe` True
                )
                pipelineRuns

        it "sameLaneDoubleAliasFrameClearBoundaryExpr is the next explicit milestone-3 representative broader-positive clear-boundary packet on both authoritative entrypoints" $ do
            let pipelineRuns =
                    [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneDoubleAliasFrameClearBoundaryExpr))
                    , ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneDoubleAliasFrameClearBoundaryExpr))
                    ]
            mapM_
                (\(label, result) -> case result of
                    Left err ->
                        expectationFailure (label ++ ": " ++ renderPipelineError err)
                    Right (_term, ty) ->
                        containsMu ty `shouldBe` True
                )
                pipelineRuns

        it "sameLaneTripleAliasFrameClearBoundaryExpr is the next milestone-3 representative broader-positive clear-boundary packet after the merged double-alias anchor on both authoritative entrypoints" $ do
            let pipelineRuns =
                    [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr sameLaneTripleAliasFrameClearBoundaryExpr))
                    , ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr sameLaneTripleAliasFrameClearBoundaryExpr))
                    ]
            mapM_
                (\(label, result) -> case result of
                    Left err ->
                        expectationFailure (label ++ ": " ++ renderPipelineError err)
                    Right (_term, ty) ->
                        containsMu ty `shouldBe` True
                )
                pipelineRuns

        it "selected same-wrapper nested-forall preserved merged-baseline packet stays recursive on both authoritative entrypoints" $ do
            let pipelineRuns =
                    [ ("unchecked", runPipelineElab Set.empty (unsafeNormalizeExpr nestedForallContrastExpr))
                    , ("checked", runPipelineElabChecked Set.empty (unsafeNormalizeExpr nestedForallContrastExpr))
                    ]
            mapM_
                (\(label, result) -> case result of
                    Left err ->
                        expectationFailure (label ++ ": " ++ renderPipelineError err)
                    Right (_term, ty) ->
                        containsMu ty `shouldBe` True
                )
                pipelineRuns

sameLaneClearBoundaryExpr :: SurfaceExpr
sameLaneClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "k")) (EVar "u"))

sameLaneAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "hold")) (EVar "u")))

sameLaneDoubleAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneDoubleAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "keep" (EVar "hold")
                (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "keep")) (EVar "u"))))

sameLaneTripleAliasFrameClearBoundaryExpr :: SurfaceExpr
sameLaneTripleAliasFrameClearBoundaryExpr =
    ELet "k" (ELamAnn "x" recursiveAnn (EVar "x"))
        (ELet "hold" (EVar "k")
            (ELet "keep" (EVar "hold")
                (ELet "more" (EVar "keep")
                    (ELet "u" (EApp (ELam "y" (EVar "y")) (EVar "more")) (EVar "u")))))

nestedForallContrastExpr :: SurfaceExpr
nestedForallContrastExpr =
    ELet "id" (ELam "z" (EVar "z"))
        (ELet "k" (EApp (EVar "id") (ELamAnn "x" recursiveAnn (EVar "x")))
            (EApp (ELam "y" (EVar "y")) (EVar "k")))

recursiveAnn :: SrcType
recursiveAnn = STMu "a" (STArrow (STVar "a") (STBase "Int"))

sameLaneClearBoundaryFallbackType :: IO ElabType
sameLaneClearBoundaryFallbackType = do
    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneClearBoundaryExpr)
    let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
        innerCanon = extractInnerLetRhs annCanon0
        innerPre = extractInnerLetRhs annPre0
        (retainedRoot, retainedChild) = case innerCanon of
            AApp _ (AVar _ nid) _ _ rootNid -> (rootNid, nid)
            _ -> error ("expected retained-child app shape, got " ++ show innerCanon)
        inputs = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
    requireRight (computeResultTypeFallback inputs innerCanon innerPre)

sameLaneAliasFrameClearBoundaryFallbackType :: IO ElabType
sameLaneAliasFrameClearBoundaryFallbackType = do
    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneAliasFrameClearBoundaryExpr)
    let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
        innerCanon = extractFirstApp annCanon0
        innerPre = extractFirstApp annPre0
        (retainedRoot, retainedChild) = case innerCanon of
            AApp _ (AVar _ nid) _ _ rootNid -> (rootNid, nid)
            _ -> error ("expected retained-child app shape, got " ++ show innerCanon)
        inputs = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
    requireRight (computeResultTypeFallback inputs innerCanon innerPre)

sameLaneDoubleAliasFrameClearBoundaryFallbackType :: IO ElabType
sameLaneDoubleAliasFrameClearBoundaryFallbackType = do
    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneDoubleAliasFrameClearBoundaryExpr)
    let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
        innerCanon = extractFirstApp annCanon0
        innerPre = extractFirstApp annPre0
        (retainedRoot, retainedChild) = case innerCanon of
            AApp _ (AVar _ nid) _ _ rootNid -> (rootNid, nid)
            _ -> error ("expected retained-child app shape, got " ++ show innerCanon)
        inputs = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
    requireRight (computeResultTypeFallback inputs innerCanon innerPre)

sameLaneTripleAliasFrameClearBoundaryFallbackType :: IO ElabType
sameLaneTripleAliasFrameClearBoundaryFallbackType = do
    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty sameLaneTripleAliasFrameClearBoundaryExpr)
    let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
        innerCanon = extractFirstApp annCanon0
        innerPre = extractFirstApp annPre0
        (retainedRoot, retainedChild) = case innerCanon of
            AApp _ (AVar _ nid) _ _ rootNid -> (rootNid, nid)
            _ -> error ("expected retained-child app shape, got " ++ show innerCanon)
        inputs = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
    requireRight (computeResultTypeFallback inputs innerCanon innerPre)

fallbackType :: SurfaceExpr -> IO ElabType
fallbackType expr = do
    artifacts <- requireRight (runPipelineArtifactsDefault Set.empty expr)
    let (inputs0, annCanon0, annPre0) = resultTypeInputsForArtifacts artifacts
        bodyCanon = extractSelectedBodyApp annCanon0
        bodyPre = extractSelectedBodyApp annPre0
        (retainedRoot, retainedChild) = case bodyCanon of
            AApp _ (AVar _ nid) _ _ rootNid -> (rootNid, nid)
            other ->
                error
                    ("expected same-wrapper nested-forall retained-child app shape, got " ++ show other)
        inputs = wireSameLaneLocalRoot inputs0 retainedRoot retainedChild
    requireRight (computeResultTypeFallback inputs bodyCanon bodyPre)

extractInnerLetRhs :: AnnExpr -> AnnExpr
extractInnerLetRhs ann0 = case ann0 of
    ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ rhs _ _) _ _) _ -> rhs
    _ -> error ("unexpected retained-child wrapper shape: " ++ show ann0)

extractFirstApp :: AnnExpr -> AnnExpr
extractFirstApp ann0 = case ann0 of
    AApp {} -> ann0
    AAnn inner _ _ -> extractFirstApp inner
    ALam _ _ _ body _ -> extractFirstApp body
    ALet _ _ _ _ _ rhs body _ ->
        case extractFirstAppMaybe rhs of
            Just hit -> hit
            Nothing ->
                case extractFirstAppMaybe body of
                    Just hit -> hit
                    Nothing -> error ("unexpected retained-child wrapper shape: " ++ show ann0)
    _ -> error ("unexpected retained-child wrapper shape: " ++ show ann0)

extractFirstAppMaybe :: AnnExpr -> Maybe AnnExpr
extractFirstAppMaybe ann0 = case ann0 of
    AApp {} -> Just ann0
    AAnn inner _ _ -> extractFirstAppMaybe inner
    ALam _ _ _ body _ -> extractFirstAppMaybe body
    ALet _ _ _ _ _ rhs body _ ->
        case extractFirstAppMaybe rhs of
            Just hit -> Just hit
            Nothing -> extractFirstAppMaybe body
    _ -> Nothing

extractSelectedBodyApp :: AnnExpr -> AnnExpr
extractSelectedBodyApp ann0 = case ann0 of
    ALet _ _ _ _ _ _ (AAnn (ALet _ _ _ _ _ _ body _) _ _) _ ->
        case body of
            AAnn inner _ _ -> inner
            other -> other
    _ -> error ("unexpected same-wrapper nested-forall wrapper shape: " ++ show ann0)

wireSameLaneLocalRoot :: ResultTypeInputs -> NodeId -> NodeId -> ResultTypeInputs
wireSameLaneLocalRoot inputs rootNid childNid =
    let setVarBound' nid newBound constraint =
            let tweak node = case node of
                    TyVar{ tnId = varId } | varId == nid ->
                        TyVar{ tnId = varId, tnBound = Just newBound }
                    _ -> node
            in constraint
                { cNodes =
                    fromListNode
                        [ (nodeIdKey, tweak node)
                        | (nodeIdKey, node) <- toListNode (cNodes constraint)
                        ]
                }
        setTypeParent' child mbParent constraint =
            let childKey = nodeRefKey (typeRef child)
                bindParents' = case mbParent of
                    Nothing ->
                        IntMap.delete childKey (cBindParents constraint)
                    Just parentRef ->
                        IntMap.insert childKey (parentRef, BindFlex) (cBindParents constraint)
            in constraint { cBindParents = bindParents' }
        view0 = rtcPresolutionView inputs
        retainedTarget =
            case pvLookupVarBound view0 childNid of
                Just boundNid -> boundNid
                Nothing ->
                    error ("expected retained child bound for " ++ show childNid)
        rewrite =
            setVarBound' rootNid retainedTarget
                . setVarBound' childNid retainedTarget
                . setTypeParent' childNid (Just (typeRef rootNid))
                . setTypeParent' rootNid Nothing
        baseConstraint' = rewrite (pvConstraint view0)
        canonicalConstraint' = rewrite (pvCanonicalConstraint view0)
        view' =
            view0
                { pvConstraint = baseConstraint'
                , pvLookupNode =
                    \nid -> NodeAccess.lookupNode baseConstraint' ((pvCanonical view0) nid)
                , pvLookupVarBound =
                    \nid -> NodeAccess.lookupVarBound baseConstraint' ((pvCanonical view0) nid)
                , pvLookupBindParent = NodeAccess.lookupBindParent baseConstraint'
                , pvBindParents = cBindParents baseConstraint'
                , pvCanonicalConstraint = canonicalConstraint'
                }
        ga0 = rtcBindParentsGa inputs
        ga' =
            ga0
                { gaBindParentsBase = cBindParents baseConstraint'
                , gaBaseConstraint = baseConstraint'
                }
    in inputs
        { rtcPresolutionView = view'
        , rtcBaseConstraint = baseConstraint'
        , rtcBindParentsGa = ga'
        }

resultTypeInputsForArtifacts :: PipelineArtifacts -> (ResultTypeInputs, AnnExpr, AnnExpr)
resultTypeInputsForArtifacts
    PipelineArtifacts
        { paConstraintNorm = c1
        , paPresolution = pres
        , paSolved = solved0
        , paAnnotated = ann0
        } =
    let solvedClean = Finalize.stepPruneSolvedBindParents solved0
        canon = makeCanonicalizer (Solved.canonicalMap solvedClean) (prRedirects pres)
        canonical = canonicalizeNode canon
        annRedirected = applyRedirectsToAnn (prRedirects pres) ann0
        annCanon = canonicalizeAnn canonical annRedirected
        edgeWitnesses = IntMap.map (canonicalizeWitness canon) (prEdgeWitnesses pres)
        edgeTraces = IntMap.map (canonicalizeTrace canon) (prEdgeTraces pres)
        edgeExpansions = IntMap.map (canonicalizeExpansion canon) (prEdgeExpansions pres)
        baseNodeKeys =
            [ getNodeId nid
            | (nid, _) <- toListNode (cNodes c1)
            ]
        baseToSolved =
            IntMap.fromList
                [ (baseKey, canonical (NodeId baseKey))
                | baseKey <- baseNodeKeys
                ]
        solvedToBase =
            foldl'
                (\acc (baseKey, solvedNid) ->
                    IntMap.insertWith (\_ existing -> existing) (getNodeId solvedNid) (NodeId baseKey) acc
                )
                IntMap.empty
                (IntMap.toList baseToSolved)
        bindParentsGa =
            GaBindParents
                { gaBindParentsBase = cBindParents c1
                , gaBaseConstraint = c1
                , gaBaseToSolved = baseToSolved
                , gaSolvedToBase = solvedToBase
                }
        inputs =
            mkResultTypeInputs
                canonical
                EdgeArtifacts
                    { eaEdgeExpansions = edgeExpansions
                    , eaEdgeWitnesses = edgeWitnesses
                    , eaEdgeTraces = edgeTraces
                    }
                (PresolutionViewBoundary.fromSolved solvedClean)
                bindParentsGa
                (defaultPlanBuilder defaultTraceConfig)
                c1
                (prRedirects pres)
                defaultTraceConfig
    in (inputs, annCanon, ann0)

containsMu :: ElabType -> Bool
containsMu ty = case ty of
    TMu _ _ -> True
    TArrow dom cod -> containsMu dom || containsMu cod
    TCon _ args -> any containsMu args
    TForall _ mb body -> maybe False containsMuBound mb || containsMu body
    _ -> False
  where
    containsMuBound :: BoundType -> Bool
    containsMuBound bound = case bound of
        TArrow dom cod -> containsMu dom || containsMu cod
        TBase _ -> False
        TCon _ args -> any containsMu args
        TForall _ mb body -> maybe False containsMuBound mb || containsMu body
        TMu _ _ -> True
        TBottom -> False
