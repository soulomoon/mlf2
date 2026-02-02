{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.ResultType (
    ResultTypeContext(..),
    computeResultTypeFromAnn,
    computeResultTypeFallback
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)
import Data.Functor.Foldable (cata)

import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Presolution.Base (CopyMapping(..), lookupCopy)
import MLF.Constraint.Solve (SolveResult(..), frWith)
import MLF.Constraint.Types.Graph
    ( Constraint
    , EdgeId(..)
    , GenNode(..)
    , NodeId(..)
    , NodeRef(..)
    , TyNode(..)
    , cBindParents
    , cGenNodes
    , cLetEdges
    , cNodes
    , fromListNode
    , getEdgeId
    , getNodeId
    , lookupNodeIn
    , lookupGen
    , gnId
    , gnSchemes
    , nodeRefFromKey
    , toListNode
    )
import MLF.Constraint.Types.Witness (EdgeWitness(..), Expansion(..))
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Inst (schemeToType)
import MLF.Reify.Core (namedNodes)
import MLF.Elab.Phi (phiFromEdgeWitnessWithTrace)
import MLF.Elab.Types
import MLF.Reify.TypeOps (resolveBaseBoundForInstConstraint)
import MLF.Elab.Run.Annotation (annNode)
import MLF.Elab.Run.Debug (debugGaScope, debugGaScopeEnabled, debugWhenCondM, debugWhenM)
import MLF.Util.Trace (TraceConfig)
import MLF.Elab.Run.Scope
    ( bindingScopeRef
    , canonicalizeScopeRef
    , resolveCanonicalScope
    , schemeBodyTarget
    )
import MLF.Elab.Run.TypeOps
    ( inlineBoundVarsType
    , inlineBoundVarsTypeForBound
    )
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Run.ResultType.Util
    ( generalizeWithPlan
    , stripAnn
    , collectEdges
    )
import MLF.Elab.Run.ResultType.Types (ResultTypeContext(..))
import qualified MLF.Elab.Run.ResultType.Ann as Ann

-- Re-export computeResultTypeFromAnn from Ann module
computeResultTypeFromAnn :: ResultTypeContext -> AnnExpr -> AnnExpr -> NodeId -> EdgeId -> Either ElabError ElabType
computeResultTypeFromAnn = Ann.computeResultTypeFromAnn

-- | Compute result type when there's no direct annotation (fallback path).
computeResultTypeFallback
    :: ResultTypeContext
    -> AnnExpr      -- ^ annCanon (post-redirect)
    -> AnnExpr      -- ^ ann (pre-redirect)
    -> Either ElabError ElabType
computeResultTypeFallback ctx annCanon ann = do
    let canonical = rtcCanonical ctx
        edgeWitnesses = rtcEdgeWitnesses ctx
        edgeTraces = rtcEdgeTraces ctx
        edgeExpansions = rtcEdgeExpansions ctx
        solvedForGen = rtcSolvedForGen ctx
        solvedClean = rtcSolvedClean ctx
        solved = solvedClean
        bindParentsGa = rtcBindParentsGa ctx
        planBuilder = rtcPlanBuilder ctx
        c1 = rtcBaseConstraint ctx
        redirects = rtcRedirects ctx
        traceCfg = rtcTraceConfig ctx
        generalizeAtWith = generalizeAtWithBuilder planBuilder

    let edgeTraceCounts =
            IntMap.fromListWith
                (+)
                [ (getNodeId (etRoot tr), 1 :: Int)
                | tr <- IntMap.elems edgeTraces
                ]
    let schemeRootSet =
            IntSet.fromList
                [ getNodeId (canonical root)
                | gen <- NodeAccess.allGenNodes (srConstraint solvedForGen)
                , root <- gnSchemes gen
                ]
        isSchemeRoot nid =
            IntSet.member (getNodeId (canonical nid)) schemeRootSet
        letEdges = cLetEdges c1
        isLetEdge (EdgeId eid) = IntSet.member eid letEdges
    let rootForTypeAnn =
            let peel ann0 = case ann0 of
                    ALet _ _ _ _ _ _ bodyAnn nid ->
                        case bodyAnn of
                            AAnn inner target eid
                                | canonical target == canonical nid
                                    && isLetEdge eid ->
                                        peel inner
                                | canonical target == canonical nid
                                    && not (isSchemeRoot target) ->
                                        peel inner
                            _ -> bodyAnn
                    _ -> ann0
            in peel annCanon
        rootForType = annNode rootForTypeAnn
        rootForTypePreAnn =
            let peel ann0 = case ann0 of
                    ALet _ _ _ _ _ _ bodyAnn nid ->
                        case bodyAnn of
                            AAnn inner target eid
                                | target == nid
                                    && isLetEdge eid ->
                                        peel inner
                                | target == nid
                                    && not (isSchemeRoot target) ->
                                        peel inner
                            _ -> bodyAnn
                    _ -> ann0
            in peel ann
        rootForTypePre = annNode rootForTypePreAnn
    case rootForTypeAnn of
        AAnn inner annNodeId eid -> do
            let innerPre =
                    case rootForTypePreAnn of
                        AAnn innerPre0 _ _ -> innerPre0
                        _ -> rootForTypePreAnn
            computeResultTypeFromAnn ctx inner innerPre annNodeId eid
        _ -> do
            let rootC = canonical rootForType
                constraint = srConstraint solved
                nodes = cNodes constraint
                nodeList = map snd (toListNode nodes)
                rootInstRoots =
                    [ etRoot tr
                    | EdgeId eid <- collectEdges rootForTypeAnn
                    , Just tr <- [IntMap.lookup eid edgeTraces]
                    ]
                rootHasMultiInst =
                    any
                        (\root -> IntMap.findWithDefault 0 (getNodeId root) edgeTraceCounts > 1)
                        rootInstRoots
                collectInstApps :: Instantiation -> [ElabType]
                collectInstApps = cata alg
                  where
                    alg inst0 = case inst0 of
                        InstAppF ty -> [ty]
                        InstSeqF a b -> a ++ b
                        InstInsideF phi -> phi
                        InstUnderF _ phi -> phi
                        _ -> []
                baseNodeForTy ty =
                    case ty of
                        TBase base ->
                            listToMaybe
                                [ tnId node
                                | node@TyBase{} <- nodeList
                                , tnBase node == base
                                ]
                        _ -> Nothing
                instAppBasesFromWitness funEid =
                    case IntMap.lookup (getEdgeId funEid) edgeWitnesses of
                        Just ew ->
                            case phiFromEdgeWitnessWithTrace traceCfg generalizeAtWith solved (Just bindParentsGa) Nothing (IntMap.lookup (getEdgeId funEid) edgeTraces) ew of
                                Right inst ->
                                    IntSet.fromList
                                        [ getNodeId nid
                                        | ty <- collectInstApps inst
                                        , Just nid <- [baseNodeForTy ty]
                                        ]
                                Left _ -> IntSet.empty
                        Nothing -> IntSet.empty
                argBounds arg = do
                    baseC <- resolveBaseBoundForInstConstraint constraint canonical arg
                    case lookupNodeIn nodes baseC of
                        Just TyBase{} -> Just baseC
                        Just TyBottom{} -> Just baseC
                        _ -> Nothing
                instArgNode tr binder arg =
                    case lookupCopy binder (etCopyMap tr) of
                        Just copyN -> copyN
                        Nothing -> arg
                edgeBaseBounds =
                    IntSet.fromList
                        [ getNodeId baseC
                        | ew <- IntMap.elems edgeWitnesses
                        , Just baseC <- [argBounds (ewRight ew)]
                        ]
                rootArgBaseBounds =
                    let argEdgeBases eid =
                            case IntMap.lookup (getEdgeId eid) edgeTraces of
                                Nothing -> IntSet.empty
                                Just tr ->
                                    IntSet.fromList
                                        [ getNodeId baseC
                                        | (binder, arg) <- etBinderArgs tr
                                        , let argNode =
                                                instArgNode tr binder arg
                                        , Just baseC <- [argBounds argNode]
                                        ]
                    in case stripAnn rootForTypeAnn of
                        AApp _ arg funEid argEid _ ->
                            let fromWitness = instAppBasesFromWitness funEid
                                fromArgEdge = argEdgeBases argEid
                            in if not (IntSet.null fromWitness)
                                then fromWitness
                                else if not (IntSet.null fromArgEdge)
                                    then fromArgEdge
                                    else
                                        case resolveBaseBoundForInstConstraint constraint canonical (annNode arg) of
                                            Just baseC -> IntSet.singleton (getNodeId baseC)
                                            Nothing -> IntSet.empty
                        _ -> IntSet.empty
                instArgBaseBounds =
                    let binderBounds =
                            IntMap.fromListWith IntSet.union
                                [ (getNodeId (canonical binderRoot), IntSet.singleton (getNodeId baseC))
                                | tr <- IntMap.elems edgeTraces
                                , let copyMapInv =
                                        IntMap.fromListWith
                                            min
                                            [ (getNodeId copyN, getNodeId origN)
                                            | (origKey, copyN) <- IntMap.toList (getCopyMapping (etCopyMap tr))
                                            , let origN = NodeId origKey
                                            ]
                                , (binder, arg) <- etBinderArgs tr
                                , let binderRoot =
                                        case IntMap.lookup (getNodeId binder) copyMapInv of
                                            Just origKey -> NodeId origKey
                                            Nothing -> binder
                                , let argNode = instArgNode tr binder arg
                                , Just baseC <- [argBounds argNode]
                                ]
                    in IntSet.union
                        (IntSet.unions
                        [ bounds
                        | bounds <- IntMap.elems binderBounds
                        , IntSet.size bounds > 1
                        ])
                        edgeBaseBounds
                (rootBounds, instArgRootMultiBase) =
                    let rootBounds' =
                            IntMap.fromListWith IntSet.union
                                [ (getNodeId (canonical (etRoot tr)), IntSet.singleton (getNodeId baseC))
                                | tr <- IntMap.elems edgeTraces
                                , (binder, arg) <- etBinderArgs tr
                                , let argNode = instArgNode tr binder arg
                                , Just baseC <- [argBounds argNode]
                                ]
                        multiBase =
                            any (\s -> IntSet.size s > 1) (IntMap.elems rootBounds')
                                || IntSet.size edgeBaseBounds > 1
                    in (rootBounds', multiBase)

            let rootBaseBounds =
                    IntMap.findWithDefault IntSet.empty (getNodeId rootC) rootBounds
                rootBoundCandidates =
                    if IntSet.null rootBaseBounds
                        then rootArgBaseBounds
                        else rootBaseBounds
            let baseTarget =
                    case lookupNodeIn nodes rootC of
                        Just TyVar{} ->
                            case resolveBaseBoundForInstConstraint constraint canonical rootC of
                                Just baseC
                                    | IntSet.size rootBoundCandidates == 1
                                        && IntSet.member (getNodeId baseC) rootBoundCandidates
                                        && not rootHasMultiInst
                                        && not instArgRootMultiBase ->
                                        Just baseC
                                    | IntSet.null rootBoundCandidates
                                        && IntSet.null instArgBaseBounds
                                        && not rootHasMultiInst
                                        && not instArgRootMultiBase ->
                                        Just baseC
                                    | IntSet.null rootBaseBounds
                                        && not instArgRootMultiBase
                                        && not rootHasMultiInst
                                        && IntSet.size instArgBaseBounds == 1
                                        && IntSet.member (getNodeId baseC) instArgBaseBounds ->
                                        Just baseC
                                    | otherwise ->
                                        case IntSet.toList rootBoundCandidates of
                                            [baseKey]
                                                | not rootHasMultiInst
                                                    && not instArgRootMultiBase ->
                                                    case lookupNodeIn nodes (NodeId baseKey) of
                                                        Just TyBase{} -> Just (NodeId baseKey)
                                                        Just TyBottom{} -> Just (NodeId baseKey)
                                                        _ -> Nothing
                                            _ -> Nothing
                                _ ->
                                    case IntSet.toList rootBoundCandidates of
                                        [baseKey]
                                            | not rootHasMultiInst
                                                && not instArgRootMultiBase ->
                                                case lookupNodeIn nodes (NodeId baseKey) of
                                                    Just TyBase{} -> Just (NodeId baseKey)
                                                    Just TyBottom{} -> Just (NodeId baseKey)
                                                    _ -> Nothing
                                        _ -> Nothing
                        _ -> Nothing
            let boundTarget =
                    case (baseTarget, lookupNodeIn nodes rootC) of
                        (Nothing, Just TyVar{ tnBound = Nothing }) ->
                            case IntSet.toList rootBoundCandidates of
                                [baseKey] -> Just (NodeId baseKey)
                                _ -> Nothing
                        _ -> Nothing
            debugWhenM traceCfg
                (let showBase tr =
                        let bases =
                                [ resolveBaseBoundForInstConstraint constraint canonical argNode
                                | (binder, arg) <- etBinderArgs tr
                                , let argNode =
                                        case lookupCopy binder (etCopyMap tr) of
                                            Just copyN -> copyN
                                            Nothing -> arg
                                ]
                        in (etRoot tr, bases)
                     perEdge = map showBase (IntMap.elems edgeTraces)
                     edgeRightBases =
                        [ (EdgeId eid, ewRight ew, resolveBaseBoundForInstConstraint constraint canonical (ewRight ew))
                        | (eid, ew) <- IntMap.toList edgeWitnesses
                        ]
                     edgeExpansionsList =
                        [ (EdgeId eid, expn)
                        | (eid, expn) <- IntMap.toList edgeExpansions
                        ]
                 in "runPipelineElab: instArgBaseBounds="
                        ++ show instArgBaseBounds
                        ++ " instArgRootMultiBase="
                        ++ show instArgRootMultiBase
                        ++ " rootArgBaseBounds="
                        ++ show rootArgBaseBounds
                        ++ " rootBoundCandidates="
                        ++ show rootBoundCandidates
                        ++ " rootHasMultiInst="
                        ++ show rootHasMultiInst
                        ++ " rootForTypeAnn="
                        ++ case stripAnn rootForTypeAnn of
                            AApp _ _ funEid argEid nid ->
                                "AApp funEid=" ++ show funEid
                                    ++ " argEid=" ++ show argEid
                                    ++ " appNode=" ++ show nid
                            other -> show (annNode other)
                        ++ " baseNames="
                        ++ show
                            [ tnBase node
                            | node@TyBase{} <- map snd (toListNode nodes)
                            ]
                        ++ " perEdgeBases="
                        ++ show perEdge
                        ++ " edgeRightBases="
                        ++ show edgeRightBases
                        ++ " edgeExpansions="
                        ++ show edgeExpansionsList
                )
            let resFinal =
                    case baseTarget of
                        Just _ -> solvedClean
                        Nothing -> solvedForGen
                resFinalBounded =
                    case boundTarget of
                        Nothing -> resFinal
                        Just baseN ->
                            let nodes0 = cNodes (srConstraint resFinal)
                                adjustNode node =
                                    case node of
                                        TyVar{ tnId = nid, tnBound = Nothing } ->
                                            TyVar{ tnId = nid, tnBound = Just (canonical baseN) }
                                        _ -> node
                                nodes' =
                                    fromListNode
                                        [ (nid, if nid == rootC then adjustNode node else node)
                                        | (nid, node) <- toListNode nodes0
                                        ]
                                constraint' = (srConstraint resFinal) { cNodes = nodes' }
                            in resFinal { srConstraint = constraint' }
            let scopeRootNodePre = rootForTypePre
            scopeRootPre <- bindingToElab (resolveCanonicalScope c1 resFinalBounded redirects scopeRootNodePre)
            let scopeRootPost =
                    case bindingScopeRef (srConstraint resFinalBounded) rootC of
                        Right ref -> canonicalizeScopeRef resFinalBounded redirects ref
                        Left _ -> scopeRootPre
                scopeRoot = scopeRootPre
            debugWhenCondM traceCfg (scopeRootPre /= scopeRootPost)
                ("runPipelineElab: ga' mismatch pre="
                    ++ show scopeRootPre
                    ++ " post="
                    ++ show scopeRootPost
                    ++ " preNode="
                    ++ show rootForTypePre
                    ++ " postNode="
                    ++ show rootC
                )
            let canonicalFinal = frWith (srUnionFind resFinalBounded)
                rootFinal = canonicalFinal rootC
                nodesFinal = cNodes (srConstraint resFinalBounded)
                rootBound =
                    case lookupNodeIn nodesFinal rootFinal of
                        Just TyVar{ tnBound = Just bnd } -> Just (canonicalFinal bnd)
                        _ -> Nothing
                rootBoundIsBase =
                    case rootBound of
                        Just bnd ->
                            case lookupNodeIn nodesFinal bnd of
                                Just TyBase{} -> True
                                Just TyBottom{} -> True
                                _ -> False
                        Nothing -> False
                rootIsSchemeRoot =
                    any
                        (\gen -> any (\root -> canonicalFinal root == rootFinal) (gnSchemes gen))
                        (NodeAccess.allGenNodes (srConstraint resFinalBounded))
                rootIsSchemeAlias =
                    rootIsSchemeRoot
                        && maybe False (const True) rootBound
                rootBoundIsBaseLike = rootBoundIsBase
                boundVarTargetRoot = canonicalFinal (schemeBodyTarget resFinalBounded rootC)
                schemeRootSetFinal =
                    IntSet.fromList
                        [ getNodeId (canonicalFinal root)
                        | gen <- NodeAccess.allGenNodes (srConstraint resFinalBounded)
                        , root <- gnSchemes gen
                        ]
                schemeRootOwnerFinal =
                    IntMap.fromList
                        [ (getNodeId (canonicalFinal root), gnId gen)
                        | gen <- NodeAccess.allGenNodes (srConstraint resFinalBounded)
                        , root <- gnSchemes gen
                        ]
                boundHasForallFrom start0 =
                    let go visited nid0 =
                            let nid = canonicalFinal nid0
                                key = getNodeId nid
                                isNestedSchemeRoot =
                                    case IntMap.lookup key schemeRootOwnerFinal of
                                        Just gid ->
                                            case scopeRoot of
                                                GenRef gidScope -> gid /= gidScope
                                                TypeRef _ -> True
                                        Nothing -> False
                                reportHit label =
                                    if debugGaScopeEnabled traceCfg
                                        then
                                            let owner = IntMap.lookup key schemeRootOwnerFinal
                                                nodeTag =
                                                    case lookupNodeIn nodesFinal (NodeId key) of
                                                        Just TyVar{} -> "var"
                                                        Just TyArrow{} -> "arrow"
                                                        Just TyForall{} -> "forall"
                                                        Just TyExp{} -> "exp"
                                                        Just TyBase{} -> "base"
                                                        Just TyBottom{} -> "bottom"
                                                        Nothing -> "missing"
                                            in debugGaScope traceCfg
                                                ("runPipelineElab: boundHasForall hit="
                                                    ++ label
                                                    ++ " node="
                                                    ++ show nid
                                                    ++ " tag="
                                                    ++ nodeTag
                                                    ++ " owner="
                                                    ++ show owner
                                                    ++ " scope="
                                                    ++ show scopeRoot
                                                )
                                                ()
                                        else ()
                            in if IntSet.member key visited
                                then False
                                else if IntSet.member key schemeRootSetFinal
                                    then
                                        case lookupNodeIn nodesFinal (NodeId key) of
                                            Just TyVar{ tnBound = Just bnd } ->
                                                go (IntSet.insert key visited) bnd
                                            _ ->
                                                if isNestedSchemeRoot
                                                    then case reportHit "schemeRoot" of
                                                        () -> True
                                                    else False
                                else
                                    case lookupNodeIn nodesFinal (NodeId key) of
                                        Just TyForall{} ->
                                            case reportHit "TyForall" of
                                                () -> True
                                        Just TyVar{ tnBound = Just bnd } ->
                                            let bndC = canonicalFinal bnd
                                                bndKey = getNodeId bndC
                                                bndNested =
                                                    case IntMap.lookup bndKey schemeRootOwnerFinal of
                                                        Just gid ->
                                                            case scopeRoot of
                                                                GenRef gidScope -> gid /= gidScope
                                                                TypeRef _ -> True
                                                        Nothing -> False
                                            in if IntSet.member bndKey schemeRootSetFinal
                                                then
                                                    case lookupNodeIn nodesFinal bndC of
                                                        Just TyVar{ tnBound = Just bndInner } ->
                                                            go (IntSet.insert key visited) bndInner
                                                        _ ->
                                                            if bndNested
                                                                then case reportHit "boundSchemeRoot" of
                                                                    () -> True
                                                                else False
                                                else go (IntSet.insert key visited) bndC
                                        Just TyExp{ tnBody = b } ->
                                            go (IntSet.insert key visited) b
                                        Just TyArrow{ tnDom = d, tnCod = c } ->
                                            let visited' = IntSet.insert key visited
                                            in go visited' d || go visited' c
                                        _ -> False
                    in go IntSet.empty start0
                boundVarTarget =
                    case scopeRoot of
                        GenRef gid ->
                            let candidates =
                                    [ ( canonicalFinal child
                                      , canonicalFinal bnd
                                      , canonicalFinal (schemeBodyTarget resFinalBounded bnd)
                                      , boundHasForallFrom bnd
                                      )
                                    | (childKey, (parentRef, _flag)) <- IntMap.toList (cBindParents (srConstraint resFinalBounded))
                                    , parentRef == GenRef gid
                                    , TypeRef child <- [nodeRefFromKey childKey]
                                    , Just bnd <- [VarStore.lookupVarBound (srConstraint resFinalBounded) (canonicalFinal child)]
                                    ]
                                debugCandidates =
                                    if debugGaScopeEnabled traceCfg
                                        then
                                            debugGaScope traceCfg
                                                ("runPipelineElab: boundVarTargetRoot="
                                                    ++ show boundVarTargetRoot
                                                    ++ " candidates="
                                                    ++ show
                                                        [ (child, bnd, bndRoot, hasForall)
                                                        | (child, bnd, bndRoot, hasForall) <- candidates
                                                        ]
                                                )
                                                ()
                                        else ()
                            in case debugCandidates of
                                () ->
                                    listToMaybe
                                        [ child
                                        | (child, _bnd, bndRoot, hasForall) <- candidates
                                        , bndRoot == boundVarTargetRoot
                                        , not hasForall
                                        ]
                        _ -> Nothing
                keepTargetFinal =
                    rootHasMultiInst
                        || instArgRootMultiBase
                        || (rootIsSchemeAlias && rootBoundIsBaseLike)
                        || maybe False (const True) boundVarTarget
            let targetC =
                    case baseTarget of
                        Just baseC -> baseC
                        Nothing ->
                            if keepTargetFinal
                                then
                                    case lookupNodeIn nodesFinal rootFinal of
                                        Just TyVar{} -> rootFinal
                                        _ ->
                                            case boundVarTarget of
                                                Just v -> v
                                                Nothing -> schemeBodyTarget resFinalBounded rootC
                                else schemeBodyTarget resFinalBounded rootC
            let bindParentsGaFinal =
                    case boundTarget of
                        Just baseN ->
                            let baseConstraint = gaBaseConstraint bindParentsGa
                                baseRoot =
                                    IntMap.findWithDefault rootC
                                        (getNodeId rootC)
                                        (gaSolvedToBase bindParentsGa)
                                nodes0 = cNodes baseConstraint
                                adjustNode node =
                                    case node of
                                        TyVar{ tnId = nid, tnBound = Nothing } ->
                                            TyVar{ tnId = nid, tnBound = Just baseN }
                                        _ -> node
                                nodes' =
                                    fromListNode
                                        [ (nid, if nid == baseRoot then adjustNode node else node)
                                        | (nid, node) <- toListNode nodes0
                                        ]
                                baseConstraint' = baseConstraint { cNodes = nodes' }
                            in bindParentsGa { gaBaseConstraint = baseConstraint' }
                        Nothing -> bindParentsGa
            (sch, _subst) <-
                generalizeWithPlan planBuilder bindParentsGaFinal resFinalBounded scopeRoot targetC
            debugWhenM traceCfg
                ("runPipelineElab: final scheme="
                    ++ pretty sch
                    ++ " keepTargetBase="
                    ++ show (case baseTarget of { Just _ -> True; Nothing -> False })
                    ++ " targetC="
                    ++ show targetC
                    ++ " scopeRoot="
                    ++ show scopeRoot
                )
            let ty = case sch of
                    Forall binds body -> foldr (\(n, b) t -> TForall n b t) body binds
            pure ty
