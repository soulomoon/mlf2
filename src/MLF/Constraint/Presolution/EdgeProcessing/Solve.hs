{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.EdgeProcessing.Solve
Description : Shared solve/unify helpers for edge processing
Copyright   : (c) 2024
License     : BSD-3-Clause

Core solve and unify operations used by both the edge processing orchestrator
and the interpreter. Extracted to break the module cycle between EdgeProcessing
and Interpreter.
-}
module MLF.Constraint.Presolution.EdgeProcessing.Solve (
    unifyStructure,
    solveNonExpInstantiation,
    recordEdgeExecutionArtifacts,
) where

import Control.Monad.State
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Control.Monad (forM_, unless, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import MLF.Util.Trace (traceBindingM)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types.Graph
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.StateAccess (
    getCanonical,
    getConstraintAndCanonical,
    findSchemeIntroducerM
    )
import MLF.Constraint.Presolution.Ops (
    findRoot,
    getCanonicalNode,
    setVarBound,
    )
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Expansion (
    applyExpansionEdgeTracedAtTarget,
    decideMinimalExpansion,
    getExpansion,
    mergeExpansions,
    setExpansion
    )
import MLF.Constraint.Types.Witness
import MLF.Constraint.Presolution.Unify (unifyAcyclic)
import qualified MLF.Constraint.Unify.Decompose as UnifyDecompose

-- | Commit all proof authority for one edge in one state transition.  Equal
-- duplicate writes are replay-safe; any changed field is a hard conflict.
{-# INLINE recordEdgeExecutionArtifacts #-}
recordEdgeExecutionArtifacts
    :: EdgeId
    -> EdgeExecutionArtifacts
    -> PresolutionM p ()
recordEdgeExecutionArtifacts edgeId@(EdgeId eid) artifacts = do
    unless (ewEdgeId (eeaWitness artifacts) == edgeId) $
        throwError $
            InternalError $
                "edge execution artifact witness id mismatch for " ++ show edgeId
    st <- get
    case IntMap.lookup eid (psEdgeExecutionArtifacts st) of
        Nothing ->
            put
                st
                    { psEdgeExecutionArtifacts =
                        IntMap.insert
                            eid
                            artifacts
                            (psEdgeExecutionArtifacts st)
                    }
        Just prior
            | prior == artifacts -> pure ()
            | otherwise ->
                throwError $
                    InternalError $
                        unlines
                            [ "conflicting edge execution artifact write for " ++ show edgeId
                            , "expansion changed: "
                                ++ show (eeaExpansion prior /= eeaExpansion artifacts)
                            , "witness changed: "
                                ++ show (eeaWitness prior /= eeaWitness artifacts)
                            , "raise authority changed: "
                                ++ show
                                    ( eeaRaiseAuthorityNodes prior
                                        /= eeaRaiseAuthorityNodes artifacts
                                    )
                            , "non-source origins changed: "
                                ++ show
                                    ( eeaNonSourceOpOrigins prior
                                        /= eeaNonSourceOpOrigins artifacts
                                    )
                            , "trace changed: "
                                ++ show (eeaTrace prior /= eeaTrace artifacts)
                            , "construction changed: "
                                ++ show
                                    ( eeaExpansionConstruction prior
                                        /= eeaExpansionConstruction artifacts
                                    )
                            ]

unifyStructure :: NodeId -> NodeId -> PresolutionM p ()
unifyStructure n1 n2 = do
    root1 <- findRoot n1
    root2 <- findRoot n2
    debugBindParents
        ( "unifyStructure: n1="
            ++ show n1
            ++ " root1="
            ++ show root1
            ++ " n2="
            ++ show n2
            ++ " root2="
            ++ show root2
        )
    if root1 == root2 then return ()
    else do
        node1 <- getCanonicalNode n1
        node2 <- getCanonicalNode n2
        case (node1, node2) of
            (TyExp { tnBody = b1 }, TyExp { tnBody = b2 }) ->
                unifyStructure b1 b2
            (TyExp{}, _) ->
                unifyExpansionNode node1 (tnId node2)
            (_, TyExp{}) ->
                unifyExpansionNode node2 (tnId node1)
            _ ->
                unifyStructureNonExp node1 node2
  where
    unifyExpansionNode :: TyNode -> NodeId -> PresolutionM p ()
    unifyExpansionNode expNode@TyExp { tnExpVar = expVar, tnBody = expBody } targetId = do
        (c0, canonical) <- getConstraintAndCanonical
        gid <- findSchemeIntroducerM canonical c0 expBody
        targetNode <- getCanonicalNode targetId
        currentExp <- getExpansion expVar
        debugBindParents
            ( "unifyExpansionNode: expNode="
                ++ show (tnId expNode)
                ++ " body="
                ++ show expBody
                ++ " target="
                ++ show targetId
            )
        (reqExp, unifications) <- decideMinimalExpansion canonical gid True expNode targetNode
        debugBindParents
            ( "unifyExpansionNode: expNode="
                ++ show (tnId expNode)
                ++ " reqExp="
                ++ show reqExp
                ++ " unifications="
                ++ show unifications
            )
        finalExp <- mergeExpansions expVar currentExp reqExp
        debugBindParents
            ( "unifyExpansionNode: expNode="
                ++ show (tnId expNode)
                ++ " finalExp="
                ++ show finalExp
            )
        setExpansion expVar finalExp
        mapM_ (uncurry unifyStructure) unifications
        case finalExp of
            ExpIdentity ->
                unifyStructure expBody targetId
            _ -> do
                ( resNodeId
                  , (copyMap, _interior, frontier)
                  , _construction
                  ) <-
                    applyExpansionEdgeTracedAtTarget gid targetId finalExp expNode
                canonicalizeNodeId <- getCanonical
                let copyMapCanon =
                        IntMap.foldlWithKey'
                            (\acc orig copy ->
                                IntMap.insert
                                    (getNodeId (canonicalizeNodeId (NodeId orig)))
                                    copy acc)
                            IntMap.empty
                            (getCopyMapping copyMap)
                forM_ (IntSet.toList frontier) $ \nidInt -> do
                    case IntMap.lookup nidInt copyMapCanon of
                        Nothing -> pure ()
                        Just copy -> unifyStructure copy (NodeId nidInt)
                unifyStructure resNodeId targetId
                recordExpansionResult (tnId expNode) resNodeId
    unifyExpansionNode _ _ =
        error "unifyExpansionNode: expected TyExp node"
    unifyStructureNonExp :: TyNode -> TyNode -> PresolutionM p ()
    unifyStructureNonExp node1 node2 = do
        let isVarNode node = case node of
                TyVar{} -> True
                _ -> False
            trySetBound target bnd = do
                (c0, canonical) <- getConstraintAndCanonical
                let targetC = canonical target
                    bndC = canonical bnd
                occurs <- case Traversal.occursInUnder canonical (NodeAccess.lookupNode c0) targetC bndC of
                    Left _ -> pure True
                    Right ok -> pure ok
                if occurs
                    then throwError (OccursCheckPresolution targetC bndC)
                    else
                        when (bndC /= targetC) $
                            setVarBound targetC (Just bndC)
            isRigidNode :: Constraint p -> NodeId -> PresolutionM p Bool
            isRigidNode constraint nodeId = do
                underRigid <-
                    either
                        (throwError . BindingTreeError)
                        pure
                        (Binding.isUnderRigidBinder constraint (typeRef nodeId))
                let restricted =
                        case Binding.lookupBindParent constraint (typeRef nodeId) of
                            Just (_, BindRigid) -> True
                            _ -> False
                pure (restricted || underRigid)
            matchRigidStructure = go IntMap.empty IntMap.empty
              where
                go leftBinders rightBinders left right = do
                    leftRoot <- findRoot left
                    rightRoot <- findRoot right
                    if leftRoot == rightRoot
                        then pure ()
                        else
                            case
                                ( IntMap.lookup (getNodeId leftRoot) leftBinders
                                , IntMap.lookup (getNodeId rightRoot) rightBinders
                                ) of
                                (Just expectedRight, _) -> do
                                    expectedRightRoot <- findRoot expectedRight
                                    when (expectedRightRoot /= rightRoot) $
                                        rigidMismatch leftRoot rightRoot "inconsistent forall binder occurrence"
                                (_, Just expectedLeft) -> do
                                    expectedLeftRoot <- findRoot expectedLeft
                                    when (expectedLeftRoot /= leftRoot) $
                                        rigidMismatch leftRoot rightRoot "non-injective forall binder match"
                                _ -> do
                                    leftNode <- getCanonicalNode leftRoot
                                    rightNode <- getCanonicalNode rightRoot
                                    matchNodes leftBinders rightBinders leftNode rightNode

                matchNodes leftBinders rightBinders leftNode rightNode =
                    case (leftNode, rightNode) of
                        (TyForall {tnBody = leftBody}, TyForall {tnBody = rightBody}) -> do
                            (constraint, canonical) <- getConstraintAndCanonical
                            leftOrdered <-
                                either
                                    (throwError . BindingTreeError)
                                    pure
                                    (Binding.orderedBinders canonical constraint (typeRef (tnId leftNode)))
                            rightOrdered <-
                                either
                                    (throwError . BindingTreeError)
                                    pure
                                    (Binding.orderedBinders canonical constraint (typeRef (tnId rightNode)))
                            if length leftOrdered /= length rightOrdered
                                then rigidMismatch (tnId leftNode) (tnId rightNode) "forall binder arity mismatch"
                                else do
                                    let binderPairs =
                                            zip
                                                (map canonical leftOrdered)
                                                (map canonical rightOrdered)
                                        leftBinders' =
                                            foldr
                                                (\(leftBinder, rightBinder) ->
                                                    IntMap.insert (getNodeId leftBinder) rightBinder
                                                )
                                                leftBinders
                                                binderPairs
                                        rightBinders' =
                                            foldr
                                                (\(leftBinder, rightBinder) ->
                                                    IntMap.insert (getNodeId rightBinder) leftBinder
                                                )
                                                rightBinders
                                                binderPairs
                                    forM_ binderPairs $ \(leftBinder, rightBinder) ->
                                        matchBinderBounds
                                            leftBinders'
                                            rightBinders'
                                            constraint
                                            leftBinder
                                            rightBinder
                                    go leftBinders' rightBinders' leftBody rightBody
                        (TyArrow {tnDom = leftDom, tnCod = leftCod}, TyArrow {tnDom = rightDom, tnCod = rightCod}) -> do
                            go leftBinders rightBinders leftDom rightDom
                            go leftBinders rightBinders leftCod rightCod
                        (TyMu {tnBody = leftBody}, TyMu {tnBody = rightBody}) -> do
                            (constraint, canonical) <- getConstraintAndCanonical
                            let leftMuBinders =
                                    recursiveBinders canonical constraint (tnId leftNode) leftBody
                                rightMuBinders =
                                    recursiveBinders canonical constraint (tnId rightNode) rightBody
                            if length leftMuBinders /= length rightMuBinders
                                then rigidMismatch (tnId leftNode) (tnId rightNode) "recursive binder arity mismatch"
                                else do
                                    let binderPairs = zip leftMuBinders rightMuBinders
                                        leftBinders' =
                                            foldr
                                                (\(leftBinder, rightBinder) ->
                                                    IntMap.insert (getNodeId leftBinder) rightBinder
                                                )
                                                leftBinders
                                                binderPairs
                                        rightBinders' =
                                            foldr
                                                (\(leftBinder, rightBinder) ->
                                                    IntMap.insert (getNodeId rightBinder) leftBinder
                                                )
                                                rightBinders
                                                binderPairs
                                    go leftBinders' rightBinders' leftBody rightBody
                        (TyBase {tnBaseIdentity = leftIdentity}, TyBase {tnBaseIdentity = rightIdentity})
                            | leftIdentity == rightIdentity -> pure ()
                        (TyBottom {}, TyBottom {}) -> pure ()
                        (TyCon {tnConIdentity = leftIdentity, tnArgs = leftArgs}, TyCon {tnConIdentity = rightIdentity, tnArgs = rightArgs})
                            | leftIdentity == rightIdentity
                            , NE.length leftArgs == NE.length rightArgs ->
                                forM_
                                    (zip (NE.toList leftArgs) (NE.toList rightArgs))
                                    (uncurry (go leftBinders rightBinders))
                        (TyVarApp {tnVarHead = leftHead, tnArgs = leftArgs}, TyVarApp {tnVarHead = rightHead, tnArgs = rightArgs})
                            | NE.length leftArgs == NE.length rightArgs -> do
                                go leftBinders rightBinders leftHead rightHead
                                forM_
                                    (zip (NE.toList leftArgs) (NE.toList rightArgs))
                                    (uncurry (go leftBinders rightBinders))
                        (TyExp {tnExpVar = leftExp, tnBody = leftBody}, TyExp {tnExpVar = rightExp, tnBody = rightBody})
                            | leftExp == rightExp ->
                                go leftBinders rightBinders leftBody rightBody
                        (leftExp@TyExp {}, _) ->
                            -- A rigid outer structure can contain an
                            -- instantiable child.  Resolve that child's
                            -- expansion at this exact comparison boundary;
                            -- treating the TyExp wrapper itself as rigid
                            -- structure rejects valid nested schemes before
                            -- their presolution recipe is applied.
                            unifyStructure (tnId leftExp) (tnId rightNode)
                        (_, rightExp@TyExp {}) ->
                            unifyStructure (tnId leftNode) (tnId rightExp)
                        (leftVar@TyVar {}, rightVar@TyVar {}) ->
                            matchVariables leftBinders rightBinders leftVar rightVar
                        (leftVar@TyVar {}, _) ->
                            matchVariableWithType
                                leftBinders
                                rightBinders
                                True
                                leftVar
                                rightNode
                        (_, rightVar@TyVar {}) ->
                            matchVariableWithType
                                leftBinders
                                rightBinders
                                False
                                rightVar
                                leftNode
                        _ ->
                            rigidMismatch (tnId leftNode) (tnId rightNode) "rigid structural mismatch"

                matchBinderBounds leftBinders rightBinders constraint leftBinder rightBinder =
                    case
                        ( NodeAccess.lookupVarBound constraint leftBinder
                        , NodeAccess.lookupVarBound constraint rightBinder
                        ) of
                        (Nothing, Nothing) -> pure ()
                        (Just leftBound, Just rightBound) ->
                            go leftBinders rightBinders leftBound rightBound
                        (Nothing, Just rightBound) ->
                            requireBottom rightBound
                        (Just leftBound, Nothing) ->
                            requireBottom leftBound
                  where
                    requireBottom bound = do
                        boundNode <- getCanonicalNode bound
                        case boundNode of
                            TyBottom {} -> pure ()
                            _ -> rigidMismatch leftBinder rightBinder "forall binder bound mismatch"

                matchVariables leftBinders rightBinders leftVar@TyVar {tnBound = leftBound0} rightVar@TyVar {tnBound = rightBound0} = do
                    constraint <- getConstraint
                    leftLocked <- isRigidNode constraint (tnId leftVar)
                    rightLocked <- isRigidNode constraint (tnId rightVar)
                    case (leftLocked, rightLocked) of
                        (False, False) -> unifyStructure (tnId leftVar) (tnId rightVar)
                        (True, False) -> trySetBound (tnId rightVar) (tnId leftVar)
                        (False, True) -> trySetBound (tnId leftVar) (tnId rightVar)
                        (True, True) ->
                            case (leftBound0, rightBound0) of
                                (Just leftBound, Just rightBound) ->
                                    go leftBinders rightBinders leftBound rightBound
                                _ -> do
                                    matchedOwners <-
                                        matchOwningBinderRoots
                                            leftBinders
                                            rightBinders
                                            constraint
                                            (tnId leftVar)
                                            (tnId rightVar)
                                    unless matchedOwners $
                                        rigidMismatch (tnId leftVar) (tnId rightVar) "unmatched rigid variables"
                matchVariables _ _ leftNode rightNode =
                    rigidMismatch (tnId leftNode) (tnId rightNode) "internal rigid variable matcher expected variables"

                matchOwningBinderRoots leftBinders rightBinders constraint left right =
                    case
                        ( Binding.lookupBindParent constraint (typeRef left)
                        , Binding.lookupBindParent constraint (typeRef right)
                        ) of
                        (Just (TypeRef leftParent, _), Just (TypeRef rightParent, _)) -> do
                            leftParentNode <- getCanonicalNode leftParent
                            rightParentNode <- getCanonicalNode rightParent
                            case (leftParentNode, rightParentNode) of
                                (TyMu {}, TyMu {}) ->
                                    go leftBinders rightBinders leftParent rightParent >> pure True
                                (TyForall {}, TyForall {}) ->
                                    go leftBinders rightBinders leftParent rightParent >> pure True
                                _ -> pure False
                        _ -> pure False

                matchVariableWithType leftBinders rightBinders variableOnLeft variable@TyVar {tnBound = variableBound} otherNode = do
                    constraint <- getConstraint
                    variableLocked <- isRigidNode constraint (tnId variable)
                    if variableLocked
                        then
                            case variableBound of
                                Just bound ->
                                    if variableOnLeft
                                        then go leftBinders rightBinders bound (tnId otherNode)
                                        else go leftBinders rightBinders (tnId otherNode) bound
                                Nothing ->
                                    rigidMismatch (tnId variable) (tnId otherNode) "unmatched rigid variable"
                        else trySetBound (tnId variable) (tnId otherNode)
                matchVariableWithType _ _ _ variable otherNode =
                    rigidMismatch (tnId variable) (tnId otherNode) "internal rigid variable matcher expected a variable"

                rigidMismatch :: NodeId -> NodeId -> String -> PresolutionM p a
                rigidMismatch left right reason =
                    throwError (UnmatchableTypes left right reason)

                recursiveBinders canonical constraint root body =
                    let rootC = canonical root
                        reachable =
                            Traversal.reachableFromWithBounds
                                canonical
                                (NodeAccess.lookupNode constraint)
                                (canonical body)
                     in [ binderC
                        | (binder, TyVar {}) <- toListNode (cNodes constraint)
                        , let binderC = canonical binder
                        , IntSet.member (getNodeId binderC) reachable
                        , case Binding.lookupBindParent constraint (typeRef binderC) of
                            Just (TypeRef parent, _) -> canonical parent == rootC
                            _ -> False
                        ]
        case (node1, node2) of
            (TyVar { tnBound = mb1 }, _) | not (isVarNode node2) ->
                case mb1 of
                    Just b1 -> unifyStructure b1 (tnId node2)
                    Nothing -> trySetBound (tnId node1) (tnId node2)
            (_, TyVar { tnBound = mb2 }) | not (isVarNode node1) ->
                case mb2 of
                    Just b2 -> unifyStructure (tnId node1) b2
                    Nothing -> trySetBound (tnId node2) (tnId node1)
            _ -> do
                c0 <- getConstraint
                locked1 <- isRigidNode c0 (tnId node1)
                locked2 <- isRigidNode c0 (tnId node2)
                if (locked1 || locked2) && not (isVarNode node1 || isVarNode node2)
                    then
                        matchRigidStructure (tnId node1) (tnId node2)
                    else do
                        unifyAcyclic n1 n2
                        case (node1, node2) of
                            (TyVar { tnBound = mb1 }, TyVar { tnBound = mb2 }) ->
                                case (mb1, mb2) of
                                    (Just b1, Just b2) ->
                                        if b1 /= b2
                                            then do
                                                b1Node <- getCanonicalNode b1
                                                b2Node <- getCanonicalNode b2
                                                case (isVarNode b1Node, isVarNode b2Node) of
                                                    (True, False) -> trySetBound b1 b2
                                                    (False, True) -> trySetBound b2 b1
                                                    _ -> unifyStructure b1 b2
                                            else pure ()
                                    _ -> pure ()
                            _ ->
                                unifyStructureChildren node1 node2

    unifyStructureChildren :: TyNode -> TyNode -> PresolutionM p ()
    unifyStructureChildren node1 node2 =
        case UnifyDecompose.decomposeUnifyChildren node1 node2 of
            Right edges -> mapM_ (\edge -> unifyStructure (uniLeft edge) (uniRight edge)) edges
            Left _ -> pure ()

isSchemeRootNode :: (NodeId -> NodeId) -> Constraint p -> NodeId -> PresolutionM p Bool
isSchemeRootNode canonical c0 nid = do
    (_c, _canonical, qbp) <- cachedBindingModelM
    let refC = case typeRef nid of
            TypeRef n -> TypeRef (canonical n)
            r -> r
    case IntMap.lookup (nodeRefKey refC) (Binding.qbpBindParents qbp) of
        Just (GenRef gid, _) ->
            case NodeAccess.lookupGenNode c0 gid of
                Nothing -> pure False
                Just gen -> pure (nid `elem` map canonical (gnSchemes gen))
        _ -> pure False

getBindingPermission :: (NodeId -> NodeId) -> Constraint p -> NodeId -> PresolutionM p (Bool, Maybe GenNodeId)
getBindingPermission canonical _c0 nid = do
    (_c, _canonical, qbp) <- cachedBindingModelM
    let refC = case typeRef nid of
            TypeRef n -> TypeRef (canonical n)
            r -> r
    case IntMap.lookup (nodeRefKey refC) (Binding.qbpBindParents qbp) of
        Just (GenRef gid, BindFlex) -> pure (True, Just gid)
        Just (GenRef gid, BindRigid) -> pure (False, Just gid)
        _ -> pure (False, Nothing)

solveNonExpInstantiation :: NodeId -> NodeId -> PresolutionM p ()
solveNonExpInstantiation lhs rhs = do
    lhsNode <- getCanonicalNode lhs
    rhsNode <- getCanonicalNode rhs
    case (lhsNode, rhsNode) of
        (TyVar{}, TyVar{}) -> unifyStructure lhs rhs
        (_, TyVar{ tnBound = Nothing }) ->
            solveUnboundVarInstantiation lhs rhs
        (_, TyVar{ tnBound = Just bnd }) ->
            solveBoundVarInstantiation lhs rhs bnd
        _ -> unifyStructure lhs rhs

solveUnboundVarInstantiation :: NodeId -> NodeId -> PresolutionM p ()
solveUnboundVarInstantiation lhs rhs = do
    (c0, canonical) <- getConstraintAndCanonical
    let lhsC = canonical lhs
        rhsC = canonical rhs
    (allowBound, _parentGen) <- getBindingPermission canonical c0 rhsC
    isSchemeRoot <- isSchemeRootNode canonical c0 rhsC
    occurs <- checkOccurs canonical c0 rhsC lhsC
    if (allowBound || isSchemeRoot) && not occurs
        then setVarBound rhsC (Just lhsC)
        else unifyStructure lhs rhs

solveBoundVarInstantiation :: NodeId -> NodeId -> NodeId -> PresolutionM p ()
solveBoundVarInstantiation lhs rhs bnd = do
    (c0, canonical) <- getConstraintAndCanonical
    let lhsC = canonical lhs
        bndC = canonical bnd
    bndNode <- getCanonicalNode bndC
    case bndNode of
        TyVar{} -> do
            occurs <- checkOccurs canonical c0 bndC lhsC
            if not occurs && bndC /= lhsC
                then setVarBound bndC (Just lhsC)
                else unifyStructure lhs rhs
        _ -> unifyStructure lhs rhs

checkOccurs :: (NodeId -> NodeId) -> Constraint p -> NodeId -> NodeId -> PresolutionM p Bool
checkOccurs canonical c0 rhsC lhsC =
    case Traversal.occursInUnder canonical (NodeAccess.lookupNode c0) rhsC lhsC of
        Left _ -> pure True
        Right ok -> pure ok

debugBindParents :: String -> PresolutionM p ()
debugBindParents msg = do
    cfg <- ask
    traceBindingM cfg msg
