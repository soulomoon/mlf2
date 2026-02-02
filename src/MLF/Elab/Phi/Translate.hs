{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{- |
Module      : MLF.Elab.Phi.Translate
Description : Translate graph witnesses to xMLF instantiations
Copyright   : (c) 2024
License     : BSD-3-Clause

This module translates recorded per-edge graph witnesses to xMLF instantiation
witnesses (φ). It interprets witness operations (Graft, Weaken, Raise, Merge,
RaiseMerge) and produces explicit instantiation terms.

= Architecture

The translation has two main phases:
1. Context computation - compute instantiation-context paths (see "MLF.Elab.Phi.Context")
2. Witness interpretation - interpret witness operations to build instantiation terms

= Paper References

* Rémy & Yakobowski, "Graphic Type Constraints" (ICFP 2008) - Figure 10
* Thesis §15.3 - Witness translation

= Note on Module Structure

This module contains a large function 'phiFromEdgeWitnessWithTrace' with many
local helper functions that share a complex closure. The Omega/Step interpretation
helpers live in "MLF.Elab.Phi.Omega"; the "MLF.Elab.Phi" module re-exports
the public entry points as a facade.
-}
module MLF.Elab.Phi.Translate (
    phiFromEdgeWitness,
    phiFromEdgeWitnessWithTrace,
    canonicalNodeM,
    remapSchemeInfoM
) where

import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)

import MLF.Constraint.Types
import MLF.Elab.Types
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Constraint.BindingUtil (bindingPathToRootLocal)
import MLF.Reify.Core (namedNodes, reifyType)
import MLF.Constraint.Solve hiding (BindingTreeError, MissingNode)
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Constraint.Presolution (EdgeTrace(..))
import MLF.Constraint.Presolution.Base (CopyMapping(..), InteriorNodes(..), copiedNodes)
import qualified MLF.Binding.Tree as Binding
import MLF.Binding.Tree (checkBindingTree, checkNoGenFallback, checkSchemeClosureUnder)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Util.IntMapUtils as IntMapUtils
import MLF.Elab.Phi.Env (PhiM, askCanonical, askResult)
import MLF.Elab.Phi.Omega (OmegaContext(..), phiWithSchemeOmega)
import MLF.Util.Trace (TraceConfig, traceGeneralize)

-- | Canonicalize a node id using the union-find from the solve result.
canonicalNodeM :: NodeId -> PhiM NodeId
canonicalNodeM nid = do
    res <- askResult
    pure $ Solve.frWith (srUnionFind res) nid

-- | Remap scheme info using the copy mapping from an edge trace.
remapSchemeInfoM :: EdgeTrace -> SchemeInfo -> PhiM SchemeInfo
remapSchemeInfoM tr si = do
    canonical <- askCanonical
    let traceCopyMap = getCopyMapping (etCopyMap tr)
        subst' =
            IntMap.fromList
                [ (getNodeId (canonical mapped), name)
                | (k, name) <- IntMap.toList (siSubst si)
                , let nid = NodeId k
                      mapped = IntMap.findWithDefault nid k traceCopyMap
                ]
    pure $ si { siSubst = subst' }

-- | Translate a recorded per-edge graph witness to an xMLF instantiation.
type GeneralizeAtWith =
    Maybe GaBindParents
    -> SolveResult
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap String)

phiFromEdgeWitness
    :: TraceConfig
    -> GeneralizeAtWith
    -> SolveResult
    -> Maybe SchemeInfo
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitness traceCfg generalizeAtWith res mSchemeInfo ew =
    phiFromEdgeWitnessWithTrace traceCfg generalizeAtWith res Nothing mSchemeInfo Nothing ew

phiFromEdgeWitnessWithTrace
    :: TraceConfig
    -> GeneralizeAtWith
    -> SolveResult
    -> Maybe GaBindParents
    -> Maybe SchemeInfo
    -> Maybe EdgeTrace
    -> EdgeWitness
    -> Either ElabError Instantiation
phiFromEdgeWitnessWithTrace traceCfg generalizeAtWith res mbGaParents mSchemeInfo mTrace ew = do
    requireValidBindingTree
    namedSet0 <- namedNodes res
    case debugPhi
        ("phi ewLeft=" ++ show (ewLeft ew)
            ++ " ewRight=" ++ show (ewRight ew)
        )
        () of
        () -> pure ()
    case debugPhi
        ("phi ewRootType=" ++ show (reifyType res (ewRoot ew))
            ++ " ewLeftType=" ++ show (reifyType res (ewLeft ew))
            ++ " ewRightType=" ++ show (reifyType res (ewRight ew))
        )
        () of
        () -> pure ()
    let copied =
            case mTrace of
                Nothing -> IntSet.empty
                Just tr ->
                    IntSet.fromList
                        [ getNodeId (canonicalNode nid)
                        | nid <- copiedNodes (etCopyMap tr)
                        ]
        interior =
            case mTrace of
                Nothing -> IntSet.empty
                Just tr ->
                    case etInterior tr of
                        InteriorNodes s -> s
        namedSet1 = IntSet.difference namedSet0 copied
        rootKey = getNodeId (canonicalNode (ewRoot ew))
        namedSet =
            let base =
                    if IntSet.null interior
                        then namedSet1
                        else IntSet.intersection namedSet1 interior
            in IntSet.delete rootKey base
    let InstanceWitness ops = ewWitness ew
        remapNode nid =
            let nidC = canonicalNode nid
            in if isTyVarNode nidC
                then IntMap.findWithDefault nidC (getNodeId nidC) copyMap
                else nidC
        remapOp op = case op of
            OpGraft arg bv -> OpGraft (remapNode arg) (remapNode bv)
            OpWeaken bv -> OpWeaken (remapNode bv)
            OpRaise n -> OpRaise (remapNode n)
            OpMerge n m -> OpMerge (remapNode n) (remapNode m)
            OpRaiseMerge n m -> OpRaiseMerge (remapNode n) (remapNode m)
        remapStep step = case step of
            StepOmega op -> StepOmega (remapOp op)
            StepIntro -> StepIntro
        steps0Raw =
            case ewSteps ew of
                [] -> map StepOmega (map remapOp ops)
                xs -> map remapStep xs
        steps0 =
            debugPhi
                ("phi steps edge=" ++ show (ewEdgeId ew)
                    ++ " root=" ++ show (ewRoot ew)
                    ++ " right=" ++ show (ewRight ew)
                    ++ " steps=" ++ show steps0Raw
                )
                steps0Raw
    let mSchemeInfo' =
            case (mSchemeInfo, mTrace) of
                (Just si, Just tr) -> Just (remapSchemeInfo tr si)
                _ -> mSchemeInfo
    targetBinderKeysRaw <-
        case mTrace of
            Nothing -> pure IntSet.empty
            Just _ ->
                let targetRootC = canonicalNode (ewRight ew)
                in if nodeRefExistsLocal (srConstraint res) (typeRef targetRootC)
                    then do
                        tgtScope <- instScopeRoot targetRootC
                        case tgtScope of
                            GenRef gid -> do
                                bindParents <-
                                    bindingToElab $
                                        Binding.canonicalizeBindParentsUnder canonicalNode (srConstraint res)
                                let binders =
                                        [ childN
                                        | childN <- IntMapUtils.typeChildrenOfGen bindParents gid
                                        , case NodeAccess.lookupNode (srConstraint res) childN of
                                            Just TyVar{} -> True
                                            _ -> False
                                        ]
                                debugPhi
                                    ("phi targetScope=" ++ show tgtScope
                                        ++ " targetBinders=" ++ show binders
                                    )
                                    (pure ())
                                pure (IntSet.fromList (map (getNodeId . canonicalNode) binders))
                            TypeRef _ -> do
                                debugPhi ("phi targetScope=" ++ show tgtScope) (pure ())
                                pure IntSet.empty
                    else pure IntSet.empty
    let targetBinderKeys =
            debugPhi
                ("phi targetBinderKeys=" ++ show (IntSet.toList targetBinderKeysRaw))
                targetBinderKeysRaw
    case mSchemeInfo' of
        Nothing -> do
            let schemeRootNode = ewRoot ew
            si0 <- schemeInfoForRoot schemeRootNode
            let si1 =
                    case mTrace of
                        Just tr -> remapSchemeInfo tr si0
                        Nothing -> si0
            phiWithSchemeOmega omegaCtx namedSet targetBinderKeys si1 steps0
        Just si -> do
            phiWithSchemeOmega omegaCtx namedSet targetBinderKeys si steps0
  where
    debugPhi :: String -> a -> a
    debugPhi = traceGeneralize traceCfg

    omegaCtx :: OmegaContext
    omegaCtx =
        OmegaContext
            { ocTraceConfig = traceCfg
            , ocResult = res
            , ocCanonicalNode = canonicalNode
            , ocCopyMap = copyMap
            , ocGaParents = mbGaParents
            , ocTrace = mTrace
            , ocSchemeInfo = mSchemeInfo
            , ocEdgeRoot = ewRoot ew
            , ocEdgeLeft = ewLeft ew
            , ocEdgeRight = ewRight ew
            }

    requireValidBindingTree :: Either ElabError ()
    requireValidBindingTree =
        let (constraintCheck, schemeConstraint, schemeCanonical) =
                (srConstraint res, srConstraint res, canonicalNode)
        in case checkBindingTree constraintCheck of
            Left err -> Left (BindingTreeError err)
            Right () ->
                case checkNoGenFallback constraintCheck of
                    Left err -> Left (BindingTreeError err)
                    Right () ->
                        case checkSchemeClosureUnder schemeCanonical schemeConstraint of
                            Left err -> Left (BindingTreeError err)
                            Right () -> Right ()

    canonicalNode :: NodeId -> NodeId
    canonicalNode = Solve.frWith (srUnionFind res)

    remapSchemeInfo :: EdgeTrace -> SchemeInfo -> SchemeInfo
    remapSchemeInfo tr si =
        let traceCopyMap = getCopyMapping (etCopyMap tr)
            subst' =
                IntMap.fromList
                    [ (getNodeId (canonicalNode mapped), name)
                    | (k, name) <- IntMap.toList (siSubst si)
                    , let nid = NodeId k
                          mapped = IntMap.findWithDefault nid k traceCopyMap
                    ]
        in si { siSubst = subst' }

    schemeInfoForRoot :: NodeId -> Either ElabError SchemeInfo
    schemeInfoForRoot root0 = do
        scopeRoot <- instScopeRoot root0
        (sch, subst) <-
            case mbGaParents of
                Nothing -> generalizeAtWith Nothing res scopeRoot root0
                Just ga -> generalizeAtWith (Just ga) res scopeRoot root0
        pure SchemeInfo { siScheme = sch, siSubst = subst }

    instScopeRoot :: NodeId -> Either ElabError NodeRef
    instScopeRoot root0 =
        case mbGaParents of
            Nothing ->
                let rootC = canonicalNode root0
                    owners =
                        [ gnId gen
                        | gen <- NodeAccess.allGenNodes (srConstraint res)
                        , any (\root -> canonicalNode root == rootC) (gnSchemes gen)
                        ]
                in case owners of
                    gid:_ -> Right (genRef gid)
                    [] -> goScope IntSet.empty (typeRef rootC)
            Just ga ->
                let rootC = canonicalNode root0
                    baseFromTrace =
                        case mTrace of
                            Nothing -> Nothing
                            Just tr ->
                                let traceCopyMap = getCopyMapping (etCopyMap tr)
                                    revMatches =
                                        [ NodeId k
                                        | (k, v) <- IntMap.toList traceCopyMap
                                        , canonicalNode v == rootC
                                        ]
                                in listToMaybe revMatches
                    baseRep =
                        IntMap.lookup (getNodeId rootC) (gaSolvedToBase ga)
                            <|> baseFromTrace
                in case baseRep of
                    Nothing -> goScope IntSet.empty (typeRef rootC)
                    Just baseN ->
                        case bindingPathToRootLocal (gaBindParentsBase ga) (typeRef baseN) of
                            Left _ -> goScope IntSet.empty (typeRef rootC)
                            Right path ->
                                case listToMaybe [gid | GenRef gid <- drop 1 path] of
                                    Just gid -> Right (genRef gid)
                                    Nothing -> goScope IntSet.empty (typeRef rootC)
      where
        goScope visited ref
            | IntSet.member (nodeRefKey ref) visited =
                Right (typeRef (canonicalNode root0))
            | otherwise = do
                mbParent <- bindingToElab (Binding.lookupBindParentUnder canonicalNode (srConstraint res) ref)
                case mbParent of
                    Nothing -> Right (typeRef (canonicalNode root0))
                    Just (GenRef gid, _) -> Right (genRef gid)
                    Just (TypeRef parent, _) ->
                        goScope (IntSet.insert (nodeRefKey ref) visited) (typeRef (canonicalNode parent))
    copyMap :: IntMap.IntMap NodeId
    copyMap =
        case mTrace of
            Nothing -> IntMap.empty
            Just tr -> getCopyMapping (etCopyMap tr)

    isTyVarNode :: NodeId -> Bool
    isTyVarNode nid =
        let key = getNodeId (canonicalNode nid)
        in case NodeAccess.lookupNode (srConstraint res) (NodeId key) of
            Just TyVar{} -> True
            _ -> False

    nodeRefExistsLocal :: Constraint -> NodeRef -> Bool
    nodeRefExistsLocal c ref =
        case ref of
            TypeRef nid ->
                case lookupNodeIn (cNodes c) nid of
                    Just _ -> True
                    Nothing -> False
            GenRef gid ->
                IntMap.member (getGenNodeId gid) (getGenNodeMap (cGenNodes c))
