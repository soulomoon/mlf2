{-# LANGUAGE GADTs #-}

module MLF.Reify.Type
    ( reifyType
    , reifyTypeWithRefsNoFallback
    , reifyTypeWithRefsNoFallbackOnConstraint
    , reifyTypeWithExternalRefsNoFallback
    , reifyTypeWithExternalRefsNoFallbackOnConstraint
    , reifyTypeWithOuterBinderRefsNoFallback
    , reifyTypeWithOuterBinderRefsNoFallbackOnConstraint
    , reifyTypeWithNamedSetRefs
    , reifyTypeWithNamedSetRefsNoFallback
    , reifyTypeWithRefsNoFallbackReadModel
    , reifyTypeWithNamedSetRefsNoFallbackReadModel
    , reifyWithRefs
    , reifyWithAsRefs
    , ReifyRoot (..)
    , freeVars
    ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import MLF.Binding.Tree (lookupBindParent)
import qualified MLF.Constraint.Finalize as Finalize
import MLF.Constraint.Presolution.View (PresolutionView (..))
import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph
import MLF.Elab.ReadModel (ElabReadModel, ermPresolutionView)
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Reify.Type.Core (ReifyRoot (..), reifyWithAsRefs, reifyWithExternalRefs, reifyWithOuterBinderRefs, reifyWithReadModelRefs, reifyWithRefs)
import MLF.Types.Elab
import MLF.Util.ElabError (ElabError (..))

-- | Reify a solved NodeId into an elaborated type.
-- This version doesn't compute instance bounds (all foralls are unbounded).
reifyType :: PresolutionView p -> NodeId -> Either ElabError ElabType
reifyType presolutionView =
  reifyWithRefs "reifyType" presolutionView refFor (const False) RootType
  where
    refFor node@(NodeId i) =
      typeBinderRefFromIdentity (typeBinderIdentityFromNode node) ("t" ++ show i)

reifyTypeWithRefsNoFallback :: PresolutionView p -> IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError ElabType
reifyTypeWithRefsNoFallback presolutionView subst nid =
  reifyWithRefs "reifyTypeWithNamesNoFallback" presolutionView refForVar isNamed RootTypeNoFallback nid
  where
    canonical = pvCanonical presolutionView

    refForVar =
      refForSubstRefs canonical subst

    isNamed nodeId =
      let key = getNodeId (canonical nodeId)
       in IntMap.member key subst

reifyTypeWithRefsNoFallbackReadModel ::
  ElabReadModel p ->
  IntMap.IntMap TypeBinderRef ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithRefsNoFallbackReadModel readModel subst nid =
  reifyWithReadModelRefs "reifyTypeWithNamesNoFallback" readModel refForVar isNamed RootTypeNoFallback nid
  where
    presolutionView = ermPresolutionView readModel
    canonical = pvCanonical presolutionView

    refForVar =
      refForSubstRefs canonical subst

    isNamed nodeId =
      let key = getNodeId (canonical nodeId)
       in IntMap.member key subst

reifyTypeWithRefsNoFallbackOnConstraint :: Constraint p -> IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError ElabType
reifyTypeWithRefsNoFallbackOnConstraint constraint subst nid =
  let presolutionView = Finalize.presolutionViewFromSnapshot constraint IntMap.empty
   in reifyTypeWithRefsNoFallback presolutionView subst nid

reifyTypeWithExternalRefsNoFallback ::
  PresolutionView p ->
  IntMap.IntMap TypeBinderRef ->
  IntSet.IntSet ->
  IntMap.IntMap [NodeId] ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithExternalRefsNoFallback presolutionView subst externalKeys structuralBinders nid =
  reifyWithExternalRefs
    "reifyTypeWithExternalRefsNoFallback"
    presolutionView
    (refForSubstRefs canonical subst)
    (\nodeId -> IntMap.member (getNodeId (canonical nodeId)) subst)
    (\nodeId -> IntSet.member (getNodeId (canonical nodeId)) externalKeys)
    structuralBinders
    RootTypeNoFallback
    nid
  where
    canonical = pvCanonical presolutionView

reifyTypeWithExternalRefsNoFallbackOnConstraint ::
  Constraint p ->
  IntMap.IntMap TypeBinderRef ->
  IntSet.IntSet ->
  IntMap.IntMap [NodeId] ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithExternalRefsNoFallbackOnConstraint constraint subst externalKeys structuralBinders nid =
  let presolutionView = Finalize.presolutionViewFromSnapshot constraint IntMap.empty
   in reifyTypeWithExternalRefsNoFallback presolutionView subst externalKeys structuralBinders nid

-- | Reify the body of a scheme while treating the supplied binder identities
-- as declarations already owned by that enclosing scheme.  This differs from
-- inherited external references: structural source binders still bind
-- locally unless their exact identity is present in @outerBinderRefs@.
reifyTypeWithOuterBinderRefsNoFallback ::
  PresolutionView p ->
  IntMap.IntMap TypeBinderRef ->
  IntSet.IntSet ->
  [TypeBinderRef] ->
  IntMap.IntMap [NodeId] ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithOuterBinderRefsNoFallback presolutionView subst externalKeys outerBinderRefs structuralBinders nid =
  reifyWithOuterBinderRefs
    "reifyTypeWithOuterBinderRefsNoFallback"
    presolutionView
    refForVar
    isNamed
    isExternal
    isOuterOwned
    structuralBinders
    RootTypeNoFallback
    nid
  where
    canonical = pvCanonical presolutionView
    refForVar = refForSubstRefs canonical subst
    isNamed nodeId = IntMap.member (getNodeId (canonical nodeId)) subst
    isExternal nodeId = IntSet.member (getNodeId (canonical nodeId)) externalKeys
    isOuterOwned nodeId =
      any
        (typeBinderRefsSameIdentity (refForVar nodeId))
        outerBinderRefs

reifyTypeWithOuterBinderRefsNoFallbackOnConstraint ::
  Constraint p ->
  IntMap.IntMap TypeBinderRef ->
  IntSet.IntSet ->
  [TypeBinderRef] ->
  IntMap.IntMap [NodeId] ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithOuterBinderRefsNoFallbackOnConstraint constraint subst externalKeys outerBinderRefs structuralBinders nid =
  let presolutionView = Finalize.presolutionViewFromSnapshot constraint IntMap.empty
   in reifyTypeWithOuterBinderRefsNoFallback presolutionView subst externalKeys outerBinderRefs structuralBinders nid

reifyTypeWithNamedSetRefs :: PresolutionView p -> IntMap.IntMap TypeBinderRef -> IntSet.IntSet -> NodeId -> Either ElabError ElabType
reifyTypeWithNamedSetRefs presolutionView subst namedSet =
  reifyWithRefs "reifyTypeWithNamedSetRefs" presolutionView refForVar isNamed RootType
  where
    canonical = pvCanonical presolutionView

    refForVar =
      refForSubstRefs canonical subst

    isNamed nodeId = IntSet.member (getNodeId (canonical nodeId)) namedSet

reifyTypeWithNamedSetRefsNoFallback ::
  PresolutionView p ->
  IntMap.IntMap TypeBinderRef ->
  IntSet.IntSet ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithNamedSetRefsNoFallback presolutionView subst namedSet nid =
  reifyWithRefs "reifyTypeWithNamedSetNoFallback" presolutionView refForVar isNamed RootTypeNoFallback nid
  where
    canonical = pvCanonical presolutionView

    refForVar =
      refForSubstRefs canonical subst

    isNamed nodeId = IntSet.member (getNodeId (canonical nodeId)) namedSet

reifyTypeWithNamedSetRefsNoFallbackReadModel ::
  ElabReadModel p ->
  IntMap.IntMap TypeBinderRef ->
  IntSet.IntSet ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithNamedSetRefsNoFallbackReadModel readModel subst namedSet nid =
  reifyWithReadModelRefs "reifyTypeWithNamedSetNoFallback" readModel refForVar isNamed RootTypeNoFallback nid
  where
    presolutionView = ermPresolutionView readModel
    canonical = pvCanonical presolutionView

    refForVar =
      refForSubstRefs canonical subst

    isNamed nodeId = IntSet.member (getNodeId (canonical nodeId)) namedSet

refForSubstRefs :: (NodeId -> NodeId) -> IntMap.IntMap TypeBinderRef -> NodeId -> TypeBinderRef
refForSubstRefs canonical subst v =
  let cv@(NodeId i) = canonical v
   in fromMaybe
        (typeBinderRefFromIdentity (typeBinderIdentityFromNode cv) ("t" ++ show i))
        (IntMap.lookup i subst)

-- | Collect free variables by NodeId, skipping vars under TyForall.
freeVars :: Solved -> NodeId -> IntSet.IntSet -> IntSet.IntSet
freeVars solved nid visited
  | IntSet.member key visited = IntSet.empty
  | otherwise =
      let visited' = IntSet.insert key visited
       in case lookupNodeIn nodes (canonical nid) of
            Nothing -> IntSet.empty
            Just TyVar {} ->
              case VarStore.lookupVarBound constraint (canonical nid) of
                Nothing -> IntSet.empty
                Just bnd -> freeVars solved (canonical bnd) visited'
            Just TyBase {} -> IntSet.empty
            Just TyBottom {} -> IntSet.empty
            Just TyArrow {tnDom = d, tnCod = c} ->
              freeVarsChild visited' d
                `IntSet.union` freeVarsChild visited' c
            Just TyCon {tnArgs = args} ->
              IntSet.unions (map (freeVarsChild visited') (NE.toList args))
            Just TyVarApp {tnVarHead = headNode, tnArgs = args} ->
              IntSet.unions (map (freeVarsChild visited') (headNode : NE.toList args))
            Just TyForall {tnBody = b} ->
              freeVarsChild visited' b
            Just TyMu {tnBody = b} ->
              freeVarsChild visited' b
            Just TyExp {tnBody = b} ->
              freeVars solved (canonical b) visited'
  where
    constraint = Solved.originalConstraint solved
    nodes = cNodes constraint
    canonical = Solved.canonical solved
    key = getNodeId (canonical nid)

    freeVarsChild visited' child =
      case lookupBindParent constraint (typeRef (canonical child)) of
        Just (_, BindRigid) -> freeVars solved (canonical child) visited'
        _ -> IntSet.singleton (getNodeId (canonical child))
