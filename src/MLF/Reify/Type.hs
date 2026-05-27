{-# LANGUAGE GADTs #-}

module MLF.Reify.Type
    ( reifyType
    , reifyTypeWithNames
    , reifyTypeWithNamesNoFallback
    , reifyTypeWithNamesNoFallbackOnConstraint
    , reifyTypeWithNamedSet
    , reifyTypeWithNamedSetNoFallback
    , reifyTypeWithNamesNoFallbackReadModel
    , reifyTypeWithNamedSetNoFallbackReadModel
    , reifyWith
    , reifyWithAs
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
import MLF.Reify.Named (namedNodes)
import MLF.Reify.Type.Core (ReifyRoot (..), reifyWith, reifyWithAs, reifyWithReadModel)
import MLF.Types.Elab
import MLF.Util.ElabError (ElabError (..))

-- | Reify a solved NodeId into an elaborated type.
-- This version doesn't compute instance bounds (all foralls are unbounded).
reifyType :: PresolutionView p -> NodeId -> Either ElabError ElabType
reifyType presolutionView =
  reifyWith "reifyType" presolutionView nameFor (const False) RootType
  where
    nameFor (NodeId i) = "t" ++ show i

-- | Reify with an explicit name substitution for vars (Schi': named nodes become variables).
reifyTypeWithNames :: PresolutionView p -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNames presolutionView subst nid = do
  namedSet <- namedNodes presolutionView
  reifyTypeWithNamedSet presolutionView subst namedSet nid

-- | Reify with an explicit name substitution, but without ancestor fallback
-- quantifiers (used when an outer scheme already quantifies binders).
-- See Note [No-fallback reify preserves explicit bounds] in
-- docs/notes/2026-01-27-elab-changes.md.
reifyTypeWithNamesNoFallback :: PresolutionView p -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNamesNoFallback presolutionView subst nid =
  let canonical = pvCanonical presolutionView
      nameFor (NodeId i) = "t" ++ show i

      varNameFor v =
        let cv = canonical v
         in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

      isNamed nodeId =
        let key = getNodeId (canonical nodeId)
         in IntMap.member key subst
   in reifyWith "reifyTypeWithNamesNoFallback" presolutionView varNameFor isNamed RootTypeNoFallback nid

reifyTypeWithNamesNoFallbackReadModel ::
  ElabReadModel p ->
  IntMap.IntMap String ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithNamesNoFallbackReadModel readModel subst nid =
  reifyWithReadModel "reifyTypeWithNamesNoFallback" readModel varNameFor isNamed RootTypeNoFallback nid
  where
    presolutionView = ermPresolutionView readModel
    canonical = pvCanonical presolutionView

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
      let cv = canonical v
       in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId =
      let key = getNodeId (canonical nodeId)
       in IntMap.member key subst

-- | Reify with an explicit constraint (Schi' on base graphs).
reifyTypeWithNamesNoFallbackOnConstraint :: Constraint p -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNamesNoFallbackOnConstraint constraint subst nid =
  let presolutionView = Finalize.presolutionViewFromSnapshot constraint IntMap.empty
   in reifyTypeWithNamesNoFallback presolutionView subst nid

-- | Reify with an explicit named-node set (Schi').
reifyTypeWithNamedSet :: PresolutionView p -> IntMap.IntMap String -> IntSet.IntSet -> NodeId -> Either ElabError ElabType
reifyTypeWithNamedSet presolutionView subst namedSet =
  reifyWith "reifyTypeWithNames" presolutionView varNameFor isNamed RootType
  where
    canonical = pvCanonical presolutionView

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
      let cv = canonical v
       in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntSet.member (getNodeId (canonical nodeId)) namedSet

reifyTypeWithNamedSetNoFallback ::
  PresolutionView p ->
  IntMap.IntMap String ->
  IntSet.IntSet ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithNamedSetNoFallback presolutionView subst namedSet nid =
  reifyWith "reifyTypeWithNamedSetNoFallback" presolutionView varNameFor isNamed RootTypeNoFallback nid
  where
    canonical = pvCanonical presolutionView

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
      let cv = canonical v
       in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntSet.member (getNodeId (canonical nodeId)) namedSet

reifyTypeWithNamedSetNoFallbackReadModel ::
  ElabReadModel p ->
  IntMap.IntMap String ->
  IntSet.IntSet ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithNamedSetNoFallbackReadModel readModel subst namedSet nid =
  reifyWithReadModel "reifyTypeWithNamedSetNoFallback" readModel varNameFor isNamed RootTypeNoFallback nid
  where
    presolutionView = ermPresolutionView readModel
    canonical = pvCanonical presolutionView

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
      let cv = canonical v
       in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntSet.member (getNodeId (canonical nodeId)) namedSet

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
