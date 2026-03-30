{-# LANGUAGE GADTs #-}

module MLF.Reify.Type
    ( reifyType
    , reifyTypeWithNames
    , reifyTypeWithNamesNoFallback
    , reifyTypeWithNamesNoFallbackOnConstraint
    , reifyTypeWithNamedSet
    , reifyTypeWithNamedSetNoFallback
    , reifyWith
    , reifyWithAs
    , ReifyRoot (..)
    , solvedFromView
    , freeVars
    ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import MLF.Binding.Tree (lookupBindParent)
import MLF.Constraint.Finalize (presolutionViewFromSnapshot)
import MLF.Constraint.Presolution.View (PresolutionView (..))
import MLF.Constraint.Solved (Solved)
import qualified MLF.Constraint.Solved as Solved
import qualified MLF.Constraint.Solved.Internal as SolvedInternal
import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Reify.Named (namedNodes)
import MLF.Reify.Type.Core (ReifyRoot (..), reifyWith, reifyWithAs)
import MLF.Types.Elab
import MLF.Util.ElabError (ElabError (..))

-- | Reify a solved NodeId into an elaborated type.
-- This version doesn't compute instance bounds (all foralls are unbounded).
reifyType :: PresolutionView -> NodeId -> Either ElabError ElabType
reifyType presolutionView =
  let solved = solvedFromView presolutionView
   in reifyWith "reifyType" solved nameFor (const False) RootType
  where
    nameFor (NodeId i) = "t" ++ show i

-- | Reify with an explicit name substitution for vars (Schi': named nodes become variables).
reifyTypeWithNames :: PresolutionView -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNames presolutionView subst nid = do
  namedSet <- namedNodes presolutionView
  reifyTypeWithNamedSet presolutionView subst namedSet nid

-- | Reify with an explicit name substitution, but without ancestor fallback
-- quantifiers (used when an outer scheme already quantifies binders).
-- See Note [No-fallback reify preserves explicit bounds] in
-- docs/notes/2026-01-27-elab-changes.md.
reifyTypeWithNamesNoFallback :: PresolutionView -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNamesNoFallback presolutionView subst nid =
  reifyTypeWithNamesNoFallbackSolved (solvedFromView presolutionView) subst nid

reifyTypeWithNamesNoFallbackSolved ::
  Solved ->
  IntMap.IntMap String ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithNamesNoFallbackSolved solved subst nid =
  let canonical = Solved.canonical solved
      nameFor (NodeId i) = "t" ++ show i

      varNameFor v =
        let cv = canonical v
         in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

      isNamed nodeId =
        let key = getNodeId (canonical nodeId)
         in IntMap.member key subst
   in reifyWith "reifyTypeWithNamesNoFallback" solved varNameFor isNamed RootTypeNoFallback nid

-- | Reify with an explicit constraint (Schi' on base graphs).
reifyTypeWithNamesNoFallbackOnConstraint :: Constraint -> IntMap.IntMap String -> NodeId -> Either ElabError ElabType
reifyTypeWithNamesNoFallbackOnConstraint constraint subst nid =
  let presolutionView = presolutionViewFromSnapshot constraint IntMap.empty
   in reifyTypeWithNamesNoFallback presolutionView subst nid

-- | Reify with an explicit named-node set (Schi').
reifyTypeWithNamedSet :: PresolutionView -> IntMap.IntMap String -> IntSet.IntSet -> NodeId -> Either ElabError ElabType
reifyTypeWithNamedSet presolutionView subst namedSet =
  reifyTypeWithNamedSetSolved (solvedFromView presolutionView) subst namedSet

reifyTypeWithNamedSetSolved ::
  Solved ->
  IntMap.IntMap String ->
  IntSet.IntSet ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithNamedSetSolved solved subst namedSet =
  reifyWith "reifyTypeWithNames" solved varNameFor isNamed RootType
  where
    canonical = Solved.canonical solved

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
      let cv = canonical v
       in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntSet.member (getNodeId (canonical nodeId)) namedSet

-- | Reify with an explicit named-node set, without ancestor fallback quantifiers.
reifyTypeWithNamedSetNoFallbackSolved ::
  Solved ->
  IntMap.IntMap String ->
  IntSet.IntSet ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithNamedSetNoFallbackSolved solved subst namedSet =
  reifyWith "reifyTypeWithNamedSetNoFallback" solved varNameFor isNamed RootTypeNoFallback
  where
    canonical = Solved.canonical solved

    nameFor (NodeId i) = "t" ++ show i

    varNameFor :: NodeId -> String
    varNameFor v =
      let cv = canonical v
       in fromMaybe (nameFor cv) (IntMap.lookup (getNodeId cv) subst)

    isNamed nodeId = IntSet.member (getNodeId (canonical nodeId)) namedSet

reifyTypeWithNamedSetNoFallback ::
  PresolutionView ->
  IntMap.IntMap String ->
  IntSet.IntSet ->
  NodeId ->
  Either ElabError ElabType
reifyTypeWithNamedSetNoFallback presolutionView subst namedSet nid =
  reifyTypeWithNamedSetNoFallbackSolved (solvedFromView presolutionView) subst namedSet nid

solvedFromView :: PresolutionView -> Solved
solvedFromView presolutionView =
  let solved0 =
        SolvedInternal.fromConstraintAndUf
          (pvConstraint presolutionView)
          (pvCanonicalMap presolutionView)
   in SolvedInternal.rebuildWithConstraint solved0 (pvCanonicalConstraint presolutionView)

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
