-- |
-- Module      : MLF.Constraint.Presolution.Plan.Env
-- Description : Environment construction for generalization planning
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
module MLF.Constraint.Presolution.Plan.Env
  ( PresolutionEnv (..),
    mkGeneralizeEnv,
    softenBindParents,
    lookupNodeInMap,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Constraint.Finalize (stepSanitizeSnapshotUf)
import MLF.Constraint.Presolution.Plan.Context
  ( GaBindParents (..),
    GeneralizeEnv (..),
  )
import MLF.Constraint.Presolution.View (PresolutionView (..))
import MLF.Constraint.Types hiding (lookupNode)
import MLF.Util.Trace (TraceConfig, tcGeneralize)

lookupNodeInMap :: IntMap.IntMap TyNode -> NodeId -> Maybe TyNode
lookupNodeInMap nodes nid = IntMap.lookup (getNodeId nid) nodes

data PresolutionEnv = PresolutionEnv
  { peConstraint :: Constraint,
    pePresolutionView :: PresolutionView,
    peCanonical :: NodeId -> NodeId,
    peBindParents :: BindParents,
    peBindParentsGa :: Maybe GaBindParents,
    peScopeRoot :: NodeRef,
    peTargetNode :: NodeId,
    peTraceConfig :: TraceConfig
  }

mkGeneralizeEnv ::
  TraceConfig ->
  Maybe GaBindParents ->
  PresolutionView ->
  Either ElabError GeneralizeEnv
mkGeneralizeEnv traceCfg mbBindParentsGa presolutionView =
  let constraint = pvCanonicalConstraint presolutionView
      canonicalMap = stepSanitizeSnapshotUf constraint (pvCanonicalMap presolutionView)
      nodes =
        IntMap.fromList
          [ (getNodeId nid, node)
          | (nid, node) <- toListNode (cNodes constraint)
          ]
      canonical = pvCanonical presolutionView
      canonKey nid = getNodeId (canonical nid)
      lookupNode key = lookupNodeInMap nodes (NodeId key)
      isTyVarNode node = case node of
        TyVar {} -> True
        _ -> False
      isTyForallNode node = case node of
        TyForall {} -> True
        _ -> False
      isBaseLikeNode node = case node of
        TyBase {} -> True
        TyBottom {} -> True
        _ -> False
      isTyVarKey key = maybe False isTyVarNode (lookupNode key)
      isTyForallKey key = maybe False isTyForallNode (lookupNode key)
      isBaseLikeKey key = maybe False isBaseLikeNode (lookupNode key)
   in pure
        GeneralizeEnv
          { geConstraint = constraint,
            geOriginalConstraint = pvConstraint presolutionView,
            geNodes = nodes,
            geCanonical = canonical,
            geCanonKey = canonKey,
            geLookupNode = lookupNode,
            geIsTyVarKey = isTyVarKey,
            geIsTyForallKey = isTyForallKey,
            geIsBaseLikeKey = isBaseLikeKey,
            geBindParentsGa = mbBindParentsGa,
            geCanonicalMap = canonicalMap,
            geDebugEnabled = tcGeneralize traceCfg
          }

softenBindParents :: (NodeId -> NodeId) -> Constraint -> BindParents -> BindParents
softenBindParents canonical constraint =
  let weakened = cWeakenedVars constraint
      softenOne childKey (parent, flag) =
        case (flag, nodeRefFromKey childKey) of
          (BindRigid, TypeRef childN)
            | IntSet.member (getNodeId (canonical childN)) weakened ->
                (parent, BindFlex)
          _ -> (parent, flag)
   in IntMap.mapWithKey softenOne
