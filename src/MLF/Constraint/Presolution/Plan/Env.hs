-- |
-- Module      : MLF.Constraint.Presolution.Plan.Env
-- Description : Environment construction for generalization planning
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
module MLF.Constraint.Presolution.Plan.Env
  ( PresolutionEnv (..),
    mkGeneralizeEnv,
    lookupNodeInMap,
  )
where

{- Note [Generalization planning environment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'PresolutionEnv' bundles the inputs needed by the generalization planner
('planGeneralizeAt') and the reification planner ('planReify'):

  * The original and canonical constraints
  * Presolution view (node maps, canonical function, bind parents)
  * Trace config for conditional debug output
  * The node-lookup helper 'lookupNodeInMap'

'mkGeneralizeEnv' constructs a 'GeneralizeEnv p' for a single generalization
scope by sanitizing the canonical map and projecting scope-local binding
structure.  Reification-owned softening is applied later by
"MLF.Reify.Named", so this environment preserves the canonical binding flags.
-}

import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Presolution.Plan.Context
  ( GaBindParents (..),
    GeneralizationRequirements,
    GeneralizeEnv (..),
  )
import MLF.Constraint.Presolution.View (PresolutionView (..), sanitizedViewCanonicalMap)
import MLF.Constraint.Types.Graph hiding (lookupNode)
import MLF.Util.ElabError (ElabError)
import MLF.Util.Trace (TraceConfig, tcGeneralize)

lookupNodeInMap :: IntMap.IntMap TyNode -> NodeId -> Maybe TyNode
lookupNodeInMap nodes nid = IntMap.lookup (getNodeId nid) nodes

data PresolutionEnv p = PresolutionEnv
  { peConstraint :: Constraint p,
    pePresolutionView :: PresolutionView p,
    peCanonical :: NodeId -> NodeId,
    peBindParents :: BindParents,
    peBindParentsGa :: Maybe (GaBindParents p),
    peRequirements :: GeneralizationRequirements,
    peScopeRoot :: NodeRef,
    peTargetNode :: NodeId,
    peTraceConfig :: TraceConfig
  }

mkGeneralizeEnv ::
  TraceConfig ->
  Maybe (GaBindParents p) ->
  PresolutionView p ->
  Either ElabError (GeneralizeEnv p)
mkGeneralizeEnv traceCfg mbBindParentsGa presolutionView =
  let constraint = pvCanonicalConstraint presolutionView
      canonicalMap = sanitizedViewCanonicalMap presolutionView
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
        GeneralizeEnv { geConstraint = constraint,
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
