-- |
-- Module      : MLF.Constraint.Presolution.Plan.BinderPlan.Alias
-- Description : Alias binder helpers
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
module MLF.Constraint.Presolution.Plan.BinderPlan.Alias
  ( AliasEnv (..),
    boundMentionsSelfAliasFor,
    computeAliasBinders,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Types
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Util.ElabError (ElabError)
import qualified MLF.Util.IntMapUtils as IntMapUtils

-- | Shared environment for alias-binder analysis functions.
data AliasEnv = AliasEnv
  { -- | Chase function mapping nodes to canonical representatives
    aeCanonical :: NodeId -> NodeId,
    -- | Full constraint graph
    aeConstraint :: Constraint,
    -- | Node-to-type map from the constraint
    aeNodes :: NodeMap TyNode,
    -- | Binding tree (child -> parent+flag map)
    aeBindParents :: BindParents,
    -- | Alias depth map (gamma alias)
    aeDepthMap :: IntMap.IntMap Int,
    -- | Root nodes of scope schemes
    aeScopeSchemeRoots :: IntSet.IntSet,
    -- | Reachable nodes from a given node (with bounds)
    aeNodeChildren :: NodeId -> IntSet.IntSet
  }

-- | Check if a bound mentions a self-alias.
boundMentionsSelfAliasFor ::
  AliasEnv ->
  NodeId ->
  Bool
boundMentionsSelfAliasFor env v =
  let canonical = aeCanonical env
      constraint = aeConstraint env
      nodes = aeNodes env
      gammaAlias = aeDepthMap env
      nestedSchemeInteriorSet = aeScopeSchemeRoots env
      reachableFromWithBounds = aeNodeChildren env
   in case VarStore.lookupVarBound constraint (canonical v) of
        Just bnd -> any mentionsSelf (IntSet.toList (reachableFromWithBounds bnd))
          where
            binderKey = getNodeId (canonical v)
            mentionsSelf nidInt
              | IntSet.member keyC nestedSchemeInteriorSet = False
              | Just TyVar {} <- lookupNodeIn nodes nidC,
                Just repKey <- IntMap.lookup keyC gammaAlias =
                  repKey == binderKey
              | otherwise = False
              where
                nidC = canonical (NodeId nidInt)
                keyC = getNodeId nidC
        Nothing -> False

computeAliasBinders ::
  AliasEnv ->
  (NodeId -> Int) ->
  NodeRef ->
  (String -> Either ElabError ()) ->
  Either ElabError (IntSet.IntSet, [NodeId])
computeAliasBinders env canonKey scopeRootC traceM =
  let canonical = aeCanonical env
      constraint = aeConstraint env
      nodes = aeNodes env
      bindParents = aeBindParents env
      scopeSchemeRoots = aeScopeSchemeRoots env
      scopeHasStructuralScheme =
        case scopeRootC of
          GenRef gid ->
            case NodeAccess.lookupGenNode constraint gid of
              Just gen ->
                any
                  ( \root ->
                      not (IntSet.member (canonKey root) scopeSchemeRoots)
                  )
                  (gnSchemes gen)
              Nothing -> False
          _ -> False
      aliasBinderInfo =
        let baseBoundTargets =
              IntSet.fromList
                [ getNodeId bndC
                | (vid, node) <- toListNode nodes,
                  TyVar {} <- [node],
                  Just bnd <- [VarStore.lookupVarBound constraint vid],
                  let bndC = canonical bnd,
                  case lookupNodeIn nodes bndC of
                    Just TyBase {} -> True
                    Just TyBottom {} -> True
                    _ -> False
                ]
         in case scopeRootC of
              GenRef gid
                | scopeHasStructuralScheme ->
                    let bases =
                          IntSet.fromList
                            [ key
                            | child <- IntMapUtils.typeChildrenOfGen bindParents gid,
                              let key = getNodeId child,
                              IntSet.member key baseBoundTargets,
                              case lookupNodeIn nodes (NodeId key) of
                                Just TyBase {} -> True
                                Just TyBottom {} -> True
                                _ -> False
                            ]
                     in (bases, [NodeId key | key <- IntSet.toList bases])
              _ -> (IntSet.empty, [])
   in traceM
        ( "generalizeAt: aliasBinderBases="
            ++ show (IntSet.toList (fst aliasBinderInfo))
            ++ " scopeHasStructuralScheme="
            ++ show scopeHasStructuralScheme
        )
        *> pure aliasBinderInfo
