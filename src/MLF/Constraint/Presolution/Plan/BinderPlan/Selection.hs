-- |
-- Module      : MLF.Constraint.Presolution.Plan.BinderPlan.Selection
-- Description : Binder selection helpers
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
module MLF.Constraint.Presolution.Plan.BinderPlan.Selection
  ( BinderSelectionEnv (..),
    SelectBindersArgs (..),
    boundFlexChildrenUnder,
    bindableChildrenUnder,
    bindingScopeGen,
    isQuantifiable,
    boundContainsForall,
    isScopeSchemeRoot,
    hasExplicitBoundFor,
    mkIsBindable,
    bindersForGen,
    bindersForType,
    selectBinders,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Constraint.BindingUtil (bindingScopeFor)
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Presolution
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Util.ElabError (ElabError)
import qualified MLF.Util.IntMapUtils as IntMapUtils

boundFlexChildrenUnder ::
  (NodeId -> NodeId) ->
  BindParents ->
  (Int -> NodeId -> Bool) ->
  NodeRef ->
  [NodeId]
boundFlexChildrenUnder canonical bindParents isBindable parentRef =
  [ canonical child
  | (childKey, child) <- IntMapUtils.flexTypeChildrenWithKeyOf bindParents parentRef,
    isBindable childKey child
  ]

bindableChildrenUnder ::
  (NodeId -> NodeId) ->
  BindParents ->
  (Int -> NodeId -> Bool) ->
  NodeRef ->
  [NodeId]
bindableChildrenUnder canonical bindParents isBindable parentRef =
  [ canonical child
  | (childKey, child) <- IntMapUtils.typeChildrenWithKeyOf bindParents parentRef,
    isBindable childKey child
  ]

bindingScopeGen :: Constraint -> NodeId -> Maybe GenNodeId
bindingScopeGen constraint child = bindingScopeFor constraint (typeRef child)

isQuantifiable :: (NodeId -> NodeId) -> Constraint -> (Int -> Bool) -> NodeId -> Bool
isQuantifiable canonical constraint isTyVarKey child =
  isTyVarKey (getNodeId child) && not (VarStore.isEliminatedVar constraint (canonical child))

boundContainsForall ::
  (NodeId -> NodeId) ->
  Constraint ->
  (NodeId -> Bool) ->
  NodeId ->
  Bool
boundContainsForall canonical constraint containsForallFrom v =
  case VarStore.lookupVarBound constraint (canonical v) of
    Just bnd -> containsForallFrom bnd
    Nothing -> False

isScopeSchemeRoot ::
  (NodeId -> Int) ->
  IntSet.IntSet ->
  NodeId ->
  Bool
isScopeSchemeRoot canonKey scopeSchemeRoots child =
  IntSet.member (canonKey child) scopeSchemeRoots

hasExplicitBoundFor ::
  (NodeId -> NodeId) ->
  NodeMap TyNode ->
  Constraint ->
  NodeId ->
  Bool
hasExplicitBoundFor canonical nodes constraint v =
  case lookupNodeIn nodes (canonical v) of
    Just TyVar {} -> VarStore.lookupVarBound constraint (canonical v) /= Nothing
    _ -> False

mkIsBindable ::
  IntMap.IntMap BindFlag ->
  (NodeId -> Bool) ->
  (Int -> NodeId -> Bool)
mkIsBindable bindFlags isQuantifiableP key child =
  case IntMap.lookup key bindFlags of
    Just BindFlex -> isQuantifiableP child
    _ -> False

-- | Shared environment for binder-selection functions.
-- Bundles the constraint-graph context that 'selectBinders', 'bindersForGen',
-- and 'bindersForType' all require.
data BinderSelectionEnv = BinderSelectionEnv
  { -- | Chase function mapping nodes to canonical representatives
    bseCanonical :: NodeId -> NodeId,
    -- | Binding tree (child -> parent+flag map)
    bseBindParents :: BindParents,
    -- | Node-to-type map from the constraint
    bseNodes :: NodeMap TyNode,
    -- | Full constraint graph
    bseConstraint :: Constraint,
    -- | Predicate: is this node bindable at given depth?
    bseIsBindable :: Int -> NodeId -> Bool
  }

-- | Per-call arguments for 'selectBinders' that vary between call sites.
data SelectBindersArgs = SelectBindersArgs
  { -- | Maps canonical node to depth key
    sbaCanonKey :: NodeId -> Int,
    -- | Root nodes of scope schemes
    sbaScopeSchemeRoots :: IntSet.IntSet,
    -- | Does this node have an explicit bound?
    sbaHasExplicitBoundP :: NodeId -> Bool,
    -- | Pool of candidate binder nodes
    sbaCandidatePool :: [NodeId],
    -- | Warning trace callback
    sbaTraceWarn :: String -> Either ElabError (),
    -- | Optional gen-node context
    sbaMGenId :: Maybe GenNodeId,
    -- | Node reference being generalized
    sbaNodeRef :: NodeRef
  }

bindersForGen ::
  BinderSelectionEnv ->
  ([(NodeId, BindFlag, Maybe TyNode, Bool)] -> String) ->
  [NodeId] ->
  (String -> Either ElabError ()) ->
  GenNodeId ->
  Either ElabError [NodeId]
bindersForGen env renderAllChildren aliasBinderNodes traceM gid = do
  let canonical = bseCanonical env
      bindParents = bseBindParents env
      nodes = bseNodes env
      constraint = bseConstraint env
      isBindable = bseIsBindable env
  let allChildren = IntMapUtils.typeChildrenWithFlagOf bindParents (GenRef gid)
  traceM
    ( "generalizeAt: scopeGen child nodes="
        ++ renderAllChildren
          [ (child, flag, lookupNodeIn nodes child, VarStore.isEliminatedVar constraint (canonical child))
          | (child, flag) <- allChildren
          ]
    )
  traceM
    ( "generalizeAt: scopeGen children="
        ++ show allChildren
    )
  let bindableByScope =
        [ canonical child
        | (child, node) <- toListNode nodes,
          TyVar {} <- [node],
          let childKey = getNodeId child,
          isBindable childKey child,
          bindingScopeFor constraint (typeRef child) == Just gid
        ]
      bindableUnder = bindableChildrenUnder canonical bindParents isBindable (GenRef gid)
  pure (bindableUnder ++ aliasBinderNodes ++ bindableByScope)

bindersForType ::
  BinderSelectionEnv ->
  (NodeId -> Int) ->
  IntSet.IntSet ->
  (NodeId -> Bool) ->
  NodeId ->
  [NodeId]
bindersForType env canonKey scopeSchemeRoots hasExplicitBoundP scopeRootN =
  let canonical = bseCanonical env
      bindParents = bseBindParents env
      isBindable = bseIsBindable env
      nodes = bseNodes env
      direct = boundFlexChildrenUnder canonical bindParents isBindable (typeRef scopeRootN)
   in case lookupNodeIn nodes scopeRootN of
        Just TyForall {} -> direct
        _ ->
          if IntSet.member (canonKey scopeRootN) scopeSchemeRoots
            then direct
            else [v | v <- direct, not (hasExplicitBoundP v)]

selectBinders ::
  BinderSelectionEnv ->
  SelectBindersArgs ->
  NodeId ->
  Either ElabError [NodeId]
selectBinders env args target0 = do
  let canonKey = sbaCanonKey args
      scopeSchemeRoots = sbaScopeSchemeRoots args
      hasExplicitBoundP = sbaHasExplicitBoundP args
      aliasBinderNodes = sbaCandidatePool args
      traceM = sbaTraceWarn args
      scopeGen = sbaMGenId args
      scopeRootC = sbaNodeRef args
  binders0 <- case scopeRootC of
    GenRef gid ->
      bindersForGen
        env
        show
        aliasBinderNodes
        traceM
        gid
    TypeRef scopeRootN ->
      pure $
        bindersForType
          env
          canonKey
          scopeSchemeRoots
          hasExplicitBoundP
          scopeRootN
  traceM
    ( "generalizeAt: scopeRoot="
        ++ show scopeRootC
        ++ " scopeGen="
        ++ show scopeGen
        ++ " target="
        ++ show target0
        ++ " binders="
        ++ show binders0
    )
  pure binders0
