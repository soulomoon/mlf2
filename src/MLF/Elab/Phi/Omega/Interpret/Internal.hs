{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : MLF.Elab.Phi.Omega.Interpret.Internal
-- Description : Omega/Step interpretation flow for witness translation (implementation)
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
--
-- Internal implementation of the Omega\/Step interpretation flow.
-- The public facade "MLF.Elab.Phi.Omega.Interpret" re-exports
-- 'phiWithSchemeOmega' from this module.
module MLF.Elab.Phi.Omega.Interpret.Internal
  ( phiWithSchemeOmega,
    phiWithSchemeOmegaOccurrence,
    orderPhiBindersByPrec,
  )
where

{- Note [Omega/Step witness interpretation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'phiWithSchemeOmega' is the core of the Φ translation for witness edges with
Ω (instance-operation) payloads, as described in the thesis §15.3.4 and
xMLF paper (Fig. 10).

The interpretation proceeds in three stages:

  1. **Binder reordering (Σ)**: Reorder quantifier binders from graph <P order
     to the target scheme's binder order, emitting 'InstUnder' context steps
     where the two orders differ (thesis Def. 15.3.4 / Fig. 15.3.5).

  2. **Forall introduction**: Emit 'InstInside' steps for the ∀-intro count
     recorded per edge during presolution.

  3. **Omega loop ('go')**: Walk the list of 'InstanceOp' values (OpRaise,
     OpMerge, OpWeaken, OpGraft) and translate each into xMLF instantiation
     steps.  'continueRaise' handles the multi-step OpRaise translation that
     involves context navigation and binder application.

The function captures the presolution view, edge trace, scheme info, and
constraint structure in a large closure environment.  Reification and inferred
argument helpers stay local to this closure because they depend on the shared
canonical/constraint/trace context.

Paper references:
  * Yakobowski PhD thesis (2008), §15.3.4 — Φ translation and Ω execution
  * xMLF paper (Rémy & Yakobowski, FLOPS 2010), Fig. 10 — Ω operational rules
-}

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution (EdgeTrace (..), PresolutionView (..))
import MLF.Constraint.Presolution.Base
  ( EdgeSourceInterior (..),
    InteriorNodes (..),
    rootRaiseMergeTraceAuthority,
    rootWeakenRaiseMergeTraceAuthority,
  )
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Presolution ()
import MLF.Elab.Inst (applyInstantiation, composeInst, instMany, schemeToType)
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Phi.Context (contextToNodeBoundWithOrderKeys)
import MLF.Elab.Phi.Computation
  ( OccurrenceComputation,
    composeOccurrenceComputation,
    mkEdgeTranslation,
    mkQuantifierReordering,
    occurrenceComputationInstantiation,
  )
import MLF.Elab.Phi.Omega.Domain
  ( OmegaContext (..),
    isBinderNode,
    isTraceBinderSource,
    lookupBinderIndex,
    mkOmegaDomainEnv,
    resolveNonRootGraftBinder,
    resolveNonRootWeakenBinder,
    resolveTraceBinderTarget,
  )
import MLF.Elab.Phi.Omega.Normalize (collapseAdjacentPairs, normalizeInst)
import MLF.Elab.Phi.VSpine
  ( BodyShape (..),
    VSpine (..),
    assertSpineSync,
    mkVSpine,
    vSpineBinderAt,
    vSpineBinderRefs,
    vSpineBoundAt,
    vSpineIdAt,
    vSpineIds,
    vSpineLength,
    vSpineNull,
    vsDeleteAt,
    vsInsertAt,
    vsUpdateBound
  )
import MLF.Elab.Run.Instantiation (containsForallType, inferInstAppArgsFromSchemeRefs)
import MLF.Elab.Sigma (bubbleReorderTo)
import MLF.Elab.Types
import MLF.Reify.Bound (reifyBoundWithRefsOnConstraint)
import MLF.Reify.Type (reifyTypeWithRefsNoFallbackOnConstraint)
import MLF.Reify.TypeOps (alphaEqType, freeTypeVarRefsList, inlineAliasBoundsWithBy, inlineBaseBoundsType, splitForallsRefs, substTypeCaptureRef)
import MLF.Util.Graph (topoSortBy)
import qualified MLF.Util.Order as Order
import qualified MLF.Util.OrderKey as OrderKey
import MLF.Util.Trace (traceGeneralize)

data TypeArgKey
  = TypeArgIdentity TypeBinderIdentity
  deriving (Eq, Ord, Show)

typeArgKeyForRef :: TypeBinderRef -> TypeArgKey
typeArgKeyForRef =
  TypeArgIdentity . typeBinderRefIdentity

-- | Order one virtual quantifier spine by the paper's leftmost-lowermost
-- relation.  The caller supplies the exact identity domain that belongs to
-- the published scheme; other virtual entries retain their current order.
--
-- Kept top-level so the test-support facade can exercise missing-order and
-- dependency-cycle behavior without manufacturing an entire presolution.
orderPhiBindersByPrec ::
  (NodeId -> NodeId) ->
  (NodeId -> Bool) ->
  IntMap.IntMap Order.OrderKey ->
  [(TypeBinderRef, Maybe BoundType, Maybe NodeId)] ->
  Either ElabError [Maybe NodeId]
orderPhiBindersByPrec canonicalNode isOrderedBinder orderKeysActive binders = do
  let refs = [ref | (ref, _, _) <- binders]
      binderMap = IntMap.fromList (zip [0 ..] binders)
      refIndex ref = findIndex (typeBinderRefsSameIdentity ref) refs
      schemeNodesByIndex =
        [ (index, canonicalNode nodeId)
        | (index, (_, _, Just nodeId)) <- IntMap.toList binderMap
        , isOrderedBinder nodeId
        ]

  schemeOrderEntries <-
    mapM
      ( \(index, nodeId) ->
          case IntMap.lookup (getNodeId nodeId) orderKeysActive of
            Just orderKey -> Right (index, orderKey)
            Nothing ->
              Left $
                PhiInvariantError $
                  "PhiReorder: missing order key for binder "
                    ++ show nodeId
                    ++ "; available keys="
                    ++ show (IntMap.keys orderKeysActive)
                    ++ "; spine="
                    ++ show binders
      )
      schemeNodesByIndex
  let schemeOrderKeysByIndex = IntMap.fromList schemeOrderEntries
  mapM_
    ( \((_, leftNode), (_, rightNode)) ->
        case
          Order.compareNodesByOrderKey
            orderKeysActive
            leftNode
            rightNode
          of
            Right _ -> Right ()
            Left err ->
              Left $
                PhiInvariantError $
                  "PhiReorder: invalid order-key relation: "
                    ++ show err
    )
    [ (left, right)
    | (leftIndex, left) <- zip [(0 :: Int) ..] schemeNodesByIndex
    , right <- drop (leftIndex + 1) schemeNodesByIndex
    ]

  let
      -- Bound dependencies: if a occurs free in b's bound, then a must appear
      -- before b.
      depsFor :: Int -> [Int]
      depsFor i =
        case (IntMap.lookup i binderMap, listToMaybe (drop i refs)) of
          (Just (_binderRef, Just bnd, _), Just binderRef) ->
            [ j
            | ref <- freeTypeVarRefsList bnd
            , not (typeBinderRefsSameIdentity ref binderRef)
            , Just j <- [refIndex ref]
            ]
          _ -> []

      cmpIdx :: Int -> Int -> Ordering
      cmpIdx i j =
        case
          ( IntMap.lookup i schemeOrderKeysByIndex
          , IntMap.lookup j schemeOrderKeysByIndex
          )
          of
            (Just leftKey, Just rightKey) ->
              case Order.compareOrderKey leftKey rightKey of
                EQ -> compare i j
                ordering -> ordering
            _ -> compare i j
      indices = [0 .. length binders - 1]

  idxs <-
    topoSortBy
      "PhiReorder: cycle in bound dependencies"
      cmpIdx
      depsFor
      indices
  mapM
    ( \i ->
        case IntMap.lookup i binderMap of
          Just (_, _, mid) -> Right mid
          Nothing ->
            Left $
              PhiInvariantError $
                "PhiReorder: binder index "
                  ++ show i
                  ++ " out of range during reorder"
    )
    idxs

phiWithSchemeOmega ::
  OmegaContext p ->
  IntSet.IntSet ->
  SchemeInfo ->
  -- | forall intro count (O phase)
  Int ->
  -- | omega ops
  [InstanceOp] ->
  Either ElabError Instantiation
phiWithSchemeOmega ctx namedSet si introCount omegaOps =
  occurrenceComputationInstantiation
    <$> phiWithSchemeOmegaOccurrence ctx namedSet si introCount omegaOps

-- | Construct the paper-shaped occurrence computation @phi_R;T(e)@.
--
-- The reordering and edge-local parts are validated independently before the
-- strict identity-bearing seam between them is composed.  This is the
-- authoritative production entry point; 'phiWithSchemeOmega' is only the
-- legacy instantiation projection.
phiWithSchemeOmegaOccurrence ::
  OmegaContext p ->
  IntSet.IntSet ->
  SchemeInfo ->
  -- | forall intro count (O phase)
  Int ->
  -- | omega ops
  [InstanceOp] ->
  Either ElabError OccurrenceComputation
phiWithSchemeOmegaOccurrence ctx namedSet si introCount omegaOps = phiWithScheme
  where
    presolutionView = ocPresolutionView ctx

    canonicalNode :: NodeId -> NodeId
    canonicalNode = pvCanonical presolutionView

    lookupNodePV :: NodeId -> Maybe TyNode
    lookupNodePV = pvLookupNode presolutionView

    lookupVarBound :: NodeId -> Maybe NodeId
    lookupVarBound = pvLookupVarBound presolutionView

    lookupBindParent :: NodeRef -> Maybe (NodeRef, BindFlag)
    lookupBindParent = pvLookupBindParent presolutionView

    bindParents :: BindParents
    bindParents = pvBindParents presolutionView

    constraint = pvConstraint presolutionView

    reifyBoundWithRefsAt :: IntMap.IntMap TypeBinderRef -> NodeId -> Either ElabError ElabType
    reifyBoundWithRefsAt = ocReifyBoundWithRefs ctx

    reifyTypeWithNamedSetRefsNoFallbackAt ::
      IntMap.IntMap TypeBinderRef ->
      IntSet.IntSet ->
      NodeId ->
      Either ElabError ElabType
    reifyTypeWithNamedSetRefsNoFallbackAt = ocReifyTypeWithNamedSetRefsNoFallback ctx

    copyMap :: IntMap.IntMap NodeId
    copyMap = ocCopyMap ctx

    traceInfo :: EdgeTrace
    traceInfo = ocTrace ctx

    -- Note [Witness-domain diagnostics only]: failure messages may report raw
    -- witness-domain source matches derived from trace/copy-map artifacts, but
    -- runtime binder selection below remains direct and fail-fast on replay-
    -- spine targets. These diagnostics never participate in target recovery.

    mSchemeInfo :: Maybe SchemeInfo
    mSchemeInfo = ocSchemeInfo ctx

    traceBinderSources :: IntSet.IntSet
    traceBinderSources = ocTraceBinderSources ctx

    replaySpineSources :: IntSet.IntSet
    replaySpineSources = ocReplaySpineSources ctx

    traceBinderReplayMap :: IntMap.IntMap NodeId
    traceBinderReplayMap = ocTraceBinderReplayMap ctx

    producerReplayBinderKeys :: IntSet.IntSet
    producerReplayBinderKeys =
      IntSet.fromList (map getNodeId (etReplayDomainBinders traceInfo))

    edgeRoot :: NodeId
    edgeRoot = ocEdgeRoot ctx

    edgeLeft :: NodeId
    edgeLeft = ocEdgeLeft ctx

    edgeRight :: NodeId
    edgeRight = ocEdgeRight ctx

    domainEnv = mkOmegaDomainEnv ctx

    replayBinderKeys :: [Int]
    replayBinderKeys = schemeInfoBinderIdentityKeys si

    resolveTraceBinderTarget' :: Bool -> String -> NodeId -> Either ElabError NodeId
    resolveTraceBinderTarget' requireBinder opName =
      resolveTraceBinderTarget domainEnv requireBinder opName replayBinderKeys isSchemeBinder

    resolveNonRootGraftBinder' ::
      IntSet.IntSet ->
      VSpine ->
      (NodeId -> Maybe String) ->
      NodeId ->
      NodeId ->
      Either ElabError NodeId
    resolveNonRootGraftBinder' binderKeys vs lookupBinder =
      resolveNonRootGraftBinder domainEnv binderKeys (vSpineIds vs) lookupBinder

    resolveNonRootWeakenBinder' ::
      IntSet.IntSet ->
      VSpine ->
      NodeId ->
      NodeId ->
      Either ElabError NodeId
    resolveNonRootWeakenBinder' binderKeys vs =
      resolveNonRootWeakenBinder domainEnv binderKeys (vSpineIds vs)

    lookupBinderIndex' :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Maybe Int
    lookupBinderIndex' = lookupBinderIndex domainEnv

    isBinderNode' :: IntSet.IntSet -> NodeId -> Bool
    isBinderNode' = isBinderNode domainEnv

    isTraceBinderSource' :: NodeId -> Bool
    isTraceBinderSource' = isTraceBinderSource domainEnv

    isReplaySpineSource :: NodeId -> Bool
    isReplaySpineSource source =
      IntSet.member (getNodeId source) replaySpineSources

    debugPhi :: String -> a -> a
    debugPhi = traceGeneralize (ocTraceConfig ctx)

    interiorSet :: IntSet.IntSet
    interiorSet =
      let EdgeSourceInterior (InteriorNodes s0) = etInterior traceInfo
          remapKey k =
            let nidC = canonicalNode (NodeId k)
                keyC = getNodeId nidC
             in case lookupNodePV nidC of
                  Just TyVar {} ->
                    case IntMap.lookup keyC copyMap of
                      Nothing -> keyC
                      Just nid -> getNodeId (canonicalNode nid)
                  _ -> keyC
       in IntSet.fromList (map remapKey (IntSet.toList s0))

    rootRaiseMergeTraceProof :: NodeId -> NodeId -> Bool
    rootRaiseMergeTraceProof operated other =
      rootRaiseMergeTraceAuthority operated other traceInfo

    orderRoot :: NodeId
    -- Omega operations are frozen in the source witness domain, whose root is
    -- `etRoot`.  Keep this authority distinct from `sigmaOrderRoot`: the paper's
    -- quantifier reordering targets Typexp and therefore uses the destination
    -- expansion root `sc` instead.
    orderRoot = etRoot traceInfo

    sigmaOrderRoot :: NodeId
    -- Thesis Def. 15.3.3/15.3.4: Typexp is S'(sc), where sc is the root of the
    -- expansion at the edge destination.  `etResultRoot` is the construction
    -- authority for exactly that root.
    sigmaOrderRoot = etResultRoot traceInfo

    nodes = cNodes constraint

    boundKids nid = case lookupNodeIn nodes nid of
      Just TyVar {tnBound = Just bnd} -> [bnd]
      _ -> []

    schemeRootGenMap =
      IntMap.fromList
        [ (getNodeId (canonicalNode root), gnId gen)
          | gen <- NodeAccess.allGenNodes constraint,
            root <- gnSchemes gen
        ]

    genChildrenMap =
      IntMap.fromListWith
        (++)
        [ ( getGenNodeId gid,
            [canonicalNode child]
          )
          | (childKey, (parentRef, _flag)) <- IntMap.toList bindParents,
            GenRef gid <- [parentRef],
            TypeRef child <- [nodeRefFromKey childKey]
        ]

    bindKids rootC nid =
      let localChildren =
            [ canonicalNode child
              | (childKey, (parentRef, _flag)) <- IntMap.toList bindParents,
                TypeRef child <- [nodeRefFromKey childKey],
                parentRefCanon parentRef == TypeRef (canonicalNode nid)
            ]
          genChildren =
            case IntMap.lookup (getNodeId (canonicalNode nid)) schemeRootGenMap of
              Nothing -> []
              Just gid -> IntMap.findWithDefault [] (getGenNodeId gid) genChildrenMap
          siblingGenChildren =
            if canonicalNode nid == rootC
              then case lookupBindParent (typeRef (canonicalNode nid)) of
                Just (GenRef gid, _) -> IntMap.findWithDefault [] (getGenNodeId gid) genChildrenMap
                _ -> []
              else []
       in IntMap.elems $
            IntMap.fromList
              [ (getNodeId child, child)
                | child <- localChildren ++ genChildren ++ siblingGenChildren
              ]

    parentRefCanon (TypeRef parentN) = TypeRef (canonicalNode parentN)
    parentRefCanon (GenRef gid) = GenRef gid

    orderKeysFromRoot root =
      let rootC = canonicalNode root
          extraChildren nid = boundKids nid ++ bindKids rootC nid
       in OrderKey.orderKeysFromRootWithExtra canonicalNode nodes extraChildren root Nothing

    orderKeys :: IntMap.IntMap Order.OrderKey
    -- Order keys compare the quantifiers in Typexp (thesis Def. 15.3.4), so
    -- derive them once from the exact destination expansion root.  Missing
    -- coverage is an invalid construction, not a reason to switch roots.
    orderKeys = orderKeysFromRoot sigmaOrderRoot

    schemeBinderKeys :: IntSet.IntSet
    schemeBinderKeys = schemeInfoBinderIdentityKeySet si

    isSchemeBinder :: NodeId -> Bool
    isSchemeBinder nid =
      IntSet.member (getNodeId nid) schemeBinderKeys

    isProducerReplayBinder :: NodeId -> Bool
    isProducerReplayBinder nid =
      IntSet.member (getNodeId nid) producerReplayBinderKeys

    substForTypes :: IntMap.IntMap TypeBinderRef
    substForTypes =
      case mSchemeInfo of
        Just si' -> schemeInfoBinderRefSubst si'
        Nothing -> IntMap.empty

    schemeRefForTraceBinder :: NodeId -> Maybe TypeBinderRef
    schemeRefForTraceBinder binder =
      let exactKey = getNodeId binder
          replayRef = do
            replayBinder <- IntMap.lookup exactKey traceBinderReplayMap
            IntMap.lookup (getNodeId replayBinder) substForTypes
       in replayRef
            <|> IntMap.lookup exactKey substForTypes
            <|> IntMap.lookup (getNodeId (canonicalNode binder)) substForTypes

    sourceBinderBound :: NodeId -> Either ElabError ElabType
    sourceBinderBound binder =
      case NodeAccess.lookupNode sourceConstraint binder of
        Just TyVar {tnBound = Just _} ->
          -- Reify from the binder, not directly from its bound node.  The
          -- bound node may be the body below a TyForall wrapper; starting
          -- at the binder lets bound reification recover that wrapper.
          reifyBoundWithRefsOnConstraint sourceConstraint IntMap.empty binder
        _ -> sourceBinderBoundFromFinalConstraint binder

    -- Some operation nodes are created after the frozen Gamma snapshot.  Only
    -- absence from that snapshot admits the final constraint as their owner;
    -- an existing source node never has its bound replaced by finalized state.
    sourceBinderBoundFromFinalConstraint :: NodeId -> Either ElabError ElabType
    sourceBinderBoundFromFinalConstraint binder =
      case NodeAccess.lookupNode constraint binder of
        Just TyVar {tnBound = Just _} -> reifyBoundWithRefsAt IntMap.empty binder
        _ -> Left (PhiInvariantError ("trace binder has no explicit source bound: " ++ show binder))

    -- OpRaise is classified in the witness's frozen source domain.  The
    -- finalized graph may already mark its replay representative rigid as the
    -- result of executing this very operation; consulting that state would
    -- erase the non-identity Raise computation after construction.
    sourceRaiseBindParent sourceNode =
      NodeAccess.lookupBindParent
        sourceConstraint
        (typeRef sourceNode)

    sourceWeakenedOperationType :: NodeId -> Either ElabError ElabType
    sourceWeakenedOperationType operated =
      case NodeAccess.lookupNode sourceConstraint operated of
        Just node ->
          operationTypeOn sourceConstraint node
        -- Expansion/solve may have constructed this operation node after
        -- Gamma was frozen.  Final-state lookup is admissible only when the
        -- source certificate proves that no source-domain node existed.
        Nothing ->
          case NodeAccess.lookupNode constraint operated of
            Just node -> operationTypeOn constraint node
            Nothing ->
              Left
                ( PhiInvariantError
                    ("Weaken/RaiseMerge operated source is absent: " ++ show operated)
                )
      where
        operationTypeOn operationConstraint node =
          case node of
            TyVar {tnBound = Just _} ->
              reifyBoundWithRefsOnConstraint operationConstraint IntMap.empty operated
            TyVar {tnBound = Nothing} -> Right TBottom
            _ ->
              reifyTypeWithRefsNoFallbackOnConstraint
                operationConstraint
                IntMap.empty
                operated

    sourceConstraint = gaBaseConstraint (ocGaParents ctx)

    traceArgMap :: IntSet.IntSet -> Map.Map TypeArgKey ElabType
    traceArgMap namedSet' =
      case mSchemeInfo of
        Just si' ->
          let subst = schemeInfoBinderRefSubst si'
              reifyArg binder arg =
                let argC = canonicalNode arg
                    direct = reifyTypeWithNamedSetRefsNoFallbackAt subst namedSet' argC
                    viaBound = case lookupVarBound argC of
                      Just bnd -> reifyBoundWithRefsAt subst bnd
                      Nothing -> direct
                    viaSourceBound = sourceBinderBound binder
                 in case (direct, viaBound, viaSourceBound) of
                      (Right tyDirect, _, Right tySourceBound)
                        | TVarRef {} <- tyDirect,
                          not (containsBottomTy tySourceBound) -> Right tySourceBound
                      (Right tyDirect, Right tyBound, _)
                        | containsForallType tyDirect -> Right tyDirect
                        | containsBottomTy tyDirect && not (containsBottomTy tyBound) -> Right tyBound
                        | otherwise -> Right tyDirect
                      (Right tyDirect, Left _, _) -> Right tyDirect
                      (Left _, Right tyBound, _) -> Right tyBound
                      (Left _, Left _, Right tySourceBound) -> Right tySourceBound
                      (Left err, Left _, Left _) -> Left err
              entries =
                [ (typeArgKeyForRef ref, ty)
                  | (binder, arg) <- etBinderArgs traceInfo,
                    Just ref <- [schemeRefForTraceBinder binder],
                    Right ty <- [reifyArg binder arg]
                ]
           in Map.fromList entries
        Nothing -> Map.empty

    inferredArgMapFromTarget :: IntSet.IntSet -> Map.Map TypeArgKey ElabType
    inferredArgMapFromTarget namedSet' =
      case mSchemeInfo of
        Nothing -> Map.empty
        Just si' ->
          let inferFrom nid =
                case reifyTargetTypeForInst namedSet' nid of
                  Left _ -> Nothing
                  Right targetTy -> inferInstAppArgs (siScheme si') targetTy
              mbArgs =
                inferFrom edgeRight
                  <|> inferFrom edgeLeft
           in case mbArgs of
                Nothing -> Map.empty
                Just args ->
                  let refs = map fst (schemeBinderRefs (siScheme si'))
                   in Map.fromList (zip (map typeArgKeyForRef refs) args)

    preferInferredArg :: ElabType -> ElabType -> ElabType
    preferInferredArg targetArg traceArg =
      case targetArg of
        TVarRef _
          | not (containsBottomTy traceArg) -> traceArg
        _ -> targetArg

    inferredArgMap :: IntSet.IntSet -> Map.Map TypeArgKey ElabType
    inferredArgMap namedSet' =
      Map.unionWith preferInferredArg (inferredArgMapFromTarget namedSet') (traceArgMap namedSet')

    _binderArgType :: IntSet.IntSet -> NodeId -> Maybe ElabType
    _binderArgType namedSet' binder = do
      ref <- schemeRefForTraceBinder binder
      Map.lookup (typeArgKeyForRef ref) (inferredArgMap namedSet')

    substRefForTypeRef :: TypeBinderRef -> Maybe TypeBinderRef
    substRefForTypeRef ref = do
      nid <- typeBinderRefNode ref
      IntMap.lookup (getNodeId nid) substForTypes

    reifyTypeArg :: IntSet.IntSet -> Maybe NodeId -> NodeId -> Either ElabError ElabType
    reifyTypeArg namedSet' mbBinder arg = do
      let argC = canonicalNode arg
      ty <-
        case IntMap.lookup (getNodeId arg) (ocFrozenEndpointTypes ctx) of
          Just exactEndpoint -> pure exactEndpoint
          Nothing ->
            case lookupVarBound argC of
              Just bnd -> reifyTypeWithNamedSetRefsNoFallbackAt substForTypes namedSet' bnd
              Nothing -> reifyTypeWithNamedSetRefsNoFallbackAt substForTypes namedSet' argC
      let inferredSingleton =
            case Map.toList (inferredArgMapFromTarget namedSet') of
              [(_name, inferredTy)] -> Just inferredTy
              _ -> Nothing
          chosenTy0 =
            case (ty, inferredSingleton) of
              (TVarRef _, Just inferredTy)
                | not (containsBottomTy inferredTy) -> inferredTy
              _ -> ty
          chosenTy1 =
            case (chosenTy0, inferredSingleton) of
              (_, Just (TVarRef inferredRef)) ->
                let binderRef =
                      fromMaybe
                        inferredRef
                        (substRefForTypeRef inferredRef)
                 in case filter (not . typeBinderRefsSameIdentity binderRef) (freeTypeVarRefsList chosenTy0) of
                      [fv] -> substTypeCaptureRef fv (TVarRef binderRef) chosenTy0
                      _ -> chosenTy0
              (TVarRef _, _) -> chosenTy0
              _ -> chosenTy0
          chosenTy = substSchemeNames chosenTy1
          rescuedTy =
            case (mbBinder, chosenTy) of
              (Just binder, TBottom) ->
                case schemeRefForTraceBinder binder of
                  Just binderRef -> TVarRef binderRef
                  Nothing -> chosenTy
              _ -> chosenTy
      debugPhi
        ( "reifyTypeArg(reify) arg="
            ++ show arg
            ++ " mbBinder="
            ++ show mbBinder
            ++ " inferredFromTarget="
            ++ show (inferredArgMapFromTarget namedSet')
            ++ " inferredMap="
            ++ show (inferredArgMap namedSet')
            ++ " freeChosen="
            ++ show (freeTypeVarRefsList chosenTy0)
            ++ " ty="
            ++ show ty
            ++ " chosenTy0="
            ++ show chosenTy0
            ++ " chosenTy1="
            ++ show chosenTy1
            ++ " chosenTy="
            ++ show chosenTy
            ++ " rescuedTy="
            ++ show rescuedTy
        )
        (pure rescuedTy)

    substSchemeNames :: ElabType -> ElabType
    substSchemeNames = cataIx alg
      where
        alg :: TyIF i Ty -> Ty i
        alg tyNode = case tyNode of
          TVarIFRef ref ->
            case substRefForTypeRef ref of
              Just substRef -> TVarRef substRef
              Nothing -> TVarRef ref
          TArrowIF a b -> TArrow a b
          TConIFWithIdentity identity c args -> TConWithIdentity identity c args
          TVarAppIFRef ref args ->
            let ref' =
                  case substRefForTypeRef ref of
                    Just substRef -> substRef
                    Nothing -> ref
             in TVarAppRef ref' args
          TBaseIFWithIdentity identity b -> TBaseWithIdentity identity b
          TForallIFRef ref mb body -> TForallRef ref mb body
          TMuIFRef ref body -> TMuRef ref body
          TBottomIF -> TBottom

    containsBottomTy :: Ty v -> Bool
    containsBottomTy ty = case ty of
      TVarRef _ -> False
      TBaseWithIdentity _ _ -> False
      TBottom -> True
      TArrow a b -> containsBottomTy a || containsBottomTy b
      TConWithIdentity _ _ args -> any containsBottomTy args
      TVarAppRef _ args -> any containsBottomTy args
      TForallRef _ mb body -> maybe False containsBottomTy mb || containsBottomTy body
      TMuRef _ body -> containsBottomTy body

    reifyBoundType :: NodeId -> Either ElabError ElabType
    reifyBoundType = reifyBoundWithRefsAt substForTypes

    reifyTargetTypeForInst :: IntSet.IntSet -> NodeId -> Either ElabError ElabType
    reifyTargetTypeForInst namedSet' nid = do
      let nidC = canonicalNode nid
      ty <- case lookupVarBound nidC of
        Just bnd -> reifyTypeWithNamedSetRefsNoFallbackAt substForTypes namedSet' bnd
        Nothing -> reifyTypeWithNamedSetRefsNoFallbackAt substForTypes namedSet' nidC
      pure (inlineBaseBounds ty)

    inlineBaseBounds :: ElabType -> ElabType
    inlineBaseBounds =
      inlineBaseBoundsType
        constraint
        canonicalNode

    inlineAliasBounds :: ElabType -> ElabType
    inlineAliasBounds = inlineAliasBoundsWith False

    -- See Note [Scope-aware bound/alias inlining] in
    -- docs/notes/2026-01-27-elab-changes.md.
    inlineAliasBoundsWith :: Bool -> ElabType -> ElabType
    inlineAliasBoundsWith fallbackToBottom =
      inlineAliasBoundsWithBy
        fallbackToBottom
        canonicalNode
        (cNodes constraint)
        lookupVarBound
        (reifyBoundWithRefsAt substForTypes)

    inferInstAppArgs :: ElabScheme -> ElabType -> Maybe [ElabType]
    inferInstAppArgs scheme targetTy =
      inferInstAppArgsFromSchemeRefs (schemeBinderRefs scheme) (schemeBody scheme) targetTy

    -- \| Paper Def. 15.3.4 / Fig. 15.3.5: Φ(e) = Σ; O; Φχe(Ω).
    -- Thesis treats quantifier introduction (O) and witness replay (Ω) as
    -- separate phases. The intro count drives O as a prefix of InstIntro
    -- steps, then the omega ops are replayed via `go`.
    phiWithScheme :: Either ElabError OccurrenceComputation
    phiWithScheme = do
      let ty0 = schemeToType (siScheme si)
          subst = schemeInfoBinderRefSubst si
          lookupBinder (NodeId i) = typeBinderRefName <$> IntMap.lookup i subst
          ids0 = idsForStartType si ty0
          binderKeys = IntSet.fromAscList (IntMap.keys subst)
      -- Always attempt Σ(g) / ϕR at the start (thesis Def. 15.3.4), even if Ω has no Raise steps.
      (sigma, ty1, ids1) <- reorderBindersByPrec ty0 ids0
      -- Phase O: apply all quantifier introductions up front.
      (ty2, ids2) <- applyIntros introCount ty1 ids1
      let phiIntro = instMany (replicate introCount InstIntro)
      -- Phase Ω: replay witness operations on the intro-extended type.
      let vs2 = mkVSpine ty2 ids2
      phiOmega <- go binderKeys namedSet vs2 [] omegaOps lookupBinder
      ty3 <- applyInst "applyOmega" ty2 phiOmega
      let edgeInst = normalizeInst (instMany [phiIntro, phiOmega])
      reordering <-
        computationToElab
          "quantifier reordering"
          (mkQuantifierReordering ty0 sigma ty1)
      edgeTranslation <-
        computationToElab
          "edge translation"
          (mkEdgeTranslation ty1 edgeInst ty3)
      computationToElab
        "occurrence composition"
        (composeOccurrenceComputation reordering edgeTranslation)

    computationToElab :: Show err => String -> Either err a -> Either ElabError a
    computationToElab label =
      either
        (Left . PhiInvariantError . (("invalid " ++ label ++ ": ") ++) . show)
        Right

    -- \| Apply n quantifier introductions, prepending Nothing to ids each time.
    applyIntros :: Int -> ElabType -> [Maybe NodeId] -> Either ElabError (ElabType, [Maybe NodeId])
    applyIntros 0 ty ids = Right (ty, ids)
    applyIntros n ty ids = do
      ty' <- applyInst "applyIntros" ty InstIntro
      applyIntros (n - 1) ty' (Nothing : ids)

    applyInst :: String -> ElabType -> Instantiation -> Either ElabError ElabType
    applyInst label ty0 inst = case applyInstantiation ty0 inst of
      Left (InstantiationError msg) ->
        Left $
          PhiInvariantError $
            label ++ ": " ++ msg ++ " ; inst=" ++ pretty inst ++ " ; ty=" ++ pretty ty0
      other -> other

    reorderBindersByPrec :: ElabType -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderBindersByPrec ty ids = do
      let vs0 = mkVSpine ty ids
      assertSpineSync vs0 ty ids
      if vSpineLength vs0 < 2
        then Right (InstId, ty, ids)
        else do
          let schemeArity = length (schemeBinderRefs (siScheme si))
              missingIdPositions =
                [ i
                  | ((i, (ref, _)), Nothing) <- zip (zip [(0 :: Int) ..] schemeBinders) ids,
                    i < schemeArity,
                    binderRequiresIdentity ref
                ]
          -- Builtin type schemes (e.g. __io_bind) have synthetic binder names
          -- that never flow through the generalizer, so all identities are
          -- Nothing.  Treat this as "no reordering information available".
          if length missingIdPositions == schemeArity
            then Right (InstId, ty, ids)
            else do
              unless (null missingIdPositions) $
                Left $
                  PhiInvariantError $
                    "PhiReorder: missing binder identity at positions " ++ show missingIdPositions
              desired <- desiredBinderOrder orderKeys vs0
              reorderTo vs0 ty ids desired
      where
        schemeBinders = schemeBinderRefs (siScheme si)
        schemeBinderRefSubst = schemeInfoBinderRefSubst si

        binderRequiresIdentity ref = case typeBinderRefNode ref of
          Just _ -> True
          Nothing ->
            any
              (\substRef ->
                 typeBinderRefsSameIdentity ref substRef
              )
              (IntMap.elems schemeBinderRefSubst)

    desiredBinderOrder :: IntMap.IntMap Order.OrderKey -> VSpine -> Either ElabError [Maybe NodeId]
    desiredBinderOrder orderKeysActive vs0 = do
      let n = vSpineLength vs0
      binders <- mapM (vSpineBinderAt vs0) [0 .. n - 1]
      case
          orderPhiBindersByPrec
            canonicalNode
            isSchemeBinder
            orderKeysActive
            binders
        of
          Left (PhiInvariantError message) ->
            Left $
              PhiInvariantError $
                message
                  ++ "; sigma order root="
                  ++ show sigmaOrderRoot
                  ++ "; binder canonicalization="
                  ++ show
                    [ (nodeId, canonicalNode nodeId)
                    | (_, _, Just nodeId) <- binders
                    ]
                  ++ "; bind parents="
                  ++ show
                    [ (nodeId, lookupBindParent (typeRef (canonicalNode nodeId)))
                    | (_, _, Just nodeId) <- binders
                    ]
          other -> other

    reorderTo :: VSpine -> ElabType -> [Maybe NodeId] -> [Maybe NodeId] -> Either ElabError (Instantiation, ElabType, [Maybe NodeId])
    reorderTo _vs0 ty ids desired = bubbleReorderTo "reorderBindersByPrec" ty ids desired

    -- Interpret witness ops while tracking the current type.
    --
    -- Paper Fig. 10 uses instantiation contexts (C{.}) to reach a binder rather
    -- than swapping quantifiers. Using `InstUnder` keeps binder nesting intact,
    -- which matters for operations like Merge that reference outer binders.
    nodeExists :: NodeId -> Bool
    nodeExists nid =
      (isSchemeBinder nid && isProducerReplayBinder nid)
        || case lookupNodePV (canonicalNode nid) of
          Just _ -> True
          Nothing -> False

    go ::
      IntSet.IntSet ->
      IntSet.IntSet ->
      VSpine ->
      [Instantiation] ->
      [InstanceOp] ->
      (NodeId -> Maybe String) ->
      Either ElabError Instantiation
    go binderKeys namedSet' vs accum ops lookupBinder = case ops of
      [] -> Right (foldl' composeInst InstId (collapseAdjacentPairs (reverse accum)))
      (OpGraft _arg binder : OpWeaken weakened : rest@(OpRaiseMerge operated exterior : _))
        | binder == weakened,
          weakened == operated,
          operated == orderRoot,
          isTraceBinderSource' binder,
          not (isReplaySpineSource binder),
          rootWeakenRaiseMergeTraceAuthority operated exterior traceInfo ->
            -- The grafted root has already been inlined into S'(r), but its
            -- adjacent Weaken is also the frozen rigid-root certificate for
            -- the terminal RaiseMerge.  Drop only the inlined Graft and keep
            -- the authority pair intact; the following branch consumes it as
            -- identity.  Dropping both operations would turn a proved rigid
            -- replay into an unauthorized bare root RaiseMerge.
            go binderKeys namedSet' vs accum (OpWeaken weakened : rest) lookupBinder
      (OpGraft _arg binder : OpWeaken weakened : rest)
        | binder == weakened,
          isTraceBinderSource' binder,
          not (isReplaySpineSource binder) ->
            -- The frozen expansion records every operation-authority binder,
            -- but the finalized producer VSpine contains only binders that
            -- remain quantified.  A Graft/Weaken pair for an omitted source
            -- binder has already been inlined into S'(r); replaying it would
            -- manufacture a quantifier that the producer type does not own.
            go binderKeys namedSet' vs accum rest lookupBinder
      (OpGraft arg bv : rest) ->
        let sourceC = canonicalNode bv
            rootC = canonicalNode orderRoot
         in if sourceC == rootC
          then do
            if vSpineNull vs
              then do
                argTy <- reifyTypeArg namedSet' Nothing arg
                let inst =
                      if vsBody vs == BodyBottom
                        then InstBot argTy
                        else InstId
                    vs' = if vsBody vs == BodyBottom then vs {vsBody = BodyNonBottom} else vs
                go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
              else do
                argTy <- reifyTypeArg namedSet' Nothing arg
                let inst = InstApp argTy
                    vs' = vsDeleteAt 0 vs
                go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
          else do
            bvReplay <- resolveTraceBinderTarget' True "OpGraft" bv
            let bvC = canonicalNode bvReplay
            if not (isBinderNode' binderKeys bvReplay)
              then
                Left $
                  PhiTranslatabilityError
                    [ "OpGraft targets non-binder node",
                      "  target node: " ++ show bv,
                      "  canonical: " ++ show bvC
                    ]
              else do
                bvResolved <- resolveNonRootGraftBinder' binderKeys vs lookupBinder bv bvReplay
                case lookupBinderIndex' binderKeys (vSpineIds vs) bvResolved of
                  Nothing ->
                    Left $
                      PhiTranslatabilityError
                        [ "OpGraft: binder not found in quantifier spine",
                          "  target node: " ++ show bv,
                          "  canonical: " ++ show bvC
                        ]
                  Just i -> do
                    mbBound <- vSpineBoundAt vs i
                    if mbBound /= Just TBottom && mbBound /= Nothing
                      then do
                        argTy <- reifyTypeArg namedSet' Nothing arg
                        let boundTy = maybe TBottom tyToElab mbBound
                        if alphaEqType argTy boundTy
                          then -- Bounded-match: bound already equals graft arg, so
                          -- OpGraft is a no-op. The adjacent OpWeaken will emit
                          -- InstElim which substitutes the existing bound (thesis Def. 14.2.1).
                            go binderKeys namedSet' vs accum rest lookupBinder
                          else
                            if argTy == TBottom
                              then -- Bottom arg on bounded binder: no-op (OpWeaken will emit InstElim)
                                go binderKeys namedSet' vs accum rest lookupBinder
                              else
                                Left $
                                  PhiTranslatabilityError
                                    [ "OpGraft requires target binder to be unbounded/⊥-bounded or match its explicit bound",
                                      "  target node: " ++ show bv,
                                      "  canonical: " ++ show bvC,
                                      "  binder bound: " ++ show mbBound,
                                      "  graft arg: " ++ show argTy
                                    ]
                      else do
                        i' <- binderIndex binderKeys (vSpineIds vs) bvResolved
                        argTy <- reifyTypeArg namedSet' (Just bvResolved) arg
                        prefix <- prefixBinderRefs vs i'
                        let inst = underContext prefix (InstInside (InstBot argTy))
                            newBound = either (const Nothing) Just (elabToBound argTy)
                            vs' = vsUpdateBound i' newBound vs
                        go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
      (OpWeaken weakened : OpRaiseMerge operated exterior : rest)
        | weakened == operated,
          operated == orderRoot ->
            if rootWeakenRaiseMergeTraceAuthority operated exterior traceInfo
              then go binderKeys namedSet' vs accum rest lookupBinder
              else
                Left $
                  PhiTranslatabilityError
                    [ "root Weaken/RaiseMerge lacks exact rigid replay authority",
                      "  operated root: " ++ show operated,
                      "  exterior target: " ++ show exterior
                    ]
      (OpWeaken bv : rest) ->
        let sourceC = canonicalNode bv
            rootC = canonicalNode orderRoot
         in if sourceC == rootC
          then go binderKeys namedSet' vs accum rest lookupBinder
          else do
            bvReplay <- resolveTraceBinderTarget' False "OpWeaken" bv
            -- Strict replay invariant: non-root OpWeaken must resolve to a
            -- binder in the current replay spine and emit InstElim; otherwise
            -- translation fails fast.
            weakenBinder <- resolveNonRootWeakenBinder' binderKeys vs bv bvReplay
            -- Thesis-exact OpWeaken: always emit InstElim.
            -- For graft+weaken pairs, collapseAdjacentPairs merges
            -- the preceding InstInside(InstBot t) with this InstElim
            -- into InstApp t (thesis Def. 14.2.1).
            (inst, vs') <- atBinderWith False binderKeys vs weakenBinder (pure InstElim)
            go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
      (OpRaise n : rest) -> do
        nReplay <- resolveTraceBinderTarget' False "OpRaise" n
        let nSource = n
            replayIsSchemeBinder =
              isSchemeBinder nReplay && isProducerReplayBinder nReplay
            nAdopt = if replayIsSchemeBinder then nReplay else canonicalNode nReplay
        if not (nodeExists nAdopt)
          then
            if IntSet.member (getNodeId nSource) traceBinderSources
              then
                Left $
                  PhiInvariantError $
                    unlines
                      [ "trace/replay binder key-space mismatch (OpRaise unresolved trace-source target)",
                        "op: OpRaise",
                        "source target: " ++ show nSource,
                        "replay-map domain: " ++ show (IntMap.keys traceBinderReplayMap)
                      ]
              else
                Left $
                  PhiInvariantError $
                    unlines
                      [ "OpRaise unresolved target has no direct replay/source node",
                        "op: OpRaise",
                        "source target: " ++ show nSource,
                        "replay target: " ++ show nReplay
                      ]
          else do
            let nOrig = if replayIsSchemeBinder then nAdopt else canonicalNode nAdopt
            case debugPhi
              ( "OpRaise: nSource="
                  ++ show nSource
                  ++ " nReplay="
                  ++ show nReplay
                  ++ " nAdopt="
                  ++ show nAdopt
                  ++ " nOrig="
                  ++ show nOrig
              )
              () of
              () -> pure ()
            raiseTarget <-
              case lookupNodePV nOrig of
                Just TyForall {tnBody = body} -> do
                  binders <- bindingToElab (Binding.orderedBinders canonicalNode constraint (typeRef nOrig))
                  let bodyC = canonicalNode body
                  pure $ case binders of
                    (b : _) -> canonicalNode b
                    [] -> bodyC
                _ -> pure nOrig
            let nC = raiseTarget
            case debugPhi ("OpRaise: raiseTarget=" ++ show nC) () of
              () -> pure ()
            case debugPhi ("OpRaise: parent=" ++ show (lookupBindParent (typeRef nC))) () of
              () -> pure ()
            nContextTarget <-
              case lookupNodePV nC of
                Just TyExp {tnBody = body} -> pure (canonicalNode body)
                _ -> pure nC
            let shouldRigidSkip =
                  case sourceRaiseBindParent nSource of
                    Just (_, BindRigid) -> True
                    _ -> False
            if shouldRigidSkip
              then case debugPhi ("OpRaise: rigid skip target=" ++ show nC) () of
                () -> go binderKeys namedSet' vs accum rest lookupBinder
              else do
                continueRaise
                  binderKeys
                  namedSet'
                  vs
                  accum
                  rest
                  lookupBinder
                  nSource
                  nAdopt
                  nOrig
                  nC
                  nContextTarget
      (OpMerge eliminated retained : rest)
        | isTraceBinderSource' eliminated,
          not (isReplaySpineSource eliminated),
          isReplaySpineSource retained ->
            -- The normalized Merge orientation is also the retained-source
            -- certificate used to build the producer VSpine.  Its first
            -- operand has already disappeared from that type tree, while the
            -- second operand is the surviving quantifier.  Requiring the
            -- eliminated replay target to be a VSpine binder would recreate
            -- a quantifier that S(r) no longer contains; the merge is already
            -- reflected in S(r), and subsequent operations still act on the
            -- retained binder explicitly.
            go binderKeys namedSet' vs accum rest lookupBinder
      (OpMerge n m : rest) -> do
        nReplay <- resolveTraceBinderTarget' True "OpMerge(n)" n
        mReplay <- resolveTraceBinderTarget' True "OpMerge(m)" m
        if isRigidNode nReplay
          then go binderKeys namedSet' vs accum rest lookupBinder
          else
            if isRigidNode mReplay
              then
                Left $
                  PhiTranslatabilityError
                    [ "OpMerge: rigid endpoint appears only on non-operated node",
                      "  operated node n: " ++ show n,
                      "  other endpoint m: " ++ show m
                    ]
              else
                if not (isBinderNode' binderKeys nReplay)
                  then
                    Left $
                      PhiTranslatabilityError
                        [ "OpMerge: first target is non-binder node",
                          "  target node: " ++ show n,
                          "  canonical: " ++ show (canonicalNode nReplay)
                        ]
                  else
                    if nReplay == mReplay
                      then go binderKeys namedSet' vs accum rest lookupBinder
                      else do
                        mRef <- mergeOtherBinderRef binderKeys vs m mReplay lookupBinder
                        let hAbs = InstSeq (InstInside (instAbstrWithRef mRef)) InstElim
                        (inst, vs') <- atBinderWith False binderKeys vs nReplay (pure hAbs)
                        go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
      (OpRaiseMerge n _ : OpWeaken weakened : rest)
        | weakened == n,
          n /= orderRoot -> do
            nReplay <- resolveTraceBinderTarget' True "OpRaiseMerge/Weaken" n
            if isRigidNode nReplay
              then go binderKeys namedSet' vs accum rest lookupBinder
              else do
                if not (isBinderNode' binderKeys nReplay)
                  then
                    -- No quantifier for n exists in S(r), so its terminal
                    -- rigidification is already inlined regardless of the
                    -- source graph node constructor.
                    go binderKeys namedSet' vs accum rest lookupBinder
                  else do
                    -- Lemma 15.3.11: the bounds of n and the exterior merge
                    -- target are syntactically equal.  Figure 15.3.4's
                    -- following Weaken therefore instantiates n with that
                    -- exact bound; emit the resulting application directly
                    -- while n is still in the replay spine.
                    boundTy <- sourceWeakenedOperationType n
                    (inst, vs') <-
                      atBinderWith
                        False
                        binderKeys
                        vs
                        nReplay
                        (pure (InstApp boundTy))
                    go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
      (OpRaiseMerge n m : rest) -> do
        if n == orderRoot
          then
            if rootRaiseMergeTraceProof n m
              then do
                -- Figure 15.3.4 translates RaiseMerge(r,m) directly to
                -- alpha_m-triangle.  A prepared Gamma substitution is the
                -- authority for the outward identity of that Hyp; retain the
                -- source-domain witness identity only when no such authority
                -- is present.
                let mName = fromMaybe ("t" ++ show (getNodeId m)) (lookupBinder m)
                    sourceMRef =
                      typeBinderRefFromIdentity
                        (typeBinderIdentityFromNode m)
                        mName
                    mRef = fromMaybe sourceMRef (schemeRefForTraceBinder m)
                let vs' = VSpine [] BodyNonBottom
                go binderKeys namedSet' vs' (instAbstrWithRef mRef : accum) rest lookupBinder
              else
                Left $
                  PhiTranslatabilityError
                    [ "OpRaiseMerge: root operation lacks exact source-interior trace authority",
                      "  operated root: " ++ show n,
                      "  exterior target: " ++ show m,
                      "  trace: " ++ show traceInfo
                    ]
          else do
            nReplay <- resolveTraceBinderTarget' True "OpRaiseMerge(n)" n
            mReplay <- resolveTraceBinderTarget' True "OpRaiseMerge(m)" m
            -- Paper Fig. 15.3.4: rigid-node identity is conditioned on the operated node n.
            if isRigidNode nReplay
              then go binderKeys namedSet' vs accum rest lookupBinder
              else
                if isRigidNode mReplay
                  then
                    Left $
                    PhiTranslatabilityError
                      [ "OpRaiseMerge: rigid endpoint appears only on non-operated node",
                        "  operated node n: " ++ show n,
                        "  other endpoint m: " ++ show m,
                        "  replay n: " ++ show nReplay,
                        "  replay m: " ++ show mReplay,
                        "  binding parent n: " ++ show (lookupBindParent (typeRef nReplay)),
                        "  binding parent m: " ++ show (lookupBindParent (typeRef mReplay)),
                        "  remaining operations: " ++ show rest,
                        "  trace: " ++ show traceInfo
                      ]
                  else
                    if not (isBinderNode' binderKeys nReplay)
                      then
                        Left $
                          PhiTranslatabilityError
                            [ "OpRaiseMerge: first target is non-binder node",
                              "  target node: " ++ show n,
                              "  canonical: " ++ show (canonicalNode nReplay),
                              "  trace: " ++ show traceInfo
                            ]
                      else do
                        case lookupBinderIndex' binderKeys (vSpineIds vs) nReplay of
                          Nothing ->
                            Left (PhiTranslatabilityError ["OpRaiseMerge: binder " ++ show n ++ " not found in quantifier spine"])
                          Just _ -> do
                            mRef <- mergeOtherBinderRef binderKeys vs m mReplay lookupBinder
                            let hAbs = InstSeq (InstInside (instAbstrWithRef mRef)) InstElim
                            (inst, vs') <- atBinderWith False binderKeys vs nReplay (pure hAbs)
                            go binderKeys namedSet' vs' (inst : accum) rest lookupBinder

    continueRaise ::
      IntSet.IntSet ->
      IntSet.IntSet ->
      VSpine ->
      [Instantiation] ->
      [InstanceOp] ->
      (NodeId -> Maybe String) ->
      NodeId ->
      NodeId ->
      NodeId ->
      NodeId ->
      NodeId ->
      Either ElabError Instantiation
    continueRaise
      binderKeys
      namedSet'
      vs
      accum
      rest
      lookupBinder
      nSource
      nAdopt
      nOrig
      nC
      nContextTarget = do
        let outsideInterior =
              not (IntSet.null interiorSet)
                && IntSet.null
                  ( IntSet.intersection interiorSet $
                      IntSet.fromList
                        [ getNodeId nSource,
                          getNodeId (canonicalNode nSource),
                          getNodeId nAdopt,
                          getNodeId nOrig,
                          getNodeId nC
                        ]
                  )
            allowOutsideAlias =
              isTraceBinderSource' nSource
                || maybe
                  False
                  ( \aliased ->
                      IntSet.member
                        (getNodeId (canonicalNode aliased))
                        interiorSet
                  )
                  (IntMap.lookup (getNodeId nSource) copyMap)
        if outsideInterior && not allowOutsideAlias
          then
            Left $
              PhiTranslatabilityError
                [ "OpRaise target outside I(r)",
                  "edge: " ++ show edgeLeft ++ " <= " ++ show edgeRight,
                  "op: OpRaise " ++ show nSource,
                  "nSource=" ++ show nSource ++ ", nAdopt=" ++ show nAdopt ++ ", nOrig=" ++ show nOrig ++ ", nC=" ++ show nC,
                  "interiorSet=" ++ show (IntSet.toList interiorSet)
                ]
          else -- Paper Fig. 10: Raise(n) introduces a fresh quantifier one level higher,
          -- bounds it by Txi(n), then aliases/eliminates the old binder.
          --
          -- For spine binders: use the existing logic
          -- For non-spine nodes: use binding edges + prec ordering to compute context

            let mbIndex = lookupBinderIndex' binderKeys (vSpineIds vs) nC
             in case debugPhi ("OpRaise: binderIndex=" ++ show mbIndex) mbIndex of
                  Just i -> do
                    -- Spine binder case
                    (boundRef, mbBound, _) <- vSpineBinderAt vs i
                    let refs = vSpineBinderRefs vs
                        inferredMap = inferredArgMap namedSet'
                        inferredBound =
                          Map.lookup (typeArgKeyForRef boundRef) inferredMap
                            <|> case Map.elems inferredMap of
                              [singleTy] -> Just singleTy
                              _ -> Nothing
                        boundTyRaw =
                          case mbBound of
                            Just bnd ->
                              let bTy = tyToElab bnd
                               in if alphaEqType bTy TBottom
                                    then fromMaybe bTy inferredBound
                                    else bTy
                            Nothing -> fromMaybe TBottom inferredBound
                        boundTy =
                          case (mbBound, boundTyRaw) of
                            (Nothing, TVarRef {}) -> boundTyRaw
                            (Nothing, _) -> inlineAliasBounds boundTyRaw
                            _ -> inlineAliasBounds boundTyRaw
                        deps = filter (not . typeBinderRefsSameIdentity boundRef) (freeTypeVarRefsList boundTy)
                        depIdxs = mapMaybe (\ref -> findIndex (typeBinderRefsSameIdentity ref) refs) deps
                        cutoff = if null depIdxs then (-1) else maximum depIdxs
                        insertIndex = cutoff + 1

                    when (insertIndex > i) $
                      Left (PhiInvariantError "OpRaise: computed insertion point is after binder")

                    let prefixBefore = take insertIndex refs
                        between = take (i - insertIndex) (drop insertIndex refs)
                        betaRef = typeBinderRefFromIdentity (typeBinderIdentityFromNode nC) "β"
                        hAbsBeta = InstSeq (InstInside (instAbstrWithRef betaRef)) InstElim
                        aliasOld = underContext between hAbsBeta

                        local =
                          instMany
                            [ InstIntro,
                              InstInside (InstBot boundTy),
                              instUnderWithRef betaRef aliasOld
                            ]

                        inst = underContext prefixBefore local

                    let raisedRef = typeBinderRefFromIdentity (typeBinderIdentityFromNode nC) (typeBinderRefName boundRef)
                        vsNoN = vsDeleteAt i vs
                        newBound = either (const Nothing) Just (elabToBound boundTy)
                        vs' = vsInsertAt insertIndex (raisedRef, newBound, Just nC) vsNoN
                    go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
                  Nothing -> do
                    -- Non-spine node case: select an insertion point `m = min-prec{...}` (Fig. 10)
                    -- using the edge-local prec ordering, then insert a fresh quantifier bounded
                    -- by `Txi(n)` at that point, and then alias/eliminate the original
                    -- (nested) binder for `n` inside the chosen `m`'s bound.
                    --
                    -- Paper Fig. 10:
                    --   Phi_xi(Raise(n)) = C^r_m { O; forall(>= Txi(n)); forall(beta_n >=) C^m_n {h!beta_n i} }
                    -- where `m = min-prec{...}`.

                    nodeTy0 <-
                      case lookupBindParent (typeRef nC) of
                        Just (TypeRef parent, _) ->
                          case lookupNodePV (canonicalNode parent) of
                            Just TyForall {} -> reifyTypeWithNamedSetRefsNoFallbackAt substForTypes namedSet' nC
                            _ -> reifyBoundType nC
                        _ -> reifyBoundType nC
                    let nodeTy = inlineAliasBounds nodeTy0
                    nodeTyBound <-
                      case lookupVarBound (canonicalNode nC) of
                        Just bnd -> reifyTypeWithNamedSetRefsNoFallbackAt substForTypes namedSet' bnd
                        Nothing -> pure nodeTy
                    let nodeTyBound' = inlineAliasBounds nodeTyBound

                    _ <- pure $ debugPhi ("OpRaise: nodeTy=" ++ show nodeTy) ()
                    _ <- pure $ debugPhi ("OpRaise: nodeTyBound=" ++ show nodeTyBound) ()
                    _ <- pure $ debugPhi ("OpRaise: inferredArgMap=" ++ show (inferredArgMap namedSet')) ()
                    _ <- pure $ debugPhi ("OpRaise: traceArgs=" ++ show (etBinderArgs traceInfo)) ()

                    let ids = vSpineIds vs
                        refs = vSpineBinderRefs vs

                    -- Compute dependency cutoff: the new binder must be inserted after any
                    -- binder that appears free in `Txi(n)`.
                    let deps = freeTypeVarRefsList nodeTy
                        depIdxs = mapMaybe (\ref -> findIndex (typeBinderRefsSameIdentity ref) refs) deps
                        cutoff = if null depIdxs then (-1) else maximum depIdxs
                        minIdx = min (cutoff + 1) (vSpineLength vs)

                        findCandidate :: [Int] -> Either ElabError (Maybe (Int, [ContextStep]))
                        findCandidate [] = Right Nothing
                        findCandidate (i : is) = do
                          spineNode <- vSpineIdAt vs i
                          case spineNode of
                            Nothing -> findCandidate is
                            Just mNode -> do
                              ctxOrErr <-
                                contextToNodeBoundWithOrderKeys
                                  canonicalNode
                                  orderKeys
                                  constraint
                                  (canonicalNode mNode)
                                  nContextTarget
                              case ctxOrErr of
                                Nothing -> findCandidate is
                                Just ctx' -> Right (Just (i, ctx'))

                    mbCandidate <- findCandidate [minIdx .. length ids - 1]
                    rootCtx <-
                      contextToNodeBoundWithOrderKeys
                        canonicalNode
                        orderKeys
                        constraint
                        (canonicalNode orderRoot)
                        nContextTarget
                    let boundTy = inlineAliasBounds nodeTy
                    let mbRootInst =
                          case (rootCtx, lookupBindParent (typeRef nC)) of
                            (Just _, Just (TypeRef parent, _)) ->
                              let parentC = canonicalNode parent
                                  rootC = canonicalNode orderRoot
                               in if parentC == rootC
                                    || case lookupNodePV parentC of
                                      Just TyForall {} -> True
                                      Just TyMu {} -> True
                                      _ -> False
                                    then
                                      let nodeTyBoundInlined = inlineBaseBounds nodeTyBound'
                                          numToDelete =
                                            case mSchemeInfo of
                                              Just si' ->
                                                case inferInstAppArgs (siScheme si') nodeTyBoundInlined of
                                                  Just args
                                                    | not (null args) -> length args
                                                  _ -> 1
                                              Nothing -> 1
                                          instArgInst =
                                            case mSchemeInfo of
                                              Just si' ->
                                                case inferInstAppArgs (siScheme si') nodeTyBoundInlined of
                                                  Just args
                                                    | not (null args) ->
                                                        instMany (map InstApp args)
                                                  _ -> InstApp nodeTyBoundInlined
                                              Nothing -> InstApp nodeTyBoundInlined
                                          prefixBefore = take minIdx refs
                                          inst = underContext prefixBefore instArgInst
                                       in Just (inst, numToDelete)
                                    else Nothing
                            _ -> Nothing
                    case mbCandidate of
                      Just (insertIdx, ctxMn) -> do
                        let prefixBefore = take insertIdx refs
                            betaRef = typeBinderRefFromIdentity (typeBinderIdentityFromNode nC) "β"
                            aliasOld = applyContext ctxMn InstElim

                            local =
                              instMany
                                [ InstIntro,
                                  InstInside (InstBot boundTy),
                                  instUnderWithRef betaRef aliasOld
                                ]

                            inst = underContext prefixBefore local

                        let newBound = either (const Nothing) Just (elabToBound boundTy)
                            vs' = vsInsertAt insertIdx (betaRef, newBound, Just nC) vs
                        go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
                      Nothing ->
                        case mbRootInst of
                          Just (inst, numToDelete) -> do
                            let vs' = foldl' (\v _ -> vsDeleteAt minIdx v) vs [1 .. numToDelete]
                            go binderKeys namedSet' vs' (inst : accum) rest lookupBinder
                          Nothing ->
                            Left $
                              PhiTranslatabilityError
                                [ "OpRaise (non-spine): missing computation context",
                                  "  target node: " ++ show nOrig,
                                  "  canonical target: " ++ show nC,
                                  "  context target: " ++ show nContextTarget,
                                  "  orderRoot: " ++ show orderRoot,
                                  "  edgeRoot: " ++ show edgeRoot,
                                  "  minIdx: " ++ show minIdx,
                                  "  deps(Txi(n)): " ++ show deps,
                                  "  nodeTy: " ++ show nodeTy,
                                  "  ids: " ++ show ids,
                                  "  bindParent: " ++ show (lookupBindParent (typeRef nC))
                                ]

    idsForStartType :: SchemeInfo -> ElabType -> [Maybe NodeId]
    idsForStartType si' ty =
      let identityToId =
            Map.fromList
              [ (typeArgKeyForRef ref, NodeId key)
                | (key, ref) <- IntMap.toList (schemeInfoBinderRefSubst si')
              ]
          (qs, _) = splitForallsRefs ty
       in [ case typeBinderRefNode ref of
              Just nid -> Just nid
              Nothing ->
                Map.lookup (typeArgKeyForRef ref) identityToId
            | (ref, _) <- qs
          ]

    binderRefFor :: IntSet.IntSet -> VSpine -> NodeId -> (NodeId -> Maybe String) -> Either ElabError TypeBinderRef
    binderRefFor binderKeys vs nid lookupBinder =
      case lookupBinderIndex' binderKeys (vSpineIds vs) nid of
        Just i ->
          case drop i (vSpineBinderRefs vs) of
            ref : _ -> Right ref
            [] -> Left (PhiInvariantError "binderRefFor: index out of range")
        Nothing ->
          let name = fromMaybe ("t" ++ show (getNodeId nid)) (lookupBinder nid)
           in Right (typeBinderRefFromIdentity (typeBinderIdentityFromNode nid) name)

    -- Figure 15.3.4 abstracts over alpha_n' for Merge/RaiseMerge.  n' belongs
    -- to the edge's frozen source domain and need not be a quantifier in the
    -- expansion's current spine.  The validated witness is the construction-
    -- time certificate for that identity: source TyVars can disappear from the
    -- finalized graph after the very merge represented by this operation.
    mergeOtherBinderRef ::
      IntSet.IntSet ->
      VSpine ->
      NodeId ->
      NodeId ->
      (NodeId -> Maybe String) ->
      Either ElabError TypeBinderRef
    mergeOtherBinderRef binderKeys vs source replay lookupBinder
      | isBinderNode' binderKeys replay =
          binderRefFor binderKeys vs replay lookupBinder
      | Just ref <- schemeRefForTraceBinder source = Right ref
      | Just ref <- schemeRefForTraceBinder replay = Right ref
      | otherwise =
          let name = fromMaybe ("t" ++ show (getNodeId source)) (lookupBinder source)
           in Right (typeBinderRefFromIdentity (typeBinderIdentityFromNode source) name)

    atBinderWith ::
      Bool ->
      IntSet.IntSet ->
      VSpine ->
      NodeId ->
      Either ElabError Instantiation ->
      Either ElabError (Instantiation, VSpine)
    atBinderWith keep binderKeys vs nid mkInner = do
      i <- binderIndex binderKeys (vSpineIds vs) nid
      prefix <- prefixBinderRefs vs i
      inner <- mkInner
      let vs' = if keep then vs else vsDeleteAt i vs
      pure (underContext prefix inner, vs')

    -- \| Check if a node is bound rigidly. Some ω operations treat rigid targets as
    -- ε/identity (thesis Fig. 15.3.4), but not all (see the OpGraft/OpWeaken note).
    isRigidNode :: NodeId -> Bool
    isRigidNode nid =
      case lookupBindParent (typeRef (canonicalNode nid)) of
        Just (_, BindRigid) -> True
        _ -> False

    binderIndex :: IntSet.IntSet -> [Maybe NodeId] -> NodeId -> Either ElabError Int
    binderIndex binderKeys ids nid =
      case lookupBinderIndex' binderKeys ids nid of
        Just i -> Right i
        Nothing ->
          Left $
            PhiInvariantError $
              "binder " ++ show nid ++ " not found in identity list " ++ show ids

    prefixBinderRefs :: VSpine -> Int -> Either ElabError [TypeBinderRef]
    prefixBinderRefs vs i
      | i < 0 || i > length refs =
          Left (PhiInvariantError "prefixBinderRefs: index out of range")
      | otherwise = Right (take i refs)
      where
        refs = vSpineBinderRefs vs

    underContext :: [TypeBinderRef] -> Instantiation -> Instantiation
    underContext prefix inner = foldr instUnderWithRef inner prefix
