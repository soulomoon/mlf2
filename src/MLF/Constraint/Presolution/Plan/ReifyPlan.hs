{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module MLF.Constraint.Presolution.Plan.ReifyPlan
  ( InheritedGammaPlan,
    inheritedGammaPlanLiveRoutes,
    inheritedGammaPlanBaseRoutes,
    inheritedGammaPlanAuthorizedRefs,
    inheritedGammaPlanRoutes,
    InheritedGammaRoute,
    inheritedGammaRouteLiveNode,
    inheritedGammaRouteBaseNode,
    inheritedGammaRouteRef,
    InheritedGammaRoutes,
    inheritedGammaRoutesEntries,
    inheritedGammaRoutesLexicalRefs,
    emptyInheritedGammaRoutes,
    inheritedGammaRoutesFromLexicalRefs,
    mergeInheritedGammaRoutes,
    mapInheritedGammaRouteRefs,
    ReifyPlan (..),
    ReifyRootChoice,
    rrcSource,
    rrcSubst,
    SchemeTypeChoice (..),
    ReifyPlanInput (..),
    buildReifyPlan,
    canonicalizeSubstRefs,
    certifiedFromBaseAliasRoute,
    mergeReifySubstRefs,
    ReifyBindingEnv (..),
    bindingFor,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.BindingUtil (bindingScopeFor)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Plan.BinderPlan (AliasEnv (..), GaBindParentsInfo (..), bindingScopeGen, boundMentionsSelfAliasFor, hasExplicitBoundFor, isTargetSchemeBinderFor)
import MLF.Constraint.Presolution.Plan.Requirements (RequiredGammaBinder (..))
import MLF.Constraint.Presolution.Plan.Normalize (containsForall)
import qualified MLF.Constraint.Presolution.Plan.SchemeRoots as SchemeRoots
import MLF.Constraint.Presolution.Plan.Target.TypeRootPlan (ReifyRootSource (..))
import MLF.Constraint.Presolution.View (PresolutionView)
import MLF.Constraint.Types.Graph
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Reify.Core
  ( reifyBoundWithExternalRefs,
    reifyBoundWithExternalRefsOnConstraint
  )
import qualified MLF.Reify.Core as ReifyCore
import MLF.Reify.TypeOps (freeTypeVarRefsType)
import MLF.Types.Elab
  ( BoundType,
    ElabType,
    TypeBinderRef,
    Ty (..),
    elabToBound,
    typeBinderIdentityFromNode,
    typeBinderRefFromIdentity,
    typeBinderRefIdentity,
    typeBinderRefName,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
  )
import MLF.Types.Identity
  ( typeBinderIdentityGeneratedUnique,
    typeBinderIdentityStructural,
  )
import MLF.Util.ElabError (ElabError (..), bindingToElab)
import MLF.Util.Graph (reachableFromStop)
import MLF.Util.Names (alphaName)

-- | Exact authority for declarations that a reified inner scheme may leave
-- free because an enclosing lexical Gamma or structural source constructor
-- already owns them.  This includes routed source/rigid declarations and
-- reachable flexible declarations whose original owner is a strict ancestor
-- of the current gen.  The constructor stays private: finalization can only
-- consume authority proved while the reification plan still has the original
-- binding tree, the selected reify root, and live/base provenance.
data InheritedGammaPlan = InheritedGammaPlan
  { inheritedGammaPlanLiveRoutes :: IntMap.IntMap TypeBinderRef,
    inheritedGammaPlanBaseRoutes :: IntMap.IntMap TypeBinderRef,
    inheritedGammaPlanAuthorizedRefs :: [TypeBinderRef],
    inheritedGammaPlanRoutes :: InheritedGammaRoutes
  }

-- | A frozen proof that one inherited live declaration is the solved image
-- of one exact base declaration.  Rigid routes additionally prove the
-- unbounded shape needed by rigid inlining; outer-flex routes instead prove
-- strict lexical ancestry.  The reified ref is deliberately retained as a
-- third component: later construction may route the base node to a distinct
-- ambient identity, but it may do so only by joining against this exact
-- live/base provenance.
data InheritedGammaRoute = InheritedGammaRoute
  { inheritedGammaRouteLiveNode :: !NodeId,
    inheritedGammaRouteBaseNode :: !NodeId,
    inheritedGammaRouteRef :: !TypeBinderRef
  }
  deriving (Eq, Show)

-- | Construction-facing inherited Gamma capability.  Frozen live/base routes
-- support graph-domain transport.  Lexical refs cover declarations whose
-- strict-ancestor proof is available only in the selected live binding tree;
-- they may be consumed as ambient dependencies, never as base aliases.
data InheritedGammaRoutes =
  InheritedGammaRoutes
    [InheritedGammaRoute]
    [TypeBinderRef]
  deriving (Eq, Show)

inheritedGammaRoutesEntries :: InheritedGammaRoutes -> [InheritedGammaRoute]
inheritedGammaRoutesEntries (InheritedGammaRoutes routes _) = routes

inheritedGammaRoutesLexicalRefs :: InheritedGammaRoutes -> [TypeBinderRef]
inheritedGammaRoutesLexicalRefs (InheritedGammaRoutes _ refs) = refs

emptyInheritedGammaRoutes :: InheritedGammaRoutes
emptyInheritedGammaRoutes = InheritedGammaRoutes [] []

-- | Retain exact lexical Gamma authority when no graph route is needed.  This
-- is used for source-owned enclosing binders: their semantic identity is the
-- capability, while manufacturing a graph alias would lose that ownership.
inheritedGammaRoutesFromLexicalRefs
  :: [TypeBinderRef]
  -> InheritedGammaRoutes
inheritedGammaRoutesFromLexicalRefs = InheritedGammaRoutes []

mergeInheritedGammaRoutes
  :: InheritedGammaRoutes
  -> InheritedGammaRoutes
  -> Either ElabError InheritedGammaRoutes
mergeInheritedGammaRoutes
  (InheritedGammaRoutes left leftRefs)
  (InheritedGammaRoutes right rightRefs) =
    InheritedGammaRoutes
      <$> go left right
      <*> pure (foldr insertRef leftRefs rightRefs)
  where
    insertRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    go routes [] = pure routes
    go routes (route : rest) =
      case
          filter
            ((== inheritedGammaRouteLiveNode route) . inheritedGammaRouteLiveNode)
            routes
        of
          [] -> go (routes ++ [route]) rest
          [existing]
            | inheritedGammaRouteBaseNode existing
                == inheritedGammaRouteBaseNode route
            , typeBinderRefsSameIdentity
                (inheritedGammaRouteRef existing)
                (inheritedGammaRouteRef route) ->
                go routes rest
            | otherwise ->
                Left
                  ( ValidationFailed
                      [ "one inherited Gamma live node has conflicting frozen base routes",
                        "  live node: " ++ show (inheritedGammaRouteLiveNode route),
                        "  first route: " ++ show existing,
                        "  second route: " ++ show route
                      ]
                  )
          existing ->
            Left
              ( ValidationFailed
                  [ "inherited Gamma route set contains duplicate live authorities",
                    "  live node: " ++ show (inheritedGammaRouteLiveNode route),
                    "  routes: " ++ show existing
                  ]
              )

mapInheritedGammaRouteRefs
  :: (TypeBinderRef -> TypeBinderRef)
  -> InheritedGammaRoutes
  -> InheritedGammaRoutes
mapInheritedGammaRouteRefs renameRef (InheritedGammaRoutes routes lexicalRefs) =
  InheritedGammaRoutes
    [ route
        { inheritedGammaRouteRef =
            renameRef (inheritedGammaRouteRef route)
        }
    | route <- routes
    ]
    (map renameRef lexicalRefs)

data ReifyPlan = ReifyPlan
  { rpSubst :: IntMap.IntMap TypeBinderRef,
    rpSubstForBound :: Int -> IntMap.IntMap TypeBinderRef,
    rpSubstForBoundBase :: Int -> IntMap.IntMap TypeBinderRef,
    rpRootChoice :: ReifyRootChoice,
    rpSchemeTypeChoice :: SchemeTypeChoice,
    rpBindingScopeGen :: NodeId -> Maybe GenNodeId,
    rpHasExplicitBound :: NodeId -> Bool,
    rpIsTargetSchemeBinder :: NodeId -> Bool,
    rpBoundMentionsSelfAlias :: NodeId -> Bool,
    rpContainsForall :: ElabType -> Bool
    , rpRequiredGamma :: IntMap.IntMap RequiredGammaBinder
    , rpSourceBinderRefs :: IntMap.IntMap TypeBinderRef
    , rpSourceDeclarationsBeforeRequiredGamma :: IntSet.IntSet
    , rpExternalSourceBinderKeys :: IntSet.IntSet
    , rpExternalSourceBinderBaseKeys :: IntSet.IntSet
    , rpStructuralSourceBinders :: IntMap.IntMap [NodeId]
    , rpStructuralSourceBaseBinders :: IntMap.IntMap [NodeId]
    , rpInheritedGammaPlan :: InheritedGammaPlan
  }

data ReifyRootChoice = ReifyRootChoice
  { rrcSource :: ReifyRootSource,
    rrcSubst :: IntMap.IntMap TypeBinderRef
  }

data SchemeTypeChoice = SchemeTypeChoice
  { stcUseSchemeType :: Bool,
    stcSchemeOwnerFromBody :: Maybe GenNodeId,
    stcSchemeOwnerFromBodyIsAlias :: Bool,
    stcSchemeOwners :: [GenNodeId]
  }

data ReifyPlanInput p = ReifyPlanInput
  { rpiConstraint :: Constraint p,
    rpiNodes :: IntMap.IntMap TyNode,
    rpiCanonical :: NodeId -> NodeId,
    rpiBindParents :: BindParents,
    -- | Canonical binding parents before W-softening.  Lexical inherited-Gamma
    -- authority is proved here so a softened sibling cannot appear to be an
    -- ancestor after solving.
    rpiRigidBindParents :: BindParents,
    rpiScopeRootC :: NodeRef,
    rpiScopeGen :: Maybe GenNodeId,
    rpiSchemeRootsPlan :: SchemeRoots.SchemeRootsPlan,
    rpiTarget0 :: NodeId,
    rpiTargetIsBaseLike :: Bool,
    rpiTargetBound :: Maybe NodeId,
    rpiReachableFromWithBounds :: NodeId -> IntSet.IntSet,
    rpiBindParentsGa :: Maybe (GaBindParentsInfo p),
    rpiExtraNameStart :: Int,
    rpiOrderedExtra :: [Int],
    -- | Exact declarations selected by BinderPlan after semantic-identity
    -- quotienting.  Required-Gamma aliases may reuse one of these declarations
    -- but must not recreate a separate exterior identity.
    rpiOrderedBinderRefs :: IntMap.IntMap TypeBinderRef,
    rpiSubst0 :: IntMap.IntMap TypeBinderRef,
    rpiInheritedRigidAliasRoutes :: IntMap.IntMap TypeBinderRef,
    rpiGammaAlias :: IntMap.IntMap Int,
    rpiNamedUnderGaSet :: IntSet.IntSet,
    rpiNestedSchemeInteriorSet :: IntSet.IntSet,
    rpiBaseGammaRep :: IntMap.IntMap Int,
    rpiAliasBinderBases :: IntSet.IntSet,
    rpiSolvedToBasePref :: IntMap.IntMap NodeId,
    rpiTypeRoot :: NodeId,
    rpiReifyRootSource :: ReifyRootSource
    , rpiRequiredGamma :: IntMap.IntMap RequiredGammaBinder
    , rpiLocallyClosedGammaKeys :: IntSet.IntSet
    , rpiSourceBinderRefs :: IntMap.IntMap TypeBinderRef
    , rpiSourceDeclarationsBeforeRequiredGamma :: IntSet.IntSet
  }

data ReifyBindingEnv p = ReifyBindingEnv
  { rbeConstraint :: Constraint p,
    rbeNodes :: IntMap.IntMap TyNode,
    rbeCanonical :: NodeId -> NodeId,
    -- Canonical binding parents before W-softening.  The ordinary parent map
    -- below remains the construction view used for binder planning; this map
    -- is only the retained proof that a node was rigid in the input graph.
    rbeRigidBindParents :: BindParents,
    rbeBindParents :: BindParents,
    rbeScopeGen :: Maybe GenNodeId,
    rbeSchemeRootOwner :: IntMap.IntMap GenNodeId,
    rbeSchemeRootByBody :: IntMap.IntMap NodeId,
    rbeSchemeRootByBodyBase :: IntMap.IntMap NodeId,
    rbeSchemeRootKeySet :: IntSet.IntSet,
    rbeGammaAlias :: IntMap.IntMap Int,
    rbeAliasBinderBases :: IntSet.IntSet,
    rbeSolvedToBasePref :: IntMap.IntMap NodeId,
    rbeNamedUnderGaSet :: IntSet.IntSet,
    rbeBinderSet :: IntSet.IntSet,
    rbeUniqueUnboundedName :: Maybe String,
    rbeResForReify :: PresolutionView p,
    rbeBindParentsGa :: Maybe (GaBindParentsInfo p),
    rbeBindingScopeGen :: NodeId -> Maybe GenNodeId,
    rbeHasExplicitBound :: NodeId -> Bool,
    rbeIsTargetSchemeBinder :: NodeId -> Bool,
    rbeBoundMentionsSelfAlias :: NodeId -> Bool,
    rbeContainsForall :: ElabType -> Bool,
    rbeFirstGenAncestor :: NodeRef -> Maybe GenNodeId,
    rbeTraceM :: String -> Either ElabError ()
  }

canonicalizeSubstRefs :: (NodeId -> NodeId) -> IntMap.IntMap TypeBinderRef -> IntMap.IntMap TypeBinderRef
canonicalizeSubstRefs canonical subst =
  IntMap.fromListWith keepExisting
    [ (routeKey key ref, ref)
      | (key, ref) <- IntMap.toList subst
    ]
  where
    keepExisting _ existing = existing
    -- A graph identity is the declaration published at its exact
    -- construction key.  Canonical equality can make another node denote the
    -- same solved type, but it is not authority to republish that lexical
    -- identity at the representative.  Generated identities carry the
    -- source-level quotient explicitly; structural identities are admitted
    -- separately only after their owner proof is built below.
    routeKey key ref =
      case
          typeBinderIdentityGeneratedUnique
            (typeBinderRefIdentity ref)
        of
          Just _ -> getNodeId (canonical (NodeId key))
          Nothing -> key

-- | Assemble the substitution consumed by S' in authority order.  Exact
-- required-Gamma and direct graph routes win; structural and generated source
-- routes may then fill unowned keys, followed by the ordinary BinderPlan
-- substitution.
mergeReifySubstRefs
  :: IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
mergeReifySubstRefs required graph structural source planned =
  IntMap.unions
    [ required
    , graph
    , structural
    , source
    , planned
    ]

-- | Recover a solved construction route from the frozen base Gamma only when
-- both provenance maps name the same binder representative.  This conjunction
-- is stronger authority than identity-class expansion: it is valid for an
-- exact graph declaration as well as a generated source identity.
certifiedFromBaseAliasRoute
  :: IntMap.IntMap Int
  -> IntMap.IntMap Int
  -> IntMap.IntMap TypeBinderRef
  -> (Int, NodeId)
  -> Maybe (Int, TypeBinderRef)
certifiedFromBaseAliasRoute baseGammaRep gammaAlias substBase (solvedKey, baseNode) = do
  repKey <- IntMap.lookup (getNodeId baseNode) baseGammaRep
  aliasRepKey <- IntMap.lookup solvedKey gammaAlias
  ref <- IntMap.lookup repKey substBase
  if aliasRepKey == repKey && solvedKey /= repKey
    then Just (solvedKey, ref)
    else Nothing

buildReifyPlan :: ReifyPlanInput p -> Either ElabError ReifyPlan
buildReifyPlan ReifyPlanInput {..} =
  let isIdentityNode node =
        case node of
          TyVar {} -> True
          TyBottom {} -> True
          _ -> False
      sourceIdentityMayExpandSolvedClass ref =
        isJust
          ( typeBinderIdentityGeneratedUnique
              (typeBinderRefIdentity ref)
          )
      sourceLiveRefsDirect =
        IntMap.fromListWith (\_ existing -> existing)
          [ (liveKey, ref)
          | (sourceKey, ref) <- IntMap.toList rpiSourceBinderRefs,
            isJust (typeBinderIdentityGeneratedUnique (typeBinderRefIdentity ref)),
            let liveKey = getNodeId (rpiCanonical (NodeId sourceKey)),
            Just node <- [IntMap.lookup liveKey rpiNodes],
            isIdentityNode node
          ]
      sourceLiveRefsFromBase =
        IntMap.fromListWith (\_ existing -> existing)
          [ (liveKey, ref)
          | (liveKey0, baseNode) <- IntMap.toList rpiSolvedToBasePref
          , let liveKey = getNodeId (rpiCanonical (NodeId liveKey0))
          , Just ref <- [IntMap.lookup (getNodeId baseNode) rpiSourceBinderRefs]
          , isJust (typeBinderIdentityGeneratedUnique (typeBinderRefIdentity ref))
          , Just node <- [IntMap.lookup liveKey rpiNodes]
          , isIdentityNode node
          ]
      sourceLiveRefsLocal =
        IntMap.union sourceLiveRefsDirect sourceLiveRefsFromBase
      structuralLiveRefCandidatesDirect =
        IntMap.fromListWith (\_ existing -> existing)
          [ (liveKey, ref)
          | (sourceKey, ref) <- IntMap.toList rpiSourceBinderRefs,
            isJust
              ( typeBinderIdentityStructural
                  (typeBinderRefIdentity ref)
              ),
            let liveKey = getNodeId (rpiCanonical (NodeId sourceKey)),
            Just node <- [IntMap.lookup liveKey rpiNodes],
            isIdentityNode node
          ]
      structuralLiveRefCandidatesFromBase =
        IntMap.fromListWith (\_ existing -> existing)
          [ (liveKey, ref)
          | (liveKey0, baseNode) <- IntMap.toList rpiSolvedToBasePref,
            let liveKey = getNodeId (rpiCanonical (NodeId liveKey0)),
            Just ref <- [IntMap.lookup (getNodeId baseNode) rpiSourceBinderRefs],
            isJust
              ( typeBinderIdentityStructural
                  (typeBinderRefIdentity ref)
              ),
            Just node <- [IntMap.lookup liveKey rpiNodes],
            isIdentityNode node
          ]
      structuralLiveRefCandidates =
        IntMap.union
          structuralLiveRefCandidatesDirect
          structuralLiveRefCandidatesFromBase
      structuralLiveRefsLocal =
        IntMap.filterWithKey
          ( \liveKey _ ->
              IntSet.member
                liveKey
                structuralLiveSourceBinderKeysLocal
          )
          structuralLiveRefCandidates
      graphLiveRefsDirect =
        IntMap.fromListWith (\_ existing -> existing)
          [ (sourceKey, ref)
          | (sourceKey, ref) <- IntMap.toList rpiSourceBinderRefs
          , isJust (typeBinderRefNode ref)
          , Just node <- [IntMap.lookup sourceKey rpiNodes]
          , isIdentityNode node
          ]
      sourceBaseRefsLocal =
        case rpiBindParentsGa of
          Just ga ->
            IntMap.filterWithKey
              (\baseKey ref ->
                isJust
                  ( typeBinderIdentityGeneratedUnique
                      (typeBinderRefIdentity ref)
                  )
                  && case lookupNodeIn (cNodes (gbiBaseConstraint ga)) (NodeId baseKey) of
                    Just node -> isIdentityNode node
                    Nothing -> False
              )
              rpiSourceBinderRefs
          Nothing -> IntMap.empty
      structuralBaseRefsLocal =
        case rpiBindParentsGa of
          Just ga ->
            IntMap.filterWithKey
              (\baseKey ref ->
                isJust
                  ( typeBinderIdentityStructural
                      (typeBinderRefIdentity ref)
                  )
                  && IntSet.member
                    baseKey
                    structuralBaseSourceBinderKeysLocal
                  && case lookupNodeIn (cNodes (gbiBaseConstraint ga)) (NodeId baseKey) of
                    Just node -> isIdentityNode node
                    Nothing -> False
              )
              rpiSourceBinderRefs
          Nothing -> IntMap.empty
      locallyClosedBaseKeysLocal =
        case rpiBindParentsGa of
          Just ga ->
            IntSet.fromList
              ( [ baseKey
                | (baseKey, solvedNode) <- IntMap.toList (gbiBaseToSolved ga)
                , IntSet.member
                    (getNodeId (rpiCanonical solvedNode))
                    rpiLocallyClosedGammaKeys
                ]
                  ++ [ getNodeId baseNode
                     | (solvedKey, baseNode) <- IntMap.toList rpiSolvedToBasePref
                     , IntSet.member
                         (getNodeId (rpiCanonical (NodeId solvedKey)))
                         rpiLocallyClosedGammaKeys
                     ]
              )
          Nothing -> IntSet.empty
      isLocallyClosedLiveKey key =
        IntSet.member
          (getNodeId (rpiCanonical (NodeId key)))
          rpiLocallyClosedGammaKeys
      filterLocallyClosedLiveRefs =
        IntMap.filterWithKey (\key _ -> not (isLocallyClosedLiveKey key))
      filterLocallyClosedBaseRefs =
        IntMap.filterWithKey
          (\key _ -> not (IntSet.member key locallyClosedBaseKeysLocal))
      sourceRefsByCanonicalLive =
        IntMap.fromListWith (\_ existing -> existing)
          [ (getNodeId (rpiCanonical (NodeId sourceKey)), ref)
          | (sourceKey, ref) <- IntMap.toList rpiSourceBinderRefs
          ]
      isStructuralOwner node =
        case node of
          TyForall {} -> True
          TyMu {} -> True
          _ -> False
      structuralLiveSourceBindersDirectLocal =
        IntMap.fromListWith (++)
          ( [ (getNodeId parent, [NodeId childKey])
            | (childKey, childRef) <- IntMap.toList structuralLiveRefCandidates
            , Just (TypeRef parent0, _) <-
                [IntMap.lookup (nodeRefKey (typeRef (NodeId childKey))) rpiBindParents]
            , let parent = rpiCanonical parent0
            , Just owner <- [IntMap.lookup (getNodeId parent) rpiNodes]
            , isStructuralOwner owner
            , Just ownerRef <- [IntMap.lookup (getNodeId parent) sourceRefsByCanonicalLive]
            , typeBinderRefsSameIdentity childRef ownerRef
            ]
              ++ [ (getNodeId (rpiCanonical parent0), [NodeId liveKey])
                 | (sourceKey, childRef) <- IntMap.toList rpiSourceBinderRefs
                 , Just child <- [IntMap.lookup sourceKey rpiNodes]
                 , isIdentityNode child
                 , let liveKey = getNodeId (rpiCanonical (NodeId sourceKey))
                 , Just (TypeRef parent0, _) <-
                     [IntMap.lookup (nodeRefKey (typeRef (NodeId sourceKey))) rpiBindParents]
                 , let parentKey = getNodeId parent0
                 , Just owner <-
                     [ case IntMap.lookup parentKey rpiNodes of
                         Just node -> Just node
                         Nothing -> IntMap.lookup (getNodeId (rpiCanonical parent0)) rpiNodes
                     ]
                 , isStructuralOwner owner
                 , Just ownerRef <-
                     [ case IntMap.lookup parentKey rpiSourceBinderRefs of
                         Just ref -> Just ref
                         Nothing -> IntMap.lookup (getNodeId (rpiCanonical parent0)) sourceRefsByCanonicalLive
                     ]
                 , typeBinderRefsSameIdentity childRef ownerRef
                 ]
          )
      structuralSourceBaseBindersLocal =
        case rpiBindParentsGa of
          Nothing -> IntMap.empty
          Just ga ->
            ReifyCore.structuralBinders
              (gbiBaseConstraint ga)
              (gbiBindParentsBase ga)
              rpiSourceBinderRefs
      structuralLiveSourceBindersFromBaseLocal =
        case rpiBindParentsGa of
          Nothing -> IntMap.empty
          Just ga ->
            IntMap.fromListWith (++)
              [ ( getNodeId (rpiCanonical solvedOwner)
                , [rpiCanonical solvedChild]
                )
              | (baseOwnerKey, baseChildren) <- IntMap.toList structuralSourceBaseBindersLocal
              , Just solvedOwner <- [IntMap.lookup baseOwnerKey (gbiBaseToSolved ga)]
              , Just solvedOwnerNode <-
                  [IntMap.lookup (getNodeId (rpiCanonical solvedOwner)) rpiNodes]
              , isStructuralOwner solvedOwnerNode
              , baseChild <- baseChildren
              , Just solvedChild <- [IntMap.lookup (getNodeId baseChild) (gbiBaseToSolved ga)]
              ]
      structuralSourceBindersLocal =
        IntMap.mapWithKey selectStructuralChildren $
          IntMap.unionWith
            (++)
            structuralLiveSourceBindersDirectLocal
            structuralLiveSourceBindersFromBaseLocal
      -- Several base owners can collapse to one live structural owner.  A
      -- copied child that is bounded by another certified child is then an
      -- occurrence proxy, not a second declaration at that owner.  When the
      -- live tree still has a direct structural child, it is the declaration;
      -- base-projected siblings are retained only when solving reparented the
      -- direct child away from its owner.
      selectStructuralChildren ownerKey children =
        case directChildren of
          [] -> certifiedChildren
          _ -> directChildren
        where
          childKeys =
            IntSet.fromList
              [ getNodeId (rpiCanonical child)
              | child <- children
              ]
          certifiedChildren =
            [ NodeId childKey
            | childKey <- IntSet.toList childKeys
            , not (isAliasOfStructuralSibling childKey)
            ]
          directChildren =
            [ child
            | child <- certifiedChildren
            , Just (TypeRef parent, _) <-
                [IntMap.lookup (nodeRefKey (typeRef child)) rpiBindParents]
            , getNodeId (rpiCanonical parent) == ownerKey
            ]
          isAliasOfStructuralSibling childKey =
            case VarStore.lookupVarBound rpiConstraint (NodeId childKey) of
              Just bound ->
                let boundKey = getNodeId (rpiCanonical bound)
                 in boundKey /= childKey && IntSet.member boundKey childKeys
              Nothing -> False
      structuralLiveSourceBinderKeysLocal =
        IntSet.fromList
          [ getNodeId child
          | children <- IntMap.elems structuralSourceBindersLocal
          , child <- children
          ]
      structuralBaseSourceBinderKeysLocal =
        IntSet.fromList
          [ getNodeId child
          | children <- IntMap.elems structuralSourceBaseBindersLocal
          , child <- children
          ]
      hasBaseOrigin key node =
        case rpiBindParentsGa of
          Nothing -> True
          Just ga ->
            IntMap.member key (gbiSolvedToBase ga)
              || isJust (lookupNodeIn (cNodes (gbiBaseConstraint ga)) node)
      inheritedRigidLiveCandidatesLocal =
        let reachableRigidKeys =
              IntSet.union
                (rpiReachableFromWithBounds rpiTypeRoot)
                requiredGammaDependencyKeys
         in [ ( key,
                node,
                typeBinderRefFromIdentity
                  (typeBinderIdentityFromNode node)
                  ("__rigid" ++ show key)
              )
            | key <- IntSet.toList reachableRigidKeys
            , let node = NodeId key
            , Just TyVar {} <- [IntMap.lookup key rpiNodes]
            , isNothing (VarStore.lookupVarBound rpiConstraint node)
            , hasInheritedRigidAuthority node
            -- An exact generated source declaration is already the route for
            -- this occurrence.  Do not manufacture an inherited graph
            -- declaration for the same live key.
            , IntMap.notMember key sourceLiveRefsLocal
            , not (IntSet.member key structuralLiveSourceBinderKeysLocal)
                || IntSet.member key requiredGammaDependencyKeys
            , hasBaseOrigin key node
            ]
      -- With a frozen graph, a live rigid reference is inherited Gamma only
      -- when its exact base origin is itself an unbounded variable.  Build the
      -- live/base pair from that single proof so a bounded base variable can
      -- never first enter the capability and fail validation later.
      inheritedRigidRouteCandidatesLocal =
        case rpiBindParentsGa of
          Nothing -> []
          Just ga ->
            [ (liveKey, baseNode, ref)
            | (liveKey, liveNode, ref) <- inheritedRigidLiveCandidatesLocal
            , Just baseNode <- [frozenBaseNodeForLive ga liveNode]
            , Just TyVar {} <- [lookupNodeIn (cNodes (gbiBaseConstraint ga)) baseNode]
            , isNothing (VarStore.lookupVarBound (gbiBaseConstraint ga) baseNode)
            , IntMap.notMember (getNodeId baseNode) sourceBaseRefsLocal
            ]
      inheritedOuterFlexRouteCandidatesLocal =
        case (rpiScopeGen, rpiBindParentsGa) of
          (Just currentGen, Just ga) ->
            [ (liveKey, baseNode, graphRef liveNode)
            | liveKey <-
                IntSet.toList
                  (rpiReachableFromWithBounds rpiTypeRoot)
            , let liveNode = rpiCanonical (NodeId liveKey)
            , Just TyVar {} <-
                [IntMap.lookup (getNodeId liveNode) rpiNodes]
            , not (isLocallyClosedLiveKey (getNodeId liveNode))
            , Just (GenRef liveOwner, BindFlex) <-
                [ IntMap.lookup
                    (nodeRefKey (typeRef liveNode))
                    rpiRigidBindParents
                ]
            , strictEnclosingGen
                rpiRigidBindParents
                currentGen
                liveOwner
            -- Source and structural routes already carry their own lexical
            -- declaration authority.  This certificate is only for the
            -- otherwise-unrouted graph declaration.
            , IntMap.notMember (getNodeId liveNode) sourceLiveRefsLocal
            , not
                ( IntSet.member
                    (getNodeId liveNode)
                    structuralLiveSourceBinderKeysLocal
                )
            , Just baseNode <- [frozenBaseNodeForLive ga liveNode]
            , Just TyVar {} <-
                [lookupNodeIn (cNodes (gbiBaseConstraint ga)) baseNode]
            , Just (GenRef baseOwner, BindFlex) <-
                [ IntMap.lookup
                    (nodeRefKey (typeRef baseNode))
                    (gbiBindParentsBase ga)
                ]
            , strictEnclosingGen
                (gbiBindParentsBase ga)
                currentGen
                baseOwner
            , IntMap.notMember (getNodeId baseNode) sourceBaseRefsLocal
            ]
          _ -> []
      inheritedGraphRouteCandidatesLocal =
        inheritedRigidRouteCandidatesLocal
          ++ inheritedOuterFlexRouteCandidatesLocal
      inheritedGraphLiveRoutesLocal =
        case rpiBindParentsGa of
          Nothing ->
            IntMap.fromList
              [ (key, ref)
              | (key, _, ref) <- inheritedRigidLiveCandidatesLocal
              ]
          Just _ ->
            IntMap.fromList
              [ (liveKey, ref)
              | (liveKey, _, ref) <- inheritedGraphRouteCandidatesLocal
              ]
      requiredGammaDependencyKeys =
        IntSet.fromList
          [ getNodeId node
          | requirement <- IntMap.elems rpiRequiredGamma
          , dependency <- freeTypeVarRefsType (rgbOperatedType requirement)
          , Just node <- [typeBinderRefNode dependency]
          ]
      inheritedGraphBaseRoutesLocal =
        case rpiBindParentsGa of
          Nothing -> IntMap.empty
          Just _ ->
            IntMap.fromList
              [ (getNodeId baseNode, ref)
              | (_, baseNode, ref) <- inheritedGraphRouteCandidatesLocal
              ]
      inheritedOuterFlexBaseRoutesLocal =
        IntMap.fromList
          [ (getNodeId baseNode, ref)
          | (_, baseNode, ref) <-
              inheritedOuterFlexRouteCandidatesLocal
          ]
      inheritedRigidAliasRouteErrors =
        concatMap
          validateInheritedRigidAliasRoute
          (IntMap.toList rpiInheritedRigidAliasRoutes)
      validateInheritedRigidAliasRoute (sourceKey, plannedTargetRef) =
        case typeBinderRefNode plannedTargetRef of
          Nothing ->
            [ "inherited-rigid alias target has no graph identity: "
                ++ show (sourceKey, plannedTargetRef)
            ]
          Just targetNode ->
            let targetKey = getNodeId (rpiCanonical targetNode)
             in case
                    ( IntMap.lookup sourceKey rpiSubst0
                    , IntMap.lookup targetKey inheritedGraphLiveRoutesLocal
                    )
                  of
                  (Just sourceRoute, Just inheritedRoute)
                    | typeBinderRefsSameIdentity
                        sourceRoute
                        plannedTargetRef
                    , typeBinderRefsSameIdentity
                        inheritedRoute
                        plannedTargetRef ->
                        []
                  routes ->
                    [ "inherited-rigid alias route is not backed by the selected inherited Gamma route"
                    , "  source: " ++ show sourceKey
                    , "  planned target: " ++ show plannedTargetRef
                    , "  planner/inherited routes: " ++ show routes
                    ]
      frozenBaseNodeForLive ga liveNode =
        case IntMap.lookup (getNodeId liveNode) (gbiSolvedToBase ga) of
          Just baseNode -> Just baseNode
          Nothing ->
            case lookupNodeIn (cNodes (gbiBaseConstraint ga)) liveNode of
              Just _ -> Just liveNode
              Nothing -> Nothing
      hasInheritedRigidAuthority liveNode =
        case
            IntMap.lookup
              (nodeRefKey (typeRef liveNode))
              rpiRigidBindParents
        of
          Just (_, BindRigid) -> True
          _ ->
            case rpiBindParentsGa of
              Nothing -> False
              Just ga ->
                case frozenBaseNodeForLive ga liveNode of
                  Just baseNode ->
                    case
                        IntMap.lookup
                          (nodeRefKey (typeRef baseNode))
                          (gbiBindParentsBase ga)
                    of
                      Just (_, BindRigid) -> True
                      _ -> False
                  Nothing -> False
      extraNames = zipWith alphaName [rpiExtraNameStart ..] rpiOrderedExtra
      substExtra =
        IntMap.fromList
          [ (key, typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name)
          | (key, name) <- zip rpiOrderedExtra extraNames
          ]
      localBinderRefs = IntMap.elems rpiSubst0 ++ IntMap.elems substExtra
      sourceRefIsLocal ref = any (typeBinderRefsSameIdentity ref) localBinderRefs
      externalSourceBinderKeysLocal =
        IntSet.fromList
          [ key
          | (key, ref) <- IntMap.toList sourceLiveRefsLocal,
            not (isLocallyClosedLiveKey key),
            not (sourceRefIsLocal ref),
            not (IntSet.member key structuralLiveSourceBinderKeysLocal)
          ]
      externalSourceBinderBaseKeysLocal =
        IntSet.fromList
          [ key
          | (key, ref) <- IntMap.toList sourceBaseRefsLocal,
            not (IntSet.member key locallyClosedBaseKeysLocal),
            not (sourceRefIsLocal ref),
            not (IntSet.member key structuralBaseSourceBinderKeysLocal)
          ]
      -- Binder planning exclusively owns the local substitution used by
      -- finalization. Source identities are added only to the reification
      -- view: S' can emit an inherited name whose solved representative is
      -- Bottom without accidentally promoting that name into this packet's
      -- quantified binder list.
      substBaseLocal = IntMap.union rpiSubst0 substExtra
      substAliasesLocal =
        IntMap.fromList
          [ (aliasKey, ref)
          | (aliasKey, binderKey) <- IntMap.toList rpiGammaAlias,
            aliasKey /= binderKey,
            not (isLocallyClosedLiveKey aliasKey),
            not (IntSet.member aliasKey rpiNestedSchemeInteriorSet),
            Just ref <- [IntMap.lookup binderKey substBaseLocal],
            sourceIdentityMayExpandSolvedClass ref
          ]
      substAliasesCanonLocal =
        IntMap.fromList
          [ (aliasKeyC, ref)
          | (aliasKey, ref) <- IntMap.toList substAliasesLocal,
            let aliasKeyC = getNodeId (rpiCanonical (NodeId aliasKey)),
            aliasKeyC /= aliasKey,
            not (isLocallyClosedLiveKey aliasKeyC),
            not (IntMap.member aliasKeyC substBaseLocal),
            not (IntMap.member aliasKeyC substAliasesLocal),
            sourceIdentityMayExpandSolvedClass ref
          ]
      typeRootReachable = rpiReachableFromWithBounds rpiTypeRoot
      aliasHasNamedBinding solvedKey =
        let solvedNode = NodeId solvedKey
            parentAt node =
              IntMap.lookup (nodeRefKey (typeRef node)) rpiBindParents
         in IntSet.member solvedKey rpiNamedUnderGaSet
              || case parentAt solvedNode of
                Just (_, BindFlex) -> True
                Just (_, BindRigid) -> False
                Nothing ->
                  case parentAt (rpiCanonical solvedNode) of
                    Just (_, BindFlex) -> True
                    _ -> False
      certifiedFromBaseAliasEntriesLocal =
          [ (solvedKey, baseKey, ref)
          | solvedToBaseEntry@(solvedKey, baseN) <-
              IntMap.toList rpiSolvedToBasePref,
            let baseKey = getNodeId baseN,
            Just (_, ref) <-
              [ certifiedFromBaseAliasRoute
                  rpiBaseGammaRep
                  rpiGammaAlias
                  substBaseLocal
                  solvedToBaseEntry
              ],
            not (isLocallyClosedLiveKey solvedKey),
            aliasHasNamedBinding solvedKey,
            not (IntSet.member solvedKey rpiNestedSchemeInteriorSet)
              || ( not (IntSet.member baseKey rpiNestedSchemeInteriorSet)
                     && IntSet.member solvedKey typeRootReachable
                 )
          ]
      substAliasesFromBaseLocal =
        IntMap.fromList
          [ (solvedKey, ref)
          | (solvedKey, _baseKey, ref) <-
              certifiedFromBaseAliasEntriesLocal
          ]
      substLocal =
        IntMap.unions
          [ substBaseLocal,
            substAliasesLocal,
            substAliasesCanonLocal,
            substAliasesFromBaseLocal
          ]
      requiredDeclarationCandidates plannedRef =
        [ (declarationKey, declarationRequirement)
        | (declarationKey, declarationRef) <-
            IntMap.toList rpiOrderedBinderRefs
        , typeBinderRefsSameIdentity declarationRef plannedRef
        , Just declarationRequirement <-
            [IntMap.lookup declarationKey rpiRequiredGamma]
        ]
      requiredDeclarationRouteConflicts =
        [ (key, candidates)
        | (key, _) <- IntMap.toList rpiRequiredGamma
        , Just plannedRef <- [IntMap.lookup key rpiSubst0]
        , let candidates = requiredDeclarationCandidates plannedRef
        , length candidates > 1
        ]
      requiredGammaSubst =
        IntMap.fromList
          [ (key, requiredRef)
          | (key, requirement) <- IntMap.toList rpiRequiredGamma
          , let plannedRef = IntMap.lookup key rpiSubst0
          , let declarationRequirement =
                  case plannedRef of
                    Just ref ->
                      case requiredDeclarationCandidates ref of
                        [(_, selected)] -> selected
                        _ -> requirement
                    Nothing -> requirement
          , let requiredRef =
                  typeBinderRefFromIdentity
                    ( typeBinderIdentityFromNode
                        (rgbExteriorNode declarationRequirement)
                    )
                    ( maybe
                        ("t" ++ show (getNodeId (rgbExteriorNode declarationRequirement)))
                        typeBinderRefName
                        plannedRef
                    )
          ]
      -- BinderPlan has already quotiented every graph occurrence that carries
      -- one semantic declaration identity.  When required-Gamma construction
      -- replaces that planned declaration with its selected exterior
      -- identity, route the whole quotient class to the same exterior.  Doing
      -- this here keeps S' and the final binder spine in agreement by
      -- construction; otherwise a non-required result/root occurrence can
      -- retain the old source identity and escape as a free variable.
      requiredDeclarationAliasGroups =
        IntMap.fromListWith (++)
          [ (routeKey, [requiredRef])
          | (requiredKey, _) <- IntMap.toList rpiRequiredGamma
          , Just plannedRef <- [IntMap.lookup requiredKey rpiSubst0]
          , Just requiredRef <- [IntMap.lookup requiredKey requiredGammaSubst]
          , (routeKey, routeRef) <- IntMap.toList rpiSubst0
          , typeBinderRefsSameIdentity routeRef plannedRef
          ]
      requiredDeclarationAliasConflicts =
        [ (key, refs)
        | (key, firstRef : refs) <-
            IntMap.toList requiredDeclarationAliasGroups
        , any
            (not . typeBinderRefsSameIdentity firstRef)
            refs
        ]
      requiredDeclarationAliasSubst =
        IntMap.mapMaybe
          ( \refs ->
              case refs of
                ref : _ -> Just ref
                [] -> Nothing
          )
          requiredDeclarationAliasGroups
      requiredExteriorSubst =
        IntMap.fromList
          [ (getNodeId (rgbExteriorNode requirement), ref)
            | (key, requirement) <- IntMap.toList rpiRequiredGamma
            , Just ref <- [IntMap.lookup key requiredGammaSubst]
          ]
      requiredEdgeResultSubst =
        IntMap.fromList
          [ (resultAliasKey, ref)
            | (key, requirement) <- IntMap.toList rpiRequiredGamma
            , Just ref <- [IntMap.lookup key requiredGammaSubst]
            , resultRoot <- NonEmpty.toList (rgbResultRoots requirement)
            , resultAlias <- [resultRoot, rpiCanonical resultRoot]
            , let resultAliasKey = getNodeId resultAlias
            , requiredEdgeResultAliasAgreesWithPlan resultAliasKey ref
          ]
      -- A trace result and its canonical representative are equal as types,
      -- but either graph node may still be a separately planned lexical
      -- binder.  The binder plan is declaration authority for such a key;
      -- publish the required-Gamma alias only into an unowned slot or one
      -- that already carries the same identity.  This prevents a raw result
      -- node from being quotiented through its canonical peer merely because
      -- solving made their types equal.
      requiredEdgeResultAliasAgreesWithPlan resultAliasKey requiredRef =
        case IntMap.lookup resultAliasKey substBaseLocal of
          Nothing -> True
          Just plannedRef ->
            typeBinderRefsSameIdentity plannedRef requiredRef
      requiredConstructionSubst =
        IntMap.unions
          [ requiredDeclarationAliasSubst,
            requiredGammaSubst,
            requiredExteriorSubst,
            requiredEdgeResultSubst,
            requiredStructuralLiveAliasSubst
          ]
      -- The live half of the same structural/exterior quotient used by the
      -- base reifier below.  BinderPlan has already selected the required
      -- declaration identity; the frozen base-to-solved map proves which
      -- structural child is merely another graph route to that declaration.
      requiredStructuralLiveAliasEntries =
        [ (getNodeId (rpiCanonical structuralChild), requiredRef)
        | (requiredKey, requirement) <-
            IntMap.toList rpiRequiredGamma
        , Just requiredRef <-
            [IntMap.lookup requiredKey requiredGammaSubst]
        , Just liveExterior <-
            [ case rpiBindParentsGa of
                Just ga ->
                  IntMap.lookup
                    (getNodeId (rgbExteriorNode requirement))
                    (gbiBaseToSolved ga)
                Nothing ->
                  Just (rgbExteriorNode requirement)
            ]
        , sourceStructuralChildren <-
            IntMap.elems structuralSourceBindersLocal
        , structuralChild <- sourceStructuralChildren
        , rpiCanonical structuralChild == rpiCanonical liveExterior
        ]
      requiredStructuralLiveAliasGroups =
        IntMap.fromListWith (++)
          [ (key, [ref])
          | (key, ref) <- requiredStructuralLiveAliasEntries
          ]
      requiredStructuralLiveAliasConflicts =
        [ (key, refs)
        | (key, firstRef : refs) <-
            IntMap.toList requiredStructuralLiveAliasGroups
        , any
            (not . typeBinderRefsSameIdentity firstRef)
            refs
        ]
      requiredStructuralLiveAliasSubst =
        IntMap.mapMaybe
          (\refs -> case refs of
            ref : _ -> Just ref
            [] -> Nothing
          )
          requiredStructuralLiveAliasGroups
      substRefsLocal =
        filterLocallyClosedLiveRefs $
          IntMap.unions
            [ requiredConstructionSubst,
              canonicalizeSubstRefs rpiCanonical substLocal
            ]
      -- Structural source identities are declaration identities too.  Keep
      -- them out of inherited-Gamma authority, but publish them to S' so
      -- graph copies of one structural binder reify as one semantic binder.
      -- A required Gamma route is stronger construction authority, however:
      -- source metadata describes where a source binder came from, whereas
      -- RaiseMerge has selected the exact outward binder that this occurrence
      -- must construct.  Retain source precedence for unrelated graph keys,
      -- but never let it replace an explicit required-Gamma route.
      reifySubstLocal =
        mergeReifySubstRefs
          requiredConstructionSubst
          graphLiveRefsDirect
          structuralLiveRefsLocal
          sourceLiveRefsLocal
          substRefsLocal
      aliasBinderKeysLocal = rpiAliasBinderBases
      filterAliasKeysLocal =
        if IntSet.null aliasBinderKeysLocal
          then id
          else IntMap.filterWithKey (\k _ -> not (IntSet.member k aliasBinderKeysLocal))
      substAliasesForLocal _binderKey =
        IntMap.fromList
          [ (aliasKey, ref)
          | (aliasKey, binderKey') <- IntMap.toList rpiGammaAlias,
            aliasKey /= binderKey',
            not (IntSet.member aliasKey rpiNestedSchemeInteriorSet),
            Just ref <- [IntMap.lookup binderKey' substBaseLocal],
            sourceIdentityMayExpandSolvedClass ref
          ]
      substForBoundLocal binderKey =
        filterLocallyClosedLiveRefs $
          filterAliasKeysLocal $
            IntMap.unions
              [ requiredConstructionSubst,
                graphLiveRefsDirect,
                structuralLiveRefsLocal,
                sourceLiveRefsLocal,
                substBaseLocal,
                substAliasesForLocal binderKey
              ]
      substForBoundRefsLocal binderKey =
        canonicalizeSubstRefs rpiCanonical (substForBoundLocal binderKey)
      substBaseByKeyLocal =
        case rpiBindParentsGa of
          Just ga ->
            let fromBaseRep =
                  [ (baseKey, ref)
                  | (baseKey, solvedKey) <- IntMap.toList rpiBaseGammaRep,
                    Just ref <- [IntMap.lookup solvedKey substBaseLocal],
                    sourceIdentityMayExpandSolvedClass ref
                  ]
                solvedPreference =
                  [ (getNodeId baseN, ref)
                  | (solvedKey, ref) <- IntMap.toList substBaseLocal,
                    Just baseN <- [IntMap.lookup solvedKey rpiSolvedToBasePref],
                    sourceIdentityMayExpandSolvedClass ref
                  ]
                fromBaseToSolved =
                  [ (baseKey, ref)
                  | (baseKey, solvedN) <- IntMap.toList (gbiBaseToSolved ga),
                    let solvedKey = getNodeId (rpiCanonical solvedN),
                    Just ref <- [IntMap.lookup solvedKey substBaseLocal],
                    sourceIdentityMayExpandSolvedClass ref
                  ]
             in IntMap.unions
                  [ requiredBaseConstructionSubst,
                    requiredStructuralBaseAliasSubst,
                    inheritedOuterFlexBaseRoutesLocal,
                    structuralBaseRefsLocal,
                    sourceBaseRefsLocal,
                    IntMap.fromListWith (\_ old -> old) fromBaseRep,
                    IntMap.fromListWith (\_ old -> old) solvedPreference,
                    IntMap.fromListWith (\_ old -> old) fromBaseToSolved
                  ]
          Nothing -> IntMap.empty
      requiredBaseConstructionSubst =
        case rpiBindParentsGa of
          Nothing -> IntMap.empty
          Just ga ->
            IntMap.fromListWith
              (\_ existing -> existing)
              [ (getNodeId baseNode, requiredRef)
              | (liveKey, requiredRef) <-
                  IntMap.toList requiredConstructionSubst
              , baseNode <-
                  directBaseNode liveKey
                    ++ maybe
                      []
                      (: [])
                      (IntMap.lookup liveKey rpiSolvedToBasePref)
                    ++ maybe
                      []
                      (: [])
                      (IntMap.lookup liveKey (gbiSolvedToBase ga))
              ]
            where
              directBaseNode liveKey =
                case
                    lookupNodeIn
                      (cNodes (gbiBaseConstraint ga))
                      (NodeId liveKey)
                  of
                    Just _ -> [NodeId liveKey]
                    Nothing -> []
      -- A frozen structural binder can be a second base-domain spelling of a
      -- required Gamma exterior: both base nodes project to the same exact
      -- live construction node.  Publish that quotient in the base
      -- substitution before S' reifies the structural owner.  The outer
      -- required-Gamma declaration can then own the identity directly, so the
      -- structural forall is never constructed as a duplicate declaration.
      --
      -- Restrict the bridge to children certified by the frozen structural
      -- ownership map.  General solved equality is not declaration authority.
      requiredStructuralBaseAliasEntries =
        case rpiBindParentsGa of
          Nothing -> []
          Just ga ->
            [ (getNodeId baseChild, requiredRef)
            | (requiredKey, requirement) <-
                IntMap.toList rpiRequiredGamma
            , Just requiredRef <-
                [IntMap.lookup requiredKey requiredGammaSubst]
            , Just liveExterior <-
                [ IntMap.lookup
                    (getNodeId (rgbExteriorNode requirement))
                    (gbiBaseToSolved ga)
                ]
            , baseChildren <-
                IntMap.elems structuralSourceBaseBindersLocal
            , baseChild <- baseChildren
            , Just liveChild <-
                [ IntMap.lookup
                    (getNodeId baseChild)
                    (gbiBaseToSolved ga)
                ]
            , rpiCanonical liveChild == rpiCanonical liveExterior
            ]
      requiredStructuralBaseAliasGroups =
        IntMap.fromListWith (++)
          [ (key, [ref])
          | (key, ref) <- requiredStructuralBaseAliasEntries
          ]
      requiredStructuralBaseAliasConflicts =
        [ (key, refs)
        | (key, firstRef : refs) <-
            IntMap.toList requiredStructuralBaseAliasGroups
        , any
            (not . typeBinderRefsSameIdentity firstRef)
            refs
        ]
      requiredStructuralBaseAliasSubst =
        IntMap.mapMaybe
          (\refs -> case refs of
            ref : _ -> Just ref
            [] -> Nothing
          )
          requiredStructuralBaseAliasGroups
      substForBoundBaseLocal _binderKey =
        filterLocallyClosedBaseRefs (filterAliasKeysLocal substBaseByKeyLocal)
      substForBoundBaseRefsLocal binderKey =
        canonicalizeSubstRefs id (substForBoundBaseLocal binderKey)
      externalLiveRoutesLocal =
        IntMap.filterWithKey
          (\key _ -> IntSet.member key externalSourceBinderKeysLocal)
          sourceLiveRefsLocal
      externalBaseRoutesLocal =
        IntMap.filterWithKey
          (\key _ -> IntSet.member key externalSourceBinderBaseKeysLocal)
          sourceBaseRefsLocal
      inheritedLiveRoutesLocal =
        IntMap.union externalLiveRoutesLocal inheritedGraphLiveRoutesLocal
      inheritedBaseRoutesLocal =
        IntMap.union externalBaseRoutesLocal inheritedGraphBaseRoutesLocal
      uniqueRefs = foldr insertRef []
        where
          insertRef ref refs
            | any (typeBinderRefsSameIdentity ref) refs = refs
            | otherwise = ref : refs
      graphRef node =
        typeBinderRefFromIdentity
          (typeBinderIdentityFromNode node)
          ("t" ++ show (getNodeId node))
      baseConstraintForAuthority = gbiBaseConstraint <$> rpiBindParentsGa
      baseReachableFromWithBounds root =
        case baseConstraintForAuthority of
          Nothing -> IntSet.empty
          Just baseConstraint ->
            reachableFromStop
              getNodeId
              id
              (\node ->
                case lookupNodeIn (cNodes baseConstraint) node of
                  Just tyNode -> structuralChildrenWithBounds tyNode
                  Nothing -> []
              )
              (const False)
              root
      strictEnclosingGen parents currentGen ownerGen
        | currentGen == ownerGen = False
        | otherwise =
            case
                Binding.bindingPathToRootLocal
                  parents
                  (GenRef currentGen)
            of
              Right (_ : ancestors) -> GenRef ownerGen `elem` ancestors
              _ -> False
      refIsLocal ref =
        any (typeBinderRefsSameIdentity ref) localBinderRefs
      selectedOuterFlexAuthorityRefs =
        case (rpiScopeGen, rpiReifyRootSource) of
          (Just currentGen, ReifyLiveRoot root) ->
            uniqueRefs
              [ selectedRef
              | liveKey <-
                  IntSet.toList
                    (rpiReachableFromWithBounds root)
              , let liveNode = rpiCanonical (NodeId liveKey)
              , Just TyVar {} <-
                  [IntMap.lookup (getNodeId liveNode) rpiNodes]
              , not (isLocallyClosedLiveKey (getNodeId liveNode))
              , Just (GenRef ownerGen, BindFlex) <-
                  [ IntMap.lookup
                      (nodeRefKey (typeRef liveNode))
                      rpiRigidBindParents
                  ]
              , strictEnclosingGen
                  rpiRigidBindParents
                  currentGen
                  ownerGen
              , let selectedRef =
                      IntMap.findWithDefault
                        (graphRef liveNode)
                        (getNodeId liveNode)
                        reifySubstLocal
              , not (refIsLocal selectedRef)
              ]
          (Just currentGen, ReifyBaseSchemeRoot root)
            | Just ga <- rpiBindParentsGa ->
                let baseParents = gbiBindParentsBase ga
                    baseConstraint = gbiBaseConstraint ga
                 in uniqueRefs
                      [ selectedRef
                      | baseKey <-
                          IntSet.toList
                            (baseReachableFromWithBounds root)
                      , let baseNode = NodeId baseKey
                      , Just TyVar {} <-
                          [lookupNodeIn (cNodes baseConstraint) baseNode]
                      , not
                          ( IntSet.member
                              baseKey
                              locallyClosedBaseKeysLocal
                          )
                      , Just (GenRef ownerGen, BindFlex) <-
                          [ IntMap.lookup
                              (nodeRefKey (typeRef baseNode))
                              baseParents
                          ]
                      , strictEnclosingGen
                          baseParents
                          currentGen
                          ownerGen
                      , let selectedRef =
                              IntMap.findWithDefault
                                (graphRef baseNode)
                                baseKey
                                substBaseByKeyLocal
                      , not (refIsLocal selectedRef)
                      ]
          _ -> []
      sourceRefsAt sourceRefs rawNode canonicalNode =
        uniqueRefs
          [ ref
          | key <- [getNodeId rawNode, getNodeId canonicalNode]
          , Just ref <- [IntMap.lookup key sourceRefs]
          ]
      structuralAuthorityRefs
        domainCanonical
        reachable
        substMap
        sourceRefs
        structuralBinders =
          uniqueRefs $ concat
            [ graphRef childRaw
                : routedRef
                : sourceRefsAt sourceRefs childRaw childC
            | (ownerKey, children) <- IntMap.toList structuralBinders
            , childRaw <- children
            , let ownerC = domainCanonical (NodeId ownerKey)
            , let childC = domainCanonical childRaw
            , IntSet.member (getNodeId childC) reachable
            , not (IntSet.member (getNodeId ownerC) reachable)
            , let routedRef =
                    IntMap.findWithDefault
                      (graphRef childC)
                      (getNodeId childC)
                      substMap
            ]
      liveStructuralAuthorityRefs =
        case rpiReifyRootSource of
          ReifyLiveRoot root ->
            structuralAuthorityRefs
              rpiCanonical
              (rpiReachableFromWithBounds root)
              reifySubstLocal
              rpiSourceBinderRefs
              structuralSourceBindersLocal
          ReifyBaseSchemeRoot {} -> []
      baseStructuralAuthorityRefs =
        case rpiReifyRootSource of
          ReifyBaseSchemeRoot root ->
            structuralAuthorityRefs
              id
              (baseReachableFromWithBounds root)
              substBaseByKeyLocal
              rpiSourceBinderRefs
              structuralSourceBaseBindersLocal
          ReifyLiveRoot {} -> []
      inheritedGammaPlanLocal inheritedGammaRoutesLocal =
        let selectedRouteRefs =
              case rpiReifyRootSource of
                ReifyLiveRoot {} -> IntMap.elems inheritedLiveRoutesLocal
                ReifyBaseSchemeRoot {} -> IntMap.elems inheritedBaseRoutesLocal
         in InheritedGammaPlan
              { inheritedGammaPlanLiveRoutes = inheritedLiveRoutesLocal,
                inheritedGammaPlanBaseRoutes = inheritedBaseRoutesLocal,
                inheritedGammaPlanAuthorizedRefs =
                  uniqueRefs
                    ( selectedRouteRefs
                        ++ selectedOuterFlexAuthorityRefs
                        ++ liveStructuralAuthorityRefs
                        ++ baseStructuralAuthorityRefs
                    ),
                inheritedGammaPlanRoutes = inheritedGammaRoutesLocal
              }
      inheritedGammaRoutesLocalResult =
        pure
          ( InheritedGammaRoutes
              [ InheritedGammaRoute
                  { inheritedGammaRouteLiveNode = NodeId liveKey,
                    inheritedGammaRouteBaseNode = baseNode,
                    inheritedGammaRouteRef = inheritedRef
                  }
              | (liveKey, baseNode, inheritedRef) <-
                  inheritedGraphRouteCandidatesLocal
              ]
              selectedOuterFlexAuthorityRefs
          )
      typeRootC = rpiCanonical rpiTypeRoot
      (schemeOwnerFromBody, schemeOwnerFromBodyIsAlias) =
        SchemeRoots.schemeOwnerFromBody rpiSchemeRootsPlan rpiSolvedToBasePref typeRootC
      ownersByRoot =
        [ gnId gen
        | gen <- NodeAccess.allGenNodes rpiConstraint,
          any (\root -> rpiCanonical root == typeRootC) (gnSchemes gen)
        ]
      rootChoice =
        mkReifyRootChoice
          rpiReifyRootSource
          reifySubstLocal
          substBaseByKeyLocal
      schemeOwners =
        maybe ownersByRoot (\gid -> gid : ownersByRoot) schemeOwnerFromBody
      typeInScope =
        case rpiScopeRootC of
          GenRef gid ->
            bindingScopeFor rpiConstraint (typeRef typeRootC) == Just gid
          _ -> False
      typeInScopeAdjusted =
        case (rpiScopeGen, schemeOwnerFromBody) of
          (Just gid, Just owner)
            | owner /= gid -> False
          _ -> typeInScope
      useSchemeType =
        case (rpiScopeRootC, rpiScopeGen, schemeOwnerFromBody) of
          (GenRef _, Just gid, Just owner)
            | owner /= gid -> True
          (GenRef _, Just gid, _) ->
            not typeInScopeAdjusted
              && not (null schemeOwners)
              && not (gid `elem` schemeOwners)
          _ -> False
      typeRootIsTargetBound =
        case rpiTargetBound of
          Just bnd -> rpiCanonical bnd == typeRootC
          Nothing -> False
      useSchemeTypeAdjusted =
        case (schemeOwnerFromBody, rpiScopeGen) of
          (Just owner, Just gid)
            | owner /= gid && not typeRootIsTargetBound -> False
          _ -> useSchemeType
      schemeTypeChoice =
        SchemeTypeChoice
          { stcUseSchemeType = useSchemeTypeAdjusted,
            stcSchemeOwnerFromBody = schemeOwnerFromBody,
            stcSchemeOwnerFromBodyIsAlias = schemeOwnerFromBodyIsAlias,
            stcSchemeOwners = schemeOwners
          }
      bindingScopeGenLocal = bindingScopeGen rpiConstraint
      nodesMap = NodeMap rpiNodes
      hasExplicitBoundLocal = hasExplicitBoundFor rpiCanonical nodesMap rpiConstraint
      isTargetSchemeBinderLocal =
        isTargetSchemeBinderFor
          rpiCanonical
          rpiConstraint
          rpiTarget0
          rpiTargetIsBaseLike
      boundMentionsSelfAliasLocal =
        boundMentionsSelfAliasFor
          AliasEnv { aeCanonical = rpiCanonical,
              aeConstraint = rpiConstraint,
              aeNodes = nodesMap,
              aeBindParents = IntMap.empty, -- unused by boundMentionsSelfAliasFor
              aeDepthMap = rpiGammaAlias,
              aeScopeSchemeRoots = rpiNestedSchemeInteriorSet,
              aeNodeChildren = rpiReachableFromWithBounds
            }
   in do
      case inheritedRigidAliasRouteErrors of
        [] -> pure ()
        errors ->
          Left
            ( ValidationFailed
                ( "invalid inherited-rigid binder alias plan"
                    : errors
                )
            )
      case requiredDeclarationRouteConflicts of
        [] -> pure ()
        conflicts ->
          Left
            ( ValidationFailed
                [ "required Gamma identity quotient selected multiple declarations"
                , "  conflicts: " ++ show conflicts
                ]
            )
      case requiredDeclarationAliasConflicts of
        [] -> pure ()
        conflicts ->
          Left
            ( ValidationFailed
                [ "required Gamma identity quotient selected conflicting exterior identities"
                , "  conflicts: " ++ show conflicts
                ]
            )
      case
          ( requiredStructuralLiveAliasConflicts
          , requiredStructuralBaseAliasConflicts
          )
        of
        ([], []) -> pure ()
        (liveConflicts, baseConflicts) ->
          Left
            ( ValidationFailed
                [ "required Gamma structural aliases disagree on declaration identity"
                , "  conflicting live routes: " ++ show liveConflicts
                , "  conflicting base routes: " ++ show baseConflicts
                ]
            )
      inheritedGammaRoutesLocal <- inheritedGammaRoutesLocalResult
      pure ReifyPlan
        { rpSubst = substRefsLocal,
          rpSubstForBound = substForBoundRefsLocal,
          rpSubstForBoundBase = substForBoundBaseRefsLocal,
          rpRootChoice = rootChoice,
          rpSchemeTypeChoice = schemeTypeChoice,
          rpBindingScopeGen = bindingScopeGenLocal,
          rpHasExplicitBound = hasExplicitBoundLocal,
          rpIsTargetSchemeBinder = isTargetSchemeBinderLocal,
          rpBoundMentionsSelfAlias = boundMentionsSelfAliasLocal,
          rpContainsForall = containsForall
          , rpRequiredGamma = rpiRequiredGamma
          , rpSourceBinderRefs = rpiSourceBinderRefs
          , rpSourceDeclarationsBeforeRequiredGamma =
              rpiSourceDeclarationsBeforeRequiredGamma
          , rpExternalSourceBinderKeys = externalSourceBinderKeysLocal
          , rpExternalSourceBinderBaseKeys = externalSourceBinderBaseKeysLocal
          , rpStructuralSourceBinders = structuralSourceBindersLocal
          , rpStructuralSourceBaseBinders = structuralSourceBaseBindersLocal
          , rpInheritedGammaPlan =
              inheritedGammaPlanLocal inheritedGammaRoutesLocal
        }

-- | Pair a reification root with the substitution from the same graph domain.
-- The constructor is private so a base root cannot accidentally carry live
-- keys, or vice versa.  'buildTypeRootPlan' has already proved which graph owns
-- the semantic root; this constructor preserves that decision without a
-- post-hoc root/substitution adjustment.
mkReifyRootChoice ::
  ReifyRootSource ->
  IntMap.IntMap TypeBinderRef ->
  IntMap.IntMap TypeBinderRef ->
  ReifyRootChoice
mkReifyRootChoice source liveSubst baseSubst =
  case source of
    liveSource@ReifyLiveRoot {} ->
      ReifyRootChoice liveSource liveSubst
    baseSource@ReifyBaseSchemeRoot {} ->
      ReifyRootChoice baseSource baseSubst

bindingFor ::
  ReifyBindingEnv p ->
  ReifyPlan ->
  (TypeBinderRef, Int) ->
  Either ElabError (TypeBinderRef, Maybe BoundType)
bindingFor env plan (binderRef0, nidInt) = do
  let ReifyBindingEnv
        { rbeConstraint = constraint,
          rbeNodes = nodes,
          rbeCanonical = canonical,
          rbeBindParents = bindParents,
          rbeScopeGen = scopeGen,
          rbeSchemeRootOwner = schemeRootOwner,
          rbeSchemeRootByBody = schemeRootByBody,
          rbeSchemeRootByBodyBase = schemeRootByBodyBase,
          rbeSchemeRootKeySet = schemeRootKeySet,
          rbeGammaAlias = gammaAlias,
          rbeAliasBinderBases = aliasBinderBases,
          rbeSolvedToBasePref = solvedToBasePref,
          rbeNamedUnderGaSet = namedUnderGaSet,
          rbeBinderSet = binderSet,
          rbeUniqueUnboundedName = uniqueUnboundedName,
          rbeResForReify = resForReify,
          rbeBindParentsGa = mbBindParentsGa,
          rbeBindingScopeGen = bindingScopeGenFn,
          rbeHasExplicitBound = hasExplicitBoundFn,
          rbeIsTargetSchemeBinder = isTargetSchemeBinder,
          rbeBoundMentionsSelfAlias = boundMentionsSelfAlias,
          rbeContainsForall = containsForallFn,
          rbeFirstGenAncestor = firstGenAncestor,
          rbeTraceM = traceGeneralizeM
        } = env
      ReifyPlan
        { rpSubst = substRefs,
          rpSubstForBound = substForBound,
          rpSubstForBoundBase = substForBoundBase,
          rpRequiredGamma = requiredGamma,
          rpSourceDeclarationsBeforeRequiredGamma =
            sourceDeclarationsBeforeRequiredGamma,
          rpExternalSourceBinderKeys = externalSourceBinderKeys,
          rpExternalSourceBinderBaseKeys = externalSourceBinderBaseKeys,
          rpStructuralSourceBinders = structuralSourceBinders,
          rpStructuralSourceBaseBinders = structuralSourceBaseBinders,
          rpInheritedGammaPlan = inheritedGammaPlan
        } = plan
      substBinderRefs = IntMap.elems substRefs
      canonicalBinder v =
        let vC = canonical v
         in case IntMap.lookup (getNodeId vC) nodes of
              Just TyVar {} -> vC
              _ ->
                case IntMap.lookup (getNodeId v) nodes of
                  Just TyVar {} -> v
                  _ -> vC
      bNodeC = canonicalBinder (NodeId nidInt)
      binderKey = getNodeId bNodeC
      requiredGammaBinder = IntMap.lookup binderKey requiredGamma
      binderRef =
        case IntMap.lookup binderKey substRefs of
          Just ref -> ref
          Nothing ->
            case typeBinderRefNode binderRef0 of
              Just node
                | canonical node == bNodeC -> binderRef0
              _ ->
                typeBinderRefFromIdentity
                  (typeBinderIdentityFromNode bNodeC)
                  (typeBinderRefName binderRef0)
      binderIsNamed = IntSet.member (getNodeId bNodeC) namedUnderGaSet
      substForBoundRefs = substForBound binderKey
      mbBoundNode = VarStore.lookupVarBound constraint bNodeC
      boundIsLocalSchemeRoot =
        case (scopeGen, mbBoundNode) of
          (Just gid, Just bnd) ->
            case IntMap.lookup (getNodeId (canonical bnd)) schemeRootOwner of
              Just gid' -> gid' == gid
              Nothing -> False
          _ -> False
      boundIsLocalSchemeBody =
        case (scopeGen, mbBoundNode) of
          (Just gid, Just bnd) ->
            case IntMap.lookup (getNodeId (canonical bnd)) schemeRootByBody of
              Just root ->
                case IntMap.lookup (getNodeId (canonical root)) schemeRootOwner of
                  Just gid' -> gid' == gid
                  Nothing -> False
              Nothing -> False
          _ -> False
      boundParentIsBinder =
        case mbBoundNode of
          Just bnd ->
            case IntMap.lookup (nodeRefKey (typeRef (canonical bnd))) bindParents of
              Just (TypeRef parent, _) -> canonical parent == bNodeC
              _ -> False
          Nothing -> False
      boundRoot =
        case mbBoundNode of
          Just bnd ->
            case IntMap.lookup (getNodeId (canonical bnd)) schemeRootByBody of
              Just _root
                | boundIsLocalSchemeBody -> canonical bnd
              Just root
                | canonical root == bNodeC -> canonical bnd
              Just root -> root
              Nothing -> canonical bnd
          Nothing -> bNodeC
      liveBoundRefinesUnboundedBase =
        case (mbBoundNode, mbBindParentsGa) of
          (Just _, Just ga) ->
            let baseConstraint = gbiBaseConstraint ga
                mbBaseBinder =
                  case IntMap.lookup binderKey (gbiSolvedToBase ga) of
                    Just baseBinder -> Just baseBinder
                    Nothing -> IntMap.lookup binderKey solvedToBasePref
             in case mbBaseBinder of
                  Just baseBinder ->
                    case lookupNodeIn (cNodes baseConstraint) baseBinder of
                      Just TyVar {} ->
                        isNothing
                          (VarStore.lookupVarBound baseConstraint baseBinder)
                      _ -> False
                  Nothing -> False
          _ -> False
  traceGeneralizeM
    ( "generalizeAt: boundRoot binder="
        ++ show bNodeC
        ++ " boundRoot="
        ++ show boundRoot
        ++ " boundParentIsBinder="
        ++ show boundParentIsBinder
        ++ " boundIsLocalSchemeBody="
        ++ show boundIsLocalSchemeBody
    )
  boundSchemeBinderKeys <- case IntMap.lookup (getNodeId (canonical boundRoot)) schemeRootOwner of
    Just gid
      | Just gid /= scopeGen -> do
          nestedBinders <- bindingToElab (Binding.boundFlexChildrenUnder canonical constraint (genRef gid))
          pure (IntSet.fromList (map (getNodeId . canonical) nestedBinders))
    _ -> pure IntSet.empty
  let substForBoundFiltered =
        if IntSet.null boundSchemeBinderKeys
          then substForBoundRefs
          else IntMap.filterWithKey (\k _ -> not (IntSet.member k boundSchemeBinderKeys)) substForBoundRefs
      -- Rigid quantification is inlined, but an unbounded rigid variable from
      -- an enclosing gen scope is already a variable in the xMLF typing
      -- environment.  Name it before reifying this bound so construction
      -- preserves that variable instead of manufacturing bottom and trying to
      -- recover the lost identity during scheme finalization.  The reification
      -- planner has already proved strict lexical ancestry and base origin for
      -- every route in this capability; do not rediscover that proof here.
      outerRigidRefs =
        inheritedGammaPlanLiveRoutes inheritedGammaPlan
      substForBoundReady =
        IntMap.union substForBoundFiltered outerRigidRefs
      substForBoundNamesReady =
        IntMap.map typeBinderRefName substForBoundReady
      substNameSetForBoundReady =
        Set.fromList (IntMap.elems substForBoundNamesReady)
      outerRigidBaseRefs =
        inheritedGammaPlanBaseRoutes inheritedGammaPlan
  let mbBaseRoot =
        -- See Note [Base bounds require a bounded solved binder].
        if (isNothing mbBoundNode && not (hasExplicitBoundFn bNodeC))
          || boundIsLocalSchemeBody
          || boundParentIsBinder
          || liveBoundRefinesUnboundedBase
          then Nothing
          else case mbBindParentsGa of
            Just ga ->
              let baseConstraint = gbiBaseConstraint ga
                  baseBoundForBinder baseBinderNode =
                    case VarStore.lookupVarBound baseConstraint baseBinderNode of
                      Just baseBnd ->
                        case IntMap.lookup (getNodeId baseBnd) schemeRootByBodyBase of
                          Just baseSchemeRoot ->
                            if baseSchemeRoot == baseBinderNode
                              then Just baseBnd
                              else Just baseSchemeRoot
                          Nothing -> Just baseBnd
                      Nothing -> Nothing
                  fallbackFromBoundRoot =
                    case IntMap.lookup (getNodeId (canonical boundRoot)) solvedToBasePref of
                      Just baseRoot ->
                        case IntMap.lookup (getNodeId baseRoot) schemeRootByBodyBase of
                          Just baseSchemeRoot ->
                            if baseSchemeRoot == baseRoot
                              then Just baseRoot
                              else Just baseSchemeRoot
                          Nothing -> Just baseRoot
                      Nothing -> IntMap.lookup binderKey solvedToBasePref
                  baseBinder =
                    case IntMap.lookup binderKey (gbiSolvedToBase ga) of
                      Just binder -> Just binder
                      Nothing -> IntMap.lookup binderKey solvedToBasePref
               in case baseBinder of
                    -- A solved bound introduced for an originally-unbounded
                    -- live binder has no base counterpart.  Do not replace it
                    -- with a positional fallback from another base node.
                    Just baseBinderNode -> baseBoundForBinder baseBinderNode
                    Nothing -> fallbackFromBoundRoot
            Nothing -> Nothing
  boundTy0 <-
    case (requiredGammaBinder, mbBindParentsGa, mbBaseRoot) of
      (Just requirement, Just _, _) ->
        pure (rgbOperatedType requirement)
      (Just requirement, Nothing, _) ->
        Left
          ( ValidationFailed
              [ "root RaiseMerge bound construction requires the frozen base graph",
                "  requirement: " ++ show requirement
              ]
          )
      (Nothing, _, _)
        | IntSet.member
            binderKey
            sourceDeclarationsBeforeRequiredGamma ->
            -- BinderPlan proved from the frozen graph and exact source
            -- sidecar that this unbounded declaration is consumed by a
            -- required Gamma bound.  Its live solved bound may already route
            -- back through that Gamma result; preserve the source declaration
            -- instead of constructing a dependency cycle.
            pure TBottom
      (Nothing, Just ga, Just baseRoot) ->
        reifyBoundWithExternalRefsOnConstraint
          (gbiBaseConstraint ga)
          (IntMap.union (substForBoundBase binderKey) outerRigidBaseRefs)
          externalSourceBinderBaseKeys
          structuralSourceBaseBinders
          baseRoot
      (Nothing, _, _) ->
        reifyBoundWithExternalRefs
          resForReify
          substForBoundReady
          externalSourceBinderKeys
          structuralSourceBinders
          boundRoot
  let canonicalKeyForRef ref =
        case typeBinderRefNode ref of
          Just node -> Just (getNodeId (canonical node))
          Nothing -> Nothing
      fallbackAliasFor ref =
        case (uniqueUnboundedName, canonicalKeyForRef ref) of
          (Just fallbackName, Just keyC)
            | boundIsLocalSchemeBody
                && not (Set.member (typeBinderRefName ref) substNameSetForBoundReady) ->
                let nid = NodeId keyC
                 in case bindingScopeGenFn nid of
                      Just gid | Just gid /= scopeGen -> Just fallbackName
                      Nothing | isNothing scopeGen -> Just fallbackName
                      _ -> Nothing
          _ -> Nothing
      refMember ref refs =
        any (typeBinderRefsSameIdentity ref) refs
      aliasRefFor ref =
        case canonicalKeyForRef ref of
          Just keyC ->
            let repKey = IntMap.findWithDefault keyC keyC gammaAlias
             in case IntMap.lookup repKey substForBoundReady of
                  Just aliasRef -> Just aliasRef
                  Nothing ->
                    (\fallbackName ->
                      typeBinderRefFromIdentity
                        (typeBinderIdentityFromNode (NodeId repKey))
                        fallbackName)
                      <$> fallbackAliasFor ref
          Nothing -> Nothing
      substAliasTy boundRefs ty = case ty of
        TVarRef ref ->
          if refMember ref boundRefs
            then TVarRef ref
            else case aliasRefFor ref of
              Just ref' -> TVarRef ref'
              Nothing -> TVarRef ref
        TArrow a b -> TArrow (substAliasTy boundRefs a) (substAliasTy boundRefs b)
        TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (substAliasTy boundRefs) args)
        TVarAppRef ref args ->
          let ref' =
                if refMember ref boundRefs
                  then ref
                  else case aliasRefFor ref of
                    Just refAlias -> refAlias
                    Nothing -> ref
           in TVarAppRef ref' (fmap (substAliasTy boundRefs) args)
        TBaseWithIdentity _ _ -> ty
        TBottom -> ty
        TForallRef ref mb body ->
          let boundRefs' = ref : boundRefs
              mb' = fmap (substAliasBound boundRefs') mb
              body' = substAliasTy boundRefs' body
           in TForallRef ref mb' body'
        TMuRef ref body ->
          TMuRef ref (substAliasTy (ref : boundRefs) body)
      substAliasBound boundRefs bound = case bound of
        TArrow a b -> TArrow (substAliasTy boundRefs a) (substAliasTy boundRefs b)
        TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (substAliasTy boundRefs) args)
        TVarAppRef ref args ->
          let ref' =
                if refMember ref boundRefs
                  then ref
                  else case aliasRefFor ref of
                    Just refAlias -> refAlias
                    Nothing -> ref
           in TVarAppRef ref' (fmap (substAliasTy boundRefs) args)
        TBaseWithIdentity _ _ -> bound
        TBottom -> bound
        TForallRef ref mb body ->
          let boundRefs' = ref : boundRefs
              mb' = fmap (substAliasBound boundRefs') mb
              body' = substAliasTy boundRefs' body
           in TForallRef ref mb' body'
        TMuRef ref body ->
          TMuRef ref (substAliasTy (ref : boundRefs) body)
      normalizeSelfTy selfRef = goTy []
        where
          goTy shadow ty = case ty of
            TVarRef ref
              | typeBinderRefsSameIdentity ref selfRef,
                not (refMember ref shadow) ->
                  TBottom
              | otherwise -> TVarRef ref
            TArrow a b -> TArrow (goTy shadow a) (goTy shadow b)
            TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (goTy shadow) args)
            TVarAppRef ref args -> TVarAppRef ref (fmap (goTy shadow) args)
            TBaseWithIdentity _ _ -> ty
            TBottom -> ty
            TForallRef ref mb body ->
              let shadow' = ref : shadow
                  mb' = fmap (goBound shadow') mb
                  body' = goTy shadow' body
               in TForallRef ref mb' body'
            TMuRef ref body ->
              let shadow' = ref : shadow
               in TMuRef ref (goTy shadow' body)
          goBound shadow bound = case bound of
            TArrow a b -> TArrow (goTy shadow a) (goTy shadow b)
            TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (goTy shadow) args)
            TVarAppRef ref args -> TVarAppRef ref (fmap (goTy shadow) args)
            TBaseWithIdentity _ _ -> bound
            TBottom -> bound
            TForallRef ref mb body ->
              let shadow' = ref : shadow
                  mb' = fmap (goBound shadow') mb
                  body' = goTy shadow' body
               in TForallRef ref mb' body'
            TMuRef ref body ->
              let shadow' = ref : shadow
               in TMuRef ref (goTy shadow' body)
      boundTy0' =
        case (boundTy0, mbBoundNode) of
          (TBottom, Just _)
            | binderIsNamed -> TBottom
          (TBottom, Just bnd) ->
            let bndC = canonical bnd
                bndKey = getNodeId bndC
                nameForBound =
                  case IntMap.lookup bndKey substForBoundNamesReady of
                    Just nm -> nm
                    Nothing -> "t" ++ show bndKey
                refForBound =
                  case IntMap.lookup bndKey substForBoundReady of
                    Just ref -> ref
                    Nothing ->
                      typeBinderRefFromIdentity
                        (typeBinderIdentityFromNode bndC)
                        nameForBound
             in case (IntMap.lookup bndKey nodes, VarStore.lookupVarBound constraint bndC) of
                  (Just TyVar {}, Nothing) -> TVarRef refForBound
                  _ -> boundTy0
          _ -> boundTy0
      boundTy0'' =
        if isNothing requiredGammaBinder && boundMentionsSelfAlias bNodeC
          then TBottom
          else boundTy0'
      boundTy0Aliased =
        if isJust requiredGammaBinder
          then boundTy0''
          else substAliasTy [] boundTy0''
      boundTy0Normalized =
        if isJust requiredGammaBinder
          then boundTy0Aliased
          else normalizeSelfTy binderRef boundTy0Aliased
      extraBoundRefs =
        if isJust requiredGammaBinder
          then
            -- rgbOperatedType is the exact S'(operated) supplied by the
            -- requirement planner.  Its free references belong to the
            -- enclosing Gamma and the binder planner has already ordered
            -- their dependencies; closing them again here changes the
            -- required bound.
            []
          else
            let isAliasBound ref =
                  case canonicalKeyForRef ref of
                    Just keyC ->
                      let repKey = IntMap.findWithDefault keyC keyC gammaAlias
                       in IntMap.member repKey substForBoundNamesReady
                    Nothing -> False
                freeRefs = freeTypeVarRefsType boundTy0Normalized
             in [ ref
                | ref <- freeRefs,
                  not (refMember ref (IntMap.elems substForBoundReady)),
                  not (isAliasBound ref)
                ]
      extraBoundNames = map typeBinderRefName extraBoundRefs
  extraBindings <-
    mapM
      ( \ref ->
          -- Free graph identities lifted into this bound are genuine nested
          -- binders. Reuse the binder constructor so their live lower bounds
          -- survive instead of manufacturing an unbounded forall.
          case typeBinderRefNode ref of
            Just node
              | Just TyVar {} <- IntMap.lookup (getNodeId (canonical node)) nodes -> do
                  (_, mbExtraBound) <- bindingFor env plan (ref, getNodeId node)
                  pure (ref, mbExtraBound)
            _ -> pure (ref, Nothing)
      )
      (sortOn typeBinderRefName extraBoundRefs)
  let boundTy =
        foldr
          (\(ref, mbExtraBound) acc -> TForallRef ref mbExtraBound acc)
          boundTy0Normalized
          extraBindings
  traceGeneralizeM
    ( "generalizeAt: boundExtras binder="
        ++ show bNodeC
        ++ " extras="
        ++ show extraBoundNames
        ++ " extraInfo="
        ++ show
          [ ( typeBinderRefName ref,
              do
                keyC <- canonicalKeyForRef ref
                let baseM = IntMap.lookup keyC solvedToBasePref
                let aliasM = IntMap.lookup keyC gammaAlias
                pure (keyC, baseM, aliasM, firstGenAncestor (typeRef (NodeId keyC)))
            )
          | ref <- extraBoundRefs
          ]
    )
  traceGeneralizeM
    ( "generalizeAt: boundSelfAlias binder="
        ++ show bNodeC
        ++ " mentionsSelf="
        ++ show (boundMentionsSelfAlias bNodeC)
        ++ " boundTy0="
        ++ show boundTy0
        ++ " boundTy="
        ++ show boundTy
    )
  let boundIsFreeVar =
        case boundTy of
          TVarRef _ ->
            case mbBoundNode of
              Just bnd ->
                let bndC = canonical bnd
                    bndHasExplicitBound = VarStore.lookupVarBound constraint bndC
                 in case IntMap.lookup (getNodeId bndC) nodes of
                      Just TyVar {} ->
                        isNothing bndHasExplicitBound
                          && not (IntMap.member (getNodeId bndC) substRefs)
                      _ -> False
              _ -> False
          _ -> False
      boundIsFreeVar' =
        boundIsFreeVar && not binderIsNamed
      boundIsSelfVar =
        case boundTy of
          -- Normalize tautological bounds (e.g. ∀(a ⩾ a)) away.
          TVarRef ref -> typeBinderRefsSameIdentity ref binderRef
          _ -> False
      boundMentionsBinderVar =
        any (typeBinderRefsSameIdentity binderRef) (freeTypeVarRefsType boundTy)
      boundIsSchemeRootNode =
        case mbBoundNode of
          Just bnd -> IntSet.member (getNodeId (canonical bnd)) schemeRootKeySet
          Nothing -> False
      boundAllowed =
        if isJust requiredGammaBinder || binderIsNamed
          then True
          else
            if boundIsLocalSchemeRoot || boundIsLocalSchemeBody
              then isTargetSchemeBinder bNodeC
              else
                hasExplicitBoundFn bNodeC
                  || boundParentIsBinder
                  || boundIsSchemeRootNode
                  || case mbBoundNode of
                    Just bnd -> IntSet.member (getNodeId (canonical bnd)) binderSet
                    Nothing -> False
                  || containsForallFn boundTy
      mbBound =
        if IntSet.member (getNodeId bNodeC) aliasBinderBases
          then
            if boundTy == TBottom || boundMentionsBinderVar
              then Nothing
              else Just boundTy
          else
            if boundTy == TBottom || boundIsFreeVar' || boundIsSelfVar || boundMentionsBinderVar || not boundAllowed
              then Nothing
              else Just boundTy
      -- See Note [Inter-binder alias bounds in recursive types]
      mbBoundTyped = case mbBound of
        Just (TVarRef _)
          | isJust requiredGammaBinder ->
              -- The required-Gamma plan already records this as an alias to
              -- an existing lexical binder.  'finalizeScheme' removes the
              -- exterior binder and rewrites its result/substitution entries
              -- to that identity; a bare variable must never be encoded as an
              -- independent xMLF bound.
              Right Nothing
        Just (TVarRef ref)
          | any (typeBinderRefsSameIdentity ref) substBinderRefs ->
              -- 'BoundType' deliberately cannot encode a bare variable.
              -- 'finalizeScheme' quotients this peer alias from the live
              -- graph before publishing the scheme and its substitution.
              Right Nothing
          | otherwise ->
              Left $
                ValidationFailed
                  [ "binder planning left an unsupported bare alias for reification"
                  , "  binder: " ++ show binderRef
                  , "  binder node: " ++ show bNodeC
                  , "  bound: " ++ show ref
                  , "  scope gen: " ++ show scopeGen
                  ]
        Nothing -> Right Nothing
        Just bnd -> case elabToBound bnd of
          Left err -> Left $ ValidationFailed [err]
          Right typed -> Right (Just typed)
  case mbBoundTyped of
    Left err -> Left err
    Right typed -> pure (binderRef, typed)

{- Note [Base bounds require a bounded solved binder]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The base constraint is useful for recovering a bound whose solved copy has
been rewritten, but it must not invent a bound for a live binder that is
unbounded in the solved graph.  Solved and base node spaces can overlap, and a
valid solved-to-base correspondence can point at a base node with a structural
bound even when the live binder itself has no bound.  Reifying that structural
bound changes @forall a. a -> a@ into @forall (a >= bottom -> bottom). a -> a@.

The binder plan records whether the live binder owns an explicit bound.  When
both that fact and the live bound are absent, the solved binder is already the
complete construction: reify it directly and do not consult a base overlay.
-}

{- Note [Peer-alias bounds are quotiented during finalization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When reifying a binding whose alias bound is another binder within the same
generalization group (that is, the bound is a TVar represented by `rpSubst`),
we cannot put that bound in `BoundType`: by construction, a bound cannot be a
bare variable.  This function therefore returns a temporary `Nothing`.

`finalizeScheme` still has the live binder nodes and performs the semantic
operation required by thesis section 15.6.2: it quotients every peer-alias
component, substitutes the representative through the reified body and all
bounds, removes the duplicate binder, and maps every aliased node to the same
`TypeBinderRef`.  Thus the temporary `Nothing` never means an independently
quantified variable in the finished xMLF scheme.

For example, eMLF `forall a. forall (b >= a). b -> a` is constructed as xMLF
`forall a. a -> a`, not the unsound widening `forall a b. b -> a`. -}
