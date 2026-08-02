{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : MLF.Constraint.Presolution.Plan.BinderPlan.Build
-- Description : Binder plan construction
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
module MLF.Constraint.Presolution.Plan.BinderPlan.Build
  ( buildBinderPlan,
  )
where

import Control.Monad (guard)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Plan.BinderPlan.Alias
  ( AliasEnv (..),
    boundMentionsSelfAliasFor,
  )
import MLF.Constraint.Presolution.Plan.BinderPlan.Order (GaBindParentsInfo (..))
import MLF.Constraint.Presolution.Plan.BinderPlan.Predicate (isTargetSchemeBinderFor)
import MLF.Constraint.Presolution.Plan.BinderPlan.Types (BinderPlan (..), BinderPlanInput (..))
import MLF.Constraint.Presolution.Plan.Requirements (RequiredGammaBinder (..))
import MLF.Constraint.Presolution.View (PresolutionView (..))
import MLF.Constraint.Presolution.Plan.BinderPlan.Util
import MLF.Constraint.Types.Graph
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Reify.Core
  ( namedNodes,
    reifyBoundWithRefs,
    reifyBoundWithRefsOnConstraint,
  )
import MLF.Reify.Type (reifyTypeWithNamedSetRefs)
import MLF.Reify.TypeOps (freeTypeVarRefsType)
import MLF.Types.Elab
  ( Ty (..),
    TypeBinderRef,
    typeBinderIdentityFromNode,
    typeBinderRefFromIdentity,
    typeBinderRefIdentity,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
  )
import MLF.Types.Identity (typeBinderIdentityGeneratedUnique)
import MLF.Util.ElabError (ElabError (..))
import MLF.Util.Graph (reachableFrom)
import MLF.Util.Names (alphaName)
import MLF.Util.Trace (traceWhen)

traceBinderPlanEnabled :: Bool -> String -> a -> a
traceBinderPlanEnabled = traceWhen

traceBinderPlanEnabledM :: Bool -> String -> Either ElabError ()
traceBinderPlanEnabledM enabled msg =
  traceBinderPlanEnabled enabled msg (Right ())

-- | Binder candidates proved to belong to a descendant scheme whose root is
-- inside the selected result.  They are not selected eagerly: the body reify
-- step below must still prove that their exact identities survive free in the
-- selected root.  Keeping this as an explicit plan prevents finalization from
-- inventing quantifiers after it encounters an escaped child reference.
newtype RootBodyClosurePlan = RootBodyClosurePlan
  { rootBodyClosureCandidateNodes :: [NodeId]
  }

-- | Quotient dependency-ordered graph binders by semantic identity while
-- retaining a route for every graph key.  The first key in dependency order
-- owns the declaration; later keys with the same identity reuse its named ref.
buildBinderIdentityQuotient ::
  (Int -> TypeBinderRef) ->
  [Int] ->
  ([(Int, TypeBinderRef)], IntMap.IntMap TypeBinderRef)
buildBinderIdentityQuotient refForKey =
  go Map.empty 0 [] IntMap.empty
  where
    go _ _ declarations routes [] =
      (reverse declarations, routes)
    go refsByIdentity declarationIndex declarations routes (key : rest) =
      let sourceRef = refForKey key
          identity = typeBinderRefIdentity sourceRef
       in case Map.lookup identity refsByIdentity of
            Just declarationRef ->
              go
                refsByIdentity
                declarationIndex
                declarations
                (IntMap.insert key declarationRef routes)
                rest
            Nothing ->
              let declarationRef =
                    typeBinderRefFromIdentity
                      identity
                      (alphaName declarationIndex key)
               in go
                    (Map.insert identity declarationRef refsByIdentity)
                    (declarationIndex + 1)
                    ((key, declarationRef) : declarations)
                    (IntMap.insert key declarationRef routes)
                    rest

validateBinderIdentityQuotient ::
  (Int -> TypeBinderRef) ->
  [Int] ->
  [(Int, TypeBinderRef)] ->
  IntMap.IntMap TypeBinderRef ->
  [String]
validateBinderIdentityQuotient refForKey orderedKeys declarations routes =
  duplicateDeclarationErrors ++ routeErrors ++ unexpectedRouteErrors
  where
    declarationIdentityCounts =
      Map.fromListWith
        (+)
        [ (typeBinderRefIdentity ref, 1 :: Int)
        | (_, ref) <- declarations
        ]
    duplicateDeclarationErrors =
      [ "semantic binder identity declared more than once: " ++ show identity
      | (identity, count) <- Map.toList declarationIdentityCounts
      , count > 1
      ]
    routeErrors =
      concatMap validateRoute orderedKeys
    validateRoute key =
      case IntMap.lookup key routes of
        Nothing -> ["missing semantic binder route for graph key " ++ show key]
        Just routeRef
          | typeBinderRefsSameIdentity routeRef (refForKey key) -> []
          | otherwise ->
              [ "semantic binder route changed identity for graph key "
                  ++ show key
                  ++ ": expected "
                  ++ show (typeBinderRefIdentity (refForKey key))
                  ++ ", got "
                  ++ show (typeBinderRefIdentity routeRef)
              ]
    expectedRouteKeys = IntSet.fromList orderedKeys
    unexpectedRouteErrors =
      [ "unexpected semantic binder route for graph key " ++ show key
      | key <- IntMap.keys routes
      , not (IntSet.member key expectedRouteKeys)
      ]

buildBinderPlan :: BinderPlanInput p -> Either ElabError BinderPlan
buildBinderPlan BinderPlanInput {..} = do
  let traceGeneralize = traceBinderPlanEnabled bpiDebugEnabled
      traceGeneralizeM = traceBinderPlanEnabledM bpiDebugEnabled
      constraint = bpiConstraint
      nodes = bpiNodes
      canonical = bpiCanonical
      canonKey = bpiCanonKey
      bindParents = bpiBindParents
      mbBindParentsGa = bpiBindParentsGa
      scopeRootC = bpiScopeRootC
      scopeGen = bpiScopeGen
      target0 = bpiTarget0
      targetBound = bpiTargetBound
      targetIsSchemeRoot = bpiTargetIsSchemeRoot
      targetIsBaseLike = bpiTargetIsBaseLike
      boundUnderOtherGen = bpiBoundUnderOtherGen
      binders0 = bpiBinders0
      gammaAlias = bpiGammaAlias
      baseGammaSet = bpiBaseGammaSet
      baseGammaRep = bpiBaseGammaRep
      baseGammaRepSet = bpiBaseGammaRepSet
      solvedToBasePref = bpiSolvedToBasePref
      reachable = bpiReachable
      reachableForBinders = bpiReachableForBinders
      reachableType = bpiReachableType
      reachableTypeStructural = bpiReachableTypeStructural
      escapedFrozenForallBinders = bpiEscapedFrozenForallBinders
      escapedFrozenForallBinderKeys =
        IntSet.fromList
          (map canonKey escapedFrozenForallBinders)
      escapedSourceBinderOccurrences =
        bpiEscapedSourceBinderOccurrences
      escapedSourceBinderOccurrenceKeys =
        IntSet.fromList
          (map canonKey escapedSourceBinderOccurrences)
      typeRoot0 = bpiTypeRoot0
      typeRoot = bpiTypeRoot
      typeRootFromBoundVar = bpiTypeRootFromBoundVar
      liftToForall = bpiLiftToForall
      reachableFromWithBounds = bpiReachableFromWithBounds
      resForReify = bpiResForReify
      gammaKeyFor = bpiGammaKeyFor
      nestedSchemeInteriorSet = bpiNestedSchemeInteriorSet
      boundIsSchemeRootVar = bpiBoundIsSchemeRootVar
      boundIsSchemeRootAll = bpiBoundIsSchemeRootAll
      isNestedSchemeBound = bpiIsNestedSchemeBound
      schemeRootKeySet = bpiSchemeRootKeySet
      schemeRootByBody = bpiSchemeRootByBody
      schemeRootOwner = bpiSchemeRootOwner
      schemeRootOwnerBase = bpiSchemeRootOwnerBase
      schemeRootByBodyBase = bpiSchemeRootByBodyBase
      aliasBinderBases = bpiAliasBinderBases
      orderCandidates = bpiOrderBinderCandidates
      requiredGamma = bpiRequiredGamma
      sourceBinderRefs = bpiSourceBinderRefs
      sourceRefForLiveKey k =
        case IntMap.lookup k sourceBinderRefs of
          Just ref -> Just ref
          Nothing -> do
            baseNode <- IntMap.lookup k solvedToBasePref
            IntMap.lookup (getNodeId baseNode) sourceBinderRefs
      termUsedRootBinderGroups =
        IntMap.fromListWith (++)
          [ (canonKey node, [ref])
          | ref <- bpiTermUsedRootBinderRefs
          , Just node <- [typeBinderRefNode ref]
          ]
      termUsedAuthorityFor key candidateRef =
        case sourceRefForLiveKey key of
          Just sourceRef -> sourceRef
          Nothing -> candidateRef
      termUsedRequestedAuthorities =
        foldr insertDistinctTermUsedAuthority []
          [ termUsedAuthorityFor key candidateRef
          | (key, candidateRefs) <- IntMap.toList termUsedRootBinderGroups
          , candidateRef <- candidateRefs
          ]
      insertDistinctTermUsedAuthority ref refs
        | any (typeBinderRefsSameIdentity ref) refs = refs
        | otherwise = ref : refs
      termUsedRootBinderRefsByKey =
        IntMap.mapMaybeWithKey
          ( \key refs ->
              case (lookupNodeInMap nodes (NodeId key), refs) of
                (Just TyVar {}, ref : _) -> Just ref
                -- An exact source identity can survive in a checked
                -- construction after its graph occurrence has solved to a
                -- structural node (most commonly bottom).  The source
                -- sidecar is declaration authority for that identity, and
                -- the structural carrier still supplies its construction
                -- bound to the dependency planner below.
                (Just _, ref : _)
                  | Just _ <- sourceRefForLiveKey key -> Just ref
                _ -> Nothing
          )
          termUsedRootBinderGroups
      termUsedLiveAuthorities =
        [ termUsedAuthorityFor key candidateRef
        | (key, candidateRef) <-
            IntMap.toList termUsedRootBinderRefsByKey
        ]
      termUsedRootBinderNodes =
        [ NodeId key
        | key <- IntMap.keys termUsedRootBinderRefsByKey
        ]
      frozenLocalSchemeResultNodes =
        case (scopeGen, mbBindParentsGa) of
          (Just gid, Just ga) ->
            IntMap.elems $
              IntMap.fromList
                [ (solvedKey, solvedNode)
                | (baseKey, solvedNode0) <-
                    IntMap.toList (gbiBaseToSolved ga),
                  let solvedNode = canonical solvedNode0,
                  let solvedKey = getNodeId solvedNode,
                  IntSet.member solvedKey reachableType,
                  Just TyVar {} <- [IntMap.lookup solvedKey nodes],
                  Just TyVar {} <-
                    [ lookupNodeIn
                        (cNodes (gbiBaseConstraint ga))
                        (NodeId baseKey)
                    ],
                  VarStore.lookupVarBound
                    (gbiBaseConstraint ga)
                    (NodeId baseKey)
                    == Nothing,
                  IntMap.lookup
                    (nodeRefKey (typeRef (NodeId baseKey)))
                    (gbiBindParentsBase ga)
                    == Just (GenRef gid, BindFlex),
                  any
                    ( \gen ->
                        gnId gen == gid
                          && any
                            ((== solvedNode) . canonical)
                            (gnSchemes gen)
                    )
                    (NodeAccess.allGenNodes constraint)
                ]
          _ -> []
      frozenLocalSchemeResultKeys =
        IntSet.fromList
          (map getNodeId frozenLocalSchemeResultNodes)
      namedUnderGa =
        IntMap.elems $
          IntMap.fromList
            [ (getNodeId node, node)
            | node <-
                bpiNamedUnderGa
                  ++ frozenLocalSchemeResultNodes
            ]
      namedUnderGaSet =
        IntSet.union
          bpiNamedUnderGaSet
          frozenLocalSchemeResultKeys

  let binders0Adjusted =
        let freeVarsFromBound =
              case targetBound of
                Just bnd ->
                  [ canonical (NodeId nid)
                    | nid <- IntSet.toList (reachableFromWithBounds bnd),
                      case lookupNodeInMap nodes (NodeId nid) of
                        Just TyVar {} -> not (VarStore.isEliminatedVar constraint (NodeId nid))
                        _ -> False
                  ]
                Nothing -> []
            targetC = canonical target0
            activeBinders =
              [ v
                | v <- binders0,
                  IntSet.member (canonKey v) reachableForBinders
              ]
            onlyTarget = case activeBinders of
              [] -> True
              [v] -> canonical v == targetC
              _ -> False
            shouldAddExtras = targetIsSchemeRoot && onlyTarget && not (boundIsSchemeRootAll target0)
            extras = if shouldAddExtras then freeVarsFromBound else []
         in binders0 ++ extras ++ namedUnderGa
  traceGeneralizeM
    ( "generalizeAt: bindersAdjusted="
        ++ show binders0Adjusted
        ++ " target0="
        ++ show target0
        ++ " targetBound="
        ++ show targetBound
        ++ " targetIsBaseLike="
        ++ show targetIsBaseLike
        ++ " typeRoot0="
        ++ show typeRoot0
        ++ " typeRoot="
        ++ show typeRoot
        ++ " typeRootFromBoundVar="
        ++ show typeRootFromBoundVar
        ++ " reachable="
        ++ show (IntSet.toList reachableForBinders)
        ++ " baseGammaRep="
        ++ show (IntMap.toList baseGammaRep)
        ++ " solvedToBasePref="
        ++ show (IntMap.toList solvedToBasePref)
        ++ " gammaAlias="
        ++ show (IntMap.toList gammaAlias)
        ++ case mbBindParentsGa of
          Nothing -> ""
          Just ga ->
            " baseGammaDetails="
              ++ show
                [ ( baseKey
                  , lookupNodeIn
                      (cNodes (gbiBaseConstraint ga))
                      (NodeId baseKey)
                  , VarStore.lookupVarBound
                      (gbiBaseConstraint ga)
                      (NodeId baseKey)
                      >>= lookupNodeIn
                        (cNodes (gbiBaseConstraint ga))
                  , representativeKey
                  , [ (liveKey, IntMap.lookup liveKey nodes)
                    | (liveKey, baseNode) <-
                        IntMap.toList solvedToBasePref
                    , getNodeId baseNode == baseKey
                    ]
                  )
                | (baseKey, representativeKey) <-
                    IntMap.toList baseGammaRep
                ]
    )
  case
      [ (key, refs)
      | (key, refs) <- IntMap.toList termUsedRootBinderGroups
      , case refs of
          [] -> False
          firstRef : rest ->
            any
              ( not
                  . typeBinderRefsSameIdentity
                    (termUsedAuthorityFor key firstRef)
                  . termUsedAuthorityFor key
              )
              rest
      ]
    of
      [] -> pure ()
      conflicts ->
        Left
          ( ValidationFailed
              [ "construction-used root binder key has conflicting identities"
              , "  conflicts: " ++ show conflicts
              ]
          )
  case
      [ requiredRef
      | requiredRef <- termUsedRequestedAuthorities
      , not
          ( any
              (typeBinderRefsSameIdentity requiredRef)
              termUsedLiveAuthorities
          )
      ]
    of
      [] -> pure ()
      missing ->
        Left
          ( ValidationFailed
              [ "construction-used root binder has no live graph route"
              , "  missing declarations: " ++ show missing
              , "  candidate routes: " ++ show termUsedRootBinderGroups
              , "  source binder refs: " ++ show sourceBinderRefs
              ]
          )
  let isTargetSchemeBinder =
        isTargetSchemeBinderFor canonical constraint target0 targetIsBaseLike
  let extraReachable =
        case (scopeGen, lookupNodeInMap nodes typeRoot0) of
          (Just _, _) -> []
          (_, Just TyVar {}) -> []
          (Nothing, _) ->
            [ canonical v
              | nid <- IntSet.toList reachable,
                Just TyVar {} <- [lookupNodeInMap nodes (NodeId nid)],
                let v = NodeId nid,
                not (VarStore.isEliminatedVar constraint (canonical v)),
                not (elem (canonical v) binders0Adjusted),
                case IntMap.lookup (nodeRefKey (typeRef (canonical v))) bindParents of
                  Just (GenRef _, _) -> False
                  _ -> True
            ]
      -- A local-closure decision is provisional until the checked
      -- construction reports which graph declarations remain free in the
      -- returned term.  Such a term-used declaration must be rebuilt at this
      -- root even if an inner constructor originally owned the same graph
      -- key; its explicit identity is stronger than the pre-elaboration
      -- closure classification.
      locallyClosedGammaKeys =
        IntSet.difference
          ( IntSet.fromList
              [ canonKey (NodeId key)
              | key <- IntSet.toList bpiLocallyClosedGammaNodes
              ]
          )
          (IntSet.fromList (IntMap.keys termUsedRootBinderRefsByKey))
      ownerWithinCurrentScope ownerGen =
        case
          Binding.bindingPathToRootLocal
            bindParents
            (GenRef ownerGen)
        of
          Right path -> scopeRootC `elem` path
          Left _ -> False
      rootBodyCandidateRef child =
        case sourceRefForLiveKey (canonKey child) of
          Just sourceRef -> sourceRef
          Nothing ->
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode child)
              ("t" ++ show (getNodeId child))
      rootBodyClosurePlan =
        RootBodyClosurePlan
          { rootBodyClosureCandidateNodes =
              [ canonical child
              | (child, TyVar {}) <- toListNode (cNodes constraint)
              , let childKey = canonKey child
              , IntSet.member childKey reachableType
              , not (VarStore.isEliminatedVar constraint (canonical child))
              , Just (GenRef ownerGen, BindFlex) <-
                  [IntMap.lookup (nodeRefKey (typeRef child)) bindParents]
              , ownerWithinCurrentScope ownerGen
              -- Candidate collection deliberately includes nested-scheme
              -- interiors.  The provisional body reification below selects
              -- only identities that actually survive free in the chosen
              -- result, so a nested forall still closes its own declarations
              -- while an escaped @alpha > S(operated)@ remains constructible.
              -- A nested node that already carries a source-binder route is
              -- different: its enclosing source scheme owns that identity,
              -- so treating the graph occurrence as a local declaration
              -- would create a spurious cycle between the scheme wrapper and
              -- its routed occurrence.
              , not
                  ( IntSet.member childKey nestedSchemeInteriorSet
                      && case sourceRefForLiveKey childKey of
                        Just sourceRef ->
                          case
                            typeBinderIdentityGeneratedUnique
                              (typeBinderRefIdentity sourceRef)
                          of
                            Just _ -> True
                            Nothing -> False
                        Nothing -> False
                  )
              , not (IntSet.member childKey locallyClosedGammaKeys)
              , not
                  ( any
                      ( typeBinderRefsSameIdentity
                          (rootBodyCandidateRef child)
                      )
                      bpiAmbientBinderRefs
                  )
              ]
          }
      bindersCandidates =
        [ binder
        | binder <-
            binders0Adjusted
              ++ extraReachable
              ++ namedUnderGa
              ++ escapedFrozenForallBinders
              ++ escapedSourceBinderOccurrences
              ++ termUsedRootBinderNodes
              ++ rootBodyClosureCandidateNodes rootBodyClosurePlan
        , not (IntSet.member (canonKey binder) locallyClosedGammaKeys)
        ]
      canonicalBinder v =
        let vC = canonical v
         in case lookupNodeInMap nodes vC of
              Just TyVar {} -> vC
              _ ->
                case lookupNodeInMap nodes v of
                  Just TyVar {} -> v
                  _ -> vC
      canonicalizeBinder v =
        let vC = canonicalBinder v
            vKey = getNodeId vC
         in case IntMap.lookup vKey gammaAlias of
              Just repKey
                | IntMap.member repKey requiredGamma ->
                    canonicalBinder (NodeId repKey)
                | IntSet.member vKey baseGammaRepSet -> vC
                | otherwise ->
                    canonicalBinder (NodeId repKey)
              Nothing -> vC
      normalizedBinders =
        [ canonicalizeBinder v
          | v <- bindersCandidates
        ]
      bindersCandidatesCanonical =
        IntMap.elems $
          IntMap.fromList
            [ (getNodeId v, v)
              | v <- normalizedBinders
              , not (IntSet.member (canonKey v) locallyClosedGammaKeys)
            ]
      locallyClosedRequiredGammaKeys =
        [ (key, rgbExteriorNode requirement)
        | (key, requirement) <- IntMap.toList requiredGamma
        , IntSet.member (canonKey (NodeId key)) locallyClosedGammaKeys
            || IntSet.member
                (canonKey (rgbExteriorNode requirement))
                locallyClosedGammaKeys
        ]
  traceGeneralizeM
    ( "generalizeAt: rootBodyClosureCandidates="
        ++ show (rootBodyClosureCandidateNodes rootBodyClosurePlan)
        ++ " reachableType="
        ++ show (IntSet.toList reachableType)
    )
  case locallyClosedRequiredGammaKeys of
    [] -> pure ()
    _ ->
      Left
        ( ValidationFailed
            [ "a root Gamma requirement is already closed by a nested construction"
            , "  locally closed Gamma keys: " ++ show (IntSet.toList locallyClosedGammaKeys)
            , "  conflicting required Gamma keys: " ++ show locallyClosedRequiredGammaKeys
            ]
        )
  let typeRootForScheme = liftToForall typeRoot
      aliasEnv =
        AliasEnv { aeCanonical = canonical,
            aeConstraint = constraint,
            aeNodes = cNodes constraint,
            aeBindParents = bindParents,
            aeDepthMap = gammaAlias,
            aeScopeSchemeRoots = nestedSchemeInteriorSet,
            aeNodeChildren = reachableFromWithBounds
          }
      isSchemeRootAlias v =
        IntSet.member (getNodeId (canonical v)) schemeRootKeySet
          && canonical v /= canonical target0
          && case VarStore.lookupVarBound constraint (canonical v) of
            Just bnd ->
              let bndC = canonical bnd
               in bndC == canonical typeRoot || bndC == canonical typeRootForScheme
            Nothing -> False
      boundIsSchemeBodyAlias v =
        case VarStore.lookupVarBound constraint (canonical v) of
          Just bnd ->
            let bndC = canonical bnd
             in bndC == canonical typeRoot || bndC == canonical typeRootForScheme
          Nothing -> False
      boundIsStructuralAlias v =
        case VarStore.lookupVarBound constraint (canonical v) of
          Just bnd | bndC == canonical typeRoot || bndC == canonical typeRootForScheme ->
            case lookupNodeInMap nodes bndC of
              Just TyArrow {} -> True
              Just TyForall {} -> True
              Just TyMu {} -> True
              Just TyExp {} -> True
              _ -> False
            where
              bndC = canonical bnd
          _ -> False
      boundIsTypeRootAlias v =
        boundIsSchemeBodyAlias v
      isUnselectedStructuralRootCarrier v =
        case lookupNodeInMap nodes (canonical target0) of
          Just TyVar {} -> False
          _ ->
            canonical v /= canonical target0
              && case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd -> canonical bnd == canonical typeRoot
                Nothing -> False
      boundHasNamedOutsideGammaFor v =
        case mbBindParentsGa of
          Just ga
            | Just baseK <- IntMap.lookup (getNodeId (canonical v)) solvedToBasePref ->
                let baseConstraint = gbiBaseConstraint ga
                    baseNodes = cNodes baseConstraint
                    baseParents = gbiBindParentsBase ga
                    scopeGenBase = scopeGen
                    boundSchemeOwnerBase bnd =
                      case IntMap.lookup (getNodeId bnd) schemeRootOwnerBase of
                        Just gid -> Just gid
                        Nothing ->
                          case IntMap.lookup (getNodeId bnd) schemeRootByBodyBase of
                            Just root ->
                              IntMap.lookup (getNodeId root) schemeRootOwnerBase
                            Nothing -> Nothing
                    allowBoundTraversalBase bnd =
                      case boundSchemeOwnerBase bnd of
                        Nothing -> True
                        Just gid ->
                          case scopeGenBase of
                            Just scopeGid -> gid == scopeGid
                            Nothing -> False
                    reachableFromWithBoundsBase root0 =
                      let children nid =
                            case lookupNodeIn baseNodes nid of
                              Nothing -> []
                              Just node ->
                                case node of
                                  TyVar {tnBound = Just bnd}
                                    | allowBoundTraversalBase bnd ->
                                        structuralChildrenWithBounds node
                                  _ ->
                                    structuralChildren node
                       in reachableFrom getNodeId id children root0
                    isNamedOutsideBase nidInt =
                      case lookupNodeIn baseNodes (NodeId nidInt) of
                        Just TyVar {} ->
                          case IntMap.lookup (nodeRefKey (TypeRef (NodeId nidInt))) baseParents of
                            Just (GenRef _, _) ->
                              not (IntSet.member nidInt baseGammaSet)
                            _ -> False
                        _ -> False
                 in case VarStore.lookupVarBound baseConstraint baseK of
                      Just bnd ->
                        let reachableBound = reachableFromWithBoundsBase bnd
                         in any isNamedOutsideBase (IntSet.toList reachableBound)
                      Nothing -> False
          _ ->
            case VarStore.lookupVarBound constraint (canonical v) of
              Just bnd ->
                let reachableBound = reachableFromWithBounds bnd
                    binderKey = getNodeId (canonical v)
                    isNamedOutside nidInt =
                      let nidC = canonical (NodeId nidInt)
                          keyC = getNodeId nidC
                       in case lookupNodeInMap nodes nidC of
                            Just TyVar {} ->
                              if IntSet.member keyC nestedSchemeInteriorSet
                                then False
                                else case IntMap.lookup (nodeRefKey (typeRef nidC)) bindParents of
                                  Just (GenRef _, _) ->
                                    not (IntSet.member (gammaKeyFor binderKey keyC) namedUnderGaSet)
                                  _ -> False
                            _ -> False
                 in any isNamedOutside (IntSet.toList reachableBound)
              Nothing -> False
      boundMentionsSelfAlias =
        boundMentionsSelfAliasFor aliasEnv
      aliasBoundIsBottomOrNone v =
        isSchemeRootAlias v
          && not (isTargetSchemeBinder v)
          && case VarStore.lookupVarBound constraint (canonical v) of
            Nothing -> True
            Just bnd ->
              let bndC = canonical bnd
               in case (lookupNodeInMap nodes bndC, VarStore.lookupVarBound constraint bndC) of
                    (Just TyVar {}, Nothing) -> True
                    (Just TyBottom {}, _) -> True
                    _ -> False
      boundIsVarAlias v =
        case VarStore.lookupVarBound constraint (canonical v) of
          Just bnd ->
            case lookupNodeInMap nodes (canonical bnd) of
              Just TyVar {} -> True
              _ -> False
          _ -> False
      isTypeRootBinder v = maybe False (\v0 -> canonical v == canonical v0) typeRootFromBoundVar
      aliasBinderIsTrivial v =
        all
          ($ v)
          [ not . isTargetSchemeBinder,
            not . isTypeRootBinder,
            boundIsVarAlias,
            \x -> not (inReachableTypeStructural x),
            not . boundHasNamedOutsideGammaFor,
            not . boundMentionsSelfAlias
          ]
        where
          inReachableTypeStructural x = IntSet.member (getNodeId (canonical x)) reachableTypeStructural
      aliasBinderIsRedundant v inGamma =
        all
          ($ v)
          [ not . isTargetSchemeBinder,
            not . isTypeRootBinder,
            \x -> isSchemeRootAlias x || boundIsTypeRootAlias x,
            \x -> not (inReachableType x),
            \_ -> not inGamma || boundIsTypeRootAlias v,
            not . boundIsSchemeBodyAlias
          ]
        where
          inReachableType x = IntSet.member (getNodeId (canonical x)) reachableType
  let isStructuralBinderOwner node =
        case node of
          TyForall {} -> True
          TyMu {} -> True
          _ -> False
      leadingForallOwners = collectLeadingForallOwners IntSet.empty typeRoot
        where
          collectLeadingForallOwners seen root0 =
            let root = canonical root0
                rootKey = canonKey root
             in if IntSet.member rootKey seen
                  then IntSet.empty
                  else
                    let seen' = IntSet.insert rootKey seen
                     in case lookupNodeInMap nodes root of
                          Just TyVar {} ->
                            case VarStore.lookupVarBound constraint root of
                              Just bnd -> collectLeadingForallOwners seen' bnd
                              Nothing -> IntSet.empty
                          Just TyForall {tnBody = body} ->
                            IntSet.insert rootKey (collectLeadingForallOwners seen' body)
                          Just TyExp {tnBody = body} ->
                            collectLeadingForallOwners seen' body
                          _ -> IntSet.empty
      structuralOwnerForBinder v =
        let vC = canonical v
            lookupParent node =
              IntMap.lookup (nodeRefKey (typeRef node)) bindParents
            mbParent =
              case lookupParent vC of
                Just parent -> Just parent
                Nothing -> lookupParent v
         in case mbParent of
              Just (TypeRef parent0, _) ->
                let parent = canonical parent0
                    parentKey = canonKey parent
                 in case lookupNodeInMap nodes parent of
                      Just owner
                        | isStructuralBinderOwner owner
                        , IntSet.member parentKey reachableType ->
                            Just parentKey
                      _ -> Nothing
              _ -> Nothing
      nestedStructuralBinders =
        IntSet.fromList
          [ canonKey child
            | child <- bindersCandidatesCanonical,
              Just ownerKey <- [structuralOwnerForBinder child],
              not (IntSet.member ownerKey leadingForallOwners)
          ]
      nestedSourceStructuralRefs =
        [ ownerRef
          | ownerKey <- IntSet.toList reachableType,
            not (IntSet.member ownerKey leadingForallOwners),
            Just owner <- [lookupNodeInMap nodes (NodeId ownerKey)],
            isStructuralBinderOwner owner,
            Just ownerRef <- [sourceRefForLiveKey ownerKey]
        ]
      isNestedSourceStructuralBinder v =
        case sourceRefForLiveKey (canonKey v) of
          Nothing -> False
          Just ref -> any (typeBinderRefsSameIdentity ref) nestedSourceStructuralRefs
      candidateAuthorityRef v =
        case sourceRefForLiveKey (canonKey v) of
          Just sourceRef -> sourceRef
          Nothing ->
            case IntMap.lookup (canonKey v) termUsedRootBinderRefsByKey of
              Just termUsedRef -> termUsedRef
              Nothing ->
                typeBinderRefFromIdentity
                  (typeBinderIdentityFromNode (canonicalBinder v))
                  ("t" ++ show (canonKey v))
      candidateIsAmbient v =
        case IntMap.lookup (canonKey v) termUsedRootBinderRefsByKey of
          -- An owner-final construction certificate proves that this exact
          -- graph identity is emitted by the current constructor.  That
          -- authority wins over an identity-coincident provisional slot in
          -- the incoming Gamma; otherwise the root plan would omit the
          -- declaration that the checked ETyAbs spine already emits.
          Just _ -> False
          Nothing ->
            any
              (typeBinderRefsSameIdentity (candidateAuthorityRef v))
              bpiAmbientBinderRefs
  let binderCandidateKeys =
        IntSet.fromList
          [ canonKey v
          | v <- bindersCandidatesCanonical
          , not (candidateIsAmbient v)
          ]
  let binders =
        [ canonicalBinder v
          | v <- bindersCandidatesCanonical,
            let vKey = canonKey v,
            not (IntSet.member vKey locallyClosedGammaKeys),
            not (candidateIsAmbient v),
            let gammaKey =
                  case IntMap.lookup vKey gammaAlias of
                    Just repKey -> repKey
                    Nothing -> vKey,
            let keepTypeRootBinder =
                  case typeRootFromBoundVar of
                    Just v0 -> canonKey v0 == vKey
                    Nothing -> False,
            let inGamma =
                  case mbBindParentsGa of
                    Just _ ->
                      let keysToCheck = [vKey, gammaKey]
                          inBase =
                            any
                              ( \k ->
                                  case IntMap.lookup k solvedToBasePref of
                                    Just baseN -> IntSet.member (getNodeId baseN) baseGammaSet
                                    Nothing -> False
                              )
                              keysToCheck
                       in inBase || IntSet.member gammaKey namedUnderGaSet
                    Nothing ->
                      IntSet.member gammaKey namedUnderGaSet
                        || IntSet.member gammaKey aliasBinderBases
                        || IntSet.member vKey aliasBinderBases,
            case scopeRootC of
              GenRef _ ->
                -- The selected target scheme is direct construction evidence:
                -- it remains a binder candidate even when the ordinary
                -- reachability walk cannot recover the frozen scheme root.
                ( inGamma
                    || IntSet.member vKey aliasBinderBases
                    || isTargetSchemeBinder v
                    || IntSet.member vKey escapedFrozenForallBinderKeys
                    || IntSet.member
                      vKey
                      escapedSourceBinderOccurrenceKeys
                )
                  && ( IntSet.member vKey reachableForBinders
                         || isTargetSchemeBinder v
                         || IntSet.member
                           vKey
                           escapedSourceBinderOccurrenceKeys
                     )
              TypeRef _ -> IntSet.member vKey reachableForBinders,
            case mbBindParentsGa of
              Just _ ->
                if keepTypeRootBinder
                  then True
                  else case IntMap.lookup vKey solvedToBasePref of
                    Just baseN ->
                      case IntMap.lookup (getNodeId baseN) baseGammaRep of
                        Just repKey ->
                          repKey == vKey
                            || not (IntSet.member repKey binderCandidateKeys)
                            || IntSet.member vKey baseGammaRepSet
                        Nothing -> True
                    Nothing -> True
              Nothing -> True,
            not
              ( IntSet.member (canonKey v) nestedSchemeInteriorSet
                  && not (isTargetSchemeBinder v)
                  && not (IntSet.member vKey aliasBinderBases)
                  && not
                    ( IntSet.member
                        vKey
                        escapedFrozenForallBinderKeys
                    )
                  && not
                    ( IntSet.member
                        vKey
                        escapedSourceBinderOccurrenceKeys
                    )
              ),
            not (isNestedSchemeBound v && not inGamma && not (isTargetSchemeBinder v)),
            not (boundIsSchemeRootVar v && not (isTargetSchemeBinder v) && not (hasExplicitBound v) && not inGamma),
            not
              ( boundIsSchemeRootAll v
                  && not (isTargetSchemeBinder v)
                  && not (boundHasNamedOutsideGammaFor v)
                  && not (IntSet.member (canonKey v) reachableForBinders)
              ),
            not
              ( isSchemeRootAlias v
                  && not (isTargetSchemeBinder v)
                  && not (boundHasNamedOutsideGammaFor v)
                  && not (boundMentionsSelfAlias v)
                  && (not inGamma || boundIsTypeRootAlias v)
              ),
            not
              ( boundIsSchemeBodyAlias v
                  && not (isTargetSchemeBinder v)
                  && not inGamma
              ),
            not
              ( boundIsStructuralAlias v
                  && canonical v /= canonical target0
                  && not (boundIsSchemeBodyAlias v)
              ),
            not (aliasBoundIsBottomOrNone v && not inGamma),
            not (aliasBinderIsTrivial v),
            not (aliasBinderIsRedundant v inGamma),
            not (isUnselectedStructuralRootCarrier v),
            not
              ( IntSet.member (canonKey v) nestedStructuralBinders
                  && not
                    ( IntSet.member
                        vKey
                        escapedSourceBinderOccurrenceKeys
                    )
              ),
            not
              ( isNestedSourceStructuralBinder v
                  && not
                    ( IntSet.member
                        vKey
                        escapedSourceBinderOccurrenceKeys
                    )
              )
        ]
  traceGeneralizeM
    ( "generalizeAt: binder filters="
        ++ show
          [ let vKey = canonKey v
                gammaKey =
                  case IntMap.lookup vKey gammaAlias of
                    Just repKey -> repKey
                    Nothing -> vKey
                inGammaDbg =
                  case mbBindParentsGa of
                    Just _ ->
                      let keysToCheck = [vKey, gammaKey]
                          inBase =
                            any
                              ( \k ->
                                  case IntMap.lookup k solvedToBasePref of
                                    Just baseN -> IntSet.member (getNodeId baseN) baseGammaSet
                                    Nothing -> False
                              )
                              keysToCheck
                       in inBase
                    Nothing -> IntSet.member gammaKey namedUnderGaSet
             in ( v,
                  IntSet.member vKey reachable,
                  isNestedSchemeBound v,
                  boundIsSchemeRootVar v,
                  boundIsSchemeRootAll v,
                  boundIsTypeRootAlias v,
                  boundUnderOtherGen,
                  IntSet.member vKey nestedStructuralBinders || isNestedSourceStructuralBinder v,
                  inGammaDbg,
                  IntMap.lookup (nodeRefKey (typeRef (canonical v))) bindParents
                )
            | v <- binders0Adjusted
          ]
    )
  let requiredGammaBinders :: [NodeId]
      requiredGammaBinders =
        [ NodeId liveKey
          | liveKey <- IntMap.keys requiredGamma
        ]
      termUsedRootBinders =
        [ canonicalBinder node
        | node <- termUsedRootBinderNodes
        , not (candidateIsAmbient node)
        ]
      binders' =
        traceGeneralize
          ( "generalizeAt: bindersFiltered="
              ++ show binders
              ++ " requiredGammaBinders="
              ++ show requiredGammaBinders
              ++ " termUsedRootBinders="
              ++ show termUsedRootBinders
              ++ " typeRoot="
              ++ show typeRoot
          )
          [ binder
          | binder <- binders ++ requiredGammaBinders ++ termUsedRootBinders
          , not (IntSet.member (canonKey binder) locallyClosedGammaKeys)
          ]
      bindersCanon =
        IntMap.elems $
          IntMap.fromList
            [ (getNodeId v, v)
              | v <- binders'
            ]
  let binderIds = map getNodeId bindersCanon
      -- A required Gamma bound can mention a source-projected existential
      -- whose graph occurrence belongs to a nested scheme, or has already
      -- solved to a structural node (including bottom), and was therefore
      -- filtered out of the ordinary variable candidates.  The source
      -- sidecar is exact construction provenance for that identity.  Retain
      -- the live carrier in the dependency domain unless the enclosing
      -- source boundary explicitly declares the same identity ambient.
      sourceDependencyRefGroups =
        IntMap.fromListWith
          (++)
          [ (getNodeId sourceNodeC, [sourceRef])
          | (sourceKey, sourceRef) <- IntMap.toList sourceBinderRefs
          , not
              ( any
                  (typeBinderRefsSameIdentity sourceRef)
                  bpiAmbientBinderRefs
              )
          , let sourceNodeC = canonical (NodeId sourceKey)
          , IntMap.member (getNodeId sourceNodeC) nodes
          ]
      sourceDependencyIdentityConflicts =
        [ (carrierKey, refs)
        | (carrierKey, refs@(firstRef : remainingRefs)) <-
            IntMap.toList sourceDependencyRefGroups
        , any
            (not . typeBinderRefsSameIdentity firstRef)
            remainingRefs
        ]
      sourceDependencyRefs =
        IntMap.mapMaybe
          ( \refs ->
              case refs of
                ref : _ -> Just ref
                [] -> Nothing
          )
          sourceDependencyRefGroups
      sourceDependencyBinders =
        map NodeId (IntMap.keys sourceDependencyRefs)
      sourceDependencyKeys =
        IntSet.fromList (map getNodeId sourceDependencyBinders)
  case sourceDependencyIdentityConflicts of
    [] -> pure ()
    conflicts ->
      Left
        ( ValidationFailed
            [ "one solved source-binder carrier has conflicting identities"
            , "  conflicts: " ++ show conflicts
            ]
        )
  let
      -- The exact operated type of a required Gamma binder is itself part of
      -- the construction plan.  Its graph-variable occurrences therefore
      -- belong to the dependency candidate domain even when the ordinary
      -- target walk cannot see them.  This is the application-result case
      -- where @alpha@ occurs only in @exterior > alpha -> alpha@: selecting
      -- @exterior@ must pull @alpha@ into the same binder plan.
      requiredGammaBoundDependencyBinders =
        IntMap.elems $
          IntMap.fromList
            [ (getNodeId dependencyNodeC, dependencyNodeC)
            | requirement <- IntMap.elems requiredGamma
            , dependencyRef <-
                freeTypeVarRefsType (rgbOperatedType requirement)
            , Just dependencyNode <- [typeBinderRefNode dependencyRef]
            , let dependencyNodeC = canonical dependencyNode
            , Just TyVar {} <-
                [IntMap.lookup (getNodeId dependencyNodeC) nodes]
            , not
                ( IntSet.member
                    (canonKey dependencyNodeC)
                    locallyClosedGammaKeys
                )
            , let dependencyAuthorityRef =
                    case sourceRefForLiveKey (getNodeId dependencyNodeC) of
                      Just sourceRef -> sourceRef
                      Nothing ->
                        typeBinderRefFromIdentity
                          (typeBinderIdentityFromNode dependencyNodeC)
                          ("t" ++ show (getNodeId dependencyNodeC))
            , not
                ( any
                    (typeBinderRefsSameIdentity dependencyAuthorityRef)
                    bpiAmbientBinderRefs
                )
            ]
      requiredGammaBoundDependencyKeys =
        IntSet.fromList
          (map getNodeId requiredGammaBoundDependencyBinders)
      -- The selected binders seed dependency closure, but a selected
      -- binder's bound may mention a candidate filtered out of that seed.
      -- Keep the complete construction-time candidate domain available so
      -- closure can recover those dependencies (for example paper K's
      -- @c >= forall e. e -> a@ must pull @a@ back in through @c@'s bound).
      -- This is deliberately not a free-variable repair on the final type:
      -- every admitted dependency still comes from a binder candidate or a
      -- required Gamma construction.
      dependencyCandidateSet =
        IntSet.unions
          [ binderCandidateKeys
          , sourceDependencyKeys
          , requiredGammaBoundDependencyKeys
          , IntSet.fromList
              [ liveKey
              | liveKey <- IntMap.keys requiredGamma
              , not
                  ( IntSet.member
                      (canonKey (NodeId liveKey))
                      locallyClosedGammaKeys
                  )
              ]
          ]
      dependencyCandidateIds = IntSet.toList dependencyCandidateSet
      lookupBindingParent node =
        case
            IntMap.lookup
              (nodeRefKey (typeRef node))
              bindParents
          of
          Just parent -> Just parent
          Nothing ->
            IntMap.lookup
              (nodeRefKey (typeRef (canonical node)))
              bindParents
      frozenBaseNodeForLive ga node =
        case
            IntMap.lookup
              (getNodeId node)
              (gbiSolvedToBase ga)
          of
          Just baseNode -> Just baseNode
          Nothing ->
            case IntMap.lookup (getNodeId node) solvedToBasePref of
              Just baseNode -> Just baseNode
              Nothing ->
                case lookupNodeIn (cNodes (gbiBaseConstraint ga)) node of
                  Just _ -> Just node
                  Nothing -> Nothing
      isUnboundedBaseVar ga node =
        case lookupNodeIn (cNodes (gbiBaseConstraint ga)) node of
          Just TyVar {} ->
            VarStore.lookupVarBound
              (gbiBaseConstraint ga)
              node
              == Nothing
          _ -> False
      inheritedRigidAliasFor sourceKey = do
        ga <- mbBindParentsGa
        let source = canonical (NodeId sourceKey)
        TyVar {} <- IntMap.lookup (getNodeId source) nodes
        -- An exact source sidecar already names the declaration that this
        -- occurrence must use.  It is stronger than a graph-derived rigid
        -- alias, and replacing it here would later make the frozen base route
        -- disagree with the generalized substitution.
        guard (isNothing (sourceRefForLiveKey sourceKey))
        targetBoundNode <- VarStore.lookupVarBound constraint source
        let target = canonical targetBoundNode
        TyVar {} <- IntMap.lookup (getNodeId target) nodes
        guard (isNothing (sourceRefForLiveKey (getNodeId target)))
        let targetOwnedByCurrentScope =
              case scopeGen of
                Nothing -> False
                Just currentGen ->
                  any
                    ( \gen ->
                        gnId gen == currentGen
                          && any
                            ((== target) . canonical)
                            (gnSchemes gen)
                    )
                    (NodeAccess.allGenNodes constraint)
        if source == target
          || VarStore.lookupVarBound constraint target /= Nothing
          || targetOwnedByCurrentScope
          then Nothing
          else do
            (sourceParent, BindFlex) <- lookupBindingParent source
            (_, BindRigid) <- lookupBindingParent target
            baseSource <- frozenBaseNodeForLive ga source
            baseTarget <- frozenBaseNodeForLive ga target
            if sourceParent == scopeRootC
              && isUnboundedBaseVar ga baseSource
              && isUnboundedBaseVar ga baseTarget
              then
                Just
                  ( typeBinderRefFromIdentity
                      (typeBinderIdentityFromNode target)
                      ("__rigid" ++ show (getNodeId target))
                  )
              else Nothing
      inheritedRigidAliasRoutes =
        IntMap.fromList
          [ (sourceKey, targetRef)
          | sourceKey <- dependencyCandidateIds,
            Just targetRef <- [inheritedRigidAliasFor sourceKey]
          ]
      -- A root RaiseMerge constructs every certified result occurrence with
      -- its exterior declaration.  ReifyPlan publishes the same routes for
      -- final S', but binder dependency closure runs earlier and must see the
      -- construction quotient too; otherwise it can select a result carrier
      -- and then rediscover the surrounding result binder through that
      -- carrier's bound, manufacturing a dependency cycle.
      requiredGammaResultRouteGroups =
        IntMap.fromListWith
          (++)
          [ ( resultAliasKey
            , [ ( requiredKey
                , typeBinderRefFromIdentity
                    ( typeBinderIdentityFromNode
                        (rgbExteriorNode requirement)
                    )
                    ("t" ++ show requiredKey)
                )
              ]
            )
          | (requiredKey, requirement) <- IntMap.toList requiredGamma
          , resultRoot <- NonEmpty.toList (rgbResultRoots requirement)
          , resultAlias <- [resultRoot, canonical resultRoot]
          , let resultAliasKey = getNodeId resultAlias
          ]
      requiredGammaResultRouteConflicts =
        [ (resultKey, routes)
        | (resultKey, routes@((_, firstRef) : remainingRoutes)) <-
            IntMap.toList requiredGammaResultRouteGroups
        , any
            ( \(_, routeRef) ->
                not (typeBinderRefsSameIdentity firstRef routeRef)
            )
            remainingRoutes
        ]
      requiredGammaResultRoutes =
        IntMap.mapMaybe
          ( \routes ->
              case routes of
                route : _ -> Just route
                [] -> Nothing
          )
          requiredGammaResultRouteGroups
      requiredGammaResultSubst =
        IntMap.map snd requiredGammaResultRoutes
      nameForDep k = "t" ++ show k
      refForDep k =
        case IntMap.lookup k requiredGamma of
          Just requirement ->
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode (rgbExteriorNode requirement))
              (nameForDep k)
          Nothing ->
            case IntMap.lookup k requiredGammaResultRoutes of
              Just (_, requiredRef) -> requiredRef
              Nothing ->
                case IntMap.lookup k termUsedRootBinderRefsByKey of
                  Just _ -> candidateAuthorityRef (NodeId k)
                  Nothing ->
                    case IntMap.lookup k sourceDependencyRefs of
                      Just sourceRef -> sourceRef
                      Nothing ->
                        case IntMap.lookup k inheritedRigidAliasRoutes of
                          Just inheritedRef -> inheritedRef
                          Nothing ->
                            maybe
                              (typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId k)) (nameForDep k))
                              (\ref -> typeBinderRefFromIdentity (typeBinderRefIdentity ref) (nameForDep k))
                              (sourceRefForLiveKey k)
      dependencyKeyForCandidate key
        | IntMap.member key requiredGamma = key
        | Just (requiredKey, _) <-
            IntMap.lookup key requiredGammaResultRoutes = requiredKey
        | IntMap.member key termUsedRootBinderRefsByKey = key
        | Just _ <- sourceRefForLiveKey key = key
        | otherwise =
            case IntMap.lookup key solvedToBasePref of
              Just baseNode ->
                case IntMap.lookup (getNodeId baseNode) baseGammaRep of
                  -- The frozen Gamma representative is the declaration
                  -- authority for an otherwise anonymous solved expansion.
                  -- Route its dependencies to that declaration before
                  -- topological ordering; treating the expansion carrier as
                  -- a second binder can manufacture a dependency cycle that
                  -- does not exist in the frozen construction.
                  Just representativeKey -> representativeKey
                  Nothing -> key
              Nothing -> key
      depFromRef ref =
        case
          [ key
            | (key, candidateRef) <- IntMap.toList substDeps,
              typeBinderRefsSameIdentity candidateRef ref
          ]
        of
          key : _ -> Just (dependencyKeyForCandidate key)
          [] ->
            case typeBinderRefNode ref of
              Nothing -> Nothing
              Just liveNode ->
                Just (dependencyKeyForCandidate (getNodeId liveNode))
      depsFromRefs k allowed refs =
        [ dep
          | ref <- refs,
            Just dep <- [depFromRef ref],
            dep /= k,
            not
              ( typeBinderRefsSameIdentity
                  (refForDep k)
                  (refForDep dep)
              ),
            IntMap.notMember dep inheritedRigidAliasRoutes,
            IntSet.member dep allowed
        ]
      substDeps =
        IntMap.fromList
          [ (k, refForDep k)
            | k <- dependencyCandidateIds
          ]
      substDepsBase =
        case mbBindParentsGa of
          Just _ ->
            IntMap.fromListWith
              (\_ old -> old)
              [ ( getNodeId baseN
                , maybe
                    (typeBinderRefFromIdentity (typeBinderIdentityFromNode baseN) (nameForDep k))
                    (\ref -> typeBinderRefFromIdentity (typeBinderRefIdentity ref) (nameForDep k))
                    (IntMap.lookup (getNodeId baseN) sourceBinderRefs)
                )
                | k <- dependencyCandidateIds,
                  Just baseN <- [IntMap.lookup k solvedToBasePref]
              ]
          Nothing -> IntMap.empty
      substDepsFor k =
        IntMap.unions
          [ requiredGammaResultSubst
          , substDeps
          , sourceBinderRefs
          , IntMap.fromList
              [ (aliasKey, typeBinderRefFromIdentity (typeBinderIdentityFromNode (canonical (NodeId aliasKey))) (nameForDep binderKey))
                | (aliasKey, binderKey) <- IntMap.toList gammaAlias,
                  aliasKey /= binderKey,
                  binderKey /= k
              ]
          ]
      boundRootForDeps bnd0 =
        boundRootWith
          getNodeId
          canonical
          (`IntMap.lookup` nodes)
          (VarStore.lookupVarBound constraint)
          (`IntMap.lookup` schemeRootByBody)
          False
          bnd0
      boundDepsForCandidate k =
        let liveBinder = canonicalBinder (NodeId k)
            mbLiveBound = VarStore.lookupVarBound constraint liveBinder
            liveBoundIsLocalSchemeBody =
              case (scopeGen, mbLiveBound) of
                (Just gid, Just bnd) ->
                  case
                      IntMap.lookup
                        (getNodeId (canonical bnd))
                        schemeRootByBody
                    of
                    Just root ->
                      IntMap.lookup
                        (getNodeId (canonical root))
                        schemeRootOwner
                        == Just gid
                    Nothing -> False
                _ -> False
            liveBoundRefinesUnboundedBase =
              case (mbLiveBound, mbBindParentsGa, IntMap.lookup k solvedToBasePref) of
                (Just _, Just ga, Just baseBinder) ->
                  let baseConstraint = gbiBaseConstraint ga
                   in case lookupNodeIn (cNodes baseConstraint) baseBinder of
                        Just TyVar {} ->
                          isNothing
                            (VarStore.lookupVarBound baseConstraint baseBinder)
                        _ -> False
                _ -> False
            baseBoundEligible =
              not
                ( (isNothing mbLiveBound && not (hasExplicitBound liveBinder))
                    || liveBoundIsLocalSchemeBody
                    || liveBoundRefinesUnboundedBase
                )
            isBaseRep =
              case IntMap.lookup k solvedToBasePref of
                Just baseK ->
                  case IntMap.lookup (getNodeId baseK) baseGammaRep of
                    Just repKey -> repKey == k
                    Nothing -> False
                Nothing -> False
         in case
              ( IntMap.lookup k requiredGamma
              , IntMap.lookup k sourceDependencyRefs
              , mbBindParentsGa
              )
            of
              (Just requirement, _, Just _) -> do
                let boundTy = rgbOperatedType requirement
                let freeRefs = freeTypeVarRefsType boundTy
                    deps = depsFromRefs k dependencyCandidateSet freeRefs
                traceGeneralizeM
                  ( "generalizeAt: required Gamma boundDeps k="
                      ++ show k
                      ++ " sourceRoot="
                      ++ show (rgbOperatedRoot requirement)
                      ++ " boundTy="
                      ++ show boundTy
                      ++ " deps="
                      ++ show deps
                  )
                pure deps
              (Just requirement, _, Nothing) ->
                Left
                  (ValidationFailed
                    [ "root RaiseMerge binder ordering requires the frozen base graph"
                    , "  requirement: " ++ show requirement
                    ])
              (Nothing, Just _, _) -> do
                let sourceCarrier = canonical (NodeId k)
                    sourceBoundRoot =
                      case VarStore.lookupVarBound constraint sourceCarrier of
                        Just bound -> boundRootForDeps bound
                        Nothing -> sourceCarrier
                    subst = substDepsFor k
                boundTy <-
                  reifyBoundWithRefs
                    resForReify
                    subst
                    sourceBoundRoot
                let deps =
                      depsFromRefs
                        k
                        dependencyCandidateSet
                        (freeTypeVarRefsType boundTy)
                pure deps
              (Nothing, Nothing, Just ga)
                | Just baseK <- IntMap.lookup k solvedToBasePref,
                  isBaseRep,
                  baseBoundEligible ->
                    let baseConstraint = gbiBaseConstraint ga
                        baseNodes = cNodes baseConstraint
                        boundRootForDepsBase bnd0 =
                          boundRootWith
                            getNodeId
                            id
                            (\key -> lookupNodeIn baseNodes (NodeId key))
                            (VarStore.lookupVarBound baseConstraint)
                            (`IntMap.lookup` schemeRootByBodyBase)
                            False
                            bnd0
                     in case VarStore.lookupVarBound baseConstraint baseK of
                          Nothing -> do
                            let boundTy = TVarRef (refForDep k)
                                freeRefs = freeTypeVarRefsType boundTy
                                deps = depsFromRefs k dependencyCandidateSet freeRefs
                            pure deps
                          Just bnd -> do
                            let bndRoot = boundRootForDepsBase bnd
                            boundTy <-
                              reifyBoundWithRefsOnConstraint
                                baseConstraint
                                substDepsBase
                                bndRoot
                            let bndRootKey = getNodeId bndRoot
                                freeRefs0 = freeTypeVarRefsType boundTy
                                freeRefs =
                                  case (boundTy, lookupNodeIn baseNodes bndRoot, VarStore.lookupVarBound baseConstraint bndRoot) of
                                    (TBottom, Just TyVar {}, Nothing) ->
                                      [refForDep bndRootKey]
                                    _ -> freeRefs0
                                deps = depsFromRefs k dependencyCandidateSet freeRefs
                            traceGeneralizeM
                              ( "generalizeAt: boundDeps k="
                                  ++ show k
                                  ++ " bndRoot="
                                  ++ show bndRoot
                                  ++ " boundTy="
                                  ++ show boundTy
                                  ++ " freeRefs="
                                  ++ show freeRefs
                                  ++ " deps="
                                  ++ show deps
                              )
                            pure deps
              (Nothing, Nothing, _) -> do
                let subst = substDepsFor k
                case VarStore.lookupVarBound constraint (canonical (NodeId k)) of
                  Nothing -> do
                    let boundTy = TVarRef (refForDep k)
                        freeRefs = freeTypeVarRefsType boundTy
                        deps = depsFromRefs k dependencyCandidateSet freeRefs
                    pure deps
                  Just bnd -> do
                    let bndRoot = boundRootForDeps bnd
                    boundTy <-
                      reifyBoundWithRefs
                        resForReify
                        subst
                        bndRoot
                    let bndRootC = canonical bndRoot
                        bndRootKey = getNodeId bndRootC
                        freeRefs0 = freeTypeVarRefsType boundTy
                        freeRefs =
                          case (boundTy, lookupNodeInMap nodes bndRootC, VarStore.lookupVarBound constraint bndRootC) of
                            (TBottom, Just TyVar {}, Nothing) ->
                              [refForDep bndRootKey]
                            _ -> freeRefs0
                        deps = depsFromRefs k dependencyCandidateSet freeRefs
                    traceGeneralizeM
                      ( "generalizeAt: boundDeps k="
                          ++ show k
                          ++ " bndRoot="
                          ++ show bndRoot
                          ++ " boundTy="
                          ++ show boundTy
                          ++ " freeRefs="
                          ++ show freeRefs
                          ++ " deps="
                          ++ show deps
                      )
                    pure deps
      orderBinders candidates =
        orderCandidates
          [ key
          | key <- candidates
          , not (IntSet.member (canonKey (NodeId key)) locallyClosedGammaKeys)
          ]
          boundDepsForCandidate

  case requiredGammaResultRouteConflicts of
    [] -> pure ()
    conflicts ->
      Left
        ( ValidationFailed
            [ "one required Gamma result has conflicting exterior identities"
            , "  conflicts: " ++ show conflicts
            ]
        )

  let binderCandidateMap =
        IntMap.fromList
          ( [ (getNodeId (canonicalBinder v), canonicalBinder v)
              | v <- bindersCandidatesCanonical
              , not (candidateIsAmbient v)
              , not (IntSet.member (canonKey v) locallyClosedGammaKeys)
            ]
              ++ [ (liveKey, NodeId liveKey)
                   | liveKey <- IntMap.keys requiredGamma
                   , not
                      ( IntSet.member
                          (canonKey (NodeId liveKey))
                          locallyClosedGammaKeys
                      )
                 ]
              ++ [ (getNodeId sourceNode, sourceNode)
                   | sourceNode <- sourceDependencyBinders
                   , not
                      ( IntSet.member
                          (canonKey sourceNode)
                          locallyClosedGammaKeys
                      )
                 ]
              ++ [ (getNodeId dependencyNode, dependencyNode)
                   | dependencyNode <-
                      requiredGammaBoundDependencyBinders
                 ]
          )
      closeBinderSet current = do
        deps <- fmap concat $ mapM boundDepsForCandidate (IntSet.toList current)
        let next = IntSet.union current (IntSet.fromList deps)
        if next == current then pure current else closeBinderSet next

  closedBinderSet <- closeBinderSet (IntSet.fromList binderIds)
  let provisionalIds = IntSet.toList closedBinderSet
      provisionalSubst = IntMap.fromList [(key, refForDep key) | key <- provisionalIds]
  bodyClosureIds <- do
    let liveTypeRoot = canonical typeRoot
    case lookupNodeIn (cNodes (pvCanonicalConstraint resForReify)) liveTypeRoot of
      Nothing -> pure []
      Just _ -> do
        targetNamedSet <- namedNodes resForReify
        targetTy <-
          reifyTypeWithNamedSetRefs
            resForReify
            ( IntMap.unions
                [ requiredGammaResultSubst
                , provisionalSubst
                , sourceDependencyRefs
                ]
            )
            ( IntSet.unions
                [ targetNamedSet
                , sourceDependencyKeys
                , IntSet.fromList
                    (IntMap.keys requiredGammaResultSubst)
                ]
            )
            liveTypeRoot
        let selectedBodyClosureIds =
              [ dep
              | ref <- freeTypeVarRefsType targetTy,
                Just dep <-
                  [ case [bid | (bid, provisionalRef) <- IntMap.toList provisionalSubst, typeBinderRefsSameIdentity provisionalRef ref] of
                      (bid : _) -> Just bid
                      [] -> depFromRef ref
                  ],
                -- Select the declaration from the complete
                -- construction-authorized dependency domain.  A source
                -- annotation existential can survive free in the chosen body
                -- even when its graph occurrence belongs to a nested scheme
                -- and is therefore absent from the ordinary binder candidates.
                -- Its source-identity sidecar is still exact declaration
                -- authority, so the packet must quantify it here rather than
                -- relying on finalization to repair a free variable.
                IntSet.member dep dependencyCandidateSet
              ]
        pure selectedBodyClosureIds
  closedBinderSet' <- closeBinderSet (IntSet.union closedBinderSet (IntSet.fromList bodyClosureIds))
  let selectedInheritedRigidAliasRoutes =
        IntMap.restrictKeys
          inheritedRigidAliasRoutes
          closedBinderSet'
      bindersCanonClosed =
        [ binder
          | bid <- IntSet.toList closedBinderSet',
            IntMap.notMember bid selectedInheritedRigidAliasRoutes,
            Just binder <- [IntMap.lookup bid binderCandidateMap]
        ]
      binderIdsClosed = map getNodeId bindersCanonClosed
      requiredDeclarationKeysByIdentity =
        Map.fromList
          [ (typeBinderRefIdentity (refForDep key), key)
          | key <- IntMap.keys requiredGamma
          ]
      requiredDeclarationOwnerFor key =
        Map.lookup
          (typeBinderRefIdentity (refForDep key))
          requiredDeclarationKeysByIdentity
      binderIsShadowedByRequiredDeclaration key =
        case requiredDeclarationOwnerFor key of
          Just requiredKey -> key /= requiredKey
          Nothing -> False
      binderIdsForOrdering =
        filter
          (not . binderIsShadowedByRequiredDeclaration)
          binderIdsClosed
      requiredDeclarationAliasKeys =
        filter
          binderIsShadowedByRequiredDeclaration
          binderIdsClosed

  ordered0 <- orderBinders binderIdsForOrdering
  traceGeneralizeM
    ( "generalizeAt: binderIds="
        ++ show binderIdsClosed
        ++ " ordered0="
        ++ show ordered0
    )
  let (orderedBinders, localBinderRefRoutes) =
        buildBinderIdentityQuotient refForDep ordered0
      identityQuotientErrors =
        validateBinderIdentityQuotient
          refForDep
          ordered0
          orderedBinders
          localBinderRefRoutes
      frozenExpansionAliasRoutes =
        IntMap.fromList
          [ (candidateKey, representativeRef)
          | candidateKey <- IntMap.keys binderCandidateMap
          , IntSet.notMember candidateKey closedBinderSet'
          , let representativeKey =
                  dependencyKeyForCandidate candidateKey
          , representativeKey /= candidateKey
          , Just representativeRef <-
              [IntMap.lookup representativeKey localBinderRefRoutes]
          ]
      ambientBinderRefRoutes =
        IntMap.fromList
          [ (canonKey candidate, candidateAuthorityRef candidate)
          | candidate <- bindersCandidatesCanonical
          , candidateIsAmbient candidate
          ]
      requiredDeclarationAliasRoutes =
        IntMap.fromList
          [ (aliasKey, declarationRef)
          | aliasKey <- requiredDeclarationAliasKeys
          , Just declarationKey <-
              [requiredDeclarationOwnerFor aliasKey]
          , Just declarationRef <-
              [IntMap.lookup declarationKey localBinderRefRoutes]
          ]
      binderRefRoutes =
        IntMap.unions
          [ selectedInheritedRigidAliasRoutes
          , requiredDeclarationAliasRoutes
          , localBinderRefRoutes
          , frozenExpansionAliasRoutes
          , ambientBinderRefRoutes
          ]
  case identityQuotientErrors of
    [] -> pure ()
    errors ->
      Left
        ( ValidationFailed
            ("invalid semantic binder identity quotient" : errors)
        )
  let distinctTermUsedRefs =
        termUsedRequestedAuthorities
      selectTermUsedBinder requiredRef =
        let routedRequiredRef =
              case typeBinderRefNode requiredRef of
                Nothing -> requiredRef
                Just requiredNode ->
                  let routedNode =
                        canonicalizeBinder requiredNode
                   in case
                        IntMap.lookup
                          (getNodeId routedNode)
                          binderRefRoutes
                      of
                        Just routedRef -> routedRef
                        Nothing -> requiredRef
         in
        case
          [ plannedRef
          | (_, plannedRef) <- orderedBinders
          , typeBinderRefsSameIdentity routedRequiredRef plannedRef
          ]
        of
          [plannedRef] -> pure plannedRef
          [] ->
            Left
              ( ValidationFailed
                  [ "term-used root binder was not selected by the binder plan"
                  , "  required ref: " ++ show requiredRef
                  , "  routed ref: " ++ show routedRequiredRef
                  , "  selected binders: " ++ show orderedBinders
                  , "  seed binder ids: " ++ show binderIds
                  , "  closed binder ids: " ++ show binderIdsClosed
                  , "  ordering input: " ++ show binderIdsForOrdering
                  , "  ordered graph binders: " ++ show ordered0
                  , "  candidate keys: "
                      ++ show (IntMap.keys binderCandidateMap)
                  , "  term-used groups: "
                      ++ show termUsedRootBinderGroups
                  , "  term-used live nodes: "
                      ++ show termUsedRootBinderNodes
                  , "  locally closed Gamma keys: "
                      ++ show (IntSet.toList locallyClosedGammaKeys)
                  ]
              )
          matches ->
            Left
              ( ValidationFailed
                  [ "term-used root binder matched multiple planned declarations"
                  , "  required ref: " ++ show requiredRef
                  , "  routed ref: " ++ show routedRequiredRef
                  , "  matches: " ++ show matches
                  ]
              )
  termUsedRootBinderRefs <-
    traverse selectTermUsedBinder distinctTermUsedRefs
  pure
    BinderPlan
      { bpOrderedBinders = orderedBinders,
        bpBinderRefRoutes = binderRefRoutes,
        bpRootBodyClosureKeys = IntSet.fromList bodyClosureIds,
        bpInheritedRigidAliasRoutes =
          selectedInheritedRigidAliasRoutes,
        bpLocallyClosedGammaKeys = locallyClosedGammaKeys,
        bpNestedSchemeInteriorSet = nestedSchemeInteriorSet,
        bpGammaAlias = gammaAlias,
        bpBaseGammaSet = baseGammaSet,
        bpBaseGammaRep = baseGammaRep,
        bpNamedUnderGaSet = namedUnderGaSet,
        bpSolvedToBasePref = solvedToBasePref,
        bpReachableForBinders = reachableForBinders,
        bpAliasBinderBases = aliasBinderBases,
        bpOrderBinders = orderBinders,
        bpRequiredGamma = requiredGamma,
        bpSourceBinderRefs = sourceBinderRefs,
        bpAmbientBinderRefs = bpiAmbientBinderRefs,
        bpTermUsedRootBinderRefs = termUsedRootBinderRefs
      }
  where
    hasExplicitBound v =
      case IntMap.lookup (getNodeId (bpiCanonical v)) bpiNodes of
        Just TyVar {} -> VarStore.lookupVarBound bpiConstraint (bpiCanonical v) /= Nothing
        _ -> False
