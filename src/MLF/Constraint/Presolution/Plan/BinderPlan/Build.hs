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

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified MLF.Binding.Tree as Binding
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
      namedUnderGa = bpiNamedUnderGa
      gammaAlias = bpiGammaAlias
      baseGammaSet = bpiBaseGammaSet
      baseGammaRep = bpiBaseGammaRep
      baseGammaRepSet = bpiBaseGammaRepSet
      namedUnderGaSet = bpiNamedUnderGaSet
      solvedToBasePref = bpiSolvedToBasePref
      reachable = bpiReachable
      reachableForBinders = bpiReachableForBinders
      reachableType = bpiReachableType
      reachableTypeStructural = bpiReachableTypeStructural
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
        ++ " reachable="
        ++ show (IntSet.toList reachableForBinders)
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
      locallyClosedGammaKeys =
        IntSet.fromList
          [ canonKey (NodeId key)
          | key <- IntSet.toList bpiLocallyClosedGammaNodes
          ]
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
  let binderCandidateKeys =
        IntSet.fromList [canonKey v | v <- bindersCandidatesCanonical]
  let binders =
        [ canonicalBinder v
          | v <- bindersCandidatesCanonical,
            let vKey = canonKey v,
            not (IntSet.member vKey locallyClosedGammaKeys),
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
                (inGamma || IntSet.member vKey aliasBinderBases)
                  && (IntSet.member vKey reachableForBinders || isTargetSchemeBinder v)
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
            not (IntSet.member (canonKey v) nestedStructuralBinders),
            not (isNestedSourceStructuralBinder v)
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
      binders' =
        traceGeneralize
          ( "generalizeAt: bindersFiltered="
              ++ show binders
              ++ " requiredGammaBinders="
              ++ show requiredGammaBinders
              ++ " typeRoot="
              ++ show typeRoot
          )
          [ binder
          | binder <- binders ++ requiredGammaBinders
          , not (IntSet.member (canonKey binder) locallyClosedGammaKeys)
          ]
      bindersCanon =
        IntMap.elems $
          IntMap.fromList
            [ (getNodeId v, v)
              | v <- binders'
            ]
  let binderIds = map getNodeId bindersCanon
      -- The selected binders seed dependency closure, but a selected
      -- binder's bound may mention a candidate filtered out of that seed.
      -- Keep the complete construction-time candidate domain available so
      -- closure can recover those dependencies (for example paper K's
      -- @c >= forall e. e -> a@ must pull @a@ back in through @c@'s bound).
      -- This is deliberately not a free-variable repair on the final type:
      -- every admitted dependency still comes from a binder candidate or a
      -- required Gamma construction.
      dependencyCandidateSet =
        IntSet.union
          binderCandidateKeys
          ( IntSet.fromList
              [ liveKey
              | liveKey <- IntMap.keys requiredGamma
              , not
                  ( IntSet.member
                      (canonKey (NodeId liveKey))
                      locallyClosedGammaKeys
                  )
              ]
          )
      dependencyCandidateIds = IntSet.toList dependencyCandidateSet
      nameForDep k = "t" ++ show k
      refForDep k =
        case IntMap.lookup k requiredGamma of
          Just requirement ->
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode (rgbExteriorNode requirement))
              (nameForDep k)
          Nothing ->
            maybe
              (typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId k)) (nameForDep k))
              (\ref -> typeBinderRefFromIdentity (typeBinderRefIdentity ref) (nameForDep k))
              (sourceRefForLiveKey k)
      depFromRef ref =
        case
          [ key
            | (key, candidateRef) <- IntMap.toList substDeps,
              typeBinderRefsSameIdentity candidateRef ref
          ]
        of
          key : _ -> Just key
          [] -> getNodeId <$> typeBinderRefNode ref
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
        IntMap.union substDeps $
          IntMap.fromList
            [ (aliasKey, typeBinderRefFromIdentity (typeBinderIdentityFromNode (canonical (NodeId aliasKey))) (nameForDep binderKey))
              | (aliasKey, binderKey) <- IntMap.toList gammaAlias,
                aliasKey /= binderKey,
                binderKey /= k
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
        let isBaseRep =
              case IntMap.lookup k solvedToBasePref of
                Just baseK ->
                  case IntMap.lookup (getNodeId baseK) baseGammaRep of
                    Just repKey -> repKey == k
                    Nothing -> False
                Nothing -> False
         in case (IntMap.lookup k requiredGamma, mbBindParentsGa) of
              (Just requirement, Just _) -> do
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
              (Just requirement, Nothing) ->
                Left
                  (ValidationFailed
                    [ "root RaiseMerge binder ordering requires the frozen base graph"
                    , "  requirement: " ++ show requirement
                    ])
              (Nothing, Just ga)
                | Just baseK <- IntMap.lookup k solvedToBasePref,
                  isBaseRep ->
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
              _ -> do
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

  let binderCandidateMap =
        IntMap.fromList
          ( [ (getNodeId (canonicalBinder v), canonicalBinder v)
              | v <- bindersCandidatesCanonical
              , not (IntSet.member (canonKey v) locallyClosedGammaKeys)
            ]
              ++ [ (liveKey, NodeId liveKey)
                   | liveKey <- IntMap.keys requiredGamma
                   , not (IntSet.member (canonKey (NodeId liveKey)) locallyClosedGammaKeys)
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
            provisionalSubst
            targetNamedSet
            liveTypeRoot
        pure
          [ dep
            | ref <- freeTypeVarRefsType targetTy,
              Just dep <-
                [ case [bid | (bid, provisionalRef) <- IntMap.toList provisionalSubst, typeBinderRefsSameIdentity provisionalRef ref] of
                    (bid : _) -> Just bid
                    [] -> depFromRef ref
                ],
              IntSet.member dep binderCandidateKeys
          ]
  closedBinderSet' <- closeBinderSet (IntSet.union closedBinderSet (IntSet.fromList bodyClosureIds))
  let bindersCanonClosed =
        [ binder
          | bid <- IntSet.toList closedBinderSet',
            Just binder <- [IntMap.lookup bid binderCandidateMap]
        ]
      binderIdsClosed = map getNodeId bindersCanonClosed

  ordered0 <- orderBinders binderIdsClosed
  traceGeneralizeM
    ( "generalizeAt: binderIds="
        ++ show binderIdsClosed
        ++ " ordered0="
        ++ show ordered0
    )
  let (orderedBinders, binderRefRoutes) =
        buildBinderIdentityQuotient refForDep ordered0
      identityQuotientErrors =
        validateBinderIdentityQuotient
          refForDep
          ordered0
          orderedBinders
          binderRefRoutes
  case identityQuotientErrors of
    [] -> pure ()
    errors ->
      Left
        ( ValidationFailed
            ("invalid semantic binder identity quotient" : errors)
        )
  pure
    BinderPlan
      { bpOrderedBinders = orderedBinders,
        bpBinderRefRoutes = binderRefRoutes,
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
        bpAmbientBinderRefs = bpiAmbientBinderRefs
      }
  where
    hasExplicitBound v =
      case IntMap.lookup (getNodeId (bpiCanonical v)) bpiNodes of
        Just TyVar {} -> VarStore.lookupVarBound bpiConstraint (bpiCanonical v) /= Nothing
        _ -> False
