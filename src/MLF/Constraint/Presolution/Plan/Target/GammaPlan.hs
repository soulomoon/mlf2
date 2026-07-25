{-# LANGUAGE RecordWildCards #-}

module MLF.Constraint.Presolution.Plan.Target.GammaPlan (
    GammaPlanInput(..),
    GammaPlan(..),
    buildGammaPlan,
    expandSourceBinderRefs,
    expandSourceBinderRefsWithPreference
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Control.Monad (foldM)
import Data.List (find, sort, sortBy)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust, listToMaybe)
import MLF.Util.Trace (traceWhen)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types.Graph
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Util.IntMapUtils as IntMapUtils
import MLF.Constraint.Presolution.Plan.BinderPlan (GaBindParentsInfo(..), firstSchemeRootAncestorWith)
import MLF.Constraint.Presolution.Plan.Requirements
    ( GeneralizationRequirements(..)
    , RequiredGammaBinder(..)
    , RequiredGammaPlacement(..)
    , expansionConstructionRoleKeys
    , requiredGammaPlacementIsLocal
    )
import MLF.Reify.TypeOps (alphaEqType)
import MLF.Types.Elab
    ( Ty (..)
    , TypeBinderRef
    , typeBinderRefIdentity
    , typeBinderRefNode
    , typeBinderRefsSameIdentity
    )
import MLF.Types.Identity (typeBinderIdentityGeneratedUnique)
import MLF.Util.ElabError (ElabError(..))
import MLF.Constraint.BindingUtil (firstGenAncestorFrom)
import qualified MLF.Util.Order as Order

data GammaPlanInput p = GammaPlanInput
    { gpiDebugEnabled :: Bool
    , gpiConstraint :: Constraint p
    , gpiNodes :: IntMap.IntMap TyNode
    , gpiCanonical :: NodeId -> NodeId
    , gpiCanonKey :: NodeId -> Int
    , gpiIsTyVarKey :: Int -> Bool
    , gpiBindParents :: BindParents
    , gpiBindParentsGa :: Maybe (GaBindParentsInfo p)
    , gpiScopeGen :: Maybe GenNodeId
    , gpiTarget0 :: NodeId
    , gpiTargetBound :: Maybe NodeId
    , gpiSchemeRootOwnerBase :: IntMap.IntMap GenNodeId
    , gpiSchemeRootOwner :: IntMap.IntMap GenNodeId
    , gpiSchemeRootByBody :: IntMap.IntMap NodeId
    , gpiSchemeRootKeySet :: IntSet.IntSet
    , gpiOrderRoot :: NodeId
    , gpiOrderRootBase :: NodeId
    , gpiTypeRoot0 :: NodeId
    , gpiNamedUnderGaInterior :: IntSet.IntSet
    , gpiNestedSchemeInteriorSet :: IntSet.IntSet
    , gpiReachableForBinders0 :: IntSet.IntSet
    , gpiReachableFromWithBounds :: NodeId -> IntSet.IntSet
    , gpiBindableChildrenUnder :: NodeRef -> [NodeId]
    , gpiAliasBinderNodes :: [NodeId]
    , gpiFirstGenAncestor :: NodeRef -> Maybe GenNodeId
    , gpiRequirements :: GeneralizationRequirements
    }

data GammaPlan = GammaPlan
    { gpBaseGammaSet :: IntSet.IntSet
    , gpBaseGammaRep :: IntMap.IntMap Int
    , gpNamedUnderGaSet :: IntSet.IntSet
    , gpSolvedToBasePref :: IntMap.IntMap NodeId
    , gpGammaAlias :: IntMap.IntMap Int
    , gpBaseGammaRepSet :: IntSet.IntSet
    , gpReachableForBinders :: IntSet.IntSet
    , gpGammaKeyFor :: Int -> Int -> Int
    , gpNamedUnderGa :: [NodeId]
    , gpBoundHasNamedOutsideGamma :: Bool
    , gpTypeRootHasNamedOutsideGamma :: Bool
    , gpRequiredGamma :: IntMap.IntMap RequiredGammaBinder
    , gpSourceBinderRefs :: IntMap.IntMap TypeBinderRef
    }

tracePlanEnabled :: Bool -> String -> a -> a
tracePlanEnabled = traceWhen

-- | Route one semantic source-binder identity through every base alias and
-- the live node in its solved equivalence class. A source identity can be
-- attached to a different base alias from the one selected for reification
-- (for example, an exact-annotation binder versus its field occurrence).
-- Distinct identities in one solved class require an explicit, scope-owned
-- base preference; without one there is no traversal-order-independent name
-- that S' could assign to that class. Graph and structural identities are
-- construction-local declarations, so they remain only at their direct keys:
-- solved type equality cannot make them lexical aliases.
expandSourceBinderRefs
    :: (NodeId -> NodeId)
    -> IntMap.IntMap NodeId
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
expandSourceBinderRefs =
    expandSourceBinderRefsWithPreference IntMap.empty

-- | Resolve any multi-identity solved class through the base binder already
-- selected by the local Gamma plan.  If that structural binder has no source
-- identity of its own, the identity already attached to the selected live
-- representative owns the class.  Once selected, route that one identity
-- through every alias so later reification cannot recover a different lexical
-- binder from its traversal order.
expandSourceBinderRefsWithPreference
    :: IntMap.IntMap NodeId
    -> (NodeId -> NodeId)
    -> IntMap.IntMap NodeId
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
expandSourceBinderRefsWithPreference preferredBase canonical baseToSolved directRefs = do
    classRefs <- IntMap.traverseWithKey uniqueClassRef classCandidates
    let expandedGeneratedRefs =
            IntMap.unions [baseAliases classRefs, classRefs, generatedDirectRefs]
    pure (IntMap.union constructionLocalRefs expandedGeneratedRefs)
  where
    generatedDirectRefs =
        IntMap.filter
            ( isJust
                . typeBinderIdentityGeneratedUnique
                . typeBinderRefIdentity
            )
            directRefs

    constructionLocalRefs =
        IntMap.filter
            ( not
                . isJust
                . typeBinderIdentityGeneratedUnique
                . typeBinderRefIdentity
            )
            directRefs

    classCandidates =
        IntMap.fromListWith (++)
            [ (classKeyForBase baseKey, [(baseKey, ref)])
            | (baseKey, ref) <- IntMap.toList generatedDirectRefs
            ]

    classKeyForBase baseKey =
        getNodeId
            ( canonical
                (IntMap.findWithDefault (NodeId baseKey) baseKey baseToSolved)
            )

    baseAliases classRefs =
        IntMap.fromList
            [ (baseKey, ref)
            | (baseKey, solvedNode) <- IntMap.toList baseToSolved
            , Just ref <- [IntMap.lookup (getNodeId (canonical solvedNode)) classRefs]
            ]

    uniqueClassRef classKey candidates@((_, firstRef) : rest)
        | all (typeBinderRefsSameIdentity firstRef . snd) rest = Right firstRef
        | Just (NodeId preferredKey) <- IntMap.lookup classKey preferredBase
        , Just preferredRef <- lookup preferredKey candidates = Right preferredRef
        | IntMap.member classKey preferredBase
        , Just representativeRef <- lookup classKey candidates = Right representativeRef
        | otherwise =
            Left
                (ValidationFailed
                    [ "one solved equivalence class carries conflicting source-binder identities"
                    , "  class: " ++ show (NodeId classKey)
                    , "  preferred base: " ++ show (IntMap.lookup classKey preferredBase)
                    , "  references: " ++ show candidates
                    ])
    uniqueClassRef _ [] =
        Left
            (ValidationFailed
                ["source-binder identity class was unexpectedly empty"])

buildGammaPlan :: GammaPlanInput p -> Either ElabError GammaPlan
buildGammaPlan GammaPlanInput{..} = do
    requiredGamma <- validateRequiredGamma
    requiredGlobalAliases <- validateRequiredGlobalAliases requiredGamma
    certifiedExpansionGammaSet <- validateExpansionConstructionRoles
    let tracePlan = tracePlanEnabled gpiDebugEnabled
        constraint = gpiConstraint
        nodes = gpiNodes
        canonical = gpiCanonical
        canonKey = gpiCanonKey
        isTyVarKey = gpiIsTyVarKey
        bindParents = gpiBindParents
        mbBindParentsGa = gpiBindParentsGa
        scopeGen = gpiScopeGen
        target0 = gpiTarget0
        targetBound = gpiTargetBound
        schemeRootOwnerBase = gpiSchemeRootOwnerBase
        schemeRootOwner = gpiSchemeRootOwner
        schemeRootByBody = gpiSchemeRootByBody
        schemeRootKeySet = gpiSchemeRootKeySet
        orderRoot = gpiOrderRoot
        orderRootBase = gpiOrderRootBase
        typeRoot0 = gpiTypeRoot0
        namedUnderGaInterior = gpiNamedUnderGaInterior
        nestedSchemeInteriorSet = gpiNestedSchemeInteriorSet
        reachableForBinders0 = gpiReachableForBinders0
        reachableFromWithBounds = gpiReachableFromWithBounds
        bindableChildrenUnder' = gpiBindableChildrenUnder
        aliasBinderNodes = gpiAliasBinderNodes
        firstGenAncestorGa = gpiFirstGenAncestor
        namedUnderGaRaw =
            case scopeGen of
                Just gid ->
                    case mbBindParentsGa of
                        Nothing ->
                            IntMap.elems $
                                IntMap.fromList
                                    [ (getNodeId nid, nid)
                                    | nid <- bindableChildrenUnder' (GenRef gid) ++ aliasBinderNodes
                                    ]
                        Just ga ->
                            let solvedKids = bindableChildrenUnder' (GenRef gid)
                                baseKids =
                                    [ canonical solvedNid
                                    | (childKey, (parent, flag)) <- IntMap.toList (gbiBindParentsBase ga)
                                    , parent == GenRef gid
                                    , flag == BindFlex
                                    , case lookupNodeIn (cNodes (gbiBaseConstraint ga)) (NodeId childKey) of
                                        Just TyVar{} -> True
                                        _ -> False
                                    , Just solvedNid <- [IntMap.lookup childKey (gbiBaseToSolved ga)]
                                    , case IntMap.lookup (getNodeId (canonical solvedNid)) nodes of
                                        Just TyVar{} -> True
                                        _ -> False
                                    ]
                            in IntMap.elems $
                                IntMap.fromList
                                    [ (getNodeId nid, nid)
                                    | nid <- baseKids ++ solvedKids ++ aliasBinderNodes
                                    ]
                Nothing -> []
        (baseGammaSetLocalOut, baseGammaRepLocalOut, namedUnderGaSetLocalOut, solvedToBasePrefLocalOut0) =
            case (scopeGen, mbBindParentsGa) of
                (Just gid, Just ga) ->
                    let baseConstraint = gbiBaseConstraint ga
                        baseNodes = cNodes baseConstraint
                        baseGammaSetLocalRaw =
                            IntSet.fromList
                                [ childKey
                                | (childKey, (parent, flag)) <- IntMap.toList (gbiBindParentsBase ga)
                                , parent == GenRef gid
                                , flag == BindFlex
                                , case lookupNodeIn baseNodes (NodeId childKey) of
                                    Just TyVar{} -> True
                                    _ -> False
                                ]
                        baseSchemeRootSetLocal =
                            IntSet.fromList
                                [ rootKey
                                | rootKey <- IntMap.keys schemeRootOwnerBase
                                ]
                        firstSchemeRootAncestorBase baseKey =
                            let parentOf ref =
                                    fmap fst (IntMap.lookup (nodeRefKey ref) (gbiBindParentsBase ga))
                                keyOfRef ref = nodeRefKey ref
                                isSchemeRootKey = (`IntSet.member` baseSchemeRootSetLocal)
                            in firstSchemeRootAncestorWith parentOf keyOfRef isSchemeRootKey baseKey
                        keepBaseGamma baseKey =
                            case firstSchemeRootAncestorBase baseKey of
                                Nothing -> True
                                Just rootKey ->
                                    case IntMap.lookup rootKey schemeRootOwnerBase of
                                        Just ownerGid -> ownerGid == gid
                                        Nothing -> True
                        baseGammaSetLocal0 =
                            IntSet.filter keepBaseGamma baseGammaSetLocalRaw
                        baseGammaDirect =
                            IntSet.fromList
                                [ getNodeId baseN
                                | solvedN <- namedUnderGaRaw
                                , Just baseN <- [IntMap.lookup (getNodeId (canonical solvedN)) (gbiSolvedToBase ga)]
                                ]
                        baseGammaSetLocal =
                            IntSet.union baseGammaSetLocal0 baseGammaDirect
                        firstSchemeRootAncestorSolved solvedKey =
                            let parentOf ref =
                                    fmap fst (IntMap.lookup (nodeRefKey ref) bindParents)
                                keyOfRef ref =
                                    case ref of
                                        GenRef gidRef -> genNodeKey gidRef
                                        TypeRef nid -> canonKey nid
                                isSchemeRootKey = (`IntSet.member` schemeRootKeySet)
                            in firstSchemeRootAncestorWith parentOf keyOfRef isSchemeRootKey solvedKey
                        keepSolvedGamma solvedKey =
                            case firstSchemeRootAncestorSolved solvedKey of
                                Nothing -> True
                                Just rootKey ->
                                    case IntMap.lookup rootKey schemeRootOwner of
                                        Just ownerGid -> ownerGid == gid
                                        Nothing -> False
                        solvedGammaSetLocal =
                            case Binding.boundFlexChildren constraint (GenRef gid) of
                                Right kids ->
                                    IntSet.fromList
                                        [ getNodeId (canonical kid)
                                        | kid <- kids
                                        , let key = getNodeId (canonical kid)
                                        , case IntMap.lookup key nodes of
                                            Just TyVar{} -> keepSolvedGamma key
                                            _ -> False
                                        ]
                                Left _ -> IntSet.empty
                        baseToSolved = gbiBaseToSolved ga
                        solvedToBase = gbiSolvedToBase ga
                        keysSolved = Order.orderKeysFromConstraintWith canonical constraint orderRoot Nothing
                        keysBase = Order.orderKeysFromConstraintWith id baseConstraint orderRootBase Nothing
                        sortByKeys keys =
                            let keyMaybe k = IntMap.lookup k keys
                                cmp a b =
                                    case (keyMaybe a, keyMaybe b) of
                                        (Just ka, Just kb) ->
                                            case Order.compareOrderKey ka kb of
                                                EQ -> compare a b
                                                other -> other
                                        (Just _, Nothing) -> LT
                                        (Nothing, Just _) -> GT
                                        _ -> compare a b
                            in sortBy cmp
                        solvedGammaOrdered = sortByKeys keysSolved (IntSet.toList solvedGammaSetLocal)
                        baseGammaOrdered = sortByKeys keysBase (IntSet.toList baseGammaSetLocal)
                        qAlignSolvedToBaseLocal =
                            IntMap.fromList
                                [ (solvedKey, NodeId baseKey)
                                | (solvedKey, baseKey) <- zip solvedGammaOrdered baseGammaOrdered
                                ]
                        alignBaseToSolved =
                            case ( Binding.boundFlexChildren (gbiBaseConstraint ga) (GenRef gid)
                                 , Binding.boundFlexChildrenUnder canonical constraint (GenRef gid)
                                 ) of
                                (Right baseBinders, Right solvedBinders) ->
                                    IntMap.fromList
                                        [ (getNodeId baseB, getNodeId (canonical solvedB))
                                        | (baseB, solvedB) <- zip baseBinders solvedBinders
                                        ]
                                _ -> IntMap.empty
                        solvedToBaseAll =
                            IntMap.fromListWith
                                (++)
                                [ (getNodeId (canonical solvedNid), [NodeId baseKey])
                                | (baseKey, solvedNid) <- IntMap.toList baseToSolved
                                ]
                        solvedUnderScope solvedKey =
                            firstGenAncestorGa (typeRef (NodeId solvedKey)) == Just gid
                        preferGamma =
                            IntMap.mapMaybeWithKey
                                (\_solvedKey baseList ->
                                    listToMaybe
                                        [ baseN
                                        | baseN@(NodeId baseKey) <- baseList
                                        , IntSet.member baseKey baseGammaSetLocal
                                        ]
                                )
                                solvedToBaseAll
                        identityGamma =
                            IntMap.fromList
                                [ (baseKey, NodeId baseKey)
                                | baseKey <- IntSet.toList baseGammaSetLocal
                                , IntMap.member baseKey nodes
                                ]
                        identityGammaScoped =
                            IntMap.fromList
                                [ (baseKey, NodeId baseKey)
                                | baseKey <- IntSet.toList baseGammaSetLocal
                                , IntMap.member baseKey nodes
                                , solvedUnderScope baseKey
                                ]
                        solvedToBasePrefLocal =
                            IntMap.union identityGammaScoped $
                                IntMap.union preferGamma $
                                    IntMap.union solvedToBase $
                                        IntMap.union qAlignSolvedToBaseLocal identityGamma
                        solvedByBasePref =
                            IntMap.fromListWith
                                (++)
                                [ (getNodeId baseN, [solvedKey])
                                | (solvedKey, baseN) <- IntMap.toList solvedToBasePrefLocal
                                ]
                        solvedFallback =
                            IntMap.fromListWith
                                min
                                [ (getNodeId baseNid, getNodeId (canonical (NodeId solvedKey)))
                                | (solvedKey, baseNid) <- IntMap.toList solvedToBase
                                ]
                        pickFromSolvedKeys solvedKeys =
                            let underScopeKeys = filter solvedUnderScope solvedKeys
                                pickFrom keys =
                                    case filter isTyVarKey keys of
                                        (k:_) -> Just k
                                        [] -> listToMaybe keys
                            in case pickFrom underScopeKeys of
                                Just key -> Just key
                                Nothing -> pickFrom solvedKeys
                        pickMappedFallback baseKey =
                            case IntMap.lookup baseKey solvedByBasePref of
                                Just solvedKeys -> pickFromSolvedKeys solvedKeys
                                Nothing ->
                                    case IntMap.lookup baseKey alignBaseToSolved of
                                        Just solvedKey -> Just solvedKey
                                        Nothing -> IntMap.lookup baseKey solvedFallback
                        pickIdentity baseKey
                            | IntMap.member baseKey nodes = Just baseKey
                            | otherwise = Nothing
                        pickSolved baseKey =
                            case IntMap.lookup baseKey baseToSolved of
                                Just solvedNid ->
                                    let solvedKey = getNodeId (canonical solvedNid)
                                    in if solvedUnderScope solvedKey
                                        then Just solvedKey
                                        else
                                            case pickMappedFallback baseKey of
                                                Just fallbackKey -> Just fallbackKey
                                                Nothing -> pickIdentity baseKey
                                Nothing ->
                                    case pickMappedFallback baseKey of
                                        Just fallbackKey -> Just fallbackKey
                                        Nothing -> pickIdentity baseKey
                        baseGammaRepLocal =
                            IntMap.fromList
                                [ (baseKey, solvedKey)
                                | baseKey <- IntSet.toList baseGammaSetLocal
                                , Just solvedKey <- [pickSolved baseKey]
                                , case IntMap.lookup solvedKey nodes of
                                    Just TyVar{} -> True
                                    _ -> False
                                ]
                        solvedBindersUnderScope =
                            [ canonical child
                            | child <- IntMapUtils.typeChildrenOfGenWithFlag bindParents gid BindFlex
                            , case IntMap.lookup (getNodeId (canonical child)) nodes of
                                Just TyVar{} -> True
                                _ -> False
                            ]
                        isSchemeRootAliasSolved nid =
                            case VarStore.lookupVarBound constraint (canonical nid) of
                                Just bnd ->
                                    let bndC = canonical bnd
                                    in IntSet.member (getNodeId bndC) schemeRootKeySet
                                        || IntMap.member (getNodeId bndC) schemeRootByBody
                                Nothing -> False
                        baseSchemeRootSet =
                            IntSet.fromList
                                [ getNodeId root
                                | gen <- NodeAccess.allGenNodes baseConstraint
                                , root <- gnSchemes gen
                                ]
                        isSchemeRootAliasBase baseKey =
                            case lookupNodeIn baseNodes (NodeId baseKey) of
                                Just TyVar{ tnBound = Just bnd } ->
                                    IntSet.member (getNodeId bnd) baseSchemeRootSet
                                _ -> False
                        baseSchemeAliases =
                            let keys = IntSet.toList baseGammaSetLocal
                            in filter isSchemeRootAliasBase keys
                        solvedSchemeAliases =
                            let keys = map getNodeId solvedBindersUnderScope
                            in filter (\k -> isSchemeRootAliasSolved (NodeId k)) keys
                        scopeAliasOverrides =
                            IntMap.fromList
                                [ (solvedKey, NodeId baseKey)
                                | (solvedKey, baseKey) <- zip solvedSchemeAliases baseSchemeAliases
                                ]
                        alignSolvedToBase =
                            case ( Binding.boundFlexChildren baseConstraint (GenRef gid)
                                 , Binding.boundFlexChildrenUnder canonical constraint (GenRef gid)
                                 ) of
                                (Right baseBinders, Right solvedBinders) ->
                                    IntMap.fromList
                                        [ (getNodeId (canonical solvedB), NodeId (getNodeId baseB))
                                        | (baseB, solvedB) <- zip baseBinders solvedBinders
                                        ]
                                _ -> IntMap.empty
                        solvedBinderKeys =
                            IntSet.fromList (map getNodeId solvedBindersUnderScope)
                        alignPrefer =
                            IntMap.filterWithKey
                                (\k v -> IntSet.member k solvedBinderKeys
                                    && case IntMap.lookup k solvedToBase of
                                        Just baseN
                                            | baseN /= v
                                            , IntSet.member (getNodeId baseN) baseGammaSetLocal
                                            , getNodeId baseN /= k -> False
                                        _ -> True
                                )
                                alignSolvedToBase
                        solvedToBasePrefLocal' =
                            IntMap.union alignPrefer
                                (IntMap.union scopeAliasOverrides solvedToBasePrefLocal)
                        namedUnderGaSetLocal =
                            IntSet.unions
                                [ IntSet.fromList
                                    [ solvedKey
                                    | solvedKey <- IntMap.elems baseGammaRepLocal
                                    ]
                                , namedUnderGaInterior
                                , certifiedExpansionGammaSet
                                ]
                    in tracePlan
                        ("generalizeAt: baseGammaSet="
                            ++ show (IntSet.toList baseGammaSetLocal)
                            ++ " baseGammaPick="
                            ++ show
                                [ ( baseKey
                                  , IntMap.findWithDefault [] baseKey solvedByBasePref
                                  , pickSolved baseKey
                                  , IntMap.lookup baseKey nodes
                                  )
                                | baseKey <- IntSet.toList baseGammaSetLocal
                                ]
                            ++ " solvedToBasePref[6]="
                            ++ show (IntMap.lookup 6 solvedToBasePrefLocal')
                            ++ " scopeAliasOverrides="
                            ++ show (IntMap.toList scopeAliasOverrides)
                            ++ " baseGammaRep="
                            ++ show (IntMap.toList baseGammaRepLocal)
                            ++ " namedUnderGaSet="
                            ++ show (IntSet.toList namedUnderGaSetLocal)
                        )
                        (baseGammaSetLocal, baseGammaRepLocal, namedUnderGaSetLocal, solvedToBasePrefLocal')
                (Nothing, Just ga) ->
                    ( IntSet.empty
                    , IntMap.empty
                    , IntSet.union
                        (IntSet.fromList
                            [ getNodeId nid
                            | nid <- namedUnderGaRaw
                            , not (IntSet.member (getNodeId (canonical nid)) nestedSchemeInteriorSet)
                            ])
                        namedUnderGaInterior
                    , gbiSolvedToBase ga
                    )
                _ ->
                    ( IntSet.empty
                    , IntMap.empty
                    , IntSet.union
                        (IntSet.fromList
                            [ getNodeId nid
                            | nid <- namedUnderGaRaw
                            , not (IntSet.member (getNodeId (canonical nid)) nestedSchemeInteriorSet)
                            ])
                        namedUnderGaInterior
                    , IntMap.empty
                    )
        requiredBasePreferences =
            IntMap.fromList
                (concatMap requiredBasePreference (IntMap.toList requiredGamma))
        requiredBasePreference (requirementKey, requirement) =
            case rgbPlacement requirement of
                RequiredGammaAtCurrentScope ->
                    [ (getNodeId (canonical resultRoot), rgbExteriorNode requirement)
                    | resultRoot <- NonEmpty.toList (rgbResultRoots requirement)
                    ]
                RequiredGammaAtConstructionScope _ ->
                    [ (getNodeId (canonical resultRoot), rgbExteriorNode requirement)
                    | resultRoot <- NonEmpty.toList (rgbResultRoots requirement)
                    ]
                RequiredGammaAtNestedScope _ ->
                    [(requirementKey, rgbExteriorNode requirement)]
        solvedToBasePrefLocalOut =
            IntMap.union requiredBasePreferences solvedToBasePrefLocalOut0
        gammaAliasLocalBase =
            case mbBindParentsGa of
                Just ga ->
                    let baseToSolved = gbiBaseToSolved ga
                        targetKey = canonKey target0
                        targetIsLocalSchemeStructure =
                            case scopeGen of
                                Nothing -> False
                                Just gid ->
                                    IntMap.lookup targetKey schemeRootOwner == Just gid
                                        || case IntMap.lookup targetKey schemeRootByBody of
                                            Just root -> IntMap.lookup (canonKey root) schemeRootOwner == Just gid
                                            Nothing -> False
                        aliasRetainsBinderIdentity solvedKey =
                            go IntSet.empty (canonical (NodeId solvedKey))
                          where
                            go seen nid
                                | IntSet.member key seen = True
                                | otherwise =
                                    case IntMap.lookup key nodes of
                                        Just TyVar{} ->
                                            case VarStore.lookupVarBound constraint nid of
                                                Nothing -> True
                                                Just bound -> go (IntSet.insert key seen) (canonical bound)
                                        Just TyBottom{} -> True
                                        _ -> False
                              where
                                key = getNodeId nid
                        aliasEligible solvedKey =
                            (targetIsLocalSchemeStructure || aliasRetainsBinderIdentity solvedKey)
                                && case scopeGen of
                                    Nothing -> True
                                    Just gid ->
                                        let underSolved =
                                                firstGenAncestorGa (typeRef (NodeId solvedKey)) == Just gid
                                            underBasePref =
                                                case IntMap.lookup solvedKey solvedToBasePrefLocalOut of
                                                    Just baseN ->
                                                        firstGenAncestorFrom (gbiBindParentsBase ga) (TypeRef baseN) == Just gid
                                                    Nothing -> False
                                            underBaseGamma =
                                                case IntMap.lookup solvedKey solvedToBasePrefLocalOut of
                                                    Just baseN -> IntSet.member (getNodeId baseN) baseGammaSetLocalOut
                                                    Nothing -> False
                                        in underSolved || underBasePref || underBaseGamma
                        solvedToBaseAll =
                            IntMap.fromListWith
                                (++)
                                [ (getNodeId (canonical solvedNid), [baseKey])
                                | (baseKey, solvedNid) <- IntMap.toList baseToSolved
                                ]
                        pickBaseGamma baseKeys =
                            listToMaybe
                                [ baseKey
                                | baseKey <- sort baseKeys
                                , IntSet.member baseKey baseGammaSetLocalOut
                                ]
                        aliasFromBase =
                            IntMap.fromList
                                [ (solvedKey, repKey)
                                | (solvedKey, baseKeys) <- IntMap.toList solvedToBaseAll
                                , aliasEligible solvedKey
                                , Just baseKey <- [pickBaseGamma baseKeys]
                                , Just repKey <- [IntMap.lookup baseKey baseGammaRepLocalOut]
                                ]
                        aliasFromPref =
                            IntMap.fromList
                                [ (solvedKeyC, repKey)
                                | (solvedKey, node) <- IntMap.toList nodes
                                , case node of
                                    TyVar{} -> True
                                    _ -> False
                                , let solvedKeyC = getNodeId (canonical (NodeId solvedKey))
                                , aliasEligible solvedKeyC
                                , Just baseNid <- [IntMap.lookup solvedKeyC solvedToBasePrefLocalOut]
                                , let baseKey = getNodeId baseNid
                                , IntSet.member baseKey baseGammaSetLocalOut
                                , Just repKey <- [IntMap.lookup baseKey baseGammaRepLocalOut]
                                ]
                    in IntMap.union aliasFromBase aliasFromPref
                Nothing -> IntMap.empty
        requiredResultKeys =
            IntSet.fromList
                (concatMap requiredRoutingKeys (IntMap.toList requiredGamma))
        requiredRoutingKeys (requirementKey, requirement) =
            case rgbPlacement requirement of
                RequiredGammaAtCurrentScope ->
                    [ getNodeId (canonical resultRoot)
                    | resultRoot <- NonEmpty.toList (rgbResultRoots requirement)
                    ]
                RequiredGammaAtConstructionScope _ ->
                    [ getNodeId (canonical resultRoot)
                    | resultRoot <- NonEmpty.toList (rgbResultRoots requirement)
                    ]
                RequiredGammaAtNestedScope _ -> [requirementKey]
        gammaAliasLocal =
            IntMap.union
                requiredGlobalAliases
                (IntMap.withoutKeys gammaAliasLocalBase requiredResultKeys)
        baseGammaRepSetLocal =
            IntSet.fromList (IntMap.elems baseGammaRepLocalOut)
        reachableForBindersLocal =
            let aliasReachable =
                    [ repKey
                    | (aliasKey, repKey) <- IntMap.toList gammaAliasLocal
                    , IntSet.member aliasKey reachableForBinders0
                    ]
                typeRootC = canonical typeRoot0
                schemeBodyAliasReachable =
                    [ getNodeId (canonical (NodeId vidKey))
                    | (vidKey, node) <- IntMap.toList nodes
                    , TyVar{} <- [node]
                    , case VarStore.lookupVarBound constraint (NodeId vidKey) of
                        Just bnd ->
                            let bndC = canonical bnd
                            in bndC == typeRootC
                        Nothing -> False
                    ]
            in IntSet.union
                reachableForBinders0
                (IntSet.fromList (aliasReachable ++ schemeBodyAliasReachable))
        gammaKeyForLocal binderKey k =
            case IntMap.lookup k gammaAliasLocal of
                Just repKey | repKey == binderKey -> k
                Just repKey -> repKey
                Nothing -> k
        namedUnderGaLocal =
            [ NodeId nid
            | nid <- IntSet.toList namedUnderGaSetLocalOut
            ]
        boundHasNamedOutsideGammaLocal =
            case targetBound of
                Just bnd ->
                    let reachableBound = reachableFromWithBounds bnd
                        targetKey = getNodeId (canonical target0)
                        isNamedOutside nidInt =
                            let nidC = canonical (NodeId nidInt)
                                keyC = getNodeId nidC
                            in case IntMap.lookup keyC nodes of
                                Just TyVar{} ->
                                    if IntSet.member keyC nestedSchemeInteriorSet
                                        then False
                                        else case IntMap.lookup (nodeRefKey (typeRef nidC)) bindParents of
                                            Just (GenRef _, _) ->
                                                not (IntSet.member (gammaKeyForLocal targetKey keyC) namedUnderGaSetLocalOut)
                                            _ -> False
                                _ -> False
                    in any isNamedOutside (IntSet.toList reachableBound)
                Nothing -> False
        typeRootHasNamedOutsideGammaLocal = False
    sourceBinderRefsLocal <-
        case mbBindParentsGa of
            Just ga ->
                expandSourceBinderRefsWithPreference
                    solvedToBasePrefLocalOut
                    canonical
                    (gbiBaseToSolved ga)
                    (grSourceBinderRefs gpiRequirements)
            Nothing -> pure (grSourceBinderRefs gpiRequirements)
    pure GammaPlan
        { gpBaseGammaSet = baseGammaSetLocalOut
        , gpBaseGammaRep = baseGammaRepLocalOut
        , gpNamedUnderGaSet = namedUnderGaSetLocalOut
        , gpSolvedToBasePref = solvedToBasePrefLocalOut
        , gpGammaAlias = gammaAliasLocal
        , gpBaseGammaRepSet = baseGammaRepSetLocal
        , gpReachableForBinders = reachableForBindersLocal
        , gpGammaKeyFor = gammaKeyForLocal
        , gpNamedUnderGa = namedUnderGaLocal
        , gpBoundHasNamedOutsideGamma = boundHasNamedOutsideGammaLocal
        , gpTypeRootHasNamedOutsideGamma = typeRootHasNamedOutsideGammaLocal
        , gpRequiredGamma = requiredGamma
        , gpSourceBinderRefs = sourceBinderRefsLocal
        }
  where
    validateExpansionConstructionRoles =
        case (gpiScopeGen, gpiBindParentsGa) of
            (Just scopeGen, Just ga) ->
                foldM
                    (insertCertified scopeGen)
                    IntSet.empty
                    ( IntSet.toList
                        ( expansionConstructionRoleKeys
                            (gbiExpansionConstructionPlacements ga)
                        )
                    )
            _ -> pure IntSet.empty

    insertCertified scopeGen certified argumentKey =
        let argument = gpiCanonical (NodeId argumentKey)
            canonicalKey = getNodeId argument
            actualOwner = gpiFirstGenAncestor (typeRef argument)
            constructedBinding =
                IntMap.lookup
                    (nodeRefKey (typeRef argument))
                    gpiBindParents
        in case IntMap.lookup canonicalKey gpiNodes of
            Just TyVar{}
                | VarStore.isEliminatedVar gpiConstraint argument ->
                    Left
                        (ValidationFailed
                            [ "an expansion construction role was eliminated before generalization"
                            , "  node: " ++ show argument
                            , "  planning scope: " ++ show scopeGen
                            ])
                | Nothing <- actualOwner ->
                    Left
                        (ValidationFailed
                            [ "an expansion construction role has no owner in the current binding tree"
                            , "  node: " ++ show argument
                            , "  planning scope: " ++ show scopeGen
                            , "  constructed binding: " ++ show constructedBinding
                            ])
                | actualOwner /= Just scopeGen ->
                    pure certified
                | Just (_parent, BindFlex) <- constructedBinding ->
                    pure (IntSet.insert canonicalKey certified)
                | Just (_parent, BindRigid) <- constructedBinding ->
                    -- A role starts flexible in chi_e, but the final chi_p
                    -- witness may rigidify it.  Gamma is defined from chi_p,
                    -- so such a role is no longer a binder candidate.
                    pure certified
                | otherwise ->
                    Left
                        (ValidationFailed
                            [ "an expansion construction role is not flexibly bound in the current tree"
                            , "  node: " ++ show argument
                            , "  planning scope: " ++ show scopeGen
                            , "  current owner: " ++ show actualOwner
                            , "  constructed binding: " ++ show constructedBinding
                            ])
            node ->
                Left
                    (ValidationFailed
                        [ "an expansion construction role is not a live type variable"
                        , "  node: " ++ show argument
                        , "  value: " ++ show node
                        ])

    validateRequiredGlobalAliases required =
        case gpiBindParentsGa of
            Nothing -> pure IntMap.empty
            Just ga ->
                foldM
                    (insertAlias ga)
                    IntMap.empty
                    [ entry
                    | entry@(_, requirement) <- IntMap.toList required
                    , requiredGammaPlacementIsLocal
                        (rgbPlacement requirement)
                    ]

    insertAlias ga acc (resultKey, requirement) =
        case IntMap.lookup (getNodeId (rgbExteriorNode requirement)) (gbiBaseToSolved ga) of
            Nothing -> pure acc
            Just globalSolved ->
                let globalKey = getNodeId (gpiCanonical globalSolved)
                in if globalKey == resultKey
                    then pure acc
                    else
                        case IntMap.lookup globalKey acc of
                            Nothing -> pure (IntMap.insert globalKey resultKey acc)
                            Just existingResult
                                | existingResult == resultKey -> pure acc
                                | otherwise ->
                                    Left
                                        (ValidationFailed
                                            [ "one global exterior representative feeds multiple edge-local Gamma results"
                                            , "  global representative: " ++ show (NodeId globalKey)
                                            , "  first result: " ++ show (NodeId existingResult)
                                            , "  second result: " ++ show (NodeId resultKey)
                                            ])

    validateRequiredGamma = do
        mergedRequirements <-
            foldM mergeRequired [] (grRequiredGammaBinders gpiRequirements)
        foldM insertRequired IntMap.empty mergedRequirements

    mergeRequired existing requirement =
        case find (sameExterior requirement) existing of
            Nothing -> pure (existing ++ [requirement])
            Just prior
                | rgbPlacement prior /= rgbPlacement requirement ->
                    Left
                        (ValidationFailed
                            [ "root RaiseMerge entries assign one Gamma exterior to different construction scopes"
                            , "  exterior: " ++ show (rgbExteriorNode requirement)
                            , "  first placement: " ++ show (rgbPlacement prior)
                            , "  second placement: " ++ show (rgbPlacement requirement)
                            ])
                | alphaEqType
                    (rgbOperatedType prior)
                    (rgbOperatedType requirement) ->
                    pure (map (mergeMatchingExterior requirement) existing)
                | TBottom <- rgbOperatedType prior ->
                    -- Bottom is the neutral lower-bound obligation.  Preserve
                    -- the operated-root provenance of the non-bottom edge when
                    -- it supplies the authoritative bound for this exterior.
                    pure (map (replaceMatchingExterior requirement) existing)
                | TBottom <- rgbOperatedType requirement ->
                    pure (map (mergeMatchingExterior requirement) existing)
                | otherwise ->
                    Left
                        (ValidationFailed
                            [ "root RaiseMerge entries require incompatible bounds for one Gamma exterior"
                            , "  exterior: " ++ show (rgbExteriorNode requirement)
                            , "  first bound: " ++ show (rgbOperatedType prior)
                            , "  second bound: " ++ show (rgbOperatedType requirement)
                            ])

    sameExterior left right =
        rgbExteriorNode left == rgbExteriorNode right

    mergeMatchingExterior incoming prior
        | sameExterior incoming prior =
            prior
                { rgbEdgeIds =
                    foldl
                        appendEdgeId
                        (rgbEdgeIds prior)
                        (NonEmpty.toList (rgbEdgeIds incoming))
                , rgbResultRoots =
                    foldl
                        appendResultRoot
                        (rgbResultRoots prior)
                        (NonEmpty.toList (rgbResultRoots incoming))
                }
        | otherwise = prior

    replaceMatchingExterior incoming prior
        | sameExterior incoming prior =
            incoming
                { rgbEdgeIds =
                    foldl
                        appendEdgeId
                        (rgbEdgeIds incoming)
                        (NonEmpty.toList (rgbEdgeIds prior))
                , rgbResultRoots =
                    foldl
                        appendResultRoot
                        (rgbResultRoots incoming)
                        (NonEmpty.toList (rgbResultRoots prior))
                }
        | otherwise = prior

    appendResultRoot roots resultRoot
        | resultRoot `elem` roots = roots
        | otherwise = roots <> NonEmpty.singleton resultRoot

    appendEdgeId edges edgeId
        | edgeId `elem` edges = edges
        | otherwise = edges <> NonEmpty.singleton edgeId

    insertRequired acc requirement =
        case gpiBindParentsGa of
            Nothing ->
                Left
                    (ValidationFailed
                        [ "root RaiseMerge Γ construction requires the frozen base graph"
                        , "  requirement: " ++ show requirement
                        ])
            Just ga -> do
                let requirementExterior = rgbExteriorNode requirement
                    operatedRoot = rgbOperatedRoot requirement
                    resultRoots = NonEmpty.toList (rgbResultRoots requirement)
                    primaryResultRoot = NonEmpty.head (rgbResultRoots requirement)
                    baseConstraint = gbiBaseConstraint ga
                exterior <-
                    case lookupNodeIn (cNodes baseConstraint) requirementExterior of
                        Just _ -> pure requirementExterior
                        Nothing ->
                            case
                                IntMap.lookup
                                    (getNodeId requirementExterior)
                                    (gbiSolvedToBase ga)
                            of
                                Just baseExterior
                                    | Just _ <-
                                        lookupNodeIn
                                            (cNodes baseConstraint)
                                            baseExterior ->
                                        pure baseExterior
                                _ ->
                                    Left
                                        (ValidationFailed
                                            [ "root RaiseMerge exterior has no frozen base provenance"
                                            , "  exterior: " ++ show requirementExterior
                                            ])
                case lookupNodeIn (cNodes baseConstraint) exterior of
                    Just _ -> pure ()
                    Nothing ->
                        Left
                            (ValidationFailed
                                [ "root RaiseMerge exterior provenance is absent from the frozen base graph"
                                , "  requirement exterior: " ++ show requirementExterior
                                , "  exterior: " ++ show exterior
                                ])
                let bindParentsBase = gbiBindParentsBase ga
                    placementOwner =
                        case rgbPlacement requirement of
                            RequiredGammaAtCurrentScope ->
                                GenRef <$> gpiScopeGen
                            RequiredGammaAtConstructionScope owner ->
                                Just owner
                            RequiredGammaAtNestedScope owner ->
                                Just owner
                    placementIsValid =
                        case (rgbPlacement requirement, placementOwner) of
                            (RequiredGammaAtCurrentScope, Just owner) ->
                                flexiblyOwnedByScope bindParentsBase owner exterior
                            (RequiredGammaAtConstructionScope owner, Just _) ->
                                flexiblyOwnedByScope bindParentsBase owner exterior
                                    || parentlessResultEndpointOwnedByCurrentScope
                                        bindParentsBase
                                        owner
                                        exterior
                                        requirement
                            (RequiredGammaAtNestedScope owner, Just _) ->
                                -- Nested placement is positive evidence
                                -- constructed by 'placeNestedRootRequirements':
                                -- it has already checked either the exact
                                -- term-local closure or the frozen exterior
                                -- path.  Canonicalization may subsequently
                                -- move that exterior to another graph path, so
                                -- rechecking the path here would discard valid
                                -- lexical ownership.  The remaining invariant
                                -- at this solved-graph boundary is that the
                                -- certified owner belongs to this scope.
                                withinCurrentScope bindParentsBase owner
                            _ -> False
                if placementIsValid
                    then pure ()
                    else
                        Left
                            (ValidationFailed
                                [ "root RaiseMerge exterior is not owned by its declared construction Gamma"
                                , "  requirement exterior: " ++ show requirementExterior
                                , "  frozen exterior: " ++ show exterior
                                , "  requirement: " ++ show requirement
                                , "  current scope: " ++ show gpiScopeGen
                                , "  declared owner: " ++ show placementOwner
                                , "  target: " ++ show gpiTarget0
                                , "  path: "
                                    ++ show
                                        (Binding.bindingPathToRootLocal
                                            (gbiBindParentsBase ga)
                                            (typeRef exterior)
                                        )
                                , "  path bindings: "
                                    ++ show
                                        [ (ref, IntMap.lookup (nodeRefKey ref) (gbiBindParentsBase ga))
                                        | ref <-
                                            either
                                                (const [])
                                                id
                                                ( Binding.bindingPathToRootLocal
                                                    (gbiBindParentsBase ga)
                                                    (typeRef exterior)
                                                )
                                        ]
                                ])
                case lookupNodeIn (cNodes baseConstraint) operatedRoot of
                    Nothing ->
                        Left
                            (ValidationFailed
                                [ "root RaiseMerge operated source root is absent from the frozen base graph"
                                , "  operated root: " ++ show operatedRoot
                                ])
                    Just _ -> pure ()
                globalSolved <-
                    case IntMap.lookup (getNodeId exterior) (gbiBaseToSolved ga) of
                        Just node -> pure node
                        Nothing ->
                            Left
                                (ValidationFailed
                                    [ "root RaiseMerge exterior has no base-to-solved bridge"
                                    , "  exterior: " ++ show exterior
                                    , "  result roots: " ++ show resultRoots
                                    ])
                let globalSolvedC = gpiCanonical globalSolved
                    globalKey = getNodeId globalSolvedC
                    primaryResultC = gpiCanonical primaryResultRoot
                    primaryResultKey = getNodeId primaryResultC
                    resultEntries =
                        [ (resultRoot, gpiCanonical resultRoot)
                        | resultRoot <- resultRoots
                        ]
                    resultKeys =
                        IntSet.fromList
                            [ getNodeId resultC
                            | (_, resultC) <- resultEntries
                            ]
                    sourceRouteKeys =
                        [ getNodeId sourceNode
                        | sourceKey <-
                            getNodeId requirementExterior
                                : getNodeId operatedRoot
                                : map getNodeId resultRoots
                        , Just sourceRef <-
                            [IntMap.lookup sourceKey (grSourceBinderRefs gpiRequirements)]
                        , Just sourceNode <- [typeBinderRefNode sourceRef]
                        ]
                    nestedRouteCandidates =
                        sourceRouteKeys ++ [getNodeId requirementExterior]
                    nestedRouteKey =
                        listToMaybe
                            [ candidate
                            | candidate <- nestedRouteCandidates
                            , case IntMap.lookup candidate gpiNodes of
                                Just TyVar{} -> True
                                _ -> False
                            ]
                    requirementKey =
                        case rgbPlacement requirement of
                            RequiredGammaAtCurrentScope -> primaryResultKey
                            RequiredGammaAtConstructionScope _ ->
                                primaryResultKey
                            RequiredGammaAtNestedScope _ ->
                                case nestedRouteKey of
                                    Just key -> key
                                    Nothing -> primaryResultKey
                case IntMap.lookup globalKey gpiNodes of
                    Just _ -> pure ()
                    Nothing ->
                        Left
                            (ValidationFailed
                                [ "root RaiseMerge global exterior bridge is absent from the solved graph"
                                , "  exterior: " ++ show exterior
                                , "  mapped result: " ++ show globalSolvedC
                                ])
                mapM_
                    (\(_, resultC) ->
                        case IntMap.lookup (getNodeId resultC) gpiNodes of
                            Just _ -> pure ()
                            Nothing ->
                                Left
                                    (ValidationFailed
                                        [ "root RaiseMerge result root is absent from the solved graph"
                                        , "  exterior: " ++ show exterior
                                        , "  global representative: " ++ show globalSolvedC
                                        , "  trace result: " ++ show resultC
                                        ]))
                    resultEntries
                -- Current-scope requirements route through the witness result.
                -- A nested Figure 15.3.5 constructor instead retains its own
                -- live source/exterior key in the enclosing closure scheme.
                -- This lets two lexical Gammas share a solved result without
                -- collapsing their distinct binder identities.
                case
                    find
                        ( \(_, existing) ->
                            rgbPlacement existing == rgbPlacement requirement
                                && requirementsOverlapResults resultKeys existing
                        )
                        (IntMap.toList acc)
                  of
                    Just (existingKey, existing) ->
                        Left
                            (ValidationFailed
                                [ "multiple root RaiseMerge Gamma entries in one construction scope collapse to one edge-local result"
                                , "  result roots: " ++ show resultEntries
                                , "  existing binder key: " ++ show (NodeId existingKey)
                                , "  first: " ++ show existing
                                , "  second: " ++ show requirement
                                ])
                    Nothing ->
                        case IntMap.lookup requirementKey acc of
                            Nothing ->
                                pure (IntMap.insert requirementKey requirement acc)
                            Just existing ->
                                Left
                                    (ValidationFailed
                                        [ "distinct root RaiseMerge Gamma scopes have no distinct live routing keys"
                                        , "  routing key: " ++ show (NodeId requirementKey)
                                        , "  source route candidates: " ++ show (map NodeId nestedRouteCandidates)
                                        , "  first: " ++ show existing
                                        , "  second: " ++ show requirement
                                        ])

    requirementsOverlapResults resultKeys requirement =
        any
            (\resultRoot ->
                IntSet.member
                    (getNodeId (gpiCanonical resultRoot))
                    resultKeys)
            (rgbResultRoots requirement)

    flexiblyOwnedByScope bindParents owner exterior =
        go IntSet.empty (typeRef exterior)
      where
        go seen child
            | IntSet.member childKey seen = False
            | otherwise =
                case IntMap.lookup childKey bindParents of
                    Just (parent, BindFlex)
                        | parent == owner -> True
                        | TypeRef{} <- parent ->
                            go (IntSet.insert childKey seen) parent
                    _ -> False
          where
            childKey = nodeRefKey child

    -- A root RaiseMerge can expose its Gamma exterior directly as the result
    -- endpoint.  Such an exterior is deliberately parentless in the frozen
    -- binding tree, so the exact construction-scope stamp produced by
    -- 'placeNestedRootRequirements' is the ownership proof.  Keep the check
    -- narrow: raw current-scope requirements, non-result exteriors, and
    -- placements owned by any other scope still require a flexible path.
    parentlessResultEndpointOwnedByCurrentScope
        bindParents
        owner
        exterior
        requirement =
            Just owner == (GenRef <$> gpiScopeGen)
                && IntMap.notMember
                    (nodeRefKey (typeRef exterior))
                    bindParents
                && exterior `elem` rgbResultRoots requirement

    -- A nested source constructor can share the same gen node as its
    -- enclosing constructor; 'LocalGammaOwner' distinguishes those lexical
    -- placements before requirements reach this graph planner.  Here we only
    -- verify that the declared graph scope is the current scope or a true
    -- descendant of it, never a sibling or ancestor.
    withinCurrentScope bindParents nestedOwner =
        case gpiScopeGen of
            Nothing -> False
            Just currentScope ->
                let currentOwner = GenRef currentScope
                in nestedOwner == currentOwner
                    || case Binding.bindingPathToRootLocal bindParents nestedOwner of
                        Right (_ : ancestors) -> currentOwner `elem` ancestors
                        _ -> False
