{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module MLF.Constraint.Presolution.Plan.ReifyPlan
  ( ReifyPlan (..),
    SchemeTypeChoice (..),
    ReifyPlanInput (..),
    buildReifyPlan,
    ReifyBindingEnv (..),
    bindingFor,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (sortOn)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.BindingUtil (bindingScopeFor)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Plan.BinderPlan (AliasEnv (..), GaBindParentsInfo (..), bindingScopeGen, boundMentionsSelfAliasFor, hasExplicitBoundFor, isTargetSchemeBinderFor)
import MLF.Constraint.Presolution.Plan.Normalize (containsForall)
import qualified MLF.Constraint.Presolution.Plan.SchemeRoots as SchemeRoots
import MLF.Constraint.Presolution.View (PresolutionView)
import MLF.Constraint.Types.Graph
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Reify.Core
  ( reifyBoundWithRefs,
    reifyBoundWithRefsOnConstraint,
  )
import MLF.Reify.TypeOps (freeTypeVarRefsType)
import MLF.Types.Elab
  ( BoundType,
    ElabType,
    TypeBinderRef,
    Ty (..),
    elabToBound,
    typeBinderIdentityFromNode,
    typeBinderRefFromIdentity,
    typeBinderRefName,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
  )
import MLF.Util.ElabError (ElabError (..), bindingToElab)
import MLF.Util.Names (alphaName)

data ReifyPlan = ReifyPlan
  { rpSubst :: IntMap.IntMap TypeBinderRef,
    rpSubstForBound :: Int -> IntMap.IntMap TypeBinderRef,
    rpSubstForBoundBase :: Int -> IntMap.IntMap TypeBinderRef,
    rpTypeRootForReify :: NodeId,
    rpSubstForReify :: IntMap.IntMap TypeBinderRef,
    rpSchemeTypeChoice :: SchemeTypeChoice,
    rpBindingScopeGen :: NodeId -> Maybe GenNodeId,
    rpHasExplicitBound :: NodeId -> Bool,
    rpIsTargetSchemeBinder :: NodeId -> Bool,
    rpBoundMentionsSelfAlias :: NodeId -> Bool,
    rpContainsForall :: ElabType -> Bool
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
    rpiSubst0 :: IntMap.IntMap TypeBinderRef,
    rpiGammaAlias :: IntMap.IntMap Int,
    rpiNestedSchemeInteriorSet :: IntSet.IntSet,
    rpiBaseGammaRep :: IntMap.IntMap Int,
    rpiAliasBinderBases :: IntSet.IntSet,
    rpiSolvedToBasePref :: IntMap.IntMap NodeId,
    rpiTypeRoot :: NodeId
  }

data ReifyBindingEnv p = ReifyBindingEnv
  { rbeConstraint :: Constraint p,
    rbeNodes :: IntMap.IntMap TyNode,
    rbeCanonical :: NodeId -> NodeId,
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
canonicalizeSubstRefs canonical =
  IntMap.mapWithKey
    ( \key ref ->
        typeBinderRefFromIdentity
          (typeBinderIdentityFromNode (canonical (NodeId key)))
          (typeBinderRefName ref)
    )

buildReifyPlan :: ReifyPlanInput p -> ReifyPlan
buildReifyPlan ReifyPlanInput {..} =
  let extraNames = zipWith alphaName [rpiExtraNameStart ..] rpiOrderedExtra
      substExtra =
        IntMap.fromList
          [ (key, typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name)
          | (key, name) <- zip rpiOrderedExtra extraNames
          ]
      substBaseLocal = IntMap.unions [rpiSubst0, substExtra]
      substAliasesLocal =
        IntMap.fromList
          [ (aliasKey, ref)
          | (aliasKey, binderKey) <- IntMap.toList rpiGammaAlias,
            aliasKey /= binderKey,
            not (IntSet.member aliasKey rpiNestedSchemeInteriorSet),
            Just ref <- [IntMap.lookup binderKey substBaseLocal]
          ]
      substAliasesCanonLocal =
        IntMap.fromList
          [ (aliasKeyC, ref)
          | (aliasKey, ref) <- IntMap.toList substAliasesLocal,
            let aliasKeyC = getNodeId (rpiCanonical (NodeId aliasKey)),
            aliasKeyC /= aliasKey,
            not (IntMap.member aliasKeyC substBaseLocal),
            not (IntMap.member aliasKeyC substAliasesLocal)
          ]
      typeRootReachable = rpiReachableFromWithBounds rpiTypeRoot
      substAliasesFromBaseLocal =
        IntMap.fromList
          [ (solvedKey, ref)
          | (solvedKey, baseN) <- IntMap.toList rpiSolvedToBasePref,
            let baseKey = getNodeId baseN,
            Just repKey <- [IntMap.lookup baseKey rpiBaseGammaRep],
            Just ref <- [IntMap.lookup repKey substBaseLocal],
            solvedKey /= repKey,
            not (IntSet.member solvedKey rpiNestedSchemeInteriorSet)
              || ( not (IntSet.member baseKey rpiNestedSchemeInteriorSet)
                     && IntSet.member solvedKey typeRootReachable
                 )
          ]
      substLocal =
        IntMap.unions
          [ substBaseLocal,
            substAliasesLocal,
            substAliasesCanonLocal,
            substAliasesFromBaseLocal
          ]
      substRefsLocal = canonicalizeSubstRefs rpiCanonical substLocal
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
            Just ref <- [IntMap.lookup binderKey' substBaseLocal]
          ]
      substForBoundLocal binderKey =
        filterAliasKeysLocal $
          IntMap.union substBaseLocal (substAliasesForLocal binderKey)
      substForBoundRefsLocal binderKey =
        canonicalizeSubstRefs rpiCanonical (substForBoundLocal binderKey)
      substBaseByKeyLocal =
        case rpiBindParentsGa of
          Just ga ->
            let fromBaseRep =
                  [ (baseKey, ref)
                  | (baseKey, solvedKey) <- IntMap.toList rpiBaseGammaRep,
                    Just ref <- [IntMap.lookup solvedKey substBaseLocal]
                  ]
                solvedPreference =
                  [ (getNodeId baseN, ref)
                  | (solvedKey, ref) <- IntMap.toList substBaseLocal,
                    Just baseN <- [IntMap.lookup solvedKey rpiSolvedToBasePref]
                  ]
                fromBaseToSolved =
                  [ (baseKey, ref)
                  | (baseKey, solvedN) <- IntMap.toList (gbiBaseToSolved ga),
                    let solvedKey = getNodeId (rpiCanonical solvedN),
                    Just ref <- [IntMap.lookup solvedKey substBaseLocal]
                  ]
             in IntMap.unions
                  [ IntMap.fromListWith (\_ old -> old) fromBaseRep,
                    IntMap.fromListWith (\_ old -> old) solvedPreference,
                    IntMap.fromListWith (\_ old -> old) fromBaseToSolved
                  ]
          Nothing -> IntMap.empty
      substForBoundBaseLocal _binderKey = filterAliasKeysLocal substBaseByKeyLocal
      substForBoundBaseRefsLocal binderKey =
        canonicalizeSubstRefs id (substForBoundBaseLocal binderKey)
      (typeRootForReifyLocal, substForReifyLocal) =
        case rpiBindParentsGa of
          Just _ ->
            case IntMap.lookup (getNodeId (rpiCanonical rpiTypeRoot)) rpiSolvedToBasePref of
              Just _baseN
                | rpiTargetIsBaseLike ->
                    (rpiTypeRoot, substLocal)
              Just baseN
                | rpiCanonical baseN /= rpiCanonical rpiTypeRoot ->
                    (baseN, substBaseByKeyLocal)
              _ -> (rpiTypeRoot, substLocal)
          Nothing -> (rpiTypeRoot, substLocal)
      typeRootC = rpiCanonical rpiTypeRoot
      (schemeOwnerFromBody, schemeOwnerFromBodyIsAlias) =
        SchemeRoots.schemeOwnerFromBody rpiSchemeRootsPlan rpiSolvedToBasePref typeRootC
      ownersByRoot =
        [ gnId gen
        | gen <- NodeAccess.allGenNodes rpiConstraint,
          any (\root -> rpiCanonical root == typeRootC) (gnSchemes gen)
        ]
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
   in ReifyPlan
        { rpSubst = substRefsLocal,
          rpSubstForBound = substForBoundRefsLocal,
          rpSubstForBoundBase = substForBoundBaseRefsLocal,
          rpTypeRootForReify = typeRootForReifyLocal,
          rpSubstForReify = substForReifyLocal,
          rpSchemeTypeChoice = schemeTypeChoice,
          rpBindingScopeGen = bindingScopeGenLocal,
          rpHasExplicitBound = hasExplicitBoundLocal,
          rpIsTargetSchemeBinder = isTargetSchemeBinderLocal,
          rpBoundMentionsSelfAlias = boundMentionsSelfAliasLocal,
          rpContainsForall = containsForall
        }

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
          rpSubstForBoundBase = substForBoundBase
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
      binderRef =
        case typeBinderRefNode binderRef0 of
          Just node
            | canonical node == bNodeC -> binderRef0
          _ ->
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode bNodeC)
              (typeBinderRefName binderRef0)
      name = typeBinderRefName binderRef
      binderIsNamed = IntSet.member (getNodeId bNodeC) namedUnderGaSet
      binderKey = getNodeId bNodeC
      substForBoundRefs = substForBound binderKey
      substForBoundNames = IntMap.map typeBinderRefName substForBoundRefs
      substNameSetForBound = Set.fromList (IntMap.elems substForBoundNames)
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
  let mbBaseRoot =
        if boundIsLocalSchemeBody || boundParentIsBinder
          then Nothing
          else case mbBindParentsGa of
            Just ga ->
              let baseConstraint = gbiBaseConstraint ga
                  baseBoundForBinder =
                    case IntMap.lookup binderKey solvedToBasePref of
                      Just baseBinder ->
                        case VarStore.lookupVarBound baseConstraint baseBinder of
                          Just baseBnd ->
                            case IntMap.lookup (getNodeId baseBnd) schemeRootByBodyBase of
                              Just baseSchemeRoot ->
                                if baseSchemeRoot == baseBinder
                                  then Just baseBnd
                                  else Just baseSchemeRoot
                              Nothing -> Just baseBnd
                          Nothing -> Nothing
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
               in case baseBoundForBinder of
                    Just _ -> baseBoundForBinder
                    Nothing -> fallbackFromBoundRoot
            Nothing -> Nothing
  boundTy0 <-
    case (mbBindParentsGa, mbBaseRoot) of
      (Just ga, Just baseRoot) ->
        reifyBoundWithRefsOnConstraint
          (gbiBaseConstraint ga)
          (substForBoundBase binderKey)
          baseRoot
      _ ->
        reifyBoundWithRefs
          resForReify
          substForBoundFiltered
          boundRoot
  let canonicalKeyForRef ref =
        case typeBinderRefNode ref of
          Just node -> Just (getNodeId (canonical node))
          Nothing -> Nothing
      fallbackAliasFor ref =
        case (uniqueUnboundedName, canonicalKeyForRef ref) of
          (Just fallbackName, Just keyC)
            | boundIsLocalSchemeBody
                && not (Set.member (typeBinderRefName ref) substNameSetForBound) ->
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
             in case IntMap.lookup repKey substForBoundRefs of
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
                  case IntMap.lookup bndKey substForBoundNames of
                    Just nm -> nm
                    Nothing -> "t" ++ show bndKey
                refForBound =
                  case IntMap.lookup bndKey substForBoundRefs of
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
        if boundMentionsSelfAlias bNodeC
          then TBottom
          else boundTy0'
      boundTy0Aliased = substAliasTy [] boundTy0''
      boundTy0Normalized = normalizeSelfTy binderRef boundTy0Aliased
      extraBoundRefs =
        let isAliasBound ref =
              case canonicalKeyForRef ref of
                Just keyC ->
                  let repKey = IntMap.findWithDefault keyC keyC gammaAlias
                   in IntMap.member repKey substForBoundNames
                Nothing -> False
            freeRefs = freeTypeVarRefsType boundTy0Normalized
         in [ ref
            | ref <- freeRefs,
              not (refMember ref (IntMap.elems substForBoundRefs)),
              not (isAliasBound ref)
            ]
      extraBoundNames = map typeBinderRefName extraBoundRefs
      boundTy =
        foldr
          (\ref acc -> TForallRef ref Nothing acc)
          boundTy0Normalized
          (sortOn typeBinderRefName extraBoundRefs)
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
        if binderIsNamed
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
        Just (TVarRef ref)
          | any (typeBinderRefsSameIdentity ref) substBinderRefs ->
              -- Inter-binder alias bound from recursive cycle;
              -- normalize to unbounded (safe over-approximation).
              Right Nothing
          | otherwise ->
              Left $
                ValidationFailed
                  [ "alias bounds survived scheme finalization: "
                      ++ show [name]
                  ]
        Nothing -> Right Nothing
        Just bnd -> case elabToBound bnd of
          Left err -> Left $ ValidationFailed [err]
          Right typed -> Right (Just typed)
  case mbBoundTyped of
    Left err -> Left err
    Right typed -> pure (binderRef, typed)

{- Note [Inter-binder alias bounds in recursive types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When reifying a binding whose alias bound is another binder within the same
generalization group (i.e. the bound is a TVar whose name appears in
`rpSubst`), we normalize the bound to Nothing (unbounded) rather than
rejecting it as "alias bounds survived scheme finalization".

This situation arises legitimately in recursive types.  During presolution the
solver may introduce alias edges between co-recursive binders; these edges
carry `TVar` bounds that point to peer binders rather than concrete types.
Such bounds have no representation in the surface type language—they are
artefacts of the internal constraint graph—so the faithful reification is to
treat the binding as unbounded (∀α.…) rather than bounded (∀(α ≥ β).…)
where β is another binder in the same group.

This is a safe over-approximation: dropping an alias bound can only widen
the set of types the variable may be instantiated to, never narrow it.
The thesis (§5, graphic constraints) permits unbounded quantification
whenever a tighter bound cannot be expressed in the target language. -}
