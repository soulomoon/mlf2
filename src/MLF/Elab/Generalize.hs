{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : MLF.Elab.Generalize
-- Description : Apply generalization plans to produce elaborated types
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
--
-- This module applies generalization plans (produced by the presolution phase)
-- to produce elaborated types with explicit polymorphism. It coordinates the
-- binder naming, scheme reification, and finalization steps.
--
-- = Process
--
-- 1. Receive 'GeneralizePlan' from presolution
-- 2. Build 'BinderPlan' for naming quantified variables
-- 3. Reify the type using 'ReifyPlan'
-- 4. Finalize schemes with proper binders
--
-- See 'MLF.Constraint.Presolution.Plan' for the plan generation logic.
module MLF.Elab.Generalize
  ( GaBindParents (..),
    applyGeneralizePlan,
    inlineRigidTypes,
    shadowCompareTypes,
    selectSolvedOrderWithShadow,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Finalize (presolutionViewFromSnapshot)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Plan
  ( GeneralizePlan (..),
    ReifyPlan (..),
  )
import MLF.Constraint.Presolution.Plan.BinderPlan (BinderPlan (..))
import MLF.Constraint.Presolution.Plan.Context
  ( GaBindParents (..),
    GeneralizeCtx (..),
    GeneralizeEnv (..),
    traceGeneralizeM,
  )
import MLF.Constraint.Presolution.Plan.Finalize (FinalizeInput (..), finalizeScheme)
import qualified MLF.Constraint.Presolution.Plan.ReifyPlan as Reify
import MLF.Constraint.Presolution.Plan.SchemeRoots
  ( SchemeRootInfo (..),
    SchemeRootsPlan (..),
    allowBoundTraversalFor,
  )
import MLF.Constraint.Presolution.Plan.Target (TypeRootPlan (..))
import MLF.Constraint.Presolution.View (PresolutionView (..), pvCanonicalMap)
import MLF.Constraint.Types.Graph
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Elab.Types
import MLF.Reify.Core
  ( reifyBoundWithRefs,
    reifyBoundWithRefsOnConstraint,
    reifyTypeWithRefsNoFallback,
    reifyTypeWithRefsNoFallbackOnConstraint,
  )
import MLF.Reify.TypeOps (alphaEqType, inlineAliasBoundsWithBy)
import MLF.Util.Graph (reachableFromStop)
import qualified MLF.Util.IntMapUtils as IntMapUtils
import MLF.Util.Names (alphaName)

-- | Generate a name for a rigid type variable based on its key.
rigidNameFor :: Int -> String
rigidNameFor key = "__rigid" ++ show key

canonicalizeSubstRefs :: (NodeId -> NodeId) -> IntMap.IntMap TypeBinderRef -> IntMap.IntMap TypeBinderRef
canonicalizeSubstRefs canonical =
  IntMap.mapWithKey
    ( \key ref ->
        typeBinderRefFromIdentity
          (typeBinderIdentityFromNode (canonical (NodeId key)))
          (typeBinderRefName ref)
    )

buildForallTypeRefs :: [(TypeBinderRef, Maybe BoundType)] -> ElabType -> ElabType
buildForallTypeRefs binds body = foldr (\(ref, b) t -> TForallRef ref b t) body binds

-- | Validate that solved-order and base-path shadow reification are semantically equivalent.
shadowCompareTypes :: String -> ElabType -> ElabType -> Either ElabError ()
shadowCompareTypes context solvedTy baseTy =
  shadowCompareTypesWithDetails context defaultShadowDetails solvedTy baseTy

shadowCompareTypesWithDetails :: String -> [String] -> ElabType -> ElabType -> Either ElabError ()
shadowCompareTypesWithDetails context detailLines solvedTy baseTy
  | alphaEqType solvedTy baseTy || alphaEqTypeModuloVarRenaming solvedTy baseTy = Right ()
  | otherwise =
      Left $
        ValidationFailed
          ( [ "shadow reify mismatch",
              "context=" ++ context
            ]
              ++ detailLines
              ++ [ "solved=" ++ pretty solvedTy,
                   "base=" ++ pretty baseTy
                 ]
          )

data RenameEnv = RenameEnv
  { reForward :: [(TypeBinderRef, TypeBinderRef)],
    reBackward :: [(TypeBinderRef, TypeBinderRef)]
  }

alphaEqTypeModuloVarRenaming :: ElabType -> ElabType -> Bool
alphaEqTypeModuloVarRenaming tyL tyR =
  case goType (RenameEnv [] []) tyL tyR of
    Just _ -> True
    Nothing -> False
  where
    goType :: RenameEnv -> ElabType -> ElabType -> Maybe RenameEnv
    goType env t1 t2 = case (t1, t2) of
      (TVarRef ref1, TVarRef ref2) ->
        matchVar env ref1 ref2
      (TArrow a1 b1, TArrow a2 b2) -> do
        env' <- goType env a1 a2
        goType env' b1 b2
      (TCon c1 args1, TCon c2 args2)
        | c1 == c2 ->
            goTypes env (NonEmpty.toList args1) (NonEmpty.toList args2)
      (TBase b1, TBase b2)
        | b1 == b2 ->
            Just env
      (TBottom, TBottom) ->
        Just env
      (TVarAppRef ref1 args1, TVarAppRef ref2 args2) -> do
        env' <- matchVar env ref1 ref2
        goTypes env' (NonEmpty.toList args1) (NonEmpty.toList args2)
      (TForallRef ref1 mb1 body1, TForallRef ref2 mb2 body2) -> do
        env' <- goMaybeBound env mb1 mb2
        withScopedVar env' ref1 ref2 (\scoped -> goType scoped body1 body2)
      (TMuRef ref1 body1, TMuRef ref2 body2) ->
        withScopedVar env ref1 ref2 (\scoped -> goType scoped body1 body2)
      _ ->
        Nothing

    goBound :: RenameEnv -> BoundType -> BoundType -> Maybe RenameEnv
    goBound env b1 b2 = case (b1, b2) of
      (TArrow a1 b1', TArrow a2 b2') -> do
        env' <- goType env a1 a2
        goType env' b1' b2'
      (TCon c1 args1, TCon c2 args2)
        | c1 == c2 ->
            goTypes env (NonEmpty.toList args1) (NonEmpty.toList args2)
      (TBase base1, TBase base2)
        | base1 == base2 ->
            Just env
      (TBottom, TBottom) ->
        Just env
      (TVarAppRef ref1 args1, TVarAppRef ref2 args2) -> do
        env' <- matchVar env ref1 ref2
        goTypes env' (NonEmpty.toList args1) (NonEmpty.toList args2)
      (TForallRef ref1 mb1 body1, TForallRef ref2 mb2 body2) -> do
        env' <- goMaybeBound env mb1 mb2
        withScopedVar env' ref1 ref2 (\scoped -> goType scoped body1 body2)
      (TMuRef ref1 body1, TMuRef ref2 body2) ->
        withScopedVar env ref1 ref2 (\scoped -> goType scoped body1 body2)
      _ ->
        Nothing

    goTypes :: RenameEnv -> [ElabType] -> [ElabType] -> Maybe RenameEnv
    goTypes env left right = case (left, right) of
      ([], []) -> Just env
      (l : ls, r : rs) -> do
        env' <- goType env l r
        goTypes env' ls rs
      _ -> Nothing

    goMaybeBound :: RenameEnv -> Maybe BoundType -> Maybe BoundType -> Maybe RenameEnv
    goMaybeBound env mb1 mb2 = case (mb1, mb2) of
      (Nothing, Nothing) -> Just env
      (Just b1, Just b2) -> goBound env b1 b2
      _ -> Nothing

    matchVar :: RenameEnv -> TypeBinderRef -> TypeBinderRef -> Maybe RenameEnv
    matchVar env@RenameEnv {reForward = forward, reBackward = backward} v1 v2 =
      case (lookupRef v1 forward, lookupRef v2 backward) of
        (Just mappedV2, Just mappedV1)
          | typeBinderRefsSameIdentity mappedV2 v2 && typeBinderRefsSameIdentity mappedV1 v1 ->
              Just env
        (Just mappedV2, Nothing)
          | typeBinderRefsSameIdentity mappedV2 v2 ->
              Just env {reBackward = insertPair v2 v1 backward}
        (Nothing, Just mappedV1)
          | typeBinderRefsSameIdentity mappedV1 v1 ->
              Just env {reForward = insertPair v1 v2 forward}
        (Nothing, Nothing)
          | refsCanRename v1 v2 ->
          Just
            env
              { reForward = insertPair v1 v2 forward,
                reBackward = insertPair v2 v1 backward
              }
        _ ->
          Nothing

    withScopedVar ::
      RenameEnv ->
      TypeBinderRef ->
      TypeBinderRef ->
      (RenameEnv -> Maybe RenameEnv) ->
      Maybe RenameEnv
    withScopedVar env@RenameEnv {reForward = forward, reBackward = backward} v1 v2 runScoped = do
      let oldForward = lookupEntry v1 forward
          oldBackward = lookupEntry v2 backward
          scopedEnv =
            env
              { reForward = insertPair v1 v2 forward,
                reBackward = insertPair v2 v1 backward
              }
          restore key oldValue pairs = case oldValue of
            Just (_, value) -> insertPair key value pairs
            Nothing -> deleteRef key pairs
      scopedResult <- runScoped scopedEnv
      pure
        scopedResult
          { reForward = restore v1 oldForward (reForward scopedResult),
            reBackward = restore v2 oldBackward (reBackward scopedResult)
          }

    lookupRef ref =
      fmap snd . find (typeBinderRefsSameIdentity ref . fst)

    lookupEntry ref =
      find (typeBinderRefsSameIdentity ref . fst)

    insertPair key value pairs =
      (key, value) : deleteRef key pairs

    deleteRef key =
      filter (not . typeBinderRefsSameIdentity key . fst)

    refsCanRename left right =
      typeBinderRefsSameIdentity left right

selectSolvedOrderWithShadow :: String -> ElabType -> Maybe ElabType -> Either ElabError ElabType
selectSolvedOrderWithShadow context solvedTy mbBaseTy =
  selectSolvedOrderWithShadowWithDetails context defaultShadowDetails solvedTy mbBaseTy

selectSolvedOrderWithShadowWithDetails ::
  String ->
  [String] ->
  ElabType ->
  Maybe ElabType ->
  Either ElabError ElabType
selectSolvedOrderWithShadowWithDetails context detailLines solvedTy mbBaseTy =
  case mbBaseTy of
    Nothing -> Right solvedTy
    Just baseTy -> do
      shadowCompareTypesWithDetails context detailLines solvedTy baseTy
      Right solvedTy

defaultShadowDetails :: [String]
defaultShadowDetails =
  [ "scopeRootC=<unknown>",
    "typeRoot=<unknown>",
    "binders=[]"
  ]

-- | Inline rigid type variables by substituting them with their bounds.
-- Uses cycle detection to prevent infinite loops when bounds reference each other.
inlineRigidTypes :: Map.Map TypeBinderRef ElabType -> ElabType -> ElabType
inlineRigidTypes rigidBounds = go Set.empty Set.empty
  where
    go bound seen ty = case ty of
      TVarRef ref ->
        case Map.lookup ref rigidBounds of
          Just rigidTy
            | Set.notMember ref bound && Set.notMember ref seen ->
                go bound (Set.insert ref seen) rigidTy
          _ -> TVarRef ref
      TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (go bound seen) args)
      TVarAppRef ref args -> TVarAppRef ref (fmap (go bound seen) args)
      TBaseWithIdentity identity b -> TBaseWithIdentity identity b
      TBottom -> TBottom
      TArrow a b -> TArrow (go bound seen a) (go bound seen b)
      TForallRef ref mb body ->
        TForallRef ref (fmap (goBound bound seen) mb) (go (Set.insert ref bound) seen body)
      TMuRef ref body -> TMuRef ref (go (Set.insert ref bound) seen body)
    goBound bound seen = \case
      TArrow a b -> TArrow (go bound seen a) (go bound seen b)
      TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (go bound seen) args)
      TVarAppRef ref args -> TVarAppRef ref (fmap (go bound seen) args)
      TBaseWithIdentity identity b -> TBaseWithIdentity identity b
      TBottom -> TBottom
      TForallRef ref mb body -> TForallRef ref (fmap (goBound bound seen) mb) (go (Set.insert ref bound) seen body)
      TMuRef ref body -> TMuRef ref (go (Set.insert ref bound) seen body)

applyGeneralizePlan ::
  GeneralizePlan p ->
  ReifyPlan ->
  Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)
applyGeneralizePlan plan reifyPlanWrapper = do
  let GeneralizePlan
        { gpEnv = env,
          gpContext = ctx,
          gpSchemeRootsPlan = schemeRootsPlan,
          gpTypeRootPlan = typeRootPlan,
          gpBinderPlan = binderPlan,
          gpScopeHasStructuralScheme = scopeHasStructuralScheme,
          gpBinders0 = binders0,
          gpReachableFromWithBounds = reachableFromWithBounds,
          gpBindParents = bindParents
        } = plan
      GeneralizeEnv { geConstraint = constraint,
          geOriginalConstraint = originalConstraint,
          geNodes = nodes,
          geCanonical = canonical,
          geBindParentsGa = mbBindParentsGa
        } = env
      GeneralizeCtx
        { gcTarget0 = target0,
          gcScopeRootC = scopeRootC,
          gcScopeGen = scopeGen,
          gcFirstGenAncestor = firstGenAncestorGa,
          gcResForReify = resForReify,
          gcBindParentsGaInfo = mbBindParentsGaInfo
        } = ctx
      SchemeRootsPlan
        { srInfo = schemeRootInfo,
          srSchemeRootByBodyBase = schemeRootByBodyBase
        } = schemeRootsPlan
      SchemeRootInfo
        { sriRootKeySet = schemeRootKeySet,
          sriRootOwner = schemeRootOwner,
          sriRootByBody = schemeRootByBody
        } = schemeRootInfo
      TypeRootPlan
        { trTypeRoot = typeRoot
        } = typeRootPlan
      BinderPlan
        { bpBinderNames = binderNames,
          bpOrderedBinderIds = orderedBinders,
          bpGammaAlias = gammaAliasPlan,
          bpNamedUnderGaSet = namedUnderGaSetPlan,
          bpSolvedToBasePref = solvedToBasePrefPlan,
          bpAliasBinderBases = aliasBinderBasesPlan
        } = binderPlan
      ReifyPlan
        { rpPlan = reifyPlan,
          rpTypeRootForReifyAdjusted = typeRootForReifyAdjusted,
          rpSubstForReifyAdjusted = substForReifyAdjusted
        } = reifyPlanWrapper
      Reify.ReifyPlan
        { Reify.rpSubst = substRefs,
          Reify.rpSchemeTypeChoice = schemeTypeChoice,
          Reify.rpBindingScopeGen = bindingScopeGenPlan,
          Reify.rpHasExplicitBound = hasExplicitBoundPlan,
          Reify.rpIsTargetSchemeBinder = isTargetSchemeBinderPlan,
          Reify.rpBoundMentionsSelfAlias = boundMentionsSelfAliasPlan,
          Reify.rpContainsForall = containsForallPlan,
          Reify.rpParseNameId = parseNameIdPlan
        } = reifyPlan
      allowBoundTraversal =
        allowBoundTraversalFor schemeRootsPlan canonical scopeGen target0
      childrenWithBounds nid =
        case IntMap.lookup (getNodeId nid) nodes of
          Just node@TyVar {tnBound = Just bnd}
            | allowBoundTraversal bnd ->
                structuralChildrenWithBounds node
          Just node -> structuralChildren node
          Nothing -> []
  let lookupCanonicalBound nid =
        case VarStore.lookupVarBound constraint (canonical nid) of
          Just bnd
            | Just _ <- NodeAccess.lookupNode constraint (canonical bnd) ->
                Just (canonical bnd)
          _ -> Nothing
      uniqueUnboundedName =
        case [ name
               | (name, nidInt) <- zip binderNames orderedBinders,
                 Nothing <- [lookupCanonicalBound (NodeId nidInt)]
             ] of
          [nm] -> Just nm
          _ -> Nothing
  let binderSet = IntSet.fromList orderedBinders
      bindingEnv =
        Reify.ReifyBindingEnv
          { Reify.rbeConstraint = constraint,
            Reify.rbeNodes = nodes,
            Reify.rbeCanonical = canonical,
            Reify.rbeBindParents = bindParents,
            Reify.rbeScopeGen = scopeGen,
            Reify.rbeSchemeRootOwner = schemeRootOwner,
            Reify.rbeSchemeRootByBody = schemeRootByBody,
            Reify.rbeSchemeRootByBodyBase = schemeRootByBodyBase,
            Reify.rbeSchemeRootKeySet = schemeRootKeySet,
            Reify.rbeGammaAlias = gammaAliasPlan,
            Reify.rbeAliasBinderBases = aliasBinderBasesPlan,
            Reify.rbeSolvedToBasePref = solvedToBasePrefPlan,
            Reify.rbeNamedUnderGaSet = namedUnderGaSetPlan,
            Reify.rbeBinderSet = binderSet,
            Reify.rbeUniqueUnboundedName = uniqueUnboundedName,
            Reify.rbeResForReify = resForReify,
            Reify.rbeBindParentsGa = mbBindParentsGaInfo,
            Reify.rbeBindingScopeGen = bindingScopeGenPlan,
            Reify.rbeHasExplicitBound = hasExplicitBoundPlan,
            Reify.rbeIsTargetSchemeBinder = isTargetSchemeBinderPlan,
            Reify.rbeBoundMentionsSelfAlias = boundMentionsSelfAliasPlan,
            Reify.rbeContainsForall = containsForallPlan,
            Reify.rbeParseNameId = parseNameIdPlan,
            Reify.rbeFirstGenAncestor = firstGenAncestorGa,
            Reify.rbeTraceM = traceGeneralizeM env
          }
  -- Phase 8: construct per-binder bounds.
  bindings <- mapM (Reify.bindingFor bindingEnv reifyPlan) (zip binderNames orderedBinders)
  reachableType <- Right (reachableFromWithBounds typeRoot)

  -- Phase 9: scheme ownership and type reification.
  let typeRootC = canonical typeRoot
      Reify.SchemeTypeChoice
        { Reify.stcUseSchemeType = useSchemeTypeAdjusted,
          Reify.stcSchemeOwnerFromBody = schemeOwnerFromBody,
          Reify.stcSchemeOwnerFromBodyIsAlias = schemeOwnerFromBodyIsAlias,
          Reify.stcSchemeOwners = schemeOwners
        } = schemeTypeChoice
      ownersByRoot =
        [ gnId gen
          | gen <- NodeAccess.allGenNodes constraint,
            root <- gnSchemes gen,
            canonical root == typeRootC
        ]
  -- Thesis §15.2.5: rigid quantification is always inlined (no abstractions for rigid nodes).
  let reifyTypeWithAliases bodyRoot substBase binderPairs =
        if null aliasEntries
          then reifyAndInlineRigid bodyRoot substBaseRigid
          else do
            let aliasNodes =
                  IntMap.fromList
                    [ (key, TyVar {tnId = NodeId key, tnBound = Nothing})
                      | (key, _) <- aliasEntries
                    ]
                constraintAlias =
                  constraint {cNodes = NodeMap (IntMap.union aliasNodes nodes)}
                originalNodes =
                  IntMap.fromList
                    [ (getNodeId nid, node)
                      | (nid, node) <- toListNode (cNodes originalConstraint)
                    ]
                originalConstraintAlias =
                  originalConstraint {cNodes = NodeMap (IntMap.union aliasNodes originalNodes)}
                substAlias =
                  IntMap.union (IntMap.fromList aliasEntries) substBaseRigid
                resAlias =
                  if useConstraintReify
                    then resForReify
                    else presolutionViewFromSnapshot constraintAlias (pvCanonicalMap resForReify)
            ty <- reifyWithOrig originalConstraintAlias bodyRoot substAlias constraintAlias resAlias
            inlineRigidOrig originalConstraintAlias substAlias constraintAlias resAlias ty
        where
          -- Basic setup
          bodyRootC = canonical bodyRoot
          canonicalKey = getNodeId . canonical
          lookupBound nid =
            case VarStore.lookupVarBound constraint (canonical nid) of
              Just bnd
                | Just _ <- NodeAccess.lookupNode constraint (canonical bnd) ->
                    Just (canonical bnd)
              _ -> Nothing

          -- Determine whether to use constraint-based or result-based reification
          useConstraintReify =
            case IntMap.lookup (getNodeId bodyRootC) nodes of
              Just TyVar {}
                | Just bnd <- lookupBound bodyRootC ->
                    getNodeId bnd == getNodeId bodyRoot
              _ -> False

          -- Reification helpers
          -- Note [Identity canonical for scheme reification]:
          -- The OnConstraint path builds an identity PresolutionView over the
          -- original constraint so that solved-away binders are preserved in
          -- scheme types.
          reifyWithOrig origC substRoot substMap _constraintArg resArg
            | useConstraintReify =
                let substOrigRefs = canonicalizeSubstRefs id substMap
                    substResRefs = canonicalizeSubstRefs (pvCanonical resArg) substMap
                 in case reifyTypeWithRefsNoFallbackOnConstraint origC substOrigRefs substRoot of
                      Left (MissingNode _) ->
                        case reifyTypeWithRefsNoFallback resArg substResRefs substRoot of
                          Left (MissingNode _) -> reifyTypeWithRefsNoFallback resArg substResRefs typeRootC
                          other -> other
                      other -> other
            | otherwise =
                let substResRefs = canonicalizeSubstRefs (pvCanonical resArg) substMap
                 in case reifyTypeWithRefsNoFallback resArg substResRefs substRoot of
                      Left (MissingNode _) -> reifyTypeWithRefsNoFallback resArg substResRefs typeRootC
                      other -> other

          reifyBoundWithOrig origC substMap _constraintArg resArg bndRoot
            | useConstraintReify =
                let substOrigRefs = canonicalizeSubstRefs id substMap
                    substResRefs = canonicalizeSubstRefs (pvCanonical resArg) substMap
                 in case reifyBoundWithRefsOnConstraint origC substOrigRefs bndRoot of
                      Left (MissingNode _) -> reifyBoundWithRefs resArg substResRefs bndRoot
                      other -> other
            | otherwise =
                reifyBoundWithRefs resArg (canonicalizeSubstRefs (pvCanonical resArg) substMap) bndRoot

          -- Convenience wrappers using the base original constraint
          reifyWith = reifyWithOrig originalConstraint

          -- Rigid type handling
          isReachableRigidVar nid =
            case IntMap.lookup (canonicalKey nid) nodes of
              Just TyVar {} ->
                let cidKey = canonicalKey nid
                 in IntSet.member cidKey (reachableFromWithBounds bodyRoot)
                      && cidKey `IntSet.notMember` binderSet
              _ -> False

          rigidNodeKeys =
            IntSet.toList $
              IntSet.fromList
                [ canonicalKey nid
                  | nid <- IntMapUtils.rigidTypeChildren bindParents,
                    isReachableRigidVar nid
                ]

          rigidSubstMap =
            IntMap.fromList
              [ (key, typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) (rigidNameFor key))
                | key <- rigidNodeKeys
              ]

          -- Alias handling
          reachableWithoutBound bnd =
            let shouldStop nid = getNodeId nid == getNodeId (canonical bnd)
             in reachableFromStop
                  getNodeId
                  canonical
                  childrenWithBounds
                  shouldStop
                  bodyRoot

          aliasEntries =
            [ (getNodeId (canonical bnd), ref)
              | (b, ref) <- binderPairs,
                Just bnd <- [lookupBound b],
                canonical bnd /= bodyRootC,
                canonicalKey b `IntSet.notMember` reachableWithoutBound bnd
            ]

          substBaseRigid = IntMap.union substBase rigidSubstMap

          -- Main reification logic
          reifyAndInlineRigid root substMap = do
            ty <- reifyWith root substMap constraint resForReify
            inlineRigid substMap constraint resForReify ty

          inlineRigid = inlineRigidOrig originalConstraint

          inlineRigidOrig origC substMap constraintArg resArg ty
            | null rigidNodeKeys = pure ty
            | otherwise = do
                let computeRigidBound key = do
                      let nid = NodeId key
                          name = rigidNameFor key
                          rigidRef =
                            case IntMap.lookup key substMap of
                              Just substRef -> substRef
                              Nothing ->
                                typeBinderRefFromIdentity (typeBinderIdentityFromNode nid) name
                          fallbackTy =
                            TVarRef rigidRef
                      case lookupBound nid of
                        Nothing -> pure (rigidRef, fallbackTy)
                        Just bnd -> do
                          case reifyBoundWithOrig origC substMap constraintArg resArg (canonical bnd) of
                            Left (MissingNode _) -> pure (rigidRef, fallbackTy)
                            Left err -> Left err
                            Right bndTy -> pure (rigidRef, bndTy)
                rigidBounds <- mapM computeRigidBound rigidNodeKeys
                let rigidMap = Map.fromList rigidBounds
                pure (inlineRigidTypes rigidMap ty)

  let adjustedTypeRootForReify = typeRootForReifyAdjusted
      adjustedSubstForReify = substForReifyAdjusted
      solvedTypeRootForReify = typeRoot
      solvedSubstForReify = substRefs
      orderedBinderPairs =
        [ (NodeId key, ref)
          | key <- orderedBinders,
            Just ref <- [IntMap.lookup key substRefs]
        ]
      reifyTypeWithOrderedBinders =
        reifyTypeWithAliases
          adjustedTypeRootForReify
          adjustedSubstForReify
          orderedBinderPairs

  let reifySchemeType
        | useSchemeTypeAdjusted = reifySchemeTypeAdjusted
        | otherwise = reifySchemeTypeExplicit
        where
          -- Adjusted scheme type: use scheme ownership to determine scope
          reifySchemeTypeAdjusted = do
            let reachableVars =
                  [ NodeId nid
                    | nid <- IntSet.toList reachableType,
                      nid /= getNodeId typeRootC,
                      Just TyVar {} <- [IntMap.lookup nid nodes]
                  ]
                hasReachableBinder gid =
                  any (\nid -> firstGenAncestorGa (typeRef nid) == Just gid) reachableVars
                schemeOwnerCandidates = filter hasReachableBinder schemeOwners
                schemeScope = case schemeOwnerFromBody of
                  Just _ | schemeOwnerFromBodyIsAlias, (owner : _) <- ownersByRoot -> genRef owner
                  Just gid -> genRef gid
                  Nothing -> case schemeOwnerCandidates ++ schemeOwners of
                    (gid : _) -> genRef gid
                    [] -> typeRef typeRootC
            if schemeScope == scopeRootC
              then do
                traceGeneralizeM
                  env
                  ( "generalizeAt: schemeScope equals scopeRootC; skipping recursive scheme-type fallback"
                      ++ " scopeRootC="
                      ++ show scopeRootC
                      ++ " typeRootC="
                      ++ show typeRootC
                  )
                reifyTypeWithOrderedBinders
              else do
                traceGeneralizeM
                  env
                  ( "generalizeAt: schemeScope differs from scopeRootC; using direct structural scheme reification"
                      ++ " scopeRootC="
                      ++ show scopeRootC
                      ++ " schemeScope="
                      ++ show schemeScope
                      ++ " typeRootC="
                      ++ show typeRootC
                  )
                reifySchemeTypeExplicit

          -- Explicit scheme type: use structural scheme if available
          reifySchemeTypeExplicit = do
            explicitSchemeTy <- explicitStructuralSchemeType
            case explicitSchemeTy of
              Just ty -> pure ty
              Nothing
                | scopeHasStructuralScheme && null bindings ->
                    reifyTypeWithRefsNoFallbackOnConstraint
                      originalConstraint
                      (canonicalizeSubstRefs id solvedSubstForReify)
                      solvedTypeRootForReify
                | otherwise ->
                    reifyTypeWithOrderedBinders

          explicitStructuralSchemeType
            | null bindings,
              scopeHasStructuralScheme,
              explicitBinders0@(_ : _) <- binders0 =
                case explicitSchemePlan explicitBinders0 of
                  Nothing -> pure Nothing
                  Just (binders, names, substExplicit, explicitBodyRoot) -> do
                    let binderRefs =
                          [ ref
                            | binder <- binders,
                              Just ref <- [IntMap.lookup (getNodeId binder) substExplicit]
                          ]
                    bodyTy <- reifyTypeWithAliases explicitBodyRoot substExplicit (zip binders binderRefs)
                    bounds <- explicitBounds binders names substExplicit
                    pure (Just (buildForallTypeRefs bounds bodyTy))
            | otherwise = pure Nothing

          explicitSchemePlan explicitBinders0 =
            let binderKeysList =
                  IntSet.toList $
                    IntSet.fromList
                      [getNodeId (canonical b) | b <- explicitBinders0]
                names = zipWith alphaName [0 ..] binderKeysList
                refs =
                  [ typeBinderRefFromIdentity (typeBinderIdentityFromNode (NodeId key)) name
                    | (key, name) <- zip binderKeysList names
                  ]
             in case binderKeysList of
                  [] -> Nothing
                  _ ->
                    Just
                      ( map NodeId binderKeysList,
                        names,
                        IntMap.fromList (zip binderKeysList refs),
                        case IntMap.lookup (getNodeId typeRootC) nodes of
                          Just TyVar {}
                            | Just bnd <- lookupCanonicalBound typeRootC ->
                                canonical bnd
                          _ -> adjustedTypeRootForReify
                      )

          explicitBounds binders names substExplicit =
            let lookupBound nid =
                  case VarStore.lookupVarBound constraint (canonical nid) of
                    Just bnd
                      | Just _ <- NodeAccess.lookupNode constraint (canonical bnd) ->
                          Just (canonical bnd)
                    _ -> Nothing
                useConstraintBoundReify =
                  scopeHasStructuralScheme && null bindings
                reifyBoundForExplicit bndRoot
                  | useConstraintBoundReify =
                      reifyBoundWithRefsOnConstraint originalConstraint (canonicalizeSubstRefs id substExplicit) bndRoot
                  | otherwise =
                      reifyBoundWithRefs resForReify (canonicalizeSubstRefs (pvCanonical resForReify) substExplicit) bndRoot
                inlineNamedBounds = inlineNamedBoundsFor substExplicit
                computeBound (b, name) =
                  let ref = typeBinderRefFromIdentity (typeBinderIdentityFromNode b) name
                   in case lookupBound b of
                        Nothing -> pure (ref, Nothing)
                        Just bnd -> do
                          bndTy <- reifyBoundForExplicit (canonical bnd)
                          let bndTy' = inlineNamedBounds bndTy
                              mbBound = case bndTy' of
                                TBottom -> Nothing
                                TVarRef bndRef
                                  | typeBinderRefsSameIdentity bndRef ref -> Nothing
                                TVarRef {} -> Nothing
                                _ -> either (const Nothing) Just (elabToBound bndTy')
                          pure (ref, mbBound)
             in mapM computeBound (zip binders names)

          inlineNamedBoundsFor substExplicit =
            -- See Note [Scope-aware bound/alias inlining] in
            -- docs/notes/2026-01-27-elab-changes.md.
            let useConstraintBoundReify =
                  scopeHasStructuralScheme && null bindings
                reifyBoundForInline bndRoot
                  | useConstraintBoundReify =
                      reifyBoundWithRefsOnConstraint originalConstraint (canonicalizeSubstRefs id substExplicit) bndRoot
                  | otherwise =
                      reifyBoundWithRefs resForReify (canonicalizeSubstRefs (pvCanonical resForReify) substExplicit) bndRoot
             in inlineAliasBoundsWithBy
                  False
                  canonical
                  (NodeMap nodes)
                  (VarStore.lookupVarBound constraint)
                  reifyBoundForInline

  ty0Raw <- reifySchemeType
  finalizeScheme
    FinalizeInput
      { fiEnv = env,
        fiConstraint = constraint,
        fiCanonical = canonical,
        fiBindParents = bindParents,
        fiScopeRootC = scopeRootC,
        fiTypeRoot = typeRoot,
        fiTypeRootC = typeRootC,
        fiScopeGen = scopeGen,
        fiFirstGenAncestorGa = firstGenAncestorGa,
        fiBindParentsGa = mbBindParentsGa,
        fiSolvedToBasePref = solvedToBasePrefPlan,
        fiGammaAlias = gammaAliasPlan,
        fiNamedUnderGaSet = namedUnderGaSetPlan,
        fiOrderedBinders = orderedBinders,
        fiBinderNames = binderNames,
        fiBindings = bindings,
        fiSubst = substRefs,
        fiTyRaw = ty0Raw
      }
