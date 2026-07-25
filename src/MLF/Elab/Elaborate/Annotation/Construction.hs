module MLF.Elab.Elaborate.Annotation.Construction
  ( checkedArgumentClosedTopology,
    checkedOccurrenceSchemeInfo,
    scopedAnnotationConstructionBinderRenames,
    strictReplayCheckedSchemeInfo,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find)
import MLF.Constraint.Presolution.Base (EdgeTrace (..))
import MLF.Constraint.Types.Graph (NodeId (..), getNodeId)
import MLF.Constraint.Types.Witness (isStrictReplayContract)
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.SourceBinder
  ( sourceBinderConstructionRenames,
    typeBinderDeclarationRefs,
  )
import MLF.Elab.Types
  ( ElabType,
    SchemeInfo (..),
    Ty (..),
    TypeBinderRef,
    schemeBinderRefs,
    schemeBody,
    schemeFromType,
    typeBinderRefsSameIdentity,
  )
import MLF.Reify.TypeOps
  ( alphaEqType,
    freeTypeVarRefsType,
    splitForallsRefs,
  )

-- | Close an application argument topology with the exact scheme constructed
-- by that checked occurrence.  Constraint replay can expose the body of a
-- source forall as the graph destination even though the argument value still
-- carries the complete scheme.  The source scheme may restore that prefix
-- only when the checked argument agrees with it, the projected topology is
-- exactly its opened body, and its free references are exactly the
-- declarations in that prefix.  Requiring every declaration to occur keeps a
-- vacuous forall on the ordinary elimination path: its body carries no
-- identity evidence that the graph topology represents an opened scheme.
--
-- The caller must project graph references into the source identity domain
-- before invoking this function.  This keeps graph/source routing and lexical
-- closure as two explicit construction proofs.
checkedArgumentClosedTopology ::
  Maybe SchemeInfo ->
  ElabType ->
  ElabType ->
  Maybe ElabType
checkedArgumentClosedTopology mbSourceScheme checkedSourceTy projectedTopology = do
  sourceSchemeInfo <- mbSourceScheme
  let closedTy = schemeToType (siScheme sourceSchemeInfo)
      (declarations, openedBody) = splitForallsRefs closedTy
      declarationRefs = map fst declarations
      topologyFreeRefs = freeTypeVarRefsType projectedTopology
  if not (null declarations)
      && alphaEqType closedTy checkedSourceTy
      && alphaEqType openedBody projectedTopology
      && all
        ( \declarationRef ->
            any
              (typeBinderRefsSameIdentity declarationRef)
              topologyFreeRefs
        )
        declarationRefs
      && all
        ( \freeRef ->
            any
              (typeBinderRefsSameIdentity freeRef)
              declarationRefs
        )
        topologyFreeRefs
    then Just closedTy
    else Nothing

-- | Retain the exact type tree that was successfully checked at an occurrence,
-- importing graph routes only when their semantic binder identity occurs in
-- that tree.
--
-- The recovered syntactic scheme is optional because deferred method
-- occurrences need not be present in the elaboration environment.  It may
-- contribute routing metadata, but never binder declarations, ordering, or
-- display-name authority.
checkedOccurrenceSchemeInfo ::
  ElabType ->
  Maybe SchemeInfo ->
  Either String SchemeInfo
checkedOccurrenceSchemeInfo checkedType recoveredSchemeInfo = do
  compatibleRoutes <-
    case recoveredSchemeInfo of
      Nothing -> Right IntMap.empty
      Just recovered ->
        IntMap.traverseMaybeWithKey
          retainCompatibleRoute
          (siSubstRefs recovered)
  pure
    SchemeInfo
      { siScheme = schemeFromType checkedType,
        siSubstRefs = compatibleRoutes
      }
 where
  declarationRefs = typeBinderDeclarationRefs checkedType
  checkedRefs =
    foldr
      insertDistinctRef
      (freeTypeVarRefsType checkedType)
      declarationRefs

  retainCompatibleRoute _nodeKey recoveredRef =
    case
        [ checkedRef
        | checkedRef <- checkedRefs
        , typeBinderRefsSameIdentity checkedRef recoveredRef
        ]
      of
        [] -> Right Nothing
        [checkedRef] -> Right (Just checkedRef)
        matches ->
          Left
            ( "checked occurrence contains multiple route-compatible binder identities: recovered="
                ++ show recoveredRef
                ++ ", matches="
                ++ show matches
            )

  insertDistinctRef ref refs
    | any (typeBinderRefsSameIdentity ref) refs = refs
    | otherwise = ref : refs

-- | Select source-to-Gamma routes owned by one annotation construction.
--
-- A direct graph-node intersection is the authority: representative peers do
-- not make an unrelated sibling route visible here.  The source identity must
-- also occur free in this annotation's expected type, which prevents a
-- same-spelled but distinct source binder from being captured.
scopedAnnotationConstructionBinderRenames ::
  (NodeId -> NodeId) ->
  IntMap.IntMap TypeBinderRef ->
  IntMap.IntMap TypeBinderRef ->
  ElabType ->
  Either String [(TypeBinderRef, TypeBinderRef)]
scopedAnnotationConstructionBinderRenames representative sourceBinderRefs constructionIdentityRoutes expectedType =
  sourceBinderConstructionRenames
    representative
    scopedSourceBinderRefs
    constructionIdentityRoutes
  where
    expectedFreeBinderRefs = freeTypeVarRefsType expectedType
    scopedSourceBinderRefs =
      IntMap.filterWithKey
        ( \nodeKey sourceRef ->
            IntMap.member nodeKey constructionIdentityRoutes
              && any
                (typeBinderRefsSameIdentity sourceRef)
                expectedFreeBinderRefs
        )
        sourceBinderRefs

-- | Put a checked occurrence scheme into a strict edge's replay binder
-- domain without changing the checked binder identities or its type tree.
--
-- Constraint generation records the exact semantic source identity for each
-- graph binder in @sourceBinderRefs@.  Joining that sidecar with the frozen
-- trace source key and the producer-owned replay map gives the only
-- construction-authoritative route:
--
-- @
-- checked binder identity <- source key -> replay binder
-- @
--
-- A finalized producer scheme is not a substitute for this bridge.  Its
-- leading spine may already have consumed a grafted source quantifier, while
-- the checked occurrence still starts at that quantifier.  Stale source/copy
-- routes for a covered identity are therefore removed and the identity is
-- keyed only at its producer-approved replay targets.  Several frozen source
-- nodes may carry one exact semantic binder identity; each such occurrence has
-- its own trace route, but they remain one binder in the checked scheme.
strictReplayCheckedSchemeInfo ::
  IntMap.IntMap TypeBinderRef ->
  EdgeTrace ->
  SchemeInfo ->
  Either String SchemeInfo
strictReplayCheckedSchemeInfo sourceBinderRefs traceInfo schemeInfo
  | not (isStrictReplayContract (etReplayContract traceInfo)) =
      Right schemeInfo
  | otherwise = do
      ensureDistinctCheckedBinders checkedBinderRefs
      routeGroups <- traverse replayRoutesFor checkedBinderRefs
      let coveredRoutes = concat routeGroups
          coveredRefs =
            [ checkedRef
            | (checkedRef, _sourceKey, _replayTarget) <- coveredRoutes
            ]
          replayTargets =
            [ replayTarget
            | (_checkedRef, _sourceKey, replayTarget) <- coveredRoutes
            ]
          replayTargetOwners =
            IntMap.fromListWith
              (++)
              [ (getNodeId replayTarget, [checkedRef])
              | (checkedRef, _sourceKey, replayTarget) <- coveredRoutes
              ]
          conflictingReplayTargets =
            [ (NodeId targetKey, ownerRefs)
            | (targetKey, ownerRefs) <- IntMap.toList replayTargetOwners
            , firstOwner : remainingOwners <- [ownerRefs]
            , any
                (not . typeBinderRefsSameIdentity firstOwner)
                remainingOwners
            ]
          explicitReplayDomain =
            IntSet.fromList (map getNodeId (etReplayDomainBinders traceInfo))
      case conflictingReplayTargets of
        [] -> pure ()
        conflicts ->
          Left
            ( "strict replay routes multiple checked binders to one replay target: "
                ++ show conflicts
            )
      if null coveredRoutes
        then Right schemeInfo
        else do
          if
              not (IntSet.null explicitReplayDomain)
                && all
                  ((`IntSet.member` explicitReplayDomain) . getNodeId)
                  replayTargets
            then pure ()
            else
              Left
                ( "strict replay checked-scheme target is outside the explicit replay domain: targets="
                    ++ show replayTargets
                    ++ ", domain="
                    ++ show (etReplayDomainBinders traceInfo)
                )
          let retainedSubst =
                IntMap.filter
                  ( \existingRef ->
                      not
                        ( any
                            (typeBinderRefsSameIdentity existingRef)
                            coveredRefs
                        )
                  )
                  (siSubstRefs schemeInfo)
          mapM_ (rejectUntouchedTargetConflict retainedSubst) coveredRoutes
          let replaySubst =
                IntMap.fromList
                  [ (getNodeId replayTarget, checkedRef)
                  | (checkedRef, _sourceKey, replayTarget) <- coveredRoutes
                  ]
          pure
            schemeInfo
              { siSubstRefs = IntMap.union replaySubst retainedSubst
              }
 where
  checkedBinderRefs =
    map fst (schemeBinderRefs (siScheme schemeInfo))
      ++ leadingBodyBinderRefs (schemeBody (siScheme schemeInfo))

  leadingBodyBinderRefs ty =
    case ty of
      TForallRef ref _bound body -> ref : leadingBodyBinderRefs body
      _ -> []

  traceSourceKeys =
    foldl insertUniqueSource [] (map (getNodeId . fst) (etBinderArgs traceInfo))

  insertUniqueSource keys key
    | key `elem` keys = keys
    | otherwise = keys ++ [key]

  replayRoutesFor checkedRef =
    traverse
      replayRouteForSource
      [ sourceKey
      | sourceKey <- traceSourceKeys
      , Just sourceRef <- [IntMap.lookup sourceKey sourceBinderRefs]
      , typeBinderRefsSameIdentity checkedRef sourceRef
      ]
    where
      replayRouteForSource sourceKey = do
          replayTarget <-
            case IntMap.lookup sourceKey (etBinderReplayMap traceInfo) of
              Just target -> Right target
              Nothing ->
                Left
                  ( "strict replay checked binder has no replay-map target: source="
                      ++ show (NodeId sourceKey)
                      ++ ", binder="
                      ++ show checkedRef
                  )
          Right (checkedRef, sourceKey, replayTarget)

  ensureDistinctCheckedBinders refs =
    case
        find
          ( \ref ->
              length (filter (typeBinderRefsSameIdentity ref) refs) > 1
          )
          refs
      of
        Nothing -> Right ()
        Just duplicate ->
          Left
            ( "strict replay checked scheme repeats a binder identity: "
                ++ show duplicate
            )

  rejectUntouchedTargetConflict retainedSubst (checkedRef, _sourceKey, replayTarget) =
    case IntMap.lookup (getNodeId replayTarget) retainedSubst of
      Nothing -> Right ()
      Just existingRef
        | typeBinderRefsSameIdentity existingRef checkedRef -> Right ()
        | otherwise ->
            Left
              ( "strict replay target already belongs to an untouched checked binder: target="
                  ++ show replayTarget
                  ++ ", covered="
                  ++ show checkedRef
                  ++ ", existing="
                  ++ show existingRef
              )
