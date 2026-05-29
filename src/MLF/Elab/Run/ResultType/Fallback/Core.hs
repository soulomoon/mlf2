{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module MLF.Elab.Run.ResultType.Fallback.Core
  ( computeResultTypeFallbackCore,
    computeResultTypeFallbackCoreWithRoots,
  )
where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.List (nub)
import MLF.Constraint.Presolution (EdgeTrace (..), PresolutionView (..))
import MLF.Constraint.Presolution.Base (lookupCopy)
import MLF.Constraint.Types.Graph
  ( EdgeId (..),
    NodeId (..),
    NodeRef (..),
    TyNode (..),
    cBindParents,
    cNodes,
    getEdgeId,
    getNodeId,
    lookupNodeIn,
    nodeRefFromKey,
    toListNode,
  )
import MLF.Constraint.Types.Witness (ewRight)
import MLF.Elab.Phi (phiFromEdgeWitnessWithTrace)
import MLF.Elab.Run.Annotation (annNode)
import MLF.Elab.Run.Debug (debugGaScope, debugGaScopeEnabled, debugWhenCondM, debugWhenM)
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Run.ResultType.Types
  ( ResultTypeInputs (..),
    rtcEdgeExpansions,
    rtcEdgeTraces,
    rtcEdgeWitnesses,
  )
import MLF.Elab.Run.ResultType.Util
  ( CandidateSelection (..),
    candidateSelectionIsAmbiguous,
    candidateSelectionValue,
    collectEdges,
    resultTypeRoots,
    selectUniqueCandidate,
    stripAnn,
  )
import qualified MLF.Elab.Run.ResultType.View as View
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen (AnnExpr (..))

data RootLocality
  = LocalTypeRoot
  | NonLocalTypeRoot
  deriving (Eq, Show)

data BaseTargetAdmissionReason
  = AdmitByUniqueRootCandidate
  | AdmitByUniqueInstArgCandidate
  | AdmitBySchemeAliasBaseLike
  deriving (Eq, Show)

data BaseTargetAdmission = BaseTargetAdmission
  { baseTargetAdmissionLocality :: RootLocality,
    baseTargetAdmissionReason :: BaseTargetAdmissionReason,
    baseTargetAdmissionNode :: NodeId
  }
  deriving (Eq, Show)

type UniqueCandidate = CandidateSelection

pattern NoCandidate :: UniqueCandidate a
pattern NoCandidate = NoCandidateSelection

pattern UniqueCandidate :: a -> UniqueCandidate a
pattern UniqueCandidate candidate = UniqueCandidateSelection candidate

pattern AmbiguousCandidate :: UniqueCandidate a
pattern AmbiguousCandidate = AmbiguousCandidateSelection

{-# COMPLETE NoCandidate, UniqueCandidate, AmbiguousCandidate #-}

data RetainedChildProof = RetainedChildProof
  { retainedChildProofChild :: NodeId,
    retainedChildProofChildTarget :: NodeId,
    retainedChildProofChosenTarget :: NodeId,
    retainedChildProofChosenScopeRoot :: NodeRef
  }
  deriving (Eq, Show)

data RecursiveCandidateProof
  = RecursiveCandidateBaseTarget BaseTargetAdmission
  | RecursiveCandidateRetainedChild RetainedChildProof
  deriving (Eq, Show)

uniqueCandidate :: (Eq a) => [a] -> UniqueCandidate a
uniqueCandidate = selectUniqueCandidate

-- | Core implementation of computeResultTypeFallback (non-annotated-lambda case).
computeResultTypeFallbackCore ::
  ResultTypeInputs p ->
  View.ResultTypeView p ->
  -- | annCanon (post-redirect)
  AnnExpr ->
  -- | ann (pre-redirect)
  AnnExpr ->
  Either ElabError ElabType
computeResultTypeFallbackCore ctx viewBase annCanon ann = do
  let canonical = rtcCanonical ctx
      presolutionView = View.rtvPresolutionViewOverlay viewBase
      c1 = rtcBaseConstraint ctx
      roots =
        resultTypeRoots
          canonical
          (pvConstraint presolutionView)
          c1
          annCanon
          ann
  computeResultTypeFallbackCoreWithRoots ctx viewBase roots annCanon ann

computeResultTypeFallbackCoreWithRoots ::
  ResultTypeInputs p ->
  View.ResultTypeView p ->
  -- | Roots computed by the outer result-type dispatcher.
  (AnnExpr, AnnExpr) ->
  -- | annCanon (post-redirect)
  AnnExpr ->
  -- | ann (pre-redirect)
  AnnExpr ->
  Either ElabError ElabType
computeResultTypeFallbackCoreWithRoots ctx viewBase (rootForTypeAnn, rootForTypePreAnn) _annCanon _ann = do
  let canonical = rtcCanonical ctx
      edgeWitnesses = rtcEdgeWitnesses ctx
      edgeTraces = rtcEdgeTraces ctx
      edgeExpansions = rtcEdgeExpansions ctx
      presolutionView = View.rtvPresolutionViewOverlay viewBase
      bindParentsGa = rtcBindParentsGa ctx
      planBuilder = rtcPlanBuilder ctx
      traceCfg = rtcTraceConfig ctx
      generalizeAtWith mbGa =
        generalizeAtWithBuilder planBuilder mbGa presolutionView
      fallbackIndex = View.rtvFallbackIndex viewBase
      edgeTraceCounts = View.rtfiEdgeTraceCounts fallbackIndex
      traceBinderArgBaseBounds = View.rtfiTraceBinderArgBaseBounds fallbackIndex
      instArgBaseBounds = View.rtfiInstArgBaseBounds fallbackIndex
      rootBounds = View.rtfiRootBounds fallbackIndex
      instArgRootMultiBase = View.rtfiInstArgRootMultiBase fallbackIndex
      baseNodeByTy = View.rtfiBaseNodeByTy fallbackIndex
      schemeRootSetBase = View.rtfiSchemeRootSet fallbackIndex
      schemeRootOwnerBase = View.rtfiSchemeRootOwner fallbackIndex
  let rootForType = annNode rootForTypeAnn
      rootForTypePre = annNode rootForTypePreAnn
  -- Note: The AAnn case is handled by the facade in ResultType.hs
  -- which dispatches to computeResultTypeFromAnn. This module only
  -- handles the non-AAnn case.
  case rootForTypeAnn of
    AAnn _ _ _ ->
      -- This case should be handled by the facade, not called directly.
      -- If we reach here, it means the facade didn't properly dispatch.
      Left (ValidationFailed ["computeResultTypeFallback called with AAnn - use facade instead"])
    AUnfold _ _ _ ->
      Left (ValidationFailed ["computeResultTypeFallback called with AUnfold - use facade instead"])
    _ -> do
      let rootC = canonical rootForType
          nodes = cNodes (pvConstraint presolutionView)
          resolveBaseBoundCanonical start =
            let go visited nid0 =
                  let nid = canonical nid0
                      key = getNodeId nid
                   in if IntSet.member key visited
                        then Nothing
                        else case View.rtvLookupNode viewBase nid of
                          Just TyBase {} -> Just nid
                          Just TyBottom {} -> Just nid
                          Just TyVar {} ->
                            case View.rtvLookupVarBound viewBase nid of
                              Just bnd -> go (IntSet.insert key visited) bnd
                              Nothing -> Nothing
                          _ -> Nothing
             in go IntSet.empty start
          rootInstRoots =
            [ etRoot tr
              | EdgeId eid <- collectEdges rootForTypeAnn,
                Just tr <- [IntMap.lookup eid edgeTraces]
            ]
          rootHasMultiInst =
            any
              (\root -> IntMap.findWithDefault 0 (getNodeId root) edgeTraceCounts > 1)
              rootInstRoots
          collectInstApps :: Instantiation -> [ElabType]
          collectInstApps = cata alg
            where
              alg inst0 = case inst0 of
                InstAppF ty -> [ty]
                InstSeqF a b -> a ++ b
                InstInsideF phi -> phi
                InstUnderF _ phi -> phi
                _ -> []
          baseNodeForTy ty =
            case ty of
              TBase base ->
                Map.lookup base baseNodeByTy
              _ -> Nothing
          instAppBasesFromWitness funEid =
            case IntMap.lookup (getEdgeId funEid) edgeWitnesses of
              Just ew ->
                case phiFromEdgeWitnessWithTrace traceCfg generalizeAtWith presolutionView (Just bindParentsGa) Nothing (IntMap.lookup (getEdgeId funEid) edgeTraces) ew of
                  Right inst ->
                    IntSet.fromList
                      [ getNodeId nid
                        | ty <- collectInstApps inst,
                          Just nid <- [baseNodeForTy ty]
                      ]
                  Left _ -> IntSet.empty
              Nothing -> IntSet.empty
          rootArgBaseBounds =
            let argEdgeBases eid =
                  IntMap.findWithDefault IntSet.empty (getEdgeId eid) traceBinderArgBaseBounds
             in case stripAnn rootForTypeAnn of
                  AApp _ arg funEid argEid _ ->
                    let fromWitness = instAppBasesFromWitness funEid
                        fromArgEdge = argEdgeBases argEid
                     in if not (IntSet.null fromWitness)
                          then fromWitness
                          else
                            if not (IntSet.null fromArgEdge)
                              then fromArgEdge
                            else case resolveBaseBoundCanonical (annNode arg) of
                              Just baseC -> IntSet.singleton (getNodeId baseC)
                              Nothing -> IntSet.empty
                  _ -> IntSet.empty

      let rootBaseBounds =
            IntMap.findWithDefault IntSet.empty (getNodeId rootC) rootBounds
          rootBoundCandidates =
            if IntSet.null rootBaseBounds
              then rootArgBaseBounds
              else rootBaseBounds
      let baseTargetCandidate =
            case lookupNodeIn nodes rootC of
              Just TyVar {} ->
                case resolveBaseBoundCanonical rootC of
                  Just baseC
                    | IntSet.size rootBoundCandidates == 1
                        && IntSet.member (getNodeId baseC) rootBoundCandidates
                        && not rootHasMultiInst
                        && not instArgRootMultiBase ->
                        Just baseC
                    | IntSet.null rootBoundCandidates
                        && IntSet.null instArgBaseBounds
                        && not rootHasMultiInst
                        && not instArgRootMultiBase ->
                        Just baseC
                    | IntSet.null rootBaseBounds
                        && not instArgRootMultiBase
                        && not rootHasMultiInst
                        && IntSet.size instArgBaseBounds == 1
                        && IntSet.member (getNodeId baseC) instArgBaseBounds ->
                        Just baseC
                    | otherwise ->
                        case IntSet.toList rootBoundCandidates of
                          [baseKey]
                            | not rootHasMultiInst
                                && not instArgRootMultiBase ->
                                case lookupNodeIn nodes (NodeId baseKey) of
                                  Just TyBase {} -> Just (NodeId baseKey)
                                  Just TyBottom {} -> Just (NodeId baseKey)
                                  _ -> Nothing
                          _ -> Nothing
                  _ ->
                    case IntSet.toList instArgBaseBounds of
                      [baseKey]
                        | IntSet.null rootBaseBounds
                            && not rootHasMultiInst
                            && not instArgRootMultiBase ->
                            case lookupNodeIn nodes (NodeId baseKey) of
                              Just TyBase {} -> Just (NodeId baseKey)
                              Just TyBottom {} -> Just (NodeId baseKey)
                              _ -> Nothing
                      _ ->
                        case IntSet.toList rootBoundCandidates of
                          [baseKey]
                            | not rootHasMultiInst
                                && not instArgRootMultiBase ->
                                case lookupNodeIn nodes (NodeId baseKey) of
                                  Just TyBase {} -> Just (NodeId baseKey)
                                  Just TyBottom {} -> Just (NodeId baseKey)
                                  _ -> Nothing
                          _ -> Nothing
              _ -> Nothing
      let boundTarget =
            case (baseTargetCandidate, lookupNodeIn nodes rootC) of
              (Nothing, Just TyVar {tnBound = Nothing}) ->
                case IntSet.toList rootBoundCandidates of
                  [baseKey] -> Just (NodeId baseKey)
                  _ -> Nothing
              _ -> Nothing
      debugWhenM
        traceCfg
        ( let showBase tr =
                let bases =
                      [ resolveBaseBoundCanonical argNode
                        | (binder, arg) <- etBinderArgs tr,
                          let argNode =
                                case lookupCopy binder (etCopyMap tr) of
                                  Just copyN -> copyN
                                  Nothing -> arg
                      ]
                 in (etRoot tr, bases)
              perEdge = map showBase (IntMap.elems edgeTraces)
              edgeRightBases =
                [ (EdgeId eid, ewRight ew, resolveBaseBoundCanonical (ewRight ew))
                  | (eid, ew) <- IntMap.toList edgeWitnesses
                ]
              edgeExpansionsList =
                [ (EdgeId eid, expn)
                  | (eid, expn) <- IntMap.toList edgeExpansions
                ]
           in "runPipelineElab: instArgBaseBounds="
                ++ show instArgBaseBounds
                ++ " instArgRootMultiBase="
                ++ show instArgRootMultiBase
                ++ " rootArgBaseBounds="
                ++ show rootArgBaseBounds
                ++ " rootBoundCandidates="
                ++ show rootBoundCandidates
                ++ " rootHasMultiInst="
                ++ show rootHasMultiInst
                ++ " rootForTypeAnn="
                ++ case stripAnn rootForTypeAnn of
                  AApp _ _ funEid argEid nid ->
                    "AApp funEid="
                      ++ show funEid
                      ++ " argEid="
                      ++ show argEid
                      ++ " appNode="
                      ++ show nid
                  other -> show (annNode other)
                ++ " baseNames="
                ++ show
                  [ nodeBase
                    | TyBase {tnBase = nodeBase} <- map snd (toListNode nodes)
                  ]
                ++ " perEdgeBases="
                ++ show perEdge
                ++ " edgeRightBases="
                ++ show edgeRightBases
                ++ " edgeExpansions="
                ++ show edgeExpansionsList
        )
      {- Note [Bound overlay for fallback target refinement]
         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         For the boundTarget case we model the root's inferred bound as an
         overlay at the result-type view boundary, then query through the
         overlay-aware view when needed.  This keeps fallback logic
         read-oriented and avoids local rebuildWithNodes patching. -}
      let viewFinalBounded =
            case boundTarget of
              Nothing -> viewBase
              Just baseN -> View.rtvWithBoundOverlay rootC baseN viewBase
          presolutionViewFinal = View.rtvPresolutionViewOverlay viewFinalBounded
      let scopeRootNodePre = rootForTypePre
      scopeRootPre <- View.rtvResolveCanonicalScope viewFinalBounded scopeRootNodePre
      let rootBindingIsLocalType =
            case View.rtvBindingScopeRefCanonical viewFinalBounded rootC of
              Right TypeRef {} -> True
              _ -> False
      let scopeRootPost =
            case View.rtvBindingScopeRefCanonical viewFinalBounded rootC of
              Right ref -> View.rtvScopeRefCanonical viewFinalBounded ref
              Left _ -> scopeRootPre
          scopeRoot = scopeRootPre
      debugWhenCondM
        traceCfg
        (scopeRootPre /= scopeRootPost)
        ( "runPipelineElab: ga' mismatch pre="
            ++ show scopeRootPre
            ++ " post="
            ++ show scopeRootPost
            ++ " preNode="
            ++ show rootForTypePre
            ++ " postNode="
            ++ show rootC
        )
      let canonicalFinal = rtcCanonical ctx
          rootFinal = canonicalFinal rootC
          nodesFinal = cNodes (pvConstraint presolutionViewFinal)
          targetView =
            if rootBindingIsLocalType
              then viewFinalBounded
              else viewBase
          retainedChildView =
            if rootBindingIsLocalType
              then viewBase
              else targetView
          retainedChildPresolutionView = View.rtvPresolutionViewOverlay retainedChildView
          rootBound =
            case lookupNodeIn nodesFinal rootFinal of
              Just TyVar {tnBound = Just bnd} -> Just (canonicalFinal bnd)
              _ -> Nothing
          rootBoundIsBase =
            case rootBound of
              Just bnd ->
                case lookupNodeIn nodesFinal bnd of
                  Just TyBase {} -> True
                  Just TyBottom {} -> True
                  _ -> False
              Nothing -> False
          {- Note [Recursive type opening for non-local fallback]
             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
             The result-type fallback is fail-closed for non-local types:
             it returns the quantified rootFinal variable, producing a ∀a. …
             shell that loses any μ-structure in the type's bound chain.

             For well-formed recursive types (μ-types introduced by explicit
             annotations), this quantified shell is too lossy — the surface
             type should preserve the μ.  We detect this by walking the
             rootFinal bound chain through TyVar bounds, TyForall/TyExp
             bodies, looking for a TyMu node.  When found, we route through
             schemeBodyTarget to unwrap the scheme root and expose the
             actual recursive type for generalization.

             This opening is controlled:
             - The bound chain and arrow children are walked
             - Only affects the non-local targetC fallback (line 735)
             - Local-type behavior is completely unchanged
             - Non-recursive non-local types still get rootFinal -}
          rootFinalInvolvesMu =
            let go visited nid0 =
                  let nid = canonicalFinal nid0
                      key = getNodeId nid
                   in if IntSet.member key visited
                        then False
                        else case lookupNodeIn nodesFinal nid of
                          Just TyMu {} -> True
                          Just TyVar {tnBound = Just bnd} ->
                            go (IntSet.insert key visited) bnd
                          Just TyForall {tnBody = b} ->
                            go (IntSet.insert key visited) b
                          Just TyExp {tnBody = b} ->
                            go (IntSet.insert key visited) b
                          Just TyArrow {tnDom = l, tnCod = r} ->
                            let visited' = IntSet.insert key visited
                             in go visited' l || go visited' r
                          _ -> False
             in go IntSet.empty rootFinal
          rootIsSchemeRoot =
            IntSet.member (getNodeId rootFinal) schemeRootSetBase
          rootIsSchemeAlias =
            rootIsSchemeRoot
              && maybe False (const True) rootBound
          rootBoundIsBaseLike = rootBoundIsBase
          rootLocalMultiInst =
            rootBindingIsLocalType
              && rootHasMultiInst
          rootLocalInstArgMultiBase =
            rootBindingIsLocalType
              && instArgRootMultiBase
          rootLocalSchemeAliasBaseLike =
            rootBindingIsLocalType
              && rootIsSchemeAlias
              && rootBoundIsBaseLike
          rootLocality = if rootBindingIsLocalType then LocalTypeRoot else NonLocalTypeRoot
          rootAllowsExplicitBaseTarget =
            not rootHasMultiInst
              && not instArgRootMultiBase
          admitBaseTarget reason baseC =
            BaseTargetAdmission
              { baseTargetAdmissionLocality = rootLocality,
                baseTargetAdmissionReason = reason,
                baseTargetAdmissionNode = baseC
              }
          classifyBaseTargetAdmission resolvedBaseC
            | IntSet.size rootBoundCandidates == 1
                && IntSet.member (getNodeId resolvedBaseC) rootBoundCandidates
                && rootAllowsExplicitBaseTarget =
                Just (admitBaseTarget AdmitByUniqueRootCandidate resolvedBaseC)
            | IntSet.null rootBaseBounds
                && IntSet.size instArgBaseBounds == 1
                && IntSet.member (getNodeId resolvedBaseC) instArgBaseBounds
                && rootAllowsExplicitBaseTarget =
                Just (admitBaseTarget AdmitByUniqueInstArgCandidate resolvedBaseC)
            | rootIsSchemeAlias
                && rootBoundIsBaseLike
                && rootAllowsExplicitBaseTarget
                && ( not rootBindingIsLocalType
                       || ( IntSet.null rootBoundCandidates
                              && IntSet.null instArgBaseBounds
                          )
                   ) =
                Just (admitBaseTarget AdmitBySchemeAliasBaseLike resolvedBaseC)
            | otherwise = Nothing
          baseTargetAdmission =
            case baseTargetCandidate of
              Just resolvedBaseC -> classifyBaseTargetAdmission resolvedBaseC
              Nothing -> Nothing
          boundVarTargetRoot = canonicalFinal (View.rtvSchemeBodyTarget targetView rootC)
          schemeRootSetFinal = schemeRootSetBase
          schemeRootOwnerFinal = schemeRootOwnerBase
          alignedScopeRootFor target =
            case View.rtvBindingScopeRefCanonical retainedChildView target of
              Right ref -> View.rtvScopeRefCanonical retainedChildView ref
              Left _ -> scopeRoot
          boundHasForallFrom scopeRootAligned start0 =
            let go visited nid0 =
                  let nid = canonicalFinal nid0
                      key = getNodeId nid
                      isNestedSchemeRoot =
                        case IntMap.lookup key schemeRootOwnerFinal of
                          Just gid ->
                            case scopeRootAligned of
                              GenRef gidScope -> gid /= gidScope
                              TypeRef _ -> True
                          Nothing -> False
                      reportHit label =
                        if debugGaScopeEnabled traceCfg
                          then
                            let owner = IntMap.lookup key schemeRootOwnerFinal
                                nodeTag =
                                  case lookupNodeIn nodesFinal (NodeId key) of
                                    Just TyVar {} -> "var"
                                    Just TyArrow {} -> "arrow"
                                    Just TyForall {} -> "forall"
                                    Just TyMu {} -> "mu"
                                    Just TyExp {} -> "exp"
                                    Just TyBase {} -> "base"
                                    Just TyCon {} -> "con"
                                    Just TyVarApp {} -> "varapp"
                                    Just TyBottom {} -> "bottom"
                                    Nothing -> "missing"
                             in debugGaScope
                                  traceCfg
                                  ( "runPipelineElab: boundHasForall hit="
                                      ++ label
                                      ++ " node="
                                      ++ show nid
                                      ++ " tag="
                                      ++ nodeTag
                                      ++ " owner="
                                      ++ show owner
                                      ++ " scope="
                                      ++ show scopeRootAligned
                                  )
                                  ()
                          else ()
                   in if IntSet.member key visited
                        then False
                        else
                          if IntSet.member key schemeRootSetFinal
                            then case lookupNodeIn nodesFinal (NodeId key) of
                              Just TyVar {tnBound = Just bnd} ->
                                go (IntSet.insert key visited) bnd
                              _ ->
                                if isNestedSchemeRoot
                                  then case reportHit "schemeRoot" of
                                    () -> True
                                  else False
                            else case lookupNodeIn nodesFinal (NodeId key) of
                              Just TyForall {} ->
                                case reportHit "TyForall" of
                                  () -> True
                              Just TyVar {tnBound = Just bnd} ->
                                let bndC = canonicalFinal bnd
                                    bndKey = getNodeId bndC
                                    bndNested =
                                      case IntMap.lookup bndKey schemeRootOwnerFinal of
                                        Just gid ->
                                          case scopeRootAligned of
                                            GenRef gidScope -> gid /= gidScope
                                            TypeRef _ -> True
                                        Nothing -> False
                                 in if IntSet.member bndKey schemeRootSetFinal
                                      then case lookupNodeIn nodesFinal bndC of
                                        Just TyVar {tnBound = Just bndInner} ->
                                          go (IntSet.insert key visited) bndInner
                                        _ ->
                                          if bndNested
                                            then case reportHit "boundSchemeRoot" of
                                              () -> True
                                            else False
                                      else go (IntSet.insert key visited) bndC
                              Just TyExp {tnBody = b} ->
                                go (IntSet.insert key visited) b
                              Just TyArrow {tnDom = d, tnCod = c} ->
                                let visited' = IntSet.insert key visited
                                 in go visited' d || go visited' c
                              _ -> False
             in go IntSet.empty start0
          recursiveTargetsFrom start0 =
            let go visited nid0 =
                  let nid = canonicalFinal nid0
                      key = getNodeId nid
                   in if IntSet.member key visited
                        then []
                        else case pvLookupNode retainedChildPresolutionView nid of
                          Just TyMu {} -> [nid]
                          Just TyVar {tnBound = Just bnd} ->
                            go (IntSet.insert key visited) bnd
                          Just TyForall {tnBody = b} ->
                            go (IntSet.insert key visited) b
                          Just TyExp {tnBody = b} ->
                            go (IntSet.insert key visited) b
                          Just TyArrow {tnDom = d, tnCod = c} ->
                            let visited' = IntSet.insert key visited
                             in go visited' d ++ go visited' c
                          _ -> []
             in nub (go IntSet.empty start0)
          instRootRecursiveTargetSelection =
            uniqueCandidate
              [ recursiveTarget
                | instRoot <- rootInstRoots,
                  recursiveTarget <- recursiveTargetsFrom instRoot
              ]
          sameWrapperRetainedChildSelection =
            let sameLocalTypeLane parentRef child =
                  parentRef == scopeRootPost
                    || case View.rtvBindingScopeRefCanonical viewFinalBounded child of
                      Right ref -> ref == scopeRootPost
                      Left _ -> False
                selectedSameWrapperNestedForallTarget childTarget bndRoot hasForall =
                  bndRoot == boundVarTargetRoot
                    && ( not hasForall
                           || childTarget == boundVarTargetRoot
                       )
                recursiveTargetProofFor childTarget =
                  let directRecursiveTargetSelection =
                        uniqueCandidate (recursiveTargetsFrom childTarget)
                      liftSelection selection =
                        case selection of
                          UniqueCandidate recursiveTarget ->
                            UniqueCandidate (recursiveTarget, alignedScopeRootFor recursiveTarget)
                          AmbiguousCandidate -> AmbiguousCandidate
                          NoCandidate -> NoCandidate
                   in case liftSelection directRecursiveTargetSelection of
                        UniqueCandidate directProof -> UniqueCandidate directProof
                        AmbiguousCandidate -> AmbiguousCandidate
                        NoCandidate -> liftSelection instRootRecursiveTargetSelection
                candidates =
                  [ let childC = canonicalFinal child
                        childTarget = canonicalFinal (View.rtvSchemeBodyTarget retainedChildView child)
                        bndC = canonicalFinal bnd
                        bndRoot = canonicalFinal (View.rtvSchemeBodyTarget retainedChildView bnd)
                        chosenTargetProof =
                          case recursiveTargetProofFor childTarget of
                            NoCandidate -> UniqueCandidate (childC, scopeRoot)
                            other -> other
                        hasForall =
                          case chosenTargetProof of
                            UniqueCandidate (_chosenTarget, chosenScopeRoot) ->
                              Just (boundHasForallFrom chosenScopeRoot bndC)
                            _ -> Nothing
                     in (childC, childTarget, bndC, bndRoot, chosenTargetProof, hasForall)
                    | (childKey, (parentRef, _flag)) <- IntMap.toList (cBindParents (pvCanonicalConstraint retainedChildPresolutionView)),
                      TypeRef child <- [nodeRefFromKey childKey],
                      sameLocalTypeLane parentRef (canonicalFinal child),
                      Just bnd <- [View.rtvLookupVarBound retainedChildView (canonicalFinal child)]
                  ]
                matchingCandidates =
                  [ RetainedChildProof
                      { retainedChildProofChild = child,
                        retainedChildProofChildTarget = childTarget,
                        retainedChildProofChosenTarget = chosenTarget,
                        retainedChildProofChosenScopeRoot = chosenScopeRoot
                      }
                    | (child, childTarget, _bnd, bndRoot, UniqueCandidate (chosenTarget, chosenScopeRoot), Just hasForall) <- candidates,
                      selectedSameWrapperNestedForallTarget childTarget bndRoot hasForall
                  ]
                matchingCandidateSelection =
                  uniqueCandidate matchingCandidates
                hasAmbiguousMatchingCandidate =
                  any
                    ( \(_child, _childTarget, _bnd, bndRoot, chosenTargetProof, _hasForall) ->
                        bndRoot == boundVarTargetRoot
                          && case chosenTargetProof of
                            AmbiguousCandidate -> True
                            _ -> False
                    )
                    candidates
                debugCandidates =
                  if debugGaScopeEnabled traceCfg
                    then
                      debugGaScope
                        traceCfg
                        ( "runPipelineElab: boundVarTargetRoot="
                            ++ show boundVarTargetRoot
                            ++ " scopeRoot="
                            ++ show scopeRoot
                            ++ " scopeRootPost="
                            ++ show scopeRootPost
                            ++ " rootBindingIsLocalType="
                            ++ show rootBindingIsLocalType
                            ++ " candidates="
                            ++ show
                              [ (child, childTarget, bnd, bndRoot, chosenTargetProof, hasForall)
                                | (child, childTarget, bnd, bndRoot, chosenTargetProof, hasForall) <- candidates
                              ]
                            ++ " matchingCandidateSelection="
                            ++ show matchingCandidateSelection
                            ++ " hasAmbiguousMatchingCandidate="
                            ++ show hasAmbiguousMatchingCandidate
                        )
                        ()
                    else ()
             in case debugCandidates of
                  () ->
                    if rootBindingIsLocalType
                      then
                        if hasAmbiguousMatchingCandidate
                          then AmbiguousCandidate
                          else matchingCandidateSelection
                      else NoCandidate
          sameWrapperRetainedChildProof = candidateSelectionValue sameWrapperRetainedChildSelection
          sameWrapperRetainedChildAmbiguous = candidateSelectionIsAmbiguous sameWrapperRetainedChildSelection
          recursiveCandidateSelection =
            if sameWrapperRetainedChildAmbiguous
              then AmbiguousCandidate
              else
                uniqueCandidate
                  ( maybe [] (pure . RecursiveCandidateBaseTarget) baseTargetAdmission
                      ++ maybe [] (pure . RecursiveCandidateRetainedChild) sameWrapperRetainedChildProof
                  )
          selectedRecursiveCandidate = candidateSelectionValue recursiveCandidateSelection
          selectedBaseTargetAdmission =
            case selectedRecursiveCandidate of
              Just (RecursiveCandidateBaseTarget admission) -> Just admission
              _ -> Nothing
          selectedRetainedChildProof =
            case selectedRecursiveCandidate of
              Just (RecursiveCandidateRetainedChild retainedChildProof') -> Just retainedChildProof'
              _ -> Nothing
          recursiveCandidateAmbiguous = candidateSelectionIsAmbiguous recursiveCandidateSelection
          boundVarTarget =
            fmap retainedChildProofChild selectedRetainedChildProof
          sameLaneLocalRetainedChildTarget =
            if rootBindingIsLocalType
              then fmap retainedChildProofChosenTarget selectedRetainedChildProof
              else Nothing
          sameLaneLocalRetainedChildScopeRoot =
            if rootBindingIsLocalType
              then fmap retainedChildProofChosenScopeRoot selectedRetainedChildProof
              else Nothing
          keepTargetFinal =
            rootBindingIsLocalType
              && ( rootLocalMultiInst
                     || rootLocalInstArgMultiBase
                     || rootLocalSchemeAliasBaseLike
                     || recursiveCandidateAmbiguous
                     || maybe False (const True) sameLaneLocalRetainedChildTarget
                 )
      let targetC =
            case selectedBaseTargetAdmission of
              Just admission -> baseTargetAdmissionNode admission
              _ ->
                if keepTargetFinal
                  then
                    if rootLocalSchemeAliasBaseLike
                      || rootLocalMultiInst
                      || rootLocalInstArgMultiBase
                      || recursiveCandidateAmbiguous
                      then rootFinal
                      else
                        case sameLaneLocalRetainedChildTarget of
                          Just v
                            | v /= rootFinal -> v
                          _ ->
                            case lookupNodeIn nodesFinal rootFinal of
                              Just TyVar {} -> rootFinal
                              _ -> View.rtvSchemeBodyTarget targetView rootC
                  else
                    if rootBindingIsLocalType
                      then View.rtvSchemeBodyTarget targetView rootC
                      else
                        if rootFinalInvolvesMu
                          then View.rtvSchemeBodyTarget viewFinalBounded rootC
                          else rootFinal
      let useSameLaneLocalRetainedChildScopeRoot =
            case selectedBaseTargetAdmission of
              Just _ -> False
              Nothing ->
                keepTargetFinal
                  && not rootLocalSchemeAliasBaseLike
                  && not rootLocalMultiInst
                  && not rootLocalInstArgMultiBase
                  && maybe False (/= rootFinal) sameLaneLocalRetainedChildTarget
      let generalizeScopeRoot =
            if useSameLaneLocalRetainedChildScopeRoot
              then
                case sameLaneLocalRetainedChildScopeRoot of
                  Just alignedScopeRoot -> alignedScopeRoot
                  Nothing -> scopeRoot
              else scopeRoot
      (sch, _subst) <-
        View.rtvGeneralizeTarget viewFinalBounded generalizeScopeRoot targetC
      debugWhenM
        traceCfg
        ( "runPipelineElab: final scheme="
            ++ pretty sch
            ++ " keepTargetBase="
            ++ show (case selectedBaseTargetAdmission of Just _ -> True; Nothing -> False)
            ++ " keepTargetFinal="
            ++ show keepTargetFinal
            ++ " targetC="
            ++ show targetC
            ++ " recursiveCandidateSelection="
            ++ show recursiveCandidateSelection
            ++ " baseTargetAdmission="
            ++ show baseTargetAdmission
            ++ " boundVarTarget="
            ++ show boundVarTarget
            ++ " sameWrapperRetainedChildProof="
            ++ show selectedRetainedChildProof
            ++ " rootInstRoots="
            ++ show rootInstRoots
            ++ " boundVarTargetRoot="
            ++ show boundVarTargetRoot
            ++ " scopeRoot="
            ++ show scopeRoot
            ++ " generalizeScopeRoot="
            ++ show generalizeScopeRoot
            ++ " rootBindingIsLocalType="
            ++ show rootBindingIsLocalType
            ++ " rootFinalInvolvesMu="
            ++ show rootFinalInvolvesMu
        )
      let ty = case sch of
            Forall binds body -> foldr (\(n, b) t -> TForall n b t) body binds
      pure ty
