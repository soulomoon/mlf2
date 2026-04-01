{-# LANGUAGE GADTs #-}

module MLF.Elab.Run.ResultType.Fallback.Core
  ( computeResultTypeFallbackCore,
  )
where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)
import MLF.Constraint.Presolution (EdgeTrace (..), PresolutionView (..))
import MLF.Constraint.Presolution.Base (CopyMapping (..), lookupCopy)
import MLF.Constraint.Types.Graph
  ( EdgeId (..),
    GenNode (..),
    NodeId (..),
    NodeRef (..),
    TyNode (..),
    cBindParents,
    cGenNodes,
    cNodes,
    fromListNode,
    genNodeKey,
    getEdgeId,
    getNodeId,
    gnId,
    gnSchemes,
    lookupNodeIn,
    nodeRefFromKey,
    toListGen,
    toListNode,
  )
import MLF.Constraint.Types.Witness (EdgeWitness (..))
import MLF.Elab.Generalize (GaBindParents (..))
import MLF.Elab.Phi (phiFromEdgeWitnessWithTrace)
import MLF.Elab.Run.Annotation (annNode)
import qualified MLF.Elab.Run.ChiQuery as ChiQuery
import MLF.Elab.Run.Debug (debugGaScope, debugGaScopeEnabled, debugWhenCondM, debugWhenM)
import MLF.Elab.Run.Generalize (generalizeAtWithBuilder)
import MLF.Elab.Run.Generalize.Common (canonicalSchemeRootOwners)
import MLF.Elab.Run.ResultType.Types
  ( ResultTypeInputs (..),
    rtcEdgeExpansions,
    rtcEdgeTraces,
    rtcEdgeWitnesses,
  )
import MLF.Elab.Run.ResultType.Util
  ( collectEdges,
    generalizeWithPlan,
    resultTypeRoots,
    stripAnn,
  )
import qualified MLF.Elab.Run.ResultType.View as View
import MLF.Elab.Run.Scope
  ( bindingScopeRefCanonical,
    canonicalizeScopeRef,
    resolveCanonicalScope,
    schemeBodyTarget,
  )
import MLF.Elab.Types
import MLF.Frontend.ConstraintGen (AnnExpr (..))

-- | Core implementation of computeResultTypeFallback (non-annotated-lambda case).
computeResultTypeFallbackCore ::
  ResultTypeInputs ->
  View.ResultTypeView ->
  -- | annCanon (post-redirect)
  AnnExpr ->
  -- | ann (pre-redirect)
  AnnExpr ->
  Either ElabError ElabType
computeResultTypeFallbackCore ctx viewBase annCanon ann = do
  let canonical = rtcCanonical ctx
      edgeWitnesses = rtcEdgeWitnesses ctx
      edgeTraces = rtcEdgeTraces ctx
      edgeExpansions = rtcEdgeExpansions ctx
      presolutionView = View.rtvPresolutionViewOverlay viewBase
      bindParentsGa = rtcBindParentsGa ctx
      planBuilder = rtcPlanBuilder ctx
      c1 = rtcBaseConstraint ctx
      redirects = rtcRedirects ctx
      traceCfg = rtcTraceConfig ctx
      generalizeAtWith mbGa =
        generalizeAtWithBuilder planBuilder mbGa presolutionView

  let edgeTraceCounts =
        IntMap.fromListWith
          (+)
          [ (getNodeId (etRoot tr), 1 :: Int)
          | tr <- IntMap.elems edgeTraces
          ]
  let (rootForTypeAnn, rootForTypePreAnn) =
        resultTypeRoots
          canonical
          (ChiQuery.chiConstraint presolutionView)
          c1
          annCanon
          ann
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
    _ -> do
      let rootC = canonical rootForType
          nodes = cNodes (pvConstraint presolutionView)
          nodeList = map snd (toListNode nodes)
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
                listToMaybe
                  [ tnId node
                  | node@TyBase { tnBase = nodeBase } <- nodeList,
                    nodeBase == base
                  ]
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
          argBounds arg = do
            baseC <- resolveBaseBoundCanonical arg
            case lookupNodeIn nodes baseC of
              Just TyBase {} -> Just baseC
              Just TyBottom {} -> Just baseC
              _ -> Nothing
          instArgNode tr binder arg =
            case lookupCopy binder (etCopyMap tr) of
              Just copyN -> copyN
              Nothing -> arg
          edgeBaseBounds =
            IntSet.fromList
              [ getNodeId baseC
              | ew <- IntMap.elems edgeWitnesses,
                Just baseC <- [argBounds (ewRight ew)]
              ]
          rootArgBaseBounds =
            let argEdgeBases eid =
                  case IntMap.lookup (getEdgeId eid) edgeTraces of
                    Nothing -> IntSet.empty
                    Just tr ->
                      IntSet.fromList
                        [ getNodeId baseC
                        | (binder, arg) <- etBinderArgs tr,
                          let argNode =
                                instArgNode tr binder arg,
                          Just baseC <- [argBounds argNode]
                        ]
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
          instArgBaseBounds =
            let binderBounds =
                  IntMap.fromListWith
                    IntSet.union
                    [ (getNodeId (canonical binderRoot), IntSet.singleton (getNodeId baseC))
                    | tr <- IntMap.elems edgeTraces,
                      let copyMapInv =
                            IntMap.fromListWith
                              min
                              [ (getNodeId copyN, getNodeId origN)
                              | (origKey, copyN) <- IntMap.toList (getCopyMapping (etCopyMap tr)),
                                let origN = NodeId origKey
                              ],
                      (binder, arg) <- etBinderArgs tr,
                      let binderRoot =
                            case IntMap.lookup (getNodeId binder) copyMapInv of
                              Just origKey -> NodeId origKey
                              Nothing -> binder,
                      let argNode = instArgNode tr binder arg,
                      Just baseC <- [argBounds argNode]
                    ]
             in IntSet.union
                  ( IntSet.unions
                      [ bounds
                      | bounds <- IntMap.elems binderBounds,
                        IntSet.size bounds > 1
                      ]
                  )
                  edgeBaseBounds
          (rootBounds, instArgRootMultiBase) =
            let rootBounds' =
                  IntMap.fromListWith
                    IntSet.union
                    [ (getNodeId (canonical (etRoot tr)), IntSet.singleton (getNodeId baseC))
                    | tr <- IntMap.elems edgeTraces,
                      (binder, arg) <- etBinderArgs tr,
                      let argNode = instArgNode tr binder arg,
                      Just baseC <- [argBounds argNode]
                    ]
                multiBase =
                  any (\s -> IntSet.size s > 1) (IntMap.elems rootBounds')
                    || IntSet.size edgeBaseBounds > 1
             in (rootBounds', multiBase)

      let rootBaseBounds =
            IntMap.findWithDefault IntSet.empty (getNodeId rootC) rootBounds
          rootBoundCandidates =
            if IntSet.null rootBaseBounds
              then rootArgBaseBounds
              else rootBaseBounds
      let baseTarget =
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
            case (baseTarget, lookupNodeIn nodes rootC) of
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
                  | TyBase { tnBase = nodeBase } <- map snd (toListNode nodes)
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
         overlay at the result-type view boundary, then materialize a
         solved view from that overlay when needed.  This keeps fallback
         logic read-oriented and avoids local rebuildWithNodes patching. -}
      let viewFinalBounded =
            case boundTarget of
              Nothing -> viewBase
              Just baseN -> View.rtvWithBoundOverlay rootC baseN viewBase
          presolutionViewFinal = View.rtvPresolutionViewOverlay viewFinalBounded
      let scopeRootNodePre = rootForTypePre
      scopeRootPre <- bindingToElab (resolveCanonicalScope c1 presolutionViewFinal redirects scopeRootNodePre)
      let rootBindingIsLocalType =
            case bindingScopeRefCanonical presolutionViewFinal rootC of
              Right TypeRef {} -> True
              _ -> False
      let scopeRootPost =
            case bindingScopeRefCanonical presolutionViewFinal rootC of
              Right ref -> canonicalizeScopeRef presolutionViewFinal redirects ref
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
          genNodesFinal = map snd (toListGen (cGenNodes (ChiQuery.chiConstraint presolutionViewFinal)))
          targetPresolutionView =
            if rootBindingIsLocalType
              then presolutionViewFinal
              else presolutionView
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
            any
              (\gen -> any (\root -> canonicalFinal root == rootFinal) (gnSchemes gen))
              genNodesFinal
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
          rootLocalInstArgSingleBase =
            rootBindingIsLocalType
              && IntSet.null rootBaseBounds
              && IntSet.size instArgBaseBounds == 1
              && not rootHasMultiInst
              && not instArgRootMultiBase
          rootLocalEmptyCandidateSchemeAliasBaseLike =
            rootBindingIsLocalType
              && rootIsSchemeAlias
              && rootBoundIsBaseLike
              && IntSet.null rootBoundCandidates
              && IntSet.null instArgBaseBounds
              && not rootHasMultiInst
              && not instArgRootMultiBase
          rootNonLocalSchemeAliasBaseLike =
            not rootBindingIsLocalType
              && rootIsSchemeAlias
              && rootBoundIsBaseLike
          rootLocalSchemeAliasBaseLike =
            rootBindingIsLocalType
              && rootIsSchemeAlias
              && rootBoundIsBaseLike
          rootLocalSingleBase =
            rootBindingIsLocalType
              && IntSet.size rootBoundCandidates == 1
              && not rootHasMultiInst
              && not instArgRootMultiBase
          boundVarTargetRoot = canonicalFinal (schemeBodyTarget targetPresolutionView rootC)
          (schemeRootSetFinal, schemeRootOwnerFinal) =
            canonicalSchemeRootOwners
              canonicalFinal
              (IntMap.fromList [(genNodeKey (gnId gen), gen) | gen <- genNodesFinal])
          boundHasForallFrom start0 =
            let go visited nid0 =
                  let nid = canonicalFinal nid0
                      key = getNodeId nid
                      isNestedSchemeRoot =
                        case IntMap.lookup key schemeRootOwnerFinal of
                          Just gid ->
                            case scopeRoot of
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
                                      ++ show scopeRoot
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
                                          case scopeRoot of
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
          boundVarTarget =
            let sameLocalTypeLane child =
                  case bindingScopeRefCanonical presolutionViewFinal child of
                    Right ref -> ref == scopeRootPost
                    Left _ -> False
                candidatesFor keepCandidate =
                  [ ( canonicalFinal child,
                      canonicalFinal bnd,
                      canonicalFinal (schemeBodyTarget targetPresolutionView bnd),
                      boundHasForallFrom bnd
                    )
                  | (childKey, (parentRef, _flag)) <- IntMap.toList (cBindParents (ChiQuery.chiCanonicalConstraint presolutionViewFinal)),
                    TypeRef child <- [nodeRefFromKey childKey],
                    keepCandidate parentRef (canonicalFinal child),
                    Just bnd <- [View.rtvLookupVarBound viewFinalBounded (canonicalFinal child)]
                  ]
                pickCandidate keepCandidate =
                  let candidates = candidatesFor keepCandidate
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
                                    [ (child, bnd, bndRoot, hasForall)
                                    | (child, bnd, bndRoot, hasForall) <- candidates
                                    ]
                              )
                              ()
                          else ()
                   in case debugCandidates of
                        () ->
                          listToMaybe
                            [ child
                            | (child, _bnd, bndRoot, hasForall) <- candidates,
                              bndRoot == boundVarTargetRoot,
                              not hasForall
                            ]
             in if rootBindingIsLocalType
                  then pickCandidate (\_parentRef child -> sameLocalTypeLane child)
                  else pickCandidate (\parentRef _child -> parentRef == scopeRoot)
          sameLaneLocalRetainedChildTarget =
            if rootBindingIsLocalType
              then boundVarTarget
              else Nothing
          keepTargetFinal =
            rootBindingIsLocalType
              && ( rootLocalMultiInst
                     || rootLocalInstArgMultiBase
                     || rootLocalSchemeAliasBaseLike
                     || maybe False (const True) sameLaneLocalRetainedChildTarget
                 )
      let targetC =
            case baseTarget of
              Just baseC
                | rootLocalSingleBase -> baseC
              Just baseC
                | rootLocalInstArgSingleBase -> baseC
              Just baseC
                | rootLocalEmptyCandidateSchemeAliasBaseLike -> baseC
              Just baseC
                | rootNonLocalSchemeAliasBaseLike -> baseC
              _ ->
                if keepTargetFinal
                  then
                    if rootLocalSchemeAliasBaseLike
                      || rootLocalMultiInst
                      || rootLocalInstArgMultiBase
                      then rootFinal
                      else case lookupNodeIn nodesFinal rootFinal of
                        Just TyVar {} -> rootFinal
                        _ ->
                          case sameLaneLocalRetainedChildTarget of
                            Just v -> v
                            Nothing -> schemeBodyTarget targetPresolutionView rootC
                  else
                    if rootBindingIsLocalType
                      then schemeBodyTarget targetPresolutionView rootC
                      else
                        if rootFinalInvolvesMu
                          then schemeBodyTarget presolutionViewFinal rootC
                          else rootFinal
      let bindParentsGaFinal =
            case boundTarget of
              Just baseN ->
                let baseConstraint = gaBaseConstraint bindParentsGa
                    baseRoot =
                      IntMap.findWithDefault
                        rootC
                        (getNodeId rootC)
                        (gaSolvedToBase bindParentsGa)
                    nodes0 = cNodes baseConstraint
                    adjustNode node =
                      case node of
                        TyVar {tnId = nid, tnBound = Nothing} ->
                          TyVar {tnId = nid, tnBound = Just baseN}
                        _ -> node
                    nodes' =
                      fromListNode
                        [ (nid, if nid == baseRoot then adjustNode node else node)
                        | (nid, node) <- toListNode nodes0
                        ]
                    baseConstraint' = baseConstraint {cNodes = nodes'}
                 in bindParentsGa {gaBaseConstraint = baseConstraint'}
              Nothing -> bindParentsGa
      (sch, _subst) <-
        generalizeWithPlan planBuilder bindParentsGaFinal presolutionViewFinal scopeRoot targetC
      debugWhenM
        traceCfg
        ( "runPipelineElab: final scheme="
            ++ pretty sch
            ++ " keepTargetBase="
            ++ show (case baseTarget of Just _ -> True; Nothing -> False)
            ++ " keepTargetFinal="
            ++ show keepTargetFinal
            ++ " targetC="
            ++ show targetC
            ++ " boundVarTarget="
            ++ show boundVarTarget
            ++ " boundVarTargetRoot="
            ++ show boundVarTargetRoot
            ++ " scopeRoot="
            ++ show scopeRoot
            ++ " rootBindingIsLocalType="
            ++ show rootBindingIsLocalType
            ++ " rootFinalInvolvesMu="
            ++ show rootFinalInvolvesMu
        )
      let ty = case sch of
            Forall binds body -> foldr (\(n, b) t -> TForall n b t) body binds
      pure ty
