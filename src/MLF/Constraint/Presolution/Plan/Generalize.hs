-- |
-- Module      : MLF.Constraint.Presolution.Plan.Generalize
-- Description : Build a generalization plan for a single scope
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
--
-- Builds a 'GeneralizePlan' for a single generalization scope, determining
-- which variables to generalize, how to order binders, and how to handle
-- scheme roots.
module MLF.Constraint.Presolution.Plan.Generalize
  ( GeneralizePlan (..),
    planGeneralizeAt,
  )
where

{- Note [Generalization plan construction — planGeneralizeAt]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'planGeneralizeAt' builds a 'GeneralizePlan' for a single generalization scope
(one gen node in the constraint graph).  This is the core of the thesis §15.3
generalization algorithm:

  1. Identify the scope root and type root for the gen node.
  2. Collect bindable children (variables under the gen scope).
  3. Build binder plans: determine which variables become ∀-binders, their
     ordering (via the <P topological sort), and their bounds.
  4. Determine scheme roots and handle alias/wrapper nodes.
  5. Compute the reify plan for type structure reconstruction.

The plan is a pure data structure ('GeneralizePlan') consumed later by
elaboration ('MLF.Elab.Elaborate') and reification ('MLF.Reify.Type.Core').
Separating planning from execution keeps presolution logic deterministic and
testable independently of the elaboration pipeline.
-}

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe, maybeToList)
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Plan.BinderPlan
  ( AliasEnv (..),
    BinderPlan (..),
    BinderPlanInput (..),
    BinderSelectionEnv (..),
    GaBindParentsInfo (..),
    SelectBindersArgs (..),
    bindableChildrenUnder,
    buildBinderPlan,
    computeAliasBinders,
    hasExplicitBoundFor,
    isQuantifiable,
    mkIsBindable,
    orderBinderCandidates,
    selectBinders,
  )
import MLF.Constraint.Presolution.Plan.Requirements
  ( expansionConstructionRoleKeys,
  )
import MLF.Constraint.Presolution.Plan.Context
  ( GaBindParents (..),
    GeneralizationRequirements (..),
    GeneralizeCtx (..),
    GeneralizeEnv (..),
    resolveContext,
    traceGeneralize,
    traceGeneralizeM,
  )
import MLF.Constraint.Presolution.Plan.Env
  ( PresolutionEnv (..),
    lookupNodeInMap,
    mkGeneralizeEnv,
  )
import MLF.Constraint.Presolution.Plan.SchemeRoots
  ( SchemeRootInfo (..),
    SchemeRootsPlan (..),
    allowBoundTraversalFor,
  )
import MLF.Constraint.Presolution.Plan.Target
  ( GammaPlan (..),
    GammaPlanInput (..),
    TargetPlan (..),
    TargetPlanInput (..),
    ReifyRootSource (..),
    TypeRootPlan (..),
    TypeRootPlanInput (..),
    buildGammaPlan,
    buildTargetPlan,
    buildTypeRootPlan,
  )
import MLF.Constraint.Types.Graph hiding (lookupNode)
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Reify.Named (softenCanonicalBindParentsUnder)
import MLF.Reify.Type (reifyTypeWithRefsNoFallback)
import MLF.Reify.TypeOps (freeTypeVarRefsType)
import MLF.Types.Elab (typeBinderRefsSameIdentity)
import MLF.Util.ElabError (ElabError (..), bindingToElab)
import MLF.Util.Graph (reachableFrom, reachableFromStop)
import qualified MLF.Util.IntMapUtils as IntMapUtils

data GeneralizePlan p = GeneralizePlan
  { gpEnv :: GeneralizeEnv p,
    gpContext :: GeneralizeCtx p,
    gpSchemeRootsPlan :: SchemeRootsPlan,
    gpTargetPlan :: TargetPlan,
    gpGammaPlan :: GammaPlan,
    gpTypeRootPlan :: TypeRootPlan,
    gpBinderPlan :: BinderPlan,
    gpScopeSchemeRoots :: IntSet.IntSet,
    gpScopeHasStructuralScheme :: Bool,
    gpBinders0 :: [NodeId],
    gpReachableFromWithBounds :: NodeId -> IntSet.IntSet,
    gpReachableFromStructural :: NodeId -> IntSet.IntSet,
    -- Canonical binding parents before W-softening.  Generalization uses the
    -- softened view to select flexible binders, but elaboration must retain
    -- the original rigid ownership in order to inline rigid quantification.
    gpRigidBindParents :: BindParents,
    gpBindParents :: BindParents
  }

planGeneralizeAt :: PresolutionEnv p -> Either ElabError (GeneralizePlan p)
planGeneralizeAt
  PresolutionEnv
    { pePresolutionView = presolutionView,
      peBindParentsGa = mbBindParentsGa,
      peRequirements = requirements,
      peScopeRoot = scopeRoot,
      peTargetNode = targetNode,
      peTraceConfig = traceCfg
    } = do
    env <- mkGeneralizeEnv traceCfg mbBindParentsGa presolutionView
    let constraint = geConstraint env
        nodes = geNodes env
        canonical = geCanonical env
        canonKey = geCanonKey env
        isTyVarKey = geIsTyVarKey env
        isBaseLikeKey = geIsBaseLikeKey env
    bindParents0 <- bindingToElab (Binding.canonicalizeBindParentsUnder canonical constraint)
    let bindParentsSoft = softenCanonicalBindParentsUnder canonical constraint bindParents0
    let _ =
          traceGeneralize
            env
            ( "generalizeAt: gaParents sizes="
                ++ case mbBindParentsGa of
                  Nothing -> "None"
                  Just ga ->
                    " baseParents="
                      ++ show (IntMap.size (gaBindParentsBase ga))
                      ++ " baseToSolved="
                      ++ show (IntMap.size (gaBaseToSolved ga))
                      ++ " solvedToBase="
                      ++ show (IntMap.size (gaSolvedToBase ga))
            )
            ()
    ctx <- resolveContext env bindParentsSoft scopeRoot targetNode
    let GeneralizeCtx
          { gcTarget0 = target0,
            gcTargetBase = targetBase,
            gcScopeRootC = scopeRootC,
            gcOrderRoot = orderRoot,
            gcTypeRoot0 = typeRoot0,
            gcOrderRootBase = orderRootBase,
            gcScopeGen = scopeGen,
            gcBindParents = bindParents,
            gcFirstGenAncestor = firstGenAncestorGa,
            gcResForReify = resForReify,
            gcBindParentsGaInfo = mbBindParentsGaInfo,
            gcSchemeRootsPlan = schemeRootsPlan
          } = ctx
    -- Phase 4: scheme-root metadata and bound traversal policy.
    let SchemeRootsPlan
          { srInfo = schemeRootInfo,
            srSchemeRootOwnerBase = schemeRootOwnerBase,
            srSchemeRootByBodyBase = schemeRootByBodyBase,
            srLookupSchemeRootOwner = lookupSchemeRootOwner,
            srContainsForallForTarget = containsForallForTarget,
            srBoundHasForallForVar = boundHasForallForVar
          } = schemeRootsPlan
        SchemeRootInfo
          { sriRootKeySetRaw = schemeRootKeySetRaw,
            sriRootKeySet = schemeRootKeySet,
            sriRootOwner = schemeRootOwner,
            sriRootByBody = schemeRootByBody
          } = schemeRootInfo
        typeRootFromBoundVar =
          case (isTyVarKey (canonKey target0), scopeGen) of
            (True, Just gid) ->
              listToMaybe
                [ canonical child
                | child <- IntMapUtils.typeChildrenOfGen bindParents gid,
                  isTyVarKey (canonKey child),
                  case VarStore.lookupVarBound constraint (canonical child) of
                    Just bnd -> canonical bnd == canonical target0
                    Nothing -> False
                ]
            _ -> Nothing
    let orderRootForBinders = orderRoot
        orderRootBaseForBinders =
          case mbBindParentsGa of
            Nothing -> orderRootForBinders
            Just ga ->
              let baseConstraint = gaBaseConstraint ga
                  baseNodes = cNodes baseConstraint
                  useSchemeBody baseN =
                    case lookupNodeIn baseNodes baseN of
                      Just TyVar {tnBound = Just bnd}
                        | IntMap.member (getNodeId baseN) schemeRootOwnerBase ->
                            bnd
                      _ -> baseN
               in case IntMap.lookup (getNodeId orderRootForBinders) (gaSolvedToBase ga) of
                    Just baseN -> useSchemeBody baseN
                    Nothing -> orderRootBase
        allowBoundTraversal =
          allowBoundTraversalFor schemeRootsPlan canonical scopeGen target0
        childrenWithBoundsWith nodes' allowBoundTraversal' nid =
          case lookupNodeInMap nodes' nid of
            Nothing -> []
            Just node ->
              case node of
                TyVar {tnBound = Just bnd}
                  | allowBoundTraversal' bnd ->
                      structuralChildrenWithBounds node
                _ ->
                  structuralChildren node
        reachableFromWithBoundsWith canonical' nodes' allowBoundTraversal' =
          reachableFrom getNodeId canonical' (childrenWithBoundsWith nodes' allowBoundTraversal')
        reachableFromWithBounds root0 =
          reachableFromWithBoundsWith canonical nodes allowBoundTraversal root0
        childrenStructural nid =
          case lookupNodeInMap nodes nid of
            Nothing -> []
            Just node -> structuralChildren node
        reachableFromStructural root0 =
          reachableFrom getNodeId canonical childrenStructural root0

    reachable <- Right (reachableFromWithBounds orderRoot)
    -- Binder membership belongs to the selected live type root.  The frozen
    -- base graph supplies ordering and source provenance only: after solving,
    -- a base placeholder can have been replaced by a structural type and must
    -- not be promoted back into the final Gamma merely because it was
    -- reachable before that replacement.
    let reachableForBinders0 = reachable
    traceGeneralizeM
      env
      ( "generalizeAt: reachable var parents="
          ++ show
            [ (NodeId nid, IntMap.lookup (nodeRefKey (typeRef (NodeId nid))) bindParents)
            | nid <- IntSet.toList reachable,
              case lookupNodeInMap nodes (NodeId nid) of
                Just TyVar {} -> True
                _ -> False
            ]
      )
    traceGeneralizeM
      env
      ("generalizeAt: schemeRootByBodyKeys=" ++ show (IntMap.keys schemeRootByBody))
    -- Phase 5: binder selection helpers and candidates.
    let scopeSchemeRootsFor gid =
          case NodeAccess.lookupGenNode constraint gid of
            Just gen ->
              IntSet.fromList
                [ getNodeId (canonical root)
                | root <- gnSchemes gen,
                  case IntMap.lookup (nodeRefKey (typeRef root)) bindParents of
                    Just (GenRef gid', _) | gid' == gid -> True
                    _ -> False
                ]
            Nothing -> IntSet.empty
        scopeSchemeRoots =
          case scopeGen of
            Just gid -> scopeSchemeRootsFor gid
            Nothing -> IntSet.empty
        scopeHasStructuralScheme =
          case scopeRootC of
            GenRef gid ->
              case NodeAccess.lookupGenNode constraint gid of
                Just gen ->
                  let schemeRoots = scopeSchemeRootsFor gid
                   in any
                        ( \root ->
                            not (IntSet.member (canonKey root) schemeRoots)
                        )
                        (gnSchemes gen)
                Nothing -> False
            _ -> False
        isQuantifiable' = isQuantifiable canonical constraint isTyVarKey
        bindFlags = IntMapUtils.allBindFlags bindParents
        isBindable =
          mkIsBindable
            bindFlags
            isQuantifiable'
        nodesMap = NodeMap nodes
    (aliasBinderBases, aliasBinderNodes) <-
      computeAliasBinders
        AliasEnv { aeCanonical = canonical,
            aeConstraint = constraint,
            aeNodes = nodesMap,
            aeBindParents = bindParents,
            aeDepthMap = IntMap.empty,
            aeScopeSchemeRoots = scopeSchemeRoots,
            aeNodeChildren = const IntSet.empty
          }
        canonKey
        scopeRootC
        (traceGeneralizeM env)
    let bindableChildrenUnder' =
          bindableChildrenUnder canonical bindParents isBindable
        hasExplicitBound' = hasExplicitBoundFor canonical nodesMap constraint
        bseEnv =
          BinderSelectionEnv { bseCanonical = canonical,
              bseBindParents = bindParents,
              bseNodes = nodesMap,
              bseConstraint = constraint,
              bseIsBindable = isBindable
            }
    selectedBinders <-
      selectBinders
        bseEnv
        SelectBindersArgs
          { sbaCanonKey = canonKey,
            sbaScopeSchemeRoots = scopeSchemeRoots,
            sbaHasExplicitBoundP = hasExplicitBound',
            sbaCandidatePool = aliasBinderNodes,
            sbaTraceWarn = traceGeneralizeM env,
            sbaMGenId = scopeGen,
            sbaNodeRef = scopeRootC
        }
        target0
    let locallyClosedGammaKeys =
          IntSet.fromList
            [ canonKey (NodeId key)
            | key <- IntSet.toList (grLocallyClosedGammaNodes requirements)
            ]
        -- These are construction-role candidates, not yet declarations in
        -- the emitted term.  Only the elaboration boundary has the lexical
        -- environment needed for the paper's Gen(Gamma, tau) subtraction;
        -- it must remove candidates whose final identity route is already
        -- owned by ambient Gamma before constructing ETyAbs nodes.
        reachableConstructionBinders =
          case (scopeGen, mbBindParentsGaInfo) of
            (Just currentScope, Just ga) ->
              [ node
              | nodeKey <-
                  IntSet.toList
                    ( expansionConstructionRoleKeys
                        (gbiExpansionConstructionPlacements ga)
                    )
              , let node = canonical (NodeId nodeKey)
              , firstGenAncestorGa (typeRef node) == Just currentScope
              , IntSet.member (canonKey node) reachable
              , case lookupNodeInMap nodes node of
                  Just TyVar{} -> not (VarStore.isEliminatedVar constraint node)
                  _ -> False
              , case IntMap.lookup (nodeRefKey (typeRef node)) bindParents of
                  Just (_parent, BindFlex) -> True
                  _ -> False
              ]
            _ -> []
        binders0 =
          foldl insertDistinctBinder []
            [ binder
            | binder <- selectedBinders ++ reachableConstructionBinders
            , not (IntSet.member (canonKey binder) locallyClosedGammaKeys)
            ]
        insertDistinctBinder binders binder
          | any ((== canonKey binder) . canonKey) binders = binders
          | otherwise = binders ++ [binder]
    let schemeRootSkipSet =
          IntSet.difference
            ( IntSet.fromList
                [ getNodeId (canonical root)
                | (gid, root) <- sriRootsWithGen schemeRootInfo,
                  Just gid /= scopeGen
                ]
            )
            scopeSchemeRoots
        schemeRootBodySkipSet =
          IntSet.fromList
            [ bodyKey
            | (bodyKey, root) <- IntMap.toList schemeRootByBody,
              IntSet.member (canonKey root) schemeRootSkipSet
            ]
        schemeRootSkipKey key = IntSet.member key schemeRootSkipSet
        schemeRootBodySkipKey key = IntSet.member key schemeRootBodySkipSet
        schemeRootByBodyKey key = IntMap.member key schemeRootByBody
        schemeRootSkipOrBodyKey key =
          schemeRootSkipKey key || schemeRootByBodyKey key
        schemeRootSkipOrBodySkipKey key =
          schemeRootSkipKey key || schemeRootBodySkipKey key
        reachableFromWithBoundsStop root0 =
          let stopSet = schemeRootKeySet
              rootKey = getNodeId (canonical root0)
              -- The selected nested scheme root owns the interior we need to
              -- exclude.  Stop only at *other* scheme roots; stopping at the
              -- starting node reduces the interior to the wrapper itself and
              -- lets its lexical binders leak into the enclosing root subst.
              shouldStop nid =
                let key = getNodeId (canonical nid)
                 in key /= rootKey && IntSet.member key stopSet
              schemeInteriorChildren nid =
                case IntMap.lookup (getNodeId (canonical nid)) nodes of
                  Just node -> structuralChildrenWithBounds node
                  Nothing -> []
           in reachableFromStop getNodeId canonical schemeInteriorChildren shouldStop root0
        nestedSchemeInteriorSet =
          IntSet.unions
            [ IntSet.filter
                (\nodeKey ->
                  firstGenAncestorGa (typeRef (NodeId nodeKey))
                    == Just owner
                )
                (reachableFromWithBoundsStop (NodeId rootKey))
            | rootKey <- IntSet.toList schemeRootSkipSet,
              Just owner <- [IntMap.lookup rootKey schemeRootOwner]
            ]
        isNestedSchemeBound v =
          case lookupNodeInMap nodes (canonical v) of
            Just TyVar {tnBound = Just bnd} ->
              let bndC = canonical bnd
                  bndKey = getNodeId bndC
               in schemeRootSkipOrBodySkipKey bndKey
                    && not (IntSet.member bndKey reachable)
            _ -> False
        boundIsSchemeRootVar v =
          let walkBoundChain visited nid =
                let nidC = canonical nid
                    key = getNodeId nidC
                 in if IntSet.member key visited
                      then False
                      else case VarStore.lookupVarBound constraint nidC of
                        Just bnd ->
                          let bndC = canonical bnd
                              bndKey = getNodeId bndC
                           in if schemeRootSkipKey bndKey
                                then True
                                else case lookupNodeInMap nodes bndC of
                                  Just TyVar {} ->
                                    walkBoundChain (IntSet.insert key visited) bndC
                                  _ -> False
                        Nothing -> False
           in walkBoundChain IntSet.empty v
        boundIsSchemeRootAll v =
          case VarStore.lookupVarBound constraint (canonical v) of
            Just bnd ->
              let bndC = canonical bnd
                  bndKey = getNodeId bndC
                  hasSchemeRoot = schemeRootSkipOrBodyKey bndKey
               in hasSchemeRoot
            Nothing -> False
        targetPlan =
          buildTargetPlan
            TargetPlanInput
              { tpiConstraint = constraint,
                tpiNodes = nodes,
                tpiCanonical = canonical,
                tpiCanonKey = canonKey,
                tpiIsTyVarKey = isTyVarKey,
                tpiScopeGen = scopeGen,
                tpiScopeRootC = scopeRootC,
                tpiBindParents = bindParents,
                tpiTarget0 = target0,
                tpiSchemeRootKeySetRaw = schemeRootKeySetRaw,
                tpiSchemeRootKeySet = schemeRootKeySet,
                tpiSchemeRootOwnerBase = schemeRootOwnerBase,
                tpiSchemeRootByBodyBase = schemeRootByBodyBase,
                tpiContainsForallForTarget = containsForallForTarget,
                tpiFirstGenAncestor = firstGenAncestorGa,
                tpiReachableFromWithBounds = reachableFromWithBounds,
                tpiBindParentsGa = mbBindParentsGaInfo
              }
        TargetPlan
          { tpTargetBound = targetBound,
            tpTargetBoundUnderOtherGen = targetBoundUnderOtherGen,
            tpBoundUnderOtherGen = boundUnderOtherGen,
            tpTargetIsSchemeRoot = targetIsSchemeRoot,
            tpTargetIsSchemeRootForScope = targetIsSchemeRootForScope,
            tpTargetIsTyVar = targetIsTyVar
          } = targetPlan
    traceGeneralizeM
      env
      ( "generalizeAt: targetBound="
          ++ show targetBound
          ++ " schemeRootSkipSet="
          ++ show (IntSet.toList schemeRootSkipSet)
          ++ " boundParent="
          ++ case targetBound of
            Just bnd ->
              case IntMap.lookup (nodeRefKey (typeRef bnd)) bindParents of
                Just (parentRef, _flag) -> show parentRef
                Nothing -> "None"
            Nothing -> "None"
      )
    traceGeneralizeM
      env
      ( "generalizeAt: targetBoundOwner="
          ++ case targetBound of
            Just bnd -> show (lookupSchemeRootOwner bnd)
            Nothing -> "None"
          ++ " scopeGen="
          ++ show scopeGen
      )
    traceGeneralizeM
      env
      ( "generalizeAt: targetBoundNode="
          ++ case targetBound of
            Just bnd ->
              show (lookupNodeInMap nodes bnd)
                ++ " boundOfBound="
                ++ show (VarStore.lookupVarBound constraint bnd)
            Nothing -> "None"
      )
    gammaPlan <-
      buildGammaPlan
        GammaPlanInput
              { gpiDebugEnabled = geDebugEnabled env,
                gpiConstraint = constraint,
                gpiNodes = nodes,
                gpiCanonical = canonical,
                gpiCanonKey = canonKey,
                gpiIsTyVarKey = isTyVarKey,
                gpiBindParents = bindParents,
                gpiBindParentsGa = mbBindParentsGaInfo,
                gpiScopeGen = scopeGen,
                gpiTarget0 = target0,
                gpiTargetBound = targetBound,
                gpiSchemeRootOwnerBase = schemeRootOwnerBase,
                gpiSchemeRootOwner = schemeRootOwner,
                gpiSchemeRootByBody = schemeRootByBody,
                gpiSchemeRootKeySet = schemeRootKeySet,
                gpiOrderRoot = orderRoot,
                gpiOrderRootBase = orderRootBase,
                gpiTypeRoot0 = typeRoot0,
                gpiNamedUnderGaInterior = IntSet.empty,
                gpiNestedSchemeInteriorSet = nestedSchemeInteriorSet,
                gpiReachableForBinders0 = reachableForBinders0,
                gpiReachableFromWithBounds = reachableFromWithBounds,
                gpiBindableChildrenUnder = bindableChildrenUnder',
                gpiAliasBinderNodes = aliasBinderNodes,
                gpiFirstGenAncestor = firstGenAncestorGa,
                gpiRequirements = requirements
              }
    let GammaPlan
          { gpBaseGammaSet = baseGammaSet,
            gpBaseGammaRep = baseGammaRep,
            gpNamedUnderGaSet = namedUnderGaSet,
            gpSolvedToBasePref = solvedToBasePref,
            gpGammaAlias = gammaAlias,
            gpBaseGammaRepSet = baseGammaRepSet,
            gpReachableForBinders = reachableForBinders,
            gpGammaKeyFor = gammaKeyFor,
            gpNamedUnderGa = namedUnderGa,
            gpTypeRootHasNamedOutsideGamma = typeRootHasNamedOutsideGamma,
            gpRequiredGamma = requiredGamma,
            gpSourceBinderRefs = sourceBinderRefs
          } = gammaPlan
    let liftToForall bnd0 =
          case IntMap.lookup (getNodeId (canonical bnd0)) schemeRootByBody of
            Just root -> canonical root
            Nothing ->
              let climbToForall cur =
                    case IntMap.lookup (nodeRefKey (typeRef (canonical cur))) bindParents of
                      Just (TypeRef parent, _) ->
                        case lookupNodeInMap nodes (canonical parent) of
                          Just TyForall {} -> climbToForall (canonical parent)
                          _ -> cur
                      _ -> cur
               in climbToForall bnd0
        typeRootPlan =
          buildTypeRootPlan
            TypeRootPlanInput
              { trpiNodes = nodes,
                trpiCanonical = canonical,
                trpiCanonKey = canonKey,
                trpiIsTyVarKey = isTyVarKey,
                trpiIsBaseLikeKey = isBaseLikeKey,
                trpiBindParents = bindParents,
                trpiScopeRootC = scopeRootC,
                trpiScopeGen = scopeGen,
                trpiTarget0 = target0,
                trpiTargetBound = targetBound,
                trpiTargetIsSchemeRoot = targetIsSchemeRoot,
                trpiTargetIsSchemeRootForScope = targetIsSchemeRootForScope,
                trpiTargetIsTyVar = targetIsTyVar,
                trpiTargetBoundUnderOtherGen = targetBoundUnderOtherGen,
                trpiNamedUnderGaSet = namedUnderGaSet,
                trpiRequiredGammaKeys = IntSet.fromList (IntMap.keys requiredGamma),
                trpiTypeRoot0 = typeRoot0,
                trpiTypeRootFromBoundVar = typeRootFromBoundVar,
                trpiTypeRootHasNamedOutsideGamma = typeRootHasNamedOutsideGamma,
                trpiBoundHasForallForVar = boundHasForallForVar,
                trpiSchemeRootByBody = schemeRootByBody,
                trpiSchemeRootOwner = schemeRootOwner,
                trpiSchemeRootsPlan = schemeRootsPlan,
                trpiSolvedToBasePref = solvedToBasePref,
                trpiBindParentsGa = mbBindParentsGaInfo,
                trpiRequiresLiveRefinements = not (IntMap.null requiredGamma),
                trpiLiftToForall = liftToForall
              }
        TypeRootPlan
          { trTargetIsBaseLike = targetIsBaseLike,
            trTypeRoot = typeRoot,
            trReifyRootSource = reifyRootSource
          } = typeRootPlan
    reachableType <- Right (reachableFromWithBounds typeRoot)
    reachableTypeStructural <- Right (reachableFromStructural typeRoot)
    let reachableForBinderPlan =
          case (reifyRootSource, mbBindParentsGaInfo) of
            (ReifyBaseSchemeRoot baseRoot, Just ga) ->
              IntSet.union
                reachableForBinders
                (projectBaseBinderReachability ga baseRoot)
            _ -> reachableForBinders
        escapedFrozenForallBinders =
          case (reifyRootSource, mbBindParentsGaInfo) of
            (ReifyLiveRoot _, Just ga) ->
              case
                  IntMap.lookup
                    (canonKey typeRoot)
                    solvedToBasePref
              of
                Just baseRoot ->
                  IntMap.elems $
                    IntMap.fromList
                      [ (canonKey liveBinder, liveBinder)
                      | baseKey <-
                          IntSet.toList
                            (baseReachableFromWithBounds ga baseRoot)
                      , let baseBinder = NodeId baseKey
                      , Just TyVar {} <-
                          [ lookupNodeIn
                              (cNodes (gbiBaseConstraint ga))
                              baseBinder
                          ]
                      , Just (TypeRef baseOwner, BindFlex) <-
                          [ IntMap.lookup
                              (nodeRefKey (typeRef baseBinder))
                              (gbiBindParentsBase ga)
                          ]
                      , Just TyForall {} <-
                          [ lookupNodeIn
                              (cNodes (gbiBaseConstraint ga))
                              baseOwner
                          ]
                      , not
                          ( frozenForallOwnerSurvives
                              ga
                              baseOwner
                          )
                      , liveBinder <-
                          projectedLiveBinders
                            ga
                            baseBinder
                      , IntSet.member
                          (canonKey liveBinder)
                          reachableType
                      , Just TyVar {} <-
                          [lookupNodeInMap nodes liveBinder]
                      ]
                Nothing -> []
            _ -> []
        projectedLiveBinders ga baseBinder =
          IntMap.elems $
            IntMap.fromList
              ( directProjection ++ reversePreferredProjections )
          where
            directProjection =
              [ (canonKey projected, canonical projected)
              | projected <-
                  maybeToList
                    ( IntMap.lookup
                        (getNodeId baseBinder)
                        (gbiBaseToSolved ga)
                    )
              ]
            reversePreferredProjections =
              [ (liveKey, NodeId liveKey)
              | liveKey <- IntSet.toList reachableType
              , IntMap.lookup liveKey solvedToBasePref
                  == Just baseBinder
              ]
        frozenForallOwnerSurvives ga baseOwner =
          case
              IntMap.lookup
                (getNodeId baseOwner)
                (gbiBaseToSolved ga)
          of
            Just projectedOwner ->
              let liveOwner = canonical projectedOwner
              in IntSet.member
                    (canonKey liveOwner)
                    reachableType
                  && case lookupNodeInMap nodes liveOwner of
                    Just TyForall {} -> True
                    _ -> False
            Nothing -> False
        projectBaseBinderReachability ga baseRoot =
          IntSet.fromList
            [ canonKey liveNode
            | baseKey <-
                IntSet.toList
                  (baseReachableFromWithBounds ga baseRoot),
              let baseNode = NodeId baseKey,
              Just TyVar {} <-
                [lookupNodeIn (cNodes (gbiBaseConstraint ga)) baseNode],
              Just liveNode <- [projectBaseNode ga baseNode],
              Just TyVar {} <- [lookupNodeInMap nodes liveNode]
            ]
        projectBaseNode ga baseNode =
          let baseKey = getNodeId baseNode
              solvedNode =
                case IntMap.lookup baseKey (gbiBaseToSolved ga) of
                  Just solved -> Just solved
                  Nothing
                    | IntMap.member baseKey nodes -> Just baseNode
                    | otherwise -> Nothing
           in canonical <$> solvedNode
        baseReachableFromWithBounds ga baseRoot =
          let baseConstraint = gbiBaseConstraint ga
              baseNodes = cNodes baseConstraint
              baseSchemeOwner bnd =
                case
                    IntMap.lookup
                      (getNodeId bnd)
                      schemeRootOwnerBase
                  of
                    Just owner -> Just owner
                    Nothing -> do
                      root <-
                        IntMap.lookup
                          (getNodeId bnd)
                          schemeRootByBodyBase
                      IntMap.lookup
                        (getNodeId root)
                        schemeRootOwnerBase
              boundIsTargetSchemeBody bnd =
                case
                    IntMap.lookup
                      (getNodeId bnd)
                      schemeRootByBodyBase
                  of
                    Just root -> root == targetBase
                    Nothing -> False
              allowBaseBoundTraversal bnd =
                case baseSchemeOwner bnd of
                  Nothing -> True
                  Just owner ->
                    Just owner == scopeGen
                      || boundIsTargetSchemeBody bnd
              children baseNode =
                case lookupNodeIn baseNodes baseNode of
                  Just node@TyVar {tnBound = Just bnd}
                    | allowBaseBoundTraversal bnd ->
                        structuralChildrenWithBounds node
                  Just node -> structuralChildren node
                  Nothing -> []
           in reachableFrom getNodeId id children baseRoot
        sourceRefForLiveKey liveKey =
          case IntMap.lookup liveKey sourceBinderRefs of
            Just sourceRef -> Just sourceRef
            Nothing -> do
              baseNode <- IntMap.lookup liveKey solvedToBasePref
              IntMap.lookup (getNodeId baseNode) sourceBinderRefs
        sourceFacingSubst =
          IntMap.union
            ( IntMap.fromList
                [ (sourceKey, sourceRef)
                | (sourceKey, sourceRef) <-
                    IntMap.toList sourceBinderRefs
                , Just _ <-
                    [lookupNodeInMap nodes (NodeId sourceKey)]
                ]
            )
            ( IntMap.fromList
                [ (liveKey, sourceRef)
                | liveKey <- IntSet.toList reachableType
                , Just sourceRef <- [sourceRefForLiveKey liveKey]
                ]
            )
        reachableSourceRefs =
          [ sourceRef
          | liveKey <- IntSet.toList reachableType
          , Just sourceRef <- [sourceRefForLiveKey liveKey]
          ]
    escapedSourceBinderOccurrences <-
      case reifyRootSource of
        ReifyLiveRoot liveRoot
          | not (IntMap.null sourceFacingSubst) -> do
              sourceFacingType <-
                reifyTypeWithRefsNoFallback
                  resForReify
                  sourceFacingSubst
                  (canonical liveRoot)
              let freeSourceRefs =
                    freeTypeVarRefsType sourceFacingType
                  escapedOccurrences =
                    IntMap.elems $
                      IntMap.fromList
                        ( [ (sourceKey, sourceNode)
                          | (sourceKey, sourceRef) <-
                              IntMap.toList sourceBinderRefs
                          , let sourceNode = NodeId sourceKey
                          , Just TyVar {} <-
                              [lookupNodeInMap nodes sourceNode]
                          , any
                              (typeBinderRefsSameIdentity sourceRef)
                              reachableSourceRefs
                          , any
                              (typeBinderRefsSameIdentity sourceRef)
                              freeSourceRefs
                          , not
                              ( any
                                  (typeBinderRefsSameIdentity sourceRef)
                                  (grAmbientBinderRefs requirements)
                              )
                          ]
                            ++ [ (liveKey, liveNode)
                               | liveKey <- IntSet.toList reachableType
                               , let liveNode = NodeId liveKey
                               , Just TyVar {} <-
                                   [lookupNodeInMap nodes liveNode]
                               , Just sourceRef <-
                                   [sourceRefForLiveKey liveKey]
                               , any
                                   (typeBinderRefsSameIdentity sourceRef)
                                   freeSourceRefs
                               , not
                                   ( any
                                       (typeBinderRefsSameIdentity sourceRef)
                                       (grAmbientBinderRefs requirements)
                                   )
                               ]
                        )
              traceGeneralizeM
                env
                ( "generalizeAt: source-facing target="
                    ++ show sourceFacingType
                    ++ " source subst="
                    ++ show sourceFacingSubst
                    ++ " escaped source occurrences="
                    ++ show escapedOccurrences
                )
              pure escapedOccurrences
        _ -> pure []
    let orderBinderCandidatesFor =
          orderBinderCandidates
            (geDebugEnabled env)
            mbBindParentsGaInfo
            canonical
            constraint
            orderRootForBinders
            orderRootBaseForBinders
    binderPlan <-
      buildBinderPlan
        ( BinderPlanInput
          { bpiDebugEnabled = geDebugEnabled env,
            bpiConstraint = constraint,
            bpiNodes = nodes,
            bpiCanonical = canonical,
            bpiCanonKey = canonKey,
            bpiIsTyVarKey = isTyVarKey,
            bpiBindParents = bindParents,
            bpiRigidBindParents = bindParents0,
            bpiBindParentsGa = mbBindParentsGaInfo,
            bpiScopeRootC = scopeRootC,
            bpiScopeGen = scopeGen,
            bpiTarget0 = target0,
            bpiTargetBound = targetBound,
            bpiTargetIsSchemeRoot = targetIsSchemeRoot,
            bpiTargetIsBaseLike = targetIsBaseLike,
            bpiBoundUnderOtherGen = boundUnderOtherGen,
            bpiBinders0 = binders0,
            bpiNamedUnderGa = namedUnderGa,
            bpiGammaAlias = gammaAlias,
            bpiBaseGammaSet = baseGammaSet,
            bpiBaseGammaRep = baseGammaRep,
            bpiBaseGammaRepSet = baseGammaRepSet,
            bpiNamedUnderGaSet = namedUnderGaSet,
            bpiSolvedToBasePref = solvedToBasePref,
            bpiReachable = reachable,
            -- Binder membership must use the same graph domain selected for
            -- S'. A frozen base scheme can retain a source variable that is
            -- no longer reachable from its solved live root; project that
            -- exact base reachability through frozen base-to-solved
            -- provenance before filtering candidates.
            bpiReachableForBinders = reachableForBinderPlan,
            bpiReachableType = reachableType,
            bpiReachableTypeStructural = reachableTypeStructural,
            bpiEscapedFrozenForallBinders =
              escapedFrozenForallBinders,
            bpiEscapedSourceBinderOccurrences =
              escapedSourceBinderOccurrences,
            bpiTypeRoot0 = typeRoot0,
            bpiTypeRoot = typeRoot,
            bpiTypeRootFromBoundVar = typeRootFromBoundVar,
            bpiLiftToForall = liftToForall,
            bpiReachableFromWithBounds = reachableFromWithBounds,
            bpiResForReify = resForReify,
            bpiGammaKeyFor = gammaKeyFor,
            bpiNestedSchemeInteriorSet = nestedSchemeInteriorSet,
            bpiBoundIsSchemeRootVar = boundIsSchemeRootVar,
            bpiBoundIsSchemeRootAll = boundIsSchemeRootAll,
            bpiIsNestedSchemeBound = isNestedSchemeBound,
            bpiSchemeRootKeySet = schemeRootKeySet,
            bpiSchemeRootByBody = schemeRootByBody,
            bpiSchemeRootOwner = schemeRootOwner,
            bpiSchemeRootOwnerBase = schemeRootOwnerBase,
            bpiSchemeRootByBodyBase = schemeRootByBodyBase,
            bpiAliasBinderBases = aliasBinderBases,
            bpiOrderBinderCandidates = orderBinderCandidatesFor,
            bpiRequiredGamma = requiredGamma,
            bpiLocallyClosedGammaNodes = grLocallyClosedGammaNodes requirements,
            bpiSourceBinderRefs = sourceBinderRefs,
            bpiAmbientBinderRefs = grAmbientBinderRefs requirements,
            bpiTermUsedRootBinderRefs =
              grTermUsedRootBinderRefs requirements
          }
        )
    pure
      GeneralizePlan
        { gpEnv = env,
          gpContext = ctx,
          gpSchemeRootsPlan = schemeRootsPlan,
          gpTargetPlan = targetPlan,
          gpGammaPlan = gammaPlan,
          gpTypeRootPlan = typeRootPlan,
          gpBinderPlan = binderPlan,
          gpScopeSchemeRoots = scopeSchemeRoots,
          gpScopeHasStructuralScheme = scopeHasStructuralScheme,
          gpBinders0 = binders0,
          gpReachableFromWithBounds = reachableFromWithBounds,
          gpReachableFromStructural = reachableFromStructural,
          gpRigidBindParents = bindParents0,
          gpBindParents = bindParents
        }
