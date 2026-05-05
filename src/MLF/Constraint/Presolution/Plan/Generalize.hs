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
import Data.Maybe (listToMaybe)
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Plan.BinderPlan
  ( AliasEnv (..),
    BinderPlan (..),
    BinderPlanInput (..),
    BinderSelectionEnv (..),
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
import MLF.Constraint.Presolution.Plan.Context
  ( GaBindParents (..),
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
    softenBindParents,
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
    TypeRootPlan (..),
    TypeRootPlanInput (..),
    buildGammaPlan,
    buildTargetPlan,
    buildTypeRootPlan,
  )
import MLF.Constraint.Types.Graph hiding (lookupNode)
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Util.ElabError (ElabError (..), bindingToElab)
import MLF.Util.Graph (reachableFrom, reachableFromStop)
import qualified MLF.Util.IntMapUtils as IntMapUtils
import MLF.Util.Names (parseNameId)

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
    gpBindParents :: BindParents
  }

planGeneralizeAt :: PresolutionEnv p -> Either ElabError (GeneralizePlan p)
planGeneralizeAt
  PresolutionEnv
    { pePresolutionView = presolutionView,
      peBindParentsGa = mbBindParentsGa,
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
        isTyForallKey = geIsTyForallKey env
        isBaseLikeKey = geIsBaseLikeKey env
    bindParents0 <- bindingToElab (Binding.canonicalizeBindParentsUnder canonical constraint)
    let bindParentsSoft = softenBindParents canonical constraint bindParents0
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
          case scopeGen of
            Just gid ->
              listToMaybe
                [ canonical child
                | child <- IntMapUtils.typeChildrenOfGen bindParents gid,
                  isTyVarKey (canonKey child),
                  case VarStore.lookupVarBound constraint (canonical child) of
                    Just bnd -> canonical bnd == canonical target0
                    Nothing -> False
                ]
            Nothing -> Nothing
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
        childrenWithBounds =
          childrenWithBoundsWith nodes allowBoundTraversal
        reachableFromWithBounds root0 =
          reachableFromWithBoundsWith canonical nodes allowBoundTraversal root0
        childrenStructural nid =
          case lookupNodeInMap nodes nid of
            Nothing -> []
            Just node -> structuralChildren node
        reachableFromStructural root0 =
          reachableFrom getNodeId canonical childrenStructural root0

    reachable <- Right (reachableFromWithBounds orderRoot)
    let reachableForBinders0 =
          case mbBindParentsGa of
            Nothing -> reachable
            Just ga ->
              let baseConstraint = gaBaseConstraint ga
                  baseNodes = cNodes baseConstraint
                  baseNodesMap =
                    IntMap.fromList
                      [ (getNodeId nid, node)
                      | (nid, node) <- toListNode baseNodes
                      ]
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
                    reachableFromWithBoundsWith id baseNodesMap allowBoundTraversalBase root0
                  reachableBase = reachableFromWithBoundsBase orderRootBaseForBinders
                  reachableBaseSolved =
                    IntSet.fromList
                      [ getNodeId solvedNid
                      | baseKey <- IntSet.toList reachableBase,
                        Just solvedNid <- [IntMap.lookup baseKey (gaBaseToSolved ga)]
                      ]
               in IntSet.union reachable reachableBaseSolved
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
    binders0 <-
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
              shouldStop nid = IntSet.member (getNodeId nid) stopSet
           in reachableFromStop getNodeId canonical childrenWithBounds shouldStop root0
        nestedSchemeInteriorSet =
          IntSet.unions
            [ reachableFromWithBoundsStop (NodeId rootKey)
            | rootKey <- IntSet.toList schemeRootSkipSet
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
    let gammaPlan =
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
                gpiFirstGenAncestor = firstGenAncestorGa
              }
        GammaPlan
          { gpBaseGammaSet = baseGammaSet,
            gpBaseGammaRep = baseGammaRep,
            gpNamedUnderGaSet = namedUnderGaSet,
            gpSolvedToBasePref = solvedToBasePref,
            gpGammaAlias = gammaAlias,
            gpBaseGammaRepSet = baseGammaRepSet,
            gpReachableForBinders = reachableForBinders,
            gpGammaKeyFor = gammaKeyFor,
            gpNamedUnderGa = namedUnderGa,
            gpTypeRootHasNamedOutsideGamma = typeRootHasNamedOutsideGamma
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
                trpiTypeRoot0 = typeRoot0,
                trpiTypeRootFromBoundVar = typeRootFromBoundVar,
                trpiTypeRootHasNamedOutsideGamma = typeRootHasNamedOutsideGamma,
                trpiBoundHasForallForVar = boundHasForallForVar,
                trpiSchemeRootByBody = schemeRootByBody,
                trpiSchemeRootOwner = schemeRootOwner,
                trpiLiftToForall = liftToForall
              }
        TypeRootPlan
          { trTargetIsBaseLike = targetIsBaseLike,
            trTypeRoot = typeRoot
          } = typeRootPlan
    reachableType <- Right (reachableFromWithBounds typeRoot)
    reachableTypeStructural <- Right (reachableFromStructural typeRoot)
    let typeRootIsForall =
          isTyForallKey (canonKey typeRoot)
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
        BinderPlanInput
          { bpiDebugEnabled = geDebugEnabled env,
            bpiConstraint = constraint,
            bpiNodes = nodes,
            bpiCanonical = canonical,
            bpiCanonKey = canonKey,
            bpiIsTyVarKey = isTyVarKey,
            bpiBindParents = bindParents,
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
            bpiReachableForBinders = reachableForBinders,
            bpiReachableType = reachableType,
            bpiReachableTypeStructural = reachableTypeStructural,
            bpiTypeRoot0 = typeRoot0,
            bpiTypeRoot = typeRoot,
            bpiTypeRootFromBoundVar = typeRootFromBoundVar,
            bpiTypeRootIsForall = typeRootIsForall,
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
            bpiSchemeRootOwnerBase = schemeRootOwnerBase,
            bpiSchemeRootByBodyBase = schemeRootByBodyBase,
            bpiAliasBinderBases = aliasBinderBases,
            bpiParseNameId = parseNameId,
            bpiOrderBinderCandidates = orderBinderCandidatesFor
          }
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
          gpBindParents = bindParents
        }
