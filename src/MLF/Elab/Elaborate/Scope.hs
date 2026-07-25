{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module MLF.Elab.Elaborate.Scope
  ( GeneralizeAtWith,
    GeneralizeAtWithRequirements,
    GeneralizeAtWithResultCertificate,
    ScopeContext (..),
    generalizeAtNode,
    generalizeAtNodeWithRequirements,
    normalizeSchemeSubstPair,
    normalizeSubstForScheme,
    reifyNodeTypeDirect,
    reifyNodeTypePreferringBound,
    reifyTargetType,
    reifyTargetNodeType,
    scopeRootForNode,
    scopeRootForBoundary,
    scopeTypeBinderIdentityRepresentative,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Constraint.Presolution (PresolutionView (..))
import MLF.Constraint.Presolution.Plan.Context
  ( SolvedToBaseResolution (..),
    resolveGaSolvedToBase,
  )
import MLF.Constraint.Presolution.Plan.Requirements (GeneralizationRequirements)
import MLF.Constraint.Types.Graph
  ( EdgeId,
    NodeId,
    NodeRef (..),
    getNodeId,
  )
import MLF.Constraint.Types.Phase (Phase)
import MLF.Elab.Generalize
  ( GaBindParents (..),
    GeneralizedResultRoute,
    GeneralizedResultRouteRequest,
  )
import MLF.Elab.Inst (schemeToType)
import MLF.Elab.ReadModel (ElabReadModel)
import MLF.Elab.Run.Scope
  ( ConstructionScopes,
    generalizeTargetNode,
    resolveConstructionScopeForBoundary,
    resolveConstructionScopeForNode,
    schemeBodyTarget,
  )
import MLF.Elab.Run.TypeOps (InlineBoundVarsContext, inlineBoundVarsTypeWithContext)
import MLF.Elab.Types
  ( ElabError (..),
    ElabScheme,
    ElabType,
    SchemeInfo (..),
    TypeBinderRef,
    schemeBinderRefs,
    schemeInfoBinderRefSubst,
    schemeFromType,
    typeBinderRefNode,
    typeBinderRefsSameIdentity,
  )
import MLF.Reify.Type (reifyTypeWithNamedSetRefsNoFallbackReadModel)
import MLF.Reify.TypeOps (inlineBaseBoundsType)

type GeneralizeAtWith (p :: Phase) =
  Maybe (GaBindParents p) ->
  NodeRef ->
  NodeId ->
  Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)

type GeneralizeAtWithRequirements (p :: Phase) =
  GeneralizationRequirements ->
  Maybe (GaBindParents p) ->
  NodeRef ->
  NodeId ->
  Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)

type GeneralizeAtWithResultCertificate (p :: Phase) =
  GeneralizedResultRouteRequest ->
  GeneralizationRequirements ->
  Maybe (GaBindParents p) ->
  NodeRef ->
  NodeId ->
  Either
    ElabError
    ( ElabScheme,
      IntMap.IntMap TypeBinderRef,
      GeneralizedResultRoute
    )

data ScopeContext (p :: Phase) = ScopeContext
  { scPresolutionView :: PresolutionView p,
    scCanonical :: NodeId -> NodeId,
    scGaParents :: GaBindParents p,
    scScopeOverrides :: ConstructionScopes,
    scGeneralizeAtWith :: GeneralizeAtWith p,
    scGeneralizeAtWithRequirements :: GeneralizeAtWithRequirements p,
    scGeneralizeAtWithResultCertificate ::
      GeneralizeAtWithResultCertificate p,
    scReadModel :: ElabReadModel p,
    scNamedSetReify :: IntSet.IntSet,
    scInlineBoundVarsContext :: InlineBoundVarsContext p
  }

scopeRootForNode :: ScopeContext p -> NodeId -> Either ElabError NodeRef
scopeRootForNode scopeContext nodeId =
  resolveConstructionScopeForNode
    (scCanonical scopeContext)
    (scGaParents scopeContext)
    (scScopeOverrides scopeContext)
    nodeId

-- | Resolve an exact source-constructor occurrence before using its result
-- node. Boundary edges remain unique when canonicalization merges result nodes
-- from distinct applications and nested lets.
scopeRootForBoundary
  :: ScopeContext p
  -> EdgeId
  -> NodeId
  -> Either ElabError NodeRef
scopeRootForBoundary scopeContext edgeId fallbackNode =
  resolveConstructionScopeForBoundary
    (scCanonical scopeContext)
    (scGaParents scopeContext)
    (scScopeOverrides scopeContext)
    edgeId
    fallbackNode

-- | Project graph refs onto the base identity class used by generalization.
-- Redirect/UF canonicalization alone cannot identify distinct solved copies
-- of one lexical binder; 'gaSolvedToBase' retains that source provenance.
scopeTypeBinderIdentityRepresentative :: ScopeContext p -> NodeId -> NodeId
scopeTypeBinderIdentityRepresentative scopeContext nodeId =
  case resolveGaSolvedToBase (scGaParents scopeContext) nodeC of
    SolvedToBaseMapped baseNode -> baseNode
    SolvedToBaseSameDomain baseNode -> baseNode
    SolvedToBaseMissing -> nodeC
  where
    nodeC = scCanonical scopeContext nodeId

generalizeAtNode :: ScopeContext p -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)
generalizeAtNode scopeContext nodeId = do
  scopeRoot <- scopeRootForNode scopeContext nodeId
  let targetC = generalizeTargetNode presolutionView (scCanonical scopeContext nodeId)
  scGeneralizeAtWith scopeContext (Just (scGaParents scopeContext)) scopeRoot targetC
  where
    presolutionView = scPresolutionView scopeContext

generalizeAtNodeWithRequirements
  :: ScopeContext p
  -> GeneralizationRequirements
  -> NodeId
  -> Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)
generalizeAtNodeWithRequirements scopeContext requirements nodeId = do
  scopeRoot <- scopeRootForNode scopeContext nodeId
  let targetC = generalizeTargetNode presolutionView (scCanonical scopeContext nodeId)
  scGeneralizeAtWithRequirements scopeContext
    requirements
    (Just (scGaParents scopeContext))
    scopeRoot
    targetC
  where
    presolutionView = scPresolutionView scopeContext

normalizeSchemeSubstPair :: (ElabScheme, IntMap.IntMap TypeBinderRef) -> (ElabScheme, IntMap.IntMap TypeBinderRef)
normalizeSchemeSubstPair (schemeRaw, substRaw) =
  let scheme = schemeFromType (schemeToType schemeRaw)
      subst = normalizeSubstForScheme scheme substRaw
   in (scheme, subst)

normalizeSubstForScheme :: ElabScheme -> IntMap.IntMap TypeBinderRef -> IntMap.IntMap TypeBinderRef
normalizeSubstForScheme scheme substRaw =
  let refKey ref =
        case typeBinderRefNode ref of
          Just node -> Just (getNodeId node)
          Nothing -> Nothing
   in foldl'
        ( \acc ref ->
            if any (typeBinderRefsSameIdentity ref) (IntMap.elems acc)
              then acc
              else case refKey ref of
                Just nid -> IntMap.insertWith (\_ old -> old) nid ref acc
                Nothing -> acc
        )
        substRaw
        (map fst (schemeBinderRefs scheme))

reifyNodeTypeDirect :: ScopeContext p -> NodeId -> Either ElabError ElabType
reifyNodeTypeDirect scopeContext nodeId = do
  reifyTypeForParam scopeContext (canonical nodeId)
  where
    canonical = scCanonical scopeContext

reifyNodeTypePreferringBound :: ScopeContext p -> NodeId -> Either ElabError ElabType
reifyNodeTypePreferringBound scopeContext nodeId = do
  let nodeC = canonical nodeId
  case pvLookupVarBound presolutionView nodeC of
    Just bnd -> reifyTypeForParam scopeContext bnd
    Nothing -> reifyTypeForParam scopeContext nodeC
  where
    presolutionView = scPresolutionView scopeContext
    canonical = scCanonical scopeContext

reifyTargetType :: ScopeContext p -> IntSet.IntSet -> SchemeInfo -> NodeId -> Either ElabError ElabType
reifyTargetType scopeContext namedSetReify schemeInfo nodeId =
  let presolutionView = scPresolutionView scopeContext
      subst = schemeInfoBinderRefSubst schemeInfo
      targetNode = schemeBodyTarget presolutionView (scCanonical scopeContext nodeId)
   in reifyTypeWithNamedSetRefsNoFallbackReadModel (scReadModel scopeContext) subst namedSetReify targetNode

reifyTargetNodeType :: ScopeContext p -> IntSet.IntSet -> SchemeInfo -> NodeId -> Either ElabError ElabType
reifyTargetNodeType scopeContext namedSetReify schemeInfo nodeId =
  let canonical = scCanonical scopeContext
      subst = schemeInfoBinderRefSubst schemeInfo
      targetNode = canonical nodeId
   in reifyTypeWithNamedSetRefsNoFallbackReadModel (scReadModel scopeContext) subst namedSetReify targetNode

reifyTypeForParam :: ScopeContext p -> NodeId -> Either ElabError ElabType
reifyTypeForParam scopeContext nodeId = do
  ty <- reifyTypeWithNamedSetRefsNoFallbackReadModel (scReadModel scopeContext) IntMap.empty namedSet nodeId
  let ty' = inlineBaseBounds (scCanonical scopeContext) presolutionView ty
  pure (inlineBoundVarsTypeWithContext (scInlineBoundVarsContext scopeContext) ty')
  where
    presolutionView = scPresolutionView scopeContext
    namedSet = scNamedSetReify scopeContext

inlineBaseBounds :: (NodeId -> NodeId) -> PresolutionView p -> ElabType -> ElabType
inlineBaseBounds canonical presolutionView =
  inlineBaseBoundsType
    (pvConstraint presolutionView)
    canonical
