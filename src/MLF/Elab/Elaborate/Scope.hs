{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module MLF.Elab.Elaborate.Scope
  ( GeneralizeAtWith,
    ScopeContext (..),
    generalizeAtNode,
    normalizeSchemeSubstPair,
    normalizeSubstForScheme,
    reifyNodeTypeDirect,
    reifyNodeTypePreferringBound,
    reifyTargetType,
    reifyTargetNodeType,
    scopeRootForNode,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (listToMaybe)
import MLF.Constraint.BindingUtil (bindingPathToRootLocal)
import MLF.Constraint.Presolution (PresolutionView (..))
import MLF.Constraint.Types.Graph
  ( NodeId,
    NodeRef (..),
    getNodeId,
    typeRef,
  )
import MLF.Constraint.Types.Phase (Phase)
import MLF.Elab.Generalize (GaBindParents (..))
import MLF.Elab.Inst (schemeToType)
import qualified MLF.Elab.Inst as Inst
import MLF.Elab.ReadModel (ElabReadModel)
import MLF.Elab.Run.Scope (generalizeTargetNode, schemeBodyTarget)
import MLF.Elab.Run.TypeOps (InlineBoundVarsContext, inlineBoundVarsTypeWithContext)
import MLF.Elab.Types
  ( ElabError,
    ElabScheme,
    ElabType,
    SchemeInfo (..),
    schemeFromType,
  )
import MLF.Reify.Core (reifyTypeWithNamedSetNoFallbackReadModel)
import MLF.Reify.TypeOps (inlineBaseBoundsType, parseNameId)

type GeneralizeAtWith (p :: Phase) =
  Maybe (GaBindParents p) ->
  NodeRef ->
  NodeId ->
  Either ElabError (ElabScheme, IntMap.IntMap String)

data ScopeContext (p :: Phase) = ScopeContext
  { scPresolutionView :: PresolutionView p,
    scGaParents :: GaBindParents p,
    scScopeOverrides :: IntMap.IntMap NodeRef,
    scGeneralizeAtWith :: GeneralizeAtWith p,
    scReadModel :: ElabReadModel p,
    scNamedSetReify :: IntSet.IntSet,
    scInlineBoundVarsContext :: InlineBoundVarsContext p
  }

scopeRootForNode :: ScopeContext p -> NodeId -> Either ElabError NodeRef
scopeRootForNode scopeContext nodeId =
  case IntMap.lookup (getNodeId (canonical nodeId)) (scScopeOverrides scopeContext) of
    Just ref -> pure ref
    Nothing -> scopeRootFromBase scopeContext nodeId
  where
    canonical = pvCanonical presolutionView
    presolutionView = scPresolutionView scopeContext

scopeRootFromBase :: ScopeContext p -> NodeId -> Either ElabError NodeRef
scopeRootFromBase scopeContext root =
  case IntMap.lookup (getNodeId (canonical root)) (gaSolvedToBase gaParents) of
    Nothing -> pure (typeRef root)
    Just baseNode -> do
      path <- bindingPathToRootLocal (gaBindParentsBase gaParents) (typeRef baseNode)
      pure $ case listToMaybe [gid | GenRef gid <- drop 1 path] of
        Just gid -> GenRef gid
        Nothing -> typeRef root
  where
    presolutionView = scPresolutionView scopeContext
    gaParents = scGaParents scopeContext
    canonical = pvCanonical presolutionView

generalizeAtNode :: ScopeContext p -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
generalizeAtNode scopeContext nodeId = do
  scopeRoot <- scopeRootForNode scopeContext nodeId
  let targetC = generalizeTargetNode presolutionView nodeId
  scGeneralizeAtWith scopeContext (Just (scGaParents scopeContext)) scopeRoot targetC
  where
    presolutionView = scPresolutionView scopeContext

normalizeSchemeSubstPair :: (ElabScheme, IntMap.IntMap String) -> (ElabScheme, IntMap.IntMap String)
normalizeSchemeSubstPair (schemeRaw, substRaw) =
  let scheme = schemeFromType (schemeToType schemeRaw)
      subst = normalizeSubstForScheme scheme substRaw
   in (scheme, subst)

normalizeSubstForScheme :: ElabScheme -> IntMap.IntMap String -> IntMap.IntMap String
normalizeSubstForScheme scheme substRaw =
  let (binds, _) = Inst.splitForalls (schemeToType scheme)
   in foldl'
        ( \acc (name, _) ->
            if name `elem` IntMap.elems acc
              then acc
              else case parseNameId name of
                Just nid -> IntMap.insertWith (\_ old -> old) nid name acc
                Nothing -> acc
        )
        substRaw
        binds

reifyNodeTypeDirect :: ScopeContext p -> NodeId -> Either ElabError ElabType
reifyNodeTypeDirect scopeContext nodeId = do
  reifyTypeForParam scopeContext (canonical nodeId)
  where
    presolutionView = scPresolutionView scopeContext
    canonical = pvCanonical presolutionView

reifyNodeTypePreferringBound :: ScopeContext p -> NodeId -> Either ElabError ElabType
reifyNodeTypePreferringBound scopeContext nodeId = do
  let nodeC = canonical nodeId
  case pvLookupVarBound presolutionView nodeC of
    Just bnd -> reifyTypeForParam scopeContext bnd
    Nothing -> reifyTypeForParam scopeContext nodeC
  where
    presolutionView = scPresolutionView scopeContext
    canonical = pvCanonical presolutionView

reifyTargetType :: ScopeContext p -> IntSet.IntSet -> SchemeInfo -> NodeId -> Either ElabError ElabType
reifyTargetType scopeContext namedSetReify schemeInfo nodeId =
  let presolutionView = scPresolutionView scopeContext
      subst = siSubst schemeInfo
      targetNode = schemeBodyTarget presolutionView nodeId
   in reifyTypeWithNamedSetNoFallbackReadModel (scReadModel scopeContext) subst namedSetReify targetNode

reifyTargetNodeType :: ScopeContext p -> IntSet.IntSet -> SchemeInfo -> NodeId -> Either ElabError ElabType
reifyTargetNodeType scopeContext namedSetReify schemeInfo nodeId =
  let presolutionView = scPresolutionView scopeContext
      canonical = pvCanonical presolutionView
      subst = siSubst schemeInfo
      targetNode = canonical nodeId
   in reifyTypeWithNamedSetNoFallbackReadModel (scReadModel scopeContext) subst namedSetReify targetNode

reifyTypeForParam :: ScopeContext p -> NodeId -> Either ElabError ElabType
reifyTypeForParam scopeContext nodeId = do
  ty <- reifyTypeWithNamedSetNoFallbackReadModel (scReadModel scopeContext) IntMap.empty namedSet nodeId
  let ty' = inlineBaseBounds presolutionView ty
  pure (inlineBoundVarsTypeWithContext (scInlineBoundVarsContext scopeContext) ty')
  where
    presolutionView = scPresolutionView scopeContext
    namedSet = scNamedSetReify scopeContext

inlineBaseBounds :: PresolutionView p -> ElabType -> ElabType
inlineBaseBounds presolutionView =
  inlineBaseBoundsType
    (pvConstraint presolutionView)
    (pvCanonical presolutionView)
