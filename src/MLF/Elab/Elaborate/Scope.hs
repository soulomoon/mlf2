{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

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
import MLF.Constraint.Presolution (PresolutionView)
import MLF.Constraint.Types
  ( NodeId,
    NodeRef (..),
    getNodeId,
    typeRef,
  )
import MLF.Elab.Generalize (GaBindParents (..))
import MLF.Elab.Inst (schemeToType)
import qualified MLF.Elab.Inst as Inst
import qualified MLF.Elab.Run.ChiQuery as ChiQuery
import MLF.Elab.Run.Scope (generalizeTargetNode, schemeBodyTarget)
import MLF.Elab.Run.TypeOps (inlineBoundVarsType)
import MLF.Elab.Types
  ( ElabError,
    ElabScheme,
    ElabType,
    SchemeInfo (..),
    schemeFromType,
  )
import MLF.Reify.Core (namedNodes, reifyTypeWithNamedSetNoFallback)
import MLF.Reify.TypeOps (inlineBaseBoundsType, parseNameId)

type GeneralizeAtWith =
  Maybe GaBindParents ->
  NodeRef ->
  NodeId ->
  Either ElabError (ElabScheme, IntMap.IntMap String)

data ScopeContext = ScopeContext
  { scPresolutionView :: PresolutionView,
    scGaParents :: GaBindParents,
    scScopeOverrides :: IntMap.IntMap NodeRef,
    scGeneralizeAtWith :: GeneralizeAtWith
  }

scopeRootForNode :: ScopeContext -> NodeId -> Either ElabError NodeRef
scopeRootForNode scopeContext nodeId =
  case IntMap.lookup (getNodeId (canonical nodeId)) (scScopeOverrides scopeContext) of
    Just ref -> pure ref
    Nothing -> scopeRootFromBase scopeContext nodeId
  where
    canonical = ChiQuery.chiCanonical presolutionView
    presolutionView = scPresolutionView scopeContext

scopeRootFromBase :: ScopeContext -> NodeId -> Either ElabError NodeRef
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
    canonical = ChiQuery.chiCanonical presolutionView

generalizeAtNode :: ScopeContext -> NodeId -> Either ElabError (ElabScheme, IntMap.IntMap String)
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

reifyNodeTypeDirect :: ScopeContext -> NodeId -> Either ElabError ElabType
reifyNodeTypeDirect scopeContext nodeId = do
  namedSet <- namedNodes presolutionView
  reifyTypeForParam presolutionView namedSet (canonical nodeId)
  where
    presolutionView = scPresolutionView scopeContext
    canonical = ChiQuery.chiCanonical presolutionView

reifyNodeTypePreferringBound :: ScopeContext -> NodeId -> Either ElabError ElabType
reifyNodeTypePreferringBound scopeContext nodeId = do
  namedSet <- namedNodes presolutionView
  let nodeC = canonical nodeId
  case ChiQuery.chiLookupVarBound presolutionView nodeC of
    Just bnd -> reifyTypeForParam presolutionView namedSet bnd
    Nothing -> reifyTypeForParam presolutionView namedSet nodeC
  where
    presolutionView = scPresolutionView scopeContext
    canonical = ChiQuery.chiCanonical presolutionView

reifyTargetType :: ScopeContext -> IntSet.IntSet -> SchemeInfo -> NodeId -> Either ElabError ElabType
reifyTargetType scopeContext namedSetReify schemeInfo nodeId =
  let presolutionView = scPresolutionView scopeContext
      subst = siSubst schemeInfo
      targetNode = schemeBodyTarget presolutionView nodeId
   in reifyTypeWithNamedSetNoFallback presolutionView subst namedSetReify targetNode

reifyTargetNodeType :: ScopeContext -> IntSet.IntSet -> SchemeInfo -> NodeId -> Either ElabError ElabType
reifyTargetNodeType scopeContext namedSetReify schemeInfo nodeId =
  let presolutionView = scPresolutionView scopeContext
      canonical = ChiQuery.chiCanonical presolutionView
      subst = siSubst schemeInfo
      targetNode = canonical nodeId
   in reifyTypeWithNamedSetNoFallback presolutionView subst namedSetReify targetNode

reifyTypeForParam :: PresolutionView -> IntSet.IntSet -> NodeId -> Either ElabError ElabType
reifyTypeForParam presolutionView namedSet nodeId = do
  ty <- reifyTypeWithNamedSetNoFallback presolutionView IntMap.empty namedSet nodeId
  let ty' = inlineBaseBounds presolutionView ty
  pure (inlineBoundVarsType presolutionView ty')

inlineBaseBounds :: PresolutionView -> ElabType -> ElabType
inlineBaseBounds presolutionView =
  inlineBaseBoundsType
    (ChiQuery.chiConstraint presolutionView)
    (ChiQuery.chiCanonical presolutionView)
