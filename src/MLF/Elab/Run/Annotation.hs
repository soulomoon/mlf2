{-# LANGUAGE GADTs #-}

module MLF.Elab.Run.Annotation
  ( mapAnnNodes,
    alignAnnInstantiationSites,
    applyRedirectsToAnn,
    canonicalizeAnn,
    redirectAndCanonicalizeAnn,
    annNode,
  )
where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Types.Graph (NodeId (..), getEdgeId)
import MLF.Constraint.Types.Witness (EdgeWitness, ewLeft, ewRight)
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Frontend.ConstraintGen (AnnExpr (..))
import MLF.Frontend.ConstraintGen.Types
  ( AnnExprF (..),
    InstantiationSite (..),
    mapInstantiationSiteNodes,
  )

mapAnnNodes :: (NodeId -> NodeId) -> AnnExpr -> AnnExpr
mapAnnNodes f = mapAnn f (mapInstantiationSiteNodes f)

-- | Replace the prepared endpoints of every replay-owned application site
-- with the canonical endpoints carried by that edge's witness.  Constraint
-- normalization may insert a @TyExp@ wrapper and expansion may copy its left
-- endpoint, so redirecting the source expression node alone cannot construct
-- this relation.  The allocation endpoints remain untouched and continue to
-- own the original application topology.
alignAnnInstantiationSites :: IntMap.IntMap EdgeWitness -> AnnExpr -> AnnExpr
alignAnnInstantiationSites witnesses = mapAnn id alignSite
  where
    alignSite site =
      case IntMap.lookup (getEdgeId (instantiationSiteEdgeId site)) witnesses of
        Nothing -> site
        Just witness ->
          site
            { instantiationSiteSource = ewLeft witness,
              instantiationSiteTarget = ewRight witness
            }

mapAnn :: (NodeId -> NodeId) -> (InstantiationSite -> InstantiationSite) -> AnnExpr -> AnnExpr
mapAnn f mapSite = cata $ \case
  ALitF l nid -> ALit l (f nid)
  AResolvedVarF details v nid -> AResolvedVar details v (f nid)
  ALamF v details pNode x bodyAnn bodyEid nid ->
    ALam v details (f pNode) x bodyAnn bodyEid (f nid)
  AAppF fAnn argAnn funSite argSite nid ->
    AApp
      fAnn
      argAnn
      (mapSite funSite)
      (mapSite argSite)
      (f nid)
  ALetF v details schemeGenId schemeRootId ev rhsGen rhsAnn bodyAnn nid ->
    ALet v details schemeGenId (f schemeRootId) ev rhsGen rhsAnn bodyAnn (f nid)
  AExactAnnF exprAnn exactTy nid eid -> AExactAnn exprAnn exactTy (f nid) eid
  AAnnF exprAnn nid eid -> AAnn exprAnn (f nid) eid
  ALetScopeF exprAnn nid eid -> ALetScope exprAnn (f nid) eid
  AUnfoldF exprAnn nid eid -> AUnfold exprAnn (f nid) eid

applyRedirectsToAnn :: IntMap.IntMap NodeId -> AnnExpr -> AnnExpr
applyRedirectsToAnn redirects = mapAnnNodes (chaseRedirects redirects)

canonicalizeAnn :: (NodeId -> NodeId) -> AnnExpr -> AnnExpr
canonicalizeAnn canonical = mapAnnNodes canonical

redirectAndCanonicalizeAnn :: (NodeId -> NodeId) -> IntMap.IntMap NodeId -> AnnExpr -> AnnExpr
redirectAndCanonicalizeAnn canonical redirects =
  canonicalizeAnn canonical . applyRedirectsToAnn redirects

annNode :: AnnExpr -> NodeId
annNode = cata alg
  where
    alg ann = case ann of
      ALitF _ nid -> nid
      AResolvedVarF _ _ nid -> nid
      ALamF _ _ _ _ _ _ nid -> nid
      AAppF _ _ _ _ nid -> nid
      ALetF _ _ _ _ _ _ _ _ nid -> nid
      AAnnF _ nid _ -> nid
      ALetScopeF _ nid _ -> nid
      AUnfoldF _ nid _ -> nid
