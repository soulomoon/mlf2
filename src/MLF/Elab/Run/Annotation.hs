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
import qualified Data.IntSet as IntSet
import MLF.Constraint.Presolution.Base
  ( EdgeArtifacts,
    eaIdentityEdges,
    edgeArtifactWitness,
    lookupEdgeArtifact,
  )
import MLF.Constraint.Types.Graph (NodeId (..), getEdgeId)
import MLF.Constraint.Types.Witness (ewLeft, ewRight)
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Frontend.ConstraintGen (AnnExpr (..))
import MLF.Frontend.ConstraintGen.Types
  ( AnnExprF (..),
    InstantiationSite (..),
    mapInstantiationSiteNodes,
  )
import MLF.Util.ElabError (ElabError (PhiInvariantError))
import MLF.Util.RecursionSchemes (cataM)

mapAnnNodes :: (NodeId -> NodeId) -> AnnExpr -> AnnExpr
mapAnnNodes f = mapAnn f (mapInstantiationSiteNodes f)

-- | Prepare every application site from the complete presolution packet that
-- owns its replay endpoints. Constraint normalization may insert a @TyExp@
-- wrapper and expansion may copy its left endpoint, so redirecting the source
-- expression node alone cannot construct this relation. A deliberately
-- simplified identity edge retains its redirected endpoints; every other
-- missing packet is an invariant failure instead of a silent stale-site path.
--
-- The allocation endpoints remain untouched and continue to own the original
-- application topology.
alignAnnInstantiationSites :: EdgeArtifacts -> AnnExpr -> Either ElabError AnnExpr
alignAnnInstantiationSites edgeArtifacts = cataM alignLayer
  where
    alignLayer ann = case ann of
      ALitF l nid -> pure (ALit l nid)
      AResolvedVarF details v nid -> pure (AResolvedVar details v nid)
      ALamF v details pNode x bodyAnn bodyEid nid ->
        pure (ALam v details pNode x bodyAnn bodyEid nid)
      AAppF fAnn argAnn funSite argSite nid -> do
        funSite' <- alignSite funSite
        argSite' <- alignSite argSite
        pure (AApp fAnn argAnn funSite' argSite' nid)
      ALetF v details schemeGenId schemeRootId ev rhsGen rhsAnn bodyAnn nid ->
        pure (ALet v details schemeGenId schemeRootId ev rhsGen rhsAnn bodyAnn nid)
      AExactAnnF exprAnn exactTy nid eid ->
        pure (AExactAnn exprAnn exactTy nid eid)
      AAnnF exprAnn nid eid -> pure (AAnn exprAnn nid eid)
      ALetScopeF exprAnn nid eid -> pure (ALetScope exprAnn nid eid)
      AUnfoldF exprAnn nid eid -> pure (AUnfold exprAnn nid eid)

    alignSite site =
      case lookupEdgeArtifact edgeId edgeArtifacts of
        Just artifact ->
          let witness = edgeArtifactWitness artifact
           in pure
                site
                  { instantiationSiteSource = ewLeft witness,
                    instantiationSiteTarget = ewRight witness
                  }
        Nothing
          | IntSet.member edgeKey (eaIdentityEdges edgeArtifacts) ->
              pure site
          | otherwise ->
              Left
                ( PhiInvariantError
                    ( "application instantiation site has neither a complete "
                        ++ "edge artifact nor identity-edge authority: "
                        ++ show edgeId
                    )
                )
      where
        edgeId = instantiationSiteEdgeId site
        edgeKey = getEdgeId edgeId

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
