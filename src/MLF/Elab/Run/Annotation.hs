{-# LANGUAGE GADTs #-}

module MLF.Elab.Run.Annotation
  ( mapAnnNodes,
    applyRedirectsToAnn,
    canonicalizeAnn,
    redirectAndCanonicalizeAnn,
    annNode,
    adjustAnnotationInst,
  )
where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Types.Graph (NodeId (..))
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Elab.Types (Instantiation (..))
import MLF.Frontend.ConstraintGen (AnnExpr (..))
import MLF.Frontend.ConstraintGen.Types (AnnExprF (..))

mapAnnNodes :: (NodeId -> NodeId) -> AnnExpr -> AnnExpr
mapAnnNodes f = cata $ \case
  ALitF l nid -> ALit l (f nid)
  AVarF v nid -> AVar v (f nid)
  ALamF v pNode x bodyAnn nid ->
    ALam v (f pNode) x bodyAnn (f nid)
  AAppF fAnn argAnn funEid argEid nid ->
    AApp fAnn argAnn funEid argEid (f nid)
  ALetF v schemeGenId schemeRootId ev rhsGen rhsAnn bodyAnn nid ->
    ALet v schemeGenId (f schemeRootId) ev rhsGen rhsAnn bodyAnn (f nid)
  AAnnF exprAnn nid eid -> AAnn exprAnn (f nid) eid
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
      AVarF _ nid -> nid
      ALamF _ _ _ _ nid -> nid
      AAppF _ _ _ _ nid -> nid
      ALetF _ _ _ _ _ _ _ nid -> nid
      AAnnF _ nid _ -> nid
      AUnfoldF _ nid _ -> nid

-- | Annotation instantiation should preserve quantifiers by updating bounds
-- rather than eliminating binders. Drop eliminations and treat applications
-- as inside-bound updates.
-- See Note [Annotation instantiation preserves foralls] in
-- docs/notes/2026-01-27-elab-changes.md.
adjustAnnotationInst :: Instantiation -> Instantiation
adjustAnnotationInst inst = case inst of
  InstElim -> InstId
  InstApp ty -> InstInside (InstBot ty)
  InstBot ty -> InstInside (InstBot ty)
  InstSeq (InstInside (InstBot ty)) InstElim -> InstInside (InstBot ty)
  InstSeq a b ->
    let a' = adjustAnnotationInst a
        b' = adjustAnnotationInst b
     in case (a', b') of
          (InstId, x) -> x
          (x, InstId) -> x
          _ -> InstSeq a' b'
  InstInside a -> InstInside (adjustAnnotationInst a)
  InstUnder v a -> InstUnder v (adjustAnnotationInst a)
  _ -> inst
