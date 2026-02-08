{-# LANGUAGE GADTs #-}
module MLF.Elab.Run.Annotation (
    mapAnnNodes,
    applyRedirectsToAnn,
    canonicalizeAnn,
    annNode,
    adjustAnnotationInst
) where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap

import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Frontend.ConstraintGen.Types (AnnExprF(..))
import MLF.Constraint.Types.Graph (NodeId(..))
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Elab.Types (ElabType, Instantiation(..), Ty(..))

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

applyRedirectsToAnn :: IntMap.IntMap NodeId -> AnnExpr -> AnnExpr
applyRedirectsToAnn redirects = mapAnnNodes (chaseRedirects redirects)

canonicalizeAnn :: (NodeId -> NodeId) -> AnnExpr -> AnnExpr
canonicalizeAnn canonical = mapAnnNodes canonical

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

-- | Annotation instantiation should preserve quantifiers by updating bounds
-- rather than eliminating binders. Drop eliminations and treat applications
-- as inside-bound updates.
-- See Note [Annotation instantiation preserves foralls] in
-- docs/notes/2026-01-27-elab-changes.md.
adjustAnnotationInst :: Instantiation -> Instantiation
adjustAnnotationInst inst = case inst of
    InstApp ty -> InstInside (InstBot (sanitizeBoundTop ty))
    InstElim -> InstId
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

-- See Note [Instantiation arg sanitization] in
-- docs/notes/2026-01-27-elab-changes.md.
sanitizeBoundTop :: ElabType -> ElabType
sanitizeBoundTop ty = case ty of
    TVar _ -> TBottom
    _ -> ty
