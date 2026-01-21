module MLF.Elab.Run.Annotation (
    applyRedirectsToAnn,
    canonicalizeAnn,
    annNode,
    adjustAnnotationInst
) where

import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap

import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Frontend.ConstraintGen.Types (AnnExprF(..))
import MLF.Constraint.Types (NodeId(..))
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Elab.Types (Instantiation(..))

applyRedirectsToAnn :: IntMap.IntMap NodeId -> AnnExpr -> AnnExpr
applyRedirectsToAnn redirects = cata alg
  where
    redir = chaseRedirects redirects
    alg ann = case ann of
        ALitF l nid -> ALit l (redir nid)
        AVarF v nid -> AVar v (redir nid)
        ALamF v pNode x bodyAnn nid ->
            ALam v (redir pNode) x bodyAnn (redir nid)
        AAppF fAnn argAnn funEid argEid nid ->
            AApp fAnn argAnn funEid argEid (redir nid)
        ALetF v schemeGenId schemeRootId ev rhsGen rhsAnn bodyAnn nid ->
            ALet v schemeGenId schemeRootId ev rhsGen rhsAnn bodyAnn (redir nid)
        AAnnF exprAnn nid eid -> AAnn exprAnn (redir nid) eid

canonicalizeAnn :: (NodeId -> NodeId) -> AnnExpr -> AnnExpr
canonicalizeAnn canonical = cata alg
  where
    alg ann = case ann of
        ALitF l nid -> ALit l (canonical nid)
        AVarF v nid -> AVar v (canonical nid)
        ALamF v pNode x bodyAnn nid ->
            ALam v (canonical pNode) x bodyAnn (canonical nid)
        AAppF fAnn argAnn funEid argEid nid ->
            AApp fAnn argAnn funEid argEid (canonical nid)
        ALetF v schemeGenId schemeRootId ev rhsGen rhsAnn bodyAnn nid ->
            ALet v schemeGenId (canonical schemeRootId) ev rhsGen rhsAnn bodyAnn (canonical nid)
        AAnnF exprAnn nid eid -> AAnn exprAnn (canonical nid) eid

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
adjustAnnotationInst :: Instantiation -> Instantiation
adjustAnnotationInst inst = case inst of
    InstApp ty -> InstInside (InstBot ty)
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
