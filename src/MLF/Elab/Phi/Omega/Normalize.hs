{- |
Module      : MLF.Elab.Phi.Omega.Normalize
Description : Omega instantiation normalization helpers
-}
module MLF.Elab.Phi.Omega.Normalize
    ( normalizeInst
    , collapseAdjacentPairs
    ) where

import Data.Functor.Foldable (cata)

import MLF.Elab.Types
import MLF.Reify.TypeOps (alphaEqType)

-- | Normalize an instantiation by collapsing redundant sequences.
-- Extracted as a top-level function for testability.
normalizeInst :: Instantiation -> Instantiation
normalizeInst = cata alg
  where
    instArgTy :: Instantiation -> Maybe ElabType
    instArgTy inst0 = case inst0 of
        InstInside (InstBot t) -> Just t
        InstApp t -> Just t
        _ -> Nothing

    alg inst = case inst of
        InstSeqF a b ->
            case (a, b) of
                -- Rule 1: Thesis 14.2.1 identity — InstApp t ≡ InstSeq (InstInside (InstBot t)) InstElim
                (InstInside (InstBot t), InstElim) -> InstApp t
                -- Rule 1b: Context-wrapped graft+weaken — same collapse under matching InstUnder
                (InstUnder v1 underA, InstUnder v2 underB)
                    | v1 == v2 ->
                        let inner = case (underA, underB) of
                                (InstInside (InstBot t), InstElim) -> InstApp t
                                _ -> InstSeq underA underB
                        in InstUnder v1 inner
                -- Rule 2: Structural intro-elim cancellation with matching binder names.
                -- The intro/under/abstr/elim sequence is an identity when the binder
                -- names match, so the whole sequence collapses to InstApp t.
                ( InstSeq InstIntro (InstSeq (InstInside (InstBot t)) (InstUnder beta (InstSeq (InstInside (InstAbstr beta')) InstElim)))
                    , InstElim
                    )
                        | beta == beta' ->
                            InstApp t
                -- Rule 3: Prefix-arg collapse. When a prefix instantiation carries the
                -- same arg type as the inner app, the prefix is redundant and the whole
                -- sequence reduces to InstApp tArg.
                -- REVIEW: This relies on alpha-equality of arg types as a proxy for
                -- semantic equivalence. Sound when the prefix and inner app originate
                -- from the same constraint-graph edge (guaranteed by Phi translation),
                -- but could over-collapse if two independent instantiation paths happen
                -- to share the same arg type. Audit if Phi translation changes.
                ( InstSeq
                    prefix
                    (InstSeq InstIntro (InstSeq appArg (InstUnder beta (InstSeq (InstInside (InstAbstr beta')) InstElim))))
                    , InstElim
                    )
                        | beta == beta'
                        , Just tPrefix <- instArgTy prefix
                        , Just tArg <- instArgTy appArg
                        , alphaEqType tPrefix tArg ->
                            InstApp tArg
                (InstId, x) -> x
                (x, InstId) -> x
                _ -> InstSeq a b
        InstInsideF a -> InstInside a
        InstUnderF v a -> InstUnder v a
        InstAppF t -> InstApp t
        InstBotF t -> InstBot t
        InstAbstrF v -> InstAbstr v
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstIdF -> InstId

-- | Collapse adjacent graft+weaken pairs in a flat instantiation list.
-- When an @InstInside(InstBot t)@ is immediately followed by @InstElim@
-- (possibly wrapped in matching @InstUnder@ contexts), collapse them to
-- @InstApp t@ per thesis Def. 14.2.1.
collapseAdjacentPairs :: [Instantiation] -> [Instantiation]
collapseAdjacentPairs [] = []
collapseAdjacentPairs [x] = [x]
collapseAdjacentPairs (a : b : rest) =
    case tryCollapse a b of
        Just collapsed -> collapseAdjacentPairs (collapsed : rest)
        Nothing -> a : collapseAdjacentPairs (b : rest)

tryCollapse :: Instantiation -> Instantiation -> Maybe Instantiation
tryCollapse (InstInside (InstBot t)) InstElim = Just (InstApp t)
tryCollapse (InstUnder v1 a) (InstUnder v2 b)
    | v1 == v2 = InstUnder v1 <$> tryCollapse a b
tryCollapse _ _ = Nothing
