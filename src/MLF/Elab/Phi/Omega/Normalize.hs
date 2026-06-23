{- |
Module      : MLF.Elab.Phi.Omega.Normalize
Description : Omega instantiation normalization helpers
-}
module MLF.Elab.Phi.Omega.Normalize
    ( normalizeInst
    , collapseAdjacentPairs
    ) where

import Data.Functor.Foldable (Recursive (project), cata)

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
                _
                    | Just (underRef, underA, underB) <- matchingUnderPair a b ->
                        let inner = case (underA, underB) of
                                (InstInside (InstBot t), InstElim) -> InstApp t
                                _ -> InstSeq underA underB
                        in instUnderWithRef underRef inner
                -- Rule 2: Structural intro-elim cancellation with matching binder identity.
                (_, InstElim)
                    | Just t <- introUnderAbstrElimArg a ->
                        InstApp t
                -- Rule 3: Prefix-arg collapse. When a prefix instantiation carries the
                -- same arg type as the inner app, the prefix is redundant and the whole
                -- sequence reduces to InstApp tArg.
                -- REVIEW: This relies on alpha-equality of arg types as a proxy for
                -- semantic equivalence. Sound when the prefix and inner app originate
                -- from the same constraint-graph edge (guaranteed by Phi translation),
                -- but could over-collapse if two independent instantiation paths happen
                -- to share the same arg type. Audit if Phi translation changes.
                (InstSeq prefix inner, InstElim)
                        | Just appArg <- introUnderAbstrElimAppArg inner
                        , Just tPrefix <- instArgTy prefix
                        , Just tArg <- instArgTy appArg
                        , alphaEqType tPrefix tArg ->
                            InstApp tArg
                (InstId, x) -> x
                (x, InstId) -> x
                _ -> InstSeq a b
        InstInsideF a -> InstInside a
        InstUnderFRef ref a -> instUnderWithRef ref a
        InstAppF t -> InstApp t
        InstBotF t -> InstBot t
        InstAbstrFRef ref -> instAbstrWithRef ref
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
tryCollapse a b = case (project a, project b) of
    (InstInsideF (InstBot t), InstElimF) -> Just (InstApp t)
    (InstUnderFRef ref1 innerA, InstUnderFRef ref2 innerB)
        | sameBinderRef ref1 ref2 -> instUnderWithRef ref1 <$> tryCollapse innerA innerB
    _ -> Nothing

matchingUnderPair :: Instantiation -> Instantiation -> Maybe (TypeBinderRef, Instantiation, Instantiation)
matchingUnderPair a b = case (project a, project b) of
    (InstUnderFRef ref1 innerA, InstUnderFRef ref2 innerB)
        | sameBinderRef ref1 ref2 -> Just (ref1, innerA, innerB)
    _ -> Nothing

sameBinderRef :: TypeBinderRef -> TypeBinderRef -> Bool
sameBinderRef = typeBinderRefsSameIdentity

introUnderAbstrElimArg :: Instantiation -> Maybe ElabType
introUnderAbstrElimArg inst = do
    appArg <- introUnderAbstrElimAppArg inst
    case appArg of
        InstInside (InstBot t) -> Just t
        _ -> Nothing

introUnderAbstrElimAppArg :: Instantiation -> Maybe Instantiation
introUnderAbstrElimAppArg inst = case project inst of
    InstSeqF InstIntro rest -> underAbstrElimAppArg rest
    _ -> Nothing

underAbstrElimAppArg :: Instantiation -> Maybe Instantiation
underAbstrElimAppArg inst = case project inst of
    InstSeqF appArg underElim
        | Just () <- underAbstrElim underElim -> Just appArg
    _ -> Nothing

underAbstrElim :: Instantiation -> Maybe ()
underAbstrElim inst = case project inst of
    InstUnderFRef underRef inner ->
        case project inner of
            InstSeqF (InstInside abstr) InstElim ->
                case project abstr of
                    InstAbstrFRef abstrRef
                        | sameBinderRef underRef abstrRef -> Just ()
                    _ -> Nothing
            _ -> Nothing
    _ -> Nothing
