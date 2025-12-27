{- |
Module      : MLF.Constraint.Unify.Decompose
Description : Shared one-step structural unification decomposition

This module centralizes the *pure* "head-constructor" decomposition used by
unification-like phases.

Given two term-DAG nodes that are intended to be unified, we can sometimes
decompose the equality into a list of child equalities (e.g. arrow dom/cod).
This helper is intentionally oblivious to the binding tree: phases that need
additional side conditions (notably ∀ binder-arity checks) must enforce those
separately.
 -}
module MLF.Constraint.Unify.Decompose (
    DecomposeMismatch(..),
    decomposeUnifyChildren
) where

import MLF.Constraint.Types (BaseTy, ExpVarId, TyNode(..), UnifyEdge(..))

-- | Structural mismatch reasons for a single head-constructor decomposition.
data DecomposeMismatch
    = MismatchConstructor
    | MismatchBase BaseTy BaseTy
    | MismatchExpVar ExpVarId ExpVarId
    deriving (Eq, Show)

-- | Decompose a "same-head" equality into a list of child equalities.
--
-- This is a single-step decomposition only; recursive processing belongs in
-- the caller.
decomposeUnifyChildren :: TyNode -> TyNode -> Either DecomposeMismatch [UnifyEdge]
decomposeUnifyChildren n1 n2 = case (n1, n2) of
    (TyArrow { tnDom = d1, tnCod = c1 }, TyArrow { tnDom = d2, tnCod = c2 }) ->
        Right [UnifyEdge d1 d2, UnifyEdge c1 c2]

    (TyForall { tnBody = b1 }, TyForall { tnBody = b2 }) ->
        Right [UnifyEdge b1 b2]

    (TyBase { tnBase = b1 }, TyBase { tnBase = b2 })
        | b1 == b2 -> Right []
        | otherwise -> Left (MismatchBase b1 b2)

    (TyExp { tnExpVar = s1, tnBody = b1 }, TyExp { tnExpVar = s2, tnBody = b2 })
        | s1 == s2 -> Right [UnifyEdge b1 b2]
        | otherwise -> Left (MismatchExpVar s1 s2)

    _ ->
        Left MismatchConstructor

