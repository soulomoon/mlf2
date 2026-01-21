{-# LANGUAGE LambdaCase #-}
module MLF.Elab.Inst (
    applyInstantiation,
    composeInst,
    instMany,
    schemeToType,
    splitForalls
) where

import Data.Functor.Foldable (para)

import MLF.Elab.Types
import MLF.Elab.TypeOps (freeTypeVarsType, freshTypeNameFromCounter, substTypeCapture)
import MLF.Elab.TypeOps (splitForalls)

-- | Turn a scheme into its corresponding type (nested `∀`).
schemeToType :: ElabScheme -> ElabType
schemeToType (Forall binds body) =
    foldr (\(v, b) t -> TForall v b t) body binds

composeInst :: Instantiation -> Instantiation -> Instantiation
composeInst InstId i = i
composeInst i InstId = i
composeInst i1 i2 = InstSeq i1 i2

instMany :: [Instantiation] -> Instantiation
instMany = foldr composeInst InstId

-- | Apply an xMLF instantiation to an xMLF type (xmlf Fig. 3).
--
-- This is a *partial* function: it fails if the instantiation expects a certain
-- type form (e.g. ∀ for `N`) but the type does not match.
applyInstantiation :: ElabType -> Instantiation -> Either ElabError ElabType
applyInstantiation ty inst = snd <$> go 0 ty inst
  where
    -- counter threads fresh names for `O`.
    go :: Int -> ElabType -> Instantiation -> Either ElabError (Int, ElabType)
    go k t i = case i of
        InstId -> Right (k, t)
        InstSeq i1 i2 -> do
            (k1, t1) <- go k t i1
            go k1 t1 i2

        -- Sugar: ⟨τ⟩ ≡ (∀(⩾ τ); N)
        InstApp argTy ->
            go k t (InstSeq (InstInside (InstBot argTy)) InstElim)

        -- Bottom instantiation: ⊥ τ = τ
        InstBot tArg -> case t of
            TBottom -> Right (k, tArg)
            _ | t == tArg -> Right (k, t)
            _ -> Left (InstantiationError ("InstBot expects ⊥, got: " ++ pretty t))

        -- Abstract bound: τ (!α) = α (no environment checking here; see xmlf §1.2)
        InstAbstr v -> Right (k, TVar v)

        -- Quantifier intro: τ O = ∀(α ⩾ ⊥). τ   (α fresh)
        InstIntro -> do
            let used = freeTypeVarsType t
                (fresh, k') = freshTypeNameFromCounter k used
            Right (k', TForall fresh Nothing t)

        -- Quantifier elim: (∀(α ⩾ τ) τ') N = τ'{α ← τ}
        InstElim -> case t of
            TForall v mbBound body -> do
                let bTy = maybe TBottom id mbBound
                Right (k, substTypeCapture v bTy body)
            _ -> Left (InstantiationError ("InstElim expects ∀, got: " ++ pretty t))

        -- Inside: (∀(α ⩾ τ) τ') (∀(⩾ φ)) = ∀(α ⩾ (τ φ)) τ'
        InstInside phi -> case t of
            TForall v mbBound body -> do
                let b0 = maybe TBottom id mbBound
                (k1, b1) <- go k b0 phi
                let mb' = if b1 == TBottom then Nothing else Just b1
                Right (k1, TForall v mb' body)
            _ -> Left (InstantiationError ("InstInside expects ∀, got: " ++ pretty t))

        -- Under: (∀(α ⩾ τ) τ') (∀(α ⩾) φ) = ∀(α ⩾ τ) (τ' φ)
        InstUnder vParam phi -> case t of
            TForall v mbBound body -> do
                let phi' = renameInstBound vParam v phi
                (k1, body') <- go k body phi'
                Right (k1, TForall v mbBound body')
            _ -> Left (InstantiationError ("InstUnder expects ∀, got: " ++ pretty t))

    -- free type variables (for freshness)
    -- Rename bound variable occurrences inside an instantiation body.
    -- This is α-renaming of the instantiation’s binder: occurrences of `old`
    -- are renamed to `new`, except under a nested `∀(old ⩾)` which re-binds it.
    renameInstBound :: String -> String -> Instantiation -> Instantiation
    renameInstBound old new = para alg
      where
        alg inst0 = case inst0 of
            InstIdF -> InstId
            InstAppF t -> InstApp t
            InstBotF t -> InstBot t
            InstIntroF -> InstIntro
            InstElimF -> InstElim
            InstAbstrF v -> InstAbstr (if v == old then new else v)
            InstInsideF i -> InstInside (snd i)
            InstSeqF a b -> InstSeq (snd a) (snd b)
            InstUnderF v i
                | v == old -> InstUnder v (fst i)  -- shadowing: stop renaming under this binder
                | otherwise -> InstUnder v (snd i)
