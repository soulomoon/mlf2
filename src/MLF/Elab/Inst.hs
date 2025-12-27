{-# LANGUAGE LambdaCase #-}
module MLF.Elab.Inst (
    applyInstantiation,
    composeInst,
    instMany,
    schemeToType,
    splitForalls
) where

import Data.List (nub)

import MLF.Elab.Types

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

splitForalls :: ElabType -> ([(String, Maybe ElabType)], ElabType)
splitForalls = \case
    TForall v b t -> let (qs, body) = splitForalls t in ((v, b) : qs, body)
    t -> ([], t)

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
            _ -> Left (InstantiationError ("InstBot expects ⊥, got: " ++ pretty t))

        -- Abstract bound: τ (!α) = α (no environment checking here; see xmlf §1.2)
        InstAbstr v -> Right (k, TVar v)

        -- Quantifier intro: τ O = ∀(α ⩾ ⊥). τ   (α fresh)
        InstIntro -> do
            let used = ftvType t
                (fresh, k') = freshName k used
            Right (k', TForall fresh Nothing t)

        -- Quantifier elim: (∀(α ⩾ τ) τ') N = τ'{α ← τ}
        InstElim -> case t of
            TForall v mbBound body -> do
                let bTy = maybe TBottom id mbBound
                Right (k, substType v bTy body)
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
    ftvType :: ElabType -> [String]
    ftvType = nub . goF
      where
        goF ty0 = case ty0 of
            TVar v -> [v]
            TArrow a b -> goF a ++ goF b
            TBase _ -> []
            TBottom -> []
            TForall v mb b ->
                let fvB = goF b
                    fvBound = maybe [] goF mb
                in filter (/= v) fvB ++ fvBound

    freshName :: Int -> [String] -> (String, Int)
    freshName n used =
        let candidate = "u" ++ show n
        in if candidate `elem` used then freshName (n + 1) used else (candidate, n + 1)

    -- Capture-avoiding substitution [x ↦ s]t (only for types).
    substType :: String -> ElabType -> ElabType -> ElabType
    substType x s t0 = case t0 of
        TVar v | v == x -> s
        TVar v -> TVar v
        TArrow a b -> TArrow (substType x s a) (substType x s b)
        TBase b -> TBase b
        TBottom -> TBottom
        TForall v mb body
            | v == x -> TForall v (fmap (substType x s) mb) body
            | v `elem` ftvType s ->
                let v' = pickFresh v (ftvType s ++ ftvType body ++ maybe [] ftvType mb)
                    body' = substType v (TVar v') body
                in TForall v' (fmap (substType x s) mb) (substType x s body')
            | otherwise ->
                TForall v (fmap (substType x s) mb) (substType x s body)

    pickFresh :: String -> [String] -> String
    pickFresh base used =
        let cands = base : [base ++ show i | i <- [(1::Int)..]]
        in case filter (`notElem` used) cands of
            (x:_) -> x
            [] -> base  -- unreachable (cands is infinite)

    -- Rename bound variable occurrences inside an instantiation body.
    -- This is α-renaming of the instantiation’s binder: occurrences of `old`
    -- are renamed to `new`, except under a nested `∀(old ⩾)` which re-binds it.
    renameInstBound :: String -> String -> Instantiation -> Instantiation
    renameInstBound old new = goR
      where
        goR inst0 = case inst0 of
            InstId -> InstId
            InstApp t -> InstApp t
            InstBot t -> InstBot t
            InstIntro -> InstIntro
            InstElim -> InstElim
            InstAbstr v -> InstAbstr (if v == old then new else v)
            InstInside i -> InstInside (goR i)
            InstSeq a b -> InstSeq (goR a) (goR b)
            InstUnder v i
                | v == old -> InstUnder v i  -- shadowing: stop renaming under this binder
                | otherwise -> InstUnder v (goR i)
