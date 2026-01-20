{-# LANGUAGE LambdaCase #-}
module MLF.Elab.Inst (
    applyInstantiation,
    composeInst,
    instMany,
    schemeToType,
    splitForalls
) where

import Data.Functor.Foldable (cata, para)
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
splitForalls = para alg
  where
    alg ty = case ty of
        TForallF v mb bodyPair ->
            let mbOrig = fmap fst mb
                (binds, body) = snd bodyPair
            in ((v, mbOrig) : binds, body)
        TVarF v -> ([], TVar v)
        TArrowF (d, _) (c, _) -> ([], TArrow d c)
        TBaseF b -> ([], TBase b)
        TBottomF -> ([], TBottom)

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
    ftvType = nub . cata alg
      where
        alg ty0 = case ty0 of
            TVarF v -> [v]
            TArrowF a b -> a ++ b
            TBaseF _ -> []
            TBottomF -> []
            TForallF v mb body ->
                let fvBound = maybe [] id mb
                    fvBody = filter (/= v) body
                in fvBody ++ fvBound

    freshName :: Int -> [String] -> (String, Int)
    freshName n used =
        let candidate = "u" ++ show n
        in if candidate `elem` used then freshName (n + 1) used else (candidate, n + 1)

    -- Capture-avoiding substitution [x ↦ s]t (only for types).
    substType :: String -> ElabType -> ElabType -> ElabType
    substType x s = goSub
      where
        freeS = ftvType s

        goSub = para alg
          where
            alg node = case node of
                TVarF v
                    | v == x -> s
                    | otherwise -> TVar v
                TArrowF d c -> TArrow (snd d) (snd c)
                TBaseF b -> TBase b
                TBottomF -> TBottom
                TForallF v mb body
                    | v == x ->
                        let mb' = fmap snd mb
                        in TForall v mb' (fst body)
                    | v `elem` freeS ->
                        let used =
                                freeS
                                    ++ ftvType (fst body)
                                    ++ maybe [] (ftvType . fst) mb
                            v' = pickFresh v used
                            body' = substType v (TVar v') (fst body)
                        in TForall v' (fmap snd mb) (goSub body')
                    | otherwise ->
                        TForall v (fmap snd mb) (snd body)

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
