module MLF.Elab.Sigma (
    bubbleReorderTo,
    sigmaReorder
) where

import Data.List (elemIndex)

import MLF.Elab.Types
import MLF.Elab.Inst (applyInstantiation, composeInst, instMany, splitForalls)

-- | Generate the (paper) commutation instantiation that swaps the first two
-- quantifiers of a type ∀(α ⩾ τα). ∀(β ⩾ τβ). τ.
swapFront :: (String, Maybe BoundType) -> (String, Maybe BoundType) -> Instantiation
swapFront (_a, mbTa) (_b, mbTb) =
    -- xmlf §3.4 “Reordering quantifiers”:
    --   O; ∀(⩾ τα); O; ∀(⩾ τβ); ∀(β ⩾) ∀(α ⩾) h!αi; h!βi
    --
    -- We keep binder names symbolic; `applyInstantiation` α-renames under-binders.
    let ta = maybe TBottom tyToElab mbTa
        tb = maybe TBottom tyToElab mbTb
        hAbs v = InstSeq (InstInside (InstAbstr v)) InstElim
    in instMany
        [ InstIntro
        , InstInside (InstBot ta)
        , InstIntro
        , InstInside (InstBot tb)
        , InstUnder "β" (InstUnder "α" (InstSeq (hAbs "α") (hAbs "β")))
        ]

-- | Swap quantifiers at depth i and i+1 (0-based) by applying `swapFront`
-- under the first i binders.
swapAt :: Int -> ElabType -> Either ElabError Instantiation
swapAt i ty = case (i, ty) of
    (0, TForall _a ta (TForall _b tb _)) ->
        Right (swapFront (_a, ta) (_b, tb))
    (n, TForall v _ body) | n > 0 ->
        InstUnder v <$> swapAt (n - 1) body
    _ ->
        Left (InstantiationError ("swapAt: cannot swap at depth " ++ show i ++ " in type " ++ pretty ty))

swapAdjacent :: Int -> [a] -> [a]
swapAdjacent i xs =
    let (pre, rest) = splitAt i xs
    in case rest of
        (a : b : rs) -> pre ++ (b : a : rs)
        _ -> xs

bubbleReorderTo
    :: Eq a
    => String
    -> ElabType
    -> [a]
    -> [a]
    -> Either ElabError (Instantiation, ElabType, [a])
bubbleReorderTo context ty0 ids0 desired0 = go InstId ty0 ids0 0
  where
    go acc ty ids idx
        | idx >= length desired0 = Right (acc, ty, ids)
        | length ids < length desired0 =
            Left (InstantiationError (context ++ ": type has only " ++ show (length ids) ++ " binders"))
        | ids !! idx == desired0 !! idx = go acc ty ids (idx + 1)
        | otherwise =
            case elemIndex (desired0 !! idx) (drop idx ids) of
                Nothing ->
                    Left (InstantiationError (context ++ ": desired binder not found in source"))
                Just off -> do
                    let k = idx + off
                    (acc', ty', ids') <- bubbleLeft acc ty ids k idx
                    go acc' ty' ids' (idx + 1)

    bubbleLeft acc ty ids k idx
        | k <= idx = Right (acc, ty, ids)
        | otherwise = do
            sw <- swapAt (k - 1) ty
            ty' <- applyInstantiation ty sw
            let ids' = swapAdjacent (k - 1) ids
            bubbleLeft (composeInst acc sw) ty' ids' (k - 1) idx

-- | Reorder the leading quantifier spine of `src` so its binder order matches `tgt`.
-- Returns the instantiation Σ that performs the reordering.
sigmaReorder :: ElabType -> ElabType -> Either ElabError Instantiation
sigmaReorder src tgt =
    let (srcQs, _) = splitForalls src
        (tgtQs, _) = splitForalls tgt
        srcIds = map fst srcQs
        desired = map fst tgtQs
    in sigmaReorderTo src srcIds desired

-- | Reorder the leading quantifiers of a type to a desired binder *identity* order
-- using adjacent swaps (bubble-style), producing a Σ instantiation.
--
-- Important: applying the commutation instantiations introduces fresh binder names,
-- so we must *not* use the post-swap binder names for bookkeeping. Instead we track
-- the intended binder identities in a separate list (`ids`) and update it as we swap.
sigmaReorderTo :: ElabType -> [String] -> [String] -> Either ElabError Instantiation
sigmaReorderTo ty0 ids0 desired = do
    (sig, _ty1, _ids1) <- bubbleReorderTo "sigmaReorder" ty0 ids0 desired
    pure sig
