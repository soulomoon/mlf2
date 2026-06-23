module MLF.Elab.Sigma (
    bubbleReorderTo,
    bubbleReorderToFromSpineRefs,
    sigmaReorder
) where

import Data.List (elemIndex)

import MLF.Elab.Types
import MLF.Elab.Inst (applyInstantiation, composeInst, instMany)
import MLF.Reify.TypeOps (splitForallsRefs)

swapFrontRefs :: (TypeBinderRef, Maybe BoundType) -> (TypeBinderRef, Maybe BoundType) -> Instantiation
swapFrontRefs (a, mbTa) (b, mbTb) =
    -- xmlf §3.4 “Reordering quantifiers”:
    --   O; ∀(⩾ τα); O; ∀(⩾ τβ); ∀(β ⩾) ∀(α ⩾) h!αi; h!βi
    --
    -- We keep binder refs symbolic; `applyInstantiation` α-renames under-binders.
    let ta = maybe TBottom tyToElab mbTa
        tb = maybe TBottom tyToElab mbTb
        hAbs ref = InstSeq (InstInside (instAbstrWithRef ref)) InstElim
    in instMany
        [ InstIntro
        , InstInside (InstBot ta)
        , InstIntro
        , InstInside (InstBot tb)
        , instUnderWithRef b (instUnderWithRef a (InstSeq (hAbs a) (hAbs b)))
        ]

-- | Swap quantifiers at depth i and i+1 (0-based) by applying `swapFront`
-- under the first i binders.
swapAt :: Int -> ElabType -> Either ElabError Instantiation
swapAt i ty = case (i, ty) of
    (0, TForallRef a ta (TForallRef b tb _)) ->
        Right (swapFrontRefs (a, ta) (b, tb))
    (n, TForallRef ref _ body) | n > 0 ->
        instUnderWithRef ref <$> swapAt (n - 1) body
    _ ->
        Left (InstantiationError ("swapAt: cannot swap at depth " ++ show i ++ " in type " ++ pretty ty))

swapAdjacent :: Int -> [a] -> [a]
swapAdjacent i xs =
    let (pre, rest) = splitAt i xs
    in case rest of
        (a : b : rs) -> pre ++ (b : a : rs)
        _ -> xs

checkedReorderBinderPair :: String -> [a] -> [a] -> Int -> Either ElabError (Maybe (a, a))
checkedReorderBinderPair context sourceIds desiredIds idx =
    case drop idx desiredIds of
        [] -> Right Nothing
        desiredBinder : _ ->
            case drop idx sourceIds of
                currentBinder : _ -> Right (Just (currentBinder, desiredBinder))
                [] ->
                    Left
                        ( InstantiationError
                            (context ++ ": type has only " ++ show (length sourceIds) ++ " binders")
                        )

bubbleReorderTo
    :: Eq a
    => String
    -> ElabType
    -> [a]
    -> [a]
    -> Either ElabError (Instantiation, ElabType, [a])
bubbleReorderTo context ty0 ids0 desired0 = go InstId ty0 ids0 0
  where
    go acc ty ids idx = do
        step <- checkedReorderBinderPair context ids desired0 idx
        case step of
            Nothing -> Right (acc, ty, ids)
            Just (currentBinder, desiredBinder) ->
                if currentBinder == desiredBinder
                    then go acc ty ids (idx + 1)
                    else case elemIndex desiredBinder (drop idx ids) of
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

bubbleReorderToFromSpineRefs
    :: Eq a
    => String
    -> [(TypeBinderRef, Maybe BoundType)]
    -> [a]
    -> [a]
    -> Either ElabError (Instantiation, [(TypeBinderRef, Maybe BoundType)], [a])
bubbleReorderToFromSpineRefs context binders0 ids0 desired0 = go InstId binders0 ids0 0
  where
    go acc binders ids idx = do
        step <- checkedReorderBinderPair context ids desired0 idx
        case step of
            Nothing -> Right (acc, binders, ids)
            Just (currentBinder, desiredBinder) ->
                if currentBinder == desiredBinder
                    then go acc binders ids (idx + 1)
                    else case elemIndex desiredBinder (drop idx ids) of
                        Nothing ->
                            Left (InstantiationError (context ++ ": desired binder not found in source"))
                        Just off -> do
                            let k = idx + off
                            (acc', binders', ids') <- bubbleLeft acc binders ids k idx
                            go acc' binders' ids' (idx + 1)

    bubbleLeft acc binders ids k idx
        | k <= idx = Right (acc, binders, ids)
        | otherwise = do
            sw <- swapAtFromSpineRefs (k - 1) binders
            let binders' = swapAdjacent (k - 1) binders
                ids' = swapAdjacent (k - 1) ids
            bubbleLeft (composeInst acc sw) binders' ids' (k - 1) idx

swapAtFromSpineRefs :: Int -> [(TypeBinderRef, Maybe BoundType)] -> Either ElabError Instantiation
swapAtFromSpineRefs i binders = case drop i binders of
    (a : b : _) -> Right (underPrefix (take i binders) (swapFrontRefs a b))
    _ -> Left (InstantiationError ("swapAtFromSpine: cannot swap at depth " ++ show i))
  where
    underPrefix [] inst = inst
    underPrefix ((ref, _) : rest) inst = instUnderWithRef ref (underPrefix rest inst)

-- | Reorder the leading quantifier spine of `src` so its binder order matches `tgt`.
-- Returns the instantiation Σ that performs the reordering.
sigmaReorder :: ElabType -> ElabType -> Either ElabError Instantiation
sigmaReorder src tgt =
    let (srcQs, _) = splitForallsRefs src
        (tgtQs, _) = splitForallsRefs tgt
        srcIds = map (binderOrderKey . fst) srcQs
        desired = map (binderOrderKey . fst) tgtQs
    in sigmaReorderTo src srcIds desired

-- | Reorder the leading quantifiers of a type to a desired binder *identity* order
-- using adjacent swaps (bubble-style), producing a Σ instantiation.
--
-- Important: applying the commutation instantiations introduces fresh binder names,
-- so we must *not* use the post-swap binder names for bookkeeping. Instead we track
-- the intended binder identities in a separate list (`ids`) and update it as we swap.
sigmaReorderTo :: Eq a => ElabType -> [a] -> [a] -> Either ElabError Instantiation
sigmaReorderTo ty0 ids0 desired = do
    (sig, _ty1, _ids1) <- bubbleReorderTo "sigmaReorder" ty0 ids0 desired
    pure sig

binderOrderKey :: TypeBinderRef -> TypeBinderIdentity
binderOrderKey =
    typeBinderRefIdentity
