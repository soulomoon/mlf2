{-# LANGUAGE GADTs #-}
module MLF.Constraint.Presolution.Plan.Normalize (
    substType,
    simplifySchemeBindings,
    promoteArrowAlias,
    isBaseBound,
    isVarBound,
    containsForall,
    containsArrow
) where

import qualified Data.Set as Set

import MLF.Reify.TypeOps (freeTypeVarsFrom, freeTypeVarsType, substTypeSimple)
import MLF.Types.Elab
    ( BoundType
    , ElabType
    , Ty(..)
    , TyIF(..)
    , K(..)
    , cataIxConst
    , tyToElab
    )

substType :: String -> ElabType -> ElabType -> ElabType
substType = substTypeSimple

simplifySchemeBindings
    :: Bool
    -> Set.Set String
    -> [(String, Maybe BoundType)]
    -> ElabType
    -> ([(String, Maybe BoundType)], ElabType)
simplifySchemeBindings inlineBaseBounds namedBinders binds ty =
    let binders = Set.fromList (map fst binds)
    in simplify binders binds ty
  where
    simplify
        :: Set.Set String
        -> [(String, Maybe BoundType)]
        -> ElabType
        -> ([(String, Maybe BoundType)], ElabType)
    simplify _ [] body = ([], body)
    simplify binders ((v, mbBound):rest) body =
        let isNamedBinder = Set.member v namedBinders
        in case mbBound of
            Nothing ->
                let (rest', body') = simplify binders rest body
                in ((v, Nothing) : rest', body')
            Just bound ->
                let boundElab = tyToElab bound
                    bodyUsesV = Set.member v (freeTypeVarsFrom Set.empty body)
                    restUsesV =
                        Set.member v $
                            Set.unions
                                [ freeTypeVarsType b
                                | (_, Just b) <- rest
                                ]
                in if not bodyUsesV && not restUsesV
                    then simplify (Set.delete v binders) rest body
                    else case body of
                    TVar v' | v' == v ->
                        let freeBound = freeTypeVarsFrom Set.empty bound
                            boundMentionsSelf = Set.member v freeBound
                            boundDeps = Set.delete v freeBound
                            boundIsBase = isBaseBound bound
                            boundIsVar = isVarBound bound
                            boundMentionsNamed =
                                not (Set.null (Set.intersection freeBound namedBinders))
                            canInlineAliasSimple =
                                Set.null boundDeps
                                    && (not boundIsBase || inlineBaseBounds)
                                    && not isNamedBinder
                                    && not boundMentionsNamed
                            canInlineStructured =
                                not boundIsBase
                                    && not boundIsVar
                                    && not isNamedBinder
                        in if not boundMentionsSelf
                            && (canInlineAliasSimple || canInlineStructured)
                            then
                                let body' = boundElab
                                    restSub =
                                        [ (name, fmap (substBound v boundElab) mb)
                                        | (name, mb) <- rest
                                        ]
                                in simplify (Set.delete v binders) restSub body'
                            else
                                let (rest', body') = simplify binders rest body
                                in ((v, Just bound) : rest', body')
                    _ ->
                        let freeBound = freeTypeVarsFrom Set.empty bound
                            boundMentionsSelf = Set.member v freeBound
                            boundDeps = Set.delete v freeBound
                            dependsOnBinders =
                                not (Set.null (Set.intersection boundDeps (Set.delete v binders)))
                            boundMentionsNamed =
                                not (Set.null (Set.intersection freeBound namedBinders))
                            canInlineBase =
                                inlineBaseBounds
                                    && not dependsOnBinders
                                    && not restUsesV
                                    && isBaseBound bound
                                    && not boundMentionsNamed
                            canInlineNonBase =
                                not dependsOnBinders
                                    && not (isBaseBound bound)
                                    && isVarBound bound
                                    && not isNamedBinder
                                    && not boundMentionsNamed
                        in if not boundMentionsSelf
                            && (canInlineBase || canInlineNonBase)
                            then
                                let replacement = boundElab
                                    bodySub = substType v replacement body
                                    restSub =
                                        [ (name, fmap (substBound v replacement) mb)
                                        | (name, mb) <- rest
                                        ]
                                in simplify binders restSub bodySub
                            else
                                let (rest', body') = simplify binders rest body
                                in ((v, Just bound) : rest', body')

promoteArrowAlias :: [(String, Maybe BoundType)] -> ElabType -> ([(String, Maybe BoundType)], ElabType)
promoteArrowAlias binds ty = case ty of
    TArrow (TVar v1) (TVar v2)
        | v1 == v2 ->
            case lookup v1 binds of
                Just (Just bnd)
                    | isBaseBound bnd || bnd == TBottom ->
                        let bnd' = TArrow (tyToElab bnd) (tyToElab bnd)
                            binds' = map (\(n, mb) -> if n == v1 then (n, Just bnd') else (n, mb)) binds
                        in (binds', TVar v1)
                _ -> (binds, ty)
    _ -> (binds, ty)

substBound :: String -> ElabType -> BoundType -> BoundType
substBound v replacement bound = case bound of
    TArrow a b ->
        TArrow (substType v replacement a) (substType v replacement b)
    TBase b -> TBase b
    TBottom -> TBottom
    TForall name mb body
        | name == v ->
            let mb' = fmap (substBound v replacement) mb
            in TForall name mb' body
        | otherwise ->
            let mb' = fmap (substBound v replacement) mb
            in TForall name mb' (substType v replacement body)

isBaseBound :: Ty v -> Bool
isBaseBound ty = case ty of
    TBase{} -> True
    TBottom -> True
    _ -> False

isVarBound :: Ty v -> Bool
isVarBound ty = case ty of
    TVar{} -> True
    _ -> False

containsForall :: ElabType -> Bool
containsForall = cataIxConst alg
  where
    alg ty = case ty of
        TForallIF _ _ _ -> True
        TArrowIF d c -> unK d || unK c
        _ -> False

containsArrow :: ElabType -> Bool
containsArrow = cataIxConst alg
  where
    alg ty = case ty of
        TArrowIF _ _ -> True
        TForallIF _ mb body ->
            let boundHasArrow = maybe False unK mb
            in boundHasArrow || unK body
        _ -> False
