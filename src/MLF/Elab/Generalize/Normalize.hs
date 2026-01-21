module MLF.Elab.Generalize.Normalize (
    substType,
    simplifySchemeBindings,
    promoteArrowAlias,
    isBaseBound,
    isVarBound,
    containsForall,
    containsArrow
) where

import Data.Functor.Foldable (cata, para)
import qualified Data.Set as Set

import MLF.Elab.FreeNames (freeNamesFrom)
import MLF.Elab.Types

substType :: String -> ElabType -> ElabType -> ElabType
substType name replacement = para alg
  where
    alg ty = case ty of
        TVarF v
            | v == name -> replacement
            | otherwise -> TVar v
        TArrowF d c -> TArrow (snd d) (snd c)
        TBaseF b -> TBase b
        TBottomF -> TBottom
        TForallF v mb t'
            | v == name -> TForall v (fmap fst mb) (fst t')
            | otherwise -> TForall v (fmap snd mb) (snd t')

simplifySchemeBindings
    :: Bool
    -> Set.Set String
    -> [(String, Maybe ElabType)]
    -> ElabType
    -> ([(String, Maybe ElabType)], ElabType)
simplifySchemeBindings inlineBaseBounds namedBinders binds ty =
    let binders = Set.fromList (map fst binds)
    in simplify binders binds ty
  where
    simplify _ [] body = ([], body)
    simplify binders ((v, mbBound):rest) body =
        let isNamedBinder = Set.member v namedBinders
        in case mbBound of
            Nothing ->
                let (rest', body') = simplify binders rest body
                in ((v, Nothing) : rest', body')
            Just bound ->
                case body of
                    TVar v' | v' == v ->
                        let freeBound = freeNamesFrom Set.empty bound
                            boundMentionsSelf = Set.member v freeBound
                            boundDeps = Set.delete v freeBound
                            boundIsBase = isBaseBound bound
                            boundMentionsNamed =
                                not (Set.null (Set.intersection freeBound namedBinders))
                            boundIsAliasToBinder =
                                case bound of
                                    TVar v2 ->
                                        v2 /= v
                                            && Set.member v2 binders
                                            && not isNamedBinder
                                    _ -> False
                            canInlineAlias =
                                Set.null boundDeps
                                    && (not boundIsBase || inlineBaseBounds)
                                    && not isNamedBinder
                                    && not boundMentionsNamed
                                    || boundIsAliasToBinder
                        in if not boundMentionsSelf
                            && canInlineAlias
                            && not (containsForall bound)
                            && not (containsArrow bound)
                            then
                                let body' = bound
                                    restSub =
                                        [ (name, fmap (substType v bound) mb)
                                        | (name, mb) <- rest
                                        ]
                                in simplify (Set.delete v binders) restSub body'
                            else
                                let (rest', body') = simplify binders rest body
                                in ((v, Just bound) : rest', body')
                    _ ->
                        let freeBound = freeNamesFrom Set.empty bound
                            boundMentionsSelf = Set.member v freeBound
                            boundDeps = Set.delete v freeBound
                            dependsOnBinders =
                                not (Set.null (Set.intersection boundDeps (Set.delete v binders)))
                            boundMentionsNamed =
                                not (Set.null (Set.intersection freeBound namedBinders))
                            boundIsAliasToBinder =
                                case bound of
                                    TVar v2 ->
                                        v2 /= v
                                            && Set.member v2 binders
                                            && not isNamedBinder
                                    _ -> False
                            restUsesV =
                                Set.member v $
                                    Set.unions
                                        [ freeNamesFrom Set.empty b
                                        | (_, Just b) <- rest
                                        ]
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
                                    || boundIsAliasToBinder
                        in if not boundMentionsSelf
                            && (canInlineBase || canInlineNonBase)
                            then
                                let replacement = bound
                                    bodySub = substType v replacement body
                                    restSub =
                                        [ (name, fmap (substType v replacement) mb)
                                        | (name, mb) <- rest
                                        ]
                                in simplify binders restSub bodySub
                            else
                                let (rest', body') = simplify binders rest body
                                in ((v, Just bound) : rest', body')

promoteArrowAlias :: [(String, Maybe ElabType)] -> ElabType -> ([(String, Maybe ElabType)], ElabType)
promoteArrowAlias binds ty = case ty of
    TArrow (TVar v1) (TVar v2)
        | v1 == v2 ->
            case lookup v1 binds of
                Just (Just bnd)
                    | isBaseBound bnd || bnd == TBottom ->
                        let bnd' = TArrow bnd bnd
                            binds' = map (\(n, mb) -> if n == v1 then (n, Just bnd') else (n, mb)) binds
                        in (binds', TVar v1)
                _ -> (binds, ty)
    _ -> (binds, ty)

isBaseBound :: ElabType -> Bool
isBaseBound ty = case ty of
    TBase{} -> True
    TBottom -> True
    _ -> False

isVarBound :: ElabType -> Bool
isVarBound ty = case ty of
    TVar{} -> True
    _ -> False

containsForall :: ElabType -> Bool
containsForall = cata alg
  where
    alg ty = case ty of
        TForallF _ _ _ -> True
        TArrowF d c -> d || c
        _ -> False

containsArrow :: ElabType -> Bool
containsArrow = cata alg
  where
    alg ty = case ty of
        TArrowF _ _ -> True
        TForallF _ mb body ->
            let boundHasArrow = maybe False id mb
            in boundHasArrow || body
        _ -> False
