{-# LANGUAGE GADTs #-}
module MLF.Constraint.Presolution.Plan.Normalize (
    substTypeRef,
    simplifySchemeBindingsRefs,
    promoteArrowAliasRefs,
    isBaseBound,
    isVarBound,
    containsForall,
    containsArrow
) where

import MLF.Reify.TypeOps
    ( composeTypeHeadRef
    , freeTypeVarRefsFrom
    , freeTypeVarRefsType
    , substTypeSimpleRef
    )
import MLF.Types.Elab
    ( BoundType
    , ElabType
    , TypeBinderRef
    , Ty(..)
    , TyIF(..)
    , K(..)
    , cataIxConst
    , typeBinderRefsSameIdentity
    , tyToElab
    )

substTypeRef :: TypeBinderRef -> ElabType -> ElabType -> ElabType
substTypeRef = substTypeSimpleRef

simplifySchemeBindingsRefs
    :: Bool
    -> [TypeBinderRef]
    -> [(TypeBinderRef, Maybe BoundType)]
    -> ElabType
    -> ([(TypeBinderRef, Maybe BoundType)], ElabType)
simplifySchemeBindingsRefs inlineBaseBounds namedBinders binds ty =
    let binders = map fst binds
    in simplify binders binds ty
  where
    simplify
        :: [TypeBinderRef]
        -> [(TypeBinderRef, Maybe BoundType)]
        -> ElabType
        -> ([(TypeBinderRef, Maybe BoundType)], ElabType)
    simplify _ [] body = ([], body)
    simplify binders ((ref, mbBound):rest) body =
        let isNamedBinder = refMember ref namedBinders
        in case mbBound of
            Nothing ->
                let (rest', body') = simplify binders rest body
                in ((ref, Nothing) : rest', body')
            Just bound ->
                let boundElab = tyToElab bound
                    bodyUsesV = refMember ref (freeTypeVarRefsFrom [] body)
                    restUsesV =
                        any
                            (refMember ref)
                                [ freeTypeVarRefsType b
                                | (_, Just b) <- rest
                                ]
                in if not bodyUsesV && not restUsesV
                    then simplify (deleteRef ref binders) rest body
                    else case body of
                    TVarRef bodyRef | typeBinderRefsSameIdentity bodyRef ref ->
                        let freeBound = freeTypeVarRefsFrom [] bound
                            boundMentionsSelf = refMember ref freeBound
                            boundDeps = deleteRef ref freeBound
                            boundIsBase = isBaseBound bound
                            boundIsVar = isVarBound bound
                            boundMentionsNamed =
                                any (`refMember` namedBinders) freeBound
                            canInlineAliasSimple =
                                null boundDeps
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
                                        [ (name, fmap (substBoundRef ref boundElab) mb)
                                        | (name, mb) <- rest
                                        ]
                                in simplify (deleteRef ref binders) restSub body'
                            else
                                let (rest', body') = simplify binders rest body
                                in ((ref, Just bound) : rest', body')
                    _ ->
                        let freeBound = freeTypeVarRefsFrom [] bound
                            boundMentionsSelf = refMember ref freeBound
                            boundDeps = deleteRef ref freeBound
                            dependsOnBinders =
                                let remainingBinders = deleteRef ref binders
                                in any (`refMember` remainingBinders) boundDeps
                            boundMentionsNamed =
                                any (`refMember` namedBinders) freeBound
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
                            canInlineStructured =
                                not (isBaseBound bound)
                                    && not (isVarBound bound)
                                    && not isNamedBinder
                        in if not boundMentionsSelf
                            && (canInlineBase || canInlineNonBase || canInlineStructured)
                            then
                                let replacement = boundElab
                                    bodySub = substTypeRef ref replacement body
                                    restSub =
                                        [ (name, fmap (substBoundRef ref replacement) mb)
                                        | (name, mb) <- rest
                                        ]
                                in simplify binders restSub bodySub
                            else
                                let (rest', body') = simplify binders rest body
                                in ((ref, Just bound) : rest', body')

promoteArrowAliasRefs :: [(TypeBinderRef, Maybe BoundType)] -> ElabType -> ([(TypeBinderRef, Maybe BoundType)], ElabType)
promoteArrowAliasRefs binds ty = case ty of
    TArrow (TVarRef ref1) (TVarRef ref2)
        | typeBinderRefsSameIdentity ref1 ref2 ->
            case lookupRef ref1 binds of
                Just (Just bnd)
                    | isBaseBound bnd || bnd == TBottom ->
                        let bnd' = TArrow (tyToElab bnd) (tyToElab bnd)
                            binds' = map (\(ref, mb) -> if typeBinderRefsSameIdentity ref ref1 then (ref, Just bnd') else (ref, mb)) binds
                        in (binds', TVarRef ref1)
                _ -> (binds, ty)
    _ -> (binds, ty)

substBoundRef :: TypeBinderRef -> ElabType -> BoundType -> BoundType
substBoundRef ref replacement bound = case bound of
    TArrow a b ->
        TArrow (substTypeRef ref replacement a) (substTypeRef ref replacement b)
    TCon c args -> TCon c (fmap (substTypeRef ref replacement) args)
    TVarAppRef headRef args ->
        let args' = fmap (substTypeRef ref replacement) args
        in if typeBinderRefsSameIdentity headRef ref
            then composeTypeHeadRef headRef replacement args'
            else TVarAppRef headRef args'
    TBase b -> TBase b
    TBottom -> TBottom
    TForallRef binderRef mb body
        | typeBinderRefsSameIdentity binderRef ref ->
            let mb' = fmap (substBoundRef ref replacement) mb
            in TForallRef binderRef mb' body
        | otherwise ->
            let mb' = fmap (substBoundRef ref replacement) mb
            in TForallRef binderRef mb' (substTypeRef ref replacement body)
    TMuRef binderRef body
        | typeBinderRefsSameIdentity binderRef ref -> TMuRef binderRef body
        | otherwise -> TMuRef binderRef (substTypeRef ref replacement body)

isBaseBound :: Ty v -> Bool
isBaseBound ty = case ty of
    TBase{} -> True
    TBottom -> True
    _ -> False

isVarBound :: Ty v -> Bool
isVarBound ty = case ty of
    TVarRef{} -> True
    _ -> False

refMember :: TypeBinderRef -> [TypeBinderRef] -> Bool
refMember ref = any (typeBinderRefsSameIdentity ref)

deleteRef :: TypeBinderRef -> [TypeBinderRef] -> [TypeBinderRef]
deleteRef ref = filter (not . typeBinderRefsSameIdentity ref)

lookupRef :: TypeBinderRef -> [(TypeBinderRef, a)] -> Maybe a
lookupRef ref = fmap snd . findRef
  where
    findRef [] = Nothing
    findRef (entry@(candidate, _) : rest)
        | typeBinderRefsSameIdentity ref candidate = Just entry
        | otherwise = findRef rest

containsForall :: ElabType -> Bool
containsForall = cataIxConst alg
  where
    alg ty = case ty of
        TForallIFRef _ _ _ -> True
        TMuIFRef _ body -> unK body
        TArrowIF d c -> unK d || unK c
        TConIF _ args -> any unK args
        _ -> False

containsArrow :: ElabType -> Bool
containsArrow = cataIxConst alg
  where
    alg ty = case ty of
        TArrowIF _ _ -> True
        TForallIFRef _ mb body ->
            let boundHasArrow = maybe False unK mb
            in boundHasArrow || unK body
        TMuIFRef _ body -> unK body
        TConIF _ args -> any unK args
        _ -> False
