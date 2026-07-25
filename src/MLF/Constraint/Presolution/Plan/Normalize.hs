{-# LANGUAGE GADTs #-}
module MLF.Constraint.Presolution.Plan.Normalize (
    substTypeRef,
    simplifySchemeBindingsRefs,
    simplifySchemeBindingsRefsWhen,
    simplifySchemeBindingsRefsWhenPreserving,
    promoteArrowAliasRefs,
    promoteArrowAliasRefsWhen,
    isBaseBound,
    isVarBound,
    containsForall,
    containsArrow
) where

import MLF.Reify.TypeOps
    ( composeTypeHeadRef
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
simplifySchemeBindingsRefs inlineBaseBounds =
    simplifySchemeBindingsRefsWhen (const inlineBaseBounds)

simplifySchemeBindingsRefsWhen
    :: (TypeBinderRef -> Bool)
    -> [TypeBinderRef]
    -> [(TypeBinderRef, Maybe BoundType)]
    -> ElabType
    -> ([(TypeBinderRef, Maybe BoundType)], ElabType)
simplifySchemeBindingsRefsWhen shouldInlineBaseBound namedBinders binds ty =
    simplifySchemeBindingsRefsWhenPreserving
        shouldInlineBaseBound
        (const False)
        namedBinders
        binds
        ty

simplifySchemeBindingsRefsWhenPreserving
    :: (TypeBinderRef -> Bool)
    -> (TypeBinderRef -> Bool)
    -> [TypeBinderRef]
    -> [(TypeBinderRef, Maybe BoundType)]
    -> ElabType
    -> ([(TypeBinderRef, Maybe BoundType)], ElabType)
simplifySchemeBindingsRefsWhenPreserving shouldInlineBaseBound preserveBinder namedBinders binds ty =
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
                    bindingIsUsed = bindingOccursFree ref rest body
                in if not bindingIsUsed && not (preserveBinder ref)
                    then simplify (deleteRef ref binders) rest body
                    else case body of
                    TVarRef bodyRef | typeBinderRefsSameIdentity bodyRef ref ->
                        -- See Note [Eq-Var normalization].
                        let freeBound = freeTypeVarRefsType bound
                            boundMentionsSelf = refMember ref freeBound
                        in if not boundMentionsSelf && not (preserveBinder ref)
                            then
                                let body' = boundElab
                                    restSub =
                                        [ (name, fmap (substBoundRef ref boundElab) mb)
                                        | (name, mb) <- rest
                                        ]
                                in eliminateBinding ref bound binders rest restSub body body'
                            else
                                let (rest', body') = simplify binders rest body
                                in ((ref, Just bound) : rest', body')
                    _ ->
                        let freeBound = freeTypeVarRefsType bound
                            boundMentionsSelf = refMember ref freeBound
                            boundDeps = deleteRef ref freeBound
                            dependsOnBinders =
                                let remainingBinders = deleteRef ref binders
                                in any (`refMember` remainingBinders) boundDeps
                            boundMentionsNamed =
                                any (`refMember` namedBinders) freeBound
                            canInlineBase =
                                shouldInlineBaseBound ref
                                    && not dependsOnBinders
                                    && isBaseBound bound
                                    && not boundMentionsNamed
                            canInlineNonBase =
                                not dependsOnBinders
                                    && not (isBaseBound bound)
                                    && isVarBound bound
                                    && not isNamedBinder
                                    && not boundMentionsNamed
                        in if not boundMentionsSelf
                            && not (preserveBinder ref)
                            && (canInlineBase || canInlineNonBase)
                            then
                                let replacement = boundElab
                                    bodySub = substTypeRef ref replacement body
                                    restSub =
                                        [ (name, fmap (substBoundRef ref replacement) mb)
                                        | (name, mb) <- rest
                                        ]
                                in eliminateBinding ref bound binders rest restSub body bodySub
                            else
                                let (rest', body') = simplify binders rest body
                                in ((ref, Just bound) : rest', body')

    bindingOccursFree ref remainingBindings body =
        refMember ref (freeTypeVarRefsType body)
            || any
                (refMember ref . freeTypeVarRefsType)
                [ bound
                | (_, Just bound) <- remainingBindings
                ]

    -- Binder elimination is one capability: rewrite the body and every
    -- remaining bound, then retire the declaration only if that exact
    -- identity is absent from the rewritten scope.  A nested forall shadows
    -- its body but not its own bound, so the lexical free-reference query is
    -- essential here.
    eliminateBinding ref bound binders originalRest rewrittenRest originalBody rewrittenBody
        | bindingOccursFree ref rewrittenRest rewrittenBody =
            let (rest', body') = simplify binders originalRest originalBody
            in ((ref, Just bound) : rest', body')
        | otherwise =
            simplify (deleteRef ref binders) rewrittenRest rewrittenBody

{- Note [Eq-Var normalization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The thesis's Eq-Var rule states that @forall (a > tau). a@ is equivalent
to @tau@ (Section 8.2, Figure 8.2.3), provided the bound does not mention
the binder itself.  Apply that equivalence regardless of the shape of @tau@
for ordinary binders.  A caller-protected binder is construction authority
(notably a required Gamma entry for a root RaiseMerge), whose explicit
abstraction and instantiation must survive even when it is type-vacuous or its
erased type is Eq-Var equivalent to the bound.
-}

promoteArrowAliasRefs :: [(TypeBinderRef, Maybe BoundType)] -> ElabType -> ([(TypeBinderRef, Maybe BoundType)], ElabType)
promoteArrowAliasRefs = promoteArrowAliasRefsWhen (const True)

promoteArrowAliasRefsWhen
    :: (TypeBinderRef -> Bool)
    -> [(TypeBinderRef, Maybe BoundType)]
    -> ElabType
    -> ([(TypeBinderRef, Maybe BoundType)], ElabType)
promoteArrowAliasRefsWhen canPromote binds ty = case ty of
    TArrow (TVarRef ref1) (TVarRef ref2)
        | typeBinderRefsSameIdentity ref1 ref2
        , canPromote ref1 ->
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
    TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (substTypeRef ref replacement) args)
    TVarAppRef headRef args ->
        let args' = fmap (substTypeRef ref replacement) args
        in if typeBinderRefsSameIdentity headRef ref
            then composeTypeHeadRef headRef replacement args'
            else TVarAppRef headRef args'
    TBaseWithIdentity identity b -> TBaseWithIdentity identity b
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
    TBaseWithIdentity{} -> True
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
        TConIFWithIdentity _ _ args -> any unK args
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
        TConIFWithIdentity _ _ args -> any unK args
        _ -> False
