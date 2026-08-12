{-# LANGUAGE GADTs #-}

module MLF.Elab.Inst
  ( InstEvalSpec (..),
    applyInstantiation,
    composeInst,
    evalInstantiationWith,
    identityGeneratorAfterTypeAndInstantiation,
    instForLeadingTypeArgument,
    instMany,
    renameInstBoundRef,
    schemeToType,
    substBinderWithFreshDeclarationCopies,
  )
where

import Data.Functor.Foldable (Recursive (project), para)
import Data.List.NonEmpty (NonEmpty (..))
import MLF.Elab.Types
import MLF.Reify.TypeOps (alphaEqType, freeTypeVarsType, substTypeCaptureRef)
import MLF.Types.Elab (generatedIdentitiesInInstantiation)
import MLF.Types.Identity
  ( IdentityGenerator,
    freshIdentity,
    freshenTypeBinderIdentity,
    identityGeneratorAfter,
  )

-- | Turn a scheme into its corresponding type (nested `∀`).
schemeToType :: ElabScheme -> ElabType
schemeToType scheme =
  foldr
    (\(ref, mbBound) body -> tForallWithRef ref mbBound body)
    (schemeBody scheme)
    (schemeBinderRefs scheme)

composeInst :: Instantiation -> Instantiation -> Instantiation
composeInst InstId i = i
composeInst i InstId = i
composeInst i1 i2 = InstSeq i1 i2

instMany :: [Instantiation] -> Instantiation
instMany = foldr composeInst InstId

identityGeneratorAfterTypeAndInstantiation :: ElabType -> Instantiation -> IdentityGenerator
identityGeneratorAfterTypeAndInstantiation ty inst =
  identityGeneratorAfter (generatedIdentitiesInType ty ++ generatedIdentitiesInInstantiation inst)

-- | Perform a capture-avoiding binder substitution while allocating a fresh
-- lexical declaration for every repeated copy of a declaration in the
-- replacement.  Quant-Elim and recursive unfolding share this rule.  The
-- thesis identifies types up to renaming of bound variables, so substituting
-- @forall b. tau@ or @mu b. tau@ into both sides of @a -> a@ constructs two
-- alpha-equivalent scopes, not two declarations with one identity.
--
-- Keep the first declaration identity when it is available.  Later copies are
-- allocated from the instantiation's threaded supply, preserving structural
-- provenance when the copied declaration is a structural binder.  Existing
-- duplicate declarations unrelated to the replacement are deliberately not
-- repaired here; their owner remains responsible for rejecting them.
substBinderWithFreshDeclarationCopies
  :: IdentityGenerator
  -> TypeBinderRef
  -> ElabType
  -> ElabType
  -> (IdentityGenerator, ElabType)
substBinderWithFreshDeclarationCopies generator target replacement body =
  let substituted = substTypeCaptureRef target replacement body
      replacementDeclarations = declarationRefs replacement
      (substituted', (_, generator')) =
        freshenRepeatedDeclarations
          replacementDeclarations
          []
          ([], generator)
          substituted
   in (generator', substituted')
  where
    declarationRefs :: Ty v -> [TypeBinderRef]
    declarationRefs ty =
      case ty of
        TVarRef _ -> []
        TArrow domain codomain ->
          declarationRefs domain ++ declarationRefs codomain
        TConWithIdentity _ _ arguments ->
          concatMap declarationRefs arguments
        TVarAppRef _ arguments ->
          concatMap declarationRefs arguments
        TBaseWithIdentity _ _ -> []
        TForallRef ref mbBound forallBody ->
          ref
            : maybe [] declarationRefs mbBound
              ++ declarationRefs forallBody
        TMuRef ref muBody -> ref : declarationRefs muBody
        TBottom -> []

    freshenRepeatedDeclarations
      :: [TypeBinderRef]
      -> [(TypeBinderRef, TypeBinderRef)]
      -> ([TypeBinderRef], IdentityGenerator)
      -> Ty v
      -> (Ty v, ([TypeBinderRef], IdentityGenerator))
    freshenRepeatedDeclarations eligible active state ty =
      case ty of
        TVarRef ref -> (TVarRef (activeRef active ref), state)
        TArrow domain codomain ->
          let (domain', state') =
                freshenRepeatedDeclarations eligible active state domain
              (codomain', state'') =
                freshenRepeatedDeclarations eligible active state' codomain
           in (TArrow domain' codomain', state'')
        TConWithIdentity identity constructor arguments ->
          let (arguments', state') =
                freshenNonEmpty eligible active state arguments
           in (TConWithIdentity identity constructor arguments', state')
        TVarAppRef ref arguments ->
          let (arguments', state') =
                freshenNonEmpty eligible active state arguments
           in ( TVarAppRef (activeRef active ref) arguments'
              , state'
              )
        TBaseWithIdentity identity base ->
          (TBaseWithIdentity identity base, state)
        TForallRef ref mbBound forallBody ->
          let (mbBound', state') =
                freshenMaybeBound eligible active state mbBound
              (ref', state'') =
                allocateDeclaration eligible ref state'
              bodyActive = enterActiveRef ref ref' active
              (forallBody', state''') =
                freshenRepeatedDeclarations
                  eligible
                  bodyActive
                  state''
                  forallBody
           in (TForallRef ref' mbBound' forallBody', state''')
        TMuRef ref muBody ->
          let (ref', state') = allocateDeclaration eligible ref state
              bodyActive = enterActiveRef ref ref' active
              (muBody', state'') =
                freshenRepeatedDeclarations
                  eligible
                  bodyActive
                  state'
                  muBody
           in (TMuRef ref' muBody', state'')
        TBottom -> (TBottom, state)

    freshenMaybeBound _ _ state Nothing = (Nothing, state)
    freshenMaybeBound eligible active state (Just bound) =
      let (bound', state') =
            freshenRepeatedDeclarations eligible active state bound
       in (Just bound', state')

    freshenNonEmpty eligible active state (first :| remaining) =
      let (first', state') =
            freshenRepeatedDeclarations eligible active state first
          (remaining', state'') =
            freshenList eligible active state' remaining
       in (first' :| remaining', state'')

    freshenList _ _ state [] = ([], state)
    freshenList eligible active state (ty : types) =
      let (ty', state') =
            freshenRepeatedDeclarations eligible active state ty
          (types', state'') =
            freshenList eligible active state' types
       in (ty' : types', state'')

    allocateDeclaration eligible ref (seen, generator')
      | refMember ref seen
      , refMember ref eligible =
          let (freshUnique, nextGenerator) = freshIdentity generator'
              freshRef =
                typeBinderRefFromIdentity
                  ( freshenTypeBinderIdentity
                      (typeBinderRefIdentity ref)
                      freshUnique
                  )
                  (typeBinderRefName ref)
           in (freshRef, (freshRef : seen, nextGenerator))
      | otherwise = (ref, (insertRef ref seen, generator'))

    activeRef [] ref = ref
    activeRef ((sourceRef, targetRef) : remaining) ref
      | typeBinderRefsSameIdentity sourceRef ref = targetRef
      | otherwise = activeRef remaining ref

    enterActiveRef sourceRef targetRef active =
      (sourceRef, targetRef)
        : filter
          (not . typeBinderRefsSameIdentity sourceRef . fst)
          active

    insertRef ref refs
      | refMember ref refs = refs
      | otherwise = ref : refs

    refMember ref = any (typeBinderRefsSameIdentity ref)

-- | Construct the computation for applying the leading quantifier to an
-- argument type.  A flexible quantifier instantiated at its own explicit
-- bound is eliminated with @N@; grafting that bound with 'InstApp' would try
-- to apply 'InstBot' to a non-bottom bound when the computation is reduced.
--
-- This is the direct xMLF construction used by the thesis's bounded
-- application examples (for example, @f[N;N]v@ in section 15.2.5.1).
instForLeadingTypeArgument :: ElabType -> ElabType -> Instantiation
instForLeadingTypeArgument sourceTy argTy =
  case sourceTy of
    TForallRef _ (Just bound) _
      | alphaEqType argTy (tyToElab bound) -> InstElim
    _ -> InstApp argTy

data InstEvalSpec env err = InstEvalSpec
  { instBot :: ElabType -> (IdentityGenerator, env, ElabType) -> Either err (IdentityGenerator, env, ElabType),
    instAbstr :: TypeBinderRef -> (IdentityGenerator, env, ElabType) -> Either err (IdentityGenerator, env, ElabType),
    instElimError :: Instantiation -> ElabType -> err,
    instInsideError :: Instantiation -> ElabType -> err,
    instUnderError :: Instantiation -> ElabType -> err,
    instElimEnv :: TypeBinderRef -> ElabType -> env -> env,
    instUnderEnv :: TypeBinderRef -> ElabType -> env -> env,
    renameBound :: TypeBinderRef -> TypeBinderRef -> Instantiation -> Instantiation
  }

evalInstantiationWith ::
  InstEvalSpec env err ->
  Instantiation ->
  (IdentityGenerator, env, ElabType) ->
  Either err (IdentityGenerator, env, ElabType)
evalInstantiationWith spec inst = eval inst
  where
    eval = para instAlg

    instElimFn errInst (k, env', t) = case t of
      TForallRef ref mbBound body -> do
        let bTy = maybe TBottom tyToElab mbBound
            env'' = instElimEnv spec ref bTy env'
            (k', substituted) =
              substBinderWithFreshDeclarationCopies k ref bTy body
        Right (k', env'', substituted)
      _ -> Left (instElimError spec errInst t)

    instInsideFn errInst phiFn (k, env', t) = case t of
      TForallRef ref mbBound body -> do
        let b0 = maybe TBottom tyToElab mbBound
        (k1, _env'', b1) <- phiFn (k, env', b0)
        let mb' = case b1 of
              TBottom -> Nothing
              TVarRef {} -> Nothing
              _ -> either (const Nothing) Just (elabToBound b1)
        Right (k1, env', TForallRef ref mb' body)
      _ -> Left (instInsideError spec errInst t)

    -- InstApp applies a concrete type argument directly to the front forall,
    -- but first validates it against the binder bound via instBot semantics.
    -- For explicit non-bottom bounds, a bound-matching InstApp is accepted
    -- directly and substitutes the binder with that bound type.
    instAppFn argTy (k, env', t) = case t of
      TForallRef ref mbBound body -> do
        let b0 = maybe TBottom tyToElab mbBound
        (k1, env'', checkedArg) <-
          case mbBound of
            Just _
              | alphaEqType argTy b0 ->
                  Right (k, env', b0)
            _ ->
              instBot spec argTy (k, env', b0)
        let env''' = instElimEnv spec ref checkedArg env''
            (k2, substituted) =
              substBinderWithFreshDeclarationCopies
                k1
                ref
                checkedArg
                body
        Right (k2, env''', substituted)
      _ ->
        Left
          (instElimError spec (InstSeq (InstInside (InstBot argTy)) InstElim) t)

    instAbstrRefArg instArg = case project instArg of
      InstAbstrFRef ref -> Just ref
      _ -> Nothing

    instElimAbstr ref k env' t = case t of
      TForallRef forallRef _mbBound body ->
        let replacement = TVarRef ref
            env'' = instElimEnv spec forallRef replacement env'
         in Right (k, env'', substTypeCaptureRef forallRef replacement body)
      _ -> Left (instElimError spec InstElim t)

    instAlg inst0 = case inst0 of
      InstIdF -> \(k, env', t) -> Right (k, env', t)
      InstSeqF (left, i1) (right, i2) ->
        \(k, env', t) ->
          case (left, right) of
            (InstInside (InstBot tyArg), InstElim) ->
              instAppFn tyArg (k, env', t)
            (InstInside abstr, InstElim)
              | Just ref <- instAbstrRefArg abstr ->
                  instElimAbstr ref k env' t
            _ -> do
              (k1, env'', t1) <- i1 (k, env', t)
              i2 (k1, env'', t1)
      InstAppF argTy -> instAppFn argTy
      InstBotF tArg -> instBot spec tArg
      InstAbstrFRef ref -> instAbstr spec ref
      InstIntroF ->
        \(generator, env', t) -> do
          let used = freeTypeVarsType t
              (ref, generator') = freshTypeBinderRefFromNames used generator
          Right (generator', env', tForallWithRef ref Nothing t)
      InstElimF -> instElimFn InstElim
      InstInsideF (_, phiFn) -> instInsideFn InstId phiFn
      InstUnderFRef paramRef (phiInst, _phiFn) ->
        \(k, env', t) -> case t of
          TForallRef ref mbBound body -> do
            let b0 = maybe TBottom tyToElab mbBound
                env'' = instUnderEnv spec ref b0 env'
                phi' = renameBound spec paramRef ref phiInst
            (k1, _env''', body') <- eval phi' (k, env'', body)
            Right (k1, env', TForallRef ref mbBound body')
          _ -> Left (instUnderError spec phiInst t)

-- | Apply an xMLF instantiation to an xMLF type (xmlf Fig. 3).
--
-- This is a *partial* function: it fails if the instantiation expects a certain
-- type form (e.g. ∀ for `N`) but the type does not match.
applyInstantiation :: ElabType -> Instantiation -> Either ElabError ElabType
applyInstantiation ty inst =
  (\(_, _, ty') -> ty') <$> evalInstantiationWith spec inst (identityGeneratorAfterTypeAndInstantiation ty inst, [], ty)
  where
    resolveReplayVars :: [(TypeBinderRef, ElabType)] -> ElabType -> ElabType
    resolveReplayVars replayEnv ty0 =
      foldl
        (\tyAcc (ref, replacement) -> substTypeCaptureRef ref replacement tyAcc)
        ty0
        replayEnv

    {- Note [InstBot replay-bound match]
       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       The thesis (§15.3.4) specifies that InstBot (⊥-instantiation) should only
       match a bottom type.  However, during witness replay the bounded no-fallback
       path (reifyTypeWithNamedSetNoFallback) can carry a localized shape such as
       t9 -> t9 through the replay environment, replacing what was originally ⊥ at
       the constraint graph level.

       allowReplayBoundMatch detects this situation: if resolving replay-environment
       variables in tArg produces a *different* type that is alpha-equivalent to the
       current scrutinee t, the InstBot step is accepted.  This is sound because the
       replay environment records the specific instantiation that was already
       validated during presolution — we are merely replaying a witness that was
       correct at solve time.

       See: BUG-2026-03-16-001, test "BUG-2026-03-16-001 regression" in
       ElaborationSpec.hs.
    -}
    allowReplayBoundMatch :: [(TypeBinderRef, ElabType)] -> ElabType -> ElabType -> Bool
    allowReplayBoundMatch replayEnv tArg t =
      let resolvedArg = resolveReplayVars replayEnv tArg
       in not (alphaEqType resolvedArg tArg)
            && alphaEqType resolvedArg t

    spec =
      InstEvalSpec
        { instBot = \tArg (k, replayEnv, t) -> case t of
            TBottom -> Right (k, replayEnv, tArg)
            _
              | allowReplayBoundMatch replayEnv tArg t ->
                  Right (k, replayEnv, t)
            _ -> Left (InstantiationError ("InstBot expects ⊥, got: " ++ pretty t)),
          instAbstr = \ref (k, replayEnv, _t) -> Right (k, replayEnv, TVarRef ref),
          instElimError = \_inst0 t ->
            InstantiationError ("InstElim expects ∀, got: " ++ pretty t),
          instInsideError = \_inst0 t ->
            InstantiationError ("InstInside expects ∀, got: " ++ pretty t),
          instUnderError = \_inst0 t ->
            InstantiationError ("InstUnder expects ∀, got: " ++ pretty t),
          instElimEnv = \ref replacement replayEnv -> (ref, replacement) : replayEnv,
          instUnderEnv = \_v _bound replayEnv -> replayEnv,
          renameBound = renameInstBoundRef
        }

renameInstBoundRef :: TypeBinderRef -> TypeBinderRef -> Instantiation -> Instantiation
renameInstBoundRef oldRef newRef = para alg
  where
    alg inst0 = case inst0 of
      InstIdF -> InstId
      InstAppF t -> InstApp (renameType t)
      InstBotF t -> InstBot (renameType t)
      InstIntroF -> InstIntro
      InstElimF -> InstElim
      InstAbstrFRef ref ->
        instAbstrWithRef $
          if typeBinderRefsSameIdentity ref oldRef
            then newRef
            else ref
      InstInsideF i -> InstInside (snd i)
      InstSeqF a b -> InstSeq (snd a) (snd b)
      InstUnderFRef ref i
        | typeBinderRefsSameIdentity ref oldRef -> instUnderWithRef ref (fst i) -- shadowing: stop renaming under this binder
        | otherwise -> instUnderWithRef ref (snd i)
    renameType =
      substTypeCaptureRef oldRef (TVarRef newRef)
