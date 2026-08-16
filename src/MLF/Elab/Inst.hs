{-# LANGUAGE GADTs #-}

module MLF.Elab.Inst
  ( InstEvalSpec (..),
    applyInstantiation,
    composeInst,
    evalInstantiationWith,
    identityGeneratorAfterTypeAndInstantiation,
    freshenInstantiationTypeDeclarationScopes,
    instForLeadingTypeArgument,
    instMany,
    renameInstBoundRef,
    schemeToType,
    substBinderAtOccurrencesWithFreshDeclarationCopies,
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
    advanceIdentityGeneratorPastMany,
    freshIdentity,
    freshenTypeBinderIdentity,
    identityGeneratorAfter,
    typeBinderIdentityIsCanonicalStructural,
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

-- | Allocate every lexical declaration carried by an instantiation payload in
-- the identity domain where that computation will run.  A graph-derived exact
-- target can mention the same source forall at several independent type
-- positions.  Embedding that target verbatim in 'InstBot' or 'InstApp' would
-- make those positions one nominal declaration even though the xMLF
-- computation constructs separate scopes.
--
-- The source declarations and preceding payloads reserve their identities;
-- later declarations are alpha-copied with the instantiation's own fresh
-- supply.  Free references are deliberately unchanged, and canonical
-- structural declarations remain reusable owner presentations.
freshenInstantiationTypeDeclarationScopes
  :: ElabType
  -> Instantiation
  -> Instantiation
freshenInstantiationTypeDeclarationScopes source instantiation =
  fst
    (go initialState instantiation)
  where
    initialState =
      ( lexicalDeclarationRefs source
      , identityGeneratorAfterTypeAndInstantiation source instantiation
      )

    go state inst =
      case inst of
        InstId -> (InstId, state)
        InstApp ty ->
          let (ty', state') = freshenPayload state ty
           in (InstApp ty', state')
        InstBot ty ->
          let (ty', state') = freshenPayload state ty
           in (InstBot ty', state')
        InstIntro -> (InstIntro, state)
        InstElim -> (InstElim, state)
        InstAbstrRef ref -> (InstAbstrRef ref, state)
        InstUnderRef ref inner ->
          let (inner', state') =
                go (reserveRef ref state) inner
           in (InstUnderRef ref inner', state')
        InstInside inner ->
          let (inner', state') = go state inner
           in (InstInside inner', state')
        InstSeq left right ->
          let (left', state') = go state left
              (right', state'') = go state' right
           in (InstSeq left' right', state'')

    freshenPayload (reserved, generator) ty =
      let (generator', ty') =
            freshenDeclarationsWhere
              ( \ref seen ->
                  isLexicalDeclaration ref
                    && refMember ref seen
              )
              generator
              reserved
              ty
          reserved' =
            foldr insertRef reserved (lexicalDeclarationRefs ty')
       in (ty', (reserved', generator'))

    reserveRef ref (reserved, generator) =
      (insertRef ref reserved, generator)

    lexicalDeclarationRefs = filter isLexicalDeclaration . declarationRefs

    isLexicalDeclaration ref =
      not
        ( typeBinderIdentityIsCanonicalStructural
            (typeBinderRefIdentity ref)
        )

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
      (generator', substituted') =
        freshenDeclarationsWhere
          ( \ref seen ->
              refMember ref seen
                && refMember ref replacementDeclarations
          )
          generator
          []
          substituted
   in (generator', substituted')

-- | Substitute each free occurrence of a binder with its own lexical copy of
-- the replacement's declarations.  Unlike
-- 'substBinderWithFreshDeclarationCopies', this marks the occurrences before
-- substitution and never traverses pre-existing declarations as candidates
-- for freshening.  It is the construction rule needed when an owner-final
-- result publishes the same bounded type at several occurrence sites.
substBinderAtOccurrencesWithFreshDeclarationCopies
  :: IdentityGenerator
  -> TypeBinderRef
  -> ElabType
  -> ElabType
  -> (IdentityGenerator, ElabType)
substBinderAtOccurrencesWithFreshDeclarationCopies generator target replacement body =
  substituteMarkedOccurrences markerGenerator markedBody markers
  where
    occupiedGenerator =
      advanceIdentityGeneratorPastMany
        ( generatedIdentitiesInType replacement
            ++ generatedIdentitiesInType body
        )
        generator
    (markerGenerator, markedBody, markers) =
      markFreeBinderOccurrences target occupiedGenerator body

    substituteMarkedOccurrences generator' ty [] = (generator', ty)
    substituteMarkedOccurrences generator' ty (marker : remaining) =
      let (copyGenerator, replacementCopy) =
            freshenDeclarationsWhere
              (\ref seen -> refMember ref seen)
              generator'
              (declarationRefs ty)
              replacement
          substituted = substTypeCaptureRef marker replacementCopy ty
          nextGenerator =
            advanceIdentityGeneratorPastMany
              (generatedIdentitiesInType substituted)
              copyGenerator
       in substituteMarkedOccurrences nextGenerator substituted remaining

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

markFreeBinderOccurrences
  :: TypeBinderRef
  -> IdentityGenerator
  -> Ty v
  -> (IdentityGenerator, Ty v, [TypeBinderRef])
markFreeBinderOccurrences target generator ty =
  case ty of
    TVarRef ref
      | typeBinderRefsSameIdentity ref target ->
          let (nextGenerator, marker) = freshMarkerRef generator ref
           in (nextGenerator, TVarRef marker, [marker])
      | otherwise -> (generator, TVarRef ref, [])
    TArrow domain codomain ->
      let (domainGenerator, domain', domainMarkers) =
            markFreeBinderOccurrences target generator domain
          (codomainGenerator, codomain', codomainMarkers) =
            markFreeBinderOccurrences target domainGenerator codomain
       in ( codomainGenerator
          , TArrow domain' codomain'
          , domainMarkers ++ codomainMarkers
          )
    TConWithIdentity identity constructor arguments ->
      let (nextGenerator, arguments', markers) =
            markFreeBinderOccurrencesNonEmpty target generator arguments
       in ( nextGenerator
          , TConWithIdentity identity constructor arguments'
          , markers
          )
    TVarAppRef ref arguments ->
      let (headGenerator, ref', headMarkers)
            | typeBinderRefsSameIdentity ref target =
                let (markerGenerator, marker) = freshMarkerRef generator ref
                 in (markerGenerator, marker, [marker])
            | otherwise = (generator, ref, [])
          (nextGenerator, arguments', argumentMarkers) =
            markFreeBinderOccurrencesNonEmpty target headGenerator arguments
       in ( nextGenerator
          , TVarAppRef ref' arguments'
          , headMarkers ++ argumentMarkers
          )
    TBaseWithIdentity identity base ->
      (generator, TBaseWithIdentity identity base, [])
    TForallRef ref mbBound forallBody ->
      let (boundGenerator, mbBound', boundMarkers) =
            markFreeBinderOccurrencesMaybe target generator mbBound
       in if typeBinderRefsSameIdentity ref target
            then
              ( boundGenerator
              , TForallRef ref mbBound' forallBody
              , boundMarkers
              )
            else
              let (bodyGenerator, forallBody', bodyMarkers) =
                    markFreeBinderOccurrences target boundGenerator forallBody
               in ( bodyGenerator
                  , TForallRef ref mbBound' forallBody'
                  , boundMarkers ++ bodyMarkers
                  )
    TMuRef ref muBody
      | typeBinderRefsSameIdentity ref target ->
          (generator, TMuRef ref muBody, [])
      | otherwise ->
          let (nextGenerator, muBody', markers) =
                markFreeBinderOccurrences target generator muBody
           in (nextGenerator, TMuRef ref muBody', markers)
    TBottom -> (generator, TBottom, [])

markFreeBinderOccurrencesMaybe
  :: TypeBinderRef
  -> IdentityGenerator
  -> Maybe (Ty v)
  -> (IdentityGenerator, Maybe (Ty v), [TypeBinderRef])
markFreeBinderOccurrencesMaybe _ generator Nothing =
  (generator, Nothing, [])
markFreeBinderOccurrencesMaybe target generator (Just ty) =
  let (nextGenerator, ty', markers) =
        markFreeBinderOccurrences target generator ty
   in (nextGenerator, Just ty', markers)

markFreeBinderOccurrencesNonEmpty
  :: TypeBinderRef
  -> IdentityGenerator
  -> NonEmpty (Ty v)
  -> (IdentityGenerator, NonEmpty (Ty v), [TypeBinderRef])
markFreeBinderOccurrencesNonEmpty target generator (first :| remaining) =
  let (firstGenerator, first', firstMarkers) =
        markFreeBinderOccurrences target generator first
      (nextGenerator, remaining', remainingMarkers) =
        markFreeBinderOccurrencesList target firstGenerator remaining
   in (nextGenerator, first' :| remaining', firstMarkers ++ remainingMarkers)

markFreeBinderOccurrencesList
  :: TypeBinderRef
  -> IdentityGenerator
  -> [Ty v]
  -> (IdentityGenerator, [Ty v], [TypeBinderRef])
markFreeBinderOccurrencesList _ generator [] = (generator, [], [])
markFreeBinderOccurrencesList target generator (ty : types) =
  let (typeGenerator, ty', typeMarkers) =
        markFreeBinderOccurrences target generator ty
      (nextGenerator, types', remainingMarkers) =
        markFreeBinderOccurrencesList target typeGenerator types
   in (nextGenerator, ty' : types', typeMarkers ++ remainingMarkers)

freshMarkerRef
  :: IdentityGenerator
  -> TypeBinderRef
  -> (IdentityGenerator, TypeBinderRef)
freshMarkerRef generator ref =
  let (freshUnique, nextGenerator) = freshIdentity generator
   in ( nextGenerator
      , typeBinderRefFromIdentity
          (freshenTypeBinderIdentity (typeBinderRefIdentity ref) freshUnique)
          (typeBinderRefName ref)
      )

freshenDeclarationsWhere
  :: (TypeBinderRef -> [TypeBinderRef] -> Bool)
  -> IdentityGenerator
  -> [TypeBinderRef]
  -> Ty v
  -> (IdentityGenerator, Ty v)
freshenDeclarationsWhere shouldFreshen generator seen ty =
  let (ty', (_, generator')) = go [] (seen, generator) ty
   in (generator', ty')
  where
    go
      :: [(TypeBinderRef, TypeBinderRef)]
      -> ([TypeBinderRef], IdentityGenerator)
      -> Ty w
      -> (Ty w, ([TypeBinderRef], IdentityGenerator))
    go active state current =
      case current of
        TVarRef ref -> (TVarRef (activeRef active ref), state)
        TArrow domain codomain ->
          let (domain', state') = go active state domain
              (codomain', state'') = go active state' codomain
           in (TArrow domain' codomain', state'')
        TConWithIdentity identity constructor arguments ->
          let (arguments', state') = goNonEmpty active state arguments
           in (TConWithIdentity identity constructor arguments', state')
        TVarAppRef ref arguments ->
          let (arguments', state') = goNonEmpty active state arguments
           in (TVarAppRef (activeRef active ref) arguments', state')
        TBaseWithIdentity identity base ->
          (TBaseWithIdentity identity base, state)
        TForallRef ref mbBound forallBody ->
          let (mbBound', state') = goMaybe active state mbBound
              (ref', state'') = allocateDeclaration ref state'
              bodyActive = enterActiveRef ref ref' active
              (forallBody', state''') = go bodyActive state'' forallBody
           in (TForallRef ref' mbBound' forallBody', state''')
        TMuRef ref muBody ->
          let (ref', state') = allocateDeclaration ref state
              bodyActive = enterActiveRef ref ref' active
              (muBody', state'') = go bodyActive state' muBody
           in (TMuRef ref' muBody', state'')
        TBottom -> (TBottom, state)

    goMaybe
      :: [(TypeBinderRef, TypeBinderRef)]
      -> ([TypeBinderRef], IdentityGenerator)
      -> Maybe (Ty w)
      -> (Maybe (Ty w), ([TypeBinderRef], IdentityGenerator))
    goMaybe _ state Nothing = (Nothing, state)
    goMaybe active state (Just bound) =
      let (bound', state') = go active state bound
       in (Just bound', state')

    goNonEmpty
      :: [(TypeBinderRef, TypeBinderRef)]
      -> ([TypeBinderRef], IdentityGenerator)
      -> NonEmpty (Ty w)
      -> (NonEmpty (Ty w), ([TypeBinderRef], IdentityGenerator))
    goNonEmpty active state (first :| remaining) =
      let (first', state') = go active state first
          (remaining', state'') = goList active state' remaining
       in (first' :| remaining', state'')

    goList
      :: [(TypeBinderRef, TypeBinderRef)]
      -> ([TypeBinderRef], IdentityGenerator)
      -> [Ty w]
      -> ([Ty w], ([TypeBinderRef], IdentityGenerator))
    goList _ state [] = ([], state)
    goList active state (current : remaining) =
      let (current', state') = go active state current
          (remaining', state'') = goList active state' remaining
       in (current' : remaining', state'')

    allocateDeclaration ref (seen', generator')
      | shouldFreshen ref seen' =
          let (freshUnique, nextGenerator) = freshIdentity generator'
              freshRef =
                typeBinderRefFromIdentity
                  ( freshenTypeBinderIdentity
                      (typeBinderRefIdentity ref)
                      freshUnique
                  )
                  (typeBinderRefName ref)
           in (freshRef, (freshRef : seen', nextGenerator))
      | otherwise = (ref, (insertRef ref seen', generator'))

activeRef
  :: [(TypeBinderRef, TypeBinderRef)]
  -> TypeBinderRef
  -> TypeBinderRef
activeRef [] ref = ref
activeRef ((sourceRef, targetRef) : remaining) ref
  | typeBinderRefsSameIdentity sourceRef ref = targetRef
  | otherwise = activeRef remaining ref

enterActiveRef
  :: TypeBinderRef
  -> TypeBinderRef
  -> [(TypeBinderRef, TypeBinderRef)]
  -> [(TypeBinderRef, TypeBinderRef)]
enterActiveRef sourceRef targetRef active =
  (sourceRef, targetRef)
    : filter
      (not . typeBinderRefsSameIdentity sourceRef . fst)
      active

insertRef :: TypeBinderRef -> [TypeBinderRef] -> [TypeBinderRef]
insertRef ref refs
  | refMember ref refs = refs
  | otherwise = ref : refs

refMember :: TypeBinderRef -> [TypeBinderRef] -> Bool
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
