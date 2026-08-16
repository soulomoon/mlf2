{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module MLF.Reify.TypeOps
  ( splitForallsRefs,
    stripForallsType,
    implicitForallClosureMatches,
    freeTypeVarRefsFrom,
    freeTypeVarsType,
    freeTypeVarAliasNamesType,
    freeTypeVarRefsType,
    freeTypeVarRefsList,
    substTypeCaptureRef,
    substTypeSimpleRef,
    composeTypeHeadRef,
    freshNameLike,
    alphaEqType,
    alphaEqTypePreservingStructuralBinders,
    alphaEqTypePreservingRecursiveBinders,
    churchMuEquivalent,
    churchAwareEqType,
    churchRepresentationEqType,
    typeHeadMatches,
    firstNonContractiveRecursiveType,
    matchTypeRefs,
    matchChurchAwareTypeRefs,
    resolveBaseBoundForInstConstraint,
    resolveBaseBoundForInstSolved,
    resolveBoundBodyConstraint,
    inlineBaseBoundsType,
    inlineAliasBoundsWithBy,
    inlineAliasBoundsWithBySeen,
    inlineAliasBoundsWithBySeenProtected,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import qualified Data.IntSet as IntSet
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Frontend.Symbol (SymbolIdentity)
import MLF.Types.Elab
import MLF.Types.Identity
  ( UniqueIdentity (..),
    freshenTypeBinderIdentity,
    freshIdentity,
    typeBinderIdentityStructural,
  )
import MLF.Util.ElabError (ElabError (..))
import MLF.Util.Names (freshNameLike)

newtype BoundRefFun (i :: TopVar) = BoundRefFun {runBoundRefFun :: [TypeBinderRef] -> [TypeBinderRef]}

splitForallsRefs :: Ty v -> ([(TypeBinderRef, Maybe BoundType)], ElabType)
splitForallsRefs = go
  where
    go :: forall w. Ty w -> ([(TypeBinderRef, Maybe BoundType)], ElabType)
    go ty = case ty of
      TForallRef ref mb body ->
        let (binds, body') = go body
         in ((ref, mb) : binds, body')
      _ -> ([], tyToElab ty)

stripForallsType :: Ty v -> ElabType
stripForallsType = snd . splitForallsRefs

-- | A free binder in a source-facing type is implicitly generalized by the
-- graph pipeline.  Accept that closure only when the constructed forall spine
-- carries exactly the same semantic binder identities.  This is deliberately
-- stricter than ordinary alpha-equivalence: same-spelled free binders from
-- different scopes remain incompatible.
implicitForallClosureMatches :: ElabType -> ElabType -> Bool
implicitForallClosureMatches expectedTy candidateTy =
  null expectedForalls
    && not (null candidateForalls)
    && not (Set.null expectedFreeIdentities)
    && all ((== Nothing) . snd) candidateForalls
    && candidateBinderIdentities == expectedFreeIdentities
    && alphaEqType candidateBody expectedTy
  where
    (expectedForalls, _) = splitForallsRefs expectedTy
    (candidateForalls, candidateBody) = splitForallsRefs candidateTy
    candidateBinderIdentities =
      Set.fromList
        [ typeBinderRefIdentity ref
        | (ref, _) <- candidateForalls
        ]
    expectedFreeIdentities =
      Set.fromList
        (map typeBinderRefIdentity (freeTypeVarRefsType expectedTy))

freeTypeVarsType :: Ty v -> Set.Set String
freeTypeVarsType =
  Set.fromList . map typeBinderRefName . freeTypeVarRefsType

freeTypeVarAliasNamesType :: Ty v -> Set.Set String
freeTypeVarAliasNamesType =
  Set.unions . map typeBinderRefAliasNames . freeTypeVarRefsType

freeTypeVarRefsType :: Ty v -> [TypeBinderRef]
freeTypeVarRefsType = freeTypeVarRefsFromWith False []

freeTypeVarRefsFrom :: [TypeBinderRef] -> Ty v -> [TypeBinderRef]
freeTypeVarRefsFrom = freeTypeVarRefsFromWith True

freeTypeVarRefsList :: Ty v -> [TypeBinderRef]
freeTypeVarRefsList = freeTypeVarRefsType

freeTypeVarRefsFromWith :: Bool -> [TypeBinderRef] -> Ty v -> [TypeBinderRef]
freeTypeVarRefsFromWith bindInBound bound0 ty =
  runBoundRefFun (cataIx alg ty) bound0
  where
    alg :: TyIF i BoundRefFun -> BoundRefFun i
    alg node = case node of
      TVarIFRef ref ->
        BoundRefFun $ \boundRefs ->
          if refMember ref boundRefs
            then []
            else [ref]
      TArrowIF d c ->
        BoundRefFun $ \boundRefs ->
          unionRefs (runBoundRefFun d boundRefs) (runBoundRefFun c boundRefs)
      TConIFWithIdentity _ _ args ->
        BoundRefFun $ \boundRefs ->
          foldr
            (\arg acc -> unionRefs (runBoundRefFun arg boundRefs) acc)
            []
            args
      TVarAppIFRef ref args ->
        BoundRefFun $ \boundRefs ->
          let headFree =
                if refMember ref boundRefs
                  then []
                  else [ref]
              argsFree =
                foldr
                  (\arg acc -> unionRefs (runBoundRefFun arg boundRefs) acc)
                  []
                  args
           in unionRefs headFree argsFree
      TBaseIFWithIdentity _ _ -> BoundRefFun (const [])
      TBottomIF -> BoundRefFun (const [])
      TForallIFRef ref mb body ->
        BoundRefFun $ \boundRefs ->
          let boundBody = insertRef ref boundRefs
              boundBound = if bindInBound then boundBody else boundRefs
              freeBound = maybe [] (\f -> runBoundRefFun f boundBound) mb
              freeBody = runBoundRefFun body boundBody
           in unionRefs freeBound freeBody
      TMuIFRef ref body ->
        BoundRefFun $ \boundRefs ->
          runBoundRefFun body (insertRef ref boundRefs)

    unionRefs left right =
      foldr insertRef right left

    insertRef ref refs
      | refMember ref refs = refs
      | otherwise = ref : refs

    refMember ref =
      any (typeBinderRefsSameIdentity ref)

substTypeCaptureRef :: TypeBinderRef -> ElabType -> ElabType -> ElabType
substTypeCaptureRef target s = goSub
  where
    freeSRefs = freeTypeVarRefsType s
    freeSNames = Set.unions (map typeBinderRefAliasNames freeSRefs)

    freshCaptureRef :: TypeBinderRef -> String -> ElabType -> Maybe BoundType -> TypeBinderRef
    freshCaptureRef originalRef name body mbBound =
      typeBinderRefFromIdentity
        (freshenTypeBinderIdentity (typeBinderRefIdentity originalRef) freshUnique)
        name
      where
        (baseUnique, _) =
          freshIdentity (identityGeneratorAfterType seed)
        -- Capture avoidance is deliberately pure, so there is no mutable
        -- supply to thread through nested binders.  The seed alone is not
        -- enough: a vacuous outer freshened binder does not occur in an inner
        -- binder's body, which used to allocate the same generated identity
        -- for several distinct binders.  Pair the seed's next identity with
        -- the original binder identity so distinct lexical declarations are
        -- distinct by construction.
        freshUnique =
          UniqueIdentity
            ( uniqueIdentityValue baseUnique
                + captureIdentityOffset
                  (typeBinderIdentityKey (typeBinderRefIdentity originalRef))
            )
        seed =
          TArrow
            s
            (maybe body (\bound -> TArrow (tyToElab bound) body) mbBound)
        captureIdentityOffset key
          | key >= 0 = key * 2
          | otherwise = negate key * 2 - 1

    replacementMentionsRef :: TypeBinderRef -> Bool
    replacementMentionsRef ref =
      any (binderMayCaptureReplacementRef ref) freeSRefs

    targetOccursFreeIn :: ElabType -> Bool
    targetOccursFreeIn ty =
      any
        (typeBinderRefsSameIdentity target)
        (freeTypeVarRefsType ty)

    binderMayCaptureReplacementRef :: TypeBinderRef -> TypeBinderRef -> Bool
    binderMayCaptureReplacementRef binder replacementRef =
      typeBinderRefsSameIdentity binder replacementRef

    substBoundCaptureLocal :: TypeBinderRef -> ElabType -> BoundType -> BoundType
    substBoundCaptureLocal targetRef replacement bound = case bound of
      TArrow a b ->
        TArrow (substTypeCaptureRef targetRef replacement a) (substTypeCaptureRef targetRef replacement b)
      TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (substTypeCaptureRef targetRef replacement) args)
      TVarAppRef ref args ->
        let args' = fmap (substTypeCaptureRef targetRef replacement) args
         in if typeBinderRefsSameIdentity ref targetRef
              then composeTypeHeadRef ref replacement args'
              else TVarAppRef ref args'
      TBaseWithIdentity identity b -> TBaseWithIdentity identity b
      TBottom -> TBottom
      TForallRef ref mb body
        | typeBinderRefsSameIdentity ref targetRef ->
            let mb' = fmap (substBoundCaptureLocal targetRef replacement) mb
             in TForallRef ref mb' body
        | replacementMentionsRef ref
        , targetOccursFreeIn (tyToElab body) ->
            let used =
                  Set.unions
                    [ freeSNames,
                      freeTypeVarAliasNamesType body,
                      maybe Set.empty freeTypeVarAliasNamesType mb,
                      Set.singleton v
                    ]
                v' = freshNameLike v used
                ref' = freshCaptureRef ref v' (tyToElab body) mb
                body' = substTypeCaptureRef ref (TVarRef ref') body
                mb' = fmap (substBoundCaptureLocal targetRef replacement) mb
             in TForallRef ref' mb' (substTypeCaptureRef targetRef replacement body')
        | otherwise ->
            let mb' = fmap (substBoundCaptureLocal targetRef replacement) mb
             in TForallRef ref mb' (substTypeCaptureRef targetRef replacement body)
        where
          v = typeBinderRefName ref
      TMuRef ref body
        | typeBinderRefsSameIdentity ref targetRef -> TMuRef ref body
        | replacementMentionsRef ref
        , targetOccursFreeIn (tyToElab body) ->
            let used =
                  Set.unions
                    [ freeSNames,
                      freeTypeVarAliasNamesType body,
                      Set.singleton v
                    ]
                v' = freshNameLike v used
                ref' = freshCaptureRef ref v' (tyToElab body) Nothing
                body' = substTypeCaptureRef ref (TVarRef ref') body
             in TMuRef ref' (substTypeCaptureRef targetRef replacement body')
        | otherwise ->
            TMuRef ref (substTypeCaptureRef targetRef replacement body)
        where
          v = typeBinderRefName ref

    goSub = paraIx alg
      where
        alg :: TyIF i (IxPair Ty Ty) -> Ty i
        alg ty = case ty of
          TVarIFRef ref
            | typeBinderRefsSameIdentity ref target -> s
            | otherwise -> TVarRef ref
          TArrowIF d c -> TArrow (snd (unIxPair d)) (snd (unIxPair c))
          TConIFWithIdentity identity c args -> TConWithIdentity identity c (fmap (snd . unIxPair) args)
          TVarAppIFRef ref args ->
            let args' = fmap (snd . unIxPair) args
             in if typeBinderRefsSameIdentity ref target
                  then composeTypeHeadRef ref s args'
                  else TVarAppRef ref args'
          TBaseIFWithIdentity identity b -> TBaseWithIdentity identity b
          TBottomIF -> TBottom
          TForallIFRef ref mb body
            | typeBinderRefsSameIdentity ref target ->
                let mb' = fmap (substBoundCaptureLocal target s . fst . unIxPair) mb
                 in TForallRef ref mb' (fst (unIxPair body))
            | replacementMentionsRef ref
            , targetOccursFreeIn (fst (unIxPair body)) ->
                let used =
                      Set.unions
                        [ freeSNames,
                          freeTypeVarAliasNamesType (fst (unIxPair body)),
                          maybe Set.empty (freeTypeVarAliasNamesType . fst . unIxPair) mb,
                          Set.singleton v
                        ]
                    v' = freshNameLike v used
                    ref' = freshCaptureRef ref v' (fst (unIxPair body)) (fmap (fst . unIxPair) mb)
                    body' = substTypeCaptureRef ref (TVarRef ref') (fst (unIxPair body))
                    mb' = fmap (substBoundCaptureLocal target s . fst . unIxPair) mb
                 in TForallRef ref' mb' (substTypeCaptureRef target s body')
            | otherwise ->
                let mb' = fmap (substBoundCaptureLocal target s . fst . unIxPair) mb
                 in TForallRef ref mb' (snd (unIxPair body))
            where
              v = typeBinderRefName ref
          TMuIFRef ref body
            | typeBinderRefsSameIdentity ref target -> TMuRef ref (fst (unIxPair body))
            | replacementMentionsRef ref
            , targetOccursFreeIn (fst (unIxPair body)) ->
                let used =
                      Set.unions
                        [ freeSNames,
                          freeTypeVarAliasNamesType (fst (unIxPair body)),
                          Set.singleton v
                        ]
                    v' = freshNameLike v used
                    ref' = freshCaptureRef ref v' (fst (unIxPair body)) Nothing
                    body' = substTypeCaptureRef ref (TVarRef ref') (fst (unIxPair body))
                 in TMuRef ref' (substTypeCaptureRef target s body')
            | otherwise ->
                TMuRef ref (snd (unIxPair body))
            where
              v = typeBinderRefName ref

substTypeSimpleRef :: TypeBinderRef -> ElabType -> ElabType -> ElabType
substTypeSimpleRef target replacement = paraIx alg
  where
    substBoundSimpleLocal :: TypeBinderRef -> ElabType -> BoundType -> BoundType
    substBoundSimpleLocal target0 replacement0 bound = case bound of
      TArrow a b ->
        TArrow (substTypeSimpleRef target0 replacement0 a) (substTypeSimpleRef target0 replacement0 b)
      TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (substTypeSimpleRef target0 replacement0) args)
      TVarAppRef ref args ->
        let args' = fmap (substTypeSimpleRef target0 replacement0) args
         in if typeBinderRefsSameIdentity ref target0
              then composeTypeHeadRef ref replacement0 args'
              else TVarAppRef ref args'
      TBaseWithIdentity identity b -> TBaseWithIdentity identity b
      TBottom -> TBottom
      TForallRef ref mb body
        | typeBinderRefsSameIdentity ref target0 ->
            let mb' = fmap (substBoundSimpleLocal target0 replacement0) mb
             in TForallRef ref mb' body
        | otherwise ->
            let mb' = fmap (substBoundSimpleLocal target0 replacement0) mb
             in TForallRef ref mb' (substTypeSimpleRef target0 replacement0 body)
      TMuRef ref body
        | typeBinderRefsSameIdentity ref target0 -> TMuRef ref body
        | otherwise -> TMuRef ref (substTypeSimpleRef target0 replacement0 body)

    alg :: TyIF i (IxPair Ty Ty) -> Ty i
    alg ty = case ty of
      TVarIFRef ref
        | typeBinderRefsSameIdentity ref target -> replacement
        | otherwise -> TVarRef ref
      TArrowIF d c -> TArrow (snd (unIxPair d)) (snd (unIxPair c))
      TConIFWithIdentity identity c args -> TConWithIdentity identity c (fmap (snd . unIxPair) args)
      TVarAppIFRef ref args ->
        let args' = fmap (snd . unIxPair) args
         in if typeBinderRefsSameIdentity ref target
              then composeTypeHeadRef ref replacement args'
              else TVarAppRef ref args'
      TBaseIFWithIdentity identity b -> TBaseWithIdentity identity b
      TBottomIF -> TBottom
      TForallIFRef ref mb body
        | typeBinderRefsSameIdentity ref target ->
            let mb' = fmap (substBoundSimpleLocal target replacement . fst . unIxPair) mb
             in TForallRef ref mb' (fst (unIxPair body))
        | otherwise ->
            let mb' = fmap (substBoundSimpleLocal target replacement . fst . unIxPair) mb
             in TForallRef ref mb' (snd (unIxPair body))
      TMuIFRef ref body
        | typeBinderRefsSameIdentity ref target -> TMuRef ref (fst (unIxPair body))
        | otherwise -> TMuRef ref (snd (unIxPair body))

composeTypeHeadRef :: TypeBinderRef -> ElabType -> NE.NonEmpty ElabType -> Ty v
composeTypeHeadRef original replacement args =
  case replacement of
    TVarRef ref -> TVarAppRef ref args
    TVarAppRef ref existingArgs -> TVarAppRef ref (existingArgs <> args)
    TBaseWithIdentity identity con -> TConWithIdentity identity con args
    TConWithIdentity identity con existingArgs -> TConWithIdentity identity con (existingArgs <> args)
    _ -> TVarAppRef original args

-- | Return the first explicit recursive type that violates the M4 v1
-- contractiveness policy.
--
-- Contractive occurrences must be guarded by an arrow or constructor node.
-- `forall` binders do not introduce a guard, but they still shadow the
-- recursive variable when names coincide.
firstNonContractiveRecursiveType :: ElabType -> Maybe ElabType
firstNonContractiveRecursiveType = goType
  where
    goType :: ElabType -> Maybe ElabType
    goType ty = case ty of
      TVarRef _ -> Nothing
      TArrow a b -> goType a <|> goType b
      TConWithIdentity _ _ args -> foldr (\arg acc -> goType arg <|> acc) Nothing args
      TVarAppRef _ args -> foldr (\arg acc -> goType arg <|> acc) Nothing args
      TBaseWithIdentity _ _ -> Nothing
      TBottom -> Nothing
      TForallRef _ mb body -> maybe Nothing goBound mb <|> goType body
      TMuRef ref body
        | muBodyContractive ref body -> goType body
        | otherwise -> Just ty

    goBound :: BoundType -> Maybe ElabType
    goBound bound = case bound of
      TArrow a b -> goType a <|> goType b
      TConWithIdentity _ _ args -> foldr (\arg acc -> goType arg <|> acc) Nothing args
      TVarAppRef _ args -> foldr (\arg acc -> goType arg <|> acc) Nothing args
      TBaseWithIdentity _ _ -> Nothing
      TBottom -> Nothing
      TForallRef _ mb body -> maybe Nothing goBound mb <|> goType body
      TMuRef ref body
        | muBodyContractive ref body -> goType body
        | otherwise -> Just (tyToElab bound)

    muBodyContractive :: TypeBinderRef -> ElabType -> Bool
    muBodyContractive needle = bodyType False False
      where
        bodyType :: Bool -> Bool -> ElabType -> Bool
        bodyType guarded shadowed ty = case ty of
          TVarRef ref -> shadowed || not (typeBinderRefsSameIdentity ref needle) || guarded
          TArrow a b -> bodyType True shadowed a && bodyType True shadowed b
          TConWithIdentity _ _ args -> all (bodyType True shadowed) args
          TVarAppRef ref args ->
            (shadowed || not (typeBinderRefsSameIdentity ref needle) || guarded)
              && all (bodyType guarded shadowed) args
          TBaseWithIdentity _ _ -> True
          TBottom -> True
          TForallRef ref mb body ->
            let shadowed' = shadowed || typeBinderRefsSameIdentity ref needle
                boundOk = maybe True (bodyBound guarded shadowed') mb
             in boundOk && bodyType guarded shadowed' body
          TMuRef ref body ->
            let shadowed' = shadowed || typeBinderRefsSameIdentity ref needle
             in bodyType guarded shadowed' body

        bodyBound :: Bool -> Bool -> BoundType -> Bool
        bodyBound guarded shadowed bound = case bound of
          TArrow a b -> bodyType True shadowed a && bodyType True shadowed b
          TConWithIdentity _ _ args -> all (bodyType True shadowed) args
          TVarAppRef ref args ->
            (shadowed || not (typeBinderRefsSameIdentity ref needle) || guarded)
              && all (bodyType guarded shadowed) args
          TBaseWithIdentity _ _ -> True
          TBottom -> True
          TForallRef ref mb body ->
            let shadowed' = shadowed || typeBinderRefsSameIdentity ref needle
                boundOk = maybe True (bodyBound guarded shadowed') mb
             in boundOk && bodyType guarded shadowed' body
          TMuRef ref body ->
            let shadowed' = shadowed || typeBinderRefsSameIdentity ref needle
             in bodyType guarded shadowed' body

type AlphaEnv = [(TypeBinderRef, TypeBinderRef)]

lookupAlphaRef :: TypeBinderRef -> AlphaEnv -> Maybe TypeBinderRef
lookupAlphaRef ref =
  fmap snd . find (typeBinderRefsSameIdentity ref . fst)

alphaEqRef :: AlphaEnv -> AlphaEnv -> TypeBinderRef -> TypeBinderRef -> Bool
alphaEqRef envL envR left right =
  case lookupAlphaRef left envL of
    Just expectedRight -> typeBinderRefsSameIdentity right expectedRight
    Nothing ->
      case lookupAlphaRef right envR of
        Just expectedLeft -> typeBinderRefsSameIdentity left expectedLeft
        Nothing -> typeBinderRefsSameIdentity left right

typeHeadMatches :: SymbolIdentity -> SymbolIdentity -> Bool
typeHeadMatches =
  typeHeadRefMatches

alphaEqType :: ElabType -> ElabType -> Bool
alphaEqType = go [] []
  where
    headMatches = typeHeadRefMatches

    go envL envR t1 t2 = case (t1, t2) of
      (TVarRef a, TVarRef b) ->
        alphaEqRef envL envR a b
      (TArrow a1 b1, TArrow a2 b2) ->
        go envL envR a1 a2 && go envL envR b1 b2
      (TConWithIdentity identity1 _ args1, TConWithIdentity identity2 _ args2) ->
        headMatches identity1 identity2 && alphaEqArgs envL envR (toList args1) (toList args2)
      (TVarAppRef a args1, TVarAppRef b args2) ->
        alphaEqVar envL envR a b && alphaEqArgs envL envR (toList args1) (toList args2)
      (TBaseWithIdentity identity1 _, TBaseWithIdentity identity2 _) ->
        headMatches identity1 identity2
      (TBottom, TBottom) -> True
      (TForallRef ref1 mb1 body1, TForallRef ref2 mb2 body2) ->
        let envL' = (ref1, ref2) : envL
            envR' = (ref2, ref1) : envR
         in alphaEqMaybeBound envL envR mb1 mb2 && go envL' envR' body1 body2
      (TMuRef ref1 body1, TMuRef ref2 body2) ->
        let envL' = (ref1, ref2) : envL
            envR' = (ref2, ref1) : envR
         in go envL' envR' body1 body2
      _ -> False

    alphaEqArgs envL envR as bs = case (as, bs) of
      ([], []) -> True
      (a : as', b : bs') -> go envL envR a b && alphaEqArgs envL envR as' bs'
      _ -> False

    alphaEqVar = alphaEqRef

    alphaEqMaybeBound envL envR mb1 mb2 = case (mb1, mb2) of
      (Nothing, Nothing) -> True
      (Just b1, Just b2) -> alphaEqBound envL envR b1 b2
      _ -> False

    alphaEqBound envL envR b1 b2 = case (b1, b2) of
      (TArrow a1 b1', TArrow a2 b2') ->
        go envL envR a1 a2 && go envL envR b1' b2'
      (TConWithIdentity identity1 _ args1, TConWithIdentity identity2 _ args2) ->
        headMatches identity1 identity2 && alphaEqArgs envL envR (toList args1) (toList args2)
      (TVarAppRef a args1, TVarAppRef b args2) ->
        alphaEqVar envL envR a b && alphaEqArgs envL envR (toList args1) (toList args2)
      (TBaseWithIdentity identity1 _, TBaseWithIdentity identity2 _) ->
        headMatches identity1 identity2
      (TBottom, TBottom) -> True
      (TForallRef ref1 mb1 body1, TForallRef ref2 mb2 body2) ->
        let envL' = (ref1, ref2) : envL
            envR' = (ref2, ref1) : envR
         in alphaEqMaybeBound envL envR mb1 mb2 && go envL' envR' body1 body2
      (TMuRef ref1 body1, TMuRef ref2 body2) ->
        let envL' = (ref1, ref2) : envL
            envR' = (ref2, ref1) : envR
         in go envL' envR' body1 body2
      _ -> False

-- | Alpha-equivalence at a construction boundary where structural data
-- ownership is part of the endpoint certificate.  Ordinary graph/generated
-- binders remain alpha-renamable, but a structural self/result binder may only
-- align with the same structural owner and role.
--
-- Keep this stricter relation separate from 'alphaEqType': ordinary xMLF
-- binders are still alpha-renamable, while construction sites can opt into the
-- identity-preserving relation before emitting explicit type applications.
alphaEqTypePreservingStructuralBinders :: ElabType -> ElabType -> Bool
alphaEqTypePreservingStructuralBinders left right =
  alphaEqType left right && structuralBindersAgree left right
  where
    structuralBindersAgree :: Ty v -> Ty v -> Bool
    structuralBindersAgree leftTy rightTy =
      case (leftTy, rightTy) of
        (TVarRef {}, TVarRef {}) -> True
        (TArrow leftDomain leftCodomain, TArrow rightDomain rightCodomain) ->
          structuralBindersAgree leftDomain rightDomain
            && structuralBindersAgree leftCodomain rightCodomain
        (TConWithIdentity _ _ leftArgs, TConWithIdentity _ _ rightArgs) ->
          allStructuralBindersAgree (toList leftArgs) (toList rightArgs)
        (TVarAppRef _ leftArgs, TVarAppRef _ rightArgs) ->
          allStructuralBindersAgree (toList leftArgs) (toList rightArgs)
        (TBaseWithIdentity {}, TBaseWithIdentity {}) -> True
        (TBottom, TBottom) -> True
        (TForallRef leftRef leftBound leftBody, TForallRef rightRef rightBound rightBody) ->
          structuralBinderIdentityAgrees leftRef rightRef
            && maybeStructuralBindersAgree leftBound rightBound
            && structuralBindersAgree leftBody rightBody
        (TMuRef leftRef leftBody, TMuRef rightRef rightBody) ->
          structuralBinderIdentityAgrees leftRef rightRef
            && structuralBindersAgree leftBody rightBody
        _ -> False

    allStructuralBindersAgree leftTypes rightTypes =
      length leftTypes == length rightTypes
        && and
          ( zipWith
              structuralBindersAgree
              leftTypes
              rightTypes
          )

    maybeStructuralBindersAgree leftBound rightBound =
      case (leftBound, rightBound) of
        (Nothing, Nothing) -> True
        (Just leftTy, Just rightTy) ->
          structuralBindersAgree leftTy rightTy
        _ -> False

    structuralBinderIdentityAgrees leftRef rightRef =
      case
          ( typeBinderIdentityStructural (typeBinderRefIdentity leftRef),
            typeBinderIdentityStructural (typeBinderRefIdentity rightRef)
          )
        of
          (Nothing, Nothing) -> True
          (Just leftIdentity, Just rightIdentity) ->
            leftIdentity == rightIdentity
          _ -> False

-- | Identity-sensitive alpha-equivalence for an exact occurrence refresh.
-- Forall declarations may retain an already constructed lexical
-- presentation, but a mu declaration selected by the current sibling-scope
-- constructor must already name that exact copy or be rewritten together
-- with the occurrence payload.
--
-- This is deliberately not the general equality for recursive types:
-- lexical mu binders are alpha-renamable under ordinary alpha-equivalence.
-- The stricter relation exists only to decide whether an exact
-- construction-time refresh still has work to do.
alphaEqTypePreservingRecursiveBinders :: ElabType -> ElabType -> Bool
alphaEqTypePreservingRecursiveBinders left right =
  alphaEqType left right
    && recursiveBindersAgree left right
  where
    recursiveBindersAgree :: Ty v -> Ty v -> Bool
    recursiveBindersAgree leftTy rightTy =
      case (leftTy, rightTy) of
        (TVarRef {}, TVarRef {}) -> True
        (TArrow leftDomain leftCodomain, TArrow rightDomain rightCodomain) ->
          recursiveBindersAgree leftDomain rightDomain
            && recursiveBindersAgree leftCodomain rightCodomain
        (TConWithIdentity _ _ leftArgs, TConWithIdentity _ _ rightArgs) ->
          allRecursiveBindersAgree (toList leftArgs) (toList rightArgs)
        (TVarAppRef _ leftArgs, TVarAppRef _ rightArgs) ->
          allRecursiveBindersAgree (toList leftArgs) (toList rightArgs)
        (TBaseWithIdentity {}, TBaseWithIdentity {}) -> True
        (TBottom, TBottom) -> True
        (TForallRef _ leftBound leftBody, TForallRef _ rightBound rightBody) ->
          maybeRecursiveBindersAgree leftBound rightBound
            && recursiveBindersAgree leftBody rightBody
        (TMuRef leftRef leftBody, TMuRef rightRef rightBody) ->
          typeBinderRefsSameIdentity leftRef rightRef
            && recursiveBindersAgree leftBody rightBody
        _ -> False

    allRecursiveBindersAgree leftTypes rightTypes =
      length leftTypes == length rightTypes
        && and
          ( zipWith
              recursiveBindersAgree
              leftTypes
              rightTypes
          )

    maybeRecursiveBindersAgree leftBound rightBound =
      case (leftBound, rightBound) of
        (Nothing, Nothing) -> True
        (Just leftTy, Just rightTy) ->
          recursiveBindersAgree leftTy rightTy
        _ -> False

stripChurchForallsType :: ElabType -> ([TypeBinderRef], ElabType)
stripChurchForallsType ty =
  case ty of
    TForallRef ref Nothing body ->
      let (refs, core) = stripChurchForallsType body
       in (ref : refs, core)
    _ -> ([], ty)

churchSelfAliasVariants :: [TypeBinderRef] -> TypeBinderRef -> ElabType -> [ElabType]
churchSelfAliasVariants quantRefs muRef core
  | any (typeBinderRefsSameIdentity muRef) (freeTypeVarRefsType core) = [core]
  | otherwise =
      core
        : [ substTypeSimpleRef alias (TVarRef muRef) core
            | alias <- churchFreeRefsExcept quantRefs core
          ]

churchFreeRefsExcept :: [TypeBinderRef] -> ElabType -> [TypeBinderRef]
churchFreeRefsExcept excluded ty =
  [ ref
    | ref <- freeTypeVarRefsType ty,
      not (any (typeBinderRefsSameIdentity ref) excluded)
  ]

unionTypeBinderRefs :: [TypeBinderRef] -> [TypeBinderRef] -> [TypeBinderRef]
unionTypeBinderRefs left right =
  foldr insertRef right left
  where
    insertRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

churchNormalForms :: Ty v -> [([TypeBinderRef], Ty v)]
churchNormalForms ty =
  case ty of
    TVarRef {} -> unchanged
    TArrow domain codomain ->
      [ (unionTypeBinderRefs domainRefs codomainRefs, TArrow domain' codomain')
        | (domainRefs, domain') <- churchNormalForms domain,
          (codomainRefs, codomain') <- churchNormalForms codomain
      ]
    TConWithIdentity identity con args ->
      [ (unionMany (fmap fst normalizedArgs), TConWithIdentity identity con (fmap snd normalizedArgs))
        | normalizedArgs <- traverse churchNormalForms args
      ]
    TVarAppRef ref args ->
      [ (unionMany (fmap fst normalizedArgs), TVarAppRef ref (fmap snd normalizedArgs))
        | normalizedArgs <- traverse churchNormalForms args
      ]
    TBaseWithIdentity {} -> unchanged
    TBottom -> unchanged
    TForallRef ref mbBound body ->
      [ ( unionTypeBinderRefs boundRefs bodyRefs,
          TForallRef ref mbBound' body'
        )
        | (boundRefs, mbBound') <- churchNormalMaybeBoundForms mbBound,
          (bodyRefs, body') <- churchNormalForms body
      ]
    TMuRef ref body ->
      let (localRefs, core) = stripChurchForallsType body
       in [ ( matchableRefs,
              TMuRef ref coreVariant
            )
            | (nestedRefs, core') <- churchNormalForms core,
              let matchableRefs = unionTypeBinderRefs localRefs nestedRefs,
              coreVariant <- churchSelfAliasVariants matchableRefs ref core'
          ]
  where
    unchanged = [([], ty)]

churchNormalMaybeBoundForms :: Maybe BoundType -> [([TypeBinderRef], Maybe BoundType)]
churchNormalMaybeBoundForms mbBound =
  case mbBound of
    Nothing -> [([], Nothing)]
    Just bound ->
      [ (refs, Just bound')
        | (refs, bound') <- churchNormalForms bound
      ]

unionMany :: Foldable f => f [TypeBinderRef] -> [TypeBinderRef]
unionMany =
  foldr unionTypeBinderRefs []

{- Note [churchMuEquivalent]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Church-encoded recursive ADTs produce μ types of the form

  μself. ∀result. body(self, result)

When the presolver processes constraint graphs, it correctly instantiates
the leading ∀result inside the μ body.  The reified inferred type is then

  μself'. body'(selfAlias, result')

where result' is a free variable (the instantiated result binder) and
selfAlias may differ from self' due to presolution variable aliasing.

This helper recognises the two representations as equivalent by:
  1. Stripping leading unbounded TForall from each μ body.
  2. Repairing a "self alias" — a free var in the body that structurally
     occupies the self-reference position but has a different name than the
     μ binder (a presolution/reification artifact).
  3. Using matchTypeRefs with the stripped quantifier refs as matchable
     placeholders to align the remaining free variables.

IMPORTANT: This is NOT a general semantic equivalence.  It is a narrow
Church-encoding-specific comparison, intentionally kept out of alphaEqType.
-}

-- | Check whether two types are equivalent Church-encoded μ types.
-- Handles the specific mismatch between annotated μ types (which retain
-- inner ∀result) and inferred μ types (where ∀result was instantiated and
-- the self-variable may be aliased).
churchMuEquivalent :: ElabType -> ElabType -> Bool
churchMuEquivalent t1 t2 =
  case (t1, t2) of
    (TMuRef {}, TMuRef {}) ->
      let forms1 = churchNormalForms t1
          forms2 = churchNormalForms t2
       in or
            [ tryMatch
                (unionTypeBinderRefs refs1 refs2)
                normalized1
                normalized2
              | (refs1, normalized1) <- forms1,
                (refs2, normalized2) <- forms2
            ]
    _ -> False
  where
    tryMatch :: [TypeBinderRef] -> ElabType -> ElabType -> Bool
    tryMatch matchableVars lhs rhs =
      alphaEqType lhs rhs
        || isRight (matchTypeRefs matchableVars lhs rhs)
        || isRight (matchTypeRefs matchableVars rhs lhs)

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _ = False

-- | Structural equality that falls back to 'churchMuEquivalent' at TMu-vs-TMu
-- junctions.  Keeps 'alphaEqType' strict while allowing Church-μ relaxation
-- at every nesting depth.
churchAwareEqType :: ElabType -> ElabType -> Bool
churchAwareEqType = go [] []
  where
    tryMatch :: [TypeBinderRef] -> ElabType -> ElabType -> Bool
    tryMatch matchableVars lhs rhs =
      alphaEqType lhs rhs
        || isRight (matchTypeRefs matchableVars lhs rhs)
        || isRight (matchTypeRefs matchableVars rhs lhs)

    churchMuMatchesCore :: ElabType -> ElabType -> Bool
    churchMuMatchesCore muTy@(TMuRef ref body) otherTy =
      let muTy' = tyToElab muTy
          (quantVars, coreBody) = stripChurchForallsType (tyToElab body)
          (_, unfoldedCoreBody) = stripChurchForallsType (substTypeSimpleRef ref muTy' (tyToElab body))
          candidateBodies =
            churchSelfAliasVariants quantVars ref coreBody
              ++ [ candidate
                 | not (alphaEqType coreBody unfoldedCoreBody)
                 , candidate <- churchSelfAliasVariants quantVars ref unfoldedCoreBody
                 ]
          (otherQuantVars, otherCore) = stripChurchForallsType otherTy
          -- A leading forall in the unfolded Church body is representation
          -- detail.  A forall wrapped around a complete recursive type is an
          -- explicit xMLF boundary and still requires an InstElim/InstApp;
          -- treating the latter as Church equality erases that computation.
          explicitForallWrapsRecursiveType =
            leadingForallsWrapRecursiveType otherTy
       in not explicitForallWrapsRecursiveType
            && or
              [ tryMatch quantVars candidate otherTy
                  || (not (null otherQuantVars) && tryMatch quantVars candidate otherCore)
                | candidate <- candidateBodies
              ]
    churchMuMatchesCore _ _ = False

    unfoldMuOnce :: Ty v -> Maybe ElabType
    unfoldMuOnce muTy@(TMuRef ref body) =
      let muTy' = tyToElab muTy
          unfolded = substTypeSimpleRef ref muTy' (tyToElab body)
       in if alphaEqType unfolded muTy' then Nothing else Just unfolded
    unfoldMuOnce _ = Nothing

    leadingForallsWrapRecursiveType ty =
      let (refs, core) = stripChurchForallsType ty
       in not (null refs)
            && case core of
              TMuRef {} -> True
              _ -> False

    go envL envR t1 t2 = case (t1, t2) of
      (TVarRef a, TVarRef b) ->
        alphaEqRef envL envR a b
      (TArrow a1 b1, TArrow a2 b2) ->
        go envL envR a1 a2 && go envL envR b1 b2
      (TConWithIdentity identity1 _ args1, TConWithIdentity identity2 _ args2) ->
        typeHeadMatches identity1 identity2 && eqArgs envL envR (toList args1) (toList args2)
      (TVarAppRef a args1, TVarAppRef b args2) ->
        eqVar envL envR a b && eqArgs envL envR (toList args1) (toList args2)
      (TBaseWithIdentity identity1 _, TBaseWithIdentity identity2 _) ->
        typeHeadMatches identity1 identity2
      (TBottom, TBottom) -> True
      (TForallRef ref1 mb1 body1, TForallRef ref2 mb2 body2) ->
        let envL' = (ref1, ref2) : envL
            envR' = (ref2, ref1) : envR
         in eqMaybeBound envL envR mb1 mb2 && go envL' envR' body1 body2
      (TMuRef ref1 body1, TMuRef ref2 body2) ->
        let envL' = (ref1, ref2) : envL
            envR' = (ref2, ref1) : envR
         in go envL' envR' body1 body2
              || churchMuEquivalent t1 t2
      (TMuRef {}, _) ->
        not (leadingForallsWrapRecursiveType t2)
          && ( churchMuMatchesCore t1 t2
                 || maybe False (\unfolded -> go envL envR unfolded t2) (unfoldMuOnce t1)
             )
      (_, TMuRef {}) ->
        not (leadingForallsWrapRecursiveType t1)
          && ( churchMuMatchesCore t2 t1
                 || maybe False (\unfolded -> go envL envR t1 unfolded) (unfoldMuOnce t2)
             )
      _ -> False

    eqArgs envL envR as bs = case (as, bs) of
      ([], []) -> True
      (a : as', b : bs') -> go envL envR a b && eqArgs envL envR as' bs'
      _ -> False

    eqVar = alphaEqRef

    eqMaybeBound envL envR mb1 mb2 = case (mb1, mb2) of
      (Nothing, Nothing) -> True
      (Just b1, Just b2) -> eqBound envL envR b1 b2
      _ -> False

    eqBound :: AlphaEnv -> AlphaEnv -> BoundType -> BoundType -> Bool
    eqBound envL envR b1 b2 = case (b1, b2) of
      (TArrow a1 b1', TArrow a2 b2') ->
        go envL envR a1 a2 && go envL envR b1' b2'
      (TConWithIdentity identity1 _ args1, TConWithIdentity identity2 _ args2) ->
        typeHeadMatches identity1 identity2 && eqArgs envL envR (toList args1) (toList args2)
      (TVarAppRef a args1, TVarAppRef b args2) ->
        eqVar envL envR a b && eqArgs envL envR (toList args1) (toList args2)
      (TBaseWithIdentity identity1 _, TBaseWithIdentity identity2 _) ->
        typeHeadMatches identity1 identity2
      (TBottom, TBottom) -> True
      (TForallRef ref1 mb1 body1, TForallRef ref2 mb2 body2) ->
        let envL' = (ref1, ref2) : envL
            envR' = (ref2, ref1) : envR
         in eqMaybeBound envL envR mb1 mb2 && go envL' envR' body1 body2
      (TMuRef ref1 body1, TMuRef ref2 body2) ->
        let envL' = (ref1, ref2) : envL
            envR' = (ref2, ref1) : envR
         in go envL' envR' body1 body2
              || churchMuEquivalent (TMuRef ref1 body1) (TMuRef ref2 body2)
      (TMuRef {}, _) ->
        churchMuMatchesCore (tyToElab b1) (tyToElab b2)
          || maybe False (\unfolded -> go envL envR unfolded (tyToElab b2)) (unfoldMuOnce b1)
      (_, TMuRef {}) ->
        churchMuMatchesCore (tyToElab b2) (tyToElab b1)
          || maybe False (\unfolded -> go envL envR (tyToElab b1) unfolded) (unfoldMuOnce b2)
      _ -> False

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _ = False

-- | Equality for the one operational Church representation transition that
-- construction may observe: one recursive body still owns its leading
-- result forall while the other has instantiated that forall.  Two
-- forall-preserving recursive bodies are ordinary source types and therefore
-- remain alpha-strict; symmetrically normalizing both would turn unrelated
-- result algebras into the same type.
--
-- Explicit foralls outside the recursive body are never representation
-- detail.  The recursive cases below deliberately occur only at matching
-- nested type positions.
churchRepresentationEqType :: ElabType -> ElabType -> Bool
churchRepresentationEqType = go [] []
  where
    go envL envR (TVarRef leftRef) (TVarRef rightRef) =
      alphaEqRef envL envR leftRef rightRef
    go envL envR
      (TArrow leftDomain leftCodomain)
      (TArrow rightDomain rightCodomain) =
        go envL envR leftDomain rightDomain
          && go envL envR leftCodomain rightCodomain
    go envL envR
      (TConWithIdentity leftIdentity _ leftArgs)
      (TConWithIdentity rightIdentity _ rightArgs) =
        typeHeadMatches leftIdentity rightIdentity
          && pairwise (go envL envR) (toList leftArgs) (toList rightArgs)
    go envL envR
      (TVarAppRef leftRef leftArgs)
      (TVarAppRef rightRef rightArgs) =
        alphaEqRef envL envR leftRef rightRef
          && pairwise (go envL envR) (toList leftArgs) (toList rightArgs)
    go _envL _envR
      (TBaseWithIdentity leftIdentity _)
      (TBaseWithIdentity rightIdentity _) =
        typeHeadMatches leftIdentity rightIdentity
    go _envL _envR TBottom TBottom = True
    go envL envR
      (TForallRef leftRef leftBound leftBody)
      (TForallRef rightRef rightBound rightBody) =
        equivalentBounds envL envR leftBound rightBound
          && go
            ((leftRef, rightRef) : envL)
            ((rightRef, leftRef) : envR)
            leftBody
            rightBody
    go envL envR left@(TMuRef leftRef leftBody) right@(TMuRef rightRef rightBody) =
      ( churchRepresentationTransition left right
          && churchMuEquivalent left right
      )
        || go
          ((leftRef, rightRef) : envL)
          ((rightRef, leftRef) : envR)
          leftBody
          rightBody
    go _envL _envR _left _right = False

    pairwise relation left right =
      length left == length right
        && and (zipWith relation left right)

    equivalentBounds _envL _envR Nothing Nothing = True
    equivalentBounds envL envR (Just leftBound) (Just rightBound) =
      go envL envR (tyToElab leftBound) (tyToElab rightBound)
    equivalentBounds _envL _envR _leftBound _rightBound = False

churchRepresentationTransition :: ElabType -> ElabType -> Bool
churchRepresentationTransition
  (TMuRef _ leftBody)
  (TMuRef _ rightBody) =
    leadingChurchResultForall leftBody
      /= leadingChurchResultForall rightBody
  where
    leadingChurchResultForall body =
      case body of
        TForallRef _ Nothing _ -> True
        _ -> False
churchRepresentationTransition _ _ = False

matchTypeRefs ::
  [TypeBinderRef] ->
  ElabType ->
  ElabType ->
  Either ElabError (Map.Map TypeBinderRef ElabType)
matchTypeRefs = matchTypeRefsWithChurchNormalForms False

-- | Match a source-owned type pattern against a presolver presentation while
-- retaining the substitution for the requested source binders.  Unlike
-- 'matchTypeRefs', this matcher admits only the narrow Church-encoded mu
-- normalization described in Note [churchMuEquivalent].  Every successful
-- normal form must induce the same requested substitution; otherwise the
-- identity quotient is rejected as ambiguous.
--
-- This is deliberately directional: @pattern@ owns @binderRefs@ and the
-- returned types come from @target@.  In particular, an annotated Church
-- binder can be mapped to the graph identity that represents it after the
-- presolver instantiated an inner result forall.
matchChurchAwareTypeRefs ::
  [TypeBinderRef] ->
  ElabType ->
  ElabType ->
  Either ElabError (Map.Map TypeBinderRef ElabType)
matchChurchAwareTypeRefs = matchTypeRefsWithChurchNormalForms True

matchTypeRefsWithChurchNormalForms ::
  Bool ->
  [TypeBinderRef] ->
  ElabType ->
  ElabType ->
  Either ElabError (Map.Map TypeBinderRef ElabType)
matchTypeRefsWithChurchNormalForms allowChurchNormalForms binderRefs = goMatch [] Map.empty
  where
    goMatch env subst tyP tyT = case (tyP, tyT) of
      (TVarRef ref, _)
        | Just binder <- matchableRef ref ->
        case Map.lookup binder subst of
          Nothing -> Right (Map.insert binder tyT subst)
          Just ty0 ->
            if
                alphaEqType ty0 tyT
                  || ( allowChurchNormalForms
                         && churchRepresentationEqType ty0 tyT
                     )
              then Right subst
              else Left (InstantiationError "matchType: binder mismatch")
      (TVarRef ref, TVarRef ref')
        | boundVarMatches env ref ref' -> Right subst
      (TArrow a b, TArrow a' b') -> do
        subst1 <- goMatch env subst a a'
        goMatch env subst1 b b'
      (TConWithIdentity identity0 _ args0, TConWithIdentity identity1 _ args1)
        | typeHeadMatches identity0 identity1 ->
            matchArgs env subst (toList args0) (toList args1)
      (TVarAppRef ref argsP, _)
        | Just binder <- matchableRef ref ->
            matchVarHead env subst binder (toList argsP) tyT
      (TVarAppRef ref args0, TVarAppRef ref' args1)
        | boundVarMatches env ref ref' ->
            matchArgs env subst (toList args0) (toList args1)
      (TBaseWithIdentity identity0 _, TBaseWithIdentity identity1 _)
        | typeHeadMatches identity0 identity1 -> Right subst
      (TBottom, TBottom) -> Right subst
      (TForallRef ref mb b, TForallRef ref' mb' b') -> do
        subst1 <- case (mb, mb') of
          (Nothing, Nothing) -> Right subst
          (Just x, Just y) -> matchBound env subst x y
          _ -> Left (InstantiationError "matchType: forall bound mismatch")
        goMatch ((ref, ref') : env) subst1 b b'
      (TMuRef ref b, TMuRef ref' b') ->
        case goMatch ((ref, ref') : env) subst b b' of
          Right matched -> Right matched
          Left directFailure
            | allowChurchNormalForms
            , churchRepresentationTransition tyP tyT ->
                matchChurchNormalForms subst tyP tyT directFailure
            | otherwise -> Left directFailure
      _ -> Left (InstantiationError "matchType: structure mismatch")

    matchArgs env subst0 argsP argsT = case (argsP, argsT) of
      ([], []) -> Right subst0
      (a : as, b : bs) -> do
        subst1 <- goMatch env subst0 a b
        matchArgs env subst1 as bs
      _ -> Left (InstantiationError "matchType: structure mismatch")

    matchVarHead env subst0 v argsP tyT = do
      (targetHead, targetArgs) <-
        maybe
          (Left (InstantiationError "matchType: higher-kinded target mismatch"))
          Right
          (unapplyTypeHead tyT)
      let prefixLen = length targetArgs - length argsP
      if prefixLen < 0
        then Left (InstantiationError "matchType: higher-kinded arity mismatch")
        else do
          let (prefixArgs, suffixArgs) = splitAt prefixLen targetArgs
              replacement = applyHeadArgs targetHead prefixArgs
          subst1 <- bindHeadSubst v replacement subst0
          matchArgs env subst1 argsP suffixArgs

    bindHeadSubst binder replacement subst0 =
      case Map.lookup binder subst0 of
        Nothing -> Right (Map.insert binder replacement subst0)
        Just existing
          | alphaEqType existing replacement -> Right subst0
          | otherwise -> Left (InstantiationError "matchType: higher-kinded binder mismatch")

    boundVarMatches env ref ref' =
      case lookupAlphaRef ref env of
        Just expected -> typeBinderRefsSameIdentity ref' expected
        Nothing -> typeBinderRefsSameIdentity ref ref'

    matchableRef ref =
      find (typeBinderRefsSameIdentity ref) binderRefs

    matchChurchNormalForms subst patternMu targetMu directFailure =
      case distinctSubstitutions candidates of
        [] -> Left directFailure
        [candidate] -> Right candidate
        _ -> Left (InstantiationError "matchType: ambiguous Church-normal-form binder match")
      where
        candidates =
          [ merged
          | (localPatternRefs, patternForm) <- churchNormalForms patternMu
          , (_, targetForm) <- churchNormalForms targetMu
          , Right matched <-
              [ matchTypeRefs
                  (unionTypeBinderRefs binderRefs localPatternRefs)
                  patternForm
                  targetForm
              ]
          , Right merged <- [mergeRequestedSubstitution subst matched]
          ]

    mergeRequestedSubstitution =
      Map.foldlWithKey' mergeOne . Right
      where
        mergeOne acc binder replacement
          | not (any (typeBinderRefsSameIdentity binder) binderRefs) = acc
          | otherwise = do
              subst <- acc
              case Map.lookup binder subst of
                Nothing -> Right (Map.insert binder replacement subst)
                Just existing
                  | alphaEqType existing replacement
                      || ( allowChurchNormalForms
                             && churchRepresentationEqType
                               existing
                               replacement
                         ) ->
                      Right subst
                  | otherwise -> Left (InstantiationError "matchType: Church binder mismatch")

    distinctSubstitutions =
      foldr insertDistinctSubstitution []

    insertDistinctSubstitution subst substs
      | any (substitutionsEquivalent subst) substs = substs
      | otherwise = subst : substs

    substitutionsEquivalent left right =
      Map.size left == Map.size right
        && all
          (\(binder, replacement) ->
            maybe False (alphaEqType replacement) (Map.lookup binder right)
          )
          (Map.toList left)

    unapplyTypeHead ty = case ty of
      TVarRef ref -> Just (TVarRef ref, [])
      TVarAppRef ref args -> Just (TVarRef ref, toList args)
      TBaseWithIdentity identity base -> Just (TBaseWithIdentity identity base, [])
      TConWithIdentity identity con args -> Just (TBaseWithIdentity identity con, toList args)
      _ -> Nothing

    applyHeadArgs headTy args =
      case NE.nonEmpty args of
        Nothing -> headTy
        Just argsNE ->
          let (fallbackRef, _) = freshTypeBinderRef "_" (identityGeneratorAfterType headTy)
           in composeTypeHeadRef fallbackRef headTy argsNE

    matchBound env subst boundP boundT = case (boundP, boundT) of
      (TArrow a b, TArrow a' b') -> do
        subst1 <- goMatch env subst a a'
        goMatch env subst1 b b'
      (TConWithIdentity identity0 _ args0, TConWithIdentity identity1 _ args1)
        | typeHeadMatches identity0 identity1 ->
            matchArgs env subst (toList args0) (toList args1)
      (TVarAppRef ref argsP, _)
        | Just binder <- matchableRef ref ->
            matchVarHead env subst binder (toList argsP) (tyToElab boundT)
      (TVarAppRef ref args0, TVarAppRef ref' args1)
        | boundVarMatches env ref ref' ->
            matchArgs env subst (toList args0) (toList args1)
      (TBaseWithIdentity identity0 _, TBaseWithIdentity identity1 _)
        | typeHeadMatches identity0 identity1 -> Right subst
      (TBottom, TBottom) -> Right subst
      (TForallRef ref mb b, TForallRef ref' mb' b') -> do
        subst1 <- case (mb, mb') of
          (Nothing, Nothing) -> Right subst
          (Just x, Just y) -> matchBound env subst x y
          _ -> Left (InstantiationError "matchType: forall bound mismatch")
        goMatch ((ref, ref') : env) subst1 b b'
      (TMuRef ref b, TMuRef ref' b') ->
        goMatch ((ref, ref') : env) subst b b'
      _ -> Left (InstantiationError "matchType: structure mismatch")

resolveBaseBoundForInstConstraint ::
  Constraint p ->
  (NodeId -> NodeId) ->
  NodeId ->
  Maybe NodeId
resolveBaseBoundForInstConstraint constraint canonical start =
  let nodes = cNodes constraint
      goResolve visited nid0 =
        let nid = canonical nid0
            key = getNodeId nid
         in if IntSet.member key visited
              then Nothing
              else case lookupNodeIn nodes nid of
                Just TyBase {} -> Just nid
                Just TyBottom {} -> Just nid
                Just TyVar {} ->
                  case VarStore.lookupVarBound constraint nid of
                    Just bnd -> goResolve (IntSet.insert key visited) bnd
                    Nothing -> Nothing
                _ -> Nothing
   in goResolve IntSet.empty start

resolveBaseBoundForInstSolved :: Solved.Solved -> NodeId -> Maybe NodeId
resolveBaseBoundForInstSolved solved =
  let constraint = Solved.originalConstraint solved
      canonical = Solved.canonical solved
   in resolveBaseBoundForInstConstraint constraint canonical

resolveBoundBodyConstraint ::
  (NodeId -> NodeId) ->
  Constraint p ->
  IntSet.IntSet ->
  NodeId ->
  NodeId
resolveBoundBodyConstraint canonical constraint visited0 start =
  let go visited nid0 =
        let nid = canonical nid0
            key = getNodeId nid
         in if IntSet.member key visited
              then nid
              else case VarStore.lookupVarBound constraint nid of
                Just bnd -> go (IntSet.insert key visited) bnd
                Nothing -> nid
   in go visited0 start

inlineBaseBoundsType :: Constraint p -> (NodeId -> NodeId) -> ElabType -> ElabType
inlineBaseBoundsType constraint canonical = goType []
  where
    goType :: [TypeBinderRef] -> Ty v -> Ty v
    goType boundRefs ty = case ty of
      TVarRef ref -> inlineVarRef boundRefs ref
      TArrow a b -> TArrow (goType boundRefs a) (goType boundRefs b)
      TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (goType boundRefs) args)
      TVarAppRef ref args ->
        let args' = fmap (goType boundRefs) args
         in inlineVarAppRef boundRefs ref args'
      TBaseWithIdentity identity b -> TBaseWithIdentity identity b
      TBottom -> TBottom
      TForallRef ref mb body ->
        let boundRefs' = insertBoundRef ref boundRefs
         in TForallRef ref (fmap (goBound boundRefs') mb) (goType boundRefs' body)
      TMuRef ref body ->
        let boundRefs' = insertBoundRef ref boundRefs
         in TMuRef ref (goType boundRefs' body)

    goBound :: [TypeBinderRef] -> BoundType -> BoundType
    goBound boundRefs bound = case bound of
      TArrow a b -> TArrow (goType boundRefs a) (goType boundRefs b)
      TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (goType boundRefs) args)
      TVarAppRef ref args ->
        let args' = fmap (goType boundRefs) args
         in inlineVarAppRef boundRefs ref args'
      TBaseWithIdentity identity b -> TBaseWithIdentity identity b
      TBottom -> TBottom
      TForallRef ref mb body ->
        let boundRefs' = insertBoundRef ref boundRefs
         in TForallRef ref (fmap (goBound boundRefs') mb) (goType boundRefs' body)
      TMuRef ref body ->
        let boundRefs' = insertBoundRef ref boundRefs
         in TMuRef ref (goType boundRefs' body)

    inlineVarRef :: [TypeBinderRef] -> TypeBinderRef -> ElabType
    inlineVarRef boundRefs ref
      | boundRefMember ref boundRefs = TVarRef ref
      | otherwise =
          case resolvedBaseBound ref of
            Just (Left headRef) -> baseWithIdentity headRef
            Just (Right ()) -> TBottom
            Nothing -> TVarRef ref

    inlineVarAppRef :: [TypeBinderRef] -> TypeBinderRef -> NE.NonEmpty ElabType -> Ty v
    inlineVarAppRef boundRefs ref args
      | boundRefMember ref boundRefs = TVarAppRef ref args
      | otherwise =
          case resolvedBaseBound ref of
            Just (Left headRef) -> conWithIdentity headRef args
            _ -> TVarAppRef ref args

    resolvedBaseBound :: TypeBinderRef -> Maybe (Either (SymbolIdentity, BaseTy) ())
    resolvedBaseBound ref = do
      nid <- refNodeId ref
      baseN <- resolveBaseBoundForInstConstraint constraint canonical nid
      case NodeAccess.lookupNode constraint baseN of
        Just TyBase {tnBaseIdentity = identity, tnBase = b} -> Just (Left (identity, b))
        Just TyBottom {} -> Just (Right ())
        _ -> Nothing

    baseWithIdentity (identity, base) =
      TBaseWithIdentity identity base

    conWithIdentity (identity, base) args =
      TConWithIdentity identity base args

    refNodeId :: TypeBinderRef -> Maybe NodeId
    refNodeId =
      typeBinderRefNode

    boundRefMember :: TypeBinderRef -> [TypeBinderRef] -> Bool
    boundRefMember ref =
      any (typeBinderRefsSameIdentity ref)

    insertBoundRef :: TypeBinderRef -> [TypeBinderRef] -> [TypeBinderRef]
    insertBoundRef ref refs
      | boundRefMember ref refs = refs
      | otherwise = ref : refs

-- | Inline alias/bound nodes in an ElabType using the supplied lookup and reify
-- functions. This is the shared implementation for scope-aware bound/alias
-- inlining; callers can wrap it with concrete environment data.
inlineAliasBoundsWithBy ::
  Bool ->
  (NodeId -> NodeId) ->
  NodeMap TyNode ->
  (NodeId -> Maybe NodeId) ->
  (NodeId -> Either err ElabType) ->
  ElabType ->
  ElabType
inlineAliasBoundsWithBy fallbackToBottom canonical nodes lookupBound reifyBound =
  inlineAliasBoundsWithBySeen
    fallbackToBottom
    canonical
    nodes
    lookupBound
    (\_ nid -> reifyBound nid)

inlineAliasBoundsWithBySeen ::
  Bool ->
  (NodeId -> NodeId) ->
  NodeMap TyNode ->
  (NodeId -> Maybe NodeId) ->
  (IntSet.IntSet -> NodeId -> Either err ElabType) ->
  ElabType ->
  ElabType
inlineAliasBoundsWithBySeen fallbackToBottom canonical nodes lookupBound reifyBound =
  inlineAliasBoundsWithBySeenProtected
    []
    fallbackToBottom
    canonical
    nodes
    lookupBound
    reifyBound

-- | Inline graph aliases while treating the supplied free references as
-- ambient binders.  Exact construction endpoints use this to prevent a
-- lexically captured graph identity from being replaced by its lower bound.
inlineAliasBoundsWithBySeenProtected ::
  [TypeBinderRef] ->
  Bool ->
  (NodeId -> NodeId) ->
  NodeMap TyNode ->
  (NodeId -> Maybe NodeId) ->
  (IntSet.IntSet -> NodeId -> Either err ElabType) ->
  ElabType ->
  ElabType
inlineAliasBoundsWithBySeenProtected protectedRefs fallbackToBottom canonical nodes lookupBound reifyBound =
  goAlias IntSet.empty protectedRefs
  where
    goAlias seen boundRefs ty = case ty of
      TVarRef ref
        | boundRefMember ref boundRefs -> ty
        | otherwise ->
            case typeBinderRefNode ref of
              Just nid ->
                let nidC = canonical nid
                    key = getNodeId nidC
                    seen' = IntSet.insert key seen
                 in if IntSet.member key seen
                      then ty
                      else case lookupNodeIn nodes nidC of
                        Just TyVar {} ->
                          case lookupBound nidC of
                            Just bnd ->
                              case reifyBound seen' (canonical bnd) of
                                Right ty' -> goAlias seen' boundRefs ty'
                                Left _ -> ty
                            Nothing -> if fallbackToBottom then TBottom else ty
                        Just _ ->
                          case reifyBound seen' nidC of
                            Right ty' -> goAlias seen' boundRefs ty'
                            Left _ -> ty
                        Nothing -> ty
              Nothing -> ty
      TArrow a b -> TArrow (goAlias seen boundRefs a) (goAlias seen boundRefs b)
      TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (goAlias seen boundRefs) args)
      TVarAppRef ref args ->
        let args' = fmap (goAlias seen boundRefs) args
            headTy = goAlias seen boundRefs (TVarRef ref)
         in composeTypeHeadRef ref headTy args'
      TForallRef ref mb body ->
        let boundRefs' = insertBoundRef ref boundRefs
            mb' = fmap (goBound seen boundRefs') mb
            body' = goAlias seen boundRefs' body
         in TForallRef ref mb' body'
      TMuRef ref body ->
        let boundRefs' = insertBoundRef ref boundRefs
         in TMuRef ref (goAlias seen boundRefs' body)
      TBaseWithIdentity _ _ -> ty
      TBottom -> ty

    boundRefMember ref =
      any (typeBinderRefsSameIdentity ref)

    insertBoundRef ref refs
      | boundRefMember ref refs = refs
      | otherwise = ref : refs

    goBound seen boundRefs bound = case bound of
      TArrow a b -> TArrow (goAlias seen boundRefs a) (goAlias seen boundRefs b)
      TConWithIdentity identity c args -> TConWithIdentity identity c (fmap (goAlias seen boundRefs) args)
      TVarAppRef ref args ->
        let args' = fmap (goAlias seen boundRefs) args
            headTy = goAlias seen boundRefs (TVarRef ref)
         in composeTypeHeadRef ref headTy args'
      TBaseWithIdentity identity b -> TBaseWithIdentity identity b
      TBottom -> TBottom
      TForallRef ref mb body ->
        let boundRefs' = insertBoundRef ref boundRefs
            mb' = fmap (goBound seen boundRefs') mb
            body' = goAlias seen boundRefs' body
         in TForallRef ref mb' body'
      TMuRef ref body ->
        let boundRefs' = insertBoundRef ref boundRefs
         in TMuRef ref (goAlias seen boundRefs' body)
