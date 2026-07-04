{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module MLF.Reify.TypeOps
  ( splitForallsRefs,
    stripForallsType,
    freeTypeVarRefsFrom,
    freeTypeVarsType,
    freeTypeVarRefsType,
    freeTypeVarRefsList,
    substTypeCaptureRef,
    substTypeSimpleRef,
    composeTypeHeadRef,
    freshNameLike,
    alphaEqType,
    churchMuEquivalent,
    churchAwareEqType,
    typeHeadMatches,
    firstNonContractiveRecursiveType,
    matchTypeRefs,
    resolveBaseBoundForInstConstraint,
    resolveBaseBoundForInstSolved,
    resolveBoundBodyConstraint,
    inlineBaseBoundsType,
    inlineAliasBoundsWithBy,
    inlineAliasBoundsWithBySeen,
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
import qualified MLF.Primitive.Identity as PrimitiveIdentity
import MLF.Types.Elab
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

    freshCaptureRef :: String -> ElabType -> Maybe BoundType -> TypeBinderRef
    freshCaptureRef name body mbBound =
      fst (freshTypeBinderRef name (identityGeneratorAfterType seed))
      where
        seed =
          TArrow
            s
            (maybe body (\bound -> TArrow (tyToElab bound) body) mbBound)

    replacementMentionsRef :: TypeBinderRef -> Bool
    replacementMentionsRef ref =
      any (binderMayCaptureReplacementRef ref) freeSRefs

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
        | replacementMentionsRef ref ->
            let used =
                  Set.unions
                    [ freeSNames,
                      freeTypeVarAliasNamesType body,
                      maybe Set.empty freeTypeVarAliasNamesType mb,
                      Set.singleton v
                    ]
                v' = freshNameLike v used
                ref' = freshCaptureRef v' (tyToElab body) mb
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
        | replacementMentionsRef ref ->
            let used =
                  Set.unions
                    [ freeSNames,
                      freeTypeVarAliasNamesType body,
                      Set.singleton v
                    ]
                v' = freshNameLike v used
                ref' = freshCaptureRef v' (tyToElab body) Nothing
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
            | replacementMentionsRef ref ->
                let used =
                      Set.unions
                        [ freeSNames,
                          freeTypeVarAliasNamesType (fst (unIxPair body)),
                          maybe Set.empty (freeTypeVarAliasNamesType . fst . unIxPair) mb,
                          Set.singleton v
                        ]
                    v' = freshNameLike v used
                    ref' = freshCaptureRef v' (fst (unIxPair body)) (fmap (fst . unIxPair) mb)
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
            | replacementMentionsRef ref ->
                let used =
                      Set.unions
                        [ freeSNames,
                          freeTypeVarAliasNamesType (fst (unIxPair body)),
                          Set.singleton v
                        ]
                    v' = freshNameLike v used
                    ref' = freshCaptureRef v' (fst (unIxPair body)) Nothing
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

typeHeadMatches :: Maybe SymbolIdentity -> BaseTy -> Maybe SymbolIdentity -> BaseTy -> Bool
typeHeadMatches =
  typeHeadRefMatches

alphaEqType :: ElabType -> ElabType -> Bool
alphaEqType = go [] []
  where
    go envL envR t1 t2 = case (t1, t2) of
      (TVarRef a, TVarRef b) ->
        alphaEqRef envL envR a b
      (TArrow a1 b1, TArrow a2 b2) ->
        go envL envR a1 a2 && go envL envR b1 b2
      (TConWithIdentity identity1 c1 args1, TConWithIdentity identity2 c2 args2) ->
        typeHeadMatches identity1 c1 identity2 c2 && alphaEqArgs envL envR (toList args1) (toList args2)
      (TVarAppRef a args1, TVarAppRef b args2) ->
        alphaEqVar envL envR a b && alphaEqArgs envL envR (toList args1) (toList args2)
      (TBaseWithIdentity identity1 b1, TBaseWithIdentity identity2 b2) ->
        typeHeadMatches identity1 b1 identity2 b2
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
      (TConWithIdentity identity1 c1 args1, TConWithIdentity identity2 c2 args2) ->
        typeHeadMatches identity1 c1 identity2 c2 && alphaEqArgs envL envR (toList args1) (toList args2)
      (TVarAppRef a args1, TVarAppRef b args2) ->
        alphaEqVar envL envR a b && alphaEqArgs envL envR (toList args1) (toList args2)
      (TBaseWithIdentity identity1 b1', TBaseWithIdentity identity2 b2') ->
        typeHeadMatches identity1 b1' identity2 b2'
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
    (TMuRef ref1 body1, TMuRef ref2 body2) ->
      let (qs1, core1) = stripChurchForalls body1
          (qs2, core2) = stripChurchForalls body2
          -- All quantifier vars from both sides are matchable
          allQs = unionRefs qs1 qs2
       in or
            [ tryMatch allQs (TMuRef ref1 c1) (TMuRef ref2 c2)
              | c1 <- selfAliasVariants qs1 ref1 core1,
                c2 <- selfAliasVariants qs2 ref2 core2
            ]
    _ -> False
  where
    -- Strip leading unbounded TForall from a μ body.
    stripChurchForalls :: ElabType -> ([TypeBinderRef], ElabType)
    stripChurchForalls ty = case ty of
      TForallRef ref Nothing body ->
        let (refs, core) = stripChurchForalls body
         in (ref : refs, core)
      _ -> ([], ty)

    -- Generate candidate cores where a free-variable self-alias is replaced
    -- by the actual μ binder name.
    selfAliasVariants :: [TypeBinderRef] -> TypeBinderRef -> ElabType -> [ElabType]
    selfAliasVariants quantRefs muRef core
      | any (typeBinderRefsSameIdentity muRef) (freeTypeVarRefsType core) = [core]
      | otherwise =
          -- The mu binder doesn't appear in the body — some alias does.
          -- Try each free var (that isn't a stripped quantifier) as the alias.
          core
            : [ substTypeSimpleRef alias (TVarRef muRef) core
                | alias <- freeRefsExcept quantRefs core
              ]

    tryMatch :: [TypeBinderRef] -> ElabType -> ElabType -> Bool
    tryMatch matchableVars lhs rhs =
      alphaEqType lhs rhs
        || isRight (matchTypeRefs matchableVars lhs rhs)
        || isRight (matchTypeRefs matchableVars rhs lhs)

    unionRefs left right =
      foldr insertRef right left

    insertRef ref refs
      | any (typeBinderRefsSameIdentity ref) refs = refs
      | otherwise = ref : refs

    freeRefsExcept excluded ty =
      [ ref
        | ref <- freeTypeVarRefsType ty,
          not (any (typeBinderRefsSameIdentity ref) excluded)
      ]

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _ = False

-- | Structural equality that falls back to 'churchMuEquivalent' at TMu-vs-TMu
-- junctions.  Keeps 'alphaEqType' strict while allowing Church-μ relaxation
-- at every nesting depth.
churchAwareEqType :: ElabType -> ElabType -> Bool
churchAwareEqType = go [] []
  where
    stripChurchForalls :: ElabType -> ([TypeBinderRef], ElabType)
    stripChurchForalls ty = case ty of
      TForallRef ref Nothing body ->
        let (refs, core) = stripChurchForalls body
         in (ref : refs, core)
      _ -> ([], ty)

    selfAliasVariants :: [TypeBinderRef] -> TypeBinderRef -> ElabType -> [ElabType]
    selfAliasVariants quantRefs muRef core
      | any (typeBinderRefsSameIdentity muRef) (freeTypeVarRefsType core) = [core]
      | otherwise =
          core
            : [ substTypeSimpleRef alias (TVarRef muRef) core
                | alias <- freeRefsExcept quantRefs core
              ]

    tryMatch :: [TypeBinderRef] -> ElabType -> ElabType -> Bool
    tryMatch matchableVars lhs rhs =
      alphaEqType lhs rhs
        || isRight (matchTypeRefs matchableVars lhs rhs)
        || isRight (matchTypeRefs matchableVars rhs lhs)

    freeRefsExcept excluded ty =
      [ ref
        | ref <- freeTypeVarRefsType ty,
          not (any (typeBinderRefsSameIdentity ref) excluded)
      ]

    churchMuMatchesCore :: ElabType -> ElabType -> Bool
    churchMuMatchesCore muTy@(TMuRef ref body) otherTy =
      let muTy' = tyToElab muTy
          (quantVars, coreBody) = stripChurchForalls (tyToElab body)
          (_, unfoldedCoreBody) = stripChurchForalls (substTypeSimpleRef ref muTy' (tyToElab body))
          candidateBodies =
            selfAliasVariants quantVars ref coreBody
              ++ selfAliasVariants quantVars ref unfoldedCoreBody
          (_, otherCore) = stripChurchForalls otherTy
       in or
            [ tryMatch quantVars candidate otherTy || tryMatch quantVars candidate otherCore
              | candidate <- candidateBodies
            ]
    churchMuMatchesCore _ _ = False

    unfoldMuOnce :: Ty v -> Maybe ElabType
    unfoldMuOnce muTy@(TMuRef ref body) =
      let muTy' = tyToElab muTy
          unfolded = substTypeSimpleRef ref muTy' (tyToElab body)
       in if alphaEqType unfolded muTy' then Nothing else Just unfolded
    unfoldMuOnce _ = Nothing

    go envL envR t1 t2 = case (t1, t2) of
      (TVarRef a, TVarRef b) ->
        alphaEqRef envL envR a b
      (TArrow a1 b1, TArrow a2 b2) ->
        go envL envR a1 a2 && go envL envR b1 b2
      (TConWithIdentity identity1 c1 args1, TConWithIdentity identity2 c2 args2) ->
        typeHeadMatches identity1 c1 identity2 c2 && eqArgs envL envR (toList args1) (toList args2)
      (TVarAppRef a args1, TVarAppRef b args2) ->
        eqVar envL envR a b && eqArgs envL envR (toList args1) (toList args2)
      (TBaseWithIdentity identity1 b1, TBaseWithIdentity identity2 b2) ->
        typeHeadMatches identity1 b1 identity2 b2
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
        churchMuMatchesCore t1 t2
          || maybe False (\unfolded -> go envL envR unfolded t2) (unfoldMuOnce t1)
      (_, TMuRef {}) ->
        churchMuMatchesCore t2 t1
          || maybe False (\unfolded -> go envL envR t1 unfolded) (unfoldMuOnce t2)
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
      (TConWithIdentity identity1 c1 args1, TConWithIdentity identity2 c2 args2) ->
        typeHeadMatches identity1 c1 identity2 c2 && eqArgs envL envR (toList args1) (toList args2)
      (TVarAppRef a args1, TVarAppRef b args2) ->
        eqVar envL envR a b && eqArgs envL envR (toList args1) (toList args2)
      (TBaseWithIdentity identity1 b1', TBaseWithIdentity identity2 b2') ->
        typeHeadMatches identity1 b1' identity2 b2'
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

matchTypeRefs ::
  [TypeBinderRef] ->
  ElabType ->
  ElabType ->
  Either ElabError (Map.Map TypeBinderRef ElabType)
matchTypeRefs binderRefs = goMatch [] Map.empty
  where
    goMatch env subst tyP tyT = case (tyP, tyT) of
      (TVarRef ref, _)
        | Just binder <- matchableRef ref ->
        case Map.lookup binder subst of
          Nothing -> Right (Map.insert binder tyT subst)
          Just ty0 ->
            if alphaEqType ty0 tyT
              then Right subst
              else Left (InstantiationError "matchType: binder mismatch")
      (TVarRef ref, TVarRef ref')
        | boundVarMatches env ref ref' -> Right subst
      (TArrow a b, TArrow a' b') -> do
        subst1 <- goMatch env subst a a'
        goMatch env subst1 b b'
      (TConWithIdentity identity0 c0 args0, TConWithIdentity identity1 c1 args1)
        | typeHeadMatches identity0 c0 identity1 c1 ->
            matchArgs env subst (toList args0) (toList args1)
      (TVarAppRef ref argsP, _)
        | Just binder <- matchableRef ref ->
            matchVarHead env subst binder (toList argsP) tyT
      (TVarAppRef ref args0, TVarAppRef ref' args1)
        | boundVarMatches env ref ref' ->
            matchArgs env subst (toList args0) (toList args1)
      (TBaseWithIdentity identity0 b0, TBaseWithIdentity identity1 b1)
        | typeHeadMatches identity0 b0 identity1 b1 -> Right subst
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
      (TConWithIdentity identity0 c0 args0, TConWithIdentity identity1 c1 args1)
        | typeHeadMatches identity0 c0 identity1 c1 ->
            matchArgs env subst (toList args0) (toList args1)
      (TVarAppRef ref argsP, _)
        | Just binder <- matchableRef ref ->
            matchVarHead env subst binder (toList argsP) (tyToElab boundT)
      (TVarAppRef ref args0, TVarAppRef ref' args1)
        | boundVarMatches env ref ref' ->
            matchArgs env subst (toList args0) (toList args1)
      (TBaseWithIdentity identity0 b0, TBaseWithIdentity identity1 b1)
        | typeHeadMatches identity0 b0 identity1 b1 -> Right subst
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
            Just (Left base) -> baseWithBuiltinIdentity base
            Just (Right ()) -> TBottom
            Nothing -> TVarRef ref

    inlineVarAppRef :: [TypeBinderRef] -> TypeBinderRef -> NE.NonEmpty ElabType -> Ty v
    inlineVarAppRef boundRefs ref args
      | boundRefMember ref boundRefs = TVarAppRef ref args
      | otherwise =
          case resolvedBaseBound ref of
            Just (Left base) -> conWithBuiltinIdentity base args
            _ -> TVarAppRef ref args

    resolvedBaseBound :: TypeBinderRef -> Maybe (Either BaseTy ())
    resolvedBaseBound ref = do
      nid <- refNodeId ref
      baseN <- resolveBaseBoundForInstConstraint constraint canonical nid
      case NodeAccess.lookupNode constraint baseN of
        Just TyBase {tnBase = b} -> Just (Left b)
        Just TyBottom {} -> Just (Right ())
        _ -> Nothing

    baseWithBuiltinIdentity base@(BaseTy name) =
      TBaseWithIdentity (PrimitiveIdentity.builtinTypeHeadIdentity name) base

    conWithBuiltinIdentity base@(BaseTy name) args =
      TConWithIdentity (PrimitiveIdentity.builtinTypeHeadIdentity name) base args

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
  goAlias IntSet.empty []
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
