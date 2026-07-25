{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.Finalize.IdentitySupply
  ( freshTypeBinderRefs,
    freshTypeBinderRefsWithSupply,
    freshenElabTypeBindersAgainstTypesFromSupply,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import MLF.Elab.Types (ElabType)
import qualified MLF.Elab.Types as X
import MLF.Reify.TypeOps
  ( freeTypeVarRefsType,
    freshNameLike,
    substTypeCaptureRef,
  )
import MLF.Types.Identity
  ( IdentityGenerator,
    UniqueIdentity,
    advanceIdentityGeneratorPastMany,
    identityGeneratorAfter,
  )

-- | Allocate source-type binder references from one caller-owned supply.  The
-- occupied inventory is applied to that supply before allocation; callers
-- without an authoritative supply retain the standalone fallback.
freshTypeBinderRefsWithSupply ::
  Maybe IdentityGenerator ->
  [UniqueIdentity] ->
  [String] ->
  (Map String X.TypeBinderRef, IdentityGenerator)
freshTypeBinderRefsWithSupply mbGenerator occupiedIdentities names =
  freshTypeBinderRefs names generator
  where
    generator =
      case mbGenerator of
        Just supplied ->
          advanceIdentityGeneratorPastMany occupiedIdentities supplied
        Nothing ->
          identityGeneratorAfter occupiedIdentities

freshTypeBinderRefs :: [String] -> IdentityGenerator -> (Map String X.TypeBinderRef, IdentityGenerator)
freshTypeBinderRefs names generator0 =
  foldr fresh (Map.empty, generator0) names
  where
    fresh name (refs, generator) =
      let (ref, generator') = X.sourceTypeBinderRefForName name generator
       in (Map.insert name ref refs, generator')

-- | Freshen colliding quantified binders while preserving the caller's
-- authoritative identity supply.  Existing identities are inventory, not a
-- replacement seed: sibling allocations therefore cannot be reissued here.
freshenElabTypeBindersAgainstTypesFromSupply ::
  IdentityGenerator ->
  [ElabType] ->
  ElabType ->
  (ElabType, IdentityGenerator)
freshenElabTypeBindersAgainstTypesFromSupply generator reservedTys ty
  | null reservedRefs = (ty, generator0)
  | otherwise = freshenTypeBindersAgainstRefs reservedRefs reservedNames generator0 ty
  where
    reservedRefs = foldMap freeTypeVarRefsType reservedTys
    reservedNames =
      Set.fromList (map X.typeBinderRefName reservedRefs)
    generator0 =
      advanceIdentityGeneratorPastMany
        (concatMap X.generatedIdentitiesInType (ty : reservedTys))
        generator

freshenTypeBindersAgainstRefs ::
  [X.TypeBinderRef] ->
  Set String ->
  IdentityGenerator ->
  ElabType ->
  (ElabType, IdentityGenerator)
freshenTypeBindersAgainstRefs reservedRefs reservedNames generator0 =
  go generator0
  where
    binderCollides ref =
      any (X.typeBinderRefsSameIdentity ref) reservedRefs

    go :: IdentityGenerator -> X.Ty v -> (X.Ty v, IdentityGenerator)
    go generator ty =
      case ty of
        X.TVarRef {} ->
          (ty, generator)
        X.TArrow dom cod ->
          let (dom', generator1) = go generator dom
              (cod', generator2) = go generator1 cod
           in (X.TArrow dom' cod', generator2)
        X.TConWithIdentity identity con args ->
          let (args', generator') = freshenNonEmpty generator args
           in (X.TConWithIdentity identity con args', generator')
        X.TVarAppRef ref args ->
          let (args', generator') = freshenNonEmpty generator args
           in (X.TVarAppRef ref args', generator')
        X.TBaseWithIdentity {} ->
          (ty, generator)
        X.TForallRef ref mbBound body ->
          let (mbBound', generator1) =
                freshenMaybeBound generator mbBound
              (ref', bodyForFreshening, generator2) =
                if binderCollides ref
                  then
                    let usedNames =
                          Set.unions
                            [ reservedNames,
                              Set.fromList (map X.typeBinderRefName (freeTypeVarRefsType body)),
                              maybe Set.empty (Set.fromList . map X.typeBinderRefName . freeTypeVarRefsType) mbBound,
                              Set.singleton (X.typeBinderRefName ref)
                            ]
                        freshName = freshNameLike (X.typeBinderRefName ref) usedNames
                        (freshRef, generator') = X.freshTypeBinderRef freshName generator1
                     in (freshRef, substTypeCaptureRef ref (X.TVarRef freshRef) body, generator')
                  else (ref, body, generator1)
              (body', generator3) = go generator2 bodyForFreshening
           in (X.TForallRef ref' mbBound' body', generator3)
        X.TMuRef ref body ->
          let (ref', bodyForFreshening, generator1) =
                if binderCollides ref
                  then
                    let usedNames =
                          Set.unions
                            [ reservedNames,
                              Set.fromList (map X.typeBinderRefName (freeTypeVarRefsType body)),
                              Set.singleton (X.typeBinderRefName ref)
                            ]
                        freshName = freshNameLike (X.typeBinderRefName ref) usedNames
                        (freshRef, generator') = X.freshTypeBinderRef freshName generator
                     in (freshRef, substTypeCaptureRef ref (X.TVarRef freshRef) body, generator')
                  else (ref, body, generator)
              (body', generator2) = go generator1 bodyForFreshening
           in (X.TMuRef ref' body', generator2)
        X.TBottom ->
          (ty, generator)

    freshenMaybeBound generator =
      \case
        Nothing -> (Nothing, generator)
        Just bound ->
          let (bound', generator') = go generator bound
           in (Just bound', generator')

    freshenNonEmpty generator (arg :| args) =
      let (arg', generator1) = go generator arg
          (argsRev, generator') =
            foldl
              ( \(acc, gen) item ->
                  let (item', gen') = go gen item
                   in (item' : acc, gen')
              )
              ([], generator1)
              args
       in (arg' :| reverse argsRev, generator')
