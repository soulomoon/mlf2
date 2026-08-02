{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MLF.Elab.SourceType
  ( sourceTypeHeadNames,
    sourceTypeToElabTypeWithIdentities,
    sourceTypeToElabTypeWithIdentitiesFromSupply,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (BaseTy (..))
import MLF.Elab.Types
  ( BoundType,
    ElabError (..),
    ElabType,
    Ty (..),
    TypeBinderRef,
    sourceTypeBinderRefOrFreshInScope,
    sourceTypeBinderRefsFromIdentities,
  )
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.Frontend.Symbol
  ( SymbolIdentity,
    lookupSymbolIdentityAlias,
  )
import MLF.Frontend.Syntax
  ( NormSrcType,
    SrcBound (..),
    SrcNorm (NormN),
    SrcTy (..),
    StructBound,
  )
import MLF.Types.Identity
  ( IdentityGenerator,
    TypeBinderIdentity,
    advanceIdentityGeneratorPastMany,
    identityGeneratorAfter,
    symbolGeneratedIdentities,
    typeBinderGeneratedIdentities,
  )

-- | Convert a normalized source annotation while preserving the semantic
-- identities chosen by source resolution.
sourceTypeToElabTypeWithIdentities ::
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  NormSrcType ->
  Either ElabError ElabType
sourceTypeToElabTypeWithIdentities headIdentities binderIdentities ty =
  fmap fst
    ( sourceTypeToElabTypeWithIdentitiesFromSupply
        (identityGeneratorAfter [])
        headIdentities
        binderIdentities
        ty
    )

-- | Convert a normalized source annotation and return the identity supply
-- after allocating every source-local binder.
sourceTypeToElabTypeWithIdentitiesFromSupply ::
  IdentityGenerator ->
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  NormSrcType ->
  Either ElabError (ElabType, IdentityGenerator)
sourceTypeToElabTypeWithIdentitiesFromSupply generator0 headIdentities binderIdentities ty =
  let generator1 =
        advanceSourceTypeIdentityGeneratorPast
          headIdentities
          binderIdentities
          ty
          generator0
      (refs, generator2) =
        sourceTypeBinderRefsFromIdentities
          binderIdentities
          (Set.toList (freeSrcTypeVars ty))
          generator1
   in srcTypeToElabTypeWith headIdentities binderIdentities refs generator2 ty

advanceSourceTypeIdentityGeneratorPast ::
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  NormSrcType ->
  IdentityGenerator ->
  IdentityGenerator
advanceSourceTypeIdentityGeneratorPast sourceHeadIdentities sourceBinderIdentities ty =
  advanceIdentityGeneratorPastMany
    ( concatMap symbolGeneratedIdentities (Map.elems headIdentities)
        ++ concatMap typeBinderGeneratedIdentities (Map.elems sourceBinderIdentities)
    )
  where
    headIdentities =
      Map.union
        sourceHeadIdentities
        (Builtins.builtinSourceTypeHeadIdentities ty)

freeSrcTypeVars :: SrcTy n v -> Set.Set String
freeSrcTypeVars ty =
  go Set.empty ty
  where
    go :: Set.Set String -> SrcTy n0 v0 -> Set.Set String
    go bound srcTy =
      case srcTy of
        STVar name
          | name `Set.member` bound -> Set.empty
          | otherwise -> Set.singleton name
        STArrow dom cod -> go bound dom `Set.union` go bound cod
        STBase {} -> Set.empty
        STCon _ args -> foldMap (go bound) args
        STVarApp name args ->
          let headVars
                | name `Set.member` bound = Set.empty
                | otherwise = Set.singleton name
           in headVars `Set.union` foldMap (go bound) args
        STTyLam name body -> go (Set.insert name bound) body
        STTyApp fun arg -> go bound fun `Set.union` go bound arg
        STForall name mb body ->
          maybe Set.empty (go bound . unSrcBound) mb
            `Set.union` go (Set.insert name bound) body
        STMu name body -> go (Set.insert name bound) body
        STBottom -> Set.empty

sourceTypeHeadNames :: SrcTy n v -> Set.Set String
sourceTypeHeadNames ty =
  case ty of
    STVar {} -> Set.empty
    STArrow dom cod ->
      sourceTypeHeadNames dom `Set.union` sourceTypeHeadNames cod
    STBase name -> Set.singleton name
    STCon name args ->
      Set.insert name (foldMap sourceTypeHeadNames args)
    STVarApp _ args -> foldMap sourceTypeHeadNames args
    STTyLam _ body -> sourceTypeHeadNames body
    STTyApp fun arg ->
      sourceTypeHeadNames fun `Set.union` sourceTypeHeadNames arg
    STForall _ mb body ->
      maybe Set.empty (sourceTypeHeadNames . unSrcBound) mb
        `Set.union` sourceTypeHeadNames body
    STMu _ body -> sourceTypeHeadNames body
    STBottom -> Set.empty

requireSourceTypeHeadIdentity ::
  Map.Map String SymbolIdentity ->
  String ->
  Either ElabError SymbolIdentity
requireSourceTypeHeadIdentity headIdentities name =
  case
      lookupSymbolIdentityAlias headIdentities name
        <|> Builtins.builtinTypeHeadIdentity name
    of
      Just identity -> Right identity
      Nothing ->
        Left
          ( InstantiationError
              ( "unresolved source type head `"
                  ++ name
                  ++ "` reached annotation elaboration"
              )
          )

srcTypeToElabTypeWith ::
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  NormSrcType ->
  Either ElabError (ElabType, IdentityGenerator)
srcTypeToElabTypeWith =
  srcTypeToElabTypeWithBound Set.empty

srcTypeToElabTypeWithBound ::
  Set.Set String ->
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  NormSrcType ->
  Either ElabError (ElabType, IdentityGenerator)
srcTypeToElabTypeWithBound boundNames headIdentities binderIdentities refs generator ty =
  case ty of
    STVar name -> do
      ref <- sourceTypeBinderRef refs name
      Right (TVarRef ref, generator)
    STArrow dom cod -> do
      (dom', generator1) <-
        srcTypeToElabTypeWithBound
          boundNames
          headIdentities
          binderIdentities
          refs
          generator
          dom
      (cod', generator2) <-
        srcTypeToElabTypeWithBound
          boundNames
          headIdentities
          binderIdentities
          refs
          generator1
          cod
      Right (TArrow dom' cod', generator2)
    STCon name args -> do
      (args', generator') <-
        srcTypesToElabTypesWith boundNames refs generator args
      identity <- requireSourceTypeHeadIdentity headIdentities name
      Right
        ( TConWithIdentity identity (builtinBaseTy name) args',
          generator'
        )
    STVarApp name args -> do
      (args', generator') <-
        srcTypesToElabTypesWith boundNames refs generator args
      ref <- sourceTypeBinderRef refs name
      Right (TVarAppRef ref args', generator')
    STTyLam {} ->
      Left (InstantiationError "residual type lambda reached elaboration")
    STTyApp {} ->
      Left (InstantiationError "residual type application reached elaboration")
    STForall name mb body ->
      let (ref, generator1) =
            sourceTypeBinderRefOrFreshInScope
              (Set.member name boundNames)
              binderIdentities
              name
              generator
          refs' = Map.insert name ref refs
          boundNames' = Set.insert name boundNames
       in do
            (mb', generator2) <-
              maybe
                (Right (Nothing, generator1))
                ( srcBoundToElabBoundWithBound
                    boundNames
                    headIdentities
                    binderIdentities
                    refs
                    generator1
                )
                mb
            (body', generator3) <-
              srcTypeToElabTypeWithBound
                boundNames'
                headIdentities
                binderIdentities
                refs'
                generator2
                body
            Right (TForallRef ref mb' body', generator3)
    STMu name body ->
      let (ref, generator1) =
            sourceTypeBinderRefOrFreshInScope
              (Set.member name boundNames)
              binderIdentities
              name
              generator
          boundNames' = Set.insert name boundNames
       in do
            (body', generator2) <-
              srcTypeToElabTypeWithBound
                boundNames'
                headIdentities
                binderIdentities
                (Map.insert name ref refs)
                generator1
                body
            Right (TMuRef ref body', generator2)
    STBase name -> do
      identity <- requireSourceTypeHeadIdentity headIdentities name
      Right
        ( TBaseWithIdentity identity (builtinBaseTy name),
          generator
        )
    STBottom -> Right (TBottom, generator)
  where
    sourceTypeBinderRef env name =
      case Map.lookup name env of
        Just ref -> Right ref
        Nothing ->
          Left
            ( InstantiationError
                ( "unresolved source type binder `"
                    ++ name
                    ++ "` reached annotation elaboration"
                )
            )

    srcTypesToElabTypesWith boundNames' refs0 generator0 (arg :| args) = do
      (arg', generator1) <-
        srcTypeToElabTypeWithBound
          boundNames'
          headIdentities
          binderIdentities
          refs0
          generator0
          arg
      (argsRev, generator') <-
        foldM
          ( \(acc, gen) next -> do
              (next', gen') <-
                srcTypeToElabTypeWithBound
                  boundNames'
                  headIdentities
                  binderIdentities
                  refs0
                  gen
                  next
              Right (next' : acc, gen')
          )
          ([], generator1)
          args
      Right (arg' :| reverse argsRev, generator')

srcBoundToElabBoundWithBound ::
  Set.Set String ->
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  SrcBound 'NormN ->
  Either ElabError (Maybe BoundType, IdentityGenerator)
srcBoundToElabBoundWithBound boundNames headIdentities binderIdentities refs generator (SrcBound ty) =
  structBoundToElabBoundWithBound
    boundNames
    headIdentities
    binderIdentities
    refs
    generator
    ty

structBoundToElabBoundWithBound ::
  Set.Set String ->
  Map.Map String SymbolIdentity ->
  Map.Map String TypeBinderIdentity ->
  Map.Map String TypeBinderRef ->
  IdentityGenerator ->
  StructBound ->
  Either ElabError (Maybe BoundType, IdentityGenerator)
structBoundToElabBoundWithBound boundNames headIdentities binderIdentities refs generator bTy =
  case bTy of
    STArrow dom cod -> do
      (dom', generator1) <-
        srcTypeToElabTypeWithBound
          boundNames
          headIdentities
          binderIdentities
          refs
          generator
          dom
      (cod', generator2) <-
        srcTypeToElabTypeWithBound
          boundNames
          headIdentities
          binderIdentities
          refs
          generator1
          cod
      Right (Just (TArrow dom' cod'), generator2)
    STBase name -> do
      identity <- requireSourceTypeHeadIdentity headIdentities name
      Right
        ( Just (TBaseWithIdentity identity (builtinBaseTy name)),
          generator
        )
    STCon name args -> do
      (args', generator1) <- srcTypesToElabTypesWith refs generator args
      identity <- requireSourceTypeHeadIdentity headIdentities name
      Right
        ( Just (TConWithIdentity identity (builtinBaseTy name) args'),
          generator1
        )
    STVarApp name args -> do
      (args', generator1) <- srcTypesToElabTypesWith refs generator args
      ref <- sourceTypeBinderRef refs name
      Right (Just (TVarAppRef ref args'), generator1)
    STTyLam {} ->
      Left (InstantiationError "residual type lambda reached elaboration")
    STTyApp {} ->
      Left (InstantiationError "residual type application reached elaboration")
    STForall name mb body ->
      let (ref, generator1) =
            sourceTypeBinderRefOrFreshInScope
              (Set.member name boundNames)
              binderIdentities
              name
              generator
          refs' = Map.insert name ref refs
          boundNames' = Set.insert name boundNames
       in do
            (mb', generator2) <-
              maybe
                (Right (Nothing, generator1))
                ( srcBoundToElabBoundWithBound
                    boundNames
                    headIdentities
                    binderIdentities
                    refs
                    generator1
                )
                mb
            (body', generator3) <-
              srcTypeToElabTypeWithBound
                boundNames'
                headIdentities
                binderIdentities
                refs'
                generator2
                body
            Right (Just (TForallRef ref mb' body'), generator3)
    STMu name body ->
      let (ref, generator1) =
            sourceTypeBinderRefOrFreshInScope
              (Set.member name boundNames)
              binderIdentities
              name
              generator
          boundNames' = Set.insert name boundNames
       in do
            (body', generator2) <-
              srcTypeToElabTypeWithBound
                boundNames'
                headIdentities
                binderIdentities
                (Map.insert name ref refs)
                generator1
                body
            Right (Just (TMuRef ref body'), generator2)
    STBottom -> Right (Nothing, generator)
  where
    sourceTypeBinderRef env name =
      case Map.lookup name env of
        Just ref -> Right ref
        Nothing ->
          Left
            ( InstantiationError
                ( "unresolved source type binder `"
                    ++ name
                    ++ "` reached annotation elaboration"
                )
            )

    srcTypesToElabTypesWith refs0 generator0 (arg :| args) = do
      (arg', generator1) <-
        srcTypeToElabTypeWith
          headIdentities
          binderIdentities
          refs0
          generator0
          arg
      (argsRev, generator') <-
        foldM
          ( \(acc, gen) next -> do
              (next', gen') <-
                srcTypeToElabTypeWith
                  headIdentities
                  binderIdentities
                  refs0
                  gen
                  next
              Right (next' : acc, gen')
          )
          ([], generator1)
          args
      Right (arg' :| reverse argsRev, generator')

builtinBaseTy :: String -> BaseTy
builtinBaseTy =
  BaseTy . Builtins.normalizeBuiltinTypeReference
