{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
module MLF.Types.Elab (
    Ty
        ( TVarRef,
          TArrow,
          TConWithIdentity,
          TCon,
          TVarAppRef,
          TBaseWithIdentity,
          TBase,
          TForallRef,
          TMuRef,
          TBottom
        ),
    TopVar(..),
    ElabType,
    BoundType,
    tVarWithRef,
    tVarAppWithRef,
    tForallWithRef,
    tMuWithRef,
    TyIF
        ( TVarIFRef,
          TArrowIF,
          TConIFWithIdentity,
          TConIF,
          TVarAppIFRef,
          TBaseIFWithIdentity,
          TBaseIF,
          TForallIFRef,
          TMuIFRef,
          TBottomIF
        ),
    IxFix(..),
    IxFunctor(..),
    IxRecursive(..),
    IxCorecursive(..),
    IxPair(..),
    cataIx,
    cataIxConst,
    paraIx,
    zygoIx,
    K(..),
    tyToElab,
    elabToBound,
    containsForallTy,
    containsArrowTy,
    typeHeadRefMatches,
    ElabScheme,
    mkElabSchemeWithRefs,
    schemeBinderRefs,
    schemeBody,
    SchemeInfo(SchemeInfo, siScheme, siSubstRefs),
    TypeBinderIdentity,
    typeBinderIdentityFromNode,
    typeBinderIdentityNode,
    typeBinderIdentityKey,
    typeBinderIdentityFromUnique,
    TypeBinderRef,
    typeBinderRefFromIdentity,
    typeBinderRefIdentity,
    typeBinderRefNode,
    typeBinderRefName,
    typeBinderRefsSameIdentity,
    typeBinderRefsSameIdentityAndName,
    renameTypeBinderRef,
    freshTypeBinderRef,
    sourceTypeBinderRefForName,
    freshTypeBinderRefFromNames,
    instAbstrWithRef,
    instUnderWithRef,
    schemeInfoBinderIdentityKeys,
    schemeInfoBinderIdentityKeySet,
    schemeInfoFromRefSubst,
    schemeInfoBinderRefSubst,
    ResolvedVar(..),
    ResolvedTermIdentityKey,
    idDetailsIdentityKey,
    resolvedVarIdentityKey,
    deferredResolvedVarFromRef,
    deferredResolvedVarRef,
    localResolvedVarFromRef,
    mkDeferredVarWithRef,
    mkLocalLamWithRef,
    mkLocalLetWithRef,
    mkLocalRecursiveLetWithRef,
    identityGeneratorAfterType,
    generatedIdentitiesInType,
    eTyAbsWithRef,
    identityGeneratorAfterTerm,
    generatedIdentitiesInTerm,
    resolvedVarName,
    resolvedVarReferenceName,
    resolvedVarConstructorRef,
    resolvedVarLocalRef,
    resolvedVarSymbolIdentity,
    resolvedVarIsLocal,
    resolvedVarIsEvidence,
    resolvedVarIsDiscard,
    resolvedVarSameIdentity,
    resolvedVarBoundBy,
    mapResolvedVarType,
    renameResolvedLocalVar,
    freshenResolvedLocalVar,
    renameResolvedDeferredVar,
    XmlfTerm
        ( ELit,
          ELam,
          EApp,
          ELet,
          ETyAbsRef,
          ETyInst,
          ERoll,
          EUnroll,
          EVarNode
        ),
    XmlfTermF
        ( EVarNodeF,
          ELitF,
          ELamF,
          EAppF,
          ELetF,
          ETyAbsFRef,
          ETyInstF,
          ERollF,
          EUnrollF
        ),
    Instantiation
        ( InstId,
          InstApp,
          InstBot,
          InstIntro,
          InstElim,
          InstInside,
          InstSeq,
          InstAbstrRef,
          InstUnderRef
        ),
    InstantiationF
        ( InstIdF,
          InstAppF,
          InstBotF,
          InstIntroF,
          InstElimF,
          InstAbstrFRef,
          InstUnderFRef,
          InstInsideF,
          InstSeqF
        )
) where

import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (mapAccumL)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set

import MLF.Constraint.Types.Graph (BaseTy(..), BindFlag(..), NodeId(..))
import MLF.Frontend.Symbol (SymbolIdentity, symbolRefMatches)
import MLF.Frontend.Syntax (Lit(..))
import qualified MLF.Primitive.Identity as PrimitiveIdentity
import MLF.Types.Identity
    ( ConstructorRef
    , DeferredRef
    , EnvRef
    , IdDetails(..)
    , IdentityGenerator
    , LocalRef
    , TypeBinderIdentity
    , UniqueIdentity(..)
    , idDetailsConstructorRef
    , idDetailsDisplayName
    , idDetailsGeneratedIdentities
    , idDetailsLocalRef
    , idDetailsIsEvidence
    , idDetailsIsDiscard
    , idDetailsIsLocal
    , idDetailsRenameLocal
    , idDetailsReferenceName
    , idDetailsSameIdentity
    , idDetailsSymbolIdentity
    , constructorRefSymbol
    , freshIdentity
    , freshenLocalRef
    , identityGeneratorAfter
    , deferredRefName
    , localRefName
    , primitiveRefSymbol
    , renameDeferredRef
    , symbolGeneratedIdentities
    , typeBinderGeneratedIdentities
    , typeBinderIdentityFromNode
    , typeBinderIdentityFromUnique
    , typeBinderIdentityKey
    , typeBinderIdentityNode
    )
import Util.IndexedRecursion
    ( IxFunctor(..)
    , IxBase
    , IxRecursive(..)
    , IxCorecursive(..)
    , IxPair(..)
    , IxFix(..)
    , K(..)
    , cataIx
    , cataIxConst
    , paraIx
    , zygoIx
    )

-- | Explicitly typed types for elaboration (xMLF).
-- Corresponds to Figure 1 in "A Church-Style Intermediate Language for MLF".
--
-- xMLF extends System F with instance-bounded polymorphism (flexible quantification):
--   ∀(α ⩾ τ). σ
--
-- This restricts the variable α to range only over instances of τ.
--
-- Constructors:
--   * TVarRef: Type variables (α), identified by 'TypeBinderRef'.
--   * TArrow: Function types (τ -> τ)
--   * TCon: Constructor application (C σ), per thesis Fig. 14.2.1.
--   * TVarAppRef: Erased higher-kinded variable application (f σ).
--   * TBase: Base types (Int, Bool, etc.). This is a 0-ary constructor convenience.
--   * TForallRef: Flexible quantification ∀(α ⩾ τ). σ.
--       - Nothing bound implies ⩾ ⊥ (standard System F unbounded quantification)
--       - Just bound implies explicit instance bound
--   * TMuRef: Explicit iso-recursive type μ α. τ.
--   * TBottom: The bottom type ⊥ (minimal type), used as the default bound.
data TopVar = AllowVar | NoTopVar

data Ty (v :: TopVar) where
    TVarRef :: TypeBinderRef -> Ty 'AllowVar
    TArrow :: Ty AllowVar -> Ty AllowVar -> Ty a
    TConWithIdentity :: Maybe SymbolIdentity -> BaseTy -> NonEmpty (Ty AllowVar) -> Ty a
    TVarAppRef :: TypeBinderRef -> NonEmpty (Ty AllowVar) -> Ty a
    TBaseWithIdentity :: Maybe SymbolIdentity -> BaseTy -> Ty a
    TForallRef :: TypeBinderRef -> Maybe (Ty 'NoTopVar) -> Ty AllowVar -> Ty a -- ∀(α ⩾ τ?). σ
    TMuRef :: TypeBinderRef -> Ty 'AllowVar -> Ty a
    TBottom :: Ty a

pattern TCon :: BaseTy -> NonEmpty (Ty AllowVar) -> Ty a
pattern TCon con args <-
    TConWithIdentity _ con args
  where
    TCon con@(BaseTy name) args =
        TConWithIdentity (PrimitiveIdentity.builtinTypeHeadIdentity name) con args

pattern TBase :: BaseTy -> Ty a
pattern TBase base <-
    TBaseWithIdentity _ base
  where
    TBase base@(BaseTy name) =
        TBaseWithIdentity (PrimitiveIdentity.builtinTypeHeadIdentity name) base

{-# COMPLETE TVarRef, TArrow, TCon, TVarAppRef, TBase, TForallRef, TMuRef, TBottom #-}

instance Eq (Ty v) where
    left == right =
        case (left, right) of
            (TVarRef leftRef, TVarRef rightRef) ->
                leftRef == rightRef
            (TArrow leftArg leftResult, TArrow rightArg rightResult) ->
                leftArg == rightArg && leftResult == rightResult
            (TConWithIdentity leftIdentity leftCon leftArgs, TConWithIdentity rightIdentity rightCon rightArgs) ->
                typeHeadRefMatches leftIdentity leftCon rightIdentity rightCon && leftArgs == rightArgs
            (TVarAppRef leftRef leftArgs, TVarAppRef rightRef rightArgs) ->
                leftRef == rightRef && leftArgs == rightArgs
            (TBaseWithIdentity leftIdentity leftBase, TBaseWithIdentity rightIdentity rightBase) ->
                typeHeadRefMatches leftIdentity leftBase rightIdentity rightBase
            (TForallRef leftRef leftBound leftBody, TForallRef rightRef rightBound rightBody) ->
                leftRef == rightRef && leftBound == rightBound && leftBody == rightBody
            (TMuRef leftRef leftBody, TMuRef rightRef rightBody) ->
                leftRef == rightRef && leftBody == rightBody
            (TBottom, TBottom) ->
                True
            _ ->
                False

typeHeadRefMatches :: Maybe SymbolIdentity -> BaseTy -> Maybe SymbolIdentity -> BaseTy -> Bool
typeHeadRefMatches leftIdentity (BaseTy leftName) rightIdentity (BaseTy rightName) =
    symbolRefMatches leftIdentity leftName rightIdentity rightName

deriving instance Show (Ty v)

type ElabType = Ty 'AllowVar
type BoundType = Ty 'NoTopVar

tVarWithRef :: TypeBinderRef -> Ty 'AllowVar
tVarWithRef = TVarRef

tVarAppWithRef :: TypeBinderRef -> NonEmpty (Ty AllowVar) -> Ty a
tVarAppWithRef = TVarAppRef

tForallWithRef :: TypeBinderRef -> Maybe BoundType -> ElabType -> Ty a
tForallWithRef = TForallRef

tMuWithRef :: TypeBinderRef -> ElabType -> Ty a
tMuWithRef = TMuRef

-- | Indexed base functor for Ty. Recursive positions are explicitly indexed.
data TyIF (v :: TopVar) (r :: TopVar -> Type) where
    TVarIFRef :: TypeBinderRef -> TyIF 'AllowVar r
    TArrowIF :: r 'AllowVar -> r 'AllowVar -> TyIF v r
    TConIFWithIdentity :: Maybe SymbolIdentity -> BaseTy -> NonEmpty (r 'AllowVar) -> TyIF v r
    TVarAppIFRef :: TypeBinderRef -> NonEmpty (r 'AllowVar) -> TyIF v r
    TBaseIFWithIdentity :: Maybe SymbolIdentity -> BaseTy -> TyIF v r
    TForallIFRef :: TypeBinderRef -> Maybe (r 'NoTopVar) -> r 'AllowVar -> TyIF v r
    TMuIFRef :: TypeBinderRef -> r 'AllowVar -> TyIF v r
    TBottomIF :: TyIF v r

pattern TConIF :: BaseTy -> NonEmpty (r 'AllowVar) -> TyIF v r
pattern TConIF con args <-
    TConIFWithIdentity _ con args
  where
    TConIF con@(BaseTy name) args =
        TConIFWithIdentity (PrimitiveIdentity.builtinTypeHeadIdentity name) con args

pattern TBaseIF :: BaseTy -> TyIF v r
pattern TBaseIF base <-
    TBaseIFWithIdentity _ base
  where
    TBaseIF base@(BaseTy name) =
        TBaseIFWithIdentity (PrimitiveIdentity.builtinTypeHeadIdentity name) base

{-# COMPLETE TVarIFRef, TArrowIF, TConIF, TVarAppIFRef, TBaseIF, TForallIFRef, TMuIFRef, TBottomIF #-}

instance IxFunctor TyIF where
    imap f node = case node of
        TVarIFRef ref -> TVarIFRef ref
        TArrowIF a b -> TArrowIF (f a) (f b)
        TConIFWithIdentity identity c args -> TConIFWithIdentity identity c (fmap f args)
        TVarAppIFRef ref args -> TVarAppIFRef ref (fmap f args)
        TBaseIFWithIdentity identity b -> TBaseIFWithIdentity identity b
        TForallIFRef ref mb body -> TForallIFRef ref (fmap f mb) (f body)
        TMuIFRef ref body -> TMuIFRef ref (f body)
        TBottomIF -> TBottomIF

type instance IxBase Ty = TyIF

instance IxRecursive Ty where
    projectIx ty = case ty of
        TVarRef ref -> TVarIFRef ref
        TArrow a b -> TArrowIF a b
        TConWithIdentity identity c args -> TConIFWithIdentity identity c args
        TVarAppRef ref args -> TVarAppIFRef ref args
        TBaseWithIdentity identity b -> TBaseIFWithIdentity identity b
        TForallRef ref mb body -> TForallIFRef ref mb body
        TMuRef ref body -> TMuIFRef ref body
        TBottom -> TBottomIF

instance IxCorecursive Ty where
    embedIx ty = case ty of
        TVarIFRef ref -> TVarRef ref
        TArrowIF a b -> TArrow a b
        TConIFWithIdentity identity c args -> TConWithIdentity identity c args
        TVarAppIFRef ref args -> TVarAppRef ref args
        TBaseIFWithIdentity identity b -> TBaseWithIdentity identity b
        TForallIFRef ref mb body -> TForallRef ref mb body
        TMuIFRef ref body -> TMuRef ref body
        TBottomIF -> TBottom

tyToElab :: Ty v -> ElabType
tyToElab ty = case ty of
    TVarRef ref -> TVarRef ref
    TArrow a b -> TArrow (tyToElab a) (tyToElab b)
    TConWithIdentity identity c args -> TConWithIdentity identity c (fmap tyToElab args)
    TVarAppRef ref args -> TVarAppRef ref (fmap tyToElab args)
    TBaseWithIdentity identity b -> TBaseWithIdentity identity b
    TBottom -> TBottom
    TForallRef ref mb body -> TForallRef ref mb (tyToElab body)
    TMuRef ref body -> TMuRef ref (tyToElab body)

elabToBound :: ElabType -> Either String BoundType
elabToBound ty = case ty of
    TVarRef ref ->
        Left ("elabToBound: unexpected variable bound " ++ show (typeBinderRefName ref))
    TArrow a b -> Right (TArrow a b)
    TConWithIdentity identity c args -> Right (TConWithIdentity identity c args)
    TVarAppRef ref args -> Right (TVarAppRef ref args)
    TBaseWithIdentity identity b -> Right (TBaseWithIdentity identity b)
    TForallRef ref mb body -> Right (TForallRef ref mb body)
    TMuRef ref body -> Right (TMuRef ref body)
    TBottom -> Right TBottom

containsForallTy :: Ty v -> Bool
containsForallTy = cataIxConst alg
  where
    alg node = case node of
        TForallIFRef _ _ _ -> True
        TMuIFRef _ body -> unK body
        TArrowIF a b -> unK a || unK b
        TConIF _ args -> any unK args
        TVarAppIFRef _ args -> any unK args
        _ -> False

containsArrowTy :: Ty v -> Bool
containsArrowTy = cataIxConst alg
  where
    alg node = case node of
        TArrowIF _ _ -> True
        TForallIFRef _ mb body -> maybe False unK mb || unK body
        TMuIFRef _ body -> unK body
        TConIF _ args -> any unK args
        TVarAppIFRef _ args -> any unK args
        _ -> False

data Binder (k :: BindFlag) where
    FlexBinder :: TypeBinderRef -> Maybe BoundType -> Binder 'BindFlex

data Scheme (k :: BindFlag) where
    Scheme :: [Binder k] -> ElabType -> Scheme k

type ElabScheme = Scheme 'BindFlex

mkElabSchemeWithRefs :: [(TypeBinderRef, Maybe BoundType)] -> ElabType -> ElabScheme
mkElabSchemeWithRefs binds body =
    Scheme (map (\(ref, mb) -> FlexBinder ref mb) binds) body

schemeBinderRefs :: ElabScheme -> [(TypeBinderRef, Maybe BoundType)]
schemeBinderRefs (Scheme binds _) =
    map (\(FlexBinder ref mb) -> (ref, mb)) binds

schemeBody :: ElabScheme -> ElabType
schemeBody (Scheme _ body) = body

schemeToResolvedType :: ElabScheme -> ElabType
schemeToResolvedType (Scheme binds body) =
    foldr (\(FlexBinder ref mbBound) acc -> TForallRef ref mbBound acc) body binds

instance Eq (Scheme 'BindFlex) where
    s1 == s2 =
        schemeBinderRefs s1 == schemeBinderRefs s2
            && schemeBody s1 == schemeBody s2

instance Show (Scheme 'BindFlex) where
    show s =
        let binds = map (\(ref, mb) -> (typeBinderRefName ref, mb)) (schemeBinderRefs s)
            body = schemeBody s
        in "Forall " ++ show binds ++ " " ++ show body

-- | Environment entry for elaboration (let-generalized schemes only).
data SchemeInfo = SchemeInfo
    { siScheme :: ElabScheme
    , siSubstRefs :: IntMap TypeBinderRef
    } deriving (Eq, Show)

data TypeBinderRef = TypeBinderRef
    { typeBinderRefIdentity :: TypeBinderIdentity
    , typeBinderRefName :: String
    }
    deriving (Show)

instance Eq TypeBinderRef where
    left == right =
        typeBinderRefIdentity left == typeBinderRefIdentity right

instance Ord TypeBinderRef where
    compare left right =
        compare (typeBinderRefIdentity left) (typeBinderRefIdentity right)

typeBinderRefFromIdentity :: TypeBinderIdentity -> String -> TypeBinderRef
typeBinderRefFromIdentity identity name =
    TypeBinderRef
        { typeBinderRefIdentity = identity
        , typeBinderRefName = name
        }

typeBinderRefNode :: TypeBinderRef -> Maybe NodeId
typeBinderRefNode =
    typeBinderIdentityNode . typeBinderRefIdentity

renameTypeBinderRef :: String -> TypeBinderRef -> TypeBinderRef
renameTypeBinderRef name ref =
    ref { typeBinderRefName = name }

typeBinderRefsSameIdentity :: TypeBinderRef -> TypeBinderRef -> Bool
typeBinderRefsSameIdentity left right =
    typeBinderRefIdentity left == typeBinderRefIdentity right

typeBinderRefsSameIdentityAndName :: TypeBinderRef -> TypeBinderRef -> Bool
typeBinderRefsSameIdentityAndName left right =
    typeBinderRefsSameIdentity left right
        && typeBinderRefName left == typeBinderRefName right

freshTypeBinderRef :: String -> IdentityGenerator -> (TypeBinderRef, IdentityGenerator)
freshTypeBinderRef name generator =
    let (identity, generator') = freshIdentity generator
     in (typeBinderRefFromIdentity (typeBinderIdentityFromUnique identity) name, generator')

sourceTypeBinderRefForName :: String -> IdentityGenerator -> (TypeBinderRef, IdentityGenerator)
sourceTypeBinderRefForName name generator =
    freshTypeBinderRef name generator

freshTypeBinderRefFromNames :: Set.Set String -> IdentityGenerator -> (TypeBinderRef, IdentityGenerator)
freshTypeBinderRefFromNames used generator =
    let (identity, generator') = freshIdentity generator
        name = "u" ++ show (uniqueIdentityValue identity)
     in if Set.member name used
            then freshTypeBinderRefFromNames used generator'
            else (typeBinderRefFromIdentity (typeBinderIdentityFromUnique identity) name, generator')

schemeInfoFromRefSubst :: ElabScheme -> IntMap TypeBinderRef -> SchemeInfo
schemeInfoFromRefSubst scheme refs =
    SchemeInfo
        { siScheme = attachBinderRefsToScheme refs scheme
        , siSubstRefs = refs
        }

schemeInfoBinderIdentityKeys :: SchemeInfo -> [Int]
schemeInfoBinderIdentityKeys =
    IntMap.keys . siSubstRefs

schemeInfoBinderIdentityKeySet :: SchemeInfo -> IntSet.IntSet
schemeInfoBinderIdentityKeySet =
    IntSet.fromAscList . schemeInfoBinderIdentityKeys

schemeInfoBinderRefSubst :: SchemeInfo -> IntMap TypeBinderRef
schemeInfoBinderRefSubst =
    siSubstRefs

attachBinderRefsToScheme :: IntMap TypeBinderRef -> ElabScheme -> ElabScheme
attachBinderRefsToScheme refs (Scheme binds body) =
    Scheme binds' (applyBinderRefRenames renames body)
  where
    refList = IntMap.elems refs
    ((_, renames), binds') = mapAccumL attachToBinder (refList, []) binds

    attachToBinder (remaining, renamesSoFar) (FlexBinder ref mb) =
        let mb' = fmap (applyBinderRefRenames renamesSoFar) mb
         in case takeFirstRef (typeBinderRefsSameIdentity ref) remaining of
                Just (ref', remaining') -> attachRef remaining' renamesSoFar ref ref' mb'
                Nothing -> ((remaining, renamesSoFar), FlexBinder ref mb')

    attachRef remaining renamesSoFar ref ref' mb
        | typeBinderRefsSameIdentityAndName ref ref' = ((remaining, renamesSoFar), FlexBinder ref' mb)
        | otherwise = ((remaining, renamesSoFar ++ [(ref, ref')]), FlexBinder ref' mb)

    takeFirstRef _ [] = Nothing
    takeFirstRef predicate (ref : rest)
        | predicate ref = Just (ref, rest)
        | otherwise = do
            (ref', rest') <- takeFirstRef predicate rest
            pure (ref', ref : rest')

    applyBinderRefRenames renames0 ty =
        foldr
            (\(oldRef, newRef) acc -> replaceBinderRef oldRef newRef acc)
            ty
            renames0

    replaceBinderRef :: TypeBinderRef -> TypeBinderRef -> Ty v -> Ty v
    replaceBinderRef target replacement ty =
        case ty of
            TVarRef ref
                | binderRefMatches target ref -> TVarRef replacement
                | otherwise -> TVarRef ref
            TArrow a b -> TArrow (replaceBinderRef target replacement a) (replaceBinderRef target replacement b)
            TConWithIdentity identity c args ->
                TConWithIdentity identity c (fmap (replaceBinderRef target replacement) args)
            TVarAppRef ref args
                | binderRefMatches target ref -> TVarAppRef replacement args'
                | otherwise -> TVarAppRef ref args'
              where
                args' = fmap (replaceBinderRef target replacement) args
            TBaseWithIdentity identity b -> TBaseWithIdentity identity b
            TBottom -> TBottom
            TForallRef ref mb forallBody ->
                let mb' = fmap (replaceBinderRef target replacement) mb
                 in if binderRefShadows target ref
                        then TForallRef ref mb' forallBody
                        else TForallRef ref mb' (replaceBinderRef target replacement forallBody)
            TMuRef ref muBody
                | binderRefShadows target ref -> TMuRef ref muBody
                | otherwise -> TMuRef ref (replaceBinderRef target replacement muBody)

    binderRefMatches target ref =
        typeBinderRefsSameIdentity target ref

    binderRefShadows target ref =
        typeBinderRefsSameIdentity target ref

data ResolvedVar = ResolvedVar
    { resolvedVarRuntimeName :: String,
      resolvedVarType :: ElabType,
      resolvedVarDetails :: IdDetails
    }
    deriving (Show)

data ResolvedTermIdentityKey
    = ResolvedTermLocalKey LocalRef
    | ResolvedTermEnvKey EnvRef
    | ResolvedTermTopLevelKey SymbolIdentity
    | ResolvedTermConstructorKey SymbolIdentity
    | ResolvedTermMethodKey SymbolIdentity
    | ResolvedTermPrimitiveKey SymbolIdentity
    | ResolvedTermDeferredKey DeferredRef
    deriving (Eq, Ord, Show)

idDetailsIdentityKey :: IdDetails -> ResolvedTermIdentityKey
idDetailsIdentityKey details =
    case details of
        LocalId ref -> ResolvedTermLocalKey ref
        EvidenceId ref -> ResolvedTermLocalKey ref
        EnvId ref -> ResolvedTermEnvKey ref
        TopLevelId symbol -> ResolvedTermTopLevelKey symbol
        ConstructorId ref -> ResolvedTermConstructorKey (constructorRefSymbol ref)
        MethodId symbol -> ResolvedTermMethodKey symbol
        PrimitiveId ref -> ResolvedTermPrimitiveKey (primitiveRefSymbol ref)
        DeferredId ref -> ResolvedTermDeferredKey ref

resolvedVarIdentityKey :: ResolvedVar -> ResolvedTermIdentityKey
resolvedVarIdentityKey =
    idDetailsIdentityKey . resolvedVarDetails

instance Eq ResolvedVar where
    left == right =
        resolvedVarType left == resolvedVarType right
            && resolvedVarDetails left == resolvedVarDetails right

resolvedVarReferenceName :: ResolvedVar -> String
resolvedVarReferenceName resolved =
    idDetailsReferenceName (resolvedVarRuntimeName resolved) (resolvedVarDetails resolved)

resolvedVarName :: ResolvedVar -> String
resolvedVarName resolved =
    idDetailsDisplayName (resolvedVarRuntimeName resolved) (resolvedVarDetails resolved)

resolvedVarConstructorRef :: ResolvedVar -> Maybe ConstructorRef
resolvedVarConstructorRef = idDetailsConstructorRef . resolvedVarDetails

resolvedVarLocalRef :: ResolvedVar -> Maybe LocalRef
resolvedVarLocalRef = idDetailsLocalRef . resolvedVarDetails

resolvedVarSymbolIdentity :: ResolvedVar -> Maybe SymbolIdentity
resolvedVarSymbolIdentity = idDetailsSymbolIdentity . resolvedVarDetails

resolvedVarIsLocal :: ResolvedVar -> Bool
resolvedVarIsLocal = idDetailsIsLocal . resolvedVarDetails

resolvedVarIsEvidence :: ResolvedVar -> Bool
resolvedVarIsEvidence = idDetailsIsEvidence . resolvedVarDetails

resolvedVarIsDiscard :: ResolvedVar -> Bool
resolvedVarIsDiscard = idDetailsIsDiscard . resolvedVarDetails

resolvedVarSameIdentity :: ResolvedVar -> ResolvedVar -> Bool
resolvedVarSameIdentity left right =
    idDetailsSameIdentity (resolvedVarDetails left) (resolvedVarDetails right)

resolvedVarBoundBy :: [ResolvedVar] -> ResolvedVar -> Bool
resolvedVarBoundBy bound resolved
    | resolvedVarIsLocal resolved = any (`resolvedVarSameIdentity` resolved) bound
    | otherwise = False

mapResolvedVarType :: (ElabType -> ElabType) -> ResolvedVar -> ResolvedVar
mapResolvedVarType f resolved =
    resolved {resolvedVarType = f (resolvedVarType resolved)}

renameResolvedLocalVar :: String -> ResolvedVar -> ResolvedVar
renameResolvedLocalVar name resolved =
    if resolvedVarIsLocal resolved
        then
            resolved
                { resolvedVarRuntimeName = name
                , resolvedVarDetails = idDetailsRenameLocal name (resolvedVarDetails resolved)
                }
        else resolved

freshenResolvedLocalVar :: String -> IdentityGenerator -> ResolvedVar -> (ResolvedVar, IdentityGenerator)
freshenResolvedLocalVar name generator resolved =
    case resolvedVarDetails resolved of
        LocalId ref ->
            freshen LocalId ref
        EvidenceId ref ->
            freshen EvidenceId ref
        _ ->
            (resolved, generator)
  where
    freshen wrap ref =
        let (ref', generator') = freshenLocalRef name generator ref
         in ( resolved
                { resolvedVarRuntimeName = name
                , resolvedVarDetails = wrap ref'
                }
            , generator'
            )

renameResolvedDeferredVar :: String -> ResolvedVar -> ResolvedVar
renameResolvedDeferredVar name resolved =
    case resolvedVarDetails resolved of
        DeferredId ref ->
            resolved
                { resolvedVarRuntimeName = name
                , resolvedVarDetails = DeferredId (renameDeferredRef name ref)
                }
        _ -> resolved

-- | Instantiation witnesses (φ) for xMLF.
-- These explicitly record how a polymorphic type is instantiated.
--
-- From the FLOPS 2010 paper:
--   φ ::= 1        -- identity
--       | ⟨τ⟩      -- type application (substitute for outermost var)
--       | τ        -- bottom instantiation (substitute ⊥ with τ)
--       | O        -- introduce ∀ (skip outermost quantifier)
--       | φ; φ'    -- sequential composition
data Instantiation
    = InstId                                -- 1 (identity)
    | InstApp ElabType                      -- ⟨τ⟩ (type application)
    | InstBot ElabType                      -- τ (instantiate ⊥)
    | InstIntro                             -- O (introduce/skip ∀)
    | InstElim                              -- N (eliminate ∀)
    | InstAbstrRef TypeBinderRef            -- !α (abstract bound)
    | InstUnderRef TypeBinderRef Instantiation -- ∀(α ⩾) φ (under)
    | InstInside Instantiation              -- ∀(⩾ φ) (inside)
    | InstSeq Instantiation Instantiation   -- φ; φ' (composition)
    deriving (Eq, Show)

instAbstrWithRef :: TypeBinderRef -> Instantiation
instAbstrWithRef = InstAbstrRef

instUnderWithRef :: TypeBinderRef -> Instantiation -> Instantiation
instUnderWithRef = InstUnderRef

data InstantiationF a
    = InstIdF
    | InstAppF ElabType
    | InstBotF ElabType
    | InstIntroF
    | InstElimF
    | InstAbstrFRef TypeBinderRef
    | InstUnderFRef TypeBinderRef a
    | InstInsideF a
    | InstSeqF a a
    deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base Instantiation = InstantiationF

instance Recursive Instantiation where
    project inst = case inst of
        InstId -> InstIdF
        InstApp ty -> InstAppF ty
        InstBot ty -> InstBotF ty
        InstIntro -> InstIntroF
        InstElim -> InstElimF
        InstAbstrRef ref -> InstAbstrFRef ref
        InstUnderRef ref i -> InstUnderFRef ref i
        InstInside i -> InstInsideF i
        InstSeq a b -> InstSeqF a b

instance Corecursive Instantiation where
    embed inst = case inst of
        InstIdF -> InstId
        InstAppF ty -> InstApp ty
        InstBotF ty -> InstBot ty
        InstIntroF -> InstIntro
        InstElimF -> InstElim
        InstAbstrFRef ref -> InstAbstrRef ref
        InstUnderFRef ref i -> InstUnderRef ref i
        InstInsideF i -> InstInside i
        InstSeqF a b -> InstSeq a b

deferredResolvedVarFromRef :: DeferredRef -> ResolvedVar
deferredResolvedVarFromRef ref =
    ResolvedVar
        { resolvedVarRuntimeName = deferredRefName ref
        , resolvedVarType = TBottom
        , resolvedVarDetails = DeferredId ref
        }

localResolvedVarFromRef :: LocalRef -> ElabType -> ResolvedVar
localResolvedVarFromRef localRef ty =
    let name = localRefName localRef
     in ResolvedVar
            { resolvedVarRuntimeName = name
            , resolvedVarType = ty
            , resolvedVarDetails = LocalId localRef
            }

deferredResolvedVarRef :: ResolvedVar -> Maybe DeferredRef
deferredResolvedVarRef resolved =
    case resolvedVarDetails resolved of
        DeferredId ref -> Just ref
        _ -> Nothing

mkDeferredVarWithRef :: DeferredRef -> XmlfTerm
mkDeferredVarWithRef = EVarNode . deferredResolvedVarFromRef

mkLocalLamWithRef :: LocalRef -> ElabType -> XmlfTerm -> XmlfTerm
mkLocalLamWithRef localRef ty body =
    let resolved = localResolvedVarFromRef localRef ty
     in ELam resolved body

mkLocalLetWithRef :: LocalRef -> ElabScheme -> XmlfTerm -> XmlfTerm -> XmlfTerm
mkLocalLetWithRef localRef scheme rhs body =
    let resolved = localResolvedVarFromRef localRef (schemeToResolvedType scheme)
     in ELet resolved scheme rhs body

mkLocalRecursiveLetWithRef :: LocalRef -> ElabScheme -> XmlfTerm -> XmlfTerm -> XmlfTerm
mkLocalRecursiveLetWithRef localRef scheme rhs body =
    let resolved = localResolvedVarFromRef localRef (schemeToResolvedType scheme)
     in ELet resolved scheme rhs body

identityGeneratorAfterType :: Ty v -> IdentityGenerator
identityGeneratorAfterType =
    identityGeneratorAfter . generatedIdentitiesInType

identityGeneratorAfterTerm :: XmlfTerm -> IdentityGenerator
identityGeneratorAfterTerm =
    identityGeneratorAfter . generatedIdentitiesInTerm

generatedIdentitiesInType :: Ty v -> [UniqueIdentity]
generatedIdentitiesInType ty =
    case ty of
        TVarRef ref -> generatedIdentitiesInTypeBinderRef ref
        TArrow a b -> generatedIdentitiesInType a ++ generatedIdentitiesInType b
        TConWithIdentity identity _ args ->
            maybe [] symbolGeneratedIdentities identity ++ foldMap generatedIdentitiesInType args
        TVarAppRef ref args ->
            generatedIdentitiesInTypeBinderRef ref ++ foldMap generatedIdentitiesInType args
        TBaseWithIdentity identity _ -> maybe [] symbolGeneratedIdentities identity
        TForallRef ref mb body ->
            generatedIdentitiesInTypeBinderRef ref
                ++ maybe [] generatedIdentitiesInType mb
                ++ generatedIdentitiesInType body
        TMuRef ref body ->
            generatedIdentitiesInTypeBinderRef ref ++ generatedIdentitiesInType body
        TBottom -> []

generatedIdentitiesInScheme :: ElabScheme -> [UniqueIdentity]
generatedIdentitiesInScheme (Scheme binds body) =
    foldMap generatedIdentitiesInBinder binds ++ generatedIdentitiesInType body
  where
    generatedIdentitiesInBinder (FlexBinder ref mb) =
        generatedIdentitiesInTypeBinderRef ref ++ maybe [] generatedIdentitiesInType mb

generatedIdentitiesInTypeBinderRef :: TypeBinderRef -> [UniqueIdentity]
generatedIdentitiesInTypeBinderRef =
    typeBinderGeneratedIdentities . typeBinderRefIdentity

generatedIdentitiesInInstantiation :: Instantiation -> [UniqueIdentity]
generatedIdentitiesInInstantiation inst =
    case inst of
        InstId -> []
        InstApp ty -> generatedIdentitiesInType ty
        InstBot ty -> generatedIdentitiesInType ty
        InstIntro -> []
        InstElim -> []
        InstAbstrRef ref -> generatedIdentitiesInTypeBinderRef ref
        InstUnderRef ref inner ->
            generatedIdentitiesInTypeBinderRef ref ++ generatedIdentitiesInInstantiation inner
        InstInside inner -> generatedIdentitiesInInstantiation inner
        InstSeq left right ->
            generatedIdentitiesInInstantiation left ++ generatedIdentitiesInInstantiation right

generatedIdentitiesInTerm :: XmlfTerm -> [UniqueIdentity]
generatedIdentitiesInTerm term =
    case term of
        EVarNode resolved -> generatedIdentitiesInResolved resolved
        ELit {} -> []
        ELam resolved body ->
            generatedIdentitiesInResolved resolved ++ generatedIdentitiesInTerm body
        EApp fun arg ->
            generatedIdentitiesInTerm fun ++ generatedIdentitiesInTerm arg
        ELet resolved scheme rhs body ->
            generatedIdentitiesInResolved resolved
                ++ generatedIdentitiesInScheme scheme
                ++ generatedIdentitiesInTerm rhs
                ++ generatedIdentitiesInTerm body
        ETyAbsRef ref mb body ->
            generatedIdentitiesInTypeBinderRef ref
                ++ maybe [] generatedIdentitiesInType mb
                ++ generatedIdentitiesInTerm body
        ETyInst inner inst ->
            generatedIdentitiesInTerm inner ++ generatedIdentitiesInInstantiation inst
        ERoll ty body -> generatedIdentitiesInType ty ++ generatedIdentitiesInTerm body
        EUnroll body -> generatedIdentitiesInTerm body

generatedIdentitiesInResolved :: ResolvedVar -> [UniqueIdentity]
generatedIdentitiesInResolved resolved =
    idDetailsGeneratedIdentities (resolvedVarDetails resolved)
        ++ generatedIdentitiesInType (resolvedVarType resolved)

-- | Explicitly typed terms with type abstractions and instantiations (xMLF).
data XmlfTerm
    = EVarNode ResolvedVar
    | ELit Lit
    | ELam ResolvedVar XmlfTerm
    | EApp XmlfTerm XmlfTerm
    | ELet ResolvedVar ElabScheme XmlfTerm XmlfTerm
    | ETyAbsRef TypeBinderRef (Maybe BoundType) XmlfTerm -- Λ(α ⩾ τ?). e (bounded type abstraction)
    | ETyInst XmlfTerm Instantiation           -- e φ (instantiation)
    | ERoll ElabType XmlfTerm                  -- internal iso-recursive runtime constructor
    | EUnroll XmlfTerm                         -- internal iso-recursive runtime destructor
    deriving (Eq, Show)

eTyAbsWithRef :: TypeBinderRef -> Maybe BoundType -> XmlfTerm -> XmlfTerm
eTyAbsWithRef = ETyAbsRef

data XmlfTermF a
    = EVarNodeF ResolvedVar
    | ELitF Lit
    | ELamF ResolvedVar a
    | EAppF a a
    | ELetF ResolvedVar ElabScheme a a
    | ETyAbsFRef TypeBinderRef (Maybe BoundType) a
    | ETyInstF a Instantiation
    | ERollF ElabType a
    | EUnrollF a
    deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Base XmlfTerm = XmlfTermF

instance Recursive XmlfTerm where
    project term = case term of
        EVarNode resolved -> EVarNodeF resolved
        ELit l -> ELitF l
        ELam resolved body -> ELamF resolved body
        EApp f a -> EAppF f a
        ELet resolved sch rhs body -> ELetF resolved sch rhs body
        ETyAbsRef ref mb body -> ETyAbsFRef ref mb body
        ETyInst e inst -> ETyInstF e inst
        ERoll ty body -> ERollF ty body
        EUnroll body -> EUnrollF body

instance Corecursive XmlfTerm where
    embed term = case term of
        EVarNodeF resolved -> EVarNode resolved
        ELitF l -> ELit l
        ELamF resolved body -> ELam resolved body
        EAppF f a -> EApp f a
        ELetF resolved sch rhs body -> ELet resolved sch rhs body
        ETyAbsFRef ref mb body -> ETyAbsRef ref mb body
        ETyInstF e inst -> ETyInst e inst
        ERollF ty body -> ERoll ty body
        EUnrollF body -> EUnroll body
