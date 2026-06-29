{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module ElabTermTestSupport
  ( generatedLocalRef,
    generatedLocalRefForName,
    generatedDeferredRef,
    generatedDeferredRefForName,
    generatedResolvedLocal,
    generatedResolvedLocalForName,
    mkTestDeferredVar,
    mkTestLocalLam,
    mkTestLocalLet,
    mkTestRecursiveLocalLet,
    mkTestTyAbs,
    testTVar,
    testTVarApp,
    testTForall,
    testTMu,
  )
where

import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import MLF.Elab.Types
  ( mapBoundType,
    schemeFromType,
  )
import MLF.Types.Elab
  ( ElabScheme,
    ElabType,
    BoundType,
    Instantiation (..),
    ResolvedVar (..),
    TypeBinderRef,
    Ty (..),
    XmlfTerm (..),
    eTyAbsWithRef,
    freshTypeBinderRef,
    generatedIdentitiesInTerm,
    identityGeneratorAfterTerm,
    mkDeferredVarWithRef,
    mkLocalRecursiveLetWithRef,
    localResolvedVarFromRef,
    mapResolvedVarType,
    mkLocalLamWithRef,
    mkLocalLetWithRef,
    resolvedVarReferenceName,
    resolvedVarSameIdentity,
    schemeBinderRefs,
    schemeBody,
    tForallWithRef,
    tMuWithRef,
    tVarAppWithRef,
    tVarWithRef,
    typeBinderIdentityFromUnique,
    typeBinderRefIdentity,
    typeBinderRefFromIdentity,
    typeBinderRefName,
  )
import MLF.Types.Identity
  ( DeferredRef,
    deferredRefFromIdentity,
    deferredRefName,
    IdDetails (..),
    LocalIdentity (..),
    LocalRef,
    localRefFromIdentity,
    UniqueIdentity (..),
  )

generatedLocalRef :: Int -> String -> LocalRef
generatedLocalRef unique name =
  localRefFromIdentity (GeneratedLocalId (UniqueIdentity unique)) name

generatedLocalRefForName :: String -> LocalRef
generatedLocalRefForName name =
  generatedLocalRef (stableFixtureIdentity name) name

generatedDeferredRef :: Int -> String -> DeferredRef
generatedDeferredRef unique name =
  deferredRefFromIdentity (UniqueIdentity unique) name

generatedDeferredRefForName :: String -> DeferredRef
generatedDeferredRefForName name =
  generatedDeferredRef (stableFixtureIdentity name) name

generatedResolvedLocal :: Int -> String -> String -> ElabType -> ResolvedVar
generatedResolvedLocal unique referenceName runtimeName ty =
  (localResolvedVarFromRef (generatedLocalRef unique referenceName) ty)
    { resolvedVarRuntimeName = runtimeName
    }

generatedResolvedLocalForName :: String -> String -> ElabType -> ResolvedVar
generatedResolvedLocalForName referenceName runtimeName ty =
  (localResolvedVarFromRef (generatedLocalRefForName referenceName) ty)
    { resolvedVarRuntimeName = runtimeName
    }

mkTestDeferredVar :: String -> XmlfTerm
mkTestDeferredVar =
  mkDeferredVarWithRef . generatedDeferredRefForName

mkTestLocalLam :: String -> ElabType -> XmlfTerm -> XmlfTerm
mkTestLocalLam name ty body =
  let localRef = fixtureLocalRef name body
      resolved = localResolvedVarFromRef localRef ty
   in mkLocalLamWithRef localRef ty (resolveFixtureLocalOccurrences resolved body)

mkTestLocalLet :: String -> ElabScheme -> XmlfTerm -> XmlfTerm -> XmlfTerm
mkTestLocalLet name scheme rhs body =
  let localRef = fixtureLocalRef name (EApp rhs body)
      resolved = localResolvedVarFromRef localRef (schemeResolvedType scheme)
   in mkLocalLetWithRef localRef scheme rhs (resolveFixtureLocalOccurrences resolved body)

mkTestRecursiveLocalLet :: String -> ElabScheme -> XmlfTerm -> XmlfTerm -> XmlfTerm
mkTestRecursiveLocalLet name scheme rhs body =
  let localRef = fixtureLocalRef name (EApp rhs body)
      resolved = localResolvedVarFromRef localRef (schemeResolvedType scheme)
   in mkLocalRecursiveLetWithRef
        localRef
        scheme
        (resolveFixtureLocalOccurrences resolved rhs)
        (resolveFixtureLocalOccurrences resolved body)

mkTestTyAbs :: String -> Maybe BoundType -> XmlfTerm -> XmlfTerm
mkTestTyAbs name mb body =
  let (ref, _) = freshTypeBinderRef name (identityGeneratorAfterTerm body)
   in eTyAbsWithRef
        ref
        (fmap (resolveFixtureTypeNameInBound name ref) mb)
        (resolveFixtureTypeNameInTerm name ref body)

testTVar :: String -> ElabType
testTVar =
  tVarWithRef . generatedTypeBinderRefForName

testTVarApp :: String -> NonEmpty ElabType -> Ty a
testTVarApp name =
  tVarAppWithRef (generatedTypeBinderRefForName name)

testTForall :: String -> Maybe BoundType -> ElabType -> Ty a
testTForall name =
  tForallWithRef (generatedTypeBinderRefForName name)

testTMu :: String -> ElabType -> Ty a
testTMu name =
  tMuWithRef (generatedTypeBinderRefForName name)

generatedTypeBinderRefForName :: String -> TypeBinderRef
generatedTypeBinderRefForName name =
  typeBinderRefFromIdentity
    (typeBinderIdentityFromUnique (UniqueIdentity (stableFixtureIdentity name)))
    name

isFixtureTypePlaceholder :: String -> TypeBinderRef -> Bool
isFixtureTypePlaceholder name ref =
  typeBinderRefName ref == name
    && typeBinderRefIdentity ref
      == typeBinderIdentityFromUnique (UniqueIdentity (stableFixtureIdentity name))

schemeResolvedType :: ElabScheme -> ElabType
schemeResolvedType scheme =
  foldr (\(ref, mbBound) acc -> tForallWithRef ref mbBound acc) (schemeBody scheme) (schemeBinderRefs scheme)

resolveFixtureLocalOccurrences :: ResolvedVar -> XmlfTerm -> XmlfTerm
resolveFixtureLocalOccurrences target =
  go
  where
    targetName = resolvedVarReferenceName target

    go term =
      case term of
        EVarNode resolved
          | deferredName resolved == Just targetName ->
              EVarNode target
        EVarNode {} -> term
        ELit {} -> term
        ELam resolved body
          | shadowsTarget resolved -> term
          | otherwise -> ELam resolved (go body)
        EApp fun arg -> EApp (go fun) (go arg)
        ELet resolved scheme rhs body
          | shadowsTarget resolved -> ELet resolved scheme (go rhs) body
          | otherwise -> ELet resolved scheme (go rhs) (go body)
        ETyAbsRef ref mb body -> ETyAbsRef ref mb (go body)
        ETyInst inner inst -> ETyInst (go inner) inst
        ERoll ty body -> ERoll ty (go body)
        EUnroll body -> EUnroll (go body)

    shadowsTarget resolved =
      resolvedVarReferenceName resolved == targetName
        && not (resolvedVarSameIdentity resolved target)

    deferredName resolved =
      case resolvedVarDetails resolved of
        DeferredId ref -> Just (deferredRefName ref)
        _ -> Nothing

resolveFixtureTypeNameInTerm :: String -> TypeBinderRef -> XmlfTerm -> XmlfTerm
resolveFixtureTypeNameInTerm name target =
  go
  where
    go term =
      case term of
        EVarNode resolved ->
          EVarNode (mapResolvedVarType resolveType resolved)
        ELit {} -> term
        ELam resolved body ->
          ELam (mapResolvedVarType resolveType resolved) (go body)
        EApp fun arg ->
          EApp (go fun) (go arg)
        ELet resolved scheme rhs body ->
          ELet
            (mapResolvedVarType resolveType resolved)
            (schemeFromType (resolveType (schemeResolvedType scheme)))
            (go rhs)
            (go body)
        ETyAbsRef ref mb body
          | shadows ref ->
              ETyAbsRef ref (fmap resolveFixtureBound mb) body
          | otherwise ->
              ETyAbsRef ref (fmap resolveFixtureBound mb) (go body)
        ETyInst inner inst ->
          ETyInst (go inner) (resolveInst inst)
        ERoll ty body ->
          ERoll (resolveType ty) (go body)
        EUnroll body ->
          EUnroll (go body)

    resolveType = resolveFixtureTypeNameInType name target
    resolveFixtureBound = resolveFixtureTypeNameInBound name target

    resolveInst inst =
      case inst of
        InstId -> InstId
        InstApp ty -> InstApp (resolveType ty)
        InstBot ty -> InstBot (resolveType ty)
        InstIntro -> InstIntro
        InstElim -> InstElim
        InstInside inner -> InstInside (resolveInst inner)
        InstSeq left right -> InstSeq (resolveInst left) (resolveInst right)
        InstAbstrRef ref -> InstAbstrRef (resolveRef ref)
        InstUnderRef ref inner
          | shadows ref -> InstUnderRef ref inner
          | otherwise -> InstUnderRef (resolveRef ref) (resolveInst inner)

    resolveRef ref
      | unresolvedName ref = target
      | otherwise = ref

    shadows ref =
      typeBinderRefName ref == name

    unresolvedName ref =
      isFixtureTypePlaceholder name ref

resolveFixtureTypeNameInType :: String -> TypeBinderRef -> ElabType -> ElabType
resolveFixtureTypeNameInType name target =
  go
  where
    go ty =
      case ty of
        TVarRef ref
          | unresolvedName ref -> TVarRef target
          | otherwise -> ty
        TArrow left right ->
          TArrow (go left) (go right)
        TCon con args ->
          TCon con (fmap go args)
        TVarAppRef ref args ->
          TVarAppRef (resolveRef ref) (fmap go args)
        TBase {} -> ty
        TBottom -> TBottom
        TForallRef ref mb body
          | shadows ref -> TForallRef ref (fmap resolveBound mb) body
          | otherwise -> TForallRef ref (fmap resolveBound mb) (go body)
        TMuRef ref body
          | shadows ref -> TMuRef ref body
          | otherwise -> TMuRef ref (go body)

    resolveBound = resolveFixtureTypeNameInBound name target

    resolveRef ref
      | unresolvedName ref = target
      | otherwise = ref

    shadows ref =
      typeBinderRefName ref == name

    unresolvedName ref =
      isFixtureTypePlaceholder name ref

resolveFixtureTypeNameInBound :: String -> TypeBinderRef -> BoundType -> BoundType
resolveFixtureTypeNameInBound name target =
  mapBoundType (resolveFixtureTypeNameInType name target)

stableFixtureIdentity :: String -> Int
stableFixtureIdentity =
  foldl' step 5381
  where
    step acc char =
      (acc * 131 + ord char) `mod` 1000000007

fixtureLocalRef :: String -> XmlfTerm -> LocalRef
fixtureLocalRef name term =
  let used = Set.fromList (generatedIdentitiesInTerm term)
      candidate = firstUnusedIdentity used (UniqueIdentity (stableFixtureIdentity name))
   in localRefFromIdentity (GeneratedLocalId candidate) name

firstUnusedIdentity :: Set.Set UniqueIdentity -> UniqueIdentity -> UniqueIdentity
firstUnusedIdentity used identity@(UniqueIdentity value)
  | Set.member identity used = firstUnusedIdentity used (UniqueIdentity (value + 1))
  | otherwise = identity
