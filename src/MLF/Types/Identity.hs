module MLF.Types.Identity
  ( UniqueIdentity (..),
    TypeBinderIdentity (..),
    typeBinderIdentityFromNode,
    typeBinderIdentityNode,
    typeBinderIdentityKey,
    typeBinderIdentityFromUnique,
    LocalIdentity (..),
    IdentityGenerator,
    initialIdentityGenerator,
    identityGeneratorFromNext,
    identityGeneratorAfter,
    freshIdentity,
    LocalRef (..),
    freshLocalRef,
    renameLocalRef,
    EnvRef (..),
    freshEnvRef,
    PrimitiveRef (..),
    DeferredRef (..),
    freshDeferredRef,
    renameDeferredRef,
    ConstructorRef (..),
    IdDetails (..),
    idDetailsReferenceName,
    idDetailsDisplayName,
    idDetailsConstructorRef,
    idDetailsIsLocal,
    idDetailsIsEvidence,
    idDetailsRenameLocal,
    idDetailsSameIdentity,
    idDetailsGeneratedIdentities,
    symbolGeneratedIdentities,
  )
where

import MLF.Constraint.Types.Graph (NodeId (..))
import MLF.Frontend.Symbol (SymbolIdentity (..), SymbolOwnerIdentity (..))
import MLF.Types.Unique

data TypeBinderIdentity
  = GraphTypeBinderIdentity NodeId
  | GeneratedTypeBinderIdentity UniqueIdentity
  deriving (Eq, Ord, Show)

typeBinderIdentityFromNode :: NodeId -> TypeBinderIdentity
typeBinderIdentityFromNode = GraphTypeBinderIdentity

typeBinderIdentityNode :: TypeBinderIdentity -> Maybe NodeId
typeBinderIdentityNode identity =
  case identity of
    GraphTypeBinderIdentity node -> Just node
    GeneratedTypeBinderIdentity {} -> Nothing

typeBinderIdentityFromUnique :: UniqueIdentity -> TypeBinderIdentity
typeBinderIdentityFromUnique = GeneratedTypeBinderIdentity

typeBinderIdentityKey :: TypeBinderIdentity -> Int
typeBinderIdentityKey identity =
  case identity of
    GraphTypeBinderIdentity node -> getNodeId node
    GeneratedTypeBinderIdentity unique -> uniqueIdentityValue unique

data LocalRef = LocalRef
  { localRefIdentity :: LocalIdentity,
    localRefName :: String
  }
  deriving (Show)

data LocalIdentity
  = GeneratedLocalId UniqueIdentity
  deriving (Eq, Ord, Show)

instance Eq LocalRef where
  left == right =
    localRefIdentity left == localRefIdentity right

instance Ord LocalRef where
  compare left right =
    compare (localRefIdentity left) (localRefIdentity right)

freshLocalRef :: String -> IdentityGenerator -> (LocalRef, IdentityGenerator)
freshLocalRef name generator =
  let (identity, generator') = freshIdentity generator
   in (LocalRef (GeneratedLocalId identity) name, generator')

renameLocalRef :: String -> LocalRef -> LocalRef
renameLocalRef name ref =
  ref {localRefName = name}

data EnvRef = EnvRef
  { envRefIdentity :: UniqueIdentity,
    envRefName :: String
  }
  deriving (Show)

instance Eq EnvRef where
  left == right =
    envRefIdentity left == envRefIdentity right

instance Ord EnvRef where
  compare left right =
    compare (envRefIdentity left) (envRefIdentity right)

freshEnvRef :: String -> IdentityGenerator -> (EnvRef, IdentityGenerator)
freshEnvRef name generator =
  let (identity, generator') = freshIdentity generator
   in (EnvRef identity name, generator')

data PrimitiveRef = PrimitiveRef
  { primitiveRefSymbol :: SymbolIdentity
  }
  deriving (Show)

instance Eq PrimitiveRef where
  left == right =
    primitiveRefSymbol left == primitiveRefSymbol right

instance Ord PrimitiveRef where
  compare left right =
    compare (primitiveRefSymbol left) (primitiveRefSymbol right)

data DeferredRef = DeferredRef
  { deferredRefIdentity :: UniqueIdentity,
    deferredRefName :: String
  }
  deriving (Show)

instance Eq DeferredRef where
  left == right =
    deferredRefIdentity left == deferredRefIdentity right

instance Ord DeferredRef where
  compare left right =
    compare (deferredRefIdentity left) (deferredRefIdentity right)

freshDeferredRef :: String -> IdentityGenerator -> (DeferredRef, IdentityGenerator)
freshDeferredRef name generator =
  let (identity, generator') = freshIdentity generator
   in (DeferredRef identity name, generator')

renameDeferredRef :: String -> DeferredRef -> DeferredRef
renameDeferredRef name ref =
  ref {deferredRefName = name}

data ConstructorRef = ConstructorRef
  { constructorRefSymbol :: SymbolIdentity
  }
  deriving (Show)

instance Eq ConstructorRef where
  left == right =
    constructorRefSymbol left == constructorRefSymbol right

data IdDetails
  = LocalId LocalRef
  | EvidenceId LocalRef
  | EnvId EnvRef
  | TopLevelId SymbolIdentity
  | ConstructorId ConstructorRef
  | MethodId SymbolIdentity
  | PrimitiveId PrimitiveRef
  | DeferredId DeferredRef
  deriving (Eq, Show)

idDetailsReferenceName :: String -> IdDetails -> String
idDetailsReferenceName runtimeName details =
  case details of
    LocalId localRef -> localRefName localRef
    EvidenceId localRef -> localRefName localRef
    EnvId envRef -> envRefName envRef
    DeferredId ref -> deferredRefName ref
    _ -> runtimeName

idDetailsDisplayName :: String -> IdDetails -> String
idDetailsDisplayName _runtimeName details =
  case details of
    LocalId localRef -> localRefName localRef
    EvidenceId localRef -> localRefName localRef
    EnvId envRef -> envRefName envRef
    TopLevelId symbol -> symbolDefiningName symbol
    ConstructorId ref -> symbolDefiningName (constructorRefSymbol ref)
    MethodId symbol -> symbolDefiningName symbol
    PrimitiveId ref -> symbolDefiningName (primitiveRefSymbol ref)
    DeferredId ref -> deferredRefName ref

idDetailsConstructorRef :: IdDetails -> Maybe ConstructorRef
idDetailsConstructorRef details =
  case details of
    ConstructorId ref -> Just ref
    _ -> Nothing

idDetailsIsLocal :: IdDetails -> Bool
idDetailsIsLocal details =
  case details of
    LocalId {} -> True
    EvidenceId {} -> True
    _ -> False

idDetailsIsEvidence :: IdDetails -> Bool
idDetailsIsEvidence details =
  case details of
    EvidenceId {} -> True
    _ -> False

idDetailsRenameLocal :: String -> IdDetails -> IdDetails
idDetailsRenameLocal name details =
  case details of
    LocalId localRef -> LocalId (renameLocalRef name localRef)
    EvidenceId localRef -> EvidenceId (renameLocalRef name localRef)
    _ -> details

idDetailsSameIdentity :: IdDetails -> IdDetails -> Bool
idDetailsSameIdentity left right =
  case (left, right) of
    (LocalId leftRef, LocalId rightRef) -> leftRef == rightRef
    (EvidenceId leftRef, EvidenceId rightRef) -> leftRef == rightRef
    (LocalId leftRef, EvidenceId rightRef) -> leftRef == rightRef
    (EvidenceId leftRef, LocalId rightRef) -> leftRef == rightRef
    (EnvId leftRef, EnvId rightRef) -> leftRef == rightRef
    (TopLevelId leftSymbol, TopLevelId rightSymbol) -> leftSymbol == rightSymbol
    (ConstructorId leftRef, ConstructorId rightRef) ->
      leftRef == rightRef
    (MethodId leftSymbol, MethodId rightSymbol) -> leftSymbol == rightSymbol
    (PrimitiveId leftRef, PrimitiveId rightRef) -> leftRef == rightRef
    (DeferredId leftRef, DeferredId rightRef) -> leftRef == rightRef
    _ -> False

idDetailsGeneratedIdentities :: IdDetails -> [UniqueIdentity]
idDetailsGeneratedIdentities details =
  case details of
    LocalId LocalRef {localRefIdentity = GeneratedLocalId identity} -> [identity]
    EvidenceId LocalRef {localRefIdentity = GeneratedLocalId identity} -> [identity]
    EnvId EnvRef {envRefIdentity = identity} -> [identity]
    TopLevelId symbol -> symbolGeneratedIdentities symbol
    ConstructorId ref ->
      symbolGeneratedIdentities (constructorRefSymbol ref)
    MethodId symbol -> symbolGeneratedIdentities symbol
    PrimitiveId ref -> symbolGeneratedIdentities (primitiveRefSymbol ref)
    DeferredId DeferredRef {deferredRefIdentity = identity} -> [identity]

symbolGeneratedIdentities :: SymbolIdentity -> [UniqueIdentity]
symbolGeneratedIdentities symbol =
  [symbolUniqueIdentity symbol]
    ++ symbolOwnerGeneratedIdentities (symbolOwnerIdentity symbol)

symbolOwnerGeneratedIdentities :: Maybe SymbolOwnerIdentity -> [UniqueIdentity]
symbolOwnerGeneratedIdentities owner =
  case owner of
    Just (SymbolOwnerType symbol) -> symbolGeneratedIdentities symbol
    Just (SymbolOwnerClass symbol) -> symbolGeneratedIdentities symbol
    Nothing -> []
