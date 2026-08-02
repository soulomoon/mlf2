module MLF.Types.Identity
  ( UniqueIdentity (..),
    uniqueIdentityStableName,
    StructuralTypeBinderRole (..),
    TypeBinderIdentity,
    typeBinderIdentityFromNode,
    typeBinderIdentityNode,
    typeBinderIdentityGeneratedUnique,
    typeBinderIdentityStructural,
    typeBinderIdentityKey,
    typeBinderIdentityStableName,
    typeBinderIdentityAliasNames,
    typeBinderIdentityAliasMap,
    lookupTypeBinderIdentityAlias,
    typeBinderIdentityFromUnique,
    typeBinderIdentityFromStructural,
    freshenTypeBinderIdentity,
    typeBinderGeneratedIdentities,
    LocalIdentity (..),
    localIdentityStableUnique,
    IdentityGenerator,
    initialIdentityGenerator,
    identityGeneratorAfter,
    advanceIdentityGeneratorPast,
    advanceIdentityGeneratorPastMany,
    freshIdentity,
    LocalRef,
    localRefFromIdentity,
    localRefIdentity,
    localRefName,
    localRefDiscard,
    freshLocalRef,
    freshenLocalRef,
    localRefFromNodeId,
    localRefFromScopedNodeId,
    localRefMatchesNodeId,
    localRefGeneratedIdentities,
    renameLocalRef,
    EnvRef,
    envRefFromIdentity,
    envRefIdentity,
    envRefName,
    freshEnvRef,
    PrimitiveRef,
    primitiveRefFromSymbol,
    primitiveRefSymbol,
    DeferredRef,
    deferredRefFromIdentity,
    deferredRefIdentity,
    deferredRefName,
    freshDeferredRef,
    renameDeferredRef,
    ConstructorRef,
    constructorRefFromSymbol,
    constructorRefSymbol,
    IdDetails (..),
    ResolvedTermIdentityKey (..),
    idDetailsIdentityKey,
    idDetailsStableName,
    idDetailsRuntimeName,
    idDetailsAliasNames,
    idDetailsAliasNamesWith,
    idDetailsAliasMap,
    idDetailsAliasMapWith,
    idDetailsReferenceName,
    idDetailsDisplayName,
    idDetailsConstructorRef,
    idDetailsLocalRef,
    idDetailsBindingSymbolIdentity,
    idDetailsSymbolIdentity,
    idDetailsIsLocal,
    idDetailsIsEvidence,
    idDetailsIsDiscard,
    idDetailsRenameLocal,
    idDetailsSameIdentity,
    idDetailsGeneratedIdentities,
    symbolGeneratedIdentities,
  )
where

import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (NodeId (..))
import MLF.Frontend.Symbol (SymbolIdentity, SymbolIdentityPayloadKey, SymbolOwnerIdentity (..), symbolDefiningModule, symbolDefiningName, symbolIdentityAliasNames, symbolIdentityPayloadKey, symbolIdentityStableName, symbolOwnerIdentity, symbolUniqueIdentity)
import MLF.Types.Unique

data StructuralTypeBinderRole
  = StructuralSelfBinder
  | StructuralResultBinder
  deriving (Eq, Ord, Show)

data TypeBinderIdentity
  = GraphTypeBinderIdentity NodeId
  | GeneratedTypeBinderIdentity UniqueIdentity
  | StructuralTypeBinderIdentity UniqueIdentity StructuralTypeBinderRole
  | FreshenedStructuralTypeBinderIdentity
      UniqueIdentity
      StructuralTypeBinderRole
      UniqueIdentity
  deriving (Eq, Ord, Show)

typeBinderIdentityFromNode :: NodeId -> TypeBinderIdentity
typeBinderIdentityFromNode = GraphTypeBinderIdentity

typeBinderIdentityNode :: TypeBinderIdentity -> Maybe NodeId
typeBinderIdentityNode identity =
  case identity of
    GraphTypeBinderIdentity node -> Just node
    GeneratedTypeBinderIdentity {} -> Nothing
    StructuralTypeBinderIdentity {} -> Nothing
    FreshenedStructuralTypeBinderIdentity {} -> Nothing

typeBinderIdentityGeneratedUnique :: TypeBinderIdentity -> Maybe UniqueIdentity
typeBinderIdentityGeneratedUnique identity =
  case identity of
    GeneratedTypeBinderIdentity unique -> Just unique
    FreshenedStructuralTypeBinderIdentity _ _ freshUnique -> Just freshUnique
    GraphTypeBinderIdentity {} -> Nothing
    StructuralTypeBinderIdentity {} -> Nothing

typeBinderIdentityFromUnique :: UniqueIdentity -> TypeBinderIdentity
typeBinderIdentityFromUnique = GeneratedTypeBinderIdentity

typeBinderIdentityFromStructural :: UniqueIdentity -> StructuralTypeBinderRole -> TypeBinderIdentity
typeBinderIdentityFromStructural =
  StructuralTypeBinderIdentity

-- | Allocate a distinct alpha-binder identity without discarding structural
-- owner provenance.  Ordinary binders remain generated identities; a
-- structural binder retains its nominal owner and role alongside the fresh
-- occurrence identity.
freshenTypeBinderIdentity :: TypeBinderIdentity -> UniqueIdentity -> TypeBinderIdentity
freshenTypeBinderIdentity identity freshUnique =
  case typeBinderIdentityStructural identity of
    Just (ownerUnique, role) ->
      FreshenedStructuralTypeBinderIdentity ownerUnique role freshUnique
    Nothing ->
      GeneratedTypeBinderIdentity freshUnique

typeBinderIdentityStructural :: TypeBinderIdentity -> Maybe (UniqueIdentity, StructuralTypeBinderRole)
typeBinderIdentityStructural identity =
  case identity of
    StructuralTypeBinderIdentity unique role -> Just (unique, role)
    FreshenedStructuralTypeBinderIdentity unique role _ -> Just (unique, role)
    GraphTypeBinderIdentity {} -> Nothing
    GeneratedTypeBinderIdentity {} -> Nothing

typeBinderIdentityKey :: TypeBinderIdentity -> Int
typeBinderIdentityKey identity =
  case identity of
    GraphTypeBinderIdentity node -> getNodeId node
    GeneratedTypeBinderIdentity unique -> negate (uniqueIdentityValue unique + 1)
    StructuralTypeBinderIdentity unique role ->
      negate (uniqueIdentityValue unique * 2 + structuralRoleKey role + 1000000)
    FreshenedStructuralTypeBinderIdentity _ _ freshUnique ->
      negate (uniqueIdentityValue freshUnique + 1)

typeBinderIdentityStableName :: TypeBinderIdentity -> String
typeBinderIdentityStableName identity =
  case identity of
    GraphTypeBinderIdentity node -> "$typevar#node#" ++ show (getNodeId node)
    GeneratedTypeBinderIdentity unique -> "$typevar#" ++ show (uniqueIdentityValue unique)
    StructuralTypeBinderIdentity unique role ->
      "$typevar#structural#" ++ show (uniqueIdentityValue unique) ++ "#" ++ structuralRoleName role
    FreshenedStructuralTypeBinderIdentity unique role freshUnique ->
      "$typevar#structural#"
        ++ show (uniqueIdentityValue unique)
        ++ "#"
        ++ structuralRoleName role
        ++ "#fresh#"
        ++ show (uniqueIdentityValue freshUnique)

typeBinderIdentityAliasNames :: String -> TypeBinderIdentity -> [String]
typeBinderIdentityAliasNames name identity =
  filter (not . null) [name, typeBinderIdentityStableName identity]

typeBinderIdentityAliasMap :: [(String, TypeBinderIdentity)] -> Map String TypeBinderIdentity
typeBinderIdentityAliasMap binders =
  Map.fromList
    [ (alias, identity)
    | (alias, identitiesForAlias) <- Map.toList identitiesByAlias,
      [identity] <- [Set.toList identitiesForAlias]
    ]
  where
    identitiesByAlias =
      Map.fromListWith
        Set.union
        [ (alias, Set.singleton identity)
        | (name, identity) <- binders,
          alias <- typeBinderIdentityAliasNames name identity
        ]

lookupTypeBinderIdentityAlias :: Map String TypeBinderIdentity -> String -> Maybe TypeBinderIdentity
lookupTypeBinderIdentityAlias identities name =
  case Map.lookup name identities of
    Just identity -> Just identity
    Nothing -> Map.lookup name (typeBinderIdentityAliasMap (Map.toList identities))

typeBinderGeneratedIdentities :: TypeBinderIdentity -> [UniqueIdentity]
typeBinderGeneratedIdentities identity =
  case identity of
    GeneratedTypeBinderIdentity unique -> [unique]
    GraphTypeBinderIdentity {} -> []
    StructuralTypeBinderIdentity unique _ -> [unique]
    FreshenedStructuralTypeBinderIdentity unique _ freshUnique ->
      [unique, freshUnique]

structuralRoleKey :: StructuralTypeBinderRole -> Int
structuralRoleKey =
  \case
    StructuralSelfBinder -> 0
    StructuralResultBinder -> 1

structuralRoleName :: StructuralTypeBinderRole -> String
structuralRoleName =
  \case
    StructuralSelfBinder -> "self"
    StructuralResultBinder -> "result"

data LocalRef = LocalRef
  { localRefIdentity :: LocalIdentity,
    localRefName :: String,
    localRefDiscard :: Bool
  }
  deriving (Show)

data LocalIdentity
  = GraphLocalId NodeId
  | ScopedGraphLocalId NodeId Int
  | GeneratedGraphLocalId UniqueIdentity NodeId
  | GeneratedLocalId UniqueIdentity
  deriving (Eq, Ord, Show)

localIdentityStableUnique :: LocalIdentity -> UniqueIdentity
localIdentityStableUnique identity =
  case identity of
    GraphLocalId nodeId -> UniqueIdentity (graphLocalIdentityBase - getNodeId nodeId)
    ScopedGraphLocalId _ binderOrdinal -> UniqueIdentity (scopedGraphLocalIdentityBase - binderOrdinal)
    GeneratedGraphLocalId unique _ -> unique
    GeneratedLocalId unique -> unique

graphLocalIdentityBase :: Int
graphLocalIdentityBase = -400000

scopedGraphLocalIdentityBase :: Int
scopedGraphLocalIdentityBase = -800000

instance Eq LocalRef where
  left == right =
    localRefIdentity left == localRefIdentity right

instance Ord LocalRef where
  compare left right =
    compare (localRefIdentity left) (localRefIdentity right)

freshLocalRef :: String -> IdentityGenerator -> (LocalRef, IdentityGenerator)
freshLocalRef name generator =
  let (identity, generator') = freshIdentity generator
   in (localRefFromIdentity (GeneratedLocalId identity) name, generator')

freshenLocalRef :: String -> IdentityGenerator -> LocalRef -> (LocalRef, IdentityGenerator)
freshenLocalRef name generator ref =
  let (identity, generator') = freshIdentity generator
      freshIdentityForOrigin =
        case localRefIdentity ref of
          GraphLocalId nodeId -> GeneratedGraphLocalId identity nodeId
          ScopedGraphLocalId nodeId _ -> GeneratedGraphLocalId identity nodeId
          GeneratedGraphLocalId _ nodeId -> GeneratedGraphLocalId identity nodeId
          GeneratedLocalId {} -> GeneratedLocalId identity
   in (LocalRef freshIdentityForOrigin name (localRefDiscard ref), generator')

localRefFromIdentity :: LocalIdentity -> String -> LocalRef
localRefFromIdentity identity name =
  LocalRef identity name (name == "_")

localRefFromNodeId :: String -> NodeId -> LocalRef
localRefFromNodeId name nodeId =
  localRefFromIdentity (GraphLocalId nodeId) name

-- | A lexical binder backed by a graph type node.  The ordinal distinguishes
-- binders whose inferred types legitimately share that node (for example,
-- @let k = h@) while retaining the graph-node association.
localRefFromScopedNodeId :: String -> NodeId -> Int -> LocalRef
localRefFromScopedNodeId name nodeId binderOrdinal =
  localRefFromIdentity (ScopedGraphLocalId nodeId binderOrdinal) name

localRefMatchesNodeId :: LocalRef -> NodeId -> Bool
localRefMatchesNodeId ref nodeId =
  case localRefIdentity ref of
    GraphLocalId refNodeId -> refNodeId == nodeId
    ScopedGraphLocalId refNodeId _ -> refNodeId == nodeId
    GeneratedGraphLocalId _ refNodeId -> refNodeId == nodeId
    GeneratedLocalId {} -> False

localRefGeneratedIdentities :: LocalRef -> [UniqueIdentity]
localRefGeneratedIdentities ref =
  case localRefIdentity ref of
    GraphLocalId {} -> []
    ScopedGraphLocalId {} -> []
    GeneratedGraphLocalId identity _ -> [identity]
    GeneratedLocalId identity -> [identity]

renameLocalRef :: String -> LocalRef -> LocalRef
renameLocalRef name ref =
  ref {localRefName = name}

data EnvRef = EnvRef
  { envRefIdentity :: UniqueIdentity,
    envRefName :: String
  }
  deriving (Show)

envRefFromIdentity :: UniqueIdentity -> String -> EnvRef
envRefFromIdentity identity name =
  EnvRef
    { envRefIdentity = identity,
      envRefName = name
    }

instance Eq EnvRef where
  left == right =
    envRefIdentity left == envRefIdentity right

instance Ord EnvRef where
  compare left right =
    compare (envRefIdentity left) (envRefIdentity right)

freshEnvRef :: String -> IdentityGenerator -> (EnvRef, IdentityGenerator)
freshEnvRef name generator =
  let (identity, generator') = freshIdentity generator
   in (envRefFromIdentity identity name, generator')

data PrimitiveRef = PrimitiveRef
  { primitiveRefSymbol :: SymbolIdentity
  }
  deriving (Show)

primitiveRefFromSymbol :: SymbolIdentity -> PrimitiveRef
primitiveRefFromSymbol symbol =
  PrimitiveRef {primitiveRefSymbol = symbol}

instance Eq PrimitiveRef where
  left == right =
    symbolIdentityPayloadKey (primitiveRefSymbol left)
      == symbolIdentityPayloadKey (primitiveRefSymbol right)

instance Ord PrimitiveRef where
  compare left right =
    compare
      (symbolIdentityPayloadKey (primitiveRefSymbol left))
      (symbolIdentityPayloadKey (primitiveRefSymbol right))

data DeferredRef = DeferredRef
  { deferredRefIdentity :: UniqueIdentity,
    deferredRefName :: String
  }
  deriving (Show)

deferredRefFromIdentity :: UniqueIdentity -> String -> DeferredRef
deferredRefFromIdentity identity name =
  DeferredRef
    { deferredRefIdentity = identity,
      deferredRefName = name
    }

instance Eq DeferredRef where
  left == right =
    deferredRefIdentity left == deferredRefIdentity right

instance Ord DeferredRef where
  compare left right =
    compare (deferredRefIdentity left) (deferredRefIdentity right)

freshDeferredRef :: String -> IdentityGenerator -> (DeferredRef, IdentityGenerator)
freshDeferredRef name generator =
  let (identity, generator') = freshIdentity generator
   in (deferredRefFromIdentity identity name, generator')

renameDeferredRef :: String -> DeferredRef -> DeferredRef
renameDeferredRef name ref =
  ref {deferredRefName = name}

data ConstructorRef = ConstructorRef
  { constructorRefSymbol :: SymbolIdentity
  }
  deriving (Show)

constructorRefFromSymbol :: SymbolIdentity -> ConstructorRef
constructorRefFromSymbol symbol =
  ConstructorRef {constructorRefSymbol = symbol}

instance Eq ConstructorRef where
  left == right =
    symbolIdentityPayloadKey (constructorRefSymbol left)
      == symbolIdentityPayloadKey (constructorRefSymbol right)

data IdDetails
  = LocalId LocalRef
  | EvidenceId LocalRef
  | EnvId EnvRef
  | TopLevelId SymbolIdentity
  | ConstructorId ConstructorRef
  | MethodId SymbolIdentity
  | PrimitiveId PrimitiveRef
  | DeferredId DeferredRef
  deriving (Show)

instance Eq IdDetails where
  left == right =
    idDetailsSameIdentity left right

data ResolvedTermIdentityKey
  = ResolvedTermLocalKey LocalRef
  | ResolvedTermEnvKey EnvRef
  | ResolvedTermTopLevelKey SymbolIdentityPayloadKey
  | ResolvedTermConstructorKey SymbolIdentityPayloadKey
  | ResolvedTermMethodKey SymbolIdentityPayloadKey
  | ResolvedTermPrimitiveKey SymbolIdentityPayloadKey
  | ResolvedTermDeferredKey DeferredRef
  deriving (Eq, Ord, Show)

idDetailsIdentityKey :: IdDetails -> ResolvedTermIdentityKey
idDetailsIdentityKey details =
  case details of
    LocalId ref -> ResolvedTermLocalKey ref
    EvidenceId ref -> ResolvedTermLocalKey ref
    EnvId ref -> ResolvedTermEnvKey ref
    TopLevelId symbol -> ResolvedTermTopLevelKey (symbolIdentityPayloadKey symbol)
    ConstructorId ref -> ResolvedTermConstructorKey (symbolIdentityPayloadKey (constructorRefSymbol ref))
    MethodId symbol -> ResolvedTermMethodKey (symbolIdentityPayloadKey symbol)
    PrimitiveId ref -> ResolvedTermPrimitiveKey (symbolIdentityPayloadKey (primitiveRefSymbol ref))
    DeferredId ref -> ResolvedTermDeferredKey ref

idDetailsAliasPayloadKey :: IdDetails -> ResolvedTermIdentityKey
idDetailsAliasPayloadKey =
  idDetailsIdentityKey

idDetailsStableName :: IdDetails -> String
idDetailsStableName details =
  case details of
    LocalId ref -> localRefStableName ref
    EvidenceId ref -> localRefStableName ref
    EnvId ref -> uniqueIdentityStableName (envRefIdentity ref)
    TopLevelId symbol -> symbolIdentityStableName symbol
    ConstructorId ref -> symbolIdentityStableName (constructorRefSymbol ref)
    MethodId symbol -> symbolIdentityStableName symbol
    PrimitiveId ref -> symbolIdentityStableName (primitiveRefSymbol ref)
    DeferredId ref -> uniqueIdentityStableName (deferredRefIdentity ref)

idDetailsRuntimeName :: IdDetails -> String
idDetailsRuntimeName details =
  case details of
    LocalId localRef -> localRefName localRef
    EvidenceId localRef -> localRefName localRef
    EnvId envRef -> envRefName envRef
    TopLevelId symbol -> symbolRuntimeName symbol
    ConstructorId ref -> symbolRuntimeName (constructorRefSymbol ref)
    MethodId symbol -> symbolDefiningName symbol
    PrimitiveId ref -> symbolDefiningName (primitiveRefSymbol ref)
    DeferredId ref -> deferredRefName ref

symbolRuntimeName :: SymbolIdentity -> String
symbolRuntimeName symbol =
  case symbolDefiningModule symbol of
    "" -> name
    "<builtin>" -> name
    "<local>" -> name
    moduleName
      | (moduleName ++ "__") `isPrefixOf` name -> name
      | otherwise -> moduleName ++ "__" ++ name
  where
    name = symbolDefiningName symbol

idDetailsAliasNames :: IdDetails -> [String]
idDetailsAliasNames details =
  idDetailsAliasNamesWith (idDetailsRuntimeName details) details

idDetailsAliasNamesWith :: String -> IdDetails -> [String]
idDetailsAliasNamesWith runtimeName details =
  Set.toList $
    Set.fromList
      ( [ runtimeName,
          idDetailsReferenceName details,
          idDetailsDisplayName details,
          idDetailsStableName details
        ]
          ++ idDetailsSymbolAliasNames details
      )

idDetailsSymbolAliasNames :: IdDetails -> [String]
idDetailsSymbolAliasNames details =
  case details of
    TopLevelId symbol -> symbolIdentityAliasNames symbol
    ConstructorId ref -> symbolIdentityAliasNames (constructorRefSymbol ref)
    MethodId symbol -> symbolIdentityAliasNames symbol
    PrimitiveId ref -> symbolIdentityAliasNames (primitiveRefSymbol ref)
    _ -> []

idDetailsAliasMap :: [IdDetails] -> Map String IdDetails
idDetailsAliasMap identities =
  idDetailsAliasMapWith [(idDetailsRuntimeName details, details) | details <- identities]

idDetailsAliasMapWith :: [(String, IdDetails)] -> Map String IdDetails
idDetailsAliasMapWith identities =
  Map.fromList
    [ (alias, details)
    | (alias, detailsForAlias) <- Map.toList identitiesByAlias,
      [details] <- [Map.elems detailsForAlias]
    ]
  where
    identitiesByAlias =
      Map.fromListWith
        Map.union
        [ (alias, Map.singleton (idDetailsAliasPayloadKey details) details)
        | (name, details) <- identities,
          alias <- idDetailsAliasNamesWith name details
        ]

localRefStableName :: LocalRef -> String
localRefStableName ref =
  uniqueIdentityStableName (localIdentityStableUnique (localRefIdentity ref))

idDetailsReferenceName :: IdDetails -> String
idDetailsReferenceName details =
  case details of
    LocalId localRef -> localRefName localRef
    EvidenceId localRef -> localRefName localRef
    EnvId envRef -> envRefName envRef
    DeferredId ref -> deferredRefName ref
    _ -> idDetailsRuntimeName details

idDetailsDisplayName :: IdDetails -> String
idDetailsDisplayName details =
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

idDetailsLocalRef :: IdDetails -> Maybe LocalRef
idDetailsLocalRef details =
  case details of
    LocalId ref -> Just ref
    EvidenceId ref -> Just ref
    _ -> Nothing

idDetailsBindingSymbolIdentity :: IdDetails -> Maybe SymbolIdentity
idDetailsBindingSymbolIdentity details =
  case details of
    TopLevelId symbol -> Just symbol
    MethodId symbol -> Just symbol
    _ -> Nothing

idDetailsSymbolIdentity :: IdDetails -> Maybe SymbolIdentity
idDetailsSymbolIdentity details =
  case details of
    TopLevelId symbol -> Just symbol
    ConstructorId ref -> Just (constructorRefSymbol ref)
    MethodId symbol -> Just symbol
    PrimitiveId ref -> Just (primitiveRefSymbol ref)
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

idDetailsIsDiscard :: IdDetails -> Bool
idDetailsIsDiscard details =
  case details of
    LocalId localRef -> localRefDiscard localRef
    EvidenceId localRef -> localRefDiscard localRef
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
    (TopLevelId leftSymbol, TopLevelId rightSymbol) ->
      symbolIdentityPayloadKey leftSymbol == symbolIdentityPayloadKey rightSymbol
    (ConstructorId leftRef, ConstructorId rightRef) ->
      symbolIdentityPayloadKey (constructorRefSymbol leftRef) == symbolIdentityPayloadKey (constructorRefSymbol rightRef)
    (MethodId leftSymbol, MethodId rightSymbol) ->
      symbolIdentityPayloadKey leftSymbol == symbolIdentityPayloadKey rightSymbol
    (PrimitiveId leftRef, PrimitiveId rightRef) ->
      symbolIdentityPayloadKey (primitiveRefSymbol leftRef) == symbolIdentityPayloadKey (primitiveRefSymbol rightRef)
    (DeferredId leftRef, DeferredId rightRef) -> leftRef == rightRef
    _ -> False

idDetailsGeneratedIdentities :: IdDetails -> [UniqueIdentity]
idDetailsGeneratedIdentities details =
  case details of
    LocalId ref -> localRefGeneratedIdentities ref
    EvidenceId ref -> localRefGeneratedIdentities ref
    EnvId ref -> [envRefIdentity ref]
    TopLevelId symbol -> symbolGeneratedIdentities symbol
    ConstructorId ref ->
      symbolGeneratedIdentities (constructorRefSymbol ref)
    MethodId symbol -> symbolGeneratedIdentities symbol
    PrimitiveId ref -> symbolGeneratedIdentities (primitiveRefSymbol ref)
    DeferredId ref -> [deferredRefIdentity ref]

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
