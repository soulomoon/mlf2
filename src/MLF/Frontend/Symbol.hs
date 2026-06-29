module MLF.Frontend.Symbol
  ( SymbolNamespace (..),
    SymbolOwnerIdentity (..),
    SymbolIdentity,
    symbolUniqueIdentity,
    symbolNamespace,
    symbolDefiningModule,
    symbolDefiningName,
    symbolOwnerIdentity,
    symbolIdentityFromParts,
    symbolIdentityWithUnique,
    renameSymbolDefiningName,
    SymbolOrigin (..),
    SymbolSpelling (..),
    ResolvedSymbol,
    resolvedSymbolIdentity,
    resolvedSymbolSpelling,
    mapResolvedSymbolIdentity,
    ResolvedReferenceKind (..),
    ResolvedReference,
    resolvedReferenceKind,
    resolvedReferenceName,
    resolvedReferenceSymbol,
    mkResolvedSymbol,
    mkResolvedReference,
    sameSymbolIdentity,
    sameResolvedSymbol,
    symbolRefMatches,
    symbolIdentityStableName,
    unqualifiedSymbolName,
  )
where

import MLF.Types.Unique (UniqueIdentity, uniqueIdentityStableName)

data SymbolNamespace
  = SymbolValue
  | SymbolConstructor
  | SymbolType
  | SymbolClass
  | SymbolMethod
  | SymbolModule
  deriving (Eq, Ord, Show)

data SymbolOwnerIdentity
  = SymbolOwnerType SymbolIdentity
  | SymbolOwnerClass SymbolIdentity
  deriving (Eq, Ord, Show)

data SymbolIdentity
  = SymbolIdentity UniqueIdentity SymbolNamespace String String (Maybe SymbolOwnerIdentity)
  deriving (Show)

symbolUniqueIdentity :: SymbolIdentity -> UniqueIdentity
symbolUniqueIdentity (SymbolIdentity unique _ _ _ _) =
  unique

symbolNamespace :: SymbolIdentity -> SymbolNamespace
symbolNamespace (SymbolIdentity _ namespace _ _ _) =
  namespace

symbolDefiningModule :: SymbolIdentity -> String
symbolDefiningModule (SymbolIdentity _ _ moduleName _ _) =
  moduleName

symbolDefiningName :: SymbolIdentity -> String
symbolDefiningName (SymbolIdentity _ _ _ name _) =
  name

symbolOwnerIdentity :: SymbolIdentity -> Maybe SymbolOwnerIdentity
symbolOwnerIdentity (SymbolIdentity _ _ _ _ owner) =
  owner

symbolIdentityFromParts :: UniqueIdentity -> SymbolNamespace -> String -> String -> Maybe SymbolOwnerIdentity -> SymbolIdentity
symbolIdentityFromParts =
  SymbolIdentity

symbolIdentityWithUnique :: UniqueIdentity -> SymbolIdentity -> SymbolIdentity
symbolIdentityWithUnique unique identity =
  symbolIdentityFromParts unique (symbolNamespace identity) (symbolDefiningModule identity) (symbolDefiningName identity) (symbolOwnerIdentity identity)

renameSymbolDefiningName :: String -> SymbolIdentity -> SymbolIdentity
renameSymbolDefiningName name identity =
  symbolIdentityFromParts (symbolUniqueIdentity identity) (symbolNamespace identity) (symbolDefiningModule identity) name (symbolOwnerIdentity identity)

instance Eq SymbolIdentity where
  left == right =
    symbolUniqueIdentity left == symbolUniqueIdentity right

instance Ord SymbolIdentity where
  compare left right =
    compare (symbolUniqueIdentity left) (symbolUniqueIdentity right)

data SymbolOrigin
  = SymbolLocal String
  | SymbolUnqualifiedImport String
  | SymbolQualifiedImport String String
  | SymbolBuiltin
  deriving (Eq, Ord, Show)

data SymbolSpelling = SymbolSpelling
  { symbolSourceName :: String,
    symbolDisplayName :: String,
    symbolSpellingOrigin :: SymbolOrigin
  }
  deriving (Eq, Ord, Show)

data ResolvedSymbol = ResolvedSymbol
  { resolvedSymbolIdentity :: SymbolIdentity,
    resolvedSymbolSpelling :: SymbolSpelling
  }
  deriving (Show)

instance Eq ResolvedSymbol where
  left == right =
    sameResolvedSymbol left right

instance Ord ResolvedSymbol where
  compare left right =
    compare (resolvedSymbolIdentity left) (resolvedSymbolIdentity right)

data ResolvedReferenceKind
  = ResolvedValueReference
  | ResolvedConstructorReference
  | ResolvedTypeReference
  | ResolvedClassReference
  | ResolvedMethodReference
  | ResolvedModuleReference
  deriving (Eq, Ord, Show)

data ResolvedReference = ResolvedReference
  { resolvedReferenceKind :: ResolvedReferenceKind,
    resolvedReferenceName :: String,
    resolvedReferenceSymbol :: ResolvedSymbol
  }
  deriving (Show)

instance Eq ResolvedReference where
  left == right =
    resolvedReferenceKind left == resolvedReferenceKind right
      && resolvedReferenceSymbol left == resolvedReferenceSymbol right

instance Ord ResolvedReference where
  compare left right =
    compare
      (resolvedReferenceKind left, resolvedSymbolIdentity (resolvedReferenceSymbol left))
      (resolvedReferenceKind right, resolvedSymbolIdentity (resolvedReferenceSymbol right))

mkResolvedSymbol :: SymbolIdentity -> String -> String -> SymbolOrigin -> ResolvedSymbol
mkResolvedSymbol identity sourceName displayName origin =
  ResolvedSymbol
    { resolvedSymbolIdentity = identity,
      resolvedSymbolSpelling =
        SymbolSpelling
          { symbolSourceName = sourceName,
            symbolDisplayName = displayName,
            symbolSpellingOrigin = origin
          }
    }

mapResolvedSymbolIdentity :: (SymbolIdentity -> SymbolIdentity) -> ResolvedSymbol -> ResolvedSymbol
mapResolvedSymbolIdentity f symbol =
  symbol {resolvedSymbolIdentity = f (resolvedSymbolIdentity symbol)}

mkResolvedReference :: ResolvedReferenceKind -> String -> ResolvedSymbol -> ResolvedReference
mkResolvedReference kind name symbol =
  ResolvedReference
    { resolvedReferenceKind = kind,
      resolvedReferenceName = name,
      resolvedReferenceSymbol = symbol
    }

sameSymbolIdentity :: SymbolIdentity -> SymbolIdentity -> Bool
sameSymbolIdentity left right =
  symbolUniqueIdentity left == symbolUniqueIdentity right

sameResolvedSymbol :: ResolvedSymbol -> ResolvedSymbol -> Bool
sameResolvedSymbol left right =
  sameSymbolIdentity (resolvedSymbolIdentity left) (resolvedSymbolIdentity right)

symbolRefMatches :: Maybe SymbolIdentity -> String -> Maybe SymbolIdentity -> String -> Bool
symbolRefMatches (Just leftIdentity) _ (Just rightIdentity) _ =
  symbolUniqueIdentity leftIdentity == symbolUniqueIdentity rightIdentity
symbolRefMatches Nothing leftName Nothing rightName =
  leftName == rightName
symbolRefMatches _ _ _ _ =
  False

symbolIdentityStableName :: SymbolIdentity -> String
symbolIdentityStableName identity =
  uniqueIdentityStableName (symbolUniqueIdentity identity)

unqualifiedSymbolName :: String -> String
unqualifiedSymbolName =
  reverse . takeWhile (/= '.') . reverse
