module MLF.Frontend.Symbol
  ( SymbolNamespace (..),
    SymbolOwnerIdentity (..),
    SymbolIdentity (..),
    SymbolOrigin (..),
    SymbolSpelling (..),
    ResolvedSymbol (..),
    ResolvedReferenceKind (..),
    ResolvedReference (..),
    mkResolvedSymbol,
    sameSymbolIdentity,
    sameResolvedSymbol,
    symbolIdentityStableName,
    unqualifiedSymbolName,
  )
where

import MLF.Types.Unique (UniqueIdentity (..))

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

data SymbolIdentity = SymbolIdentity
  { symbolUniqueIdentity :: UniqueIdentity,
    symbolNamespace :: SymbolNamespace,
    symbolDefiningModule :: String,
    symbolDefiningName :: String,
    symbolOwnerIdentity :: Maybe SymbolOwnerIdentity
  }
  deriving (Show)

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

sameSymbolIdentity :: SymbolIdentity -> SymbolIdentity -> Bool
sameSymbolIdentity left right =
  symbolUniqueIdentity left == symbolUniqueIdentity right

sameResolvedSymbol :: ResolvedSymbol -> ResolvedSymbol -> Bool
sameResolvedSymbol left right =
  sameSymbolIdentity (resolvedSymbolIdentity left) (resolvedSymbolIdentity right)

symbolIdentityStableName :: SymbolIdentity -> String
symbolIdentityStableName identity =
  "$identity#" ++ show (uniqueIdentityValue (symbolUniqueIdentity identity))

unqualifiedSymbolName :: String -> String
unqualifiedSymbolName =
  reverse . takeWhile (/= '.') . reverse
