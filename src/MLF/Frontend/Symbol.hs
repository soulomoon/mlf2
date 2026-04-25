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
    sameResolvedSymbol,
    unqualifiedSymbolName,
  )
where

data SymbolNamespace
  = SymbolValue
  | SymbolConstructor
  | SymbolType
  | SymbolClass
  | SymbolMethod
  | SymbolModule
  deriving (Eq, Ord, Show)

data SymbolOwnerIdentity
  = SymbolOwnerType String String
  | SymbolOwnerClass String String
  deriving (Eq, Ord, Show)

data SymbolIdentity = SymbolIdentity
  { symbolNamespace :: SymbolNamespace,
    symbolDefiningModule :: String,
    symbolDefiningName :: String,
    symbolOwnerIdentity :: Maybe SymbolOwnerIdentity
  }
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

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

sameResolvedSymbol :: ResolvedSymbol -> ResolvedSymbol -> Bool
sameResolvedSymbol left right =
  resolvedSymbolIdentity left == resolvedSymbolIdentity right

unqualifiedSymbolName :: String -> String
unqualifiedSymbolName =
  reverse . takeWhile (/= '.') . reverse
