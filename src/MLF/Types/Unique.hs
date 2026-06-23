module MLF.Types.Unique
  ( UniqueIdentity (..),
    IdentityGenerator,
    initialIdentityGenerator,
    identityGeneratorFromNext,
    identityGeneratorAfter,
    freshIdentity,
  )
where

newtype UniqueIdentity = UniqueIdentity
  { uniqueIdentityValue :: Int
  }
  deriving (Eq, Ord, Show)

newtype IdentityGenerator = IdentityGenerator
  { nextUniqueIdentity :: Int
  }
  deriving (Eq, Show)

initialIdentityGenerator :: IdentityGenerator
initialIdentityGenerator = IdentityGenerator 0

identityGeneratorFromNext :: Int -> IdentityGenerator
identityGeneratorFromNext next =
  IdentityGenerator next

identityGeneratorAfter :: [UniqueIdentity] -> IdentityGenerator
identityGeneratorAfter identities =
  IdentityGenerator (foldr (max . uniqueIdentityValue) (-1) identities + 1)

freshIdentity :: IdentityGenerator -> (UniqueIdentity, IdentityGenerator)
freshIdentity (IdentityGenerator next) =
  (UniqueIdentity next, IdentityGenerator (next + 1))
