module MLF.Types.Unique
  ( UniqueIdentity (..),
    uniqueIdentityStableName,
    IdentityGenerator,
    initialIdentityGenerator,
    identityGeneratorAfter,
    advanceIdentityGeneratorPast,
    freshIdentity,
  )
where

newtype UniqueIdentity = UniqueIdentity
  { uniqueIdentityValue :: Int
  }
  deriving (Eq, Ord, Show)

uniqueIdentityStableName :: UniqueIdentity -> String
uniqueIdentityStableName identity =
  "$identity#" ++ show (uniqueIdentityValue identity)

newtype IdentityGenerator = IdentityGenerator
  { nextUniqueIdentity :: Int
  }
  deriving (Eq, Show)

initialIdentityGenerator :: IdentityGenerator
initialIdentityGenerator = IdentityGenerator 0

identityGeneratorAfter :: [UniqueIdentity] -> IdentityGenerator
identityGeneratorAfter identities =
  IdentityGenerator (foldr (max . uniqueIdentityValue) (-1) identities + 1)

advanceIdentityGeneratorPast :: UniqueIdentity -> IdentityGenerator -> IdentityGenerator
advanceIdentityGeneratorPast (UniqueIdentity used) (IdentityGenerator next) =
  IdentityGenerator (max next (used + 1))

freshIdentity :: IdentityGenerator -> (UniqueIdentity, IdentityGenerator)
freshIdentity (IdentityGenerator next) =
  (UniqueIdentity next, IdentityGenerator (next + 1))
