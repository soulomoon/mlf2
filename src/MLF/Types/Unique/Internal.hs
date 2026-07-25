module MLF.Types.Unique.Internal
  ( UniqueIdentity (..),
    uniqueIdentityStableName,
    IdentityGenerator,
    initialIdentityGenerator,
    identityGeneratorAfter,
    descendingIdentityGeneratorFrom,
    advanceIdentityGeneratorPast,
    advanceIdentityGeneratorPastMany,
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

data IdentityGenerator
  = AscendingIdentityGenerator Int
  | DescendingIdentityGenerator Int
  deriving (Eq, Show)

initialIdentityGenerator :: IdentityGenerator
initialIdentityGenerator = AscendingIdentityGenerator 0

identityGeneratorAfter :: [UniqueIdentity] -> IdentityGenerator
identityGeneratorAfter identities =
  AscendingIdentityGenerator (nextIdentityAfter identities)

nextIdentityAfter :: [UniqueIdentity] -> Int
nextIdentityAfter identities =
  foldr (max . uniqueIdentityValue) (-1) identities + 1

-- | Internal owner hook for a downward identity supply.  The public
-- 'MLF.Types.Unique' facade deliberately does not expose this constructor.
descendingIdentityGeneratorFrom :: Int -> IdentityGenerator
descendingIdentityGeneratorFrom = DescendingIdentityGenerator

advanceIdentityGeneratorPast :: UniqueIdentity -> IdentityGenerator -> IdentityGenerator
advanceIdentityGeneratorPast (UniqueIdentity used) generator =
  case generator of
    AscendingIdentityGenerator next ->
      AscendingIdentityGenerator (max next (used + 1))
    DescendingIdentityGenerator next ->
      DescendingIdentityGenerator (min next (used - 1))

advanceIdentityGeneratorPastMany :: [UniqueIdentity] -> IdentityGenerator -> IdentityGenerator
advanceIdentityGeneratorPastMany identities generator =
  foldr advanceIdentityGeneratorPast generator identities

freshIdentity :: IdentityGenerator -> (UniqueIdentity, IdentityGenerator)
freshIdentity generator =
  case generator of
    AscendingIdentityGenerator next ->
      (UniqueIdentity next, AscendingIdentityGenerator (next + 1))
    DescendingIdentityGenerator next ->
      (UniqueIdentity next, DescendingIdentityGenerator (next - 1))
