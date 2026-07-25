module MLF.Types.Unique
  ( UniqueIdentity (..),
    uniqueIdentityStableName,
    IdentityGenerator,
    initialIdentityGenerator,
    identityGeneratorAfter,
    advanceIdentityGeneratorPast,
    advanceIdentityGeneratorPastMany,
    freshIdentity,
  )
where

import MLF.Types.Unique.Internal
    ( IdentityGenerator
    , UniqueIdentity (..)
    , advanceIdentityGeneratorPast
    , advanceIdentityGeneratorPastMany
    , freshIdentity
    , identityGeneratorAfter
    , initialIdentityGenerator
    , uniqueIdentityStableName
    )
