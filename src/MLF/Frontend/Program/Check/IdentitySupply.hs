module MLF.Frontend.Program.Check.IdentitySupply
    ( builtinPreludeCheckIdentityGenerator
    )
where

import MLF.Types.Unique.Internal
    ( IdentityGenerator
    , descendingIdentityGeneratorFrom
    )

-- | The builtin Prelude checker's owner-private identity supply.  Source
-- resolution and client checking allocate upward from non-negative identities;
-- the Prelude allocates downward in a disjoint part of the negative range.
builtinPreludeCheckIdentityGenerator :: IdentityGenerator
builtinPreludeCheckIdentityGenerator =
    descendingIdentityGeneratorFrom (minBound `div` 2)
