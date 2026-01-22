module MLF.Elab.FreeNames (
    freeNamesOf,
    freeNamesFrom
) where

import qualified Data.Set as Set

import MLF.Elab.TypeOps (freeTypeVarsFrom)
import MLF.Elab.Types (ElabType)

freeNamesOf :: ElabType -> Set.Set String
freeNamesOf = freeTypeVarsFrom Set.empty

freeNamesFrom :: Set.Set String -> ElabType -> Set.Set String
freeNamesFrom = freeTypeVarsFrom
