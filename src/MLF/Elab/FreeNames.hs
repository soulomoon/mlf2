module MLF.Elab.FreeNames (
    freeNamesOf,
    freeNamesFrom
) where

import Data.Functor.Foldable (cata)
import qualified Data.Set as Set

import MLF.Elab.Types

freeNamesOf :: ElabType -> Set.Set String
freeNamesOf = freeNamesFrom Set.empty

freeNamesFrom :: Set.Set String -> ElabType -> Set.Set String
freeNamesFrom bound0 ty = (cata alg ty) bound0
  where
    alg node = case node of
        TVarF v ->
            \boundSet ->
                if Set.member v boundSet then Set.empty else Set.singleton v
        TArrowF d c -> \boundSet -> Set.union (d boundSet) (c boundSet)
        TBaseF _ -> const Set.empty
        TBottomF -> const Set.empty
        TForallF v mb t' ->
            \boundSet ->
                let bound' = Set.insert v boundSet
                    freeBound = maybe Set.empty ($ bound') mb
                    freeBody = t' bound'
                in Set.union freeBound freeBody
