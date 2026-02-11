{- |
Module      : MLF.Constraint.Types.SynthesizedExpVar
Description : Encapsulated allocation/checks for synthesized wrapper ExpVarIds
Copyright   : (c) 2024
License     : BSD-3-Clause

Synthesized wrapper nodes reserve the negative `ExpVarId` space so
presolution can distinguish wrapper-introduced TyExp nodes from
frontend-originated TyExp nodes.
-}
module MLF.Constraint.Types.SynthesizedExpVar (
    SynthExpVarSupply,
    initSynthExpVarSupply,
    takeSynthExpVar,
    isSynthesizedExpVar,
) where

import MLF.Constraint.Types.Graph (Constraint (..), ExpVarId (..), TyNode (..), toListNode)

-- | Opaque allocator state for synthesized wrapper expansion variables.
newtype SynthExpVarSupply = SynthExpVarSupply Int
    deriving (Eq, Show)

-- | Initialize the supply from a constraint, preserving strict separation from
-- any existing negative IDs.
initSynthExpVarSupply :: Constraint -> SynthExpVarSupply
initSynthExpVarSupply c =
    let minSeen = foldl' minExpVar 0 (toListNode (cNodes c))
        start = if minSeen < 0 then minSeen - 1 else -1
    in SynthExpVarSupply start
  where
    minExpVar acc (_, TyExp { tnExpVar = ExpVarId s }) = min acc s
    minExpVar acc _ = acc

-- | Allocate one fresh synthesized-wrapper expansion variable.
takeSynthExpVar :: SynthExpVarSupply -> (ExpVarId, SynthExpVarSupply)
takeSynthExpVar (SynthExpVarSupply s) = (ExpVarId s, SynthExpVarSupply (s - 1))

-- | Predicate for the synthesized-wrapper reserved ID space.
isSynthesizedExpVar :: ExpVarId -> Bool
isSynthesizedExpVar (ExpVarId s) = s < 0
