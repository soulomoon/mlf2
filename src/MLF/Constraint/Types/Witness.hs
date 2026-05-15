{- |
Module      : MLF.Constraint.Types.Witness
Description : Narrowed production witness surface
Copyright   : (c) 2024
License     : BSD-3-Clause

This module defines the default production witness boundary. Raw witness
constructors stay behind "MLF.Constraint.Types.Witness.TestSupport" so normal
callers use the validating smart-constructor seam.
-}
module MLF.Constraint.Types.Witness (
    NodeId(..),
    EdgeId(..),
    BoundRef(..),
    ForallSpec(..),
    forallSpecBinderCount,
    mkForallSpec,
    Expansion(..),
    ExpansionF(..),
    InstanceOp(..),
    InstanceWitness,
    getInstanceOps,
    mkInstanceWitness,
    EdgeWitness,
    ewEdgeId,
    ewLeft,
    ewRight,
    ewRoot,
    ewForallIntros,
    ewWitness,
    mkEdgeWitness,
    WitnessError(..),
    ReplayContract(..),
    classifyReplayContract,
    isStrictReplayContract
) where

import MLF.Constraint.Types.Witness.Internal
