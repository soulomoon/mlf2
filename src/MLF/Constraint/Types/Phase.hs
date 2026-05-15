{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module      : MLF.Constraint.Types.Phase
Description : Pipeline phase tracking at the type level
Copyright   : (c) 2024
License     : BSD-3-Clause

This module is the stable boundary for the 'Phase' kind used to index the
'Constraint' type by pipeline stage. The singleton boilerplate lives in the
dedicated owner module "MLF.Constraint.Types.Phase.Singletons", which is
re-exported here so callers can keep importing the top-level phase boundary.

The phase progression is:

> Raw → Normalized → Acyclic → Presolved → Solved

See "MLF.Constraint.Types.Graph" for the phase-indexed 'Constraint' type.
-}
module MLF.Constraint.Types.Phase (
    module MLF.Constraint.Types.Phase.Singletons,
    Next,
) where

import MLF.Constraint.Types.Phase.Singletons

-- | Type-level successor for pipeline phases.
type family Next (p :: Phase) :: Phase where
    Next 'Raw        = 'Normalized
    Next 'Normalized = 'Acyclic
    Next 'Acyclic    = 'Presolved
    Next 'Presolved  = 'Solved
    Next 'Solved     = 'Solved
