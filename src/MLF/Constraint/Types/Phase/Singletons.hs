{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{- |
Module      : MLF.Constraint.Types.Phase.Singletons
Description : Dedicated owner for phase singleton generation
Copyright   : (c) 2024
License     : BSD-3-Clause

This module owns the singleton boilerplate for the pipeline 'Phase' kind.
Callers should normally import "MLF.Constraint.Types.Phase", which re-exports
the stable phase boundary together with the 'Next' transition family.
-}
module MLF.Constraint.Types.Phase.Singletons (
    Phase (..),
    SPhase (..),
) where

import Data.Singletons.TH (genSingletons)

-- | Pipeline phases for the constraint graph.
--
-- Each phase represents a distinct processing stage. The 'Constraint' type is
-- indexed by its phase, enforcing at compile time that fields are accessed
-- only when they exist.
data Phase = Raw | Normalized | Acyclic | Presolved | Solved
    deriving (Eq, Show)

genSingletons [''Phase]
