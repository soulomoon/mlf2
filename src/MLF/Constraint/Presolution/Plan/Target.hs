{-# LANGUAGE RecordWildCards #-}

module MLF.Constraint.Presolution.Plan.Target (
    -- * Target plan (re-exported from TargetPlan submodule)
    TargetPlanInput(..),
    TargetPlan(..),
    buildTargetPlan,
    -- * Gamma plan (re-exported from GammaPlan submodule)
    GammaPlanInput(..),
    GammaPlan(..),
    buildGammaPlan,
    -- * Type root plan (re-exported from TypeRootPlan submodule)
    TypeRootPlanInput(..),
    TypeRootPlan(..),
    buildTypeRootPlan
) where

-- Re-export TargetPlan from submodule
import MLF.Constraint.Presolution.Plan.Target.TargetPlan (
    TargetPlanInput(..),
    TargetPlan(..),
    buildTargetPlan
  )

-- Re-export GammaPlan from submodule
import MLF.Constraint.Presolution.Plan.Target.GammaPlan (
    GammaPlanInput(..),
    GammaPlan(..),
    buildGammaPlan
  )

-- Re-export TypeRootPlan from submodule
import MLF.Constraint.Presolution.Plan.Target.TypeRootPlan (
    TypeRootPlanInput(..),
    TypeRootPlan(..),
    buildTypeRootPlan
  )
