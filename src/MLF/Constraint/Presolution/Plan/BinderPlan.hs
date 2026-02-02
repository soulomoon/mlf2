{- |
Module      : MLF.Constraint.Presolution.Plan.BinderPlan
Description : Plan which variables become quantified binders
Copyright   : (c) 2024
License     : BSD-3-Clause

This module determines which type variables should be generalized as explicit
binders in a polymorphic type scheme. It handles:

* Filtering variables by reachability and scope
* Detecting scheme roots and alias binders
* Ordering binders by their bound dependencies
* Computing binder names (α-conversion)

The binder selection logic is complex because MLF allows variables to have
bounds, and those bounds may contain other variables. We must order binders
topologically so that each binder's bound only refers to previously-introduced
binders.

This module acts as a facade over smaller binder-plan helpers:
* Build - Binder plan construction
* Order - Topological ordering of binders
* Util - Shared utilities for bound/root traversal
* Selection - Binder selection helpers
* Alias - Alias-binder detection helpers
* Predicate - Simple binder predicates
-}
module MLF.Constraint.Presolution.Plan.BinderPlan (
    GaBindParentsInfo(..),
    BinderPlanInput(..),
    BinderPlan(..),
    buildBinderPlan,
    -- * Binder helpers
    isTargetSchemeBinderFor,
    boundMentionsSelfAliasFor,
    -- * Binder ordering
    orderBinderCandidates,
    -- * Utility functions
    boundRootWith,
    firstSchemeRootAncestorWith,
    -- * Binder selection
    boundFlexChildrenUnder,
    bindableChildrenUnder,
    isQuantifiable,
    boundContainsForall,
    isScopeSchemeRoot,
    hasExplicitBoundFor,
    mkIsBindable,
    bindingScopeGen,
    bindersForGen,
    bindersForType,
    selectBinders,
    computeAliasBinders
) where

import MLF.Constraint.Presolution.Plan.BinderPlan.Alias
import MLF.Constraint.Presolution.Plan.BinderPlan.Build
import MLF.Constraint.Presolution.Plan.BinderPlan.Order (GaBindParentsInfo(..), orderBinderCandidates)
import MLF.Constraint.Presolution.Plan.BinderPlan.Predicate
import MLF.Constraint.Presolution.Plan.BinderPlan.Selection
import MLF.Constraint.Presolution.Plan.BinderPlan.Types (BinderPlan(..), BinderPlanInput(..))
import MLF.Constraint.Presolution.Plan.BinderPlan.Util
