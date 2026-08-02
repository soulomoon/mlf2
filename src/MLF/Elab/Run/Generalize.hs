{-# LANGUAGE DataKinds #-}
module MLF.Elab.Run.Generalize (
    GeneralizeAtView,
    CertifiedGeneralizeAtView,
    pruneBindParentsConstraint,
    instantiationCopyNodes,
    constraintForGeneralization,
    mkGeneralizeAtWithBuilder,
    generalizeAtWithBuilder,
    generalizeAtWithBuilderRequired,
    generalizeAtWithBuilderRequiredCertified,
    generalizeAtWithBuilderRequiredResultCertified
) where

import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Presolution
    ( PresolutionPlanBuilder(..)
    , PresolutionView(..)
    )
import MLF.Constraint.Presolution.Plan (ReifyPlan(..))
import qualified MLF.Constraint.Presolution.Plan.ReifyPlan as Reify
import MLF.Constraint.Types.Graph
    ( Constraint
    , NodeId(..)
    , NodeRef(..)
    , typeRef
    )
import MLF.Elab.Generalize
    ( GaBindParents(..)
    , GeneralizedResultRoute
    , GeneralizedResultRouteRequest
    , applyGeneralizePlan
    , certifyGeneralizedResultRoute
    )
import MLF.Constraint.Presolution.Plan.Context
    ( GeneralizationRequirements
    , emptyGeneralizationRequirements
    )
import MLF.Elab.Run.Generalize.Constraint
    ( instantiationCopyNodes
    , pruneBindParentsConstraint
    )
import MLF.Elab.Run.Generalize.Finalize (finalizeConstraint)
import MLF.Elab.Run.Generalize.Phase1 (restoreSchemeNodes)
import MLF.Elab.Run.Generalize.Phase2 (buildNodeMappings)
import MLF.Elab.Run.Generalize.Phase3 (computeBindParentsBase)
import MLF.Elab.Run.Generalize.Phase4 (computeSchemeOwnership)
import MLF.Elab.Run.Generalize.Types
    ( ExpansionConstructionPlacements
    , GeneralizeEnv(..)
    , NodeKeySet
    )
import MLF.Elab.Run.Util (chaseRedirects)
import MLF.Util.Trace (TraceConfig)
import MLF.Frontend.ConstraintGen (AnnExpr)
import MLF.Elab.Types
    ( ElabScheme
    , TypeBinderRef
    , typeBinderRefsSameIdentity
    )
import MLF.Util.ElabError (ElabError(..))

type GeneralizeAtView p =
    Maybe (GaBindParents p)
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)

type CertifiedGeneralizeAtView p =
    Maybe (GaBindParents p)
    -> NodeRef
    -> NodeId
    -> Either
        ElabError
        ( ElabScheme
        , IntMap.IntMap TypeBinderRef
        , Reify.InheritedGammaRoutes
        )

{- Note [binding-parent projection — ga′ invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The generalization pipeline (Phases 2–3) and `resolveContext` must preserve
the thesis ga′ (Def. 15.3.2) through the base↔solved mapping.

Phase 2 (`buildNodeMappings`, Generalize/Phase2.hs):
  `chooseMapping` prefers named base nodes that still exist in solved, falls
  back to `adoptRef` (redirect+UF).  The `solvedToBase` map inserts both
  canonical and raw keys (lines 110-113) using `keepOld` — first-inserted
  wins.  Risk: if a redirect merges two base nodes with different gen
  ancestors, the first-inserted mapping wins.  In practice this is safe
  because redirects only merge structurally equivalent nodes (TyExp
  elimination), which share the same gen ancestor.

Phase 3 (`computeBindParentsBase`, Generalize/Phase3.hs):
  Merges base binding parents (via `insertBindParentBase` with KeepOld) then
  overlays solved parents (via `insertBindParentSolved`).  Base-domain
  structure takes priority.  Solved parents only fill gaps or override
  self-parents/copies.  The `isUpperRef` check (line 94) rejects parents
  that are not structurally above the child in the solved constraint.  This
  is safe: if a valid base parent is rejected, the base-domain KeepOld
  insertion already captured it.

`quotientBindParentsUnder` (Binding/Canonicalization.hs):
  Drops self-edges, merges duplicates by keeping the first parent and taking
  max flag.  "Keep first parent" (lines 92-94) is deterministic.  When UF
  merges nodes from different gen scopes, the first parent seen may not be
  the thesis-correct one.  However, this only applies to the solved-domain
  quotient; the base-domain binding parents (which define ga′) are computed
  separately in Phase 3 and are not subject to this quotient.

`resolveContext` (Presolution/Plan/Context.hs):
  `resolveScopeRoot` maps solved TypeRef through `gaSolvedToBase`, then runs
  `bindingPathToRootLocal` on base binding parents to find the gen ancestor.
  This is the authoritative ga′ recovery path.  The `Nothing -> root`
  fallback (line 119) preserves the solved-domain scope when neither base
  ownership nor certified creation-time destination ownership exists.

  Fresh arguments introduced by expansion do have thesis ownership even
  though they are absent from the base node map: expansion constructs them at
  the destination gen.  `ExpansionConstructionPlacements` preserves that fact before
  later administrative Raise/Weaken steps can move their live solved parent.

Conclusion: Phase 3 projects ownership in the order base ga′, certified fresh
destination arguments, then final solved parents.  The `keepOld`/`keep first
parent` policies are safe because (a) redirects merge structurally equivalent
nodes sharing the same gen ancestor, and (b) base and creation-time ownership
are computed independently of the solved-domain quotient.
-}
constraintForGeneralization :: TraceConfig -> PresolutionView p -> IntMap.IntMap NodeId -> NodeKeySet -> IntMap.IntMap NodeId -> ExpansionConstructionPlacements -> Constraint p -> AnnExpr -> (Constraint p, GaBindParents p)
constraintForGeneralization traceCfg presolutionView redirects instCopyNodes instCopyMap expansionConstructionPlacements base _ann =
    let env = buildGeneralizeEnv traceCfg presolutionView redirects instCopyNodes instCopyMap expansionConstructionPlacements base
        phase1 = restoreSchemeNodes env
        phase2 = buildNodeMappings env phase1
        phase3 = computeBindParentsBase env phase1 phase2
        phase4 = computeSchemeOwnership env phase1 phase2 phase3
    in finalizeConstraint env phase1 phase2 phase3 phase4

buildGeneralizeEnv
    :: TraceConfig
    -> PresolutionView p
    -> IntMap.IntMap NodeId
    -> NodeKeySet
    -> IntMap.IntMap NodeId
    -> ExpansionConstructionPlacements
    -> Constraint p
    -> GeneralizeEnv p
buildGeneralizeEnv traceCfg presolutionView redirects instCopyNodes instCopyMap expansionConstructionPlacements base =
    let canonicalConstraint = pvCanonicalConstraint presolutionView
        canonical = pvCanonical presolutionView
        applyRedirectsToRef ref =
            case ref of
                TypeRef nid -> TypeRef (chaseRedirects redirects nid)
                GenRef gid -> GenRef gid
        canonicalRef = Canonicalize.canonicalRef canonical
        adoptRef = canonicalRef . applyRedirectsToRef
        adoptNodeId nid =
            case adoptRef (typeRef nid) of
                TypeRef nid' -> nid'
                GenRef _ -> nid
    in GeneralizeEnv { geBaseConstraint = base
        , geSolvedConstraint = canonicalConstraint
        , geRedirects = redirects
        , geInstCopyNodes = instCopyNodes
        , geInstCopyMap = instCopyMap
        , geExpansionConstructionPlacements = expansionConstructionPlacements
        , geCanonical = canonical
        , geApplyRedirectsToRef = applyRedirectsToRef
        , geAdoptRef = adoptRef
        , geAdoptNodeId = adoptNodeId
        , geTraceConfig = traceCfg
        }

generalizeAtWithBuilder
    :: PresolutionPlanBuilder
    -> Maybe (GaBindParents p)
    -> PresolutionView p
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)
generalizeAtWithBuilder planBuilder mbBindParentsGa presolutionView scopeRoot targetNode =
    generalizeAtWithBuilderRequired
        planBuilder
        emptyGeneralizationRequirements
        mbBindParentsGa
        presolutionView
        scopeRoot
        targetNode

generalizeAtWithBuilderRequired
    :: PresolutionPlanBuilder
    -> GeneralizationRequirements
    -> Maybe (GaBindParents p)
    -> PresolutionView p
    -> NodeRef
    -> NodeId
    -> Either ElabError (ElabScheme, IntMap.IntMap TypeBinderRef)
generalizeAtWithBuilderRequired planBuilder requirements mbBindParentsGa presolutionView scopeRoot targetNode =
    fmap
        (\(scheme, subst, _routes) -> (scheme, subst))
        ( generalizeAtWithBuilderRequiredRouted
            planBuilder
            requirements
            mbBindParentsGa
            presolutionView
            scopeRoot
            targetNode
        )

generalizeAtWithBuilderRequiredCertified
    :: PresolutionPlanBuilder
    -> GeneralizationRequirements
    -> Maybe (GaBindParents p)
    -> PresolutionView p
    -> NodeRef
    -> NodeId
    -> Either
        ElabError
        ( ElabScheme
        , IntMap.IntMap TypeBinderRef
        , Reify.InheritedGammaRoutes
        )
generalizeAtWithBuilderRequiredCertified planBuilder requirements mbBindParentsGa presolutionView scopeRoot targetNode = do
    (scheme, subst, inheritedRoutes) <-
        generalizeAtWithBuilderRequiredRouted
            planBuilder
            requirements
            mbBindParentsGa
            presolutionView
            scopeRoot
            targetNode
    certifiedSubst <-
        attachInheritedGammaBaseRoutes inheritedRoutes subst
    pure (scheme, certifiedSubst, inheritedRoutes)

-- | Generalize one exact source-constructor result and retain the planner
-- certificate that connects its construction root to the finalized binder.
-- This path deliberately applies and certifies the same plan in one scope;
-- rebuilding a plan after finalization could observe a different graph view.
generalizeAtWithBuilderRequiredResultCertified
    :: PresolutionPlanBuilder
    -> GeneralizedResultRouteRequest
    -> GeneralizationRequirements
    -> Maybe (GaBindParents p)
    -> PresolutionView p
    -> NodeRef
    -> NodeId
    -> Either
        ElabError
        ( ElabScheme
        , IntMap.IntMap TypeBinderRef
        , GeneralizedResultRoute
        )
generalizeAtWithBuilderRequiredResultCertified
    planBuilder
    request
    requirements
    mbBindParentsGa
    presolutionView
    scopeRoot
    targetNode = do
        let PresolutionPlanBuilder buildPlans = planBuilder
        (genPlan, reifyPlan) <-
            buildPlans
                presolutionView
                mbBindParentsGa
                requirements
                scopeRoot
                targetNode
        (scheme, subst) <- applyGeneralizePlan genPlan reifyPlan
        route <-
            certifyGeneralizedResultRoute
                request
                genPlan
                reifyPlan
                scheme
                subst
        pure (scheme, subst, route)

generalizeAtWithBuilderRequiredRouted
    :: PresolutionPlanBuilder
    -> GeneralizationRequirements
    -> Maybe (GaBindParents p)
    -> PresolutionView p
    -> NodeRef
    -> NodeId
    -> Either
        ElabError
        ( ElabScheme
        , IntMap.IntMap TypeBinderRef
        , Reify.InheritedGammaRoutes
        )
generalizeAtWithBuilderRequiredRouted planBuilder requirements mbBindParentsGa presolutionView scopeRoot targetNode =
    let PresolutionPlanBuilder buildPlans = planBuilder
        go mbGa scope target = do
            (genPlan, reifyPlan) <-
                buildPlans presolutionView mbGa requirements scope target
            (scheme, subst) <- applyGeneralizePlan genPlan reifyPlan
            let ReifyPlan {rpPlan = rawReifyPlan} = reifyPlan
                inheritedRoutes =
                    Reify.inheritedGammaPlanRoutes
                        (Reify.rpInheritedGammaPlan rawReifyPlan)
            pure (scheme, subst, inheritedRoutes)
    in go mbBindParentsGa scopeRoot targetNode

-- | Add an inherited base route only when the published substitution has no
-- route for that key.  A source/generalized binder may legitimately use the
-- same base key while the separate 'InheritedGammaRoutes' value retains an
-- ambient rigid capability.  Those two authorities must remain separate;
-- dependency classification overlays the inherited route explicitly when it
-- needs the ambient view.
attachInheritedGammaBaseRoutes
    :: Reify.InheritedGammaRoutes
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
attachInheritedGammaBaseRoutes routes = go (Reify.inheritedGammaRoutesEntries routes)
  where
    go [] subst = pure subst
    go (route : rest) subst =
        let baseNode = Reify.inheritedGammaRouteBaseNode route
            baseKey = getNodeId baseNode
            inheritedRef = Reify.inheritedGammaRouteRef route
        in case IntMap.lookup baseKey subst of
            Nothing ->
                go rest (IntMap.insert baseKey inheritedRef subst)
            Just existing
                | typeBinderRefsSameIdentity existing inheritedRef ->
                    go rest subst
                | otherwise ->
                    go rest subst

mkGeneralizeAtWithBuilder
    :: PresolutionPlanBuilder
    -> PresolutionView p
    -> GeneralizeAtView p
mkGeneralizeAtWithBuilder planBuilder presolutionView mbGa scopeRoot targetNode =
    generalizeAtWithBuilder planBuilder mbGa presolutionView scopeRoot targetNode
