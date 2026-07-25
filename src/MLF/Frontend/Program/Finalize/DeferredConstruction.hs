{-# LANGUAGE GADTs #-}

module MLF.Frontend.Program.Finalize.DeferredConstruction
  ( projectDeferredConstructorConstructionRoutes,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified MLF.Elab.Types as X
import MLF.Frontend.Program.Types
  ( DeferredConstructorCall (..),
    DeferredObligations,
    DeferredProgramObligation (..),
    DeferredRef,
    TypeViewSubst,
    applyTypeViewSubst,
    typeBinderSubstFromTypeViewSubst,
    typeBinderSubstToTypeViewSubst,
    typeViewFromElabType,
  )

-- | Preserve the construction-time binder route of an explicit type redex.
--
-- The constructed xMLF term may introduce a graph-owned binder and immediately
-- apply it to the source binder that selected a deferred constructor.  Inside
-- that abstraction the constructor must be instantiated with the graph-owned
-- binder; the enclosing application performs the source specialization.  This
-- projection is scoped to the abstraction subtree, so sibling occurrences do
-- not inherit the route.
projectDeferredConstructorConstructionRoutes ::
  X.XmlfTerm ->
  DeferredObligations ->
  DeferredObligations
projectDeferredConstructorConstructionRoutes term obligations =
  Map.mapWithKey projectObligation obligations
  where
    occurrenceRoutes = collectDeferredOccurrenceRoutes Map.empty term

    projectObligation ref obligation =
      case (obligation, Map.lookup ref occurrenceRoutes) of
        (DeferredConstructor deferred, Just routes) ->
          DeferredConstructor (projectDeferred routes deferred)
        _ -> obligation

    projectDeferred routes deferred =
      deferred
        { deferredConstructorSourceTypeView =
            projectView (deferredConstructorSourceTypeView deferred),
          deferredConstructorOccurrenceTypeView =
            projectView (deferredConstructorOccurrenceTypeView deferred),
          deferredConstructorInitialSubst =
            typeBinderSubstFromTypeViewSubst
              ( Map.map
                  projectView
                  ( typeBinderSubstToTypeViewSubst
                      (deferredConstructorInitialSubst deferred)
                  )
              )
        }
      where
        projectView = applyTypeViewSubst routes

collectDeferredOccurrenceRoutes ::
  TypeViewSubst ->
  X.XmlfTerm ->
  Map DeferredRef TypeViewSubst
collectDeferredOccurrenceRoutes routes term =
  case term of
    X.EVarNode resolved ->
      case X.deferredResolvedVarRef resolved of
        Just ref -> Map.singleton ref routes
        Nothing -> Map.empty
    X.ELit {} -> Map.empty
    X.ELam _ body ->
      collectDeferredOccurrenceRoutes routes body
    X.EApp fun arg ->
      mergeOccurrences
        (collectDeferredOccurrenceRoutes routes fun)
        (collectDeferredOccurrenceRoutes routes arg)
    X.ELet _ _ rhs body ->
      mergeOccurrences
        (collectDeferredOccurrenceRoutes routes rhs)
        (collectDeferredOccurrenceRoutes routes body)
    X.ETyAbsRef _ _ body ->
      collectDeferredOccurrenceRoutes routes body
    X.ETyInst inner@(X.ETyAbsRef localRef _ _) (X.InstApp (X.TVarRef sourceRef)) ->
      let sourceIdentity = X.typeBinderRefIdentity sourceRef
          localIdentity = X.typeBinderRefIdentity localRef
          innerRoutes
            | sourceIdentity == localIdentity = routes
            | otherwise =
                Map.insert
                  sourceIdentity
                  (typeViewFromElabType (X.TVarRef localRef))
                  routes
       in collectDeferredOccurrenceRoutes innerRoutes inner
    X.ETyInst inner _ ->
      collectDeferredOccurrenceRoutes routes inner
    X.ERoll _ body ->
      collectDeferredOccurrenceRoutes routes body
    X.EUnroll body ->
      collectDeferredOccurrenceRoutes routes body

mergeOccurrences ::
  Map DeferredRef TypeViewSubst ->
  Map DeferredRef TypeViewSubst ->
  Map DeferredRef TypeViewSubst
mergeOccurrences =
  Map.unionWith commonRoutes

-- If malformed input repeats a deferred identity under different construction
-- routes, retain only the route facts shared by every occurrence.  Lowering
-- normally allocates one DeferredRef per occurrence, but making the merge
-- conservative keeps the projection sibling-safe at this boundary.
commonRoutes :: TypeViewSubst -> TypeViewSubst -> TypeViewSubst
commonRoutes =
  Map.mergeWithKey
    (\_ left right -> if left == right then Just left else Nothing)
    (const Map.empty)
    (const Map.empty)
