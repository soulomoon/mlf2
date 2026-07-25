module MLF.Frontend.Program.Finalize.TestSupport
  ( allocateDeferredRewriteBinderRefs,
    constructLocalOccurrencesForSchemeForTest,
    consumeDeferredConstructorHeadInstantiationsForTest,
    consumeDeferredMethodHeadInstantiationsForTest,
    dropStaleTypeInstsForTest,
    freshenDeferredMethodTypeBinders,
    normalizeCheckedTypeRedexesForTest,
    projectDeferredConstructorConstructionRoutesForTest,
  )
where

import Data.Map.Strict (Map)
import MLF.Elab.Types (ElabType)
import qualified MLF.Elab.Types as X
import MLF.Elab.Pipeline (Env)
import qualified MLF.Frontend.Program.Finalize as Finalize
import MLF.Frontend.Program.Finalize.DeferredConstruction
  ( projectDeferredConstructorConstructionRoutes,
  )
import MLF.Frontend.Program.Finalize.IdentitySupply
  ( freshTypeBinderRefsWithSupply,
    freshenElabTypeBindersAgainstTypesFromSupply,
  )
import MLF.Types.Identity
  ( IdentityGenerator,
    TypeBinderIdentity,
    UniqueIdentity,
  )
import MLF.Frontend.Program.Elaborate (ElaborateScope)
import MLF.Frontend.Program.Types
  ( DeferredMethodCall,
    DeferredObligations,
    ProgramError,
    TypeBinderSubst,
    TypeViewSubst,
  )

-- | Test seam shared by deferred constructor inlining and deferred case
-- result-environment construction.
allocateDeferredRewriteBinderRefs ::
  IdentityGenerator ->
  [UniqueIdentity] ->
  [String] ->
  (Map String X.TypeBinderRef, IdentityGenerator)
allocateDeferredRewriteBinderRefs generator =
  freshTypeBinderRefsWithSupply (Just generator)

-- | Test seam for the capture-avoidance allocator used while materializing
-- deferred method evidence and instance methods.
freshenDeferredMethodTypeBinders ::
  IdentityGenerator ->
  [ElabType] ->
  ElabType ->
  (ElabType, IdentityGenerator)
freshenDeferredMethodTypeBinders =
  freshenElabTypeBindersAgainstTypesFromSupply

-- | Focused test seam for construction-time deferred-constructor projection.
projectDeferredConstructorConstructionRoutesForTest ::
  X.XmlfTerm ->
  DeferredObligations ->
  DeferredObligations
projectDeferredConstructorConstructionRoutesForTest =
  projectDeferredConstructorConstructionRoutes

-- | Direct seam for proving that a finalized local scheme constructs its
-- exact-identity occurrences independently of their display spelling.
constructLocalOccurrencesForSchemeForTest ::
  Env ->
  X.ResolvedVar ->
  ElabType ->
  X.XmlfTerm ->
  X.XmlfTerm
constructLocalOccurrencesForSchemeForTest =
  Finalize.constructLocalOccurrencesForSchemeForTest

-- | Direct seam for proving that deferred cleanup only erases computations
-- which are identities at the checked target type.
dropStaleTypeInstsForTest :: Env -> X.XmlfTerm -> X.XmlfTerm
dropStaleTypeInstsForTest =
  Finalize.dropStaleTypeInstsForTest

-- | Direct seam for proving that checked-IR type reduction never publishes a
-- fresh identity outside the finalization supply.
normalizeCheckedTypeRedexesForTest :: X.XmlfTerm -> X.XmlfTerm
normalizeCheckedTypeRedexesForTest =
  Finalize.normalizeCheckedTypeRedexesForTest

-- | Direct seam for ordered, identity-aware deferred constructor head
-- instantiation.
consumeDeferredConstructorHeadInstantiationsForTest ::
  ElaborateScope ->
  String ->
  [(String, TypeBinderIdentity)] ->
  TypeBinderSubst ->
  [ElabType] ->
  Either ProgramError TypeBinderSubst
consumeDeferredConstructorHeadInstantiationsForTest =
  Finalize.consumeDeferredConstructorHeadInstantiationsForTest

-- | Direct seam for consuming the ordered binders recorded from a deferred
-- method's exact placeholder scheme.
consumeDeferredMethodHeadInstantiationsForTest ::
  ElaborateScope ->
  DeferredMethodCall ->
  [ElabType] ->
  Either ProgramError TypeViewSubst
consumeDeferredMethodHeadInstantiationsForTest =
  Finalize.consumeDeferredMethodHeadInstantiationsForTest
