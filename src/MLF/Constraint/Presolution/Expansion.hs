{- |
Module      : MLF.Constraint.Presolution.Expansion
Description : Apply expansion recipes and copy ∀ bodies

This module implements the "expansion application" part of presolution:
applying an `Expansion` recipe to a `TyExp` node, including the χe-style copying
performed during instantiation.

It keeps expansion/copy responsibilities cohesive while the public presolution
entrypoint stays as a thin orchestration layer.
-}
module MLF.Constraint.Presolution.Expansion (
    applyExpansionEdgeTracedAtTarget,
    applyExpansionEdgeTracedAtTargetWithBinders,
    bindExpansionRootLikeTarget,
    bindUnboundCopiedNodes,
    MinimalExpansionDecision(..),
    decideMinimalExpansion,
    decideMinimalExpansionDetailed,
    getExpansion,
    instantiateScheme,
    instantiateSchemeWithTrace,
    mergeExpansions,
    setExpansion
) where

import Control.Monad (foldM, zipWithM, zipWithM_)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, gets, modify)
import Data.Functor.Foldable (cata)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NE

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Presolution.Base (
    CopyMap,
    InteriorSet,
    FrontierSet,
    RawExpansionConstruction,
    PresolutionError(..),
    PresolutionM,
    PresolutionState(..),
    combineRawExpansionConstructions,
    emptyRawExpansionConstruction,
    emptyTrace,
    forallSpecM,
    instantiationBindersM,
    instantiationBindersFromGenM,
    lookupExpansionResultUnder,
    unionTrace
    )
import MLF.Constraint.Presolution.Copy (
    ExpansionBinderProjection(..),
    bindExpansionRootLikeTarget,
    bindUnboundCopiedNodes,
    instantiateScheme,
    instantiateSchemeWithTrace,
    instantiateExpansionWithTraceAtTargetSnapshot,
    projectExpansionBinders
    )
import MLF.Constraint.Presolution.ForallIntro (
    destinationOwnedRootNode,
    introduceForallFromSpec,
    requireDestinationOwnedRoot
    )
import MLF.Constraint.Presolution.Ops (
    createFreshVar,
    getCanonicalNode
    )
import MLF.Constraint.Presolution.StateAccess (
    bindingSnapshotInteriorOf,
    findSchemeIntroducerM,
    getConstraintAndCanonical,
    getBindingSnapshot,
    lookupBindParentM
    )
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Presolution
import MLF.Constraint.Types.SynthesizedExpVar (isSynthesizedExpVar)
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution.Unify (unifyAcyclic)
import MLF.Util.Trace (traceBindingM)

-- | Get the current expansion for an expansion variable.
{-# INLINE getExpansion #-}
getExpansion :: ExpVarId -> PresolutionM p Expansion
getExpansion s = do
    Presolution m <- gets psPresolution
    return $ fromMaybe ExpIdentity (IntMap.lookup (getExpVarId s) m)

-- | Set the expansion for an expansion variable.
{-# INLINE setExpansion #-}
setExpansion :: ExpVarId -> Expansion -> PresolutionM p ()
setExpansion s expansion = do
    modify $ \st ->
        st
            { psPresolution =
                Presolution $
                    IntMap.insert (getExpVarId s) expansion (getAssignments (psPresolution st))
            }

-- | Merge two expansions for the same variable.
-- This may trigger unifications if we merge two Instantiates.
-- The edge worklist validates that every ExpVar has one propagation
-- destination before any merge can retain graph-owning arguments.
mergeExpansions :: ExpVarId -> Expansion -> Expansion -> PresolutionM p Expansion
mergeExpansions _v e1 e2 =
    case (e1, e2) of
        (ExpIdentity, _) -> pure e2
        (_, ExpIdentity) -> pure e1
        _ -> (cata alg e1) e2
  where
    alg layer = case layer of
        ExpIdentityF -> \e2' -> pure e2'
        ExpInstantiateF args1 -> \e2' -> case e2' of
            ExpIdentity -> pure (ExpInstantiate args1)
            ExpInstantiate args2 ->
                if length args1 /= length args2
                    then throwError (ArityMismatch "ExpInstantiate merge" (length args1) (length args2))
                    else do
                        zipWithM_ unifyAcyclic args1 args2
                        pure (ExpInstantiate args1)
            _ -> throwError (InternalError ("Incompatible expansions: " ++ show (ExpInstantiate args1) ++ " vs " ++ show e2'))
        ExpForallF l1 -> \e2' -> case e2' of
            ExpIdentity -> pure (ExpForall l1)
            ExpForall l2 ->
                if l1 == l2
                    then pure (ExpForall l1)
                    else throwError (InternalError "Merging distinct Forall expansions not supported")
            _ -> throwError (InternalError ("Incompatible expansions: " ++ show (ExpForall l1) ++ " vs " ++ show e2'))
        ExpComposeF exps1 -> \e2' -> case e2' of
            ExpIdentity -> do
                merged <- mapM (\step -> step ExpIdentity) (NE.toList exps1)
                pure (ExpCompose (NE.fromList merged))
            ExpCompose exps2 ->
                if length exps1 /= length exps2
                    then throwError (ArityMismatch "ExpCompose merge" (length exps1) (length exps2))
                    else do
                        merged <- zipWithM (\step exp2 -> step exp2) (NE.toList exps1) (NE.toList exps2)
                        pure (ExpCompose (NE.fromList merged))
            _ -> throwError (InternalError ("Incompatible expansions: " ++ show (ExpCompose (fmap (const ExpIdentity) exps1)) ++ " vs " ++ show e2'))

{- Note [Minimal Expansion Decision]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Forall arity mismatch composes elimination and introduction].

Presolution chooses the least-committing expansion for an edge s · τ ≤ τ′ so
that E(τ) matches the shape of τ′ while keeping s as general as possible
(Rémy & Yakobowski, ICFP 2008, §5.2). We now exercise the full paper lattice:
identity, instantiation, ∀-introduction, and explicit composition.

Decision cases (as implemented):

1. ∀ ≤ ∀: if binder arities coincide, keep identity and unify bodies; if
    they differ, instantiate the source binders and re-introduce the target
    quantifiers explicitly.

2. ∀ ≤ structure: instantiate to expose the body. If there are no bound vars
    (degenerate ∀), reuse the body and just unify; otherwise allocate fresh
    nodes for each bound var and return ExpInstantiate. Unifications connect
    the exposed body to τ′ when appropriate.
    [Constraint simplification: Var-Abs (Ch 12.4.1)] — the degenerate-∀ sub-case
    is the presolution residual of Var-Abs: gen nodes that would have been
    degenerate are already avoided during constraint generation (see Note
    [Lambda Translation] in ConstraintGen/Translate.hs), but degenerate foralls
    arising from other sources are handled here by identity expansion.

3. Structure ≤ ∀: generalize to meet the target by wrapping the source body in
    ExpForall at the target level, while unifying the underlying body with the
    target’s body.

4. Structure ≤ structure: keep identity and emit component unifications
    (arrow dom/cod, base equality, etc.).

5. Var ≤ Var: same as structure—identity plus a unification of the two vars.

Level/scope notes: instantiation only introduces fresh nodes for the bound
variables of the source ∀; shared nodes beyond that scope stay shared. The
result is an Expansion (possibly composed) plus the deferred unifications
required by the component constraints.
-}
{- Note [Forall arity mismatch composes elimination and introduction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Figure 14.2.6 of `papers/these-finale-english.txt` distinguishes quantifier
elimination from quantifier introduction.  Consequently an edge such as

    (forall a. a) <= (forall b c. b -> c)

cannot retain the source quantifier in place.  It first instantiates the source
body and then introduces the target quantifiers, represented by
@ExpInstantiate; ExpForall@.  For a singleton source binder the target body is
the exact instantiation argument: the copied body therefore already exposes
the variables selected by the target's Q(n) before forall introduction runs.
Using the target forall root itself would instead put the expansion root in its
own graft bound and manufacture an occurs cycle.

For multiple source binders the edge-local structural unification still
refines fresh arguments.  In both cases the recipe records elimination and
introduction separately; collapsing it to ExpInstantiate would erase required
polymorphism and its witness step.
-}
nearestGenAncestor :: (NodeId -> NodeId) -> NodeId -> PresolutionM p (Maybe GenNodeId)
nearestGenAncestor canonical nid0 = do
    let start = typeRef (canonical nid0)
        go :: IntSet.IntSet -> NodeRef -> PresolutionM p (Maybe GenNodeId)
        go visited ref
            | IntSet.member (nodeRefKey ref) visited =
                pure Nothing
            | otherwise = do
                mbParent <- lookupBindParentM ref
                case mbParent of
                    Nothing -> pure Nothing
                    Just (GenRef gid, _) -> pure (Just gid)
                    Just (TypeRef parent, _) ->
                        go (IntSet.insert (nodeRefKey ref) visited) (typeRef (canonical parent))
    go IntSet.empty start

-- | Expose every leading forall boundary owned by one expansion source.
-- Nested 'TyExp' nodes are administrative occurrence wrappers, so their
-- expansion variables do not establish a new scheme boundary.  Crossing such
-- a wrapper is legal while its body remains under the current scheme owner;
-- when the owner changes, the already-materialized result is the sole
-- producer-owned continuation.  Requiring the nested occurrence to reuse the
-- outer 'ExpVarId' would conflate occurrence-local expansion choice with
-- source-scheme authority and reject nested uses of one polymorphic binding.
instantiationBinderSpineM
    :: (NodeId -> NodeId)
    -> GenNodeId
    -> ExpVarId
    -> NodeId
    -> PresolutionM p (NodeId, [NodeId])
instantiationBinderSpineM canonical ownerGen ownerExpVar source0 =
    collect (canonical source0) []
  where
    collect source binders = do
        st <- get
        let constraint = psConstraint st
            sourceC = canonical source
        case lookupNodeIn (cNodes constraint) sourceC of
            Nothing -> throwError (NodeLookupFailed sourceC)
            Just node ->
                case node of
                    TyForall {tnId = forallId, tnBody = inner} -> do
                        boundaryBinders <-
                            case Binding.orderedBinders id constraint (typeRef forallId) of
                                Left err -> throwError (BindingTreeError err)
                                Right ordered -> pure ordered
                        collect
                            (canonical inner)
                            (binders ++ boundaryBinders)
                    TyExp
                        { tnId = wrapper
                        , tnExpVar = nestedExpVar
                        , tnBody = inner
                        } -> do
                            nestedOwner <-
                                findSchemeIntroducerM
                                    canonical
                                    constraint
                                    (nestedOwnerRoot constraint wrapper inner)
                            if nestedOwner == ownerGen
                                then collect (canonical inner) binders
                                else do
                                    materialized <-
                                        either
                                            throwError
                                            pure
                                            ( lookupExpansionResultUnder
                                                canonical
                                                wrapper
                                                (psExpansionResults st)
                                            )
                                    case materialized of
                                        Nothing ->
                                            throwMismatch wrapper nestedExpVar nestedOwner Nothing
                                        Just result -> do
                                            resultOwner <-
                                                findSchemeIntroducerM
                                                    canonical
                                                    constraint
                                                    result
                                            if resultOwner == ownerGen
                                                then collect (canonical result) binders
                                                else
                                                    throwMismatch
                                                        wrapper
                                                        nestedExpVar
                                                        nestedOwner
                                                        (Just (result, resultOwner))
                    _
                        | null binders ->
                            instantiationBindersFromGenM ownerGen sourceC
                        | otherwise -> pure (sourceC, binders)

    nestedOwnerRoot constraint wrapper inner
        | any isAnnotationEdge (cInstEdges constraint) = wrapper
        | otherwise = inner
      where
        wrapperC = canonical wrapper
        isAnnotationEdge edge =
            canonical (instLeft edge) == wrapperC
                && IntSet.member
                    (getEdgeId (instEdgeId edge))
                    (cAnnEdges constraint)

    throwMismatch
        :: NodeId
        -> ExpVarId
        -> GenNodeId
        -> Maybe (NodeId, GenNodeId)
        -> PresolutionM p a
    throwMismatch wrapper nestedExpVar nestedOwner materialized =
        throwError
            ( NestedTyExpAuthorityMismatch
                wrapper
                nestedOwner
                nestedExpVar
                ownerGen
                ownerExpVar
                materialized
            )

data MinimalExpansionDecision = MinimalExpansionDecision
    { medExpansion :: Expansion
    , medUnifications :: [(NodeId, NodeId)]
    , medBodyRoot :: NodeId
    , medBoundVars :: [NodeId]
    }
    deriving (Eq, Show)

decideMinimalExpansion :: (NodeId -> NodeId) -> GenNodeId -> Bool -> TyNode -> TyNode -> PresolutionM p (Expansion, [(NodeId, NodeId)])
decideMinimalExpansion canonical gid allowTrivial sourceNode targetNode = do
    decision <- decideMinimalExpansionDetailed canonical gid allowTrivial sourceNode targetNode
    pure (medExpansion decision, medUnifications decision)

decideMinimalExpansionDetailed :: (NodeId -> NodeId) -> GenNodeId -> Bool -> TyNode -> TyNode -> PresolutionM p MinimalExpansionDecision
decideMinimalExpansionDetailed canonical gid allowTrivial (TyExp { tnExpVar = expVar, tnBody = bodyId }) targetNode = do
    currentExpansion <- getExpansion expVar
    let instantiationBindersForTarget source =
            case targetNode of
                -- A forall destination consumes one source boundary at this
                -- comparison step.  A structural destination requires the
                -- complete leading source spine for T(e) to reach it.
                TyForall {} -> instantiationBindersM gid source
                _ -> instantiationBinderSpineM canonical gid expVar source
    (bodyRoot, candidateBoundVars) <-
        if isSynthesizedExpVar expVar
            then do
                bodyNode <- getCanonicalNode bodyId
                case bodyNode of
                    -- Paper-shape wrapping synthesizes an expansion node for
                    -- residual edges, but an explicit forall still owns real
                    -- quantifiers.  Preserve those binders so coercion results
                    -- such as forall(beta >= sigma).beta instantiate at their
                    -- application boundary instead of being structurally
                    -- equated with the target.
                    TyForall {} -> instantiationBindersForTarget bodyId
                    TyVar {tnBound = Just bound} -> do
                        boundNode <- getCanonicalNode bound
                        case boundNode of
                            TyForall {} -> instantiationBindersForTarget bound
                            _ -> pure (canonical bodyId, [])
                    _ -> pure (canonical bodyId, [])
            else instantiationBindersForTarget bodyId
    constraint <- gets psConstraint
    binderSnapshot <- getBindingSnapshot
    binderProjection <-
        projectExpansionBinders
            binderSnapshot
            gid
            bodyRoot
            candidateBoundVars
    let boundVars = ebpSemanticBinders binderProjection
        retainedSchemeRootWrappers =
            ebpRetainedSchemeRootWrappers binderProjection
        hasRetainedSchemeRootWrappers =
            not (null retainedSchemeRootWrappers)
    debugExpansion
        ( "decideMinimalExpansion: bodyId="
            ++ show bodyId
            ++ " bodyRoot="
            ++ show bodyRoot
            ++ " boundVars="
            ++ show boundVars
            ++ " outsideSemanticLane="
            ++ show (ebpOutsideSemanticLane binderProjection)
            ++ " target="
            ++ show (tnId targetNode)
        )
    let doneWithBinders activeBinders expn unifications =
            pure
                MinimalExpansionDecision
                    { medExpansion = expn
                    , medUnifications = unifications
                    , medBodyRoot = bodyRoot
                    , medBoundVars = activeBinders
                    }
        done = doneWithBinders boundVars
        instantiateArgs activeBinders =
            case existingInstantiationArgs currentExpansion of
                Just args
                    | length args == length activeBinders -> pure args
                _ -> mapM (const createFreshVar) activeBinders
    targetIsOpaqueBoundedVariable <-
        case targetNode of
            TyVar {tnId = targetVariable, tnBound = Just _} -> do
                kind <-
                    either
                        (throwError . BindingTreeError)
                        pure
                        (Binding.nodeKind constraint (typeRef targetVariable))
                pure $
                    case kind of
                        Binding.NodeRestricted -> True
                        Binding.NodeLocked -> True
                        Binding.NodeRoot -> True
                        Binding.NodeInstantiable -> False
            _ -> pure False
    sourceIsDegenerate <-
        if targetIsOpaqueBoundedVariable
            then do
                snapshot <- getBindingSnapshot
                ownerInterior <- bindingSnapshotInteriorOf snapshot (genRef gid)
                pure $
                    IntSet.notMember
                        (nodeRefKey (typeRef (canonical bodyRoot)))
                        ownerInterior
            else pure False
    bodyShape <- lowerBoundHead bodyRoot
    targetShape <- lowerBoundHead (tnId targetNode)
    isTrivialTarget <- case targetNode of
        TyVar { tnId = targetId, tnBound = Nothing } -> do
            mbGen <- nearestGenAncestor canonical targetId
            case mbGen of
                Nothing -> pure False
                Just targetGenId -> do
                    c0 <- gets psConstraint
                    let targetC = canonical targetId
                        schemeRoots =
                            case NodeAccess.lookupGenNode c0 targetGenId of
                                Just gen -> map canonical (gnSchemes gen)
                                Nothing -> []
                    pure (allowTrivial && targetC `elem` schemeRoots)
        _ -> pure False
    if sourceIsDegenerate && targetIsOpaqueBoundedVariable
        then
            -- Definition 10.1.1: a scheme root outside I(g) has exactly one
            -- legal projection.  When the target is also opaque at this edge,
            -- copy the root as a destination-owned bottom and leave both its
            -- frontier equality and target matching to edge-local Omega.
            -- Looking through either lower bound here would fabricate
            -- structure outside chi_e or through a rigid boundary.
            doneWithBinders [] (ExpInstantiate []) []
        else if hasRetainedSchemeRootWrappers && null boundVars
        then if isTrivialTarget
            then
                done ExpIdentity [(bodyRoot, tnId targetNode)]
            else
                -- The source still needs Definition 10.1.1's fresh structural
                -- projection, but every candidate binder is a retained nested
                -- scheme boundary rather than a chi_e substitution.
                done (ExpInstantiate []) []
        else if not (null boundVars)
        then if isTrivialTarget
            then
                -- Trivial let-scheme instantiation is identity: unify without extra expansion.
                done ExpIdentity [(bodyRoot, tnId targetNode)]
            -- Only a structural target forall can match the source forall in
            -- place.  A TyVar merely *bounded by* a forall denotes a flexible
            -- instance target: treating its lower-bound head as the target
            -- quantifier would align unrelated binders by arity and compare the
            -- source bound with the target body.  Instantiate the source in
            -- that case, as required by the variable's instance relation.
            else case targetNode of
            TyForall { tnId = targetForallId, tnBody = targetBody } -> do
                targetSpec <- forallSpecM targetForallId
                if length boundVars == forallSpecBinderCount targetSpec
                    then
                        -- Note [Minimal Expansion Decision] case 1 (∀≤∀ matching arity)
                        done ExpIdentity [(bodyRoot, targetBody)]
                    else do
                        -- Note [Forall arity mismatch composes elimination and introduction]
                        args <-
                            case boundVars of
                                [_] -> pure [targetBody]
                                _ -> instantiateArgs boundVars
                        let expn =
                                ExpCompose
                                    ( ExpInstantiate args
                                        NE.:| [ExpForall (targetSpec NE.:| [])]
                                    )
                        done expn []
            _ -> do
                -- target is not a forall → instantiate to expose structure
                -- Note [Minimal Expansion Decision] case 2 (∀≤structure, with binders)
                args <- instantiateArgs boundVars
                done (ExpInstantiate args) []
        else do
            case bodyShape of
                TyArrow { tnDom = bDom, tnCod = bCod } -> do
                    case targetShape of
                        TyArrow { tnDom = tDom, tnCod = tCod } ->
                            -- Note [Minimal Expansion Decision] case 4 (structure≤structure, arrow)
                            done ExpIdentity [(bDom, tDom), (bCod, tCod)]
                        TyForall {tnId = targetForallId} -> do
                            -- need to generalize to meet target forall
                            -- Note [Minimal Expansion Decision] case 3 (structure≤∀)
                            targetSpec <- forallSpecM targetForallId
                            let expn = ExpForall (targetSpec NE.:| [])
                            done expn []
                        _ -> done ExpIdentity [(bodyRoot, tnId targetNode)]

                _ -> case targetShape of
                    TyForall {tnId = targetForallId} -> do
                        -- Note [Minimal Expansion Decision] case 3 (structure≤∀)
                        targetSpec <- forallSpecM targetForallId
                        let expn = ExpForall (targetSpec NE.:| [])
                        done expn []
                    _ -> done ExpIdentity [(bodyRoot, tnId targetNode)]

decideMinimalExpansionDetailed _canonical _ _ sourceNode _ =
    pure
        MinimalExpansionDecision
            { medExpansion = ExpIdentity
            , medUnifications = []
            , medBodyRoot = tnId sourceNode
            , medBoundVars = []
        }

existingInstantiationArgs :: Expansion -> Maybe [NodeId]
existingInstantiationArgs expansion =
    case expansion of
        ExpInstantiate args -> Just args
        ExpCompose (ExpInstantiate args NE.:| _) -> Just args
        _ -> Nothing

-- | Follow semantically transparent aliases only far enough to expose the head
-- shape used by the expansion lattice.  Constraint generation represents
-- coercion endpoints as variables with structural lower bounds (for example
-- @alpha >= tau@ and @beta >= forall ...@), and recursive-let constraints can
-- place an identity expansion wrapper on the target.  Classifying either as
-- an opaque variable or wrapper postpones the structural comparison until
-- unification, where equating a wrapper with its own body would manufacture an
-- administrative cycle.  Expansion choice is therefore made from this view
-- while all emitted equalities continue to use the original structural nodes.
lowerBoundHead :: NodeId -> PresolutionM p TyNode
lowerBoundHead = go IntSet.empty
  where
    go seen nodeId = do
        node <- getCanonicalNode nodeId
        let key = getNodeId (tnId node)
        if IntSet.member key seen
            then pure node
            else case node of
                TyVar {tnId = variable, tnBound = Just bound} -> do
                    constraint <- gets psConstraint
                    kind <-
                        either
                            (throwError . BindingTreeError)
                            pure
                            (Binding.nodeKind constraint (typeRef variable))
                    case kind of
                        Binding.NodeInstantiable ->
                            go (IntSet.insert key seen) bound
                        Binding.NodeRoot -> pure node
                        Binding.NodeRestricted -> pure node
                        Binding.NodeLocked -> pure node
                TyExp {tnExpVar = expVar, tnBody = body} -> do
                    expansion <- getExpansion expVar
                    case expansion of
                        ExpIdentity -> go (IntSet.insert key seen) body
                        _ -> pure node
                _ -> pure node

debugExpansion :: String -> PresolutionM p ()
debugExpansion msg = do
    cfg <- ask
    traceBindingM cfg msg

{- Note [Destination-aware edge expansion construction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The paper's χe construction creates an edge expansion at the destination of
that edge.  Ownership therefore has to be present before copied lower bounds
are installed; copying at the source and repairing the root, frontier, and
arguments afterwards can demand a spurious Raise while the graph is only
half-constructed.

Every edge-only expansion shape goes through the target-aware constructor
below.  An ExpInstantiate step copies directly into the target scope, binds its
fresh arguments before copying their bounds, and carries that ownership through
subsequent ExpForall steps.  A forall-only recipe and a degenerate
ExpInstantiate both copy their source projection at the destination before any
wrapper or binding edit is made.  Only identity has no copy constructor; its
result is attached once at the end.  In particular, ExpCompose does not fall
back to a source-scoped copy API for nested instantiation steps.
-}

applyExpansionEdgeTracedAtTarget
    :: GenNodeId
    -> NodeId
    -> Expansion
    -> TyNode
    -> PresolutionM p
        ( NodeId
        , (CopyMap, InteriorSet, FrontierSet)
        , RawExpansionConstruction
        )
applyExpansionEdgeTracedAtTarget gid targetNode expansion expNode =
    applyExpansionEdgeTracedAtTargetFromKnownBinders
        gid
        targetNode
        expansion
        expNode
        start
        Nothing
  where
    start =
        case expNode of
            TyExp {tnBody = body} -> body
            _ -> tnId expNode

applyExpansionEdgeTracedAtTargetWithBinders
    :: GenNodeId
    -> NodeId
    -> Expansion
    -> TyNode
    -> NodeId
    -> [NodeId]
    -> PresolutionM p
        ( NodeId
        , (CopyMap, InteriorSet, FrontierSet)
        , RawExpansionConstruction
        )
applyExpansionEdgeTracedAtTargetWithBinders gid targetNode expansion expNode bodyRoot boundVars =
    applyExpansionEdgeTracedAtTargetFromKnownBinders
        gid
        targetNode
        expansion
        expNode
        bodyRoot
        (Just (bodyRoot, boundVars))

applyExpansionEdgeTracedAtTargetFromKnownBinders
    :: GenNodeId
    -> NodeId
    -> Expansion
    -> TyNode
    -> NodeId
    -> Maybe (NodeId, [NodeId])
    -> PresolutionM p
        ( NodeId
        , (CopyMap, InteriorSet, FrontierSet)
        , RawExpansionConstruction
        )
applyExpansionEdgeTracedAtTargetFromKnownBinders gid targetNode expansion expNode bodyRoot knownBinders0 = do
    ( resultRoot
      , traceResult
      , constructionResult
      , mbDestinationRoot
      , _knownBinders
      ) <-
        applyRecipe
            expansion
            expNode
            bodyRoot
            Nothing
            knownBinders0
    finalRoot <-
        case mbDestinationRoot of
            Just destinationRoot ->
                pure (destinationOwnedRootNode destinationRoot)
            Nothing -> do
                _ <- bindExpansionRootLikeTarget resultRoot targetNode
                pure resultRoot
    pure (finalRoot, traceResult, constructionResult)
  where
    applyRecipe recipe node currentRoot mbDestinationRoot knownBinders =
        case recipe of
            ExpIdentity ->
                pure
                    ( currentRoot
                    , emptyTrace
                    , emptyRawExpansionConstruction
                    , mbDestinationRoot
                    , knownBinders
                    )
            ExpForall specs -> do
                -- A forall introduction is still an expansion: when no
                -- preceding recipe step has constructed a destination-owned
                -- graph, copy the source projection before wrapping it.  In
                -- particular, reusing and rebinding the target forall's
                -- existing body would steal shared source structure and can
                -- make the fresh wrapper a child of itself.
                (bodyAtDestination, copyTrace, copyConstruction) <-
                    case mbDestinationRoot of
                        Just destinationRoot ->
                            pure
                                ( destinationRoot
                                , emptyTrace
                                , emptyRawExpansionConstruction
                                )
                        Nothing ->
                            copyAtDestination currentRoot [] []
                (outer, forallTrace) <-
                    wrapForallTraced (NE.toList specs) bodyAtDestination
                pure
                    ( destinationOwnedRootNode outer
                    , unionTrace copyTrace forallTrace
                    , copyConstruction
                    , Just outer
                    , Nothing
                    )
            ExpCompose steps ->
                foldM
                    (\(root, traceAcc, constructionAcc, owned, known) step -> do
                        rootNode <- getCanonicalNode root
                        (root', trace', construction', owned', known') <-
                            applyRecipe step rootNode root owned known
                        constructionAcc' <-
                            either
                                ( \err ->
                                    throwError
                                        ( InternalError
                                            ( "conflicting construction evidence in "
                                                ++ "composed edge expansion: "
                                                ++ err
                                            )
                                        )
                                )
                                pure
                                ( combineRawExpansionConstructions
                                    constructionAcc
                                    construction'
                                )
                        pure
                            ( root'
                            , unionTrace traceAcc trace'
                            , constructionAcc'
                            , owned'
                            , known'
                            )
                    )
                    ( currentRoot
                    , emptyTrace
                    , emptyRawExpansionConstruction
                    , mbDestinationRoot
                    , knownBinders
                    )
                    (NE.toList steps)
            ExpInstantiate args -> do
                let sourceBody =
                        case node of
                            TyExp {tnBody = body} -> body
                            _ -> tnId node
                (instantiateRoot, instantiateBinders) <-
                    case knownBinders of
                        Just known -> pure known
                        Nothing -> instantiationBindersM gid sourceBody
                if null instantiateBinders
                    then
                        if null args
                            then do
                                ( destinationRoot
                                  , copyTrace
                                  , copyConstruction
                                  ) <-
                                    case mbDestinationRoot of
                                        Just owned ->
                                            pure
                                                ( owned
                                                , emptyTrace
                                                , emptyRawExpansionConstruction
                                                )
                                        Nothing -> copyAtDestination instantiateRoot [] []
                                pure
                                    ( destinationOwnedRootNode destinationRoot
                                    , copyTrace
                                    , copyConstruction
                                    , Just destinationRoot
                                    , Nothing
                                    )
                            else throwError (InstantiateOnNonForall sourceBody)
                    else do
                        argsForBinders <- normalizeInstantiationArgs instantiateBinders args
                        binderMetas <-
                            mapM
                                (\binder -> do
                                    meta <- createFreshVar
                                    pure (binder, meta)
                                )
                                instantiateBinders
                        let binderArgs = zip instantiateBinders argsForBinders
                        ( destinationRoot
                          , copyTrace
                          , copyConstruction
                          ) <-
                            copyAtDestination
                                instantiateRoot
                                binderMetas
                                binderArgs
                        pure
                            ( destinationOwnedRootNode destinationRoot
                            , copyTrace
                            , copyConstruction
                            , Just destinationRoot
                            , Nothing
                            )

    copyAtDestination sourceRoot binderMetas binderArgs = do
        destinationOwner <- expansionDestinationOwner targetNode
        snapshot <- getBindingSnapshot
        ( (copiedRoot, bodyCopyMap, bodyInterior, bodyFrontier)
          , (boundCopyMap, boundInterior, boundFrontier)
          , construction
          ) <-
            instantiateExpansionWithTraceAtTargetSnapshot
                snapshot
                gid
                targetNode
                sourceRoot
                binderMetas
                binderArgs
        destinationRoot <-
            requireDestinationOwnedRoot destinationOwner copiedRoot
        pure
            ( destinationRoot
            , ( bodyCopyMap <> boundCopyMap
              , IntSet.union bodyInterior boundInterior
              , IntSet.union bodyFrontier boundFrontier
              )
            , construction
            )

    -- This is the same destination rule used by the copy constructor: a
    -- non-root target contributes its current parent, while a binding-root
    -- target is owned by the unique root gen node.  The value is computed from
    -- the target, independently of the copied root, so minting
    -- DestinationOwnedRoot cannot bless an arbitrary source owner.
    expansionDestinationOwner target = do
        mbParentInfo <- lookupBindParentM (typeRef target)
        case mbParentInfo of
            Just (parentRef, _flag) -> pure parentRef
            Nothing -> do
                (constraint, _canonical) <- getConstraintAndCanonical
                rootGen <-
                    foldM
                        (\acc gidInt ->
                            case acc of
                                Just _ -> pure acc
                                Nothing -> do
                                    let gref = genRef (GenNodeId gidInt)
                                    mbParent <- lookupBindParentM gref
                                    pure $
                                        case mbParent of
                                            Nothing -> Just gref
                                            Just _ -> Nothing
                        )
                        Nothing
                        (IntMap.keys (getGenNodeMap (cGenNodes constraint)))
                case rootGen of
                    Just owner@(GenRef _) -> pure owner
                    Just (TypeRef _) ->
                        throwError
                            (InternalError "expected gen root binder for expansion target")
                    Nothing ->
                        throwError
                            (InternalError "missing gen root binder for expansion target")

    normalizeInstantiationArgs binders args
        | length binders == length args = pure args
        | length binders == 1
        , arg0 : rest <- args = do
            mapM_ (unifyAcyclic arg0) rest
            pure [arg0]
        | otherwise =
            throwError $
                ArityMismatch
                    "applyExpansionEdgeTracedAtTargetWithBinders"
                    (length binders)
                    (length args)

    wrapForallTraced [] destinationRoot = pure (destinationRoot, emptyTrace)
    wrapForallTraced (spec : specs) destinationRoot = do
        newRoot <- introduceForallFromSpec spec destinationRoot
        (outer, (cmap, interior, frontier)) <- wrapForallTraced specs newRoot
        pure
            ( outer
            , ( cmap
              , IntSet.insert
                    (getNodeId (destinationOwnedRootNode newRoot))
                    interior
              , frontier
              )
            )

-- Copying helpers (`instantiateScheme*` + binding fixes) live in
-- `MLF.Constraint.Presolution.Copy`.
