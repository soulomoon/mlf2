{- |
Module      : MLF.Constraint.Presolution.Expansion
Description : Apply expansion recipes and copy ∀ bodies

This module implements the "expansion application" part of presolution:
applying an `Expansion` recipe to a `TyExp` node, including the χe-style copying
performed during instantiation.

It is extracted from `MLF.Constraint.Presolution.Core` to reduce module size and
to keep expansion/copy responsibilities cohesive.
-}
module MLF.Constraint.Presolution.Expansion (
    applyExpansion,
    applyExpansionTraced,
    applyExpansionEdgeTraced,
    bindExpansionRootLikeTarget,
    bindUnboundCopiedNodes,
    decideMinimalExpansion,
    getExpansion,
    instantiateScheme,
    instantiateSchemeWithTrace,
    mergeExpansions,
    recordEdgeExpansion,
    setExpansion
) where

import Control.Monad (foldM, zipWithM, zipWithM_)
import Control.Monad.Except (throwError)
import Control.Monad.State
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NE
import Debug.Trace (trace)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

import qualified MLF.Util.UnionFind as UnionFind
import MLF.Constraint.Presolution.Base (
    CopyMap,
    InteriorSet,
    FrontierSet,
    PresolutionError(..),
    PresolutionM,
    PresolutionState(..),
    emptyTrace,
    forallSpecM,
    instantiationBindersM,
    unionTrace
    )
import MLF.Constraint.Presolution.Copy (
    bindExpansionRootLikeTarget,
    bindUnboundCopiedNodes,
    instantiateScheme,
    instantiateSchemeWithTrace
    )
import MLF.Constraint.Presolution.ForallIntro (introduceForallFromSpec)
import MLF.Constraint.Presolution.Ops (
    createFreshVar,
    getCanonicalNode,
    lookupVarBound,
    setVarBound
    )
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types
import MLF.Constraint.Presolution.Unify (unifyAcyclic)

-- | Get the current expansion for an expansion variable.
getExpansion :: ExpVarId -> PresolutionM Expansion
getExpansion s = do
    Presolution m <- gets psPresolution
    return $ fromMaybe ExpIdentity (IntMap.lookup (getExpVarId s) m)

-- | Set the expansion for an expansion variable.
setExpansion :: ExpVarId -> Expansion -> PresolutionM ()
setExpansion s expansion = do
    modify $ \st ->
        st
            { psPresolution =
                Presolution $
                    IntMap.insert (getExpVarId s) expansion (getAssignments (psPresolution st))
            }

recordEdgeExpansion :: EdgeId -> Expansion -> PresolutionM ()
recordEdgeExpansion (EdgeId eid) expn =
    modify $ \st -> st { psEdgeExpansions = IntMap.insert eid expn (psEdgeExpansions st) }

-- | Merge two expansions for the same variable.
-- This may trigger unifications if we merge two Instantiates.
mergeExpansions :: ExpVarId -> Expansion -> Expansion -> PresolutionM Expansion
mergeExpansions v e1 e2 = case (e1, e2) of
    (ExpIdentity, _) -> pure e2
    (_, ExpIdentity) -> pure e1
    (ExpInstantiate args1, ExpInstantiate args2) -> do
        if length args1 /= length args2
            then throwError (ArityMismatch "ExpInstantiate merge" (length args1) (length args2))
            else do
                zipWithM_ unifyAcyclic args1 args2
                pure e1
    (ExpForall l1, ExpForall l2) -> do
        if l1 == l2 then pure e1
        else throwError (InternalError "Merging distinct Forall expansions not supported")
    (ExpCompose exps1, ExpCompose exps2) -> do
        if length exps1 /= length exps2
            then throwError (ArityMismatch "ExpCompose merge" (length exps1) (length exps2))
            else do
                merged <- zipWithM (mergeExpansions v) (NE.toList exps1) (NE.toList exps2)
                pure (ExpCompose (NE.fromList merged))
    _ -> throwError (InternalError ("Incompatible expansions: " ++ show e1 ++ " vs " ++ show e2))

{- Note [Minimal Expansion Decision]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Forall Level Mismatch → Compose].

Presolution chooses the least-committing expansion for an edge s · τ ≤ τ′ so
that E(τ) matches the shape of τ′ while keeping s as general as possible
(Rémy & Yakobowski, ICFP 2008, §5.2). We now exercise the full paper lattice:
identity, instantiation, ∀-introduction, and explicit composition.

Decision cases (as implemented):

1. ∀ ≤ ∀: if quantifier levels coincide, keep identity and unify bodies; if they
    differ, instantiate the source binders (fresh vars) then re-generalize to
    the target level via ExpCompose (ExpInstantiate · ExpForall).

2. ∀ ≤ structure: instantiate to expose the body. If there are no bound vars
    (degenerate ∀), reuse the body and just unify; otherwise allocate fresh
    nodes for each bound var and return ExpInstantiate. Unifications connect
    the exposed body to τ′ when appropriate.

3. Structure ≤ ∀: generalize to meet the target by wrapping the source body in
    ExpForall at the target level, while unifying the underlying body with the
    target’s body.

4. Structure ≤ structure: keep identity and emit component unifications
    (arrow dom/cod, base equality, etc.).

5. Var ≤ Var: same as structure—identity plus a unification of the two vars.

Level/scope notes: instantiation only introduces fresh nodes for the bound
variables of the source ∀; shared nodes beyond that scope stay shared. The
result is an Expansion (possibly composed) plus the deferred unifications
required for compatibility.
-}
{- Note [Forall Level Mismatch → Compose]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Paper §5 and Definition 5 ("expansion of g at g′") in
`papers/recasting-mlf-RR.txt` and the matching discussion in
`papers/Remy-Yakobowski@icfp08_mlf-type-inference.txt` describe how a forall
body is copied and re-bound at a destination generalization node. For an edge
s · (forall@ℓ α.σ) ≤ forall@ℓ′ β.τ with ℓ ≠ ℓ′ the minimal recipe is:

    1. instantiate the source binders at level ℓ (fresh metas, drop the old ∀)
    2. re-introduce ∀ at the demanded level ℓ′

The lattice offers only “instantiate” and “add ∀”, so changing the quantifier
level is spelled as ExpCompose (ExpInstantiate · ExpForall). Instantiation
alone would lose required polymorphism; ∀ alone would quantify at the wrong
level. The sequence preserves sharing outside the binder and remains the least
expansion that satisfies the edge (principality argument in §5).
-}
nearestGenAncestor :: NodeId -> PresolutionM (Maybe GenNodeId)
nearestGenAncestor nid0 = do
    c0 <- gets psConstraint
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
        start = typeRef (canonical nid0)
        go :: IntSet.IntSet -> NodeRef -> PresolutionM (Maybe GenNodeId)
        go visited ref
            | IntSet.member (nodeRefKey ref) visited =
                pure Nothing
            | otherwise = do
                mbParent <- case Binding.lookupBindParentUnder canonical c0 ref of
                    Left err -> throwError (BindingTreeError err)
                    Right res -> pure res
                case mbParent of
                    Nothing -> pure Nothing
                    Just (GenRef gid, _) -> pure (Just gid)
                    Just (TypeRef parent, _) ->
                        go (IntSet.insert (nodeRefKey ref) visited) (typeRef (canonical parent))
    go IntSet.empty start

forallSpecFromBinders :: [NodeId] -> PresolutionM ForallSpec
forallSpecFromBinders binders0 = do
    uf0 <- gets psUnionFind
    let canonical = UnionFind.frWith uf0
        binders = map canonical binders0
        binderIndex =
            IntMap.fromList
                [ (getNodeId b, idx)
                | (idx, b) <- zip [0..] binders
                ]
        boundFor b = do
            mbBound <- lookupVarBound b
            pure $ case mbBound of
                Nothing -> Nothing
                Just bnd ->
                    let bndC = canonical bnd
                    in case IntMap.lookup (getNodeId bndC) binderIndex of
                        Just idx -> Just (BoundBinder idx)
                        Nothing -> Just (BoundNode bndC)
    bounds <- mapM boundFor binders
    pure ForallSpec
        { fsBinderCount = length binders
        , fsBounds = bounds
        }

decideMinimalExpansion :: Bool -> TyNode -> TyNode -> PresolutionM (Expansion, [(NodeId, NodeId)])
decideMinimalExpansion allowTrivial (TyExp { tnBody = bodyId }) targetNode = do
    (bodyRoot, boundVars) <- instantiationBindersM bodyId
    case debugExpansion
        ( "decideMinimalExpansion: bodyId="
            ++ show bodyId
            ++ " bodyRoot="
            ++ show bodyRoot
            ++ " boundVars="
            ++ show boundVars
            ++ " target="
            ++ show (tnId targetNode)
        )
        ()
        of
            () -> pure ()
    isTrivialTarget <- case targetNode of
        TyVar { tnId = targetId, tnBound = Nothing } -> do
            mbGen <- nearestGenAncestor targetId
            case mbGen of
                Nothing -> pure False
                Just gid -> do
                    c0 <- gets psConstraint
                    uf0 <- gets psUnionFind
                    let canonical = UnionFind.frWith uf0
                        targetC = canonical targetId
                        schemeRoots =
                            case IntMap.lookup (genNodeKey gid) (cGenNodes c0) of
                                Just gen -> map canonical (gnSchemes gen)
                                Nothing -> []
                    pure (allowTrivial && targetC `elem` schemeRoots)
        _ -> pure False
    levelMismatch <- if null boundVars
        then pure False
        else do
            srcGen <- nearestGenAncestor bodyRoot
            tgtGen <- nearestGenAncestor (tnId targetNode)
            targetIsVar <- case targetNode of
                TyVar{ tnId = targetId } -> do
                    mbBound <- lookupVarBound targetId
                    case mbBound of
                        Nothing -> pure True
                        Just bnd -> do
                            bndNode <- getCanonicalNode bnd
                            pure $ case bndNode of
                                TyArrow{} -> False
                                TyBase{} -> False
                                TyBottom{} -> False
                                _ -> True
                _ -> pure False
            pure (srcGen /= tgtGen && srcGen /= Nothing && tgtGen /= Nothing && targetIsVar)
    if not (null boundVars)
        then if isTrivialTarget
            then
                -- Trivial let-scheme instantiation is identity: unify without extra expansion.
                return (ExpIdentity, [(bodyRoot, tnId targetNode)])
            else case targetNode of
            TyForall { tnId = targetForallId, tnBody = targetBody } -> do
                targetSpec <- forallSpecM targetForallId
                if length boundVars == fsBinderCount targetSpec
                    then
                        -- Note [Minimal Expansion Decision] case 1 (∀≤∀ matching arity)
                        return (ExpIdentity, [(bodyRoot, targetBody)])
                    else do
                        -- Note [Minimal Expansion Decision] case 1 (∀≤∀ arity mismatch)
                        freshNodes <- mapM (const createFreshVar) boundVars
                        let expn =
                                ExpCompose
                                    (ExpInstantiate freshNodes NE.<| (ExpForall (targetSpec NE.:| []) NE.:| []))
                        return (expn, [])
            _ | levelMismatch -> do
                freshNodes <- mapM (const createFreshVar) boundVars
                spec <- forallSpecFromBinders boundVars
                let expn =
                        ExpCompose
                            (ExpInstantiate freshNodes NE.<| (ExpForall (spec NE.:| []) NE.:| []))
                return (expn, [])
            _ -> do
                -- target is not a forall → instantiate to expose structure
                -- Note [Minimal Expansion Decision] case 2 (∀≤structure, with binders)
                freshNodes <- mapM (const createFreshVar) boundVars
                return (ExpInstantiate freshNodes, [])
        else do
            bodyNode <- getCanonicalNode bodyRoot
            case bodyNode of
                TyArrow { tnDom = bDom, tnCod = bCod } -> do
                    case targetNode of
                        TyArrow { tnDom = tDom, tnCod = tCod } ->
                            -- Note [Minimal Expansion Decision] case 4 (structure≤structure, arrow)
                            return (ExpIdentity, [(bDom, tDom), (bCod, tCod)])
                        TyForall {} -> do
                            -- need to generalize to meet target forall
                            -- Note [Minimal Expansion Decision] case 3 (structure≤∀)
                            targetSpec <- forallSpecM (tnId targetNode)
                            let expn = ExpForall (targetSpec NE.:| [])
                            return (expn, [])
                        _ -> return (ExpIdentity, [(bodyRoot, tnId targetNode)])

                _ -> case targetNode of
                    TyForall {} -> do
                        -- Note [Minimal Expansion Decision] case 3 (structure≤∀)
                        targetSpec <- forallSpecM (tnId targetNode)
                        let expn = ExpForall (targetSpec NE.:| [])
                        return (expn, [])
                    _ -> return (ExpIdentity, [(bodyRoot, tnId targetNode)])

decideMinimalExpansion _ _ _ = return (ExpIdentity, [])

debugExpansion :: String -> a -> a
debugExpansion msg value =
    if debugExpansionEnabled
        then trace msg value
        else value

debugExpansionEnabled :: Bool
debugExpansionEnabled =
    unsafePerformIO $ do
        enabled <- lookupEnv "MLF_DEBUG_BINDING"
        pure (maybe False (const True) enabled)
{-# NOINLINE debugExpansionEnabled #-}

-- | Apply an expansion to a TyExp node.
-- Note: this helper is used twice for distinct purposes.
--   • processInstEdge: enforce a single instantiation edge now (expansion choice
--     plus the unifications it triggers) so later edges see the refined graph.
--   • materializeExpansions: after all edges are processed, rewrite the graph to
--     erase TyExp nodes and clear inst edges before Solve.
applyExpansion :: Expansion -> TyNode -> PresolutionM NodeId
applyExpansion expansion expNode = case (expansion, expNode) of
    (ExpIdentity, TyExp { tnBody = b }) -> return b
    (ExpInstantiate args, TyExp { tnBody = b }) -> do
        (bodyRoot, boundVars) <- instantiationBindersM b
        if null boundVars
            then
                if null args
                    then pure bodyRoot
                    else
                        throwError $ InstantiateOnNonForall bodyRoot
        else if length boundVars /= length args
            then throwError $ ArityMismatch "applyExpansion" (length boundVars) (length args)
            else instantiateScheme bodyRoot (zip boundVars args)
    (ExpForall specs, TyExp { tnBody = b }) -> do
        wrapForall (NE.toList specs) b
    (ExpCompose exps, TyExp { tnBody = b }) -> do
        foldM
            (\nid e -> do
                node <- getCanonicalNode nid
                applyExpansionOverNode e node nid)
            b
            (NE.toList exps)
    -- If expansion is applied to a non-expansion node (after composition), use the helper
    (expn, otherNode) -> do
        applyExpansionOverNode expn otherNode (tnId otherNode)
  where
    wrapForall :: [ForallSpec] -> NodeId -> PresolutionM NodeId
    wrapForall [] nid = return nid
    wrapForall (spec:ls) nid = do
        newId <- introduceForallFromSpec spec nid
        wrapForall ls newId

    -- Allow composing over an already-expanded node (not necessarily TyExp)
    applyExpansionOverNode :: Expansion -> TyNode -> NodeId -> PresolutionM NodeId
    applyExpansionOverNode ExpIdentity _ nid = return nid
    applyExpansionOverNode (ExpForall ls) _ nid = wrapForall (NE.toList ls) nid
    applyExpansionOverNode (ExpCompose es) _ nid =
        foldM
            (\n e -> do
                nNode <- getCanonicalNode n
                applyExpansionOverNode e nNode n)
            nid
            (NE.toList es)
    applyExpansionOverNode (ExpInstantiate args) node _ =
        case node of
            TyExp{ tnBody = b } -> do
                (bodyRoot, boundVars) <- instantiationBindersM b
                if null boundVars
                    then
                        if null args
                            then pure bodyRoot
                            else throwError $ InstantiateOnNonForall (tnId node)
                else if length boundVars /= length args
                    then throwError $ ArityMismatch "applyExpansionOverNode" (length boundVars) (length args)
                    else instantiateScheme bodyRoot (zip boundVars args)
            _ -> do
                (bodyRoot, boundVars) <- instantiationBindersM (tnId node)
                if null boundVars
                    then
                        if null args
                            then pure bodyRoot
                            else
                                throwError $ InstantiateOnNonForall (tnId node)
                else if length boundVars /= length args
                    then throwError $ ArityMismatch "applyExpansionOverNode" (length boundVars) (length args)
                    else instantiateScheme bodyRoot (zip boundVars args)

-- | Apply an expansion like 'applyExpansion', but also return a (coarse) trace
-- of the expansion interior I(r): instantiation args, copied nodes, and any
-- freshly introduced ∀ wrappers.
applyExpansionTraced :: Expansion -> TyNode -> PresolutionM (NodeId, (CopyMap, InteriorSet, FrontierSet))
applyExpansionTraced expansion expNode = case (expansion, expNode) of
    (ExpIdentity, TyExp { tnBody = b }) -> pure (b, emptyTrace)
    (ExpInstantiate args, TyExp { tnBody = b }) -> do
        (bodyRoot, boundVars) <- instantiationBindersM b
        if null boundVars
            then
                if null args
                    then pure (bodyRoot, emptyTrace)
                    else
                                throwError $ InstantiateOnNonForall bodyRoot
        else if length boundVars /= length args
            then throwError $ ArityMismatch "applyExpansionTraced" (length boundVars) (length args)
            else do
                (root, cmap, interior, frontier) <- instantiateSchemeWithTrace bodyRoot (zip boundVars args)
                pure (root, (cmap, interior, frontier))
    (ExpForall specs, TyExp { tnBody = b }) -> do
        wrapForallTraced (NE.toList specs) b
    (ExpCompose exps, TyExp { tnBody = b }) -> do
        foldM
            (\(nid, trAcc) e -> do
                node <- getCanonicalNode nid
                (nid', tr') <- applyExpansionOverNodeTraced e node nid
                pure (nid', unionTrace trAcc tr'))
            (b, emptyTrace)
            (NE.toList exps)
    (expn, otherNode) -> do
        applyExpansionOverNodeTraced expn otherNode (tnId otherNode)
  where
    wrapForallTraced :: [ForallSpec] -> NodeId -> PresolutionM (NodeId, (CopyMap, InteriorSet, FrontierSet))
    wrapForallTraced [] nid = pure (nid, emptyTrace)
    wrapForallTraced (spec:ls) nid = do
        newId <- introduceForallFromSpec spec nid
        (outer, (cmap, interior, frontier)) <- wrapForallTraced ls newId
        pure (outer, (cmap, IntSet.insert (getNodeId newId) interior, frontier))

    applyExpansionOverNodeTraced :: Expansion -> TyNode -> NodeId -> PresolutionM (NodeId, (CopyMap, InteriorSet, FrontierSet))
    applyExpansionOverNodeTraced ExpIdentity _ nid = pure (nid, emptyTrace)
    applyExpansionOverNodeTraced (ExpForall ls) _ nid =
        wrapForallTraced (NE.toList ls) nid
    applyExpansionOverNodeTraced (ExpCompose es) _ nid =
        foldM
            (\(n, trAcc) e -> do
                nNode <- getCanonicalNode n
                (n', tr') <- applyExpansionOverNodeTraced e nNode n
                pure (n', unionTrace trAcc tr'))
            (nid, emptyTrace)
            (NE.toList es)
    applyExpansionOverNodeTraced (ExpInstantiate args) node _ =
        case node of
            TyExp{ tnBody = b } -> do
                (bodyRoot, boundVars) <- instantiationBindersM b
                if null boundVars
                    then
                        if null args
                            then pure (bodyRoot, emptyTrace)
                            else throwError $ InstantiateOnNonForall (tnId node)
                else if length boundVars /= length args
                    then throwError $ ArityMismatch "applyExpansionOverNodeTraced" (length boundVars) (length args)
                    else do
                        (root, cmap, interior, frontier) <- instantiateSchemeWithTrace bodyRoot (zip boundVars args)
                        pure (root, (cmap, interior, frontier))
            _ -> do
                (bodyRoot, boundVars) <- instantiationBindersM (tnId node)
                if null boundVars
                    then
                        if null args
                            then pure (bodyRoot, emptyTrace)
                            else
                                throwError $ InstantiateOnNonForall (tnId node)
                else if length boundVars /= length args
                    then throwError $ ArityMismatch "applyExpansionOverNodeTraced" (length boundVars) (length args)
                    else do
                        (root, cmap, interior, frontier) <- instantiateSchemeWithTrace bodyRoot (zip boundVars args)
                        pure (root, (cmap, interior, frontier))

-- | Like 'applyExpansionTraced', but for edge processing: construct χe-style copies
-- so that Ω operations (Graft/Weaken/Merge) can be executed as graph transformations.
--
    -- In particular, `ExpInstantiate` copies the body by substituting binders with
    -- fresh binder-meta variables, and copies their instance bounds onto the
    -- instantiation arguments (via `MLF.Constraint.VarStore`).
applyExpansionEdgeTraced :: Expansion -> TyNode -> PresolutionM (NodeId, (CopyMap, InteriorSet, FrontierSet))
applyExpansionEdgeTraced expansion expNode = case (expansion, expNode) of
    (ExpIdentity, TyExp { tnBody = b }) -> pure (b, emptyTrace)
    (ExpInstantiate args, TyExp { tnBody = b }) -> do
        (bodyRoot, boundVars) <- instantiationBindersM b
        if null boundVars
            then
                if null args
                    then pure (bodyRoot, emptyTrace)
                    else
                                throwError $ InstantiateOnNonForall bodyRoot
        else if length boundVars /= length args
            then throwError $ ArityMismatch "applyExpansionEdgeTraced" (length boundVars) (length args)
            else do
                metas <- zipWithM binderMetaAt args boundVars
                let binderMetas = zip boundVars metas
                    binderArgs = zip boundVars args
                (root, cmap0, interior0, frontier0) <- instantiateSchemeWithTrace bodyRoot binderMetas
                (cmapB, interiorB, frontierB) <- copyBinderBounds binderMetas binderArgs
                pure (root, (IntMap.union cmap0 cmapB, IntSet.union interior0 interiorB, IntSet.union frontier0 frontierB))
    (ExpForall specs, TyExp { tnBody = b }) -> do
        wrapForallTraced (NE.toList specs) b
    (ExpCompose exps, TyExp { tnBody = b }) -> do
        foldM
            (\(nid, trAcc) e -> do
                node <- getCanonicalNode nid
                (nid', tr') <- applyExpansionOverNodeEdgeTraced e node nid
                pure (nid', unionTrace trAcc tr'))
            (b, emptyTrace)
            (NE.toList exps)
    (expn, otherNode) -> do
        applyExpansionOverNodeEdgeTraced expn otherNode (tnId otherNode)
  where
    binderMetaAt :: NodeId -> NodeId -> PresolutionM NodeId
    binderMetaAt _arg _bv =
        createFreshVar

    wrapForallTraced :: [ForallSpec] -> NodeId -> PresolutionM (NodeId, (CopyMap, InteriorSet, FrontierSet))
    wrapForallTraced [] nid = pure (nid, emptyTrace)
    wrapForallTraced (spec:ls) nid = do
        newId <- introduceForallFromSpec spec nid
        (outer, (cmap, interior, frontier)) <- wrapForallTraced ls newId
        pure (outer, (cmap, IntSet.insert (getNodeId newId) interior, frontier))

    applyExpansionOverNodeEdgeTraced :: Expansion -> TyNode -> NodeId -> PresolutionM (NodeId, (CopyMap, InteriorSet, FrontierSet))
    applyExpansionOverNodeEdgeTraced ExpIdentity _ nid = pure (nid, emptyTrace)
    applyExpansionOverNodeEdgeTraced (ExpForall ls) _ nid =
        wrapForallTraced (NE.toList ls) nid
    applyExpansionOverNodeEdgeTraced (ExpCompose es) _ nid =
        foldM
            (\(n, trAcc) e -> do
                nNode <- getCanonicalNode n
                (n', tr') <- applyExpansionOverNodeEdgeTraced e nNode n
                pure (n', unionTrace trAcc tr'))
            (nid, emptyTrace)
            (NE.toList es)
    applyExpansionOverNodeEdgeTraced (ExpInstantiate args) node _ =
        case node of
            TyExp{ tnBody = b } -> do
                (bodyRoot, boundVars) <- instantiationBindersM b
                if null boundVars
                    then
                        if null args
                            then pure (bodyRoot, emptyTrace)
                            else throwError $ InstantiateOnNonForall (tnId node)
                else if length boundVars /= length args
                    then
                        if length boundVars == 1 && length args > 1
                            then case args of
                                [] -> throwError $ ArityMismatch "applyExpansionOverNodeEdgeTraced" (length boundVars) (length args)
                                (arg0:rest) -> do
                                    mapM_ (unifyAcyclic arg0) rest
                                    metas <- zipWithM binderMetaAt [arg0] boundVars
                                    let binderMetas = zip boundVars metas
                                        binderArgs = zip boundVars [arg0]
                                    (root, cmap0, interior0, frontier0) <- instantiateSchemeWithTrace bodyRoot binderMetas
                                    (cmapB, interiorB, frontierB) <- copyBinderBounds binderMetas binderArgs
                                    pure (root, (IntMap.union cmap0 cmapB, IntSet.union interior0 interiorB, IntSet.union frontier0 frontierB))
                            else throwError $ ArityMismatch "applyExpansionOverNodeEdgeTraced" (length boundVars) (length args)
                    else do
                        metas <- zipWithM binderMetaAt args boundVars
                        let binderMetas = zip boundVars metas
                            binderArgs = zip boundVars args
                        (root, cmap0, interior0, frontier0) <- instantiateSchemeWithTrace bodyRoot binderMetas
                        (cmapB, interiorB, frontierB) <- copyBinderBounds binderMetas binderArgs
                        pure (root, (IntMap.union cmap0 cmapB, IntSet.union interior0 interiorB, IntSet.union frontier0 frontierB))
            _ -> do
                (bodyRoot, boundVars) <- instantiationBindersM (tnId node)
                if null boundVars
                    then
                        if null args
                            then pure (bodyRoot, emptyTrace)
                            else
                                throwError $ InstantiateOnNonForall (tnId node)
                else if length boundVars /= length args
                    then
                        if length boundVars == 1 && length args > 1
                            then case args of
                                [] -> throwError $ ArityMismatch "applyExpansionOverNodeEdgeTraced" (length boundVars) (length args)
                                (arg0:rest) -> do
                                    mapM_ (unifyAcyclic arg0) rest
                                    metas <- zipWithM binderMetaAt [arg0] boundVars
                                    let binderMetas = zip boundVars metas
                                        binderArgs = zip boundVars [arg0]
                                    (root, cmap0, interior0, frontier0) <- instantiateSchemeWithTrace bodyRoot binderMetas
                                    (cmapB, interiorB, frontierB) <- copyBinderBounds binderMetas binderArgs
                                    pure (root, (IntMap.union cmap0 cmapB, IntSet.union interior0 interiorB, IntSet.union frontier0 frontierB))
                            else throwError $ ArityMismatch "applyExpansionOverNodeEdgeTraced" (length boundVars) (length args)
                    else do
                        metas <- zipWithM binderMetaAt args boundVars
                        let binderMetas = zip boundVars metas
                            binderArgs = zip boundVars args
                        (root, cmap0, interior0, frontier0) <- instantiateSchemeWithTrace bodyRoot binderMetas
                        (cmapB, interiorB, frontierB) <- copyBinderBounds binderMetas binderArgs
                        pure (root, (IntMap.union cmap0 cmapB, IntSet.union interior0 interiorB, IntSet.union frontier0 frontierB))

-- | Copy (and re-bind) instance bounds from original binders onto fresh binder-metas.
copyBinderBounds :: [(NodeId, NodeId)] -> [(NodeId, NodeId)] -> PresolutionM (CopyMap, InteriorSet, FrontierSet)
copyBinderBounds binderMetas binderArgs = do
    let binderMetaMap = IntMap.fromList [(getNodeId bv, meta) | (bv, meta) <- binderMetas]
        binderArgMap = IntMap.fromList [(getNodeId bv, arg) | (bv, arg) <- binderArgs]
    foldM
        (\(cmapAcc, intAcc, frontierAcc) (bv, meta) -> do
            mbBound <- lookupVarBound bv
            case mbBound of
                Nothing -> pure (cmapAcc, intAcc, frontierAcc)
                Just bnd ->
                    case IntMap.lookup (getNodeId bnd) binderMetaMap of
                        Just bndMeta -> do
                            setVarBound meta (Just bndMeta)
                            case IntMap.lookup (getNodeId bv) binderArgMap of
                                Just arg -> setVarBound arg (Just bndMeta)
                                Nothing -> pure ()
                            pure (cmapAcc, intAcc, frontierAcc)
                        Nothing -> do
                            (bndCopy, cmapB, intB, frontierB) <- instantiateSchemeWithTrace bnd binderMetas
                            setVarBound meta (Just bndCopy)
                            case IntMap.lookup (getNodeId bv) binderArgMap of
                                Just arg -> setVarBound arg (Just bndCopy)
                                Nothing -> pure ()
                            pure (IntMap.union cmapAcc cmapB, IntSet.union intAcc intB, IntSet.union frontierAcc frontierB)
        )
        (IntMap.empty, IntSet.empty, IntSet.empty)
        binderMetas

-- Copying helpers (`instantiateScheme*` + binding fixes) live in
-- `MLF.Constraint.Presolution.Copy`.
