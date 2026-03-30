{- |
Module      : MLF.Constraint.Normalize.Merge
Description : Unification and canonicalization for normalization
Copyright   : (c) 2024
License     : BSD-3-Clause
-}
module MLF.Constraint.Normalize.Merge (
    mergeUnifyEdges,
    applyUnionFindToConstraint,
    enforcePaperShapedInstEdges
) where

import Control.Monad (when)
import Control.Monad.State.Strict (State, gets, modify')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Binding.Adjustment as BindingAdjustment
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Canonicalize as Canonicalize
import MLF.Constraint.Normalize.Internal
    ( NormalizeM
    , NormalizeState (..)
    , findRoot
    , freshNodeId
    , freshSynthExpVarNorm
    , insertNode
    , setBindParentRefNorm
    , unionNodes
    )
import qualified MLF.Constraint.Unify.Core as UnifyCore
import MLF.Constraint.Types.Graph
    ( Constraint (..)
    , InstEdge (..)
    , NodeId
    , NodeMap
    , NodeRef (..)
    , TyNode (..)
    , UnifyEdge
    , lookupNodeIn
    , nodeRefFromKey
    , nodeRefKey
    , typeRef
    )

{- Note [Merging]
~~~~~~~~~~~~~~~~~
Merging processes unification edges (=) by maintaining a union-find structure.
This is the core of the "graphical unification" algorithm for MLF types.

Paper references:
  * Rémy & Yakobowski (TLDI 2007), "A graphical representation of MLF types
    with a linear-time incremental unification algorithm" — §3 describes the
    incremental unification algorithm on graphic types.
  * Rémy & Yakobowski (ICFP 2008) §4 — refers to merging as part of the
    local transformations that put constraints into "locally-solved form."

Algorithm (from TLDI'07 §3):
  1. Find the canonical representatives of both sides using path compression.
  2. If they're the same node, the edge is redundant (already unified).
  3. If one is a variable (⊥ node) and one has structure, point the variable
     to the structured node. The paper calls this "grafting the structure."
  4. If both have structure with the same head constructor (e.g., both →),
     merge them and recursively unify their children.
  5. If both have structure with different head constructors, this is a
     type error (constructor clash).

The TLDI'07 paper proves this algorithm is:
  * Linear time: O(n · α(n)) where α is the inverse Ackermann function,
    using union-find with path compression and union-by-rank.
  * Incremental: new unification edges can be added at any time without
    reprocessing the entire constraint.
  * Sound and complete for graphic type unification.

Representative selection (from the paper):
  "We always prefer structured nodes as representatives over variables."
  This ensures that looking up a type gives the most informative view.
  In our implementation, when unifying Var with Structure, we do:
      unionNodes varId structureId  -- var points to structure

Occurs check:
  The paper notes that occurs check is handled structurally by the graph
  representation. Since nodes are shared (not copied), a cycle in the
  graph directly represents an infinite type. The algorithm detects this
  when it would create a self-loop during unification.

Implementation note:
  We process unification edges iteratively. When unifying two arrows,
  we generate new UnifyEdges for their components and add them to the
  worklist. This continues until all edges are processed or an error
  is detected.

The union-find is applied to the constraint at the end of each iteration
to update all node references to their canonical representatives.

Current implementation limitations:
  * No path compression: `findRoot` traverses without updating parent
    pointers. This is O(depth) per find rather than amortized O(α(n)).
    Could be added later for performance if needed.
  * No explicit occurs check: We rely on the graph structure to prevent
    infinite types. Self-loops are caught by `left == right` check, but
    cycles through arrow children would show as divergence. A proper
    occurs check could be added for better error messages.

See also:
  - Note [Grafting] for how InstEdges generate UnifyEdges
  - Yakobowski PhD thesis Chapter 3 for the full formal treatment
-}

-- | Process unification edges using union-find.
mergeUnifyEdges :: NormalizeM ()
mergeUnifyEdges = do
    c <- gets nsConstraint
    let edges = cUnifyEdges c
    remainingEdges <- processUnifyEdges edges
    modify' $ \s ->
        let c' = nsConstraint s
        in s { nsConstraint = c' { cUnifyEdges = remainingEdges } }

-- | Process unification edges, returning any that couldn't be resolved.
processUnifyEdges :: [UnifyEdge] -> NormalizeM [UnifyEdge]
processUnifyEdges edges =
    UnifyCore.processUnifyEdgesWith
        normalizeUnifyStrategy
        normalizeFindRoot
        normalizeLookupNode
        unionNodes
        edges

normalizeFindRoot :: NodeId -> NormalizeM NodeId
normalizeFindRoot nid = do
    uf <- gets nsUnionFind
    pure (findRoot uf nid)

normalizeLookupNode :: NodeId -> NormalizeM (Maybe TyNode)
normalizeLookupNode nid = do
    nodes <- gets (cNodes . nsConstraint)
    pure (lookupNodeIn nodes nid)

normalizeUnifyStrategy :: UnifyCore.UnifyStrategy (State NormalizeState)
normalizeUnifyStrategy = UnifyCore.UnifyStrategy
    { UnifyCore.usOccursCheck = UnifyCore.OccursCheckNone
    , UnifyCore.usTyExpPolicy = UnifyCore.TyExpAllow
    , UnifyCore.usForallArityPolicy = UnifyCore.ForallArityCheck normalizeForallArity
    , UnifyCore.usRepresentative = UnifyCore.RepresentativeChoice normalizeRepresentative
    , UnifyCore.usOnMismatch = \_ -> pure UnifyCore.KeepEdge
    }

normalizeForallArity :: NodeId -> NormalizeM (Maybe Int)
normalizeForallArity nid = do
    c0 <- gets nsConstraint
    uf <- gets nsUnionFind
    let canonical = findRoot uf
    case Binding.orderedBinders canonical c0 (typeRef nid) of
        Right bs -> pure (Just (length bs))
        Left _ -> pure Nothing

{- Note [Normalize representative choice]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Prefer keeping the *structured* node (`TyExp`) as representative so we don't
lose information (notably: λ-parameters are referenced through fresh `TyExp`
nodes created by `EVar`, and application relies on propagating those wrappers
through unification).

However, if the expansion node's body is (up to UF) the variable itself, then
unifying Var ~ (s·Var) would create a self-loop if we made Var point to the
`TyExp`. In that special case, we collapse the wrapper by making the `TyExp`
point to the variable (this corresponds to forcing `s` to be the identity
expansion).
-}
normalizeRepresentative :: NodeId -> TyNode -> NodeId -> TyNode -> NormalizeM (NodeId, NodeId)
normalizeRepresentative left leftNode right rightNode = case (leftNode, rightNode) of
    (TyVar {}, TyVar {}) -> do
        modify' $ \s ->
            let c0 = nsConstraint s
                c1 = BindingAdjustment.harmonizeBindParents (typeRef left) (typeRef right) c0
            in s { nsConstraint = c1 }
        pure (left, right)
    (TyVar {}, TyExp { tnBody = b }) -> do
        uf <- gets nsUnionFind
        let bRoot = findRoot uf b
        if bRoot == left
            then pure (right, left)
            else pure (left, right)
    (TyExp { tnBody = b }, TyVar {}) -> do
        uf <- gets nsUnionFind
        let bRoot = findRoot uf b
        if bRoot == right
            then pure (left, right)
            else pure (right, left)
    (TyVar {}, _) -> pure (left, right)
    (_, TyVar {}) -> pure (right, left)
    _ -> pure (left, right)

-- | Apply the union-find substitution to all node references in the constraint.
applyUnionFindToConstraint :: NormalizeM ()
applyUnionFindToConstraint = do
    uf <- gets nsUnionFind
    when (not (IntMap.null uf)) $
        modify' $ \s ->
            let c = nsConstraint s
                nodes' = fmap (applyToNode uf (cNodes c)) (cNodes c)
                instEdges' = Canonicalize.rewriteInstEdges (findRoot uf) (cInstEdges c)
                unifyEdges' = Canonicalize.rewriteUnifyEdges (findRoot uf) (cUnifyEdges c)
                bindParents0 = cBindParents c
                bindParents' =
                    let canonicalRef = Canonicalize.canonicalRef (findRoot uf)
                        cStruct = c { cNodes = nodes' }
                        resolveParent childRef parent0 flag0 =
                            case childRef of
                                GenRef _ ->
                                    case parent0 of
                                        GenRef _ -> Just (parent0, flag0)
                                        TypeRef _ -> Nothing
                                TypeRef _ ->
                                    let childC = canonicalRef childRef
                                        go flag parentRef =
                                            let parentC = canonicalRef parentRef
                                                ok = parentC /= childC && Binding.isUpper cStruct parentC childRef
                                            in if ok
                                                then Just (parentC, flag)
                                                else case IntMap.lookup (nodeRefKey parentRef) bindParents0 of
                                                    Nothing -> Nothing
                                                    Just (parent', flag') -> go (max flag flag') parent'
                                    in go flag0 parent0
                        insertOne bp childKey (parent0, flag0) =
                            let childRef = nodeRefFromKey childKey
                            in case resolveParent childRef parent0 flag0 of
                                Nothing -> bp
                                Just (parent, flag) ->
                                    IntMap.insertWith
                                        (\_ old -> old)
                                        childKey
                                        (parent, flag)
                                        bp
                    in IntMap.foldlWithKey' insertOne IntMap.empty bindParents0
            in s
                { nsConstraint =
                    c
                        { cNodes = nodes'
                        , cInstEdges = instEdges'
                        , cUnifyEdges = unifyEdges'
                        , cBindParents = bindParents'
                        }
                }

-- | Apply union-find to a node.
-- If the node is a variable that has been unified with a structure,
-- we replace the variable with that structure (preserving the variable's ID).
applyToNode :: IntMap NodeId -> NodeMap TyNode -> TyNode -> TyNode
applyToNode uf nodes node =
    let nid = tnId node
        rootId = findRoot uf nid
    in if nid /= rootId
        then
            case lookupNodeIn nodes rootId of
                Just rootNode ->
                    case rootNode of
                        TyVar {} -> node
                        _ -> applyToStructure uf (rootNode { tnId = nid })
                Nothing -> node
        else
            applyToStructure uf node

-- | Update children of a structure node using UF.
applyToStructure :: IntMap NodeId -> TyNode -> TyNode
applyToStructure uf node = case node of
    TyVar {} -> node
    TyBottom {} -> node
    TyArrow { tnId = nid, tnDom = dom, tnCod = cod } ->
        TyArrow { tnId = nid, tnDom = findRoot uf dom, tnCod = findRoot uf cod }
    TyBase {} -> node
    TyCon { tnId = nid, tnCon = con, tnArgs = args } ->
        TyCon { tnId = nid, tnCon = con, tnArgs = fmap (findRoot uf) args }
    TyForall { tnId = nid, tnBody = body } ->
        TyForall { tnId = nid, tnBody = findRoot uf body }
    TyMu { tnId = nid, tnBody = body } ->
        TyMu { tnId = nid, tnBody = findRoot uf body }
    TyExp { tnId = nid, tnExpVar = ev, tnBody = body } ->
        TyExp { tnId = nid, tnExpVar = ev, tnBody = findRoot uf body }

-- | Post-loop pass: force residual instantiation edges to paper-shaped form.
-- Every residual edge becomes `TyExp(s, body) ≤ τ` by wrapping non-`TyExp`
-- left nodes in a synthesized expansion node.
enforcePaperShapedInstEdges :: NormalizeM ()
enforcePaperShapedInstEdges = do
    c <- gets nsConstraint
    wrappedEdges <- mapM wrapInstEdgeLeft (cInstEdges c)
    modify' $ \st -> st { nsConstraint = (nsConstraint st) { cInstEdges = wrappedEdges } }

-- | Wrap a residual edge's left node with a fresh `TyExp` node when needed.
wrapInstEdgeLeft :: InstEdge -> NormalizeM InstEdge
wrapInstEdgeLeft edge = do
    nodes <- gets (cNodes . nsConstraint)
    case lookupNodeIn nodes (instLeft edge) of
        Just TyExp {} -> pure edge
        _ -> do
            wrapperNid <- freshNodeId
            expVar <- freshSynthExpVarNorm
            let bodyId = instLeft edge
                wrapper = TyExp { tnId = wrapperNid, tnExpVar = expVar, tnBody = bodyId }
            insertNode wrapper
            inheritWrapperBindParent bodyId wrapperNid
            pure edge { instLeft = wrapperNid }

-- | Keep synthesized wrappers on the same binding-parent chain as their body.
inheritWrapperBindParent :: NodeId -> NodeId -> NormalizeM ()
inheritWrapperBindParent bodyId wrapperId = do
    bindParents <- gets (cBindParents . nsConstraint)
    case IntMap.lookup (nodeRefKey (typeRef bodyId)) bindParents of
        Nothing -> pure ()
        Just (parent, flag) ->
            setBindParentRefNorm (typeRef wrapperId) parent flag
