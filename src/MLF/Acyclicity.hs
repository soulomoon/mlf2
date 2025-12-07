{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Acyclicity
Description : Phase 3 - Acyclicity check and dependency graph construction
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the acyclicity checking phase of MLF type inference.
It builds a dependency graph between instantiation edges and verifies that
the graph is acyclic, which is required for the presolution phase to terminate.

Primary references:
  * Rémy & Yakobowski, "Graphic Type Constraints and Efficient Type
    Inference: from ML to MLF" (ICFP 2008) - §5 "Presolution"
  * Yakobowski, PhD thesis (2008) - Chapter 4

The acyclicity property ensures that instantiation edges can be processed
in topological order, avoiding circular dependencies that would make
presolution undecidable.
-}
module MLF.Acyclicity (
    -- * Main API
    checkAcyclicity,
    AcyclicityResult(..),
    CycleError(..),
    -- * Building blocks (exported for testing)
    buildDependencyGraph,
    collectReachableNodes,
    isAcyclic,
    topologicalSort,
    findCycle
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (mapMaybe)

import MLF.Types

{- Note [Phase 3: Acyclicity Check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Phase 3 sits between normalization (Phase 2) and presolution (Phase 4).
Its purpose is to verify that the remaining instantiation edges form an
acyclic dependency structure.

Paper reference: Rémy & Yakobowski, ICFP 2008, §5 "Presolution":
  "The presolution phase requires that instantiation constraints can be
   processed in a well-founded order. This is guaranteed when the
   dependency graph between instantiation edges is acyclic."

After Phase 2 normalization, the remaining InstEdges are those that couldn't
be resolved by grafting alone:
  - Var ≤ Var (neither side has structure yet)
  - Var ≤ Forall/Exp (requires presolution decisions)
  - Forall/Exp ≤ anything (expansion variable on left)

These edges have dependencies: solving one edge may affect the types that
another edge references. We build a graph where:
  - Vertices: Each InstEdge (identified by EdgeId)
  - Edges: e₁ → e₂ if solving e₁ could affect types used by e₂

Dependency criterion (§5):
  An edge e₁ depends on e₂ if the nodes reachable from e₁'s right-hand
  side overlap with nodes reachable from e₂'s left-hand side. Intuitively,
  e₂ must be solved first because its result may propagate to e₁.

If the dependency graph is acyclic, we can topologically sort the edges
and process them in order during Phase 4. If cycles exist, the constraints
represent an unsolvable recursive instantiation pattern.

For well-typed ML-style programs, the graph is always acyclic. Cycles
indicate either:
  1. A genuinely ill-typed program with recursive type requirements
  2. A pathological case that requires more sophisticated handling
-}

-- | Result of successful acyclicity check.
data AcyclicityResult = AcyclicityResult
    { arSortedEdges :: [InstEdge]
      -- ^ InstEdges in topological order (dependencies first)
    , arDepGraph :: DepGraph EdgeId
      -- ^ The computed dependency graph
    }
    deriving (Eq, Show)

-- | Error indicating a cycle was found in the dependency graph.
data CycleError = CycleError
    { ceEdgesInCycle :: [EdgeId]
      -- ^ EdgeIds forming the cycle (for diagnostics)
    , ceMessage :: String
      -- ^ Human-readable error message
    }
    deriving (Eq, Show)

-- | Check acyclicity of instantiation dependencies.
-- Returns either a cycle error or the topologically sorted edges.
checkAcyclicity :: Constraint -> Either CycleError AcyclicityResult
checkAcyclicity c =
    let depGraph = buildDependencyGraph c
        edges = cInstEdges c
    in case topologicalSort depGraph of
        Left cycleIds ->
            Left $ CycleError
                { ceEdgesInCycle = cycleIds
                , ceMessage = "Cyclic instantiation dependency detected: " ++
                              show (map getEdgeId cycleIds)
                }
        Right sortedIds ->
            -- Map sorted EdgeIds back to InstEdges
            let edgeMap = IntMap.fromList
                    [(getEdgeId (instEdgeId e), e) | e <- edges]
                sortedEdges = mapMaybe
                    (\eid -> IntMap.lookup (getEdgeId eid) edgeMap)
                    sortedIds
            in Right $ AcyclicityResult
                { arSortedEdges = sortedEdges
                , arDepGraph = depGraph
                }

{- Note [Dependency Graph Construction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We build the dependency graph by analyzing which InstEdges share type nodes.

For each pair of edges (e₁, e₂), we check:
  - Does solving e₂ affect e₁? This happens when nodes reachable from
    e₂'s left side (which will be modified by solving e₂) intersect with
    nodes reachable from e₁'s right side (which e₁ reads).

If so, e₁ depends on e₂, meaning e₂ should be processed first.

We compute reachable nodes by traversing the type graph through:
  - TyArrow: follow both tnDom and tnCod
  - TyForall: follow tnBody
  - TyExp: follow tnBody
  - TyVar, TyBase: no children to follow

The dependency edge direction is:
  e₂.left ∩ e₁.right ≠ ∅  ⟹  e₁ → e₂ (e₁ depends on e₂)

This ensures topological order processes e₂ before e₁.
-}

-- | Build the dependency graph between instantiation edges.
buildDependencyGraph :: Constraint -> DepGraph EdgeId
buildDependencyGraph c =
    let edges = cInstEdges c
        nodes = cNodes c

        -- For each edge, compute reachable nodes from left and right
        edgeInfo :: [(InstEdge, IntSet, IntSet)]
        edgeInfo = [(e, reachLeft e, reachRight e) | e <- edges]
          where
            reachLeft e = collectReachableNodes nodes (instLeft e)
            reachRight e = collectReachableNodes nodes (instRight e)

        -- Build adjacency list: for each edge e₁, find edges e₂ such that
        -- e₁ depends on e₂ (i.e., e₂.left ∩ e₁.right ≠ ∅)
        buildAdj :: IntMap [EdgeId]
        buildAdj = foldl' addEdgeDeps IntMap.empty edgeInfo
          where
            addEdgeDeps acc (e1, _left1, right1) =
                let e1Id = getEdgeId (instEdgeId e1)
                    -- Find all e2 where e2.left intersects e1.right
                    deps = [instEdgeId e2
                           | (e2, left2, _right2) <- edgeInfo
                           , instEdgeId e2 /= instEdgeId e1  -- No self-loops
                           , not (IntSet.null (IntSet.intersection left2 right1))
                           ]
                in IntMap.insert e1Id deps acc

        vertices = map instEdgeId edges
    in DepGraph
        { dgVertices = vertices
        , dgEdges = buildAdj
        }

-- | Collect all NodeIds reachable from a given node by traversing the type graph.
collectReachableNodes :: IntMap TyNode -> NodeId -> IntSet
collectReachableNodes nodes start = go IntSet.empty [start]
  where
    go :: IntSet -> [NodeId] -> IntSet
    go visited [] = visited
    go visited (nid:rest)
        | IntSet.member (getNodeId nid) visited = go visited rest
        | otherwise =
            let visited' = IntSet.insert (getNodeId nid) visited
                children = case IntMap.lookup (getNodeId nid) nodes of
                    Nothing -> []
                    Just node -> getChildren node
            in go visited' (children ++ rest)

    getChildren :: TyNode -> [NodeId]
    getChildren = \case
        TyVar {} -> []
        TyArrow { tnDom = dom, tnCod = cod } -> [dom, cod]
        TyBase {} -> []
        TyForall { tnBody = body } -> [body]
        TyExp { tnBody = body } -> [body]

{- Note [Topological Sort and Cycle Detection]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use a DFS-based algorithm that simultaneously:
  1. Detects cycles (by finding back edges to gray nodes)
  2. Produces a topological order (by recording finish times)

The algorithm uses three colors:
  - White: Not yet visited
  - Gray: Currently being processed (on the DFS stack)
  - Black: Completely processed

A back edge to a gray node indicates a cycle. If no cycles are found,
nodes are added to the result list in reverse finish order, which gives
a valid topological ordering.

For cycle detection, we track the current DFS path. When a back edge is
found, we extract the cycle from the path.
-}

-- | Color for DFS traversal.
data Color = White | Gray | Black
    deriving (Eq, Show)

-- | Check if the dependency graph is acyclic.
isAcyclic :: DepGraph EdgeId -> Bool
isAcyclic g = case topologicalSort g of
    Left _ -> False
    Right _ -> True

-- | Topologically sort the dependency graph.
-- Returns Left with cycle edges if a cycle is found,
-- or Right with sorted vertices if acyclic.
-- The sorted list has dependencies first: if A depends on B, then B comes before A.
topologicalSort :: DepGraph EdgeId -> Either [EdgeId] [EdgeId]
topologicalSort g =
    let vertices = dgVertices g

        -- Initial state: all vertices white
        initialColors = IntMap.fromList [(getEdgeId v, White) | v <- vertices]

        -- DFS from all unvisited vertices
        result = foldl' visitIfWhite (Right (initialColors, [])) vertices
    in case result of
        Left cycleEdges -> Left cycleEdges
        -- Reverse because DFS post-order gives reverse topological order
        Right (_, sorted) -> Right (reverse sorted)
  where
    visitIfWhite :: Either [EdgeId] (IntMap Color, [EdgeId])
                 -> EdgeId
                 -> Either [EdgeId] (IntMap Color, [EdgeId])
    visitIfWhite (Left cycleEdges) _ = Left cycleEdges
    visitIfWhite (Right (colors, sorted)) v =
        case IntMap.lookup (getEdgeId v) colors of
            Just White -> dfs colors sorted [v] v
            _ -> Right (colors, sorted)

    -- DFS with path tracking for cycle detection
    dfs :: IntMap Color -> [EdgeId] -> [EdgeId] -> EdgeId
        -> Either [EdgeId] (IntMap Color, [EdgeId])
    dfs colors sorted path v =
        let colors' = IntMap.insert (getEdgeId v) Gray colors
            neighbors = IntMap.findWithDefault [] (getEdgeId v) (dgEdges g)
        in processNeighbors colors' sorted path v neighbors

    processNeighbors :: IntMap Color -> [EdgeId] -> [EdgeId] -> EdgeId -> [EdgeId]
                     -> Either [EdgeId] (IntMap Color, [EdgeId])
    processNeighbors colors sorted _path v [] =
        -- All neighbors processed, mark v as black and add to sorted
        let colors' = IntMap.insert (getEdgeId v) Black colors
        in Right (colors', v : sorted)
    processNeighbors colors sorted path v (n:ns) =
        case IntMap.lookup (getEdgeId n) colors of
            Just Gray ->
                -- Back edge found! Extract cycle from path
                Left (extractCycle path n)
            Just Black ->
                -- Already fully processed, continue
                processNeighbors colors sorted path v ns
            Just White ->
                -- Recurse into unvisited neighbor
                case dfs colors sorted (n : path) n of
                    Left cycleEdges -> Left cycleEdges
                    Right (colors', sorted') ->
                        processNeighbors colors' sorted' path v ns
            Nothing ->
                -- Should not happen for valid graph
                processNeighbors colors sorted path v ns

    extractCycle :: [EdgeId] -> EdgeId -> [EdgeId]
    extractCycle path target =
        -- Path contains nodes from current to start, find target
        target : takeWhile (/= target) path

-- | Find a cycle in the dependency graph, if one exists.
-- Returns Nothing if the graph is acyclic.
findCycle :: DepGraph EdgeId -> Maybe [EdgeId]
findCycle g = case topologicalSort g of
    Left cycleEdges -> Just cycleEdges
    Right _ -> Nothing
