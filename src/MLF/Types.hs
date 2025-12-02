module MLF.Types (
    NodeId (..),
    GNodeId (..),
    ExpVarId (..),
    EdgeId (..),
    BaseTy (..),
    TyNode (..),
    GNode (..),
    InstEdge (..),
    UnifyEdge (..),
    Constraint (..),
    Expansion (..),
    Presolution (..),
    SolverState (..),
    DepGraph (..)
) where

import Data.IntMap.Strict (IntMap)
import Data.List.NonEmpty (NonEmpty)

newtype NodeId = NodeId { getNodeId :: Int }
    deriving (Eq, Ord, Show)

newtype GNodeId = GNodeId { getGNodeId :: Int }
    deriving (Eq, Ord, Show)

newtype ExpVarId = ExpVarId { getExpVarId :: Int }
    deriving (Eq, Ord, Show)

newtype EdgeId = EdgeId { getEdgeId :: Int }
    deriving (Eq, Ord, Show)

newtype BaseTy = BaseTy { getBaseName :: String }
    deriving (Eq, Ord, Show)

-- | Graphic type node as described in the roadmap; stored as a DAG node and
-- referenced by its stable 'NodeId'.
data TyNode
    = TyVar
        { tnId :: NodeId
        , tnLevel :: GNodeId
        }
    | TyArrow
        { tnId :: NodeId
        , tnDom :: NodeId
        , tnCod :: NodeId
        }
    | TyBase
        { tnId :: NodeId
        , tnBase :: BaseTy
        }
    | TyForall
        { tnId :: NodeId
        , tnQuantLevel :: GNodeId
        , tnBody :: NodeId
        }
    | TyExp
        { tnId :: NodeId
        , tnExpVar :: ExpVarId
        , tnBody :: NodeId
        }
    deriving (Eq, Show)

-- | Generalization nodes model the levels introduced by let-generalization in
-- the graphic constraint. Each level forms a tree (parent/children) so that we
-- know which type variables may be quantified at a given scope and where that
-- scope sits relative to its ancestors.
--
-- * 'gnodeId' is the stable identifier for this level.
-- * 'gParent' points to the enclosing level (Nothing for the root g₀).
-- * 'gBinds' lists the 'TyVar' node ids whose binding level is this node; they
--   are the candidates for ∀-introduction when solving.
-- * 'gChildren' enumerates nested levels created by inner let-bindings, giving
--   the solver the forest structure it needs for scope checks.
data GNode = GNode
    { gnodeId :: GNodeId
    , gParent :: Maybe GNodeId
    , gBinds :: [NodeId]
    , gChildren :: [GNodeId]
    }
    deriving (Eq, Show)

-- | Instantiation edge (Tₗ ≤ Tᵣ) connecting a polymorphic binding to the
-- type shape it must instantiate to; Phase 4 consumes these edges when
-- computing minimal expansions.
data InstEdge = InstEdge
    { instEdgeId :: EdgeId
    , instLeft :: NodeId
    , instRight :: NodeId
    }
    deriving (Eq, Show)

data UnifyEdge = UnifyEdge
    { uniLeft :: NodeId
    , uniRight :: NodeId
    }
    deriving (Eq, Show)

-- | Snapshot of the entire graphic constraint graph produced by Phase 1. Each
-- solver phase mutates/consumes different slices of this structure, so we keep
-- them together instead of recomputing them:
--
--   * Phase 2 rewrites 'cGNodes'/'cNodes' via canonicalization rules.
--   * Phase 3 reads 'cInstEdges' to build the dependency graph.
--   * Phase 4 consumes 'cInstEdges' while extending 'cNodes'.
--   * Phase 5 consumes 'cUnifyEdges' to merge entries inside 'cNodes'.
--
-- Subsequent elaboration phases also read the same graph to reconstruct typed
-- terms, so this record is the single source of truth for the constraint state.
data Constraint = Constraint
        { cGForest :: [GNodeId]
            -- ^ Roots of the generalization forest (most programs yield a singleton [g₀]).
            --   Keeping track of the forest roots lets us traverse all scopes even when
            --   multiple disconnected components exist (e.g. for multiple top-level lets).
        , cGNodes :: IntMap GNode
            -- ^ The lookup table for G-nodes keyed by their 'GNodeId'. The solver uses
            --   this to answer queries like "what is the parent of this level?" or
            --   "which TyVar nodes are bound here?" whenever it decides whether a
            --   variable may be generalized or instantiated.
        , cNodes :: IntMap TyNode
            -- ^ The hash-consed DAG of graphic type nodes. Every structural operation
            --   (unification, grafting, elaboration) needs fast access to the canonical
            --   representation of each node, so we store them in an IntMap keyed by
            --   'NodeId'. This is the core object manipulated by Phases 2–5.
        , cInstEdges :: [InstEdge]
            -- ^ Outstanding instantiation constraints T_left ≤ T_right. Phase 4 consumes
            --   this list (usually in dependency order) to determine minimal expansions
            --   for each expansion variable and to emit follow-up unification work.
        , cUnifyEdges :: [UnifyEdge]
            -- ^ Pending monotype equalities T1 = T2 produced either during constraint
            --   generation or as a side effect of instantiation. The graphic unifier in
            --   Phase 5 processes these to merge DAG nodes and detect inconsistencies.
        }
    deriving (Eq, Show)

data Expansion
    = ExpIdentity
    | ExpForall (NonEmpty GNodeId)
    | ExpInstantiate [NodeId]
    | ExpCompose (NonEmpty Expansion)
    deriving (Eq, Show)

newtype Presolution = Presolution { getAssignments :: IntMap Expansion }
    deriving (Eq, Show)

data SolverState = SolverState
    { ssConstraint :: Constraint
    , ssPresolution :: Presolution
    , ssUnionFind :: IntMap NodeId
    , ssDepGraph :: DepGraph EdgeId
    }
    deriving (Eq, Show)

data DepGraph a = DepGraph
    { dgVertices :: [a]
    , dgEdges :: IntMap [a]
    }
    deriving (Eq, Show)
