{-# LANGUAGE DataKinds #-}
-- |
-- Module      : MLF.Constraint.Solved
-- Description : Opaque abstraction over solved constraint graphs
-- Copyright   : (c) 2024
-- License     : BSD-3-Clause
--
-- Provides an opaque 'Solved' type backed by an equivalence-class
-- representation and exposes read-only queries over the solved constraint
-- graph. This is the single entry point for post-solve access — downstream
-- phases (elaboration, phi translation, omega) should use these queries
-- instead of reaching into solver internals directly.
--
-- `fromSolveOutput` consumes `solveUnifyWithSnapshot` output and constructs
-- the equivalence-class backend from the pre-rewrite snapshot.
module MLF.Constraint.Solved.Internal
  ( -- * Opaque type
    Solved,
    fromSolveOutput,
    fromConstraintAndUf,
    mkTestSolved,
    fromPreRewriteState,

    -- * Core queries
    canonical,
    canonicalMap,
    originalConstraint,
    canonicalConstraint,
    lookupNode,
    allNodes,
    lookupBindParent,
    bindParents,
    instEdges,
    genNodes,
    lookupVarBound,

    -- * Constraint rebuilding
    rebuildWithConstraint,

    -- * Mutation helpers
    pruneBindParentsSolved,

    -- * Extended queries
    classMembers,
    originalNode,
    originalBindParent,
    wasOriginalBinder,

    -- * Canonical-domain queries
    weakenedVars,
    isEliminatedVar,
    canonicalizedBindParents,

    -- * Validation helpers
    validateCanonicalGraphStrict,
    validateOriginalCanonicalAgreement,
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Canonicalization.Shared (buildCanonicalMap, equivCanonical, nodeIdKey)
import qualified MLF.Constraint.NodeAccess as NA
import MLF.Constraint.Solve.Finalize (finalizeConstraintWithUF, validateSolvedGraphStrict)
import MLF.Constraint.Solve.Internal (SolveOutput (..), SolveResult (..), SolveSnapshot (..))
import MLF.Constraint.Solve.Worklist (SolveError)
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Constraint.Types.Graph
  ( BindFlag (..),
    BindParents,
    BindingError,
    Constraint (..),
    EliminatedVars,
    GenNode (..),
    GenNodeMap (..),
    InstEdge,
    NodeId (..),
    NodeMap (..),
    NodeRef (..),
    PolySyms,
    TyNode (..),
    UnifyEdge,
    WeakenedVars,
    genNodeKey,
    nodeRefFromKey,
    toRawConstraintForLegacy,
  )
import Prelude hiding (lookup)

-- -----------------------------------------------------------------
-- Opaque type
-- -----------------------------------------------------------------

-- | Opaque handle to a solved constraint graph, backed by an
-- equivalence-class representation.
data SolvedBackend p
  = EquivBackend
  { ebCanonicalMap :: IntMap NodeId,
    ebCanonicalNodes :: NodeMap TyNode,
    ebCanonicalInstEdges :: [InstEdge],
    ebCanonicalUnifyEdges :: [UnifyEdge],
    ebCanonicalBindParents :: BindParents,
    ebCanonicalPolySyms :: PolySyms,
    ebCanonicalEliminatedVars :: EliminatedVars,
    ebCanonicalWeakenedVars :: WeakenedVars,
    ebCanonicalAnnEdges :: IntSet,
    ebCanonicalLetEdges :: IntSet,
    ebCanonicalGenNodes :: GenNodeMap GenNode,
    ebEquivClasses :: IntMap [NodeId],
    ebOriginalConstraint :: Constraint p
  }
  deriving (Eq, Show)

-- | Opaque solved handle. Internally wraps a 'SolvedBackend' at a fixed
-- phantom phase. The phantom parameter is erased at runtime, so the
-- 'unsafeCoerce' in the smart constructors is sound.
newtype Solved = Solved {unSolved :: SolvedBackend 'Raw}
  deriving (Eq, Show)

-- | Construct an Equiv backend directly from a constraint and
-- union-find map. The input constraint is used as both original and
-- canonical; callers should only use this when no solve replay is needed.
fromConstraintAndUf :: Constraint p -> IntMap NodeId -> Solved
fromConstraintAndUf c uf =
  let cRaw = toRawConstraintForLegacy c
      canonMap = buildCanonicalMap uf cRaw
      equivClasses = buildEquivClasses canonMap cRaw
   in Solved
        EquivBackend
          { ebCanonicalMap = canonMap,
            ebCanonicalNodes = cNodes cRaw,
            ebCanonicalInstEdges = cInstEdges cRaw,
            ebCanonicalUnifyEdges = cUnifyEdges cRaw,
            ebCanonicalBindParents = cBindParents cRaw,
            ebCanonicalPolySyms = cPolySyms cRaw,
            ebCanonicalEliminatedVars = cEliminatedVars cRaw,
            ebCanonicalWeakenedVars = cWeakenedVars cRaw,
            ebCanonicalAnnEdges = cAnnEdges cRaw,
            ebCanonicalLetEdges = cLetEdges cRaw,
            ebCanonicalGenNodes = cGenNodes cRaw,
            ebEquivClasses = equivClasses,
            ebOriginalConstraint = cRaw
          }

-- | Backward-compatible alias used by tests and legacy call sites.
mkTestSolved :: Constraint 'Raw -> IntMap NodeId -> Solved
mkTestSolved = fromConstraintAndUf

-- | Build a staged equivalence backend from snapshot-enabled solve output.
fromSolveOutput :: SolveOutput p -> Either SolveError Solved
fromSolveOutput out =
  let snapshot = soSnapshot out
   in fromPreRewriteStateStrict
        (snapUnionFind snapshot)
        (snapPreRewriteConstraint snapshot)

-- | Build a staged equivalence-backend snapshot from solver pre-rewrite state.
--
-- The input pair should come from solve after unification has converged:
-- a pre-rewrite constraint and the final union-find map.
fromPreRewriteState :: IntMap NodeId -> Constraint p -> Either SolveError Solved
fromPreRewriteState = fromPreRewriteStateStrict

-- | Strict snapshot replay used by production solve output conversion.
fromPreRewriteStateStrict :: IntMap NodeId -> Constraint p -> Either SolveError Solved
fromPreRewriteStateStrict uf preRewrite = do
  let preRewriteRaw = toRawConstraintForLegacy preRewrite
  let snapshot =
        SolveSnapshot
          { snapUnionFind = uf,
            snapPreRewriteConstraint = preRewriteRaw
          }
  replayed <- finalizeConstraintWithUF (snapUnionFind snapshot) (snapPreRewriteConstraint snapshot)
  let canonMap = buildCanonicalMap (srUnionFind replayed) preRewriteRaw
      canonicalC = srConstraint replayed
      equivClasses = buildEquivClasses canonMap preRewriteRaw
  pure $
    Solved
      EquivBackend
        { ebCanonicalMap = canonMap,
          ebCanonicalNodes = cNodes canonicalC,
          ebCanonicalInstEdges = cInstEdges canonicalC,
          ebCanonicalUnifyEdges = cUnifyEdges canonicalC,
          ebCanonicalBindParents = cBindParents canonicalC,
          ebCanonicalPolySyms = cPolySyms canonicalC,
          ebCanonicalEliminatedVars = cEliminatedVars canonicalC,
          ebCanonicalWeakenedVars = cWeakenedVars canonicalC,
          ebCanonicalAnnEdges = cAnnEdges canonicalC,
          ebCanonicalLetEdges = cLetEdges canonicalC,
          ebCanonicalGenNodes = cGenNodes canonicalC,
          ebEquivClasses = equivClasses,
          ebOriginalConstraint = preRewriteRaw
        }

buildEquivClasses :: IntMap NodeId -> Constraint 'Raw -> IntMap [NodeId]
buildEquivClasses canonMap c =
  foldr addNode IntMap.empty (map tnId (NA.allNodes c))
  where
    addNode nid classes =
      let rep = equivCanonical canonMap nid
       in IntMap.insertWith (++) (nodeIdKey rep) [nid] classes

-- -----------------------------------------------------------------
-- Core queries
-- -----------------------------------------------------------------

canonicalConstraintFromBackend :: SolvedBackend 'Raw -> Constraint 'Raw
canonicalConstraintFromBackend
  EquivBackend
    { ebCanonicalNodes = nodes,
      ebCanonicalInstEdges = instEdges0,
      ebCanonicalUnifyEdges = unifyEdges0,
      ebCanonicalBindParents = bindParents0,
      ebCanonicalPolySyms = polySyms,
      ebCanonicalEliminatedVars = eliminatedVars,
      ebCanonicalWeakenedVars = weakenedVars0,
      ebCanonicalAnnEdges = annEdges,
      ebCanonicalLetEdges = letEdges,
      ebCanonicalGenNodes = genNodes0
    } =
    Constraint
      { cNodes = nodes,
        cInstEdges = instEdges0,
        cUnifyEdges = unifyEdges0,
        cBindParents = bindParents0,
        cPolySyms = polySyms,
        cEliminatedVars = eliminatedVars,
        cWeakenedVars = weakenedVars0,
        cAnnEdges = annEdges,
        cLetEdges = letEdges,
        cGenNodes = genNodes0
      }

setCanonicalConstraint :: Constraint 'Raw -> SolvedBackend 'Raw -> SolvedBackend 'Raw
setCanonicalConstraint c eb =
  eb
    { ebCanonicalNodes = cNodes c,
      ebCanonicalInstEdges = cInstEdges c,
      ebCanonicalUnifyEdges = cUnifyEdges c,
      ebCanonicalBindParents = cBindParents c,
      ebCanonicalPolySyms = cPolySyms c,
      ebCanonicalEliminatedVars = cEliminatedVars c,
      ebCanonicalWeakenedVars = cWeakenedVars c,
      ebCanonicalAnnEdges = cAnnEdges c,
      ebCanonicalLetEdges = cLetEdges c,
      ebCanonicalGenNodes = cGenNodes c
    }

-- | Chase the canonical map to the canonical representative.
canonical :: Solved -> NodeId -> NodeId
canonical (Solved EquivBackend {ebCanonicalMap = canonMap}) nid =
  equivCanonical canonMap nid

-- | The raw canonical map (equivalent to the old union-find accessor).
canonicalMap :: Solved -> IntMap NodeId
canonicalMap (Solved EquivBackend {ebCanonicalMap = cm}) = cm

-- | The original (pre-solving) constraint.  Primary accessor for all
-- post-solve operations (thesis-exact).
originalConstraint :: Solved -> Constraint 'Raw
originalConstraint (Solved EquivBackend {ebOriginalConstraint = c}) = c

-- | The canonical (post-solving) constraint.
canonicalConstraint :: Solved -> Constraint 'Raw
canonicalConstraint (Solved eb) = canonicalConstraintFromBackend eb

-- | Look up a type node, canonicalizing the id first.
lookupNode :: Solved -> NodeId -> Maybe TyNode
lookupNode s@(Solved EquivBackend {ebOriginalConstraint = c}) nid =
  NA.lookupNode c (canonical s nid)

-- | All type nodes in the solved constraint.
allNodes :: Solved -> [TyNode]
allNodes (Solved EquivBackend {ebOriginalConstraint = c}) = NA.allNodes c

-- | Look up the binding parent of a node reference.
lookupBindParent :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
lookupBindParent (Solved EquivBackend {ebOriginalConstraint = c}) ref =
  NA.lookupBindParent c ref

-- | The full bind-parents map.
bindParents :: Solved -> BindParents
bindParents (Solved EquivBackend {ebOriginalConstraint = c}) = cBindParents c

-- | All instantiation edges.
instEdges :: Solved -> [InstEdge]
instEdges (Solved EquivBackend {ebOriginalConstraint = c}) = cInstEdges c

-- | The gen-node map.
genNodes :: Solved -> GenNodeMap GenNode
genNodes (Solved EquivBackend {ebOriginalConstraint = c}) = cGenNodes c

-- | Look up the instance bound of a variable, canonicalizing the id first.
lookupVarBound :: Solved -> NodeId -> Maybe NodeId
lookupVarBound s@(Solved EquivBackend {ebOriginalConstraint = c}) nid =
  NA.lookupVarBound c (canonical s nid)

-- -----------------------------------------------------------------
-- Constraint rebuilding
-- -----------------------------------------------------------------

-- | Rebuild a Solved with a different constraint, preserving the
-- canonical map. Used by callers that modify the constraint
-- (e.g., alias insertion, canonicalization).
rebuildWithConstraint :: Solved -> Constraint p -> Solved
rebuildWithConstraint (Solved eb) c =
  Solved (setCanonicalConstraint (toRawConstraintForLegacy c) eb)

-- -----------------------------------------------------------------
-- Mutation helpers
-- -----------------------------------------------------------------

-- | Prune dead bind-parent entries from the canonical constraint.
-- Used by Pipeline.hs.
pruneBindParentsSolved :: Solved -> Solved
pruneBindParentsSolved
  ( Solved
      eb@EquivBackend
        { ebCanonicalMap = canonMap,
          ebCanonicalNodes = nodes,
          ebCanonicalGenNodes = genNodes0,
          ebCanonicalBindParents = bindParents0
        }
    ) =
    let canonicalNid = equivCanonical canonMap
        bindParentsCanonical =
          case Binding.canonicalizeBindParentsUnder canonicalNid (canonicalConstraintFromBackend eb) of
            Right bp -> bp
            Left _ -> bindParents0
        liveNodes = getNodeMap nodes
        liveGens = getGenNodeMap genNodes0
        liveRef ref =
          case ref of
            TypeRef nid -> IntMap.member (getNodeId nid) liveNodes
            GenRef gid -> IntMap.member (genNodeKey gid) liveGens
        liveChild childKey = liveRef (nodeRefFromKey childKey)
        bindParents' =
          IntMap.filterWithKey
            ( \childKey (parentRef, _flag) ->
                liveChild childKey && liveRef parentRef
            )
            bindParentsCanonical
     in Solved eb {ebCanonicalBindParents = bindParents'}

-- -----------------------------------------------------------------
-- Extended queries
-- -----------------------------------------------------------------

-- | Members of the equivalence class containing @nid@.
classMembers :: Solved -> NodeId -> [NodeId]
classMembers s@(Solved EquivBackend {ebEquivClasses = classes}) nid =
  let nidC = canonical s nid
   in IntMap.findWithDefault [] (nodeIdKey nidC) classes

-- | Look up the /original/ (pre-merge) node.
originalNode :: Solved -> NodeId -> Maybe TyNode
originalNode (Solved EquivBackend {ebOriginalConstraint = c}) nid =
  NA.lookupNode c nid

-- | Look up the /original/ (pre-merge) binding parent.
originalBindParent :: Solved -> NodeRef -> Maybe (NodeRef, BindFlag)
originalBindParent (Solved EquivBackend {ebOriginalConstraint = c}) ref =
  NA.lookupBindParent c ref

-- | Was @nid@ a binder (forall body owner) before merges?
wasOriginalBinder :: Solved -> NodeId -> Bool
wasOriginalBinder s@(Solved EquivBackend {ebOriginalConstraint = c}) nid =
  any isForall (classMembers s nid)
  where
    isForall member =
      case NA.lookupNode c member of
        Just TyForall {} -> True
        _ -> False

-- -----------------------------------------------------------------
-- Canonical-domain queries
-- -----------------------------------------------------------------

-- | Weakened variable IDs from the canonical (post-solve) constraint.
weakenedVars :: Solved -> IntSet
weakenedVars (Solved EquivBackend {ebCanonicalWeakenedVars = ws}) = ws

-- | Is the node an eliminated variable in the canonical constraint?
isEliminatedVar :: Solved -> NodeId -> Bool
isEliminatedVar (Solved EquivBackend {ebCanonicalEliminatedVars = evs}) nid =
  IntSet.member (getNodeId nid) evs

-- | Canonicalized bind parents in canonical domain (with UF/redirect collapse).
canonicalizedBindParents :: Solved -> Either BindingError BindParents
canonicalizedBindParents s =
  Binding.canonicalizeBindParentsUnder (canonical s) (canonicalConstraint s)

-- | Run strict solved-graph validation against the canonical solved view.
validateCanonicalGraphStrict :: Solved -> [String]
validateCanonicalGraphStrict s =
  validateSolvedGraphStrict
    SolveResult { srConstraint = canonicalConstraint s,
        srUnionFind = canonicalMap s
      }

-- | Check that original-domain and canonical-domain queries agree
-- for all nodes.  Returns a list of mismatch descriptions.
--
-- Used by the projection-first pipeline variant (Phase E) to detect
-- divergence between the two domains.
validateOriginalCanonicalAgreement :: Solved -> [String]
validateOriginalCanonicalAgreement s =
  [ "Node "
      ++ show (getNodeId nid)
      ++ ": original="
      ++ show origTag
      ++ " canonical="
      ++ show canonTag
  | node <- allNodes s,
    let nid = tnId node
        nidC = canonical s nid
        origNode = originalNode s nid
        canonNode = NA.lookupNode (canonicalConstraint s) nidC
        origTag = fmap tnTag origNode
        canonTag = fmap tnTag canonNode,
    origTag /= canonTag
  ]
  where
    tnTag :: TyNode -> String
    tnTag TyVar {} = "var"
    tnTag TyBottom {} = "bottom"
    tnTag TyArrow {} = "arrow"
    tnTag TyBase {} = "base"
    tnTag TyCon {} = "con"
    tnTag TyForall {} = "forall"
    tnTag TyExp {} = "exp"
    tnTag TyMu {} = "mu"
