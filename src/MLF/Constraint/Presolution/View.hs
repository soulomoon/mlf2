module MLF.Constraint.Presolution.View (
    PresolutionView(..),
    fromPresolutionResult,
    fromSolved
) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Canonicalization.Shared (buildCanonicalMap, equivCanonical)
import MLF.Constraint.Solve (rewriteConstraintWithUF)
import qualified MLF.Constraint.Solved as Solved
import MLF.Constraint.Types.Graph
    ( BindFlag
    , BindParents
    , Constraint(..)
    , NodeId(..)
    , NodeRef
    , TyNode(..)
    )
import MLF.Constraint.Types.Presolution (PresolutionSnapshot(..))

-- | Read-only presolution queries built directly from presolution snapshot data.
data PresolutionView = PresolutionView
    { pvConstraint :: Constraint
    , pvCanonicalMap :: IntMap NodeId
    , pvCanonical :: NodeId -> NodeId
    , pvLookupNode :: NodeId -> Maybe TyNode
    , pvLookupVarBound :: NodeId -> Maybe NodeId
    , pvLookupBindParent :: NodeRef -> Maybe (NodeRef, BindFlag)
    , pvBindParents :: BindParents
    , pvCanonicalConstraint :: Constraint
    }

fromPresolutionResult :: PresolutionSnapshot a => a -> PresolutionView
fromPresolutionResult pres =
    let constraint = snapshotConstraint pres
        uf = sanitizeSnapshotUf constraint (snapshotUnionFind pres)
        canonMap = buildCanonicalMap uf constraint
        canonical = equivCanonical canonMap
    in PresolutionView
        { pvConstraint = constraint
        , pvCanonicalMap = canonMap
        , pvCanonical = canonical
        , pvLookupNode = \nid -> NodeAccess.lookupNode constraint (canonical nid)
        , pvLookupVarBound = \nid -> NodeAccess.lookupVarBound constraint (canonical nid)
        , pvLookupBindParent = NodeAccess.lookupBindParent constraint
        , pvBindParents = cBindParents constraint
        , pvCanonicalConstraint = rewriteConstraintWithUF uf constraint
        }

fromSolved :: Solved.Solved -> PresolutionView
fromSolved solved =
    PresolutionView
        { pvConstraint = Solved.originalConstraint solved
        , pvCanonicalMap = Solved.canonicalMap solved
        , pvCanonical = Solved.canonical solved
        , pvLookupNode = Solved.lookupNode solved
        , pvLookupVarBound = Solved.lookupVarBound solved
        , pvLookupBindParent = Solved.lookupBindParent solved
        , pvBindParents = Solved.bindParents solved
        , pvCanonicalConstraint = Solved.canonicalConstraint solved
        }

sanitizeSnapshotUf :: Constraint -> IntMap NodeId -> IntMap NodeId
sanitizeSnapshotUf c =
    IntMap.mapMaybeWithKey keepLive
  where
    isLive nid = case NodeAccess.lookupNode c nid of
        Just _ -> True
        Nothing -> False
    keepLive k rep =
        let keyNode = NodeId k
        in if isLive keyNode && isLive rep && keyNode /= rep
            then Just rep
            else Nothing

