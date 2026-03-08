module MLF.Constraint.Presolution.View (
    PresolutionView(..),
    SnapshotPreparation(..),
    buildPresolutionView,
    fromPresolutionResult,
    fromSolved,
    prepareSnapshotPreparation,
    prepareSnapshotPreparationFromParts
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

-- | Shared snapshot preparation for presolution/finalize view construction.
data SnapshotPreparation = SnapshotPreparation
    { spConstraint :: Constraint
    , spSanitizedUf :: IntMap NodeId
    , spCanonicalMap :: IntMap NodeId
    , spCanonical :: NodeId -> NodeId
    }

fromPresolutionResult :: PresolutionSnapshot a => a -> PresolutionView
fromPresolutionResult pres =
    let prepared = prepareSnapshotPreparation pres
    in buildPresolutionView prepared (rewriteConstraintWithUF (spSanitizedUf prepared) (spConstraint prepared))

fromSolved :: Solved.Solved -> PresolutionView
fromSolved solved =
    let constraint = Solved.originalConstraint solved
        canonical = Solved.canonical solved
    in PresolutionView
        { pvConstraint = constraint
        , pvCanonicalMap = Solved.canonicalMap solved
        , pvCanonical = canonical
        , pvLookupNode = \nid -> NodeAccess.lookupNode constraint (canonical nid)
        , pvLookupVarBound = \nid -> NodeAccess.lookupVarBound constraint (canonical nid)
        , pvLookupBindParent = NodeAccess.lookupBindParent constraint
        , pvBindParents = cBindParents constraint
        , pvCanonicalConstraint = Solved.canonicalConstraint solved
        }

prepareSnapshotPreparation :: PresolutionSnapshot a => a -> SnapshotPreparation
prepareSnapshotPreparation pres =
    prepareSnapshotPreparationFromParts (snapshotConstraint pres) (snapshotUnionFind pres)

prepareSnapshotPreparationFromParts :: Constraint -> IntMap NodeId -> SnapshotPreparation
prepareSnapshotPreparationFromParts constraint uf =
    let ufSanitized = sanitizeSnapshotUf constraint uf
        canonMap = buildCanonicalMap ufSanitized constraint
    in SnapshotPreparation
        { spConstraint = constraint
        , spSanitizedUf = ufSanitized
        , spCanonicalMap = canonMap
        , spCanonical = equivCanonical canonMap
        }

buildPresolutionView :: SnapshotPreparation -> Constraint -> PresolutionView
buildPresolutionView prepared canonicalConstraint =
    let constraint = spConstraint prepared
        canonical = spCanonical prepared
    in PresolutionView
        { pvConstraint = constraint
        , pvCanonicalMap = spCanonicalMap prepared
        , pvCanonical = canonical
        , pvLookupNode = \nid -> NodeAccess.lookupNode constraint (canonical nid)
        , pvLookupVarBound = \nid -> NodeAccess.lookupVarBound constraint (canonical nid)
        , pvLookupBindParent = NodeAccess.lookupBindParent constraint
        , pvBindParents = cBindParents constraint
        , pvCanonicalConstraint = canonicalConstraint
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
