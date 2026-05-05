{-# LANGUAGE DataKinds #-}
module MLF.Constraint.Presolution.View (
    PresolutionView(..),
    SnapshotPreparation(..),
    buildPresolutionView,
    fromPresolutionResult,
    fromSolved,
    toRawPresolutionViewForLegacy,
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
    , toRawConstraintForLegacy
    )
import MLF.Constraint.Types.Phase (Phase(..))
import MLF.Constraint.Types.Presolution (PresolutionSnapshot(..))

-- | Read-only presolution queries built directly from presolution snapshot data.
data PresolutionView p = PresolutionView
    { pvConstraint :: Constraint p
    , pvCanonicalMap :: IntMap NodeId
    , pvCanonical :: NodeId -> NodeId
    , pvLookupNode :: NodeId -> Maybe TyNode
    , pvLookupVarBound :: NodeId -> Maybe NodeId
    , pvLookupBindParent :: NodeRef -> Maybe (NodeRef, BindFlag)
    , pvBindParents :: BindParents
    , pvCanonicalConstraint :: Constraint p
    }

-- | Shared snapshot preparation for presolution/finalize view construction.
data SnapshotPreparation p = SnapshotPreparation
    { spConstraint :: Constraint p
    , spSanitizedUf :: IntMap NodeId
    , spCanonicalMap :: IntMap NodeId
    , spCanonical :: NodeId -> NodeId
    }

fromPresolutionResult :: PresolutionSnapshot a => a -> PresolutionView 'Presolved
fromPresolutionResult pres =
    let prepared = prepareSnapshotPreparation pres
    in buildPresolutionView prepared (rewriteConstraintWithUF (spSanitizedUf prepared) (spConstraint prepared))

fromSolved :: Solved.Solved -> PresolutionView 'Raw
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

toRawPresolutionViewForLegacy :: PresolutionView p -> PresolutionView 'Raw
toRawPresolutionViewForLegacy view =
    PresolutionView
        { pvConstraint = toRawConstraintForLegacy (pvConstraint view)
        , pvCanonicalMap = pvCanonicalMap view
        , pvCanonical = pvCanonical view
        , pvLookupNode = pvLookupNode view
        , pvLookupVarBound = pvLookupVarBound view
        , pvLookupBindParent = pvLookupBindParent view
        , pvBindParents = pvBindParents view
        , pvCanonicalConstraint = toRawConstraintForLegacy (pvCanonicalConstraint view)
        }

prepareSnapshotPreparation :: PresolutionSnapshot a => a -> SnapshotPreparation 'Presolved
prepareSnapshotPreparation pres =
    prepareSnapshotPreparationFromParts (snapshotConstraint pres) (snapshotUnionFind pres)

prepareSnapshotPreparationFromParts :: Constraint p -> IntMap NodeId -> SnapshotPreparation p
prepareSnapshotPreparationFromParts constraint uf =
    let ufSanitized = sanitizeSnapshotUf constraint uf
        canonMap = buildCanonicalMap ufSanitized constraint
    in SnapshotPreparation
        { spConstraint = constraint
        , spSanitizedUf = ufSanitized
        , spCanonicalMap = canonMap
        , spCanonical = equivCanonical canonMap
        }

buildPresolutionView :: SnapshotPreparation p -> Constraint p -> PresolutionView p
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

sanitizeSnapshotUf :: Constraint p -> IntMap NodeId -> IntMap NodeId
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
