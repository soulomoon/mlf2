module MLF.Elab.SourceBinder.TestSupport
  ( orderSourceProjectedSchemeBindersForTest,
    publishSourceBinderOrderFromProvenanceForTest,
    resolveConstructionSourceBindersInSchemeInfoForTest,
    resolveConstructionSourceBindersInSchemeInfoExceptForTest,
    resolveConstructionSourceBindersInTypeForTest,
    resolveConstructionSourceBindersInTypeExceptForTest,
    resolveConstructionSourceBindersInTypeAtExpectedForTest,
    resolveConstructionSourceBindersInPacketAtExpectedForTest,
    sourceBinderConstructionRenamesForTest,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph (NodeId)
import MLF.Elab.SourceBinder
  ( orderSourceProjectedSchemeBinders,
    publishSourceBinderOrderFromProvenance,
    resolveConstructionSourceBindersInSchemeInfo,
    resolveConstructionSourceBindersInSchemeInfoExcept,
    resolveConstructionSourceBindersInType,
    resolveConstructionSourceBindersInTypeExcept,
    resolveConstructionSourceBindersInTypeAtExpected,
    resolveConstructionSourceBindersInPacketAtExpected,
    sourceBinderConstructionRenames,
  )
import MLF.Elab.Types
  ( ElabScheme,
    ElabType,
    SchemeInfo,
    TypeBinderIdentity,
    TypeBinderRef,
  )

orderSourceProjectedSchemeBindersForTest
  :: String
  -> ElabScheme
  -> Either String ElabScheme
orderSourceProjectedSchemeBindersForTest =
  orderSourceProjectedSchemeBinders

publishSourceBinderOrderFromProvenanceForTest
  :: IntMap.IntMap TypeBinderRef
  -> SchemeInfo
  -> SchemeInfo
publishSourceBinderOrderFromProvenanceForTest =
  publishSourceBinderOrderFromProvenance

resolveConstructionSourceBindersInSchemeInfoForTest
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> SchemeInfo
  -> Either String SchemeInfo
resolveConstructionSourceBindersInSchemeInfoForTest =
  resolveConstructionSourceBindersInSchemeInfo

resolveConstructionSourceBindersInSchemeInfoExceptForTest
  :: Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> SchemeInfo
  -> Either String SchemeInfo
resolveConstructionSourceBindersInSchemeInfoExceptForTest =
  resolveConstructionSourceBindersInSchemeInfoExcept

resolveConstructionSourceBindersInTypeForTest
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> Either String ElabType
resolveConstructionSourceBindersInTypeForTest =
  resolveConstructionSourceBindersInType

resolveConstructionSourceBindersInTypeExceptForTest
  :: Set.Set TypeBinderIdentity
  -> (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> Either String ElabType
resolveConstructionSourceBindersInTypeExceptForTest =
  resolveConstructionSourceBindersInTypeExcept

resolveConstructionSourceBindersInTypeAtExpectedForTest
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> ElabType
  -> Either String ElabType
resolveConstructionSourceBindersInTypeAtExpectedForTest =
  resolveConstructionSourceBindersInTypeAtExpected

resolveConstructionSourceBindersInPacketAtExpectedForTest
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> ElabType
  -> ElabType
  -> Either String ElabType
resolveConstructionSourceBindersInPacketAtExpectedForTest =
  resolveConstructionSourceBindersInPacketAtExpected

sourceBinderConstructionRenamesForTest
  :: (NodeId -> NodeId)
  -> IntMap.IntMap TypeBinderRef
  -> IntMap.IntMap TypeBinderRef
  -> Either String [(TypeBinderRef, TypeBinderRef)]
sourceBinderConstructionRenamesForTest =
  sourceBinderConstructionRenames
