module MLF.Elab.Elaborate.Annotation.TestSupport
  ( checkedArgumentClosedTopologyForTest,
    checkedOccurrenceSchemeInfoForTest,
    scopedAnnotationConstructionBinderRenamesForTest,
    strictReplayCheckedSchemeInfoForTest,
  )
where

import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Presolution.Base (EdgeTrace)
import MLF.Constraint.Types.Graph (NodeId)
import MLF.Elab.Elaborate.Annotation.Construction
  ( checkedArgumentClosedTopology,
    checkedOccurrenceSchemeInfo,
    scopedAnnotationConstructionBinderRenames,
    strictReplayCheckedSchemeInfo,
  )
import MLF.Elab.Types (ElabType, SchemeInfo, TypeBinderRef)

checkedArgumentClosedTopologyForTest ::
  Maybe SchemeInfo ->
  ElabType ->
  ElabType ->
  Maybe ElabType
checkedArgumentClosedTopologyForTest =
  checkedArgumentClosedTopology

checkedOccurrenceSchemeInfoForTest ::
  ElabType ->
  Maybe SchemeInfo ->
  Either String SchemeInfo
checkedOccurrenceSchemeInfoForTest =
  checkedOccurrenceSchemeInfo

scopedAnnotationConstructionBinderRenamesForTest ::
  (NodeId -> NodeId) ->
  IntMap.IntMap TypeBinderRef ->
  IntMap.IntMap TypeBinderRef ->
  ElabType ->
  Either String [(TypeBinderRef, TypeBinderRef)]
scopedAnnotationConstructionBinderRenamesForTest =
  scopedAnnotationConstructionBinderRenames

strictReplayCheckedSchemeInfoForTest ::
  IntMap.IntMap TypeBinderRef ->
  EdgeTrace ->
  SchemeInfo ->
  Either String SchemeInfo
strictReplayCheckedSchemeInfoForTest =
  strictReplayCheckedSchemeInfo
