module MLF.Elab.Elaborate.Annotation.TestSupport
  ( AnnotationSourceConstruction (..),
    checkedArgumentClosedTopologyForTest,
    checkedOccurrenceSchemeInfoForTest,
    selectAnnotationSourceConstructionForTest,
    scopedAnnotationConstructionBinderRenamesForTest,
    strictReplayCheckedSchemeInfoForTest,
  )
where

import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Presolution.Base (EdgeTrace)
import MLF.Constraint.Types.Graph (NodeId)
import MLF.Elab.Elaborate.Annotation.Construction
  ( AnnotationSourceConstruction (..),
    checkedArgumentClosedTopology,
    checkedOccurrenceSchemeInfo,
    selectAnnotationSourceConstruction,
    scopedAnnotationConstructionBinderRenames,
    strictReplayCheckedSchemeInfo,
  )
import MLF.Elab.Types
  ( ElabError,
    ElabType,
    Instantiation,
    SchemeInfo,
    TypeBinderRef,
    XmlfTerm,
  )
import MLF.Frontend.ConstraintGen.Types (BindingKey)

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

selectAnnotationSourceConstructionForTest ::
  Maybe BindingKey ->
  Maybe (XmlfTerm, Instantiation) ->
  Either ElabError (XmlfTerm, Instantiation) ->
  Either ElabError AnnotationSourceConstruction
selectAnnotationSourceConstructionForTest =
  selectAnnotationSourceConstruction

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
