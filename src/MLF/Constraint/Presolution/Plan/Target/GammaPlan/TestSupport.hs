module MLF.Constraint.Presolution.Plan.Target.GammaPlan.TestSupport
    ( expandSourceBinderRefsForTest
    , expandSourceBinderRefsWithPreferenceForTest
    ) where

import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Presolution.Plan.Target.GammaPlan
    ( expandSourceBinderRefs
    , expandSourceBinderRefsWithPreference
    )
import MLF.Constraint.Types.Graph (NodeId)
import MLF.Types.Elab (TypeBinderRef)
import MLF.Util.ElabError (ElabError)

expandSourceBinderRefsForTest
    :: (NodeId -> NodeId)
    -> IntMap.IntMap NodeId
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
expandSourceBinderRefsForTest = expandSourceBinderRefs

expandSourceBinderRefsWithPreferenceForTest
    :: IntMap.IntMap NodeId
    -> (NodeId -> NodeId)
    -> IntMap.IntMap NodeId
    -> IntMap.IntMap TypeBinderRef
    -> Either ElabError (IntMap.IntMap TypeBinderRef)
expandSourceBinderRefsWithPreferenceForTest = expandSourceBinderRefsWithPreference
