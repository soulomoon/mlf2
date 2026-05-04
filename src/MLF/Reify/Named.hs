{-# LANGUAGE GADTs #-}
module MLF.Reify.Named (
    namedNodes,
    softenedBindParentsUnder
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Binding.Tree (canonicalizeBindParentsUnder)
import MLF.Constraint.Presolution.View (PresolutionView(..))
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
import MLF.Constraint.Types.Presolution
import MLF.Util.ElabError (ElabError, bindingToElab)

softenedBindParentsUnder :: (NodeId -> NodeId) -> Constraint -> Either ElabError BindParents
softenedBindParentsUnder canonical constraint = do
    bindParents0 <- bindingToElab (canonicalizeBindParentsUnder canonical constraint)
    pure (softenBindParents canonical (cWeakenedVars constraint) bindParents0)

softenBindParents :: (NodeId -> NodeId) -> IntSet.IntSet -> BindParents -> BindParents
softenBindParents canonical weakened =
    let softenOne childKey (parent, flag) =
            case (flag, nodeRefFromKey childKey) of
                (BindRigid, TypeRef childN)
                    | IntSet.member (getNodeId (canonical childN)) weakened ->
                        (parent, BindFlex)
                _ -> (parent, flag)
    in IntMap.mapWithKey softenOne

namedNodes :: PresolutionView -> Either ElabError IntSet.IntSet
namedNodes presolutionView = do
    let constraint = pvConstraint presolutionView
        canonical = pvCanonical presolutionView
        nodes = cNodes constraint
    bindParents <- softenedBindParentsUnder canonical constraint
    pure . IntSet.fromList $
        [ getNodeId childC
        | child <- namingRootChildren bindParents
        , let childC = canonical child
        , isNamedNode nodes childC
        ]
  where
    isNamedNode nodes nid =
        case lookupNodeIn nodes nid of
            Just TyVar{} -> True
            _ -> False

namingRootChildren :: BindParents -> [NodeId]
namingRootChildren bindParents =
    [ child
    | (childKey, (parent, _flag)) <- IntMap.toList bindParents
    , GenRef _ <- [parent]
    , TypeRef child <- [nodeRefFromKey childKey]
    ]
