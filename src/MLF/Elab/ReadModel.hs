{-# LANGUAGE DataKinds #-}

module MLF.Elab.ReadModel
    ( ElabReadModel(..)
    , buildElabReadModel
    , readModelPresolutionView
    ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Binding.Tree (checkBindingTree, checkNoGenFallback, checkSchemeClosureUnder)
import MLF.Constraint.Presolution.View (PresolutionView(..))
import MLF.Constraint.Types.Graph
    ( BindFlag
    , BindParents
    , Constraint(..)
    , GenNode(..)
    , GenNodeId(..)
    , NodeMap
    , NodeId
    , NodeRef(..)
    , TyNode(..)
    , cNodes
    , fromListNode
    , getGenNodeId
    , getGenNodeMap
    , getNodeId
    , lookupNodeIn
    , nodeRefFromKey
    , nodeRefKey
    , toListNode
    )
import MLF.Reify.Named (softenedBindParentsUnder)
import MLF.Util.ElabError (ElabError, bindingToElab)

data ElabReadModel p = ElabReadModel
    { ermPresolutionView :: PresolutionView p
    , ermSoftBindParents :: BindParents
    , ermSoftChildren :: IntMap.IntMap [(Int, BindFlag)]
    , ermNamedNodes :: IntSet.IntSet
    , ermNodesVarOnly :: NodeMap TyNode
    , ermSchemeRootSet :: IntSet.IntSet
    , ermSchemeGenByRoot :: IntMap.IntMap GenNodeId
    , ermSchemeGenSet :: IntSet.IntSet
    , ermPhiValidation :: Either ElabError ()
    }

buildElabReadModel :: PresolutionView p -> Either ElabError (ElabReadModel p)
buildElabReadModel presolutionView = do
    softBindParents <- softenedBindParentsUnder canonical canonicalConstraint
    let softChildren = bindParentChildren softBindParents
        namedSet = namedNodesFromSoftParents canonical originalConstraint softBindParents
    pure
        ElabReadModel
            { ermPresolutionView = presolutionView
            , ermSoftBindParents = softBindParents
            , ermSoftChildren = softChildren
            , ermNamedNodes = namedSet
            , ermNodesVarOnly =
                fromListNode
                    [ (nid, node)
                    | (nid, node) <- toListNode (cNodes originalConstraint)
                    , isTyVar node
                    ]
            , ermSchemeRootSet = schemeRootSet
            , ermSchemeGenByRoot = schemeGenByRoot
            , ermSchemeGenSet = schemeGenSet
            , ermPhiValidation = phiValidation
            }
  where
    originalConstraint = pvConstraint presolutionView
    canonicalConstraint = pvCanonicalConstraint presolutionView
    canonical = pvCanonical presolutionView
    originalGenNodes = cGenNodes originalConstraint
    canonicalGenNodesList =
        map snd (IntMap.toList (getGenNodeMap originalGenNodes))
    schemeRootSetRaw =
        IntSet.fromList
            [ getNodeId root
            | gen <- canonicalGenNodesList
            , root <- gnSchemes gen
            ]
    schemeRootSet =
        IntSet.union schemeRootSetRaw $
            IntSet.fromList
                [ getNodeId (canonical root)
                | gen <- canonicalGenNodesList
                , root <- gnSchemes gen
                ]
    schemeGenByRootRaw =
        IntMap.fromListWith
            const
            [ (getNodeId root, gnId gen)
            | gen <- canonicalGenNodesList
            , root <- gnSchemes gen
            ]
    schemeGenByRoot =
        IntMap.union schemeGenByRootRaw $
            IntMap.fromListWith
                const
                [ (getNodeId (canonical root), gnId gen)
                | gen <- canonicalGenNodesList
                , root <- gnSchemes gen
                ]
    schemeGenSet =
        IntSet.fromList
            [ getGenNodeId gid
            | gid <- IntMap.elems schemeGenByRoot
            ]
    phiValidation =
        bindingToElab (checkBindingTree originalConstraint)
            *> bindingToElab (checkNoGenFallback originalConstraint)
            *> bindingToElab (checkSchemeClosureUnder canonical originalConstraint)

    isTyVar node = case node of
        TyVar{} -> True
        _ -> False

bindParentChildren :: BindParents -> IntMap.IntMap [(Int, BindFlag)]
bindParentChildren =
    IntMap.foldlWithKey'
        ( \acc childKey (parent, flag) ->
            IntMap.insertWith (++) (nodeRefKey parent) [(childKey, flag)] acc
        )
        IntMap.empty

namedNodesFromSoftParents :: (NodeId -> NodeId) -> Constraint p -> BindParents -> IntSet.IntSet
namedNodesFromSoftParents canonical constraint bindParents =
    IntSet.fromList
        [ getNodeId childC
        | (childKey, (GenRef{}, _flag)) <- IntMap.toList bindParents
        , TypeRef child <- [nodeRefFromKey childKey]
        , let childC = canonical child
        , isNamedNode childC
        ]
  where
    nodes = cNodes constraint
    isNamedNode nid =
        case lookupNodeIn nodes nid of
            Just TyVar{} -> True
            _ -> False

readModelPresolutionView :: ElabReadModel p -> PresolutionView p
readModelPresolutionView = ermPresolutionView
