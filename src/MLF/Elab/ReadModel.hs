{-# LANGUAGE DataKinds #-}

module MLF.Elab.ReadModel
    ( ElabReadModel
        ( ermPresolutionView
        , ermSoftBindParents
        , ermSoftChildren
        , ermNamedNodes
        , ermNodes
        , ermNodesVarOnly
        , ermSchemeRootSet
        , ermSchemeGenByRoot
        , ermSchemeGenSet
        )
    , PhiReadModel
    , buildElabReadModel
    , buildPhiReadModel
    , phiReadModelElabReadModel
    , readModelPresolutionView
    ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Binding.Tree (checkBindingTree, checkNoGenFallback)
import MLF.Constraint.Presolution.View (PresolutionView(..))
import MLF.Constraint.Types.Graph
    ( BindFlag
    , BindParents
    , Constraint(..)
    , GenNode(..)
    , GenNodeId(..)
    , NodeMap
    , TyNode(..)
    , cNodes
    , fromListNode
    , getGenNodeId
    , getGenNodeMap
    , getNodeId
    , nodeRefKey
    , toListNode
    )
import MLF.Reify.Named
    ( namedNodesFromSoftParents
    , softenCanonicalBindParentsUnder
    )
import MLF.Util.ElabError (ElabError, bindingToElab)

data ElabReadModel p = ElabReadModel
    { ermPresolutionView :: PresolutionView p
    , ermSoftBindParents :: BindParents
    , ermSoftChildren :: IntMap.IntMap [(Int, BindFlag)]
    , ermNamedNodes :: IntSet.IntSet
    , ermNodes :: NodeMap TyNode
    , ermNodesVarOnly :: NodeMap TyNode
    , ermSchemeRootSet :: IntSet.IntSet
    , ermSchemeGenByRoot :: IntMap.IntMap GenNodeId
    , ermSchemeGenSet :: IntSet.IntSet
    , ermPhiReadiness :: Either ElabError PhiReadiness
    }

-- | Proof that the read model satisfies the additional global condition used
-- by Phi translation.  Ordinary reification does not require this condition,
-- so it is a separate capability rather than an invariant of every read model.
data PhiReadiness = PhiReady

newtype PhiReadModel p = PhiReadModel (ElabReadModel p)

buildElabReadModel :: PresolutionView p -> Either ElabError (ElabReadModel p)
buildElabReadModel presolutionView = do
    let softBindParents =
            softenCanonicalBindParentsUnder
                canonical
                canonicalConstraint
                (cBindParents canonicalConstraint)
    let softChildren = bindParentChildren softBindParents
        namedSet = namedNodesFromSoftParents canonical canonicalConstraint softBindParents
    pure
        ElabReadModel
            { ermPresolutionView = presolutionView
            , ermSoftBindParents = softBindParents
            , ermSoftChildren = softChildren
            , ermNamedNodes = namedSet
            , ermNodes = cNodes originalConstraint
            , ermNodesVarOnly =
                fromListNode
                    [ (nid, node)
                    | (nid, node) <- toListNode (cNodes originalConstraint)
                    , isTyVar node
                    ]
            , ermSchemeRootSet = schemeRootSet
            , ermSchemeGenByRoot = schemeGenByRoot
            , ermSchemeGenSet = schemeGenSet
            , ermPhiReadiness = phiReadiness
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
    -- This proof is shared by every extraction from the immutable read model.
    -- Both binding-tree validity and no-Gen-fallback are Phi preconditions,
    -- not requirements for every read-only reification query. Keeping the
    -- capability separate lets those queries inspect intermediate graphs that
    -- Phi must reject.
    phiReadiness =
        bindingToElab (checkBindingTree originalConstraint)
            *> bindingToElab (checkNoGenFallback originalConstraint)
            *> pure PhiReady

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

readModelPresolutionView :: ElabReadModel p -> PresolutionView p
readModelPresolutionView = ermPresolutionView

buildPhiReadModel :: ElabReadModel p -> Either ElabError (PhiReadModel p)
buildPhiReadModel readModel = do
    _ <- ermPhiReadiness readModel
    pure (PhiReadModel readModel)

phiReadModelElabReadModel :: PhiReadModel p -> ElabReadModel p
phiReadModelElabReadModel (PhiReadModel readModel) = readModel
