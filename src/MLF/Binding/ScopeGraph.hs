module MLF.Binding.ScopeGraph (
    buildTypeEdgesFrom,
    buildScopeNodesFromPaths,
    rootsForScope,
) where

import Control.Monad (foldM, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import MLF.Constraint.Types

buildTypeEdgesFrom
    :: (NodeId -> Int)
    -> NodeMap TyNode
    -> IntMap.IntMap IntSet
buildTypeEdgesFrom toKey nodes =
    foldl' addOne IntMap.empty (map snd (toListNode nodes))
  where
    addOne m node =
        let parentKey = toKey (tnId node)
            childKeys = IntSet.fromList [ toKey child | child <- structuralChildrenWithBounds node ]
            childKeys' = IntSet.delete parentKey childKeys
        in if IntSet.null childKeys'
            then m
            else IntMap.insertWith IntSet.union parentKey childKeys' m

buildScopeNodesFromPaths
    :: (NodeRef -> Either BindingError [NodeRef])
    -> [Int]
    -> Either BindingError (IntMap.IntMap IntSet)
buildScopeNodesFromPaths pathLookup typeIds =
    foldM addOne IntMap.empty typeIds
  where
    addOne m nidInt = do
        path <- pathLookup (typeRef (NodeId nidInt))
        let gens = [ gid | GenRef gid <- path ]
        when (null gens) $
            Left (MissingBindParent (typeRef (NodeId nidInt)))
        pure $
            foldl'
                (\acc gid ->
                    IntMap.insertWith
                        IntSet.union
                        (getGenNodeId gid)
                        (IntSet.singleton nidInt)
                        acc
                )
                m
                gens

rootsForScope
    :: (Int -> Int)
    -> (Int -> Maybe Int)
    -> IntMap.IntMap IntSet
    -> IntSet
    -> IntSet
rootsForScope parentKey childKey typeEdges scopeSet =
    let referenced =
            IntSet.fromList
                [ cid
                | nidInt <- IntSet.toList scopeSet
                , let pkey = parentKey nidInt
                , childRawKey <- IntSet.toList (IntMap.findWithDefault IntSet.empty pkey typeEdges)
                , Just cid <- [childKey childRawKey]
                , IntSet.member cid scopeSet
                ]
    in IntSet.difference scopeSet referenced
