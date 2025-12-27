{- |
Module      : MLF.Constraint.Root
Description : Maintain a synthetic constraint root to make binding LCA total

The paper (`papers/xmlf.txt`) presents graphic constraints as rooted graphs. In
this repo, constraints may contain multiple disconnected term-DAG components.
Unification can still relate nodes across those components; to keep binding-edge
harmonization paper-faithful (Raise-to-LCA), we maintain a synthetic root node
that makes the binding LCA total on well-formed constraints.

This root is represented as a `TyRoot` node whose children are treated as
structure edges. By pointing the root at term-DAG roots and binding those
roots under it, all nodes share a common binding ancestor.
-}
module MLF.Constraint.Root (
    findConstraintRoot,
    ensureConstraintRoot,
    attachNodeToConstraintRoot,
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types

findConstraintRoot :: Constraint -> Maybe NodeId
findConstraintRoot c =
    fst <$> IntMap.foldl'
        (\acc node ->
            case acc of
                Just _ -> acc
                Nothing ->
                    case node of
                        TyRoot{ tnId = nid } -> Just (nid, ())
                        _ -> Nothing
        )
        Nothing
        (cNodes c)

-- | Ensure the constraint has a synthetic root and that all term-DAG roots are
-- children of that root and bound under it.
--
-- This is a pure “shaping” pass: it does not touch instantiation/unification
-- edges and only modifies `cNodes` (the root’s child list) and `cBindParents`
-- (binding the former roots).
ensureConstraintRoot :: Constraint -> Constraint
ensureConstraintRoot c0 =
    case findConstraintRoot c0 of
        Nothing ->
            let roots0 = Binding.computeTermDagRoots c0
            in if IntSet.size roots0 <= 1
                then c0
                else
                    let rootId = freshNodeId c0
                        roots = map NodeId (IntSet.toList roots0)
                        c1 =
                            c0
                                { cNodes = IntMap.insert (getNodeId rootId) (TyRoot rootId roots) (cNodes c0)
                                }
                        c2 =
                            foldr
                                (\nid c ->
                                    if IntMap.member (getNodeId nid) (cBindParents c)
                                        then c
                                        else Binding.setBindParent nid (rootId, BindFlex) c
                                )
                                c1
                                roots
                    in c2
        Just rootId ->
            let roots0 = Binding.computeTermDagRoots c0
                roots = filter (/= rootId) (map NodeId (IntSet.toList roots0))
                c1 = foldr (attachNodeToConstraintRoot rootId) c0 roots
            in foldr
                (\nid c ->
                    if IntMap.member (getNodeId nid) (cBindParents c)
                        then c
                        else Binding.setBindParent nid (rootId, BindFlex) c
                )
                c1
                roots

-- | Add a node as an additional structure child of the synthetic root (if any).
--
-- This is used incrementally during presolution when new disconnected roots may
-- be introduced (e.g. fresh copied subgraphs).
attachNodeToConstraintRoot :: NodeId -> NodeId -> Constraint -> Constraint
attachNodeToConstraintRoot rootId nid c0 =
    case IntMap.lookup (getNodeId rootId) (cNodes c0) of
        Just TyRoot{ tnChildren = cs } ->
            if nid `elem` cs
                then c0
                else
                    let root' = TyRoot rootId (cs ++ [nid])
                    in c0 { cNodes = IntMap.insert (getNodeId rootId) root' (cNodes c0) }
        _ -> c0

freshNodeId :: Constraint -> NodeId
freshNodeId c =
    case IntMap.lookupMax (cNodes c) of
        Nothing -> NodeId 0
        Just (k, _) -> NodeId (k + 1)
