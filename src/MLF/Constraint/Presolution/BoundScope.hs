{- |
Module      : MLF.Constraint.Presolution.BoundScope
Description : Pure construction-time scope repair for variable lower bounds

This owner-local module keeps the paper invariant @a >= sigma@ in the binding
tree itself.  It is deliberately pure so both the presolution state foundation
and higher-level operations can build a candidate tree, validate/repair it,
and commit once.
-}
module MLF.Constraint.Presolution.BoundScope
    ( raiseFreeBoundFrontierToVariableScope
    , raiseFreeBoundFrontierToVariableScopeForEdge
    , repairAllVarBoundScopes
    , changedBindParentRefs
    ) where

import Control.Monad (filterM, foldM)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Adjustment as BindingAdjustment
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Traversal as Traversal
import MLF.Constraint.Types.Graph

data BoundScopeRepairMode
    = InstallBound
    | WitnessedInstallBound
    | FollowScopeMove

-- | Validate and scope a newly installed lower bound.  Installation is
-- conservative: it never invents a cross-sibling Raise.
raiseFreeBoundFrontierToVariableScope
    :: (NodeId -> NodeId)
    -> NodeId
    -> NodeId
    -> Constraint p
    -> Either BindingError (Constraint p, [NodeId])
raiseFreeBoundFrontierToVariableScope canonical variable0 boundRoot0 c0 = do
    raiseFreeBoundFrontierToVariableScopeWith
        InstallBound
        canonical
        variable0
        boundRoot0
        c0

-- | Scope a lower bound installed while constructing an instantiation-edge
-- witness.  Unlike an ordinary late installation, this operation may Raise a
-- sibling frontier to the LCA because the caller records the returned nodes as
-- explicit @OpRaise@ operations.
raiseFreeBoundFrontierToVariableScopeForEdge
    :: (NodeId -> NodeId)
    -> NodeId
    -> NodeId
    -> Constraint p
    -> Either BindingError (Constraint p, [NodeId])
raiseFreeBoundFrontierToVariableScopeForEdge canonical variable0 boundRoot0 c0 =
    raiseFreeBoundFrontierToVariableScopeWith
        WitnessedInstallBound
        canonical
        variable0
        boundRoot0
        c0

raiseFreeBoundFrontierToVariableScopeWith
    :: BoundScopeRepairMode
    -> (NodeId -> NodeId)
    -> NodeId
    -> NodeId
    -> Constraint p
    -> Either BindingError (Constraint p, [NodeId])
raiseFreeBoundFrontierToVariableScopeWith mode canonical variable0 boundRoot0 c0 = do
    bindParents <- Binding.canonicalizeBindParentsUnder canonical c0
    let cCanonical = c0 { cBindParents = bindParents }
    (cRepaired, trace) <-
        raiseFreeBoundFrontierInCanonicalTree
            mode
            canonical
            (canonical variable0)
            (canonical boundRoot0)
            cCanonical
    pure (restoreAliasBindParents canonical c0 cRepaired, trace)

-- | Re-establish the lower-bound scope invariant for every live canonical
-- bounded variable after a binding-tree edit.  A real scope move may Raise a
-- free frontier to the LCA of its old and new scopes.
repairAllVarBoundScopes
    :: (NodeId -> NodeId)
    -> Constraint p
    -> Either BindingError (Constraint p, [NodeId])
repairAllVarBoundScopes canonical c0 = do
    bindParents <- Binding.canonicalizeBindParentsUnder canonical c0
    let cCanonical = c0 { cBindParents = bindParents }
        canonicalKeys =
            IntSet.fromList
                [ getNodeId (canonical (NodeId key))
                | key <- IntMap.keys (getNodeMap (cNodes cCanonical))
                ]
        boundedVariables =
            [ (variable, canonical boundRoot)
            | key <- IntSet.toAscList canonicalKeys
            , let variable = NodeId key
            , Just TyVar {tnBound = Just boundRoot} <-
                [lookupNode variable (cNodes cCanonical)]
            ]
    (cRepaired, trace) <-
        foldM
            (\(c, trace0) (variable, boundRoot) -> do
                (c', raised) <-
                    raiseFreeBoundFrontierInCanonicalTree
                        FollowScopeMove
                        canonical
                        variable
                        boundRoot
                        c
                pure (c', trace0 ++ raised)
            )
            (cCanonical, [])
            boundedVariables
    pure (restoreAliasBindParents canonical c0 cRepaired, trace)

-- | Scope repair operates on the canonical quotient, while rewrite still
-- needs allocation-identity keys for UF aliases.  Restore those alias keys
-- with the repaired representative parent so both keys carry one coherent
-- ownership fact.
restoreAliasBindParents
    :: (NodeId -> NodeId)
    -> Constraint p
    -> Constraint p
    -> Constraint p
restoreAliasBindParents canonical original repaired =
    repaired {cBindParents = foldl' restore repairedParents aliasKeys}
  where
    originalParents = cBindParents original
    repairedParents = cBindParents repaired
    aliasKeys =
        [ key
        | key <- IntMap.keys originalParents
        , TypeRef raw <- [nodeRefFromKey key]
        , canonical raw /= raw
        ]
    restore parents key =
        case nodeRefFromKey key of
            TypeRef raw ->
                case IntMap.lookup (nodeRefKey (typeRef (canonical raw))) repairedParents of
                    Just parentInfo -> IntMap.insert key parentInfo parents
                    Nothing -> IntMap.delete key parents
            GenRef _ -> parents

raiseFreeBoundFrontierInCanonicalTree
    :: BoundScopeRepairMode
    -> (NodeId -> NodeId)
    -> NodeId
    -> NodeId
    -> Constraint p
    -> Either BindingError (Constraint p, [NodeId])
raiseFreeBoundFrontierInCanonicalTree mode canonical variable boundRoot c0 = do
    frontier <- freeBoundFrontier canonical c0 variable boundRoot
    foldM raiseFrontierNode (c0, []) frontier
  where
    raiseFrontierNode (c, trace) frontier =
        case Binding.lookupBindParent c (typeRef variable) of
            Nothing ->
                case Binding.lookupBindParent c (typeRef frontier) of
                    Nothing -> Right (c, trace)
                    Just _ -> do
                        (c', raised) <-
                            BindingAdjustment.harmonizeBindParentsWithTrace
                                (TypeRefTag frontier)
                                (TypeRefTag variable)
                                c
                        Right (c', trace ++ raised)
            Just (target, _) -> do
                path <- Binding.bindingPathToRoot c (typeRef frontier)
                if target `elem` path
                    then do
                        (c', raised) <-
                            case
                                BindingAdjustment.raiseToParentWithCount
                                    (TypeRefTag frontier)
                                    target
                                    c
                            of
                                Left OperationOnLockedNode {} ->
                                    Left (FreeBoundFrontierLocked variable frontier)
                                other -> other
                        Right (c', trace ++ raised)
                    else
                        case Binding.lookupBindParent c (typeRef frontier) of
                            Nothing -> Right (c, trace)
                            Just (currentParent, _) -> do
                                targetPath <- Binding.bindingPathToRoot c target
                                if currentParent `elem` targetPath
                                    then Right (c, trace)
                                    else case mode of
                                        InstallBound ->
                                            Left $
                                                FreeBoundFrontierInSiblingScope
                                                    variable
                                                    frontier
                                                    target
                                                    currentParent
                                        WitnessedInstallBound ->
                                            raiseAcrossScopes target currentParent c trace frontier
                                        FollowScopeMove -> do
                                            raiseAcrossScopes target currentParent c trace frontier

    raiseAcrossScopes target currentParent c trace frontier = do
        lca <- Binding.bindingLCA c target currentParent
        (c', raised) <-
            case
                BindingAdjustment.raiseToParentWithCount
                    (TypeRefTag frontier)
                    lca
                    c
            of
                Left OperationOnLockedNode {} ->
                    Left (FreeBoundFrontierLocked variable frontier)
                other -> other
        Right (c', trace ++ raised)

freeBoundFrontier
    :: (NodeId -> NodeId)
    -> Constraint p
    -> NodeId
    -> NodeId
    -> Either BindingError [NodeId]
freeBoundFrontier canonical c variable0 boundRoot0 = do
    let nodes = cNodes c
        variable = canonical variable0
        boundRoot = canonical boundRoot0
        reachable =
            Traversal.reachableFromWithBounds
                canonical
                (lookupNodeIn nodes)
                boundRoot
        candidates = map NodeId (IntSet.toAscList reachable)
        variableRef = typeRef variable
    freeBoundNodes <-
        filterM
            (\nid -> do
                path <- Binding.bindingPathToRoot c (typeRef nid)
                pure (variableRef `notElem` path)
            )
            candidates
    let freeSet = IntSet.fromList (map getNodeId freeBoundNodes)
        parentIsFree nid =
            case Binding.lookupBindParent c (typeRef nid) of
                Just (TypeRef parent, _) -> IntSet.member (getNodeId parent) freeSet
                _ -> False
    pure [nid | nid <- freeBoundNodes, not (parentIsFree nid)]

changedBindParentRefs :: Constraint p -> Constraint p -> IntSet.IntSet
changedBindParentRefs before after =
    IntSet.filter changed allKeys
  where
    beforeParents = cBindParents before
    afterParents = cBindParents after
    allKeys =
        IntSet.union
            (IntSet.fromAscList (IntMap.keys beforeParents))
            (IntSet.fromAscList (IntMap.keys afterParents))
    changed key =
        IntMap.lookup key beforeParents /= IntMap.lookup key afterParents
