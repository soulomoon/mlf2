{-# LANGUAGE LambdaCase #-}
{- |
Module      : MLF.Constraint.Presolution.Validation
Description : Validation and rigidification for translatable presolution
Copyright   : (c) 2024
License     : BSD-3-Clause

This module validates that a presolution is translatable to xMLF and performs
rigidification to prepare for translation. A translatable presolution must have:
- No inert locked nodes
- All scheme roots are rigidly bound
- All arrow nodes are rigidly bound
- All non-interior flexible children are rigidly bound

= Paper References

* Rémy & Yakobowski, "Graphic Type Constraints" (ICFP 2008) - §5.3
-}
module MLF.Constraint.Presolution.Validation (
    validateTranslatablePresolution,
    rigidifyTranslatablePresolutionM,
    structuralInterior,
    translatableWeakenedNodes,
    bindingToPres,
    bindingToPresM
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
import MLF.Constraint.Presolution.Base
import MLF.Constraint.Presolution.StateAccess (liftBindingError)
import qualified MLF.Constraint.Traversal as Traversal
import qualified MLF.Constraint.Inert as Inert
import qualified MLF.Util.IntMapUtils as IntMapUtils

-- | Validate that a constraint is a translatable presolution.
--
-- Checks:
-- 1. No inert locked nodes
-- 2. All scheme roots are rigidly bound to their gen node
-- 3. All arrow nodes are rigidly bound
-- 4. All non-interior flexible children are rigidly bound
validateTranslatablePresolution :: Constraint -> Either PresolutionError ()
validateTranslatablePresolution c0 = do
    let nodes = cNodes c0
        bindParents = cBindParents c0
        genNodes = cGenNodes c0

    locked <- bindingToPres (Inert.inertLockedNodes c0)
    let issuesLocked =
            if IntSet.null locked
                then []
                else [InertLockedNodes (map NodeId (IntSet.toList locked))]

    let issuesScheme =
            [ SchemeRootNotRigid (gnId gen) root
            | gen <- IntMap.elems (getGenNodeMap genNodes)
            , root <- gnSchemes gen
            , case IntMap.lookup (nodeRefKey (typeRef root)) bindParents of
                Just (GenRef gid, BindRigid) | gid == gnId gen -> False
                Just (GenRef gid, BindFlex) | gid == gnId gen -> True
                _ -> False
            ]

    let issuesArrow =
            [ ArrowNodeNotRigid nid
            | (nid, node) <- toListNode nodes
            , TyArrow{} <- [node]
            , case IntMap.lookup (nodeRefKey (typeRef nid)) bindParents of
                Just (_, BindRigid) -> False
                _ -> True
            ]

    let interiorByGen =
            IntMap.fromList
                [ (genNodeKey (gnId gen), structuralInterior nodes (gnSchemes gen))
                | gen <- IntMap.elems (getGenNodeMap genNodes)
                ]

    let issuesOutside =
            [ NonInteriorNodeNotRigid gid child
            | (gid, child) <- IntMapUtils.flexTypeChildrenOfGen bindParents
            , let interior =
                    IntMap.findWithDefault IntSet.empty (genNodeKey gid) interiorByGen
            , not (IntSet.member (getNodeId child) interior)
            ]

    let issues = issuesLocked ++ issuesScheme ++ issuesArrow ++ issuesOutside
    if null issues
        then pure ()
        else Left (NonTranslatablePresolution issues)

-- | Convert binding errors to presolution errors.
bindingToPres :: Either BindingError a -> Either PresolutionError a
bindingToPres = either (Left . BindingTreeError) Right

-- | Convert binding errors to presolution errors in monadic context.
bindingToPresM :: Either BindingError a -> PresolutionM a
bindingToPresM = liftBindingError

-- | Rigidify a translatable presolution by converting flexible bindings to rigid.
--
-- This prepares the constraint for translation by ensuring all necessary nodes
-- are rigidly bound:
-- 1. Weaken inert locked nodes
-- 2. Rigidify arrow nodes
-- 3. Rigidify scheme roots
-- 4. Rigidify non-interior flexible children
rigidifyTranslatablePresolutionM :: PresolutionM ()
rigidifyTranslatablePresolutionM = do
    c0 <- getConstraint
    c1 <- bindingToPresM (Inert.weakenInertLockedNodes c0)
    let nodes = cNodes c1
        genNodes = cGenNodes c1
        bindParents0 = cBindParents c1

        arrowNodes =
            [ nid
            | (nid, node) <- toListNode nodes
            , TyArrow{} <- [node]
            ]

        isNonDegenerateSchemeRoot gen root =
            case IntMap.lookup (nodeRefKey (typeRef root)) bindParents0 of
                Just (GenRef gid, _) | gid == gnId gen -> True
                _ -> False

        schemeRoots =
            [ root
            | gen <- IntMap.elems (getGenNodeMap genNodes)
            , root <- gnSchemes gen
            , isNonDegenerateSchemeRoot gen root
            ]

        rigidifyKey bp ref =
            case IntMap.lookup (nodeRefKey ref) bp of
                Just (parent, BindFlex) -> IntMap.insert (nodeRefKey ref) (parent, BindRigid) bp
                _ -> bp

        bindParents1 =
            foldl' rigidifyKey bindParents0 (map typeRef arrowNodes)
        bindParents2 =
            foldl' rigidifyKey bindParents1 (map typeRef schemeRoots)

        interiorByGen =
            IntMap.fromList
                [ (genNodeKey (gnId gen), structuralInterior nodes (gnSchemes gen))
                | gen <- IntMap.elems (getGenNodeMap genNodes)
                ]

        bindParents3 =
            foldl'
                (\bp (childKey, (parent, flag)) ->
                    case (flag, parent, nodeRefFromKey childKey) of
                        (BindFlex, GenRef gid, TypeRef child) ->
                            let interior =
                                    IntMap.findWithDefault IntSet.empty (genNodeKey gid) interiorByGen
                            in if IntSet.member (getNodeId child) interior
                                then bp
                                else IntMap.insert childKey (parent, BindRigid) bp
                        _ -> bp
                )
                bindParents2
                (IntMap.toList bindParents2)

        c2 = c1 { cBindParents = bindParents3 }

    c3 <- bindingToPresM (Inert.weakenInertLockedNodes c2)
    modifyConstraint (const c3)

-- | Compute the structural interior of a set of scheme roots.
--
-- The interior is the set of all nodes reachable from the roots by following
-- structural children and instance bounds.
structuralInterior :: NodeMap TyNode -> [NodeId] -> IntSet.IntSet
structuralInterior nodes =
    Traversal.reachableFromNodes id children
  where
    children nid =
        maybe [] structuralChildrenWithBounds (lookupNodeIn nodes nid)

-- | Compute the set of nodes that should be weakened in a translatable presolution.
--
-- This includes:
-- - Explicitly weakened variables
-- - Rigidly bound scheme roots
-- - Rigidly bound arrow nodes
-- - Rigidly bound non-interior children
translatableWeakenedNodes :: Constraint -> IntSet.IntSet
translatableWeakenedNodes c0 =
    let nodes = cNodes c0
        genNodes = cGenNodes c0
        bindParents = cBindParents c0
        isRigid key =
            case IntMap.lookup key bindParents of
                Just (_, BindRigid) -> True
                _ -> False

        schemeRoots =
            [ root
            | gen <- IntMap.elems (getGenNodeMap genNodes)
            , root <- gnSchemes gen
            , case IntMap.lookup (nodeRefKey (typeRef root)) bindParents of
                Just (GenRef gid, _) | gid == gnId gen -> True
                _ -> False
            , isRigid (nodeRefKey (typeRef root))
            ]

        arrowNodes =
            [ nid
            | (nid, node) <- toListNode nodes
            , TyArrow{} <- [node]
            , isRigid (nodeRefKey (typeRef nid))
            ]

        interiorByGen =
            IntMap.fromList
                [ (genNodeKey (gnId gen), structuralInterior nodes (gnSchemes gen))
                | gen <- IntMap.elems (getGenNodeMap genNodes)
                ]

        nonInterior =
            [ child
            | (gid, child) <- IntMapUtils.rigidTypeChildrenOfGen bindParents
            , let interior =
                    IntMap.findWithDefault IntSet.empty (genNodeKey gid) interiorByGen
            , not (IntSet.member (getNodeId child) interior)
            ]

        toKey = getNodeId
        inferred = IntSet.fromList (map toKey (schemeRoots ++ arrowNodes ++ nonInterior))
    in IntSet.union (cWeakenedVars c0) inferred
