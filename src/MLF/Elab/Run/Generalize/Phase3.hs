module MLF.Elab.Run.Generalize.Phase3 (
    computeBindParentsBase
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Graph
    ( NodeId(..)
    , NodeRef(..)
    , cNodes
    , getNodeId
    , nodeRefFromKey
    , nodeRefKey
    , typeRef
    )
import qualified MLF.Constraint.Types.Graph as Types
import MLF.Elab.Run.Debug (debugGaScope)
import MLF.Elab.Run.Generalize.Common
    ( applyBindParent
    , mkIsUpperRef
    , mkOkRef
    )
import MLF.Elab.Run.Generalize.Types
    ( GeneralizeEnv(..)
    , InsertMode(..)
    , Phase1Result(..)
    , Phase2Result(..)
    , Phase3Result(..)
    )

computeBindParentsBase :: GeneralizeEnv -> Phase1Result -> Phase2Result -> Phase3Result
computeBindParentsBase env phase1 phase2 =
    let solvedConstraint = geSolvedConstraint env
        nodesSolved = p1NodesSolved phase1
        adoptRef = geAdoptRef env
        applyRedirectsToRef = geApplyRedirectsToRef env
        instCopyNodes = geInstCopyNodes env
        instCopyMap = geInstCopyMap env
        genMerged = p2GenMerged phase2
        bindParentsBase = p2BindParentsBase phase2
        bindParentsSolved = p2BindParentsSolved phase2
        debug msg = debugGaScope (geTraceConfig env) ("constraintForGeneralization: " ++ msg)
        upperConstraint = solvedConstraint { cNodes = Types.NodeMap nodesSolved }
        okRef = mkOkRef nodesSolved genMerged
        isUpperRef = mkIsUpperRef upperConstraint
        wasRedirected ref =
            case ref of
                TypeRef nid ->
                    let ref' = applyRedirectsToRef ref
                    in nodeRefKey ref' /= nodeRefKey (TypeRef nid)
                GenRef _ -> False
        allowBindEdge childRef parentRef =
            okRef childRef
                && okRef parentRef
                && nodeRefKey childRef /= nodeRefKey parentRef
                && isUpperRef parentRef childRef
        insertBindParentBase acc childKey parentRef flag =
            let childRef = nodeRefFromKey childKey
                childRef' = adoptRef childRef
                parentRef' = adoptRef parentRef
                allowParent =
                    case parentRef of
                        TypeRef _ -> True
                        GenRef _ -> isUpperRef parentRef' childRef'
            in if allowParent
                then applyBindParent allowBindEdge KeepOld childRef' parentRef' flag acc
                else acc

        insertBindParentSolved acc childKey parentRef flag =
            let childRef = nodeRefFromKey childKey
                childRef' = adoptRef childRef
                parentRef' = adoptRef parentRef
                childKey' = nodeRefKey childRef'
                existing = IntMap.lookup childKey' acc
                childIsCopy =
                    case childRef' of
                        TypeRef nid -> IntSet.member (getNodeId nid) instCopyNodes
                        GenRef _ -> False
                existingSelf =
                    case existing of
                        Just (parentExisting, _) -> nodeRefKey parentExisting == childKey'
                        Nothing -> False
                wasRed = wasRedirected childRef
                allowed = allowBindEdge childRef' parentRef'
                inserted = applyBindParent allowBindEdge SelfOrEmpty childRef' parentRef' flag acc
                insertLabel =
                    case existing of
                        Just _ | childIsCopy && existingSelf -> Just "override copy"
                        Nothing -> Just "fill"
                        Just _ | existingSelf -> Just "override self-parent"
                        _ -> Nothing
                dbgInsert msg result =
                    debug ("bind-parent " ++ msg ++ " child=" ++ show childRef'
                           ++ " parent=" ++ show parentRef'
                           ++ " redirected=" ++ show wasRed)
                          result
            in case insertLabel of
                Just label
                    | allowed
                    , IntMap.lookup childKey' inserted == Just (parentRef', flag) ->
                        dbgInsert label inserted
                _ -> inserted

        bindParentsBase' =
            IntMap.foldlWithKey'
                (\acc childKey (parentRef, flag) ->
                    insertBindParentBase acc childKey parentRef flag
                )
                IntMap.empty
                bindParentsBase
        bindParents' =
            IntMap.foldlWithKey'
                (\acc childKey (parentRef, flag) ->
                    insertBindParentSolved acc childKey parentRef flag
                )
                bindParentsBase'
                bindParentsSolved
        bindParentsWithCopies =
            IntMap.foldlWithKey'
                (\acc copyKey baseN ->
                    let childRef = typeRef (NodeId copyKey)
                        childRef' = adoptRef childRef
                    in case IntMap.lookup (nodeRefKey (typeRef baseN)) bindParentsBase of
                        Just (parentRef, flag) ->
                            let parentRef' = adoptRef parentRef
                            in applyBindParent allowBindEdge KeepOld childRef' parentRef' flag acc
                        Nothing -> acc
                )
                bindParents'
                instCopyMap
    in Phase3Result
        { p3BindParentsBaseAdjusted = bindParentsBase'
        , p3BindParentsWithCopies = bindParentsWithCopies
        }
