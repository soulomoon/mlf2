module MLF.Elab.Run.Generalize.Finalize (
    finalizeConstraint
) where

import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Types
    ( Constraint
    , NodeId(..)
    , NodeRef(..)
    , cBindParents
    , cGenNodes
    , cNodes
    , getNodeId
    , gnId
    , nodeRefFromKey
    , nodeRefKey
    , typeRef
    )
import qualified MLF.Constraint.Types as Types
import MLF.Elab.Generalize (GaBindParents(..))
import MLF.Elab.Run.Debug (debugGaScope)
import MLF.Elab.Run.Generalize.Common
    ( applyBindParent
    , mkAllowBindEdge
    , mkIsUpperRef
    , mkOkRef
    )
import MLF.Elab.Run.Generalize.Constraint (pruneBindParentsConstraint)
import MLF.Elab.Run.Generalize.Types
    ( GeneralizeEnv(..)
    , InsertMode(..)
    , NodeMapping(..)
    , Phase1Result(..)
    , Phase2Result(..)
    , Phase3Result(..)
    , Phase4Result(..)
    )
import MLF.Util.IntMapUtils (keepOld)

finalizeConstraint
    :: GeneralizeEnv
    -> Phase1Result
    -> Phase2Result
    -> Phase3Result
    -> Phase4Result
    -> (Constraint, GaBindParents)
finalizeConstraint env phase1 phase2 _phase3 phase4 =
    let base = geBaseConstraint env
        solvedConstraint = geSolvedConstraint env
        nodesSolved = p1NodesSolved phase1
        bindParentsBase = p2BindParentsBase phase2
        NodeMapping { mapBaseToSolved = baseToSolved } = p2NodeMapping phase2
        copyOverrides = p2CopyOverrides phase2
        instCopyMap = geInstCopyMap env
        canonical = geCanonical env
        adoptRef = geAdoptRef env
        bindParentsFinalAligned = p4BindParentsFinalAligned phase4
        genMerged' = p4GenMerged phase4
        schemeRootsMerged' = p4SchemeRootsMerged phase4
        rootGenIdBase = p4RootGenIdBase phase4
        debug msg = debugGaScope (geTraceConfig env) ("constraintForGeneralization: " ++ msg)
        upperConstraint = solvedConstraint { cNodes = Types.NodeMap nodesSolved }
        okRef = mkOkRef nodesSolved genMerged'
        isUpperRef = mkIsUpperRef upperConstraint
        allowBindEdge = mkAllowBindEdge okRef isUpperRef
        mapBaseRef ref =
            case ref of
                GenRef gid -> Just (GenRef gid)
                TypeRef nid ->
                    case IntMap.lookup (getNodeId nid) baseToSolved of
                        Just solvedNid -> Just (TypeRef (canonical solvedNid))
                        Nothing ->
                            if IntMap.member (getNodeId nid) nodesSolved
                                then Just (TypeRef (canonical nid))
                                else Nothing
        constraintForGen =
            let restoreTypeParents acc =
                    IntMap.foldlWithKey'
                        (\acc' childKey (parentRef, flag) ->
                            case (nodeRefFromKey childKey, parentRef) of
                                (TypeRef childBase, TypeRef _parentBase) ->
                                    case (mapBaseRef (typeRef childBase), mapBaseRef parentRef) of
                                        (Just childRef', Just parentRef') ->
                                            applyBindParent allowBindEdge SelfOrEmpty childRef' parentRef' flag acc'
                                        _ -> acc'
                                _ -> acc'
                        )
                        acc
                        bindParentsBase
                bindParentsFinalAligned' = restoreTypeParents bindParentsFinalAligned
                bindParentsFinalAligned'' =
                    IntMap.foldlWithKey'
                        (\acc copyKey baseN ->
                            let childRef = typeRef (NodeId copyKey)
                                childRef' = adoptRef childRef
                                childKey' = nodeRefKey childRef'
                            in case IntMap.lookup (nodeRefKey (typeRef baseN)) bindParentsBase of
                                Just (GenRef gid, flag)
                                    | gid == rootGenIdBase
                                    , okRef childRef' ->
                                        IntMap.insert childKey' (GenRef gid, flag) acc
                                _ -> acc
                        )
                        bindParentsFinalAligned'
                        instCopyMap
                constraint0 = solvedConstraint
                    { cNodes = Types.NodeMap nodesSolved
                    , cBindParents = bindParentsFinalAligned''
                    , cGenNodes = Types.GenNodeMap genMerged'
                    }
            in pruneBindParentsConstraint constraint0
        alignedMapping =
            let canonicalBase = id
                alignOne (accSolved, accBase) gen =
                    let gid = gnId gen
                    in case
                        ( Binding.boundFlexChildrenUnder canonicalBase base (GenRef gid)
                        , Binding.boundFlexChildrenUnder canonical constraintForGen (GenRef gid)
                        ) of
                            (Right baseBinders, Right solvedBinders) ->
                                foldl'
                                    (\(accSolved', accBase') (solvedB, baseB) ->
                                        let solvedKey = getNodeId (canonical solvedB)
                                            baseKey = getNodeId baseB
                                        in ( IntMap.insertWith keepOld solvedKey baseB accSolved'
                                           , IntMap.insertWith keepOld baseKey (canonical solvedB) accBase'
                                           )
                                    )
                                    (accSolved, accBase)
                                    (zip solvedBinders baseBinders)
                            _ -> (accSolved, accBase)
                (qAlignSolvedToBase, qAlignBaseToSolved) =
                    foldl' alignOne (IntMap.empty, IntMap.empty) (NodeAccess.allGenNodes base)
                baseToSolvedAligned = IntMap.union baseToSolved qAlignBaseToSolved
                solvedToBaseAligned0 =
                    IntMap.foldlWithKey'
                        (\acc baseKey solvedNid ->
                            let solvedKeyC = getNodeId (canonical solvedNid)
                                solvedKeyRaw = getNodeId solvedNid
                                acc' = IntMap.insertWith keepOld solvedKeyC (NodeId baseKey) acc
                            in IntMap.insertWith keepOld solvedKeyRaw (NodeId baseKey) acc'
                        )
                        IntMap.empty
                        baseToSolvedAligned
                solvedToBaseAligned =
                    IntMap.union copyOverrides (IntMap.union solvedToBaseAligned0 qAlignSolvedToBase)
            in NodeMapping
                { mapBaseToSolved = baseToSolvedAligned
                , mapSolvedToBase = solvedToBaseAligned
                }
    in debug
            ("merged gens="
                ++ show (map fst schemeRootsMerged')
                ++ " schemes="
                ++ show (map snd schemeRootsMerged')
            )
            ( constraintForGen
            , GaBindParents
                { gaBindParentsBase = bindParentsBase
                , gaBaseConstraint = base
                , gaBaseToSolved = mapBaseToSolved alignedMapping
                , gaSolvedToBase = mapSolvedToBase alignedMapping
                }
            )
