{-# LANGUAGE DataKinds #-}
module MLF.Elab.Run.Generalize.Finalize (
    finalizeConstraint
) where

import qualified Data.IntMap.Strict as IntMap

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Types.Graph
    ( Constraint
    , NodeId(..)
    , NodeRef(..)
    , cBindParents
    , cGenNodes
    , cNodes
    , getNodeId
    , gnId
    , gnSchemes
    , nodeRefFromKey
    , nodeRefKey
    , typeRef
    )
import qualified MLF.Constraint.Types.Graph as Types
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
    , expansionConstructionParentsToIntMap
    )
import MLF.Util.IntMapUtils (keepOld)

finalizeConstraint
    :: GeneralizeEnv p
    -> Phase1Result
    -> Phase2Result
    -> Phase3Result
    -> Phase4Result
    -> (Constraint p, GaBindParents p)
finalizeConstraint env phase1 phase2 _phase3 phase4 =
    let base = geBaseConstraint env
        solvedConstraint = geSolvedConstraint env
        nodesSolved = p1NodesSolved phase1
        bindParentsBase = p2BindParentsBase phase2
        bindParentsSolved = p2BindParentsSolved phase2
        NodeMapping
            { mapBaseToSolved = baseToSolved
            , mapSolvedToBase = solvedToBase
            } = p2NodeMapping phase2
        copyOverrides = p2CopyOverrides phase2
        instCopyMap = geInstCopyMap env
        expansionConstructionParents =
            expansionConstructionParentsToIntMap
                (geExpansionConstructionPlacements env)
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
                restoreLexicalTypeParents acc =
                    IntMap.foldlWithKey'
                        (\acc' childKey (parentRef, flag) ->
                            case (nodeRefFromKey childKey, parentRef) of
                                (TypeRef childBase, TypeRef parentBase)
                                    | Just parentNode <-
                                        NodeAccess.lookupNode base parentBase
                                    , case parentNode of
                                        Types.TyForall {} -> True
                                        Types.TyMu {} -> True
                                        _ -> False ->
                                        case
                                            ( mapBaseRef (typeRef childBase)
                                            , mapBaseRef parentRef
                                            )
                                        of
                                            (Just childRef', Just parentRef') ->
                                                applyBindParent
                                                    allowBindEdge
                                                    Override
                                                    childRef'
                                                    parentRef'
                                                    flag
                                                    acc'
                                            _ -> acc'
                                _ -> acc'
                        )
                        acc
                        bindParentsBase
                solvedSource node =
                    let canonicalKey = getNodeId (canonical node)
                        rawKey = getNodeId node
                    in case IntMap.lookup canonicalKey solvedToBase of
                        Just source -> Just source
                        Nothing -> IntMap.lookup rawKey solvedToBase
                constructionCollapsesToSource node =
                    case IntMap.lookup
                        (getNodeId node)
                        expansionConstructionParents of
                        Just (TypeRef parent, _) ->
                            case (solvedSource node, solvedSource parent) of
                                (Just childSource, Just parentSource) ->
                                    childSource == parentSource
                                _ -> False
                        _ -> False
                restoreSolvedParents acc =
                    -- Phase 4 has already aligned the post-witness tree.
                    -- A copy whose construction edge still crosses two source
                    -- representatives keeps that alignment.  When chi_e's
                    -- child and parent collapse to one source representative,
                    -- however, the construction edge is administrative and
                    -- chi_p's terminal Raise/Weaken owns the final placement.
                    -- All non-copy nodes likewise retain chi_p ownership.
                    IntMap.foldlWithKey'
                        (\acc' childKey (parentRef, flag) ->
                            let childRef' = adoptRef (nodeRefFromKey childKey)
                                parentRef' = adoptRef parentRef
                                insertMode =
                                    case childRef' of
                                        TypeRef node
                                            | IntMap.member
                                                (getNodeId node)
                                                instCopyMap
                                            , not
                                                ( constructionCollapsesToSource
                                                    node
                                                ) -> SelfOrEmpty
                                        _ -> Override
                            in applyBindParent
                                allowBindEdge
                                insertMode
                                childRef'
                                parentRef'
                                flag
                                acc'
                        )
                        acc
                        bindParentsSolved
                resolveConstructionParent parentRef =
                    case parentRef of
                        GenRef _ -> adoptRef parentRef
                        TypeRef parent ->
                            case IntMap.lookup (getNodeId parent) baseToSolved of
                                Just solvedParent -> adoptRef (typeRef solvedParent)
                                Nothing -> adoptRef parentRef
                restoreMissingExpansionConstructionParents acc =
                    -- chi_p after the witness is authoritative.  The projected
                    -- chi_e path may only complete an edge that disappeared
                    -- administratively; it must never undo Raise/Weaken.
                    IntMap.foldlWithKey'
                        (\acc' childKey0 (constructionParent, constructionFlag) ->
                            let childRef = adoptRef (typeRef (NodeId childKey0))
                                parentRef = resolveConstructionParent constructionParent
                            in if okRef childRef && okRef parentRef
                                && nodeRefKey childRef /= nodeRefKey parentRef
                                && isUpperRef parentRef childRef
                                then
                                    applyBindParent
                                        allowBindEdge
                                        SelfOrEmpty
                                        childRef
                                        parentRef
                                        constructionFlag
                                        acc'
                                else acc'
                        )
                        acc
                        expansionConstructionParents
                bindParentsAuthoritative =
                    restoreMissingExpansionConstructionParents
                        ( restoreLexicalTypeParents
                            (restoreSolvedParents bindParentsFinalAligned'')
                        )
                genMergedOwned =
                    Types.GenNodeMap $
                        IntMap.map
                            (\gen ->
                                gen
                                    { gnSchemes =
                                        [ root
                                        | root <- gnSchemes gen
                                        , case IntMap.lookup
                                            (nodeRefKey (typeRef root))
                                            bindParentsAuthoritative of
                                            Just (GenRef gid, _) -> gid == gnId gen
                                            _ -> False
                                        ]
                                    }
                            )
                            genMerged'
                constraint0 = solvedConstraint
                    { cNodes = Types.NodeMap nodesSolved
                    , cBindParents = bindParentsAuthoritative
                    , cGenNodes = genMergedOwned
                    }
            in pruneBindParentsConstraint constraint0
        alignedMapping =
            let canonicalBase = id
                baseQuotient = Binding.quotientBindParentsContextUnder canonicalBase base
                solvedQuotient = Binding.quotientBindParentsContextUnder canonical constraintForGen
                alignOne (accSolved, accBase) gen =
                    let gid = gnId gen
                    in case
                        ( baseQuotient >>= \qbp -> Binding.boundFlexChildrenInQuotient base qbp (GenRef gid)
                        , solvedQuotient >>= \qbp -> Binding.boundFlexChildrenInQuotient constraintForGen qbp (GenRef gid)
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
                solvedToBaseAligned0' =
                    IntMap.union copyOverrides (IntMap.union solvedToBaseAligned0 qAlignSolvedToBase)
                solvedToBaseAligned =
                    IntMap.filter
                        (\baseN ->
                            case NodeAccess.lookupNode base baseN of
                                Just _ -> True
                                Nothing -> False
                        )
                        solvedToBaseAligned0'
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
                , gaAnnotationNodeRedirects = geRedirects env
                , gaBaseToSolved = mapBaseToSolved alignedMapping
                , gaSolvedToBase = mapSolvedToBase alignedMapping
                , gaRestoredSchemeRootTargets = p1RestoredSchemeRootTargets phase1
                , gaExpansionConstructionPlacements = geExpansionConstructionPlacements env
                }
            )
