module MLF.Elab.Run.Generalize.Phase2 (
    buildNodeMappings
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
    ( NodeId(..)
    , NodeRef(..)
    , cBindParents
    , cGenNodes
    , cNodes
    , getNodeId
    , gnSchemes
    , typeRef
    )
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.Types as Types
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Elab.Run.Generalize.Common
    ( nodeMapToIntMap
    , reachableFromWithBounds
    , isTyVarAt
    )
import MLF.Elab.Run.Generalize.Types
    ( GeneralizeEnv(..)
    , NodeMapping(..)
    , Phase1Result(..)
    , Phase2Result(..)
    )
import MLF.Util.IntMapUtils (keepOld)
import qualified MLF.Util.IntMapUtils as IntMapUtils

buildNodeMappings :: GeneralizeEnv -> Phase1Result -> Phase2Result
buildNodeMappings env phase1 =
    let solvedConstraint = geSolvedConstraint env
        base = geBaseConstraint env
        baseNodes = nodeMapToIntMap (cNodes base)
        nodesSolved = p1NodesSolved phase1
        canonical = geCanonical env
        adoptRef = geAdoptRef env
        instCopyMap = geInstCopyMap env
        genSolved = Types.getGenNodeMap (cGenNodes solvedConstraint)
        genMerged = IntMap.union (Types.getGenNodeMap (cGenNodes base)) genSolved
        bindParentsBase = cBindParents base
        bindParentsSolved = cBindParents solvedConstraint
        stickyTypeParentsBase =
            IntSet.fromList
                [ childKey
                | (childKey, _parent, _flag) <- IntMapUtils.childrenWithTypeParent bindParentsBase
                ]
        baseNamedKeys =
            IntSet.fromList
                [ childKey
                | (childKey, child) <- IntMapUtils.allTypeChildrenWithKey bindParentsBase
                , isTyVarAt baseNodes (getNodeId child)
                ]
        copyOverrides =
            IntMap.fromList
                [ (copyKeyC, baseN)
                | (copyKey, baseN) <- IntMap.toList instCopyMap
                , let copyKeyC = getNodeId (canonical (NodeId copyKey))
                , IntMap.member copyKeyC nodesSolved
                ]
        (baseToSolved, solvedToBase) =
            let schemeInteriorKeys =
                    IntSet.unions
                        [ reachableFromWithBounds baseNodes root
                        | gen <- NodeAccess.allGenNodes base
                        , root <- gnSchemes gen
                        ]
                bindParentKeys =
                    IntSet.fromList
                        ( map getNodeId (IntMapUtils.typeChildNodes bindParentsBase)
                            ++ map getNodeId (IntMapUtils.typeParentNodes bindParentsBase)
                        )
                baseKeys =
                    IntSet.unions
                        [ IntSet.fromList (IntMap.keys baseNodes)
                        , schemeInteriorKeys
                        , bindParentKeys
                        ]
                adoptNode nid0 =
                    case adoptRef (typeRef (NodeId nid0)) of
                        TypeRef nid' -> nid'
                        GenRef _ -> NodeId nid0
                chooseMapping nid0 =
                    let adopted = adoptNode nid0
                        adoptedKey = getNodeId adopted
                        baseNodeExists = IntMap.member nid0 nodesSolved
                        adoptedExists = IntMap.member adoptedKey nodesSolved
                        baseIsNamed = IntSet.member nid0 baseNamedKeys
                        baseIsEliminated = VarStore.isEliminatedVar solvedConstraint (NodeId nid0)
                    in if baseIsNamed && baseNodeExists && not baseIsEliminated
                        then NodeId nid0
                        else if adoptedExists
                            then adopted
                            else if baseNodeExists
                                then NodeId nid0
                                else adopted
                baseToSolved0 =
                    IntMap.fromList
                        [ (nid0, chooseMapping nid0)
                        | nid0 <- IntSet.toList baseKeys
                        ]
                solvedToBase0 =
                    IntMap.foldlWithKey'
                        (\acc baseKey solvedNid ->
                            let solvedKeyC = getNodeId (canonical solvedNid)
                                solvedKeyRaw = getNodeId solvedNid
                                acc' = IntMap.insertWith keepOld solvedKeyC (NodeId baseKey) acc
                            in IntMap.insertWith keepOld solvedKeyRaw (NodeId baseKey) acc'
                        )
                        IntMap.empty
                        baseToSolved0
                solvedToBase1 =
                    IntMap.union copyOverrides solvedToBase0
            in (baseToSolved0, solvedToBase1)
        nodeMapping = NodeMapping
            { mapBaseToSolved = baseToSolved
            , mapSolvedToBase = solvedToBase
            }
    in Phase2Result
        { p2GenMerged = genMerged
        , p2NodeMapping = nodeMapping
        , p2CopyOverrides = copyOverrides
        , p2BindParentsBase = bindParentsBase
        , p2BindParentsSolved = bindParentsSolved
        , p2StickyTypeParentsBase = stickyTypeParentsBase
        , p2BaseNamedKeys = baseNamedKeys
        }
