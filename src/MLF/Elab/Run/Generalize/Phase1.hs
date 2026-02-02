module MLF.Elab.Run.Generalize.Phase1 (
    restoreSchemeNodes
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
    ( NodeRef(..)
    , TyNode(..)
    , cBindParents
    , cGenNodes
    , cNodes
    , getNodeId
    , gnSchemes
    , typeRef
    )
import qualified MLF.Constraint.Types as Types
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Elab.Run.Generalize.Common
    ( nodeMapToIntMap
    , reachableFromWithBounds
    , schemeRootsOf
    )
import MLF.Elab.Run.Generalize.Types
    ( GeneralizeEnv(..)
    , Phase1Result(..)
    )
import qualified MLF.Util.IntMapUtils as IntMapUtils

restoreSchemeNodes :: GeneralizeEnv -> Phase1Result
restoreSchemeNodes env =
    let solvedConstraint = geSolvedConstraint env
        base = geBaseConstraint env
        baseNodes = nodeMapToIntMap (cNodes base)
        nodesSolved0 = nodeMapToIntMap (cNodes solvedConstraint)
        canonical = geCanonical env
        adoptRef = geAdoptRef env
        adoptNodeId = geAdoptNodeId env
        applyRedirectsToRef = geApplyRedirectsToRef env
        schemeRootsBase = schemeRootsOf (Types.getGenNodeMap (cGenNodes base))
        preferBaseVar new old =
            case (new, old) of
                (TyVar{ tnBound = Nothing }, TyVar{ tnBound = Just _ }) -> old
                (TyVar{}, TyVar{}) -> new
                _ -> old
        insertBaseVarWith adoptId adoptBound acc key =
            case IntMap.lookup key baseNodes of
                Just TyVar{ tnId = baseId, tnBound = mb } ->
                    let baseId' = adoptId baseId
                        node' = TyVar { tnId = baseId', tnBound = fmap adoptBound mb }
                    in IntMap.insertWith
                        preferBaseVar
                        (getNodeId baseId')
                        node'
                        acc
                _ -> acc
        restoreSchemeRoot acc root =
            let mbBase = do
                    TyVar{ tnBound = mb } <- IntMap.lookup (getNodeId root) baseNodes
                    bnd <- mb
                    case adoptRef (typeRef bnd) of
                        TypeRef bnd' -> Just bnd'
                        GenRef _ -> Nothing
                insertRoot bnd' =
                    IntMap.insert (getNodeId root) (TyVar { tnId = root, tnBound = Just bnd' }) acc
                fillMissing nid bnd' =
                    IntMap.insert (getNodeId root) (TyVar { tnId = nid, tnBound = Just bnd' }) acc
            in case IntMap.lookup (getNodeId root) acc of
                Nothing -> maybe acc insertRoot mbBase
                Just TyVar{ tnId = nid, tnBound = Nothing } ->
                    maybe acc (fillMissing nid) mbBase
                Just _ -> acc
        (schemeRootsBaseSet, schemeRootsAllSet) =
            let rootsSolved = schemeRootsOf (Types.getGenNodeMap (cGenNodes solvedConstraint))
                baseSet = IntSet.fromList (map getNodeId schemeRootsBase)
                solvedSet = IntSet.fromList (map (getNodeId . canonical) rootsSolved)
            in (baseSet, IntSet.union baseSet solvedSet)
        nodesSolved1 = foldl' restoreSchemeRoot nodesSolved0 schemeRootsBase
        nodesSolved =
            let
                nodesSolvedBaseAdjusted =
                    IntMap.foldlWithKey'
                        (\acc key node ->
                            case node of
                                TyVar{ tnBound = Just bndBase } ->
                                    if IntSet.member (getNodeId bndBase) schemeRootsBaseSet
                                        then
                                            case applyRedirectsToRef (typeRef bndBase) of
                                                TypeRef bnd' ->
                                                    IntMap.adjust
                                                        (\n -> case n of
                                                            TyVar{} -> n { tnBound = Just bnd' }
                                                            _ -> n
                                                        )
                                                        key
                                                        acc
                                                GenRef _ -> acc
                                        else acc
                                _ -> acc
                        )
                        nodesSolved1
                        baseNodes
                restoreNamedVars acc =
                    let insertNamed acc' childKey _parentRef =
                            insertBaseVarWith id adoptNodeId acc' childKey
                    in IntMap.foldlWithKey'
                        (\acc' childKey (parentRef, _flag) ->
                            insertNamed acc' childKey parentRef
                        )
                        acc
                        (cBindParents base)
                restoreSchemeInteriorVars acc =
                    let
                        schemeInteriorsBase =
                            IntSet.unions
                                [ reachableFromWithBounds baseNodes root
                                | gen <- NodeAccess.allGenNodes base
                                , root <- gnSchemes gen
                                ]
                        insertVarFromBase acc' key =
                            if IntSet.member key schemeRootsBaseSet
                                then acc'
                                else insertBaseVarWith adoptNodeId adoptNodeId acc' key
                    in IntSet.foldl' insertVarFromBase acc schemeInteriorsBase
                restoreBindParentVars acc =
                    let
                        parentKeys = map getNodeId (IntMapUtils.typeParentNodes (cBindParents base))
                        childKeys = map getNodeId (IntMapUtils.typeChildNodes (cBindParents base))
                        keys = IntSet.fromList (parentKeys ++ childKeys)
                        insertVarFromBase =
                            insertBaseVarWith adoptNodeId adoptNodeId
                    in IntSet.foldl' insertVarFromBase acc keys
            in restoreBindParentVars (restoreSchemeInteriorVars (restoreNamedVars nodesSolvedBaseAdjusted))
    in Phase1Result
        { p1NodesSolved = nodesSolved
        , p1SchemeRootsBase = schemeRootsBase
        , p1SchemeRootsBaseSet = schemeRootsBaseSet
        , p1SchemeRootsAllSet = schemeRootsAllSet
        }
