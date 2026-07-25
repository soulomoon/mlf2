module MLF.Elab.Run.Generalize.Phase1 (
    restoreSchemeNodes
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Graph
    ( NodeRef(..)
    , TyNode(..)
    , cBindParents
    , cGenNodes
    , cInstEdges
    , cLetEdges
    , cNodes
    , getEdgeId
    , getNodeId
    , gnSchemes
    , instEdgeId
    , instRight
    , typeRef
    )
import qualified MLF.Constraint.Types.Graph as Types
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
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

restoreSchemeNodes :: GeneralizeEnv p -> Phase1Result
restoreSchemeNodes env =
    let solvedConstraint = geSolvedConstraint env
        base = geBaseConstraint env
        baseNodes = nodeMapToIntMap (cNodes base)
        nodesSolved0 = nodeMapToIntMap (cNodes solvedConstraint)
        canonical = geCanonical env
        adoptRef = geAdoptRef env
        adoptNodeId = geAdoptNodeId env
        applyRedirectsToRef = geApplyRedirectsToRef env
        schemeRootsBaseAll = schemeRootsOf (Types.getGenNodeMap (cGenNodes base))
        schemeRootsSolved = schemeRootsOf (Types.getGenNodeMap (cGenNodes solvedConstraint))
        schemeRootsSolvedSet =
            IntSet.fromList (map (getNodeId . canonical) schemeRootsSolved)
        -- A live base root omitted from finalized scheme metadata has escaped
        -- its old owner during solving.  Restore only roots that disappeared
        -- (or were eliminated); solved ownership is authoritative for live
        -- nodes.
        schemeRootsBase =
            [ root
            | root <- schemeRootsBaseAll
            , let rootC = canonical root
                  rootKey = getNodeId rootC
            , IntSet.member rootKey schemeRootsSolvedSet
                || IntMap.notMember rootKey nodesSolved0
                || VarStore.isEliminatedVar solvedConstraint rootC
            ]
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
        letSchemeRootKeys =
            IntSet.fromList
                [ getNodeId (instRight edge)
                | edge <- cInstEdges base
                , IntSet.member (getEdgeId (instEdgeId edge)) (cLetEdges base)
                ]
        rootNeedsRedirectRestoration root =
            IntSet.member (getNodeId root) letSchemeRootKeys
                && ( IntMap.notMember (getNodeId root) nodesSolved0
                    || VarStore.isEliminatedVar solvedConstraint root
                   )
        targetIsLive target =
            IntMap.member (getNodeId target) nodesSolved0
                && not (VarStore.isEliminatedVar solvedConstraint target)
        redirectedSchemeRootTarget root = do
            TyVar{ tnBound = Nothing } <- IntMap.lookup (getNodeId root) baseNodes
            if rootNeedsRedirectRestoration root
                then
                    case applyRedirectsToRef (typeRef root) of
                        TypeRef redirected
                            | redirected /= root ->
                                case adoptRef (typeRef root) of
                                    TypeRef target
                                        | target /= root
                                        , targetIsLive target -> Just target
                                    _ -> Nothing
                        _ -> Nothing
                else Nothing
        restoreSchemeRoot acc root =
            let mbBase = do
                    TyVar{ tnBound = mb } <- IntMap.lookup (getNodeId root) baseNodes
                    bnd <- mb
                    case adoptRef (typeRef bnd) of
                        TypeRef bnd' -> Just bnd'
                        GenRef _ -> Nothing
                -- Alternative-let scheme roots start unbounded.  When solving
                -- eliminates one through its identity edge, the typed redirect
                -- is the construction authority for the restored bound.
                mbRedirected = redirectedSchemeRootTarget root
                mbRestored =
                    case mbBase of
                        Just baseBound -> Just baseBound
                        Nothing -> mbRedirected
                insertRoot bnd' =
                    IntMap.insert (getNodeId root) (TyVar { tnId = root, tnBound = Just bnd' }) acc
                fillMissing nid bnd' =
                    IntMap.insert (getNodeId root) (TyVar { tnId = nid, tnBound = Just bnd' }) acc
            in case IntMap.lookup (getNodeId root) acc of
                Nothing -> maybe acc insertRoot mbRestored
                Just TyVar{ tnId = nid, tnBound = Nothing } ->
                    maybe acc (fillMissing nid) mbRestored
                Just _ -> acc
        (schemeRootsBaseSet, schemeRootsAllSet) =
            let baseSet = IntSet.fromList (map getNodeId schemeRootsBase)
                solvedSet = schemeRootsSolvedSet
                restoredSet =
                    IntSet.fromList
                        (map getNodeId (IntMap.elems restoredSchemeRootTargets))
            in (baseSet, IntSet.unions [baseSet, solvedSet, restoredSet])
        restoredSchemeRootTargets =
            IntMap.fromList
                [ (getNodeId root, target)
                | root <- schemeRootsBase
                , Just target <- [redirectedSchemeRootTarget root]
                ]
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
        , p1RestoredSchemeRootTargets = restoredSchemeRootTargets
        }
