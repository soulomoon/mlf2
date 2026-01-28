module MLF.Constraint.Presolution.Plan.SchemeRoots (
    SchemeRootInfo(..),
    SchemeRootsPlan(..),
    buildSchemeRootsPlan,
    schemeOwnerFromBody,
    preferredBaseRoot,
    allowBoundTraversalFor
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types
import qualified MLF.Constraint.NodeAccess as NodeAccess
import qualified MLF.Constraint.VarStore as VarStore
import MLF.Constraint.Presolution.Plan.BinderPlan (GaBindParentsInfo(..))

data SchemeRootInfo = SchemeRootInfo
    { sriRootsWithGen :: [(GenNodeId, NodeId)]
    , sriRootKeySetRaw :: IntSet.IntSet
    , sriRootKeySet :: IntSet.IntSet
    , sriRootOwner :: IntMap.IntMap GenNodeId
    , sriRootByBody :: IntMap.IntMap NodeId
    }

data SchemeRootsPlan = SchemeRootsPlan
    { srInfo :: SchemeRootInfo
    , srSchemeRootOwnerBase :: IntMap.IntMap GenNodeId
    , srSchemeRootByBodyBase :: IntMap.IntMap NodeId
    , srLookupSchemeRootOwner :: NodeId -> Maybe GenNodeId
    , srIsSchemeRootBody :: NodeId -> Maybe NodeId
    , srContainsForallFrom :: (Int -> Bool) -> NodeId -> Bool
    , srContainsForallForTarget :: NodeId -> Bool
    , srBoundHasForallForVar :: NodeId -> Bool
    }

buildSchemeRootInfo
    :: (NodeId -> NodeId)
    -> Constraint
    -> IntMap.IntMap TyNode
    -> SchemeRootInfo
buildSchemeRootInfo canonical constraint nodes =
    let schemeRootsWithGen =
            [ (gnId gen, root)
            | gen <- NodeAccess.allGenNodes constraint
            , root <- gnSchemes gen
            ]
        schemeRootKeySetRaw =
            IntSet.fromList [ getNodeId root | (_gid, root) <- schemeRootsWithGen ]
        schemeRootOwner =
            IntMap.fromList
                [ (getNodeId (canonical root), gid)
                | (gid, root) <- schemeRootsWithGen
                ]
        schemeRootKeySet =
            IntSet.union schemeRootKeySetRaw $
                IntSet.fromList
                    [ getNodeId (canonical root)
                    | (_gid, root) <- schemeRootsWithGen
                    ]
        schemeRootByBody =
            IntMap.fromListWith
                (\a _ -> a)
                [ (getNodeId (canonical bnd), root)
                | (_gid, root) <- schemeRootsWithGen
                , Just bnd <- [VarStore.lookupVarBound constraint root]
                , case IntMap.lookup (getNodeId (canonical bnd)) nodes of
                    Just TyBase{} -> False
                    Just TyBottom{} -> False
                    _ -> True
                ]
    in SchemeRootInfo
        { sriRootsWithGen = schemeRootsWithGen
        , sriRootKeySetRaw = schemeRootKeySetRaw
        , sriRootKeySet = schemeRootKeySet
        , sriRootOwner = schemeRootOwner
        , sriRootByBody = schemeRootByBody
        }

buildSchemeRootsPlan
    :: (NodeId -> NodeId)
    -> Constraint
    -> IntMap.IntMap TyNode
    -> Maybe GaBindParentsInfo
    -> (BindParents -> NodeRef -> Maybe GenNodeId)
    -> SchemeRootsPlan
buildSchemeRootsPlan canonical constraint nodes mbBindParentsGa firstGenAncestor =
    let schemeRootInfo = buildSchemeRootInfo canonical constraint nodes
        SchemeRootInfo
            { sriRootKeySet = schemeRootKeySet
            , sriRootOwner = schemeRootOwner
            , sriRootByBody = schemeRootByBody
            } = schemeRootInfo
        lookupSchemeRootOwner bnd =
            let bndC = canonical bnd
                key = getNodeId bndC
            in case IntMap.lookup key schemeRootOwner of
                Just gid -> Just gid
                Nothing ->
                    case IntMap.lookup key schemeRootByBody of
                        Just root ->
                            IntMap.lookup (getNodeId (canonical root)) schemeRootOwner
                        Nothing -> Nothing
        isSchemeRootBody bnd =
            case IntMap.lookup (getNodeId (canonical bnd)) schemeRootByBody of
                Just root -> Just root
                Nothing -> Nothing
        containsForallFrom treatAsForall start =
            let walkForall visited nid0 =
                    let nid = canonical nid0
                        key = getNodeId nid
                    in if IntSet.member key visited
                        then False
                        else
                            if treatAsForall key
                                then True
                                else case IntMap.lookup key nodes of
                                    Just TyForall{} -> True
                                    Just TyVar{ tnBound = Just bnd } ->
                                        walkForall (IntSet.insert key visited) bnd
                                    Just TyExp{ tnBody = b } ->
                                        walkForall (IntSet.insert key visited) b
                                    Just node ->
                                        let visited' = IntSet.insert key visited
                                        in any (walkForall visited') (structuralChildren node)
                                    Nothing -> False
            in walkForall IntSet.empty start
        containsForallForTarget bnd =
            containsForallFrom
                (\key -> IntSet.member key schemeRootKeySet || IntMap.member key schemeRootByBody)
                bnd
        boundHasForallForVar v =
            let treatAsForall key =
                    IntSet.member key schemeRootKeySet || IntMap.member key schemeRootByBody
            in case VarStore.lookupVarBound constraint (canonical v) of
                Just bnd -> containsForallFrom treatAsForall bnd
                Nothing -> False
        (schemeRootOwnerBase, schemeRootByBodyBase) =
            case mbBindParentsGa of
                Just ga ->
                    let baseConstraint = gbiBaseConstraint ga
                        baseNodes = cNodes baseConstraint
                        baseParents = gbiBindParentsBase ga
                        baseSchemeRoots =
                            [ root
                            | gen <- NodeAccess.allGenNodes baseConstraint
                            , root <- gnSchemes gen
                            ]
                        ownerFromBinding root =
                            firstGenAncestor baseParents (TypeRef root)
                    in ( IntMap.fromList
                            [ (getNodeId root, gid)
                            | root <- baseSchemeRoots
                            , Just gid <- [ownerFromBinding root]
                            ]
                       , IntMap.fromListWith
                            (\a _ -> a)
                            [ (getNodeId bnd, root)
                            | root <- baseSchemeRoots
                            , Just bnd <- [VarStore.lookupVarBound baseConstraint root]
                            , case IntMap.lookup (getNodeId bnd) baseNodes of
                                Just TyBase{} -> False
                                Just TyBottom{} -> False
                                _ -> True
                            ]
                       )
                Nothing -> (IntMap.empty, IntMap.empty)
    in SchemeRootsPlan
        { srInfo = schemeRootInfo
        , srSchemeRootOwnerBase = schemeRootOwnerBase
        , srSchemeRootByBodyBase = schemeRootByBodyBase
        , srLookupSchemeRootOwner = lookupSchemeRootOwner
        , srIsSchemeRootBody = isSchemeRootBody
        , srContainsForallFrom = containsForallFrom
        , srContainsForallForTarget = containsForallForTarget
        , srBoundHasForallForVar = boundHasForallForVar
        }

schemeOwnerFromBody
    :: SchemeRootsPlan
    -> IntMap.IntMap NodeId
    -> NodeId
    -> (Maybe GenNodeId, Bool)
schemeOwnerFromBody plan solvedToBasePref typeRootC =
    let SchemeRootInfo
            { sriRootByBody = schemeRootByBody
            } = srInfo plan
        key = getNodeId typeRootC
        schemeOwnerFromBodySolved =
            case IntMap.lookup key schemeRootByBody of
                Just root -> srLookupSchemeRootOwner plan root
                Nothing -> Nothing
        schemeOwnerFromBodyBase =
            case IntMap.lookup key solvedToBasePref of
                Just baseN ->
                    case IntMap.lookup (getNodeId baseN) (srSchemeRootByBodyBase plan) of
                        Just baseRoot ->
                            IntMap.lookup (getNodeId baseRoot) (srSchemeRootOwnerBase plan)
                        Nothing -> Nothing
                Nothing -> Nothing
        owner =
            case schemeOwnerFromBodyBase of
                Just _ -> schemeOwnerFromBodyBase
                Nothing -> schemeOwnerFromBodySolved
        ownerIsAlias = IntMap.member key schemeRootByBody
    in (owner, ownerIsAlias)

preferredBaseRoot
    :: (NodeId -> NodeId)
    -> Maybe GaBindParentsInfo
    -> IntMap.IntMap NodeId
    -> NodeId
    -> Maybe NodeId
preferredBaseRoot canonical mbBindParentsGa solvedToBasePref typeRoot =
    case mbBindParentsGa of
        Just _ ->
            case IntMap.lookup (getNodeId (canonical typeRoot)) solvedToBasePref of
                Just baseN
                    | canonical baseN /= canonical typeRoot -> Just baseN
                _ -> Nothing
        Nothing -> Nothing

allowBoundTraversalFor
    :: SchemeRootsPlan
    -> (NodeId -> NodeId)
    -> Maybe GenNodeId
    -> NodeId
    -> NodeId
    -> Bool
allowBoundTraversalFor plan canonical scopeGen target0 bnd =
    case srLookupSchemeRootOwner plan bnd of
        Nothing -> True
        Just gid ->
            case scopeGen of
                Just scopeGid ->
                    gid == scopeGid || boundIsTargetSchemeBody bnd
                Nothing -> False
  where
    boundIsTargetSchemeBody bnd0 =
        case srIsSchemeRootBody plan bnd0 of
            Just root -> canonical root == canonical target0
            Nothing -> False
