module SpecUtil (
    emptyConstraint,
    genNodeMap,
    rootedConstraint,
    bindParentsFromPairs,
    collectVarNodes,
    expectRight,
    requireRight,
    inferBindParents,
    lookupNodeMaybe,
    lookupNode,
    lookupNodeIO
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Test.Hspec (Expectation, expectationFailure)

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types (BindFlag(..), BindParents, Constraint(..), GenNode(..), GenNodeId(..), NodeId(..), TyNode(..), genRef, nodeRefKey, structuralChildren, typeRef)
import MLF.Frontend.ConstraintGen (AnnExpr(..))
import MLF.Frontend.Syntax (VarName)

emptyConstraint :: Constraint
emptyConstraint = Constraint
    { cNodes = IntMap.empty
    , cInstEdges = []
    , cUnifyEdges = []
    , cBindParents = IntMap.empty
    , cPolySyms = Set.empty
    , cEliminatedVars = IntSet.empty
    , cWeakenedVars = IntSet.empty
    , cAnnEdges = IntSet.empty
    , cLetEdges = IntSet.empty
    , cGenNodes = IntMap.empty
    }

genNodeMap :: [NodeId] -> IntMap.IntMap GenNode
genNodeMap ids =
    IntMap.fromList
        [ (getGenNodeId gid, GenNode gid [nid])
        | nid <- ids
        , let gid = GenNodeId (getNodeId nid)
        ]

-- | Attach a root gen node and bind term-DAG roots under it (test helper).
rootedConstraint :: Constraint -> Constraint
rootedConstraint c0 =
    let rootId = GenNodeId 0
        roots =
            map NodeId $
                IntSet.toList (Binding.computeTermDagRoots c0)
        genNode = GenNode rootId roots
        genNodes' = IntMap.insert (getGenNodeId rootId) genNode (cGenNodes c0)
        bindParents' =
            foldr
                (\nid bp ->
                    let key = nodeRefKey (typeRef nid)
                    in if IntMap.member key bp
                        then bp
                        else IntMap.insert key (genRef rootId, BindFlex) bp
                )
                (cBindParents c0)
                roots
    in c0 { cGenNodes = genNodes', cBindParents = bindParents' }

bindParentsFromPairs :: [(NodeId, NodeId, BindFlag)] -> BindParents
bindParentsFromPairs pairs =
    IntMap.fromList
        [ (nodeRefKey (typeRef child), (typeRef parent, flag))
        | (child, parent, flag) <- pairs
        ]

expectRight :: Show e => Either e a -> (a -> Expectation) -> Expectation
expectRight value k =
    case value of
        Left err -> expectationFailure $ "Expected success, but got: " ++ show err
        Right result -> k result

requireRight :: Show e => Either e a -> IO a
requireRight = either (\e -> expectationFailure (show e) >> fail "requireRight") pure

inferBindParents :: IntMap.IntMap TyNode -> BindParents
inferBindParents nodes =
    -- Follow term structure edges (child → structural parent).
    foldl' addEdges IntMap.empty (IntMap.elems nodes)
  where
    addEdges bp parentNode =
        let parent = tnId parentNode
            boundKids =
                case parentNode of
                    TyVar{ tnBound = Just bnd } -> [bnd]
                    _ -> []
            kids = structuralChildren parentNode ++ boundKids

            addOne m child
                | child == parent = m
                | otherwise =
                    IntMap.insertWith
                        (\_ old -> old)
                        (nodeRefKey (typeRef child))
                        (typeRef parent, BindFlex)
                        m
        in foldl' addOne bp kids

lookupNodeMaybe :: IntMap.IntMap TyNode -> NodeId -> Maybe TyNode
lookupNodeMaybe table nid = IntMap.lookup (getNodeId nid) table

lookupNode :: HasCallStack => IntMap.IntMap TyNode -> NodeId -> IO TyNode
lookupNode = lookupNodeIO

lookupNodeIO :: HasCallStack => IntMap.IntMap TyNode -> NodeId -> IO TyNode
lookupNodeIO table nid =
    case lookupNodeMaybe table nid of
        Just node -> pure node
        Nothing -> do
            expectationFailure $ "Missing node: " ++ show nid
            pure (error "unreachable: missing TyNode")

collectVarNodes :: VarName -> AnnExpr -> [NodeId]
collectVarNodes name = go
  where
    go ann = case ann of
        AVar v nid | v == name -> [nid]
        AVar _ _ -> []
        ALit _ _ -> []
        ALam _ _ _ body _ -> go body
        AApp fun arg _ _ _ -> go fun ++ go arg
        ALet _ _ _ _ _ rhs body _ -> go rhs ++ go body
        AAnn expr _ _ -> go expr
