module SpecUtil (
    emptyConstraint,
    nodeMapFromList,
    nodeMapSingleton,
    nodeMapElems,
    nodeMapKeys,
    nodeMapSize,
    nodeMapMember,
    genNodeMap,
    rootedConstraint,
    bindParentsFromPairs,
    collectVarNodes,
    defaultTraceConfig,
    unsafeNormalizeExpr,
    firstShowE,
    runToPresolutionDefault,
    runToSolvedDefault,
    expectRight,
    requireRight,
    inferBindParents,
    lookupNodeMaybe,
    lookupNode,
    lookupNodeIO,
    mkForalls
) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Test.Hspec (Expectation, expectationFailure)

import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.Types.Graph as Graph
import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , BindParents
    , Constraint(..)
    , GenNode(..)
    , GenNodeId(..)
    , GenNodeMap
    , NodeId(..)
    , NodeMap(..)
    , PolySyms
    , TyNode(..)
    , fromListGen
    , fromListNode
    , genRef
    , getNodeId
    , insertGen
    , nodeRefKey
    , structuralChildren
    , toListNode
    , typeRef
    )
import MLF.Constraint.Acyclicity (checkAcyclicity)
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Presolution (PresolutionResult(..), computePresolution)
import MLF.Constraint.Solve (SolveResult, solveUnify)
import MLF.Frontend.ConstraintGen (AnnExpr(..), ConstraintResult(..), generateConstraints)
import MLF.Frontend.Normalize (normalizeExpr)
import MLF.Frontend.Syntax (NormSurfaceExpr, SrcTy(..), SrcType, SurfaceExpr, VarName, mkSrcBound)
import MLF.Elab.Pipeline (defaultTraceConfig)

emptyConstraint :: Constraint
emptyConstraint = Constraint
    { cNodes = fromListNode []
    , cInstEdges = []
    , cUnifyEdges = []
    , cBindParents = IntMap.empty
    , cPolySyms = Set.empty
    , cEliminatedVars = IntSet.empty
    , cWeakenedVars = IntSet.empty
    , cAnnEdges = IntSet.empty
    , cLetEdges = IntSet.empty
    , cGenNodes = fromListGen []
    }

nodeMapFromList :: [(Int, TyNode)] -> NodeMap TyNode
nodeMapFromList pairs = NodeMap (IntMap.fromList pairs)

nodeMapSingleton :: Int -> TyNode -> NodeMap TyNode
nodeMapSingleton key value = NodeMap (IntMap.singleton key value)

nodeMapElems :: NodeMap a -> [a]
nodeMapElems = map snd . toListNode

nodeMapKeys :: NodeMap a -> [Int]
nodeMapKeys = map (getNodeId . fst) . toListNode

nodeMapSize :: NodeMap a -> Int
nodeMapSize = length . toListNode

nodeMapMember :: NodeId -> NodeMap a -> Bool
nodeMapMember nid nodes =
    case Graph.lookupNode nid nodes of
        Just _ -> True
        Nothing -> False

genNodeMap :: [NodeId] -> GenNodeMap GenNode
genNodeMap ids =
    fromListGen
        [ (gid, GenNode gid [nid])
        | nid <- ids
        , let gid = GenNodeId (getNodeId nid)
        ]

mkForalls :: [(String, Maybe SrcType)] -> SrcType -> SrcType
mkForalls binds body =
    foldr
        (\(name, mbBound) acc -> STForall name (fmap mkSrcBound mbBound) acc)
        body
        binds

-- | Attach a root gen node and bind term-DAG roots under it (test helper).
rootedConstraint :: Constraint -> Constraint
rootedConstraint c0 =
    let rootId = GenNodeId 0
        roots =
            map NodeId $
                IntSet.toList (Binding.computeTermDagRoots c0)
        genNode = GenNode rootId roots
        genNodes' = insertGen rootId genNode (cGenNodes c0)
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

unsafeNormalizeExpr :: SurfaceExpr -> NormSurfaceExpr
unsafeNormalizeExpr expr =
    case normalizeExpr expr of
        Left err -> error ("normalizeExpr failed in test: " ++ show err)
        Right out -> out

firstShowE :: Show e => Either e a -> Either String a
firstShowE = either (Left . show) Right

runToPresolutionDefault :: PolySyms -> SurfaceExpr -> Either String PresolutionResult
runToPresolutionDefault poly expr = do
    ConstraintResult{ crConstraint = c0 } <-
        firstShowE (generateConstraints poly (unsafeNormalizeExpr expr))
    let c1 = normalize c0
    acyc <- firstShowE (checkAcyclicity c1)
    firstShowE (computePresolution defaultTraceConfig acyc c1)

runToSolvedDefault :: PolySyms -> SurfaceExpr -> Either String SolveResult
runToSolvedDefault poly expr = do
    pres <- runToPresolutionDefault poly expr
    firstShowE (solveUnify defaultTraceConfig (prConstraint pres))

inferBindParents :: NodeMap TyNode -> BindParents
inferBindParents nodes =
    -- Follow term structure edges (child → structural parent).
    foldl' addEdges IntMap.empty (nodeMapElems nodes)
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

lookupNodeMaybe :: NodeMap TyNode -> NodeId -> Maybe TyNode
lookupNodeMaybe nodes nid = Graph.lookupNode nid nodes

lookupNode :: HasCallStack => NodeMap TyNode -> NodeId -> IO TyNode
lookupNode = lookupNodeIO

lookupNodeIO :: HasCallStack => NodeMap TyNode -> NodeId -> IO TyNode
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
