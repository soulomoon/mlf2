{-# LANGUAGE RecordWildCards #-}

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import MLF.Frontend.Syntax
import MLF.Frontend.ConstraintGen (generateConstraints)
import MLF.Frontend.ConstraintGen.Types (ConstraintResult(..))
import MLF.Frontend.Normalize (normalizeExpr)
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Acyclicity (checkAcyclicity)
import MLF.Constraint.Presolution (computePresolution, PresolutionResult(..))
import MLF.Constraint.Presolution (PresolutionState(..), runPresolutionM, decideMinimalExpansion, processInstEdge)
import MLF.Constraint.Presolution.Base (instantiationBindersM)
import MLF.Constraint.Presolution.Expansion (applyExpansionEdgeTraced)
import MLF.Constraint.Presolution.Ops (getNode, getCanonicalNode)
import MLF.Constraint.Solve (solveUnify, SolveResult(..))
import MLF.Constraint.Types
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Constraint.VarStore as VarStore
import qualified MLF.Constraint.Solve as Solve (frWith)
import MLF.Elab.Generalize (generalizeAt, generalizeAtKeepTarget)
import MLF.Elab.Types (Pretty(..))
import MLF.Elab.Run (applyRedirectsToAnn, chaseRedirects)

data EdgeInfo = EdgeInfo
    { eiId :: EdgeId
    , eiLeft :: NodeId
    , eiRight :: NodeId
    , eiBinders :: [NodeId]
    , eiReqExp :: Expansion
    }
    deriving (Show)

bindingScopeRef :: Constraint -> NodeId -> Either BindingError NodeRef
bindingScopeRef constraint root = do
    path <- Binding.bindingPathToRoot constraint (typeRef root)
    case drop 1 path of
        [] -> Left $ MissingBindParent (typeRef root)
        rest ->
            case [gid | GenRef gid <- rest] of
                (gid:_) -> Right (GenRef gid)
                [] -> Left $ InvalidBindingTree ("bindingScopeRef: missing gen ancestor for " ++ show root)

main :: IO ()
main = do
    let expr =
            EAnn
                (ELam "x" (EVar "x"))
                (STArrow (STBase "Int") (STBase "Int"))
    case normalizeExpr expr of
        Left normErr -> print normErr
        Right normExpr -> case generateConstraints Set.empty normExpr of
            Left err -> print err
            Right ConstraintResult{..} -> do
            putStrLn $ "ann (raw): " ++ show crAnnotated
            putStrLn $ "nodes (raw):"
            mapM_ print (IntMap.toList (cNodes crConstraint))
            putStrLn "bind parents (raw):"
            mapM_
                (\(childKey, (parent, flag)) ->
                    putStrLn $
                        show (nodeRefFromKey childKey)
                            ++ " -> "
                            ++ show parent
                            ++ " ("
                            ++ show flag
                            ++ ")"
                )
                (IntMap.toList (cBindParents crConstraint))
            putStrLn "gen nodes (raw):"
            mapM_ print (IntMap.toList (cGenNodes crConstraint))
            let c1 = normalize crConstraint
            putStrLn "inst edges (normalized):"
            mapM_ print (cInstEdges c1)
            let edges = cInstEdges c1
                initState =
                    PresolutionState
                        { psConstraint = c1
                        , psPresolution = Presolution IntMap.empty
                        , psUnionFind = IntMap.empty
                        , psNextNodeId = maxNodeIdKeyOr0 c1 + 1
                        , psPendingWeakens = IntSet.empty
                        , psEdgeExpansions = IntMap.empty
                        , psEdgeWitnesses = IntMap.empty
                        , psEdgeTraces = IntMap.empty
                        }
                inspectEdge edge = do
                    let leftId = instLeft edge
                        rightId = instRight edge
                    leftNode <- getNode leftId
                    rightNode <- getCanonicalNode rightId
                    case leftNode of
                        TyExp{ tnBody = bodyId } -> do
                            (_root, binders) <- instantiationBindersM bodyId
                            (reqExp, _unifs) <- decideMinimalExpansion leftNode rightNode
                            pure EdgeInfo
                                { eiId = instEdgeId edge
                                , eiLeft = leftId
                                , eiRight = rightId
                                , eiBinders = binders
                                , eiReqExp = reqExp
                                }
                        _ ->
                            pure EdgeInfo
                                { eiId = instEdgeId edge
                                , eiLeft = leftId
                                , eiRight = rightId
                                , eiBinders = []
                                , eiReqExp = ExpIdentity
                                }
            case runPresolutionM initState (mapM inspectEdge edges) of
                Left err -> print err
                Right (infos0, st0) -> do
                    putStrLn "edge decisions (initial):"
                    mapM_ print infos0
                    case edges of
                        [] -> pure ()
                        (firstEdge:_) ->
                            case runPresolutionM st0 (do
                                n1 <- getNode (instLeft firstEdge)
                                n2 <- getCanonicalNode (instRight firstEdge)
                                (reqExp, _unifs) <- decideMinimalExpansion n1 n2
                                applyExpansionEdgeTraced reqExp n1
                                ) of
                                Left err -> print err
                                Right (trace0, _stTrace) ->
                                    putStrLn $ "applyExpansionEdgeTraced (first edge): " ++ show trace0
                    case edges of
                        [] -> pure ()
                        (firstEdge:_) ->
                            case runPresolutionM st0 (processInstEdge firstEdge >> mapM inspectEdge edges) of
                                Left err -> print err
                                Right (infos1, st1) -> do
                                    putStrLn "edge decisions (after first edge):"
                                    mapM_ print infos1
                                    putStrLn "bind parents (after first edge):"
                                    mapM_
                                        (\(childKey, (parent, flag)) ->
                                            putStrLn $
                                                show (nodeRefFromKey childKey)
                                                    ++ " -> "
                                                    ++ show parent
                                                    ++ " ("
                                                    ++ show flag
                                                    ++ ")"
                                        )
                                        (IntMap.toList (cBindParents (psConstraint st1)))
            case checkAcyclicity c1 of
                Left err -> print err
                Right acyc -> do
                    case computePresolution acyc c1 of
                        Left err -> print err
                        Right PresolutionResult{..} -> do
                            putStrLn $ "edge expansions: " ++ show prEdgeExpansions
                            putStrLn $ "edge witnesses: " ++ show prEdgeWitnesses
                            putStrLn $ "edge traces: " ++ show prEdgeTraces
                            case solveUnify prConstraint of
                                Left err -> print err
                                Right solved@SolveResult{..} -> do
                                    let ann' = applyRedirectsToAnn prRedirects crAnnotated
                                    putStrLn $ "ann': " ++ show ann'
                                    let root' = chaseRedirects prRedirects crRoot
                                        canonical = Solve.frWith srUnionFind
                                        rootC = canonical root'
                                    putStrLn $ "root: " ++ show rootC
                                    putStrLn $ "canonical NodeId2: " ++ show (canonical (NodeId 2))
                                    putStrLn $ "root node: " ++ show (IntMap.lookup (getNodeId rootC) (cNodes srConstraint))
                                    putStrLn $ "root bound: " ++ show (VarStore.lookupVarBound srConstraint rootC)
                                    let body = case IntMap.lookup (getNodeId rootC) (cNodes srConstraint) of
                                            Just TyForall{ tnBody = b } -> Just b
                                            _ -> Nothing
                                    putStrLn $ "root body: " ++ show body
                                    case body of
                                        Nothing -> pure ()
                                        Just b -> do
                                            putStrLn $ "body node: " ++ show (IntMap.lookup (getNodeId b) (cNodes srConstraint))
                                            putStrLn $ "body bound: " ++ show (VarStore.lookupVarBound srConstraint b)
                                            case VarStore.lookupVarBound srConstraint b of
                                                Nothing -> pure ()
                                                Just bnd -> do
                                                    putStrLn $ "bound node: " ++ show (IntMap.lookup (getNodeId bnd) (cNodes srConstraint))
                                    case bindingScopeRef srConstraint rootC of
                                        Left err -> print err
                                        Right scopeRoot -> do
                                            putStrLn $ "scopeRoot: " ++ show scopeRoot
                                            case scopeRoot of
                                                GenRef gid -> do
                                                    let constraint = srConstraint
                                                        nodes = cNodes constraint
                                                        uf = srUnionFind
                                                        canonical' = Solve.frWith uf
                                                        bindParents0 =
                                                            case Binding.canonicalizeBindParentsUnder canonical' constraint of
                                                                Left err -> IntMap.empty
                                                                Right bp -> bp
                                                        bindFlags =
                                                            IntMap.fromList
                                                                [ (childKey, flag)
                                                                | (childKey, (_parent, flag)) <- IntMap.toList bindParents0
                                                                ]
                                                        isQuantifiable nid =
                                                            case IntMap.lookup (getNodeId nid) nodes of
                                                                Just TyVar{} -> not (VarStore.isEliminatedVar constraint (canonical' nid))
                                                                _ -> False
                                                        weakenedSet = cWeakenedVars constraint
                                                        isBindable key child =
                                                            case IntMap.lookup key bindFlags of
                                                                Just BindFlex -> isQuantifiable child
                                                                Just BindRigid ->
                                                                    isQuantifiable child && IntSet.member (getNodeId (canonical' child)) weakenedSet
                                                                _ -> False
                                                    putStrLn "Binder candidates:"
                                                    case Binding.interiorOfUnder canonical' constraint (genRef gid) of
                                                        Left err -> print err
                                                        Right interior -> do
                                                            let candidates =
                                                                    [ canonical' child
                                                                    | key <- IntSet.toList interior
                                                                    , TypeRef child <- [nodeRefFromKey key]
                                                                    , isBindable (nodeRefKey (typeRef child)) child
                                                                    ]
                                                            print candidates
                                                _ -> pure ()
                                            case generalizeAtKeepTarget solved scopeRoot rootC of
                                                Left err -> print err
                                                Right (sch, subst) -> do
                                                    putStrLn $ "scheme: " ++ pretty sch
                                                    putStrLn $ "scheme raw: " ++ show sch
                                                    putStrLn $ "subst: " ++ show subst
                                    putStrLn "Bind parents:"
                                    mapM_
                                        (\(childKey, (parent, flag)) ->
                                            putStrLn $
                                                show (nodeRefFromKey childKey)
                                                    ++ " -> "
                                                    ++ show parent
                                                    ++ " ("
                                                    ++ show flag
                                                    ++ ")"
                                        )
                                        (IntMap.toList (cBindParents srConstraint))
                                    putStrLn "Eliminated vars:"
                                    print (cEliminatedVars srConstraint)
                                    putStrLn "Weakened vars:"
                                    print (cWeakenedVars srConstraint)
                                    putStrLn "Gen nodes:"
                                    mapM_ print (IntMap.toList (cGenNodes srConstraint))
                                    putStrLn "Nodes:"
                                    mapM_ print (IntMap.toList (cNodes srConstraint))
