module SolveSpec (spec) where

import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import Data.List (isPrefixOf)
import qualified Data.IntSet as IntSet

import MLF.Types
import MLF.Solve

gNode :: Int -> GNode
gNode n = GNode (GNodeId n) Nothing [] []

emptyConstraint :: Constraint
emptyConstraint = Constraint
    { cNodes = IntMap.empty
    , cGNodes = IntMap.empty
    , cInstEdges = []
    , cUnifyEdges = []
    , cGForest = []
    , cBindParents = IntMap.empty
    , cVarBounds = IntMap.empty
    , cEliminatedVars = IntSet.empty
    }

inferBindParents :: IntMap.IntMap TyNode -> BindParents
inferBindParents nodes =
    foldl'
        (\bp parentNode ->
            let parent = tnId parentNode
                kids = case parentNode of
                    TyArrow{ tnDom = d, tnCod = c } -> [d, c]
                    TyForall{ tnBody = b } -> [b]
                    TyExp{ tnBody = b } -> [b]
                    _ -> []
                addOne m child
                    | child == parent = m
                    | otherwise = IntMap.insertWith (\_ old -> old) (getNodeId child) (parent, BindFlex) m
            in foldl' addOne bp kids
        )
        IntMap.empty
        (IntMap.elems nodes)

spec :: Spec
spec = describe "Phase 5 -- Solve" $ do
    describe "Variables and structure" $ do
        it "merges a variable with a base type and rewrites to the canonical node" $ do
            let var = TyVar (NodeId 0) (GNodeId 0)
                base = TyBase (NodeId 1) (BaseTy "Int")
                nodes = IntMap.fromList [(0, var), (1, base)]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 1)]
                    }
            case solveUnify constraint of
                Left err -> expectationFailure $ "Unexpected solve error: " ++ show err
                Right SolveResult{ srConstraint = sc, srUnionFind = uf } -> do
                    cUnifyEdges sc `shouldBe` []
                    IntMap.lookup 0 uf `shouldBe` Just (NodeId 1)
                    IntMap.lookup 1 (cNodes sc)
                        `shouldBe` Just (TyBase (NodeId 1) (BaseTy "Int"))
                    IntMap.size (cNodes sc) `shouldBe` 1

        it "merges two variables and drains the queue" $ do
            let v0 = TyVar (NodeId 0) (GNodeId 0)
                v1 = TyVar (NodeId 1) (GNodeId 0)
                nodes = IntMap.fromList [(0, v0), (1, v1)]
                constraintVV = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 1)]
                    }
            case solveUnify constraintVV of
                Left err -> expectationFailure $ "Unexpected solve error: " ++ show err
                Right SolveResult{ srConstraint = sc, srUnionFind = uf } -> do
                    cUnifyEdges sc `shouldBe` []
                    IntMap.lookup 0 uf `shouldBe` Just (NodeId 1)

        it "harmonizes binding parents to the LCA during merging" $ do
            -- Paper Raise(n): before Var/Var union, raise binders so both vars
            -- share the same binding parent.
            let vInner = NodeId 0
                vOuter = NodeId 1
                inner = NodeId 2
                root = NodeId 3

                nodeInner = TyVar vInner (GNodeId 1)
                nodeOuter = TyVar vOuter (GNodeId 0)
                nodeInnerArrow = TyArrow inner vInner vOuter
                nodeRoot = TyArrow root inner vOuter
                constraint =
                    emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId vInner, nodeInner)
                                , (getNodeId vOuter, nodeOuter)
                                , (getNodeId inner, nodeInnerArrow)
                                , (getNodeId root, nodeRoot)
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (getNodeId vInner, (inner, BindFlex))
                                , (getNodeId inner, (root, BindFlex))
                                , (getNodeId vOuter, (root, BindFlex))
                                ]
                        , cUnifyEdges = [UnifyEdge vOuter vInner]
                        }

            case solveUnify constraint of
                Left err -> expectationFailure $ "Unexpected solve error: " ++ show err
                Right SolveResult{ srConstraint = sc } -> do
                    IntMap.lookup (getNodeId vInner) (cBindParents sc)
                        `shouldBe` Just (root, BindFlex)

        it "unifies variable with arrow when acyclic" $ do
            let dom = TyBase (NodeId 2) (BaseTy "Int")
                cod = TyBase (NodeId 3) (BaseTy "Bool")
                arrow = TyArrow (NodeId 1) (tnId dom) (tnId cod)
                var = TyVar (NodeId 0) (GNodeId 0)
                nodes =
                    IntMap.fromList
                        [ (0, var)
                        , (1, arrow)
                        , (2, dom)
                        , (3, cod)
                        ]
                constraintVA = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 1)]
                    }
            case solveUnify constraintVA of
                Left err -> expectationFailure $ "Unexpected solve error: " ++ show err
                Right SolveResult{ srConstraint = sc, srUnionFind = uf } -> do
                    IntMap.lookup 0 uf `shouldBe` Just (NodeId 1)
                    IntMap.lookup 1 (cNodes sc) `shouldBe` Just arrow

        it "prefers structured representative over variable" $ do
            let dom = TyBase (NodeId 2) (BaseTy "Int")
                cod = TyBase (NodeId 3) (BaseTy "Bool")
                arrow = TyArrow (NodeId 1) (tnId dom) (tnId cod)
                var = TyVar (NodeId 0) (GNodeId 0)
                nodes =
                    IntMap.fromList
                        [ (0, var)
                        , (1, arrow)
                        , (2, dom)
                        , (3, cod)
                        ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 1)]
                    }
            case solveUnify constraint of
                Left err -> expectationFailure $ "Unexpected solve error: " ++ show err
                Right SolveResult{ srConstraint = sc, srUnionFind = uf } -> do
                    IntMap.lookup 0 uf `shouldBe` Just (NodeId 1)
                    IntMap.lookup 1 (cNodes sc) `shouldBe` Just arrow

    describe "Occurs-check" $ do
        it "fails occurs-check when a variable appears inside the structure it unifies with" $ do
            let var = TyVar (NodeId 0) (GNodeId 0)
                base = TyBase (NodeId 2) (BaseTy "Int")
                arrow = TyArrow (NodeId 1) (NodeId 0) (NodeId 2)
                nodes = IntMap.fromList [(0, var), (1, arrow), (2, base)]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 1)]
                    }
            solveUnify constraint `shouldBe` Left (OccursCheckFailed (NodeId 0) (NodeId 1))

        it "fails occurs-check even after earlier variable unions" $ do
            let base = TyBase (NodeId 3) (BaseTy "Int")
                arrow = TyArrow (NodeId 2) (NodeId 0) (tnId base)  -- dom refers to var0
                var0 = TyVar (NodeId 0) (GNodeId 0)
                var1 = TyVar (NodeId 1) (GNodeId 0)
                nodes =
                    IntMap.fromList
                        [ (0, var0)
                        , (1, var1)
                        , (2, arrow)
                        , (3, base)
                        ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [ UnifyEdge (NodeId 0) (NodeId 1)
                                    , UnifyEdge (NodeId 1) (NodeId 2)
                                    ]
                    }
            solveUnify constraint `shouldBe` Left (OccursCheckFailed (NodeId 1) (NodeId 2))

    describe "Constructor clashes" $ do
        it "detects constructor clashes (arrow vs base)" $ do
            let dom = TyVar (NodeId 1) (GNodeId 0)
                cod = TyVar (NodeId 2) (GNodeId 0)
                arrow = TyArrow (NodeId 0) (tnId dom) (tnId cod)
                base = TyBase (NodeId 3) (BaseTy "Bool")
                nodes =
                    IntMap.fromList
                        [ (0, arrow)
                        , (1, dom)
                        , (2, cod)
                        , (3, base)
                        ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (tnId arrow) (tnId base)]
                    }
            solveUnify constraint `shouldBe` Left (ConstructorClash arrow base)

        it "detects constructor clashes (base vs forall)" $ do
            let base = TyBase (NodeId 0) (BaseTy "Int")
                forallNode = TyForall (NodeId 1) (GNodeId 0) (GNodeId 1) (NodeId 2)
                forallBody = TyBase (NodeId 2) (BaseTy "Int")
                nodes =
                    IntMap.fromList
                        [ (0, base)
                        , (1, forallNode)
                        , (2, forallBody)
                        ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (tnId base) (tnId forallNode)]
                    }
            solveUnify constraint `shouldBe` Left (ConstructorClash base forallNode)

        it "detects constructor clash between arrow and forall" $ do
            let dom = TyBase (NodeId 2) (BaseTy "Int")
                cod = TyBase (NodeId 3) (BaseTy "Bool")
                arrow = TyArrow (NodeId 0) (tnId dom) (tnId cod)
                forallNode = TyForall (NodeId 1) (GNodeId 1) (GNodeId 0) (tnId dom)
                nodes =
                    IntMap.fromList
                        [ (0, arrow)
                        , (1, forallNode)
                        , (2, dom)
                        , (3, cod)
                        ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (tnId arrow) (tnId forallNode)]
                    }
            solveUnify constraint `shouldBe` Left (ConstructorClash arrow forallNode)

        it "reports base clash when base constructors differ" $ do
            let bInt = TyBase (NodeId 0) (BaseTy "Int")
                bBool = TyBase (NodeId 1) (BaseTy "Bool")
                nodes = IntMap.fromList [(0, bInt), (1, bBool)]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 1)]
                    }
            solveUnify constraint `shouldBe` Left (BaseClash (BaseTy "Int") (BaseTy "Bool"))

    describe "Forall handling" $ do
        it "fails when forall nodes have mismatched quantification levels" $ do
            let body1 = TyVar (NodeId 2) (GNodeId 2)
                body2 = TyVar (NodeId 3) (GNodeId 3)
                forall1 = TyForall (NodeId 0) (GNodeId 1) (GNodeId 0) (tnId body1)
                forall2 = TyForall (NodeId 1) (GNodeId 4) (GNodeId 0) (tnId body2)
                nodes =
                    IntMap.fromList
                        [ (0, forall1)
                        , (1, forall2)
                        , (2, body1)
                        , (3, body2)
                        ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (tnId forall1) (tnId forall2)]
                    }
            solveUnify constraint `shouldBe` Left (ForallLevelMismatch (GNodeId 1) (GNodeId 4))

        it "succeeds when forall levels match and bodies agree" $ do
            let body1 = TyBase (NodeId 2) (BaseTy "Int")
                body2 = TyBase (NodeId 3) (BaseTy "Int")
                forall1 = TyForall (NodeId 0) (GNodeId 1) (GNodeId 0) (tnId body1)
                forall2 = TyForall (NodeId 1) (GNodeId 1) (GNodeId 0) (tnId body2)
                nodes =
                    IntMap.fromList
                        [ (0, forall1)
                        , (1, forall2)
                        , (2, body1)
                        , (3, body2)
                        ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (tnId forall1) (tnId forall2)]
                    }
            case solveUnify constraint of
                Left err -> expectationFailure $ "Unexpected solve error: " ++ show err
                Right SolveResult{ srConstraint = sc } ->
                    cUnifyEdges sc `shouldBe` []

        it "fails when forall levels match but bodies clash" $ do
            let body1 = TyBase (NodeId 2) (BaseTy "Int")
                body2 = TyBase (NodeId 3) (BaseTy "Bool")
                forall1 = TyForall (NodeId 0) (GNodeId 1) (GNodeId 0) (tnId body1)
                forall2 = TyForall (NodeId 1) (GNodeId 1) (GNodeId 0) (tnId body2)
                nodes =
                    IntMap.fromList
                        [ (0, forall1)
                        , (1, forall2)
                        , (2, body1)
                        , (3, body2)
                        ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (tnId forall1) (tnId forall2)]
                    }
            solveUnify constraint `shouldBe` Left (BaseClash (BaseTy "Int") (BaseTy "Bool"))

        it "succeeds with structured bodies (arrows) when levels match" $ do
            let d1 = TyBase (NodeId 4) (BaseTy "Int")
                c1 = TyBase (NodeId 5) (BaseTy "Bool")
                d2 = TyBase (NodeId 6) (BaseTy "Int")
                c2 = TyBase (NodeId 7) (BaseTy "Bool")
                b1 = TyArrow (NodeId 2) (tnId d1) (tnId c1)
                b2 = TyArrow (NodeId 3) (tnId d2) (tnId c2)
                f1 = TyForall (NodeId 0) (GNodeId 1) (GNodeId 0) (tnId b1)
                f2 = TyForall (NodeId 1) (GNodeId 1) (GNodeId 0) (tnId b2)
                nodes =
                    IntMap.fromList
                        [ (0, f1), (1, f2)
                        , (2, b1), (3, b2)
                        , (4, d1), (5, c1), (6, d2), (7, c2)
                        ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (tnId f1) (tnId f2)]
                    }
            case solveUnify constraint of
                Left err -> expectationFailure $ "Unexpected solve error: " ++ show err
                Right SolveResult{ srConstraint = sc } ->
                    cUnifyEdges sc `shouldBe` []

    describe "Rewriting and errors" $ do
        it "rewrites inst edges to canonical representatives" $ do
            let var = TyVar (NodeId 0) (GNodeId 0)
                base = TyBase (NodeId 1) (BaseTy "Int")
                instEdge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 0)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList [(0, var), (1, base)]
                    , cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 1)]
                    , cInstEdges = [instEdge]
                    }
            case solveUnify constraint of
                Left err -> expectationFailure $ "Unexpected solve error: " ++ show err
                Right SolveResult{ srConstraint = sc, srUnionFind = uf } -> do
                    IntMap.lookup 0 uf `shouldBe` Just (NodeId 1)
                    cInstEdges sc `shouldBe` [InstEdge (EdgeId 0) (NodeId 1) (NodeId 1)]

        it "rewrites structured children through UF (arrow dom/cod)" $ do
            let domVar = TyVar (NodeId 2) (GNodeId 0)
                codBase = TyBase (NodeId 3) (BaseTy "Bool")
                arrow = TyArrow (NodeId 1) (tnId domVar) (tnId codBase)
                base = TyBase (NodeId 4) (BaseTy "Int")
                nodes =
                    IntMap.fromList
                        [ (1, arrow)
                        , (2, domVar)
                        , (3, codBase)
                        , (4, base)
                        ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (NodeId 2) (NodeId 4)]
                    }
            case solveUnify constraint of
                Left err -> expectationFailure $ "Unexpected solve error: " ++ show err
                Right SolveResult{ srConstraint = sc, srUnionFind = uf } -> do
                    IntMap.lookup 2 uf `shouldBe` Just (NodeId 4)
                    case IntMap.lookup 1 (cNodes sc) of
                        Just (TyArrow _ dom' cod') -> do
                            dom' `shouldBe` NodeId 4
                            cod' `shouldBe` NodeId 3
                        other -> expectationFailure $ "Expected rewritten arrow, got " ++ show other

        it "surfaces MissingNode when unify edge targets absent id" $ do
            let var = TyVar (NodeId 0) (GNodeId 0)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList [(0, var)]
                    , cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 99)]
                    }
            solveUnify constraint `shouldBe` Left (MissingNode (NodeId 99))

        it "rejects TyExp nodes reaching the solver" $ do
            let body = TyBase (NodeId 1) (BaseTy "Int")
                expNode = TyExp (NodeId 0) (ExpVarId 0) (tnId body)
                nodes =
                    IntMap.fromList
                        [ (0, expNode)
                        , (1, body)
                        ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (tnId expNode) (tnId body)]
                    }
            solveUnify constraint `shouldBe` Left (UnexpectedExpNode (tnId expNode))

    describe "Validation Edge Cases" $ do
        it "reports unexpected TyExp in TyExp-TyExp clash" $ do
            let exp1 = TyExp (NodeId 0) (ExpVarId 0) (NodeId 2)
                exp2 = TyExp (NodeId 1) (ExpVarId 1) (NodeId 3)
                body1 = TyBase (NodeId 2) (BaseTy "Int")
                body2 = TyBase (NodeId 3) (BaseTy "Int")
                nodes =
                    IntMap.fromList
                        [ (0, exp1), (1, exp2), (2, body1), (3, body2) ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = inferBindParents nodes
                    , cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 1)]
                    }
            solveUnify constraint `shouldBe` Left (UnexpectedExpNode (NodeId 0))

        it "validates: reports MissingNode when child is missing" $ do
            -- Arrow points to non-existent dom
            let arrow = TyArrow (NodeId 0) (NodeId 1) (NodeId 2)
                cod = TyBase (NodeId 2) (BaseTy "Int")
                -- NodeId 1 is missing
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList [(0, arrow), (2, cod)]
                    }
                res = SolveResult { srConstraint = constraint, srUnionFind = IntMap.empty }
                msgs = validateSolvedGraphStrict res
            msgs `shouldSatisfy` any ("Missing child node" `isPrefixOf`)

        it "validates: reports non-canonical node id" $ do
            -- Node stored at key 0 has id 1
            let base = TyBase (NodeId 1) (BaseTy "Int")
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList [(0, base)] -- Mismatch key vs id
                    }
                res = SolveResult { srConstraint = constraint, srUnionFind = IntMap.empty }
                msgs = validateSolvedGraphStrict res
            msgs `shouldSatisfy` any ("Node key/id mismatch" `isPrefixOf`)

        it "validates: reports non-canonical child reference" $ do
            -- Arrow points to 1, but UF says 1 -> 2
            let arrow = TyArrow (NodeId 0) (NodeId 1) (NodeId 2)
                base = TyBase (NodeId 2) (BaseTy "Int")
                -- Node 1 exists but is aliased to 2 in UF
                alias = TyBase (NodeId 1) (BaseTy "Int") 
                
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList [(0, arrow), (1, alias), (2, base)]
                    }
                uf = IntMap.fromList [(1, NodeId 2)]
                res = SolveResult { srConstraint = constraint, srUnionFind = uf }
                msgs = validateSolvedGraphStrict res
            -- Arrow child 1 is not canonical (should be 2)
            msgs `shouldSatisfy` any ("Non-canonical child id" `isPrefixOf`)
        it "accepts a solved graph when g-nodes are present" $ do
            let var = TyVar (NodeId 0) (GNodeId 0)
                base = TyBase (NodeId 1) (BaseTy "Int")
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList [(0, var), (1, base)]
                    , cUnifyEdges = [UnifyEdge (tnId var) (tnId base)]
                    , cGNodes = IntMap.fromList [(0, gNode 0)]
                    , cGForest = [GNodeId 0]
                    }
            case solveUnify constraint of
                Left err -> expectationFailure $ "Unexpected solve error: " ++ show err
                Right res ->
                    validateSolvedGraphStrict res `shouldBe` []

        it "reports residual inst edges and missing GNodes" $ do
            let base0 = TyBase (NodeId 0) (BaseTy "Int")
                base1 = TyBase (NodeId 1) (BaseTy "Int")
                strayVar = TyVar (NodeId 2) (GNodeId 1)
                inst = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, base0)
                        , (1, base1)
                        , (2, strayVar)
                        ]
                    , cInstEdges = [inst]
                    }
                res = SolveResult { srConstraint = constraint, srUnionFind = IntMap.empty }
                msgs = validateSolvedGraphStrict res
            msgs `shouldSatisfy` (not . null)
            msgs `shouldSatisfy` any ("Residual instantiation edge" `isPrefixOf`)
            msgs `shouldSatisfy` any ("Missing GNode" `isPrefixOf`)
