{-# LANGUAGE ScopedTypeVariables #-}
module BindingSpec (spec) where

import Control.Monad (foldM, forM, forM_)
import Data.List (nub)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck

import MLF.Constraint.Types
import MLF.Binding.Tree
import qualified MLF.Binding.Adjustment as BindingAdjustment
import qualified MLF.Binding.GraphOps as GraphOps
import qualified MLF.Constraint.Root as ConstraintRoot
import qualified MLF.Util.Order as Order
import MLF.Frontend.Syntax (Expr(..), Lit(..), SrcType(..), SrcScheme(..))
import MLF.Frontend.ConstraintGen (ConstraintError, ConstraintResult(..), generateConstraints)
import SpecUtil (emptyConstraint)

generateConstraintsDefault :: Expr -> Either ConstraintError ConstraintResult
generateConstraintsDefault = generateConstraints Set.empty

-- | Generate a valid binding tree with n nodes.
-- The tree is structured as a chain of TyForall nodes: node 0 -> node 1 -> ... -> node (n-1)
-- where each node's body is the next node, and the last node is a TyVar.
-- Binding edges follow the term-DAG structure: each child is bound to its structural parent.
genValidBindingTree :: Int -> Gen Constraint
genValidBindingTree n
    | n <= 0 = return emptyConstraint
    | n == 1 = return emptyConstraint
        { cNodes = IntMap.singleton 0 (TyVar { tnId = NodeId 0, tnBound = Nothing })
        }
    | otherwise = do
        -- Generate a chain: forall(0) -> forall(1) -> ... -> forall(n-2) -> var(n-1)
        let nodes = IntMap.fromList $
                [(i, TyForall (NodeId i) (NodeId (i + 1))) | i <- [0..n-2]]
                ++ [(n-1, TyVar { tnId = NodeId (n - 1), tnBound = Nothing })]
        
        -- Generate binding parents: node i has parent i-1 (node 0 is root)
        -- This respects the "parent is upper than child" invariant because
        -- node i-1 can reach node i via its body pointer.
        flags <- vectorOf (n-1) (elements [BindFlex, BindRigid])
        let bindParents = IntMap.fromList
                [(i, (NodeId (i-1), flag)) | (i, flag) <- zip [1..n-1] flags]
        
        return Constraint
            { cNodes = nodes
            , cInstEdges = []
            , cUnifyEdges = []
            , cBindParents = bindParents
            , cPolySyms = Set.empty
            , cEliminatedVars = IntSet.empty
            }

-- | Generate a tree-shaped binding structure (not just a chain).
-- Uses TyForall nodes to create a tree structure where each node can have
-- at most one structural child, and binding edges can point to any ancestor.
-- This ensures the "parent is upper than child" invariant is satisfied.
genTreeBindingTree :: Int -> Gen Constraint
genTreeBindingTree n
    | n <= 0 = return emptyConstraint
    | n == 1 = return emptyConstraint
        { cNodes = IntMap.singleton 0 (TyVar { tnId = NodeId 0, tnBound = Nothing })
        }
    | otherwise = do
        -- Build a chain structure: forall(0) -> forall(1) -> ... -> var(n-1)
        -- Each node i > 0 has structural parent i-1 (via body pointer).
        let nodes = IntMap.fromList $
                [(i, TyForall (NodeId i) (NodeId (i + 1))) | i <- [0..n-2]]
                ++ [(n-1, TyVar { tnId = NodeId (n - 1), tnBound = Nothing })]
        
        -- Generate binding parents: each node i > 0 can have any ancestor [0..i-1] as binding parent.
        -- This creates a tree-shaped binding structure while respecting the "parent is upper" invariant.
        bindParents <- fmap IntMap.fromList $ forM [1..n-1] $ \i -> do
            -- Pick any ancestor as binding parent (all ancestors are "upper" than i)
            parentIdx <- choose (0, i-1)
            flag <- elements [BindFlex, BindRigid]
            return (i, (NodeId parentIdx, flag))
        
        return $
            emptyConstraint
                { cNodes = nodes
                , cBindParents = bindParents
                }

genOrderKeyDag :: Int -> Gen (NodeId, IntMap.IntMap TyNode)
genOrderKeyDag n
    | n <= 0 = do
        let root = NodeId 0
        pure (root, IntMap.singleton 0 (TyVar { tnId = root, tnBound = Nothing }))
    | n == 1 = do
        let root = NodeId 0
        pure (root, IntMap.singleton 0 (TyVar { tnId = root, tnBound = Nothing }))
    | otherwise = do
        nodes <- foldM (addNode n) IntMap.empty [0..n-1]
        pure (NodeId (n - 1), nodes)
  where
    addNode :: Int -> IntMap.IntMap TyNode -> Int -> Gen (IntMap.IntMap TyNode)
    addNode totalNodes nodes0 i = do
        let nid = NodeId i
        node <-
            if i == 0
                then genLeaf nid
                else if i == totalNodes - 1
                    then genNonLeaf i nid
                    else frequency
                        [ (3, genLeaf nid)
                        , (2, genNonLeaf i nid)
                        ]
        pure (IntMap.insert i node nodes0)

    genLeaf :: NodeId -> Gen TyNode
    genLeaf nid =
        elements
            [ TyVar { tnId = nid, tnBound = Nothing }
            , TyBase { tnId = nid, tnBase = BaseTy "Int" }
            , TyBottom { tnId = nid }
            ]

    genNonLeaf :: Int -> NodeId -> Gen TyNode
    genNonLeaf i nid = do
        let pickChild = NodeId <$> choose (0, i - 1)
        choice <- elements [0 :: Int, 1, 2]
        case choice of
            0 -> do
                dom <- pickChild
                cod <- pickChild
                pure $ TyArrow { tnId = nid, tnDom = dom, tnCod = cod }
            1 -> do
                body <- pickChild
                pure $ TyForall { tnId = nid, tnBody = body }
            _ -> do
                k <- choose (1, min 3 i)
                children <- vectorOf k pickChild
                pure $ TyRoot { tnId = nid, tnChildren = children }

mkOrderedBinderConstraint :: Int -> (Constraint, NodeId, NodeId)
mkOrderedBinderConstraint n =
    let count = max 1 n
        varIds = [1..count]
        arrowIds = [count + 1 .. (2 * count - 1)]
        (bodyRoot, arrowNodes) = buildChain varIds arrowIds
        binder = NodeId 0
        nodes =
            IntMap.fromList $
                [ (0, TyForall (NodeId 0) bodyRoot) ]
                ++ [ (i, TyVar { tnId = NodeId i, tnBound = Nothing }) | i <- varIds ]
                ++ arrowNodes
        bindParents =
            IntMap.fromList
                [ (i, (binder, BindFlex)) | i <- varIds ]
    in (emptyConstraint { cNodes = nodes, cBindParents = bindParents }, binder, bodyRoot)
  where
    buildChain [v] _ = (NodeId v, [])
    buildChain (v:vs) (a:as) =
        let (root, rest) = buildChain vs as
            node = (a, TyArrow (NodeId a) (NodeId v) root)
        in (NodeId a, node : rest)
    buildChain _ _ = (NodeId 1, [])

spec :: Spec
spec = do
    bindingTreeSpec
    bindingAdjustmentSpec

bindingTreeSpec :: Spec
bindingTreeSpec = describe "MLF.Binding.Tree" $ do
    describe "Basic operations" $ do
        it "lookupBindParent returns Nothing for root nodes" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.singleton 0 (TyVar { tnId = NodeId 0, tnBound = Nothing })
                    }
            lookupBindParent c (NodeId 0) `shouldBe` Nothing

        it "lookupBindParent returns the parent for non-root nodes" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        ]
                    , cBindParents = IntMap.singleton 1 (NodeId 0, BindFlex)
                    }
            lookupBindParent c (NodeId 1) `shouldBe` Just (NodeId 0, BindFlex)

        it "setBindParent adds a binding parent" $ do
            let c0 = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        ]
                    }
                c1 = setBindParent (NodeId 1) (NodeId 0, BindRigid) c0
            lookupBindParent c1 (NodeId 1) `shouldBe` Just (NodeId 0, BindRigid)

        it "removeBindParent removes a binding parent" $ do
            let c0 = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        ]
                    , cBindParents = IntMap.singleton 1 (NodeId 0, BindFlex)
                    }
                c1 = removeBindParent (NodeId 1) c0
            lookupBindParent c1 (NodeId 1) `shouldBe` Nothing

    describe "Root detection" $ do
        it "bindingRoots returns all nodes without parents" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        ]
                    , cBindParents = IntMap.singleton 1 (NodeId 0, BindFlex)
                    }
            -- Nodes 0 and 2 are roots (no binding parent)
            let roots = bindingRoots c
            length roots `shouldBe` 2
            NodeId 0 `elem` roots `shouldBe` True
            NodeId 2 `elem` roots `shouldBe` True

        it "isBindingRoot correctly identifies roots" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        ]
                    , cBindParents = IntMap.singleton 1 (NodeId 0, BindFlex)
                    }
            isBindingRoot c (NodeId 0) `shouldBe` True
            isBindingRoot c (NodeId 1) `shouldBe` False

    describe "Binder enumeration (Q(n))" $ do
        it "boundFlexChildren returns flex TyVar { tnId = children, tnBound = Nothing }" $ do
            let c =
                    emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (0, TyForall (NodeId 0) (NodeId 4))
                                , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                                , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                                , (3, TyBase (NodeId 3) (BaseTy "Int"))
                                , (4, TyArrow (NodeId 4) (NodeId 1) (NodeId 5))
                                , (5, TyArrow (NodeId 5) (NodeId 2) (NodeId 3))
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (1, (NodeId 0, BindFlex))
                                , (2, (NodeId 0, BindRigid))
                                , (3, (NodeId 0, BindFlex))
                                ]
                        }
            boundFlexChildren c (NodeId 0) `shouldBe` Right [NodeId 1]

        it "orderedBinders filters unreachable binders and sorts by order keys" $ do
            let c =
                    emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (0, TyForall (NodeId 0) (NodeId 4))
                                , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                                , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                                , (3, TyVar { tnId = NodeId 3, tnBound = Nothing }) -- unreachable from body
                                , (4, TyArrow (NodeId 4) (NodeId 1) (NodeId 2))
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (1, (NodeId 0, BindFlex))
                                , (2, (NodeId 0, BindFlex))
                                , (3, (NodeId 0, BindFlex))
                                ]
                        }
            orderedBinders id c (NodeId 0) `shouldBe` Right [NodeId 1, NodeId 2]

        it "forallSpecFromForall remaps binder bounds" $ do
            let c =
                    emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (0, TyForall (NodeId 0) (NodeId 3))
                                , (1, TyVar { tnId = NodeId 1, tnBound = Just (NodeId 2) })
                                , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                                , (3, TyArrow (NodeId 3) (NodeId 1) (NodeId 2))
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (1, (NodeId 0, BindFlex))
                                , (2, (NodeId 0, BindFlex))
                                ]
                        }
            forallSpecFromForall id c (NodeId 0)
                `shouldBe` Right (ForallSpec 2 [Nothing, Just (BoundBinder 0)])

    describe "Order keys (<P)" $ do
        it "prefers leftmost paths when branches diverge" $ do
            -- Root arrow: left child is shallow, right child contains a deeper var.
            -- Paths: left = [0], deep = [1,0] so left is ≺ deep.
            let root = NodeId 0
                left = NodeId 1
                right = NodeId 2
                deep = NodeId 3
                rightLeaf = NodeId 4
                nodes =
                    IntMap.fromList
                        [ (getNodeId root, TyArrow root left right)
                        , (getNodeId left, TyVar { tnId = left, tnBound = Nothing })
                        , (getNodeId right, TyArrow right deep rightLeaf)
                        , (getNodeId deep, TyVar { tnId = deep, tnBound = Nothing })
                        , (getNodeId rightLeaf, TyVar { tnId = rightLeaf, tnBound = Nothing })
                        ]
                keys = Order.orderKeysFromRootWith id nodes root Nothing
            Order.compareNodesByOrderKey keys left deep `shouldBe` LT

        it "selects the leftmost-lowermost path for shared nodes" $ do
            -- Shared node reachable via [0,0] and [1,0]; choose [0,0].
            let root = NodeId 0
                left = NodeId 1
                right = NodeId 2
                shared = NodeId 3
                leftLeaf = NodeId 4
                rightLeaf = NodeId 5
                nodes =
                    IntMap.fromList
                        [ (getNodeId root, TyArrow root left right)
                        , (getNodeId left, TyArrow left shared leftLeaf)
                        , (getNodeId right, TyArrow right shared rightLeaf)
                        , (getNodeId shared, TyVar { tnId = shared, tnBound = Nothing })
                        , (getNodeId leftLeaf, TyVar { tnId = leftLeaf, tnBound = Nothing })
                        , (getNodeId rightLeaf, TyVar { tnId = rightLeaf, tnBound = Nothing })
                        ]
                keys = Order.orderKeysFromRootWith id nodes root Nothing
            case IntMap.lookup (getNodeId shared) keys of
                Nothing -> expectationFailure "Expected order key for shared node"
                Just key -> Order.okPath key `shouldBe` [0, 0]

        it "assigns distinct <P keys to distinct reachable nodes" $ do
            -- Distinct nodes must not share the same minimal path.
            let root = NodeId 0
                left = NodeId 1
                right = NodeId 2
                shared = NodeId 3
                leftLeaf = NodeId 4
                rightLeaf = NodeId 5
                nodes =
                    IntMap.fromList
                        [ (getNodeId root, TyArrow root left right)
                        , (getNodeId left, TyArrow left shared leftLeaf)
                        , (getNodeId right, TyArrow right rightLeaf shared)
                        , (getNodeId shared, TyVar { tnId = shared, tnBound = Nothing })
                        , (getNodeId leftLeaf, TyVar { tnId = leftLeaf, tnBound = Nothing })
                        , (getNodeId rightLeaf, TyVar { tnId = rightLeaf, tnBound = Nothing })
                        ]
                keys = Order.orderKeysFromRootWith id nodes root Nothing
                paths = [ Order.okPath key | key <- IntMap.elems keys ]
            length paths `shouldBe` length (nub paths)

        it "assigns distinct <P keys to distinct reachable nodes (random DAGs)" $ property $
            forAll (choose (1, 25)) $ \n ->
                forAll (genOrderKeyDag n) $ \(root, nodes) ->
                    let keys = Order.orderKeysFromRootWith id nodes root Nothing
                        paths = [ Order.okPath key | key <- IntMap.elems keys ]
                    in length paths === length (nub paths)

    describe "Path operations" $ do
        it "bindingPathToRoot returns path from node to root" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        ]
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindFlex))
                        , (2, (NodeId 1, BindFlex))
                        ]
                    }
            bindingPathToRoot c (NodeId 2) `shouldBe` Right [NodeId 2, NodeId 1, NodeId 0]
            bindingPathToRoot c (NodeId 1) `shouldBe` Right [NodeId 1, NodeId 0]
            bindingPathToRoot c (NodeId 0) `shouldBe` Right [NodeId 0]

        it "bindingPathToRoot detects cycles" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        ]
                    , cBindParents = IntMap.fromList
                        [ (0, (NodeId 1, BindFlex))
                        , (1, (NodeId 0, BindFlex))
                        ]
                    }
            case bindingPathToRoot c (NodeId 0) of
                Left (BindingCycleDetected _) -> return ()
                other -> expectationFailure $ "Expected cycle error, got: " ++ show other

        it "bindingLCA finds lowest common ancestor" $ do
            -- Tree structure:
            --       0
            --      / \
            --     1   2
            --    /
            --   3
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        , (3, TyVar { tnId = NodeId 3, tnBound = Nothing })
                        ]
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindFlex))
                        , (2, (NodeId 0, BindFlex))
                        , (3, (NodeId 1, BindFlex))
                        ]
                    }
            bindingLCA c (NodeId 3) (NodeId 2) `shouldBe` Right (NodeId 0)
            bindingLCA c (NodeId 3) (NodeId 1) `shouldBe` Right (NodeId 1)
            bindingLCA c (NodeId 1) (NodeId 2) `shouldBe` Right (NodeId 0)

        it "ensureConstraintRoot makes LCA total for disconnected components" $ do
            let c =
                    emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (0, TyForall (NodeId 0) (NodeId 1))
                                , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                                , (2, TyForall (NodeId 2) (NodeId 3))
                                , (3, TyVar { tnId = NodeId 3, tnBound = Nothing })
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (1, (NodeId 0, BindFlex))
                                , (3, (NodeId 2, BindFlex))
                                ]
                        }
            bindingLCA c (NodeId 1) (NodeId 3)
                `shouldBe` Left (NoCommonAncestor (NodeId 1) (NodeId 3))

            let c' = ConstraintRoot.ensureConstraintRoot c
            case ConstraintRoot.findConstraintRoot c' of
                Nothing -> expectationFailure "Expected a synthetic root after ensureConstraintRoot"
                Just rootId ->
                    bindingLCA c' (NodeId 1) (NodeId 3) `shouldBe` Right rootId

    describe "Node kind classification (paper §3.1)" $ do
        it "distinguishes Restricted vs Locked (and strict under-rigid)" $ do
            -- Term-DAG chain: forall(0) -> forall(1) -> forall(2) -> var(3)
            --
            -- Binding edges:
            --   1 -> 0 (rigid)   => 1 is Restricted
            --   2 -> 1 (flex)    => 2 is Locked (ancestor rigid at 1)
            --   3 -> 0 (flex)    => 3 is Instantiable
            let c =
                    emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (0, TyForall (NodeId 0) (NodeId 1))
                                , (1, TyForall (NodeId 1) (NodeId 2))
                                , (2, TyForall (NodeId 2) (NodeId 3))
                                , (3, TyVar { tnId = NodeId 3, tnBound = Nothing })
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (1, (NodeId 0, BindRigid))
                                , (2, (NodeId 1, BindFlex))
                                , (3, (NodeId 0, BindFlex))
                                ]
                        }

            nodeKind c (NodeId 0) `shouldBe` Right NodeRoot
            nodeKind c (NodeId 1) `shouldBe` Right NodeRestricted
            nodeKind c (NodeId 2) `shouldBe` Right NodeLocked
            nodeKind c (NodeId 3) `shouldBe` Right NodeInstantiable

            -- Under-rigid considers only strict ancestors (so restricted nodes are
            -- not “under rigid” solely because their own edge is rigid).
            isUnderRigidBinder c (NodeId 1) `shouldBe` Right False
            isUnderRigidBinder c (NodeId 2) `shouldBe` Right True

    describe "Interior computation" $ do
        it "interiorOf returns all nodes bound to root" $ do
            -- Tree structure:
            --       0
            --      / \
            --     1   2
            --    /
            --   3
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        , (3, TyVar { tnId = NodeId 3, tnBound = Nothing })
                        ]
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindFlex))
                        , (2, (NodeId 0, BindFlex))
                        , (3, (NodeId 1, BindFlex))
                        ]
                    }
            -- Interior of node 0 should be all nodes
            case interiorOf c (NodeId 0) of
                Right interior -> do
                    IntSet.size interior `shouldBe` 4
                    IntSet.member 0 interior `shouldBe` True
                    IntSet.member 1 interior `shouldBe` True
                    IntSet.member 2 interior `shouldBe` True
                    IntSet.member 3 interior `shouldBe` True
                Left err -> expectationFailure $ "Expected success, got: " ++ show err

            -- Interior of node 1 should be {1, 3}
            case interiorOf c (NodeId 1) of
                Right interior -> do
                    IntSet.size interior `shouldBe` 2
                    IntSet.member 1 interior `shouldBe` True
                    IntSet.member 3 interior `shouldBe` True
                Left err -> expectationFailure $ "Expected success, got: " ++ show err

    describe "Interior computation (quotient)" $ do
        it "interiorOfUnder collapses ids under canonicalization (Binding.interiorOfUnder)" $ do
            -- Term-DAG chain: forall(0) -> forall(1) -> var(2)
            -- Binding edges: 1 -> 0, 2 -> 1
            -- Canonicalization merges 2 into 1, turning 2's binding edge into a self-edge.
            let c =
                    emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (0, TyForall (NodeId 0) (NodeId 1))
                                , (1, TyForall (NodeId 1) (NodeId 2))
                                , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (1, (NodeId 0, BindFlex))
                                , (2, (NodeId 1, BindFlex))
                                ]
                        }

                canonical nid =
                    case nid of
                        NodeId 2 -> NodeId 1
                        other -> other

            interiorOfUnder canonical c (NodeId 0) `shouldBe` Right (IntSet.fromList [0, 1])

        it "interiorOfUnder ignores self-edges induced by canonicalization (Binding.interiorOfUnder)" $ do
            -- Two nodes with a single binding edge 1 -> 0. Canonicalization merges 1 into 0,
            -- turning the binding edge into a self-edge that must be dropped.
            let c =
                    emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
                                , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                                ]
                        , cBindParents = IntMap.singleton 1 (NodeId 0, BindFlex)
                        }

                canonical nid =
                    case nid of
                        NodeId 1 -> NodeId 0
                        other -> other

            interiorOfUnder canonical c (NodeId 0) `shouldBe` Right (IntSet.fromList [0])

    describe "Invariant checking" $ do
        it "checkBindingTree succeeds for empty constraint" $ do
            checkBindingTree emptyConstraint `shouldBe` Right ()

        it "checkBindingTree succeeds for valid single-node constraint" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.singleton 0 (TyVar { tnId = NodeId 0, tnBound = Nothing })
                    }
            checkBindingTree c `shouldBe` Right ()

        it "checkBindingTree succeeds for valid chain" $ do
            -- Create a term-DAG chain: forall(0) -> forall(1) -> var(2)
            -- Binding tree: 2 -> 1 -> 0 (each child bound to its structural parent)
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyForall (NodeId 0) (NodeId 1))
                        , (1, TyForall (NodeId 1) (NodeId 2))
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        ]
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindFlex))  -- 1's parent is 0 (0 can reach 1)
                        , (2, (NodeId 1, BindFlex))  -- 2's parent is 1 (1 can reach 2)
                        ]
                    }
            checkBindingTree c `shouldBe` Right ()

        it "checkBindingTree detects cycles" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar { tnId = NodeId 0, tnBound = Nothing })
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        ]
                    , cBindParents = IntMap.fromList
                        [ (0, (NodeId 1, BindFlex))
                        , (1, (NodeId 0, BindFlex))
                        ]
                    }
            case checkBindingTree c of
                Left (BindingCycleDetected _) -> return ()
                other -> expectationFailure $ "Expected cycle error, got: " ++ show other

        it "checkBindingTree detects missing parent nodes" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.singleton 1 (TyVar { tnId = NodeId 1, tnBound = Nothing })
                    , cBindParents = IntMap.singleton 1 (NodeId 99, BindFlex)  -- 99 doesn't exist
                    }
            case checkBindingTree c of
                Left (InvalidBindingTree _) -> return ()
                other -> expectationFailure $ "Expected invalid binding tree error, got: " ++ show other

        it "checkBindingTree detects parent not upper than child" $ do
            -- Create a term-DAG where node 1 is NOT structurally reachable from node 2
            -- Structure: node 0 is an arrow with dom=1, cod=2
            -- But we set node 1's binding parent to node 2 (which is NOT upper than node 1)
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyArrow (NodeId 0) (NodeId 1) (NodeId 2))
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        ]
                    -- Node 1's parent is node 2, but node 2 cannot reach node 1 via structure edges
                    , cBindParents = IntMap.singleton 1 (NodeId 2, BindFlex)
                    }
            case checkBindingTree c of
                Left (ParentNotUpper (NodeId 1) (NodeId 2)) -> return ()
                other -> expectationFailure $ "Expected ParentNotUpper error, got: " ++ show other

        it "checkBindingTree succeeds when parent is upper than child" $ do
            -- Create a term-DAG where node 0 is an arrow with dom=1, cod=2
            -- Node 1's binding parent is node 0 (which IS upper than node 1)
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyArrow (NodeId 0) (NodeId 1) (NodeId 2))
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        ]
                    -- Node 1's parent is node 0, and node 0 CAN reach node 1 via structure edges
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindFlex))
                        , (2, (NodeId 0, BindFlex))
                        ]
                    }
            checkBindingTree c `shouldBe` Right ()

        it "checkBindingTree detects missing binding parent in binding-edge mode" $ do
            -- Create a term-DAG where node 0 is an arrow with dom=1, cod=2
            -- Node 1 has a binding parent, but node 2 does NOT (missing)
            -- Since cBindParents is non-empty, we're in "binding-edge mode"
            -- and should detect the missing parent for node 2
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyArrow (NodeId 0) (NodeId 1) (NodeId 2))
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        ]
                    -- Only node 1 has a binding parent; node 2 is missing
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindFlex))
                        -- node 2 is missing!
                        ]
                    }
            case checkBindingTree c of
                Left (MissingBindParent (NodeId 2)) -> return ()
                other -> expectationFailure $ "Expected MissingBindParent error for node 2, got: " ++ show other

        it "checkBindingTree rejects missing parents even when cBindParents is empty" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyArrow (NodeId 0) (NodeId 1) (NodeId 2))
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        ]
                    , cBindParents = IntMap.empty
                    }
            case checkBindingTree c of
                Left (MissingBindParent (NodeId 1)) -> return ()
                other -> expectationFailure $ "Expected MissingBindParent error for node 1, got: " ++ show other

    describe "isUpper predicate" $ do
        it "isUpper returns True for reflexive case (node is upper than itself)" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.singleton 0 (TyVar { tnId = NodeId 0, tnBound = Nothing })
                    }
            isUpper c (NodeId 0) (NodeId 0) `shouldBe` True

        it "isUpper returns True when parent can reach child via TyArrow" $ do
            -- Structure: node 0 is an arrow with dom=1, cod=2
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyArrow (NodeId 0) (NodeId 1) (NodeId 2))
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        ]
                    }
            isUpper c (NodeId 0) (NodeId 1) `shouldBe` True
            isUpper c (NodeId 0) (NodeId 2) `shouldBe` True

        it "isUpper returns False when parent cannot reach child" $ do
            -- Structure: node 0 is an arrow with dom=1, cod=2
            -- Node 1 cannot reach node 2 (they are siblings)
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyArrow (NodeId 0) (NodeId 1) (NodeId 2))
                        , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        ]
                    }
            isUpper c (NodeId 1) (NodeId 2) `shouldBe` False
            isUpper c (NodeId 2) (NodeId 1) `shouldBe` False

        it "isUpper returns True for transitive reachability via TyForall" $ do
            -- Structure: node 0 is a forall with body=1, node 1 is an arrow with dom=2, cod=3
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyForall (NodeId 0) (NodeId 1))
                        , (1, TyArrow (NodeId 1) (NodeId 2) (NodeId 3))
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        , (3, TyVar { tnId = NodeId 3, tnBound = Nothing })
                        ]
                    }
            -- Node 0 can reach node 2 transitively: 0 -> 1 -> 2
            isUpper c (NodeId 0) (NodeId 2) `shouldBe` True
            isUpper c (NodeId 0) (NodeId 3) `shouldBe` True
            -- Node 1 can reach node 2 directly
            isUpper c (NodeId 1) (NodeId 2) `shouldBe` True

        it "isUpper handles shared subgraphs correctly" $ do
            -- Structure: nodes 0 and 1 are arrows that share node 2 as their domain
            -- 0 -> dom=2, cod=3
            -- 1 -> dom=2, cod=4
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyArrow (NodeId 0) (NodeId 2) (NodeId 3))
                        , (1, TyArrow (NodeId 1) (NodeId 2) (NodeId 4))
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        , (3, TyVar { tnId = NodeId 3, tnBound = Nothing })
                        , (4, TyVar { tnId = NodeId 4, tnBound = Nothing })
                        ]
                    }
            -- Both 0 and 1 can reach node 2
            isUpper c (NodeId 0) (NodeId 2) `shouldBe` True
            isUpper c (NodeId 1) (NodeId 2) `shouldBe` True
            -- But 0 cannot reach 4, and 1 cannot reach 3
            isUpper c (NodeId 0) (NodeId 4) `shouldBe` False
            isUpper c (NodeId 1) (NodeId 3) `shouldBe` False

    describe "Property tests" $ do
        -- **Feature: paper_general_raise_plan, Property 1: Valid binding trees pass checkBindingTree**
        -- **Validates: Requirements 2.1, 7.1**
        it "valid chain binding trees pass checkBindingTree" $ property $
            forAll (choose (1, 20)) $ \n -> do
                c <- generate (genValidBindingTree n)
                checkBindingTree c `shouldBe` Right ()

        -- **Feature: paper_general_raise_plan, Property 2: Valid tree binding structures pass checkBindingTree**
        -- **Validates: Requirements 2.1, 7.1**
        it "valid tree binding structures pass checkBindingTree" $ property $
            forAll (choose (1, 20)) $ \n -> do
                c <- generate (genTreeBindingTree n)
                checkBindingTree c `shouldBe` Right ()

        -- **Feature: paper_general_raise_plan, Property 3: Binding roots are exactly nodes without parents**
        -- **Validates: Requirements 1.4**
        it "binding roots are exactly nodes without parents" $ property $
            forAll (choose (1, 20)) $ \n -> do
                c <- generate (genTreeBindingTree n)
                let roots = bindingRoots c
                    allNodes = allNodeIds c
                -- Every root should not have a parent
                forM_ roots $ \r -> do
                    lookupBindParent c r `shouldBe` Nothing
                -- Every non-root should have a parent
                forM_ allNodes $ \nid -> do
                    if nid `elem` roots
                        then lookupBindParent c nid `shouldBe` Nothing
                        else lookupBindParent c nid `shouldSatisfy` isJust

        -- **Feature: paper_general_raise_plan, Property 4: Path to root terminates for valid trees**
        -- **Validates: Requirements 2.1**
        it "path to root terminates for all nodes in valid trees" $ property $
            forAll (choose (1, 20)) $ \n -> do
                c <- generate (genTreeBindingTree n)
                let allNodes = allNodeIds c
                forM_ allNodes $ \nid -> do
                    case bindingPathToRoot c nid of
                        Right path -> do
                            -- Path should start with the node itself
                            case path of
                                (first:_) -> first `shouldBe` nid
                                [] -> expectationFailure "Path should not be empty"
                            -- Path should end with a root
                            isBindingRoot c (last path) `shouldBe` True
                        Left err -> expectationFailure $ "Expected path, got: " ++ show err

        -- **Feature: paper_general_raise_plan, Property 5: Constraints from generateConstraints have binding edges for all non-root nodes**
        -- **Validates: Requirements 1.1, 1.2, 1.4, 7.1**
        it "constraints from generateConstraints have binding edges for all non-root nodes" $ do
            -- Test with various expressions
            let testExprs =
                    [ ("literal", ELit (LInt 42))
                    , ("identity", ELam "x" (EVar "x"))
                    , ("let id", ELet "id" (ELam "x" (EVar "x")) (EVar "id"))
                    , ("app", EApp (ELam "x" (EVar "x")) (ELit (LInt 1)))
                    , ("let app", ELet "f" (ELam "x" (EVar "x")) (EApp (EVar "f") (ELit (LInt 1))))
                    -- Test cases that exercise internalizeSrcType with STArrow/STForall
                    , ("term annotation with arrow", EAnn (ELam "x" (EVar "x")) (STArrow (STBase "Int") (STBase "Int")))
                    , ("term annotation with forall", EAnn (ELam "x" (EVar "x")) (STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))))
                    , ("let with annotated scheme", ELetAnn "id" (SrcScheme [("a", Nothing)] (STArrow (STVar "a") (STVar "a"))) (ELam "x" (EVar "x")) (EVar "id"))
                    , ("nested forall annotation", EAnn (ELam "x" (EVar "x")) (STForall "a" Nothing (STForall "b" Nothing (STArrow (STVar "a") (STVar "b")))))
                    ]
            forM_ testExprs $ \(name, expr) -> do
                case generateConstraintsDefault expr of
                    Left err -> expectationFailure $ name ++ ": Constraint generation failed: " ++ show err
                    Right result -> do
                        let c = crConstraint result
                            roots = bindingRoots c
                            allNodes = allNodeIds c
                            nonRoots = filter (`notElem` roots) allNodes
                        -- Every non-root node should have a binding parent
                        forM_ nonRoots $ \nid -> do
                            case lookupBindParent c nid of
                                Nothing -> expectationFailure $
                                    name ++ ": Node " ++ show nid ++ " has no binding parent but is not a root"
                                Just _ -> pure ()
                        -- The binding tree should be valid
                        case checkBindingTree c of
                            Right () -> pure ()
                            Left err -> expectationFailure $
                                name ++ ": checkBindingTree failed: " ++ show err ++
                                "\nBindParents: " ++ show (cBindParents c) ++
                                "\nNodes: " ++ show (IntMap.keys (cNodes c))

        -- **Feature: paper_general_raise_plan, Property 6: interiorOf equals nodes whose binding path reaches root**
        -- **Validates: Requirements 3.2, 3.3, 7.2**
        it "interiorOf equals nodes whose binding path reaches root" $ property $
            forAll (choose (1, 20)) $ \n -> do
                c <- generate (genTreeBindingTree n)
                let roots = bindingRoots c
                    allNodes = allNodeIds c
                -- For each root, verify interiorOf matches manual computation
                forM_ roots $ \root -> do
                    case interiorOf c root of
                        Right interior -> do
                            -- Manually compute interior: all nodes whose path includes root
                            let manualInterior = IntSet.fromList
                                    [ getNodeId nid
                                    | nid <- allNodes
                                    , case bindingPathToRoot c nid of
                                        Right path -> root `elem` path
                                        Left _ -> False
                                    ]
                            interior `shouldBe` manualInterior
                        Left err -> expectationFailure $ "interiorOf failed: " ++ show err

        -- **Feature: paper_general_raise_plan, Property 7: interiorOf always includes the root itself**
        -- **Validates: Requirements 3.2, 3.3**
        it "interiorOf always includes the root itself" $ property $
            forAll (choose (1, 20)) $ \n -> do
                c <- generate (genTreeBindingTree n)
                let allNodes = allNodeIds c
                -- For each node, verify interiorOf includes the node itself
                forM_ allNodes $ \nid -> do
                    case interiorOf c nid of
                        Right interior -> 
                            IntSet.member (getNodeId nid) interior `shouldBe` True
                        Left _ -> pure ()  -- Error is acceptable for some nodes

        -- **Feature: paper_general_raise_plan, Property 8: interiorOf is monotonic (children's interior is subset of parent's)**
        -- **Validates: Requirements 3.2, 3.3**
        it "interiorOf is monotonic (children's interior is subset of parent's)" $ property $
            forAll (choose (2, 15)) $ \n -> do
                c <- generate (genTreeBindingTree n)
                let allNodes = allNodeIds c
                -- For each non-root node, verify its interior is subset of parent's interior
                forM_ allNodes $ \nid -> do
                    case lookupBindParent c nid of
                        Nothing -> pure ()  -- Root node, skip
                        Just (parentId, _) -> do
                            case (interiorOf c nid, interiorOf c parentId) of
                                (Right childInterior, Right parentInterior) ->
                                    IntSet.isSubsetOf childInterior parentInterior `shouldBe` True
                                _ -> pure ()  -- Error is acceptable

        it "interiorOfUnder matches quotient binding-path characterization (interiorOfUnder property)" $ property $
            forAll (choose (2, 20)) $ \n -> do
                c <- generate (genTreeBindingTree n)

                repMap <- generate $ do
                    let nodeIds = IntMap.keys (cNodes c)
                        parentKey i =
                            case lookupBindParent c (NodeId i) of
                                Nothing -> (-1)
                                Just (p, _) -> getNodeId p
                        groups = IntMap.fromListWith (++) [ (parentKey i, [i]) | i <- nodeIds ]
                    choices <- forM (IntMap.elems groups) $ \group -> do
                        rep <- elements group
                        pure [ (i, rep) | i <- group ]
                    pure (IntMap.fromList (concat choices))

                let canonical (NodeId i) = NodeId (IntMap.findWithDefault i i repMap)

                r0 <- generate $ elements (IntMap.keys (cNodes c))
                let rootC = canonical (NodeId r0)
                    rootCId = getNodeId rootC

                    canonIds =
                        IntSet.fromList
                            [ getNodeId (canonical (NodeId i))
                            | i <- IntMap.keys (cNodes c)
                            ]

                    quotientParents =
                        foldl'
                            (\m (childId, (parent0, _flag)) ->
                                let childC = getNodeId (canonical (NodeId childId))
                                    parentC = getNodeId (canonical parent0)
                                in if childC == parentC
                                    then m
                                    else case IntMap.lookup childC m of
                                        Nothing -> IntMap.insert childC parentC m
                                        Just p
                                            | p == parentC -> m
                                            | otherwise -> IntMap.insert childC parentC m
                            )
                            IntMap.empty
                            (IntMap.toList (cBindParents c))

                    hasRootOnPath nid0 = go IntSet.empty nid0
                      where
                        go seen nid
                            | nid == rootCId = True
                            | IntSet.member nid seen = False
                            | otherwise =
                                case IntMap.lookup nid quotientParents of
                                    Nothing -> False
                                    Just p -> go (IntSet.insert nid seen) p

                    manualInterior = IntSet.filter hasRootOnPath canonIds

                case interiorOfUnder canonical c rootC of
                    Left err -> expectationFailure ("interiorOfUnder failed: " ++ show err)
                    Right interior -> interior `shouldBe` manualInterior

        it "orderedBinders returns a list sorted by order keys" $ property $
            forAll (choose (1, 6)) $ \n -> do
                let (c, binder, bodyRoot) = mkOrderedBinderConstraint n
                    orderKeys = Order.orderKeysFromRootWith id (cNodes c) bodyRoot Nothing
                    isSorted [] = True
                    isSorted [_] = True
                    isSorted (x:y:rest) =
                        Order.compareNodesByOrderKey orderKeys x y /= GT && isSorted (y:rest)
                case orderedBinders id c binder of
                    Left err -> expectationFailure ("orderedBinders failed: " ++ show err)
                    Right binders -> isSorted binders `shouldBe` True

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

-- | Generate a constraint for testing harmonization.
genConstraintWithBinderChain :: Int -> Gen Constraint
genConstraintWithBinderChain n
    | n <= 0 = return emptyConstraint
    | n == 1 =
        return $
            emptyConstraint
                { cNodes = IntMap.singleton 0 (TyVar { tnId = NodeId 0, tnBound = Nothing })
                }
    | otherwise =
        let nodes =
                IntMap.fromList $
                    [(i, TyForall (NodeId i) (NodeId (i + 1))) | i <- [0..n-2]]
                        ++ [(n-1, TyVar { tnId = NodeId (n - 1), tnBound = Nothing })]
            bindParents =
                IntMap.fromList
                    [(i, (NodeId (i - 1), BindFlex)) | i <- [1..n-1]]
        in return $
            emptyConstraint
                { cNodes = nodes
                , cBindParents = bindParents
                }


-- | Tests for MLF.Binding.Adjustment
bindingAdjustmentSpec :: Spec
bindingAdjustmentSpec = describe "MLF.Binding.Adjustment" $ do
    describe "harmonizeBindParentsWithTrace" $ do
        -- **Feature: paper_general_raise_plan, Property 11: Harmonization preserves checkBindingTree**
        -- **Validates: Requirements 1.2, 2.3, 7.2**
        it "harmonization preserves checkBindingTree" $ property $
            forAll (choose (3, 15)) $ \n -> do
                c <- generate (genConstraintWithBinderChain n)
                -- Pick two non-root nodes to harmonize
                let nonRoots = filter (not . isBindingRoot c) (allNodeIds c)
                case nonRoots of
                    (n1:n2:_) -> do
                        case BindingAdjustment.harmonizeBindParentsWithTrace n1 n2 c of
                            Right (c', _trace) -> 
                                checkBindingTree c' `shouldBe` Right ()
                            Left _ -> return ()  -- Error is acceptable for some inputs
                    _ -> return ()  -- Not enough non-roots to test

        -- **Feature: paper_general_raise_plan, Property 12: Harmonization trace is consistent with replaying Raise steps**
        -- **Validates: Requirements 2.3, 4.2, 4.3, 7.2**
        it "trace is consistent with replaying Raise steps" $ property $
            forAll (choose (3, 15)) $ \n -> do
                c <- generate (genConstraintWithBinderChain n)
                -- Pick two non-root nodes to harmonize
                let nonRoots = filter (not . isBindingRoot c) (allNodeIds c)
                case nonRoots of
                    (n1:n2:_) -> do
                        case BindingAdjustment.harmonizeBindParentsWithTrace n1 n2 c of
                            Right (c', trace) -> do
                                -- Replay the trace by applying Raise steps to the original constraint
                                let replayRaises constraint [] = Right constraint
                                    replayRaises constraint (nid:rest) =
                                        case GraphOps.applyRaiseStep nid constraint of
                                            Right (c'', Just _) -> replayRaises c'' rest
                                            Right (_c'', Nothing) -> Left (RaiseNotPossible nid)
                                            Left err -> Left err
                                
                                case replayRaises c trace of
                                    Right cReplayed -> do
                                        -- Assert the replayed binding-parent map equals the harmonized
                                        -- constraint's binding-parent map for nodes mentioned in the trace
                                        -- plus the two harmonized nodes
                                        let nodesToCheck = IntSet.toList $ IntSet.fromList $
                                                [getNodeId n1, getNodeId n2] ++ map getNodeId trace
                                        forM_ nodesToCheck $ \nidInt -> do
                                            let nid = NodeId nidInt
                                            let replayedParent = lookupBindParent cReplayed nid
                                                harmonizedParent = lookupBindParent c' nid
                                            replayedParent `shouldBe` harmonizedParent
                                    Left (err :: BindingError) -> 
                                        expectationFailure $ "Replay failed: " ++ show err
                            Left _ -> return ()  -- Error is acceptable for some inputs
                    _ -> return ()

        it "fails fast when raising a locked node is required" $ do
            -- Term-DAG chain: forall(0) -> forall(1) -> forall(2) -> var(3)
            --
            -- Binding edges:
            --   1 -> 0 (rigid)
            --   2 -> 1 (flex)    => 2 is locked
            --   3 -> 0 (flex)
            --
            -- Harmonizing 2 with 3 requires raising 2 to be bound on 0, which is
            -- disallowed because 2 is locked.
            let c =
                    emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (0, TyForall (NodeId 0) (NodeId 1))
                                , (1, TyForall (NodeId 1) (NodeId 2))
                                , (2, TyForall (NodeId 2) (NodeId 3))
                                , (3, TyVar { tnId = NodeId 3, tnBound = Nothing })
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (1, (NodeId 0, BindRigid))
                                , (2, (NodeId 1, BindFlex))
                                , (3, (NodeId 0, BindFlex))
                                ]
                        }
            BindingAdjustment.harmonizeBindParentsWithTrace (NodeId 2) (NodeId 3) c
                `shouldBe` Left (OperationOnLockedNode (NodeId 2))

        it "fails when no binding LCA exists" $ do
            -- Two disconnected term-DAG roots (0 and 2), so their binding roots
            -- have no common ancestor. This is a binding-tree error: the paper’s
            -- Raise-to-LCA model requires an LCA to exist when harmonization is needed.
            let c =
                    emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (0, TyForall (NodeId 0) (NodeId 1))
                                , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                                , (2, TyForall (NodeId 2) (NodeId 3))
                                , (3, TyVar { tnId = NodeId 3, tnBound = Nothing })
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (1, (NodeId 0, BindFlex))
                                , (3, (NodeId 2, BindFlex))
                                ]
                        }
            BindingAdjustment.harmonizeBindParentsWithTrace (NodeId 1) (NodeId 3) c
                `shouldBe` Left (NoCommonAncestor (NodeId 0) (NodeId 2))

        it "harmonization with empty binding edges yields an empty Raise trace" $ do
            let nodes =
                    IntMap.fromList
                        [ (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                        , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                        , (3, TyVar { tnId = NodeId 3, tnBound = Nothing })
                        ]
                c = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = IntMap.empty  -- No binding parents
                    }
            
            case BindingAdjustment.harmonizeBindParentsWithTrace (NodeId 1) (NodeId 3) c of
                Right (_c', trace) -> trace `shouldBe` []
                Left err -> expectationFailure $ "Expected success, got: " ++ show err
