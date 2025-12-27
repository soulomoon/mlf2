{-# LANGUAGE ScopedTypeVariables #-}
module GraphOpsSpec (spec) where

import Control.Monad (forM)
import qualified Data.IntMap.Strict as IntMap
import Test.Hspec
import Test.QuickCheck

import MLF.Constraint.Types
import MLF.Binding.Tree
import MLF.Binding.GraphOps
import SpecUtil (emptyConstraint)

-- | Generate a valid chain binding tree with n nodes.
-- Node 0 is root, node i has parent i-1, all edges are flexible.
-- Uses TyForall nodes to create a chain structure where each node can reach
-- the next via its body pointer, satisfying the "parent is upper" invariant.
genFlexChain :: Int -> Gen Constraint
genFlexChain n
    | n <= 0 = return emptyConstraint
    | n == 1 = return emptyConstraint
        { cNodes = IntMap.singleton 0 (TyVar (NodeId 0))
        }
    | otherwise = do
        -- Create a chain: forall(0) -> forall(1) -> ... -> var(n-1)
        let nodes = IntMap.fromList $
                [(i, TyForall (NodeId i) (NodeId (i + 1))) | i <- [0..n-2]]
                ++ [(n-1, TyVar (NodeId (n-1)))]
            bindParents = IntMap.fromList
                [(i, (NodeId (i-1), BindFlex)) | i <- [1..n-1]]
        pure emptyConstraint
            { cNodes = nodes
            , cBindParents = bindParents
            }

-- | Generate a valid tree with all flexible edges.
-- Uses TyForall nodes to create a chain structure, with binding edges that
-- can point to any ancestor, satisfying the "parent is upper" invariant.
genAllFlexTree :: Int -> Gen Constraint
genAllFlexTree n
    | n <= 0 = return emptyConstraint
    | n == 1 = return emptyConstraint
        { cNodes = IntMap.singleton 0 (TyVar (NodeId 0))
        }
    | otherwise = do
        -- Create a chain: forall(0) -> forall(1) -> ... -> var(n-1)
        let nodes = IntMap.fromList $
                [(i, TyForall (NodeId i) (NodeId (i + 1))) | i <- [0..n-2]]
                ++ [(n-1, TyVar (NodeId (n-1)))]
        -- Each node i > 0 can have any ancestor [0..i-1] as binding parent
        bindParents <- fmap IntMap.fromList $ forM [1..n-1] $ \i -> do
            parentIdx <- choose (0, i-1)
            return (i, (NodeId parentIdx, BindFlex))
        pure emptyConstraint
            { cNodes = nodes
            , cBindParents = bindParents
            }

spec :: Spec
spec = describe "MLF.Binding.GraphOps" $ do
    describe "Basic operations" $ do
        it "getBindFlag returns Nothing for root nodes" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.singleton 0 (TyVar (NodeId 0))
                    }
            getBindFlag c (NodeId 0) `shouldBe` Nothing

        it "getBindFlag returns the flag for non-root nodes" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar (NodeId 0))
                        , (1, TyVar (NodeId 1))
                        ]
                    , cBindParents = IntMap.singleton 1 (NodeId 0, BindFlex)
                    }
            getBindFlag c (NodeId 1) `shouldBe` Just BindFlex

    describe "isInstantiable" $ do
        it "returns False for root nodes" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.singleton 0 (TyVar (NodeId 0))
                    }
            isInstantiable c (NodeId 0) `shouldBe` Right False

        it "returns True for flexibly bound nodes with all-flex path" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar (NodeId 0))
                        , (1, TyVar (NodeId 1))
                        , (2, TyVar (NodeId 2))
                        ]
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindFlex))
                        , (2, (NodeId 1, BindFlex))
                        ]
                    }
            isInstantiable c (NodeId 2) `shouldBe` Right True
            isInstantiable c (NodeId 1) `shouldBe` Right True

        it "returns False for nodes with rigid edge in path" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar (NodeId 0))
                        , (1, TyVar (NodeId 1))
                        , (2, TyVar (NodeId 2))
                        ]
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindRigid))  -- Rigid edge
                        , (2, (NodeId 1, BindFlex))
                        ]
                    }
            isInstantiable c (NodeId 2) `shouldBe` Right False
            isInstantiable c (NodeId 1) `shouldBe` Right False

    describe "applyWeaken" $ do
        it "changes flexible to rigid" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar (NodeId 0))
                        , (1, TyVar (NodeId 1))
                        ]
                    , cBindParents = IntMap.singleton 1 (NodeId 0, BindFlex)
                    }
            case applyWeaken (NodeId 1) c of
                Right (c', op) -> do
                    op `shouldBe` OpWeaken (NodeId 1)
                    getBindFlag c' (NodeId 1) `shouldBe` Just BindRigid
                Left err -> expectationFailure $ "Expected success, got: " ++ show err

        it "fails on root nodes" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.singleton 0 (TyVar (NodeId 0))
                    }
            case applyWeaken (NodeId 0) c of
                Left (MissingBindParent _) -> return ()
                other -> expectationFailure $ "Expected MissingBindParent, got: " ++ show other

        it "fails on already rigid nodes" $ do
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar (NodeId 0))
                        , (1, TyVar (NodeId 1))
                        ]
                    , cBindParents = IntMap.singleton 1 (NodeId 0, BindRigid)
                    }
            case applyWeaken (NodeId 1) c of
                Left (OperationOnLockedNode _) -> return ()
                other -> expectationFailure $ "Expected OperationOnLockedNode, got: " ++ show other

    describe "applyRaiseStep" $ do
        it "moves binding edge one step toward root" $ do
            -- Chain: 0 <- 1 <- 2
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar (NodeId 0))
                        , (1, TyVar (NodeId 1))
                        , (2, TyVar (NodeId 2))
                        ]
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindFlex))
                        , (2, (NodeId 1, BindFlex))
                        ]
                    }
            case applyRaiseStep (NodeId 2) c of
                Right (c', Just op) -> do
                    op `shouldBe` OpRaise (NodeId 2)
                    -- Node 2 should now have parent 0
                    lookupBindParent c' (NodeId 2) `shouldBe` Just (NodeId 0, BindFlex)
                Right (_, Nothing) -> expectationFailure "Expected raise to succeed"
                Left err -> expectationFailure $ "Expected success, got: " ++ show err

        it "returns Nothing when parent is already root" $ do
            -- Chain: 0 <- 1
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar (NodeId 0))
                        , (1, TyVar (NodeId 1))
                        ]
                    , cBindParents = IntMap.singleton 1 (NodeId 0, BindFlex)
                    }
            case applyRaiseStep (NodeId 1) c of
                Right (c', Nothing) -> do
                    -- Constraint should be unchanged
                    lookupBindParent c' (NodeId 1) `shouldBe` Just (NodeId 0, BindFlex)
                Right (_, Just _) -> expectationFailure "Expected no-op"
                Left err -> expectationFailure $ "Expected success, got: " ++ show err

        it "fails on locked nodes" $ do
            -- Chain: 0 <- 1 (rigid) <- 2
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar (NodeId 0))
                        , (1, TyVar (NodeId 1))
                        , (2, TyVar (NodeId 2))
                        ]
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindRigid))  -- Rigid edge
                        , (2, (NodeId 1, BindFlex))
                        ]
                    }
            case applyRaiseStep (NodeId 2) c of
                Left (OperationOnLockedNode _) -> return ()
                other -> expectationFailure $ "Expected OperationOnLockedNode, got: " ++ show other

        it "preserves the binding flag" $ do
            -- Chain: 0 <- 1 <- 2 (all flex)
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar (NodeId 0))
                        , (1, TyVar (NodeId 1))
                        , (2, TyVar (NodeId 2))
                        ]
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindFlex))
                        , (2, (NodeId 1, BindFlex))
                        ]
                    }
            case applyRaiseStep (NodeId 2) c of
                Right (c', Just _) -> do
                    -- Flag should still be BindFlex
                    getBindFlag c' (NodeId 2) `shouldBe` Just BindFlex
                other -> expectationFailure $ "Expected success, got: " ++ show other

    describe "applyRaiseTo" $ do
        it "raises node to target ancestor" $ do
            -- Chain: 0 <- 1 <- 2 <- 3
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar (NodeId 0))
                        , (1, TyVar (NodeId 1))
                        , (2, TyVar (NodeId 2))
                        , (3, TyVar (NodeId 3))
                        ]
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindFlex))
                        , (2, (NodeId 1, BindFlex))
                        , (3, (NodeId 2, BindFlex))
                        ]
                    }
            case applyRaiseTo (NodeId 3) (NodeId 0) c of
                Right (c', ops) -> do
                    -- Should have 2 raise operations (3->1, then 3->0)
                    length ops `shouldBe` 2
                    -- Node 3 should now have parent 0
                    lookupBindParent c' (NodeId 3) `shouldBe` Just (NodeId 0, BindFlex)
                Left err -> expectationFailure $ "Expected success, got: " ++ show err

        it "returns empty list when already at target" $ do
            -- Chain: 0 <- 1
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar (NodeId 0))
                        , (1, TyVar (NodeId 1))
                        ]
                    , cBindParents = IntMap.singleton 1 (NodeId 0, BindFlex)
                    }
            case applyRaiseTo (NodeId 1) (NodeId 0) c of
                Right (c', ops) -> do
                    ops `shouldBe` []
                    lookupBindParent c' (NodeId 1) `shouldBe` Just (NodeId 0, BindFlex)
                Left err -> expectationFailure $ "Expected success, got: " ++ show err

        it "fails when target is not an ancestor" $ do
            -- Tree:   0
            --        / \
            --       1   2
            let c = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, TyVar (NodeId 0))
                        , (1, TyVar (NodeId 1))
                        , (2, TyVar (NodeId 2))
                        ]
                    , cBindParents = IntMap.fromList
                        [ (1, (NodeId 0, BindFlex))
                        , (2, (NodeId 0, BindFlex))
                        ]
                    }
            case applyRaiseTo (NodeId 1) (NodeId 2) c of
                Left (InvalidBindingTree _) -> return ()
                other -> expectationFailure $ "Expected InvalidBindingTree, got: " ++ show other

    describe "Property tests" $ do
        -- **Feature: paper_general_raise_plan, Property 6: Weaken preserves checkBindingTree**
        -- **Validates: Requirements 2.3, 7.2**
        it "applyWeaken preserves checkBindingTree" $ property $
            forAll (choose (2, 15)) $ \n -> do
                c <- generate (genAllFlexTree n)
                -- Find a non-root node to weaken
                let nonRoots = filter (not . isBindingRoot c) (allNodeIds c)
                case nonRoots of
                    [] -> return ()  -- No non-roots to test
                    (nid:_) -> do
                        case applyWeaken nid c of
                            Right (c', _) -> 
                                checkBindingTree c' `shouldBe` Right ()
                            Left _ -> return ()  -- Operation not applicable

        -- **Feature: paper_general_raise_plan, Property 7: RaiseStep preserves checkBindingTree**
        -- **Validates: Requirements 2.3, 7.2**
        it "applyRaiseStep preserves checkBindingTree" $ property $
            forAll (choose (3, 15)) $ \n -> do
                c <- generate (genAllFlexTree n)
                -- Find a non-root node with a non-root parent
                let candidates = filter (hasNonRootParent c) (allNodeIds c)
                case candidates of
                    [] -> return ()
                    (nid:_) -> do
                        case applyRaiseStep nid c of
                            Right (c', _) -> 
                                checkBindingTree c' `shouldBe` Right ()
                            Left _ -> return ()

        -- **Feature: paper_general_raise_plan, Property 8: Repeated RaiseStep terminates**
        -- **Validates: Requirements 4.3, 7.2**
        it "repeated applyRaiseStep terminates in bounded depth" $ property $
            forAll (choose (2, 15)) $ \n -> do
                c <- generate (genFlexChain n)
                -- Pick the deepest node (n-1)
                let deepest = NodeId (n - 1)
                -- Repeatedly raise until we can't anymore
                let raiseUntilDone constraint count
                        | count > n = expectationFailure "Raise did not terminate"
                        | otherwise = do
                            case applyRaiseStep deepest constraint of
                                Right (c', Just _) -> raiseUntilDone c' (count + 1)
                                Right (_, Nothing) -> return ()  -- Done
                                Left _ -> return ()  -- Can't raise
                raiseUntilDone c 0

        -- **Feature: paper_general_raise_plan, Property 9: RaiseTo produces correct number of ops**
        -- **Validates: Requirements 4.2, 4.3**
        it "applyRaiseTo produces correct number of operations" $ property $
            forAll (choose (3, 15)) $ \n -> do
                c <- generate (genFlexChain n)
                -- Pick a node and a target ancestor
                nodeIdx <- generate $ choose (2, n - 1)
                targetIdx <- generate $ choose (0, nodeIdx - 2)
                let nid = NodeId nodeIdx
                    target = NodeId targetIdx
                case applyRaiseTo nid target c of
                    Right (c', ops) -> do
                        -- Number of ops should equal distance - 1
                        let expectedOps = nodeIdx - targetIdx - 1
                        length ops `shouldBe` expectedOps
                        -- Final parent should be target
                        lookupBindParent c' nid `shouldBe` Just (target, BindFlex)
                    Left err -> expectationFailure $ "Expected success, got: " ++ show err

        -- **Feature: paper_general_raise_plan, Property 10: Weaken then Raise fails**
        -- **Validates: Requirements 4.1, 4.2**
        it "weakened nodes cannot be raised" $ property $
            forAll (choose (3, 15)) $ \n -> do
                c <- generate (genFlexChain n)
                -- Pick a node in the middle
                let midIdx = n `div` 2
                    midNode = NodeId midIdx
                -- Weaken it
                case applyWeaken midNode c of
                    Right (c', _) -> do
                        -- Now try to raise a node below it
                        let belowNode = NodeId (n - 1)
                        case applyRaiseStep belowNode c' of
                            Left (OperationOnLockedNode _) -> return ()
                            other -> expectationFailure $ 
                                "Expected OperationOnLockedNode, got: " ++ show other
                    Left err -> expectationFailure $ "Weaken failed: " ++ show err

-- | Check if a node has a non-root parent
hasNonRootParent :: Constraint -> NodeId -> Bool
hasNonRootParent c nid =
    case lookupBindParent c nid of
        Nothing -> False
        Just (parent, _) -> not (isBindingRoot c parent)
