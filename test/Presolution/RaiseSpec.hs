{-# LANGUAGE DataKinds #-}
module Presolution.RaiseSpec (spec) where

import IdentityTestSupport
import Test.Hspec
import Test.QuickCheck
import Control.Monad.Except (catchError)
import Control.Monad.State (gets)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Types.Witness (InstanceOp(..))
import MLF.Constraint.Types.Witness.TestSupport (EdgeWitness(..), InstanceWitness(..))
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionError(..)
    )
import MLF.Constraint.Presolution.TestSupport
    ( PresolutionState(..)
    , psEdgeTraces
    , psEdgeWitnesses
    , lookupCopy
    , processInstEdge
    , runEdgeStructureUnifyForTest
    , runEdgeTerminalStructureUnifyForTest
    , runEdgeUnifyForTest
    , runEdgeUnifyWithBinderMetasForTest
    , runPresolutionM
    , rebindWithBoundRepairTraceForTest
    , setVarBoundWithRaiseTraceForTest
    , unifyAcyclicRawWithRaiseTrace
    , unifyAcyclicRawWithRaiseTracePrefer
    )
import qualified MLF.Binding.GraphOps as GraphOps
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Util.UnionFind as UF
import SpecUtil
    ( bindParentsFromPairs
    , defaultTraceConfig
    , emptyConstraint
    , nodeMapFromList
    , rootedConstraint
    )
import MLF.Constraint.Types.Phase (Phase(Raw))

spec :: Spec
spec = do
    describe "Phase 4 — OpRaise for interior nodes" $ do
        let orderSourceFirst = NodeId 0
            orderSourceSecond = NodeId 1
            orderMetaFirst = NodeId 2
            orderMetaSecond = NodeId 3
            orderEdgeRoot = NodeId 4
            binderOrderNodes =
                nodeMapFromList
                    [ (getNodeId orderSourceFirst, TyVar orderSourceFirst Nothing)
                    , (getNodeId orderSourceSecond, TyVar orderSourceSecond Nothing)
                    , (getNodeId orderMetaFirst, TyVar orderMetaFirst Nothing)
                    , (getNodeId orderMetaSecond, TyVar orderMetaSecond Nothing)
                    -- Deliberately put the second source binder's copy on the
                    -- left.  Destination layout must not reverse the source
                    -- binder order already certified by <P.
                    , ( getNodeId orderEdgeRoot
                      , TyArrow orderEdgeRoot orderMetaSecond orderMetaFirst
                      )
                    ]
            binderOrderConstraint =
                rootedConstraint emptyConstraint
                    { cNodes = binderOrderNodes
                    , cBindParents =
                        bindParentsFromPairs
                            [ (orderMetaFirst, orderEdgeRoot, BindFlex)
                            , (orderMetaSecond, orderEdgeRoot, BindFlex)
                            ]
                    }
            binderOrderInterior =
                IntSet.fromList (map getNodeId [orderMetaFirst, orderMetaSecond])

        it "orders binder merges from frozen source order, not copied-meta layout" $ do
            let action =
                    runEdgeUnifyWithBinderMetasForTest
                        orderEdgeRoot
                        binderOrderInterior
                        [ (orderSourceFirst, orderMetaFirst)
                        , (orderSourceSecond, orderMetaSecond)
                        ]
                        orderMetaFirst
                        orderMetaSecond

            case runPresolutionM defaultTraceConfig (stateFor binderOrderConstraint 5) action of
                Left err -> expectationFailure ("edge-local binder merge failed: " ++ show err)
                Right (ops, _st1) ->
                    ops `shouldBe` [OpMerge orderSourceSecond orderSourceFirst]

        it "rejects duplicate binders while freezing source order" $ do
            let action =
                    runEdgeUnifyWithBinderMetasForTest
                        orderEdgeRoot
                        binderOrderInterior
                        [ (orderSourceFirst, orderMetaFirst)
                        , (orderSourceFirst, orderMetaSecond)
                        ]
                        orderMetaFirst
                        orderMetaSecond

            runPresolutionM defaultTraceConfig (stateFor binderOrderConstraint 5) action
                `shouldBe` Left (DuplicateEdgeBinderOrderEntry orderSourceFirst)

        it "rejects a locked bounded variable without explicit Eq-Var authority" $ do
            let rootGen = GenNodeId 0
                rigidBinder = NodeId 0
                rigidBody = NodeId 1
                boundedWrapper = NodeId 2
                lowerProxy = NodeId 3
                forallRoot = NodeId 4
                nodes =
                    nodeMapFromList
                        [ (getNodeId rigidBinder, TyVar rigidBinder Nothing)
                        , (getNodeId rigidBody, TyArrow rigidBody boundedWrapper rigidBinder)
                        , (getNodeId boundedWrapper, TyVar boundedWrapper (Just lowerProxy))
                        , (getNodeId lowerProxy, TyVar lowerProxy Nothing)
                        , (getNodeId forallRoot, TyForall forallRoot rigidBody)
                        ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cGenNodes = fromListGen [(rootGen, GenNode rootGen [forallRoot])]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef forallRoot), (genRef rootGen, BindRigid))
                                , (nodeRefKey (typeRef rigidBody), (typeRef forallRoot, BindRigid))
                                , (nodeRefKey (typeRef rigidBinder), (typeRef forallRoot, BindRigid))
                                , (nodeRefKey (typeRef boundedWrapper), (typeRef rigidBody, BindFlex))
                                , (nodeRefKey (typeRef lowerProxy), (typeRef rigidBody, BindFlex))
                                ]
                        }
                action =
                    runEdgeStructureUnifyForTest
                        forallRoot
                        (IntSet.fromList (map getNodeId [boundedWrapper, lowerProxy]))
                        boundedWrapper
                        rigidBinder

            Binding.nodeKind constraint (typeRef boundedWrapper)
                `shouldBe` Right Binding.NodeLocked
            Binding.nodeKind constraint (typeRef lowerProxy)
                `shouldBe` Right Binding.NodeLocked
            Binding.nodeKind constraint (typeRef rigidBinder)
                `shouldBe` Right Binding.NodeRestricted
            case runPresolutionM defaultTraceConfig (stateFor constraint 5) action of
                Left (BindingTreeError OperationOnLockedNode {}) -> pure ()
                Left err -> expectationFailure ("expected locked-node rejection, got " ++ show err)
                Right _ -> expectationFailure "locked bounded variable was merged without Eq-Var authority"

        it "matches a locked bounded proxy through its existing lower bound" $ do
            let rootGen = GenNodeId 0
                sourceBound = NodeId 0
                sourceProxy = NodeId 1
                sourceArrow = NodeId 2
                rigidOwner = NodeId 3
                targetBound = NodeId 4
                targetArrow = NodeId 5
                nodes =
                    nodeMapFromList
                        [ (getNodeId sourceBound, TestTyBase sourceBound (BaseTy "Int"))
                        , (getNodeId sourceProxy, TyVar sourceProxy (Just sourceBound))
                        , (getNodeId sourceArrow, TyArrow sourceArrow sourceProxy sourceProxy)
                        , (getNodeId rigidOwner, TyForall rigidOwner sourceArrow)
                        , (getNodeId targetBound, TestTyBase targetBound (BaseTy "Int"))
                        , (getNodeId targetArrow, TyArrow targetArrow targetBound targetBound)
                        ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cGenNodes =
                            fromListGen
                                [(rootGen, GenNode rootGen [rigidOwner, targetArrow])]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef sourceBound), (typeRef sourceProxy, BindFlex))
                                , (nodeRefKey (typeRef sourceProxy), (typeRef sourceArrow, BindFlex))
                                , (nodeRefKey (typeRef sourceArrow), (typeRef rigidOwner, BindFlex))
                                , (nodeRefKey (typeRef rigidOwner), (genRef rootGen, BindRigid))
                                , (nodeRefKey (typeRef targetBound), (typeRef targetArrow, BindFlex))
                                , (nodeRefKey (typeRef targetArrow), (genRef rootGen, BindRigid))
                                ]
                        }
                interior =
                    IntSet.fromList
                        (map getNodeId [sourceBound, sourceProxy, sourceArrow])
                action =
                    runEdgeStructureUnifyForTest
                        sourceArrow
                        interior
                        sourceArrow
                        targetArrow

            Binding.nodeKind constraint (typeRef sourceProxy)
                `shouldBe` Right Binding.NodeLocked
            case runPresolutionM defaultTraceConfig (stateFor constraint 6) action of
                Left err ->
                    expectationFailure ("locked bounded-proxy comparison failed: " ++ show err)
                Right (ops, finalState) -> do
                    ops `shouldBe` []
                    psUnionFind finalState `shouldBe` IntMap.empty
                    psConstraint finalState `shouldBe` constraint

        it "matches recursive binder occurrences structurally below a rigid owner" $ do
            let (constraint, sourceMu, targetMu, sourceBinder, targetBinder, interior) =
                    recursiveRigidPairFixture False
                action =
                    runEdgeStructureUnifyForTest
                        sourceMu
                        interior
                        sourceMu
                        targetMu

            Binding.nodeKind constraint (typeRef sourceMu)
                `shouldBe` Right Binding.NodeLocked
            Binding.nodeKind constraint (typeRef sourceBinder)
                `shouldBe` Right Binding.NodeLocked
            Binding.nodeKind constraint (typeRef targetBinder)
                `shouldBe` Right Binding.NodeRestricted
            case runPresolutionM defaultTraceConfig (stateFor constraint 8) action of
                Left err ->
                    expectationFailure ("recursive rigid comparison failed: " ++ show err)
                Right (ops, finalState) -> do
                    ops `shouldBe` []
                    psUnionFind finalState `shouldBe` IntMap.empty
                    psConstraint finalState `shouldBe` constraint

        it "does not descend into locked recursive binders before terminal root matching" $ do
            let (constraint, sourceMu, targetMu, _sourceBinder, _targetBinder, interior) =
                    recursiveRigidPairFixture False
                action =
                    runEdgeTerminalStructureUnifyForTest
                        sourceMu
                        interior
                        sourceMu
                        targetMu

            case runPresolutionM defaultTraceConfig (stateFor constraint 8) action of
                Left err ->
                    expectationFailure ("terminal recursive rigid comparison failed: " ++ show err)
                Right (ops, finalState) -> do
                    ops `shouldBe` []
                    psUnionFind finalState `shouldBe` IntMap.empty
                    psConstraint finalState `shouldBe` constraint

        it "rejects an inconsistent recursive binder occurrence without raising locked nodes" $ do
            let (constraint, sourceMu, targetMu, _sourceBinder, _targetBinder, interior) =
                    recursiveRigidPairFixture True
                action =
                    runEdgeStructureUnifyForTest
                        sourceMu
                        interior
                        sourceMu
                        targetMu

            case runPresolutionM defaultTraceConfig (stateFor constraint 8) action of
                Left (UnmatchableTypes _ _ "inconsistent recursive binder occurrence") -> pure ()
                Left err ->
                    expectationFailure ("expected recursive binder mismatch, got " ++ show err)
                Right _ ->
                    expectationFailure "inconsistent recursive binder occurrence was accepted"

        it "raises a bounded variable's free lower-bound frontier with the variable" $ do
            let rootGen = GenNodeId 0
                innerGen = GenNodeId 1
                outer = NodeId 0
                inner = NodeId 1
                freeBound = NodeId 2
                nodes =
                    nodeMapFromList
                        [ (getNodeId outer, TyVar {tnId = outer, tnBound = Nothing})
                        , (getNodeId inner, TyVar {tnId = inner, tnBound = Just freeBound})
                        , (getNodeId freeBound, TyVar {tnId = freeBound, tnBound = Nothing})
                        ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [outer])
                                , (innerGen, GenNode innerGen [inner])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef outer), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef inner), (genRef innerGen, BindFlex))
                                , (nodeRefKey (typeRef freeBound), (genRef innerGen, BindFlex))
                                , (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                                ]
                        }
                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty
                        3
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (unifyAcyclicRawWithRaiseTrace outer inner) of
                Left err -> expectationFailure ("unification failed: " ++ show err)
                Right (trace, st1) -> do
                    trace `shouldBe` [inner, freeBound]
                    Binding.lookupBindParent (psConstraint st1) (typeRef freeBound)
                        `shouldBe` Just (genRef rootGen, BindFlex)

        it "keeps a lower bound owned by the raised variable inside that variable" $ do
            let rootGen = GenNodeId 0
                innerGen = GenNodeId 1
                outer = NodeId 0
                inner = NodeId 1
                ownedBound = NodeId 2
                nodes =
                    nodeMapFromList
                        [ (getNodeId outer, TyVar {tnId = outer, tnBound = Nothing})
                        , (getNodeId inner, TyVar {tnId = inner, tnBound = Just ownedBound})
                        , (getNodeId ownedBound, TyVar {tnId = ownedBound, tnBound = Nothing})
                        ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [outer])
                                , (innerGen, GenNode innerGen [inner])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef outer), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef inner), (genRef innerGen, BindFlex))
                                , (nodeRefKey (typeRef ownedBound), (typeRef inner, BindFlex))
                                , (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                                ]
                        }
                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty
                        3
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (unifyAcyclicRawWithRaiseTrace outer inner) of
                Left err -> expectationFailure ("unification failed: " ++ show err)
                Right (trace, st1) -> do
                    trace `shouldBe` [inner]
                    Binding.lookupBindParent (psConstraint st1) (typeRef ownedBound)
                        `shouldBe` Just (typeRef inner, BindFlex)

        it "fails closed when a free lower-bound frontier is locked" $ do
            let rootGen = GenNodeId 0
                innerGen = GenNodeId 1
                rigidOwner = NodeId 3
                outer = NodeId 0
                inner = NodeId 1
                freeBound = NodeId 2
                nodes =
                    nodeMapFromList
                        [ (getNodeId outer, TyVar {tnId = outer, tnBound = Nothing})
                        , (getNodeId inner, TyVar {tnId = inner, tnBound = Just freeBound})
                        , (getNodeId freeBound, TyVar {tnId = freeBound, tnBound = Nothing})
                        , (getNodeId rigidOwner, TyForall rigidOwner freeBound)
                        ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [outer])
                                , (innerGen, GenNode innerGen [inner])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef outer), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef inner), (genRef innerGen, BindFlex))
                                , (nodeRefKey (typeRef freeBound), (typeRef rigidOwner, BindFlex))
                                , (nodeRefKey (typeRef rigidOwner), (genRef innerGen, BindRigid))
                                , (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                                ]
                        }
                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty
                        4
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
            runPresolutionM defaultTraceConfig st0 (unifyAcyclicRawWithRaiseTrace outer inner)
                `shouldBe` Left (BindingTreeError (FreeBoundFrontierLocked inner freeBound))

        it "raises a free lower-bound frontier before installing a late bound" $ do
            let rootGen = GenNodeId 0
                innerGen = GenNodeId 1
                variable = NodeId 0
                freeBound = NodeId 1
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId variable, TyVar variable Nothing)
                                , (getNodeId freeBound, TyVar freeBound Nothing)
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [variable])
                                , (innerGen, GenNode innerGen [freeBound])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef variable), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef freeBound), (genRef innerGen, BindFlex))
                                , (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                                ]
                        }
                action =
                    setVarBoundWithRaiseTraceForTest variable (Just freeBound)
            case runPresolutionM defaultTraceConfig (stateFor constraint 2) action of
                Left err -> expectationFailure ("setting bound failed: " ++ show err)
                Right (trace, st1) -> do
                    trace `shouldBe` [freeBound]
                    lookupNode variable (cNodes (psConstraint st1))
                        `shouldBe` Just (TyVar variable (Just freeBound))
                    Binding.lookupBindParent (psConstraint st1) (typeRef freeBound)
                        `shouldBe` Just (genRef rootGen, BindFlex)

        it "raises only a structured bound's free transitive frontier" $ do
            let rootGen = GenNodeId 0
                innerGen = GenNodeId 1
                variable = NodeId 0
                boundRoot = NodeId 1
                ownedChild = NodeId 2
                freeChild = NodeId 3
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId variable, TyVar variable Nothing)
                                , (getNodeId boundRoot, TyArrow boundRoot ownedChild freeChild)
                                , (getNodeId ownedChild, TyVar ownedChild Nothing)
                                , (getNodeId freeChild, TyVar freeChild Nothing)
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [variable])
                                , (innerGen, GenNode innerGen [freeChild])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef variable), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef boundRoot), (typeRef variable, BindFlex))
                                , (nodeRefKey (typeRef ownedChild), (typeRef boundRoot, BindFlex))
                                , (nodeRefKey (typeRef freeChild), (genRef innerGen, BindFlex))
                                , (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                                ]
                        }
                action =
                    setVarBoundWithRaiseTraceForTest variable (Just boundRoot)
            case runPresolutionM defaultTraceConfig (stateFor constraint 4) action of
                Left err -> expectationFailure ("setting structured bound failed: " ++ show err)
                Right (trace, st1) -> do
                    trace `shouldBe` [freeChild]
                    Binding.lookupBindParent (psConstraint st1) (typeRef boundRoot)
                        `shouldBe` Just (typeRef variable, BindFlex)
                    Binding.lookupBindParent (psConstraint st1) (typeRef ownedChild)
                        `shouldBe` Just (typeRef boundRoot, BindFlex)
                    Binding.lookupBindParent (psConstraint st1) (typeRef freeChild)
                        `shouldBe` Just (genRef rootGen, BindFlex)

        it "keeps a rigid TyForall binder out of the lower-bound free frontier" $ do
            let rootGen = GenNodeId 0
                innerGen = GenNodeId 1
                variable = NodeId 0
                forallRoot = NodeId 1
                forallBody = NodeId 2
                rigidBinder = NodeId 3
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId variable, TyVar variable Nothing)
                                , (getNodeId forallRoot, TyForall forallRoot forallBody)
                                , (getNodeId forallBody, TyArrow forallBody rigidBinder rigidBinder)
                                , (getNodeId rigidBinder, TyVar rigidBinder Nothing)
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [variable])
                                , (innerGen, GenNode innerGen [forallRoot])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef variable), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef forallRoot), (genRef innerGen, BindRigid))
                                , (nodeRefKey (typeRef forallBody), (typeRef forallRoot, BindFlex))
                                , (nodeRefKey (typeRef rigidBinder), (typeRef forallRoot, BindRigid))
                                , (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                                ]
                        }
                action =
                    setVarBoundWithRaiseTraceForTest variable (Just forallRoot)
            case runPresolutionM defaultTraceConfig (stateFor constraint 4) action of
                Left err -> expectationFailure ("setting forall bound failed: " ++ show err)
                Right (trace, st1) -> do
                    trace `shouldBe` [forallRoot]
                    lookupNode variable (cNodes (psConstraint st1))
                        `shouldBe` Just (TyVar variable (Just forallRoot))
                    Binding.lookupBindParent (psConstraint st1) (typeRef forallRoot)
                        `shouldBe` Just (genRef rootGen, BindRigid)
                    Binding.lookupBindParent (psConstraint st1) (typeRef rigidBinder)
                        `shouldBe` Just (typeRef forallRoot, BindRigid)

        it "raises a shared restricted variable with the lower-bound free frontier" $ do
            let rootGen = GenNodeId 0
                innerGen = GenNodeId 1
                variable = NodeId 0
                boundRoot = NodeId 1
                rigidBinder = NodeId 2
                forallOwner = NodeId 3
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId variable, TyVar variable Nothing)
                                , (getNodeId boundRoot, TyArrow boundRoot rigidBinder rigidBinder)
                                , (getNodeId rigidBinder, TyVar rigidBinder Nothing)
                                , (getNodeId forallOwner, TyForall forallOwner rigidBinder)
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [variable])
                                , (innerGen, GenNode innerGen [forallOwner])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef variable), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef boundRoot), (typeRef variable, BindFlex))
                                , (nodeRefKey (typeRef rigidBinder), (typeRef forallOwner, BindRigid))
                                , (nodeRefKey (typeRef forallOwner), (genRef innerGen, BindRigid))
                                , (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                                ]
                        }
                action =
                    setVarBoundWithRaiseTraceForTest variable (Just boundRoot)
            case runPresolutionM defaultTraceConfig (stateFor constraint 4) action of
                Left err -> expectationFailure ("setting shared rigid bound failed: " ++ show err)
                Right (trace, st1) -> do
                    trace `shouldBe` [rigidBinder, rigidBinder]
                    lookupNode variable (cNodes (psConstraint st1))
                        `shouldBe` Just (TyVar variable (Just boundRoot))
                    Binding.lookupBindParent (psConstraint st1) (typeRef rigidBinder)
                        `shouldBe` Just (genRef rootGen, BindRigid)

        it "validates prospective bound reachability before publishing a UF merge" $ do
            let rootGen = GenNodeId 0
                innerGen = GenNodeId 1
                variable = NodeId 0
                boundRoot = NodeId 1
                ownedShape = NodeId 2
                ownedLeaf = NodeId 3
                exteriorShape = NodeId 4
                lockedLeaf = NodeId 5
                rigidOwner = NodeId 6
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId variable, TyVar variable (Just boundRoot))
                                , (getNodeId boundRoot, TyArrow boundRoot ownedShape ownedShape)
                                , (getNodeId ownedShape, TyArrow ownedShape ownedLeaf ownedLeaf)
                                , (getNodeId ownedLeaf, TyVar ownedLeaf Nothing)
                                , (getNodeId exteriorShape, TyArrow exteriorShape lockedLeaf lockedLeaf)
                                , (getNodeId lockedLeaf, TyVar lockedLeaf Nothing)
                                , (getNodeId rigidOwner, TyForall rigidOwner lockedLeaf)
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [variable, exteriorShape])
                                , (innerGen, GenNode innerGen [rigidOwner])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef variable), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef boundRoot), (typeRef variable, BindFlex))
                                , (nodeRefKey (typeRef ownedShape), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef ownedLeaf), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef exteriorShape), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef lockedLeaf), (typeRef rigidOwner, BindFlex))
                                , (nodeRefKey (typeRef rigidOwner), (genRef innerGen, BindRigid))
                                , (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                                ]
                        }
                action =
                    ( unifyAcyclicRawWithRaiseTracePrefer
                        (Just exteriorShape)
                        ownedShape
                        exteriorShape
                        >> pure Nothing
                    )
                        `catchError` (\err -> do
                            currentConstraint <- gets psConstraint
                            currentUf <- gets psUnionFind
                            pure (Just (err, currentConstraint, currentUf)))
            case runPresolutionM defaultTraceConfig (stateFor constraint 7) action of
                Left err -> expectationFailure ("caught merge failure escaped: " ++ show err)
                Right (result, st1) -> do
                    result `shouldBe`
                        Just
                            ( BindingTreeError (FreeBoundFrontierLocked variable lockedLeaf)
                            , constraint
                            , IntMap.empty
                            )
                    psConstraint st1 `shouldBe` constraint
                    psUnionFind st1 `shouldBe` IntMap.empty

        it "keeps a late lower bound owned by its variable local" $ do
            let rootGen = GenNodeId 0
                variable = NodeId 0
                ownedBound = NodeId 1
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId variable, TyVar variable Nothing)
                                , (getNodeId ownedBound, TyVar ownedBound Nothing)
                                ]
                        , cGenNodes =
                            fromListGen [(rootGen, GenNode rootGen [variable])]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef variable), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef ownedBound), (typeRef variable, BindFlex))
                                ]
                        }
                action =
                    setVarBoundWithRaiseTraceForTest variable (Just ownedBound)
            case runPresolutionM defaultTraceConfig (stateFor constraint 2) action of
                Left err -> expectationFailure ("setting owned bound failed: " ++ show err)
                Right (trace, st1) -> do
                    trace `shouldBe` []
                    Binding.lookupBindParent (psConstraint st1) (typeRef ownedBound)
                        `shouldBe` Just (typeRef variable, BindFlex)

        it "rejects a late free lower-bound frontier under a rigid path" $ do
            let rootGen = GenNodeId 0
                innerGen = GenNodeId 1
                variable = NodeId 0
                freeBound = NodeId 1
                rigidOwner = NodeId 2
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId variable, TyVar variable Nothing)
                                , (getNodeId freeBound, TyVar freeBound Nothing)
                                , (getNodeId rigidOwner, TyForall rigidOwner freeBound)
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [variable])
                                , (innerGen, GenNode innerGen [rigidOwner])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef variable), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef freeBound), (typeRef rigidOwner, BindFlex))
                                , (nodeRefKey (typeRef rigidOwner), (genRef innerGen, BindRigid))
                                , (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                                ]
                        }
                action =
                    setVarBoundWithRaiseTraceForTest variable (Just freeBound)
            runPresolutionM defaultTraceConfig (stateFor constraint 3) action
                `shouldBe` Left (BindingTreeError (FreeBoundFrontierLocked variable freeBound))

        it "rejects a late free lower-bound frontier in a sibling scope" $ do
            let rootGen = GenNodeId 0
                leftGen = GenNodeId 1
                rightGen = GenNodeId 2
                variable = NodeId 0
                freeBound = NodeId 1
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId variable, TyVar variable Nothing)
                                , (getNodeId freeBound, TyVar freeBound Nothing)
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [])
                                , (leftGen, GenNode leftGen [variable])
                                , (rightGen, GenNode rightGen [freeBound])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef variable), (genRef leftGen, BindFlex))
                                , (nodeRefKey (typeRef freeBound), (genRef rightGen, BindFlex))
                                , (nodeRefKey (genRef leftGen), (genRef rootGen, BindFlex))
                                , (nodeRefKey (genRef rightGen), (genRef rootGen, BindFlex))
                                ]
                        }
                action =
                    setVarBoundWithRaiseTraceForTest variable (Just freeBound)
            runPresolutionM defaultTraceConfig (stateFor constraint 2) action
                `shouldBe`
                    Left
                        ( BindingTreeError
                            ( FreeBoundFrontierInSiblingScope
                                variable
                                freeBound
                                (genRef leftGen)
                                (genRef rightGen)
                            )
                        )

        it "records a witnessed Raise when edge-local bound installation crosses sibling scopes" $ do
            let meta = NodeId 0
                boundRoot = NodeId 1
                boundLeaf = NodeId 2
                leftOwner = NodeId 3
                edgeRoot = NodeId 4
                rightOwner = NodeId 5
                nodes =
                    nodeMapFromList
                        [ (getNodeId meta, TyVar meta Nothing)
                        , (getNodeId boundRoot, TyArrow boundRoot boundLeaf boundLeaf)
                        , (getNodeId boundLeaf, TyVar boundLeaf Nothing)
                        , (getNodeId leftOwner, TyArrow leftOwner meta meta)
                        , (getNodeId edgeRoot, TyArrow edgeRoot leftOwner rightOwner)
                        , (getNodeId rightOwner, TyArrow rightOwner boundRoot boundRoot)
                        ]
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cBindParents =
                            bindParentsFromPairs
                                [ (meta, leftOwner, BindFlex)
                                , (boundLeaf, boundRoot, BindFlex)
                                , (boundRoot, rightOwner, BindFlex)
                                , (leftOwner, edgeRoot, BindFlex)
                                , (rightOwner, edgeRoot, BindFlex)
                                ]
                        }
                st0 = stateFor constraint 6
                interior = IntSet.singleton (getNodeId boundRoot)
                action =
                    runEdgeUnifyWithBinderMetasForTest
                        edgeRoot
                        interior
                        [(meta, meta)]
                        meta
                        boundRoot
            case runPresolutionM defaultTraceConfig st0 action of
                Left err -> expectationFailure ("edge-local bound install failed: " ++ show err)
                Right (ops, st1) -> do
                    ops `shouldBe` [OpRaise boundRoot]
                    Binding.lookupBindParent (psConstraint st1) (typeRef boundRoot)
                        `shouldBe` Just (typeRef edgeRoot, BindFlex)
                    Binding.checkBindingTreeUnder
                        (UF.frWith (psUnionFind st1))
                        (psConstraint st1)
                        `shouldBe` Right ()

        it "rejects a missing variable bound target explicitly" $ do
            let missing = NodeId 7
            runPresolutionM
                defaultTraceConfig
                (stateFor emptyConstraint 0)
                (setVarBoundWithRaiseTraceForTest missing Nothing)
                `shouldBe` Left (NodeLookupFailed missing)

        it "rejects a non-variable bound target explicitly" $ do
            let target = NodeId 0
                node = TyBottom target
                constraint =
                    emptyConstraint
                        { cNodes = nodeMapFromList [(getNodeId target, node)]
                        }
            runPresolutionM
                defaultTraceConfig
                (stateFor constraint 1)
                (setVarBoundWithRaiseTraceForTest target Nothing)
                `shouldBe` Left (BoundTargetNotTyVar target node)

        it "rejects a missing lower-bound root explicitly" $ do
            let variable = NodeId 0
                missingBound = NodeId 9
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [(getNodeId variable, TyVar variable Nothing)]
                        }
            runPresolutionM
                defaultTraceConfig
                (stateFor constraint 1)
                (setVarBoundWithRaiseTraceForTest variable (Just missingBound))
                `shouldBe` Left (NodeLookupFailed missingBound)

        it "rejects a direct self lower bound" $ do
            let variable = NodeId 0
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [(getNodeId variable, TyVar variable Nothing)]
                        }
            runPresolutionM
                defaultTraceConfig
                (stateFor constraint 1)
                (setVarBoundWithRaiseTraceForTest variable (Just variable))
                `shouldBe` Left (OccursCheckPresolution variable variable)

        it "rejects a lower-bound cycle reached through another bound" $ do
            let variable = NodeId 0
                peer = NodeId 1
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId variable, TyVar variable Nothing)
                                , (getNodeId peer, TyVar peer (Just variable))
                                ]
                        }
            runPresolutionM
                defaultTraceConfig
                (stateFor constraint 2)
                (setVarBoundWithRaiseTraceForTest variable (Just peer))
                `shouldBe` Left (OccursCheckPresolution variable peer)

        it "finds a free lower-bound frontier through a UF-aliased structural child" $ do
            let rootGen = GenNodeId 0
                innerGen = GenNodeId 1
                variable = NodeId 0
                boundRoot = NodeId 1
                frontier = NodeId 2
                frontierAlias = NodeId 3
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId variable, TyVar variable Nothing)
                                , (getNodeId boundRoot, TyArrow boundRoot frontierAlias frontierAlias)
                                , (getNodeId frontier, TyVar frontier Nothing)
                                , (getNodeId frontierAlias, TyVar frontierAlias Nothing)
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [variable])
                                , (innerGen, GenNode innerGen [frontier])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef variable), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef boundRoot), (typeRef variable, BindFlex))
                                , (nodeRefKey (typeRef frontier), (genRef innerGen, BindFlex))
                                , (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                                ]
                        }
                st0 =
                    (stateFor constraint 4)
                        { psUnionFind =
                            IntMap.singleton (getNodeId frontierAlias) frontier
                        }
            case runPresolutionM
                defaultTraceConfig
                st0
                (setVarBoundWithRaiseTraceForTest variable (Just boundRoot)) of
                Left err -> expectationFailure ("setting aliased bound failed: " ++ show err)
                Right (trace, st1) -> do
                    trace `shouldBe` [frontier]
                    Binding.lookupBindParent (psConstraint st1) (typeRef frontier)
                        `shouldBe` Just (genRef rootGen, BindFlex)

        it "repairs a bounded descendant when its binding ancestor moves" $ do
            let rootGen = GenNodeId 0
                innerGen = GenNodeId 1
                ancestor = NodeId 0
                variable = NodeId 1
                frontier = NodeId 2
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId ancestor, TyArrow ancestor variable variable)
                                , (getNodeId variable, TyVar variable (Just frontier))
                                , (getNodeId frontier, TyVar frontier Nothing)
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [])
                                , (innerGen, GenNode innerGen [ancestor])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef ancestor), (genRef innerGen, BindFlex))
                                , (nodeRefKey (typeRef variable), (typeRef ancestor, BindFlex))
                                , (nodeRefKey (typeRef frontier), (genRef innerGen, BindFlex))
                                , (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                                ]
                        }
            case runPresolutionM
                defaultTraceConfig
                (stateFor constraint 3)
                ( rebindWithBoundRepairTraceForTest
                    (typeRef ancestor)
                    (genRef rootGen, BindFlex)
                ) of
                Left err -> expectationFailure ("rebind failed: " ++ show err)
                Right (trace, st1) -> do
                    trace `shouldBe` [frontier]
                    Binding.lookupBindParent (psConstraint st1) (typeRef frontier)
                        `shouldBe` Just (genRef rootGen, BindFlex)

        it "does not publish partial scope changes when bound installation fails" $ do
            let rootGen = GenNodeId 0
                leftGen = GenNodeId 1
                rightGen = GenNodeId 2
                variable = NodeId 0
                frontier = NodeId 1
                constraint =
                    emptyConstraint
                        { cNodes =
                            nodeMapFromList
                                [ (getNodeId variable, TyVar variable Nothing)
                                , (getNodeId frontier, TyVar frontier Nothing)
                                ]
                        , cGenNodes =
                            fromListGen
                                [ (rootGen, GenNode rootGen [])
                                , (leftGen, GenNode leftGen [variable])
                                , (rightGen, GenNode rightGen [frontier])
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef variable), (genRef leftGen, BindFlex))
                                , (nodeRefKey (typeRef frontier), (genRef rightGen, BindFlex))
                                , (nodeRefKey (genRef leftGen), (genRef rootGen, BindFlex))
                                , (nodeRefKey (genRef rightGen), (genRef rootGen, BindFlex))
                                ]
                        }
                action =
                    (setVarBoundWithRaiseTraceForTest variable (Just frontier) >> pure Nothing)
                        `catchError` (\err -> do
                            current <- gets psConstraint
                            pure (Just (err, current)))
            case runPresolutionM defaultTraceConfig (stateFor constraint 2) action of
                Left err -> expectationFailure ("caught action escaped: " ++ show err)
                Right (result, st1) -> do
                    result `shouldBe`
                        Just
                            ( BindingTreeError
                                ( FreeBoundFrontierInSiblingScope
                                    variable
                                    frontier
                                    (genRef leftGen)
                                    (genRef rightGen)
                                )
                            , constraint
                            )
                    psConstraint st1 `shouldBe` constraint

        it "returns a non-empty OpRaise trace when harmonization raises" $ do
            let binder = NodeId 3
                n = NodeId 1
                m = NodeId 4
                rootArrow = NodeId 5

                nodes = nodeMapFromList
                        [ (getNodeId n, TyVar { tnId = n, tnBound = Nothing })
                        , (getNodeId m, TyVar { tnId = m, tnBound = Nothing })
                        , (getNodeId binder, TyForall binder n)
                        , (getNodeId rootArrow, TyArrow rootArrow binder m)
                        ]

                bindParents =
                    bindParentsFromPairs
                        [ (binder, rootArrow, BindFlex)
                        , (n, binder, BindFlex)
                        , (m, rootArrow, BindFlex)
                        ]

                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        }

                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty
                        6
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (unifyAcyclicRawWithRaiseTrace n m) of
                Left err ->
                    expectationFailure ("unifyAcyclicRawWithRaiseTrace failed: " ++ show err)
                Right (trace, st1) -> do
                    trace `shouldBe` [n]
                    let uf = psUnionFind st1
                        nC = UF.frWith uf n
                    Binding.lookupBindParent (psConstraint st1) (typeRef nC)
                        `shouldBe` Just (typeRef rootArrow, BindFlex)

        it "records OpRaise for exactly the raised node (no spray across the UF class)" $ do
            -- Test case: a and b are in the same UF class (b ↦ a), but only a is raised.
            --
            -- This regression guards against the old “spray” behavior where a single
            -- raise count for a UF class caused OpRaise to be recorded for all interior
            -- nodes in that class.
            --
            -- Requirements: 5.1
            let a = NodeId 1
                b = NodeId 2
                c = NodeId 3
                forallNode = NodeId 4
                rootArrow = NodeId 5

                nodes = nodeMapFromList
                        [ (getNodeId a, TyVar { tnId = a, tnBound = Nothing })
                        -- b is a term-dag root (unbound) but is unioned into a's class.
                        , (getNodeId b, TyVar { tnId = b, tnBound = Nothing })
                        , (getNodeId c, TyVar { tnId = c, tnBound = Nothing })
                        , (getNodeId forallNode, TyForall forallNode a)
                        , (getNodeId rootArrow, TyArrow rootArrow forallNode c)
                        ]

                bindParents =
                    bindParentsFromPairs
                        [ (forallNode, rootArrow, BindFlex)
                        , (a, forallNode, BindFlex)
                        , (c, rootArrow, BindFlex)
                        ]

                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        }

                -- Union b into a's class (b ↦ a); b remains a binding-root node.
                uf = IntMap.fromList [(getNodeId b, a)]

                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        uf
                        6
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                interior = IntSet.fromList [getNodeId a, getNodeId b]

            case runPresolutionM defaultTraceConfig st0 (runEdgeUnifyForTest rootArrow interior a c) of
                Left err ->
                    expectationFailure ("runEdgeUnifyForTest failed: " ++ show err)
                Right (ops, _st1) -> do
                    ops `shouldBe` [OpRaise a]

        it "rejects instantiation edges without binding parents" $ do
            -- Test case: TyExp s · (∀b. b -> b) ≤ (y -> y)
            --
            -- With Phase 10, missing binding parents is no longer treated as “legacy mode”;
            -- presolution must reject ill-formed binding trees.
            let b = NodeId 1
                arrow1 = NodeId 2
                forallNode = NodeId 3
                expNode = NodeId 4
                y = NodeId 5
                targetArrow = NodeId 6

                nodes = nodeMapFromList
                        [ (1, TyVar { tnId = b, tnBound = Nothing })
                        , (2, TyArrow arrow1 b b)
                        , (3, TyForall forallNode arrow1)
                        , (4, TyExp expNode (ExpVarId 0) forallNode)
                        , (5, TyVar { tnId = y, tnBound = Nothing })
                        , (6, TyArrow targetArrow y y)
                        ]

                -- No binding edges: should fail binding-tree validation
                edge = InstEdge (EdgeId 0) expNode targetArrow
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        }
                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty
                        7
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
                Left (BindingTreeError _) -> pure ()
                Left err -> expectationFailure ("Expected BindingTreeError, got: " ++ show err)
                Right _ -> expectationFailure "Expected BindingTreeError"

        it "records OpRaise for interior nodes with binding edges" $ do
            -- Test case: TyExp s · (∀b. b -> b) ≤ (y -> y) with binding edges
            --
            -- This test verifies that when binding edges are present, the interior
            -- tracking works correctly and OpRaise is recorded for interior nodes.
            --
            -- Requirements: 5.1, 5.2, 7.3
            let b = NodeId 1
                arrow1 = NodeId 2
                forallNode = NodeId 3
                expNode = NodeId 4
                y = NodeId 5
                targetArrow = NodeId 6

                nodes = nodeMapFromList
                        [ (1, TyVar { tnId = b, tnBound = Nothing })
                        , (2, TyArrow arrow1 b b)
                        , (3, TyForall forallNode arrow1)
                        , (4, TyExp expNode (ExpVarId 0) forallNode)
                        , (5, TyVar { tnId = y, tnBound = Nothing })
                        , (6, TyArrow targetArrow y y)
                        ]

                -- Add binding edges for all non-term-dag-root nodes
                -- Term-dag roots: expNode (4), targetArrow (6)
                -- Non-roots: b (1), arrow1 (2), forallNode (3), y (5)
                bindParents =
                    bindParentsFromPairs
                        [ (b, forallNode, BindFlex)
                        , (arrow1, forallNode, BindFlex)
                        , (forallNode, expNode, BindFlex)
                        , (y, targetArrow, BindFlex)
                        ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty
                        7
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    -- Verify the edge trace contains the interior nodes
                    case IntMap.lookup 0 (psEdgeTraces st1) of
                        Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0"
                        Just tr -> do
                            -- The interior should be non-empty
                            etInterior tr `shouldSatisfy` (/= mempty)
                    -- Verify the binding tree is still valid
                    let finalConstraint = psConstraint st1
                        uf = psUnionFind st1
                    Binding.checkBindingTreeUnder (UF.frWith uf) finalConstraint `shouldBe` Right ()

        it "elides operations under rigid binders" $ do
            -- Test case: When a node is under a rigid binder, operations on it
            -- should be elided from the witness (paper normalization constraint).
            --
            -- Requirements: 5.2
            let a = NodeId 0
                intNode = NodeId 1
                innerArrow = NodeId 2
                forallNode = NodeId 3
                expNode = NodeId 4
                y = NodeId 5
                targetArrow = NodeId 6

                nodes = nodeMapFromList
                        [ (0, TyVar { tnId = a, tnBound = Nothing })
                        , (1, TestTyBase intNode (BaseTy "Int"))
                        , (2, TyArrow innerArrow a intNode)
                        , (3, TyForall forallNode innerArrow)
                        , (4, TyExp expNode (ExpVarId 0) forallNode)
                        , (5, TyVar { tnId = y, tnBound = Nothing })
                        , (6, TyArrow targetArrow y intNode)
                        ]

                -- Keep innerArrow's own edge flexible, but place it below a
                -- rigid forall edge so it is genuinely locked rather than
                -- merely restricted.
                bindParents =
                    bindParentsFromPairs
                        [ (a, forallNode, BindFlex)
                        , (intNode, innerArrow, BindFlex)
                        , (innerArrow, forallNode, BindFlex)
                        , (forallNode, expNode, BindRigid)
                        , (y, targetArrow, BindFlex)
                        ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty
                        7
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                -- Check that no OpRaise targets the rigidly bound innerArrow
                isRaiseOnRigid op = case op of
                    OpRaise n -> n == innerArrow
                    _ -> False

            Binding.nodeKind constraint (typeRef innerArrow)
                `shouldBe` Right Binding.NodeLocked
            case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    case IntMap.lookup 0 (psEdgeWitnesses st1) of
                        Nothing -> expectationFailure "Expected EdgeWitness for EdgeId 0"
                        Just ew -> do
                            let InstanceWitness ops = ewWitness ew
                            -- No OpRaise should target the rigidly bound node
                            ops `shouldNotSatisfy` any isRaiseOnRigid

        it "records OpRaise for a non-binder interior node (non-binder)" $ do
            -- This regression constructs an instantiation edge where unifying χe with
            -- the target forces raising an *interior structure node* (a TyArrow copy),
            -- and asserts the witness records that `OpRaise`.
            --
            -- Requirements: 5.1, 7.3
            let bv = NodeId 1
                innerArrow = NodeId 2
                outerArrow = NodeId 3
                forallNode = NodeId 4
                expNode = NodeId 5

                y = NodeId 6
                targetInnerArrow = NodeId 7
                targetOuterArrow = NodeId 8

                rootArrow = NodeId 9

                nodes = nodeMapFromList
                        [ (getNodeId bv, TyVar { tnId = bv, tnBound = Nothing })
                        , (getNodeId innerArrow, TyArrow innerArrow bv bv)
                        , (getNodeId outerArrow, TyArrow outerArrow innerArrow bv)
                        , (getNodeId forallNode, TyForall forallNode outerArrow)
                        , (getNodeId expNode, TyExp expNode (ExpVarId 0) forallNode)
                        , (getNodeId y, TyVar { tnId = y, tnBound = Nothing })
                        , (getNodeId targetInnerArrow, TyArrow targetInnerArrow y y)
                        , (getNodeId targetOuterArrow, TyArrow targetOuterArrow targetInnerArrow y)
                        , (getNodeId rootArrow, TyArrow rootArrow expNode targetOuterArrow)
                        ]

                bindParents =
                    bindParentsFromPairs
                        [ (expNode, rootArrow, BindFlex)
                        , (forallNode, expNode, BindFlex)
                        , (outerArrow, forallNode, BindFlex)
                        , (innerArrow, outerArrow, BindFlex)
                        , (bv, forallNode, BindFlex)
                        , (targetOuterArrow, rootArrow, BindFlex)
                        -- Bind the target's inner arrow directly to the root to force a raise of
                        -- the source-domain inner arrow during unification.
                        , (targetInnerArrow, rootArrow, BindFlex)
                        , (y, targetOuterArrow, BindFlex)
                        ]

                edge = InstEdge (EdgeId 0) expNode targetOuterArrow
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }

                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty
                        10
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                isRaiseOn nid op = case op of
                    OpRaise n -> n == nid
                    _ -> False

            case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    tr <- case IntMap.lookup 0 (psEdgeTraces st1) of
                        Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0" >> fail "missing trace"
                        Just t -> pure t
                    copiedInner <- case lookupCopy innerArrow (etCopyMap tr) of
                        Nothing ->
                            expectationFailure "Expected copyMap to include innerArrow copy" >> fail "missing copy"
                        Just nid -> pure nid

                    ew <- case IntMap.lookup 0 (psEdgeWitnesses st1) of
                        Nothing -> expectationFailure "Expected EdgeWitness for EdgeId 0" >> fail "missing witness"
                        Just w -> pure w

                    let InstanceWitness ops = ewWitness ew
                    copiedInner `shouldNotBe` innerArrow
                    -- Witness operands are frozen source identities.  The
                    -- copy map is execution data, not operation authority.
                    ops `shouldSatisfy` any (isRaiseOn innerArrow)
                    ops `shouldNotSatisfy` any (isRaiseOn copiedInner)

        it "does not record OpRaise for raised nodes outside I(r) (OpRaise outside)" $ do
            -- Direct regression for the “no outside OpRaise” rule:
            --
            -- Harmonization raises both unified nodes to their LCA, but Ω should
            -- record `OpRaise` only for nodes in the edge interior I(r).
            let a = NodeId 3
                b = NodeId 4
                p1 = NodeId 1
                p2 = NodeId 2
                r = NodeId 0

                nodes = nodeMapFromList
                        [ (getNodeId a, TyVar { tnId = a, tnBound = Nothing })
                        , (getNodeId b, TyVar { tnId = b, tnBound = Nothing })
                        , (getNodeId p1, TyForall p1 a)
                        , (getNodeId p2, TyForall p2 b)
                        , (getNodeId r, TyArrow r p1 p2)
                        ]

                bindParents =
                    bindParentsFromPairs
                        [ (p1, r, BindFlex)
                        , (a, p1, BindFlex)
                        , (p2, r, BindFlex)
                        , (b, p2, BindFlex)
                        ]

                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        }

                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty
                        5
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                interior = IntSet.fromList [getNodeId a]

            -- Sanity: both sides really do get raised by harmonization.
            case runPresolutionM defaultTraceConfig st0 (unifyAcyclicRawWithRaiseTrace a b) of
                Left err -> expectationFailure ("unifyAcyclicRawWithRaiseTrace failed: " ++ show err)
                Right (trace, _st1) ->
                    trace `shouldBe` [a, b]

            case runPresolutionM defaultTraceConfig st0 (runEdgeUnifyForTest r interior a b) of
                Left err -> expectationFailure ("runEdgeUnifyForTest failed: " ++ show err)
                Right (ops, _st1) -> do
                    ops `shouldSatisfy` elem (OpRaise a)
                    ops `shouldNotSatisfy` elem (OpRaise b)

        it "treats a nested restricted interior node as identity" $ do
            let a = NodeId 3
                b = NodeId 4
                p1 = NodeId 1
                p2 = NodeId 2
                common = NodeId 5
                root = NodeId 0
                nodes =
                    nodeMapFromList
                        [ (getNodeId a, TyVar a Nothing)
                        , (getNodeId b, TyVar b Nothing)
                        , (getNodeId p1, TyForall p1 a)
                        , (getNodeId p2, TyForall p2 b)
                        , (getNodeId common, TyArrow common p1 p2)
                        , (getNodeId root, TyArrow root common common)
                        ]
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cBindParents =
                            bindParentsFromPairs
                                [ (common, root, BindRigid)
                                , (p1, common, BindFlex)
                                , (a, p1, BindRigid)
                                , (p2, common, BindFlex)
                                , (b, p2, BindRigid)
                                ]
                        }
                st0 = stateFor constraint 6
                interior = IntSet.singleton (getNodeId a)

            Binding.nodeKind constraint (typeRef a)
                `shouldBe` Right Binding.NodeRestricted

            case runPresolutionM defaultTraceConfig st0 (unifyAcyclicRawWithRaiseTrace a b) of
                Left err -> expectationFailure ("restricted unification failed: " ++ show err)
                Right (trace, _st1) ->
                    trace `shouldBe` [a, b]

            case runPresolutionM defaultTraceConfig st0 (runEdgeUnifyForTest root interior a b) of
                Left err -> expectationFailure ("edge unification failed: " ++ show err)
                Right (ops, _st1) ->
                    ops `shouldNotSatisfy` elem (OpRaise a)


    describe "Property tests for OpRaise on interior nodes" $ do
        it "presolution preserves binding tree validity" $ do
            -- **Feature: paper_general_raise_plan, Property 1: Binding tree preservation**
            -- **Validates: Requirements 5.3, 7.3**
            --
            -- After presolution processes an instantiation edge, the binding tree
            -- should still be valid (checkBindingTree succeeds).
            let a = NodeId 0
                intNode = NodeId 1
                arrow = NodeId 2
                forallNode = NodeId 3
                expNode = NodeId 4
                y = NodeId 5
                targetArrow = NodeId 6

                nodes = nodeMapFromList
                        [ (0, TyVar { tnId = a, tnBound = Nothing })
                        , (1, TestTyBase intNode (BaseTy "Int"))
                        , (2, TyArrow arrow a intNode)
                        , (3, TyForall forallNode arrow)
                        , (4, TyExp expNode (ExpVarId 0) forallNode)
                        , (5, TyVar { tnId = y, tnBound = Nothing })
                        , (6, TyArrow targetArrow y intNode)
                        ]

                bindParents =
                    bindParentsFromPairs
                        [ (a, forallNode, BindFlex)
                        , (intNode, arrow, BindFlex)
                        , (arrow, forallNode, BindFlex)
                        , (forallNode, expNode, BindFlex)
                        , (y, targetArrow, BindFlex)
                        ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                st0 =
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty
                        7
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    let finalConstraint = psConstraint st1
                        uf = psUnionFind st1
                    -- The binding tree should still be valid after presolution (up to UF).
                    Binding.checkBindingTreeUnder (UF.frWith uf) finalConstraint `shouldBe` Right ()

        it "replay: applying recorded OpRaise reproduces presolution binding parents" $ property $
            forAll (choose (1, 10)) $ \leftDepth ->
                forAll (choose (1, 10)) $ \rightDepth -> do
                    let rootArrow = NodeId 0

                        leftStart = 1
                        leftVarId = leftStart + leftDepth
                        rightStart = leftVarId + 1
                        rightVarId = rightStart + rightDepth

                        leftForalls =
                            [ (nid, TyForall (NodeId nid) (NodeId body))
                            | (k, nid) <- zip [0 ..] [leftStart .. leftStart + leftDepth - 1]
                            , let body = if k == leftDepth - 1 then leftVarId else nid + 1
                            ]

                        rightForalls =
                            [ (nid, TyForall (NodeId nid) (NodeId body))
                            | (k, nid) <- zip [0 ..] [rightStart .. rightStart + rightDepth - 1]
                            , let body = if k == rightDepth - 1 then rightVarId else nid + 1
                            ]

                        nodes = nodeMapFromList $
                                [ (getNodeId rootArrow, TyArrow rootArrow (NodeId leftStart) (NodeId rightStart))
                                ]
                                    ++ leftForalls
                                    ++ rightForalls
                                    ++ [ (leftVarId, TyVar { tnId = NodeId leftVarId, tnBound = Nothing })
                                       , (rightVarId, TyVar { tnId = NodeId rightVarId, tnBound = Nothing })
                                       ]

                        bindParents =
                            bindParentsFromPairs $
                                -- bind the outermost foralls to the arrow root
                                [ (NodeId leftStart, rootArrow, BindFlex)
                                , (NodeId rightStart, rootArrow, BindFlex)
                                ]
                                    ++
                                    -- chain the inner foralls
                                    [ (NodeId nid, NodeId (nid - 1), BindFlex)
                                    | nid <- [leftStart + 1 .. leftStart + leftDepth - 1]
                                    ]
                                    ++ [ (NodeId nid, NodeId (nid - 1), BindFlex)
                                       | nid <- [rightStart + 1 .. rightStart + rightDepth - 1]
                                       ]
                                    ++
                                    -- bind leaf vars to their innermost foralls
                                    [ (NodeId leftVarId, NodeId (leftStart + leftDepth - 1), BindFlex)
                                    , (NodeId rightVarId, NodeId (rightStart + rightDepth - 1), BindFlex)
                                    ]

                        constraint0 =
                            rootedConstraint emptyConstraint
                                { cNodes = nodes
                                , cBindParents = bindParents
                                }

                        st0 =
                            PresolutionState constraint0 (Presolution IntMap.empty)
                                IntMap.empty
                                (rightVarId + 1)
                                IntSet.empty
                                IntMap.empty
                                IntMap.empty
                                IntMap.empty
                                IntMap.empty
                                IntMap.empty
                        interior = IntSet.fromList [0 .. rightVarId]

                        replayRaises :: Constraint 'Raw -> [InstanceOp] -> Either BindingError (Constraint 'Raw)
                        replayRaises c ops0 = go c ops0
                          where
                            go c' [] = Right c'
                            go c' (OpRaise nid : rest) = do
                                (c'', _mOp) <- GraphOps.applyRaiseStep (TypeRefTag nid) c'
                                go c'' rest
                            go c' (_ : rest) = go c' rest

                        leftVar = NodeId leftVarId
                        rightVar = NodeId rightVarId

                    case runPresolutionM defaultTraceConfig st0 (runEdgeUnifyForTest rootArrow interior leftVar rightVar) of
                        Left err ->
                            expectationFailure ("runEdgeUnifyForTest failed: " ++ show err)
                        Right (ops, st1) -> do
                            let finalConstraint = psConstraint st1
                            case replayRaises constraint0 ops of
                                Left err ->
                                    expectationFailure ("replay failed: " ++ show err)
                                Right replayed -> do
                                    let uf = psUnionFind st1
                                        canonical = UF.frWith uf
                                    case ( Binding.canonicalizeBindParentsUnder canonical replayed
                                         , Binding.canonicalizeBindParentsUnder canonical finalConstraint
                                         ) of
                                        (Left err, _) ->
                                            expectationFailure ("replay: canonicalizeBindParentsUnder failed: " ++ show err)
                                        (_, Left err) ->
                                            expectationFailure ("final: canonicalizeBindParentsUnder failed: " ++ show err)
                                        (Right bpReplay, Right bpFinal) ->
                                            bpReplay `shouldBe` bpFinal

recursiveRigidPairFixture
    :: Bool
    -> (Constraint 'Raw, NodeId, NodeId, NodeId, NodeId, IntSet.IntSet)
recursiveRigidPairFixture inconsistentOccurrence =
    (constraint, sourceMu, targetMu, sourceBinder, targetBinder, interior)
  where
    rootGen = GenNodeId 0
    sourceBinder = NodeId 0
    sourceBody = NodeId 1
    sourceMu = NodeId 2
    targetBinder = NodeId 3
    targetOther = NodeId 4
    targetBody = NodeId 5
    targetMu = NodeId 6
    rigidOwner = NodeId 7
    targetCodomain =
        if inconsistentOccurrence
            then targetOther
            else targetBinder
    nodes =
        nodeMapFromList
            ( [ (getNodeId sourceBinder, TyVar sourceBinder Nothing)
              , (getNodeId sourceBody, TyArrow sourceBody sourceBinder sourceBinder)
              , (getNodeId sourceMu, TyMu sourceMu sourceBody)
              , (getNodeId targetBinder, TyVar targetBinder Nothing)
              , (getNodeId targetBody, TyArrow targetBody targetBinder targetCodomain)
              , (getNodeId targetMu, TyMu targetMu targetBody)
              , (getNodeId rigidOwner, TyForall rigidOwner sourceMu)
              ]
                ++ [ (getNodeId targetOther, TyVar targetOther Nothing)
                   | inconsistentOccurrence
                   ]
            )
    constraint =
        emptyConstraint
            { cNodes = nodes
            , cGenNodes =
                fromListGen
                    [(rootGen, GenNode rootGen [rigidOwner, targetMu])]
            , cBindParents =
                IntMap.fromList
                    ( [ (nodeRefKey (typeRef rigidOwner), (genRef rootGen, BindRigid))
                      , (nodeRefKey (typeRef sourceMu), (typeRef rigidOwner, BindFlex))
                      , (nodeRefKey (typeRef sourceBody), (typeRef sourceMu, BindFlex))
                      , (nodeRefKey (typeRef sourceBinder), (typeRef sourceMu, BindFlex))
                      , (nodeRefKey (typeRef targetMu), (genRef rootGen, BindRigid))
                      , (nodeRefKey (typeRef targetBody), (typeRef targetMu, BindFlex))
                      , (nodeRefKey (typeRef targetBinder), (typeRef targetMu, BindRigid))
                      ]
                        ++ [ (nodeRefKey (typeRef targetOther), (typeRef targetBody, BindFlex))
                           | inconsistentOccurrence
                           ]
                    )
            }
    interior =
        IntSet.fromList
            (map getNodeId [sourceBinder, sourceBody, sourceMu])

stateFor :: Constraint p -> Int -> PresolutionState p
stateFor constraint nextNodeId =
    PresolutionState constraint (Presolution IntMap.empty)
        IntMap.empty
        nextNodeId
        IntSet.empty
        IntMap.empty
        IntMap.empty
        IntMap.empty
        IntMap.empty
        IntMap.empty
