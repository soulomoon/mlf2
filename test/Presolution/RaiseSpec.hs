module Presolution.RaiseSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
    ( EdgeWitness(..)
    , InstanceOp(..)
    , InstanceWitness(..)
    )
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionError(..)
    , PresolutionState(..)
    , lookupCopy
    , processInstEdge
    , runEdgeUnifyForTest
    , runPresolutionM
    , unifyAcyclicRawWithRaiseTrace
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

spec :: Spec
spec = do
    describe "Phase 4 — OpRaise for interior nodes" $ do
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
                        , (1, TyBase intNode (BaseTy "Int"))
                        , (2, TyArrow innerArrow a intNode)
                        , (3, TyForall forallNode innerArrow)
                        , (4, TyExp expNode (ExpVarId 0) forallNode)
                        , (5, TyVar { tnId = y, tnBound = Nothing })
                        , (6, TyArrow targetArrow y intNode)
                        ]

                -- Mark innerArrow as rigidly bound (locked)
                bindParents =
                    bindParentsFromPairs
                        [ (a, forallNode, BindFlex)
                        , (intNode, innerArrow, BindFlex)
                        , (innerArrow, forallNode, BindRigid)  -- Rigid!
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
                -- Check that no OpRaise targets the rigidly bound innerArrow
                isRaiseOnRigid op = case op of
                    OpRaise n -> n == innerArrow
                    _ -> False

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
                        -- the copied inner arrow during unification.
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
                    ops `shouldSatisfy` any (isRaiseOn copiedInner)

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
                        , (1, TyBase intNode (BaseTy "Int"))
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
                        interior = IntSet.fromList [0 .. rightVarId]

                        replayRaises :: Constraint -> [InstanceOp] -> Either BindingError Constraint
                        replayRaises c ops0 = go c ops0
                          where
                            go c' [] = Right c'
                            go c' (OpRaise nid : rest) = do
                                (c'', _mOp) <- GraphOps.applyRaiseStep (typeRef nid) c'
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
