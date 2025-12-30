module PresolutionSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (findIndex)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import MLF.Constraint.Types
import MLF.Constraint.Presolution
import MLF.Constraint.Presolution.Witness (OmegaNormalizeEnv(..), OmegaNormalizeError(..), coalesceRaiseMergeWithEnv, integratePhase2Ops, normalizeInstanceOpsFull, normalizeInstanceStepsFull, reorderWeakenWithEnv, validateNormalizedWitness, witnessFromExpansion)
import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import MLF.Constraint.Solve (SolveResult(..), validateSolvedGraphStrict)
import qualified MLF.Constraint.Inert as Inert
import qualified MLF.Binding.Tree as Binding
import qualified MLF.Binding.GraphOps as GraphOps
import qualified MLF.Util.UnionFind as UF
import qualified MLF.Util.Order as Order
import SpecUtil (emptyConstraint, inferBindParents, lookupNodeMaybe)

expectArrow :: HasCallStack => IntMap.IntMap TyNode -> NodeId -> IO TyNode
expectArrow nodes nid = case lookupNodeMaybe nodes nid of
    Just a@TyArrow{} -> return a
    other -> do
        let msg = "Expected TyArrow at " ++ show nid ++ ", found " ++ show other
        expectationFailure msg >> fail msg

expectForall :: HasCallStack => IntMap.IntMap TyNode -> NodeId -> IO TyNode
expectForall nodes nid = case lookupNodeMaybe nodes nid of
    Just f@TyForall{} -> return f
    other -> do
        let msg = "Expected TyForall at " ++ show nid ++ ", found " ++ show other
        expectationFailure msg >> fail msg

mkNormalizeConstraint :: Constraint
mkNormalizeConstraint =
    let root = NodeId 0
        arrow = NodeId 1
        dom = NodeId 2
        cod = NodeId 3
    in emptyConstraint
        { cNodes =
            IntMap.fromList
                [ (getNodeId root, TyForall root arrow)
                , (getNodeId arrow, TyArrow arrow dom cod)
                , (getNodeId dom, TyVar { tnId = dom, tnBound = Nothing })
                , (getNodeId cod, TyVar { tnId = cod, tnBound = Nothing })
                ]
        , cBindParents =
            IntMap.fromList
                [ (getNodeId arrow, (root, BindFlex))
                , (getNodeId dom, (root, BindFlex))
                , (getNodeId cod, (root, BindFlex))
                ]
        }

mkNormalizeEnv :: Constraint -> NodeId -> IntSet.IntSet -> OmegaNormalizeEnv
mkNormalizeEnv c root interior =
    OmegaNormalizeEnv
        { oneRoot = root
        , interior = interior
        , weakened = IntSet.empty
        , orderKeys = Order.orderKeysFromRootWith id (cNodes c) root Nothing
        , canonical = id
        , constraint = c
        , binderArgs = IntMap.empty
        }

orderedPairByPrec :: Constraint -> NodeId -> (NodeId, NodeId)
orderedPairByPrec c root =
    let n1 = NodeId 2
        n2 = NodeId 3
        keys = Order.orderKeysFromRootWith id (cNodes c) root Nothing
        k1 = keys IntMap.! getNodeId n1
        k2 = keys IntMap.! getNodeId n2
    in if Order.compareOrderKey k1 k2 == LT
        then (n1, n2)
        else (n2, n1)

spec :: Spec
spec = describe "Phase 4 — Principal Presolution" $ do
    describe "instantiateScheme" $ do
        it "replaces repeated bound vars with the same fresh node" $ do
            -- Scheme body (a -> a) where `a` is a bound variable to be substituted.
            let bound = NodeId 1
                body = NodeId 2
                fresh = NodeId 10
                nodes = IntMap.fromList
                    [ (1, TyVar { tnId = bound, tnBound = Nothing })
                    , (2, TyArrow body bound bound)
                    , (10, TyVar { tnId = fresh, tnBound = Nothing }) -- fresh binder image
                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = IntMap.singleton 1 (NodeId 2, BindFlex)
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty

            case runPresolutionM st0 (instantiateScheme body [(bound, fresh)]) of
                Left err -> expectationFailure $ "Instantiation failed: " ++ show err
                Right (root, st1) -> do
                    arrow <- expectArrow (cNodes (psConstraint st1)) root
                    let d = tnDom arrow
                        c = tnCod arrow
                    d `shouldBe` fresh
                    c `shouldBe` fresh

        it "shares outer-scope variables outside I(g)" $ do
            -- Body uses bound var and an outer var. Outer nodes are shared when they
            -- are not in the binder’s interior I(g) (paper `xmlf.txt` §3.2).
            let bound = NodeId 1
                outer = NodeId 3
                body = NodeId 2
                outerArrow = NodeId 4
                fresh = NodeId 10
                nodes = IntMap.fromList
                    [ (1, TyVar { tnId = bound, tnBound = Nothing })
                    , (2, TyArrow body bound outer)
                    , (3, TyVar { tnId = outer, tnBound = Nothing })
                    , (4, TyArrow outerArrow outer outer)
                    , (10, TyVar { tnId = fresh, tnBound = Nothing })
                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents =
                            IntMap.fromList
                                [ (1, (NodeId 2, BindFlex))
                                , (3, (NodeId 4, BindFlex))
                                ]
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty

            case runPresolutionM st0 (instantiateScheme body [(bound, fresh)]) of
                Left err -> expectationFailure $ "Instantiation failed: " ++ show err
                Right (root, st1) -> do
                    arrow <- expectArrow (cNodes (psConstraint st1)) root
                    let d = tnDom arrow
                        c = tnCod arrow
                    d `shouldBe` fresh
                    c `shouldBe` outer -- shared, not copied

        it "instantiateSchemeWithTrace shares non-interior structure nodes (I(g) copy)" $ do
            -- In binding-edge mode, the paper’s expansion copies only nodes in I(g).
            -- Here, `outerArrow` is structurally under the ∀ body, but it is bound
            -- above the ∀ binder (so it is not in I(g)) and must be shared.
            let b = NodeId 1
                y = NodeId 2
                outerArrow = NodeId 3
                bodyArrow = NodeId 4
                forallNode = NodeId 5
                expNode = NodeId 6
                meta = NodeId 10

                nodes =
                    IntMap.fromList
                        [ (getNodeId b, TyVar { tnId = b, tnBound = Nothing })
                        , (getNodeId y, TyVar { tnId = y, tnBound = Nothing })
                        , (getNodeId outerArrow, TyArrow outerArrow y y)
                        , (getNodeId bodyArrow, TyArrow bodyArrow outerArrow b)
                        , (getNodeId forallNode, TyForall forallNode bodyArrow)
                        , (getNodeId expNode, TyExp expNode (ExpVarId 0) forallNode)
                        , (getNodeId meta, TyVar { tnId = meta, tnBound = Nothing })
                        ]

                -- Binding edges:
                --   expNode
                --    └─ forallNode
                --        └─ bodyArrow
                --            ├─ b        (in I(g))
                --            └─ outerArrow (NOT in I(g): bound directly to expNode)
                bindParents =
                    IntMap.fromList
                        [ (getNodeId forallNode, (expNode, BindFlex))
                        , (getNodeId bodyArrow, (forallNode, BindFlex))
                        , (getNodeId b, (bodyArrow, BindFlex))
                        , (getNodeId outerArrow, (expNode, BindFlex))
                        , (getNodeId y, (outerArrow, BindFlex))
                        ]

                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        11
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM st0 (instantiateSchemeWithTrace bodyArrow [(b, meta)]) of
                Left err -> expectationFailure ("instantiateSchemeWithTrace failed: " ++ show err)
                Right ((root, copyMap, _interior), st1) -> do
                    arrow <- expectArrow (cNodes (psConstraint st1)) root
                    tnDom arrow `shouldBe` outerArrow
                    IntMap.lookup (getNodeId outerArrow) copyMap `shouldBe` Nothing

        it "instantiateSchemeWithTrace uses I(g) even when root has no binder (no level fallback)" $ do
            -- When copying a disconnected component (e.g. an instance bound),
            -- the copied root may be a binding root. In that case, we still
            -- decide share/copy purely from binding-edge interior membership.
            --
            -- Regression: a legacy fallback would copy `y` below even though it
            -- is not in I(g).
            let y = NodeId 1
                b = NodeId 2
                outerArrow = NodeId 3
                bodyArrow = NodeId 4

                nodes =
                    IntMap.fromList
                        [ (getNodeId y, TyVar { tnId = y, tnBound = Nothing })
                        , (getNodeId b, TyVar { tnId = b, tnBound = Nothing })
                        , (getNodeId outerArrow, TyArrow outerArrow y y)
                        , (getNodeId bodyArrow, TyArrow bodyArrow y b)
                        ]

                -- Binding edges:
                --   b is bound to the body root (so b ∈ I(bodyArrow))
                --   y is bound to an unrelated outer arrow (so y ∉ I(bodyArrow))
                bindParents =
                    IntMap.fromList
                        [ (getNodeId b, (bodyArrow, BindFlex))
                        , (getNodeId y, (outerArrow, BindFlex))
                        ]

                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        10
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM st0 (instantiateSchemeWithTrace bodyArrow []) of
                Left err -> expectationFailure ("instantiateSchemeWithTrace failed: " ++ show err)
                Right ((root, copyMap, _interior), st1) -> do
                    arrow <- expectArrow (cNodes (psConstraint st1)) root
                    tnDom arrow `shouldBe` y

                    case IntMap.lookup (getNodeId b) copyMap of
                        Nothing -> expectationFailure "Expected b to be copied (in I(g))"
                        Just b' -> do
                            b' `shouldNotBe` b
                            tnCod arrow `shouldBe` b'

                    IntMap.lookup (getNodeId y) copyMap `shouldBe` Nothing

        it "copies shared substructure only once (cache reuse)" $ do
            -- Body: (a1 -> a1) used twice as dom/cod; copy should reuse the same new node.
            let bound = NodeId 1
                shared = NodeId 5
                body = NodeId 6
                fresh = NodeId 10
                nodes = IntMap.fromList
                    [ (1, TyVar { tnId = bound, tnBound = Nothing })
                    , (5, TyArrow shared bound bound)    -- shared substructure
                    , (6, TyArrow body shared shared)    -- uses shared twice
                    , (10, TyVar { tnId = fresh, tnBound = Nothing })
                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents =
                            IntMap.fromList
                                [ (1, (NodeId 5, BindFlex))
                                , (5, (NodeId 6, BindFlex))
                                ]
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty

            case runPresolutionM st0 (instantiateScheme body [(bound, fresh)]) of
                Left err -> expectationFailure $ "Instantiation failed: " ++ show err
                Right (root, st1) -> do
                    arrow <- expectArrow (cNodes (psConstraint st1)) root
                    let d = tnDom arrow
                        c = tnCod arrow
                    innerArrow <- expectArrow (cNodes (psConstraint st1)) d
                    -- dom and cod of outer arrow should point to the same copied sub-node
                    d `shouldBe` c
                    -- inner arrow’s dom/cod both use the same fresh substitution
                    tnDom innerArrow `shouldBe` fresh
                    tnCod innerArrow `shouldBe` fresh

        it "shares base nodes (base sharing optimization)" $ do
            -- Body uses the same base node twice; instantiate should not duplicate it.
            let base = NodeId 2
                bound = NodeId 1
                body = NodeId 3
                fresh = NodeId 10
                nodes = IntMap.fromList
                    [ (1, TyVar { tnId = bound, tnBound = Nothing })
                    , (2, TyBase base (BaseTy "int"))
                    , (3, TyArrow body base base)
                    , (10, TyVar { tnId = fresh, tnBound = Nothing })
                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = IntMap.singleton 2 (NodeId 3, BindFlex)
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty

            case runPresolutionM st0 (instantiateScheme body [(bound, fresh)]) of
                Left err -> expectationFailure $ "Instantiation failed: " ++ show err
                Right (root, st1) -> do
                    arrow <- expectArrow (cNodes (psConstraint st1)) root
                    tnDom arrow `shouldBe` base
                    tnCod arrow `shouldBe` base

        it "copies nested forall inside the body" $ do
            -- Nested binder is copied, and substitutions apply under it.
            let outer = NodeId 1
                innerVar = NodeId 2
                innerBody = NodeId 3
                innerForall = NodeId 4
                topBody = NodeId 5
                freshOuter = NodeId 10
                freshInner = NodeId 11
                nodes = IntMap.fromList
                    [ (1, TyVar { tnId = outer, tnBound = Nothing })
                    , (2, TyVar { tnId = innerVar, tnBound = Nothing })
                    , (3, TyArrow innerBody innerVar outer)
                    , (4, TyForall innerForall innerBody)
                    , (5, TyArrow topBody innerForall innerForall)
                    , (10, TyVar { tnId = freshOuter, tnBound = Nothing })
                    , (11, TyVar { tnId = freshInner, tnBound = Nothing })
                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents =
                            IntMap.fromList
                                [ (1, (NodeId 3, BindFlex))
                                , (2, (NodeId 3, BindFlex))
                                , (3, (NodeId 4, BindFlex))
                                , (4, (NodeId 5, BindFlex))
                                ]
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 12 IntSet.empty IntMap.empty IntMap.empty IntMap.empty

            case runPresolutionM st0 (instantiateScheme topBody [(outer, freshOuter), (innerVar, freshInner)]) of
                Left err -> expectationFailure $ "Instantiation failed: " ++ show err
                Right (root, st1) -> do
                    let nodes' = cNodes (psConstraint st1)
                    arrow <- expectArrow nodes' root
                    let d = tnDom arrow
                        c = tnCod arrow
                    forall1 <- expectForall nodes' d
                    forall2 <- expectForall nodes' c
                    let innerCopy = tnBody forall1
                        innerCopy2 = tnBody forall2
                    innerArrow <- expectArrow nodes' innerCopy
                    innerCopy `shouldBe` innerCopy2
                    tnDom innerArrow `shouldBe` freshInner
                    tnCod innerArrow `shouldBe` freshOuter

        it "copies nested expansion nodes inside the body" $ do
            -- When copying in presolution, an expansion node with identity recipe is inlined.
            let bound = NodeId 1
                forallBody = NodeId 2
                forallNode = NodeId 3
                expNode = NodeId 4
                outerBody = NodeId 5
                fresh = NodeId 10
                nodes = IntMap.fromList
                    [ (1, TyVar { tnId = bound, tnBound = Nothing })
                    , (2, TyArrow forallBody bound bound)
                    , (3, TyForall forallNode forallBody)
                    , (4, TyExp expNode (ExpVarId 9) forallNode)
                    , (5, TyArrow outerBody expNode expNode)
                    , (10, TyVar { tnId = fresh, tnBound = Nothing })
                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents =
                            IntMap.fromList
                                [ (1, (NodeId 2, BindFlex))
                                , (2, (NodeId 3, BindFlex))
                                , (3, (NodeId 4, BindFlex))
                                , (4, (NodeId 5, BindFlex))
                                ]
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty

            case runPresolutionM st0 (instantiateScheme outerBody [(bound, fresh)]) of
                Left err -> expectationFailure $ "Instantiation failed: " ++ show err
                Right (root, st1) -> do
                    let nodes' = cNodes (psConstraint st1)
                    arrow <- expectArrow nodes' root
                    let d = tnDom arrow
                        c = tnCod arrow

                    _ <- expectForall nodes' d
                    _ <- expectForall nodes' c

                    d `shouldBe` c

                    d `shouldNotBe` forallNode

                    let forallCopy = d
                    fNode <- expectForall nodes' forallCopy

                    let bodyArrowId = tnBody fNode
                    bArrow <- expectArrow nodes' bodyArrowId
                    tnDom bArrow `shouldBe` fresh
                    tnCod bArrow `shouldBe` fresh

        it "returns error when a node is missing" $ do
            -- Substitution refers to a missing node; should throw NodeLookupFailed.
            let bound = NodeId 1
                body = NodeId 99  -- missing
                fresh = NodeId 10
                nodes = IntMap.fromList
                    [ (1, TyVar { tnId = bound, tnBound = Nothing })
                    , (10, TyVar { tnId = fresh, tnBound = Nothing })
                    ]
                constraint = emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty

            case runPresolutionM st0 (instantiateScheme body [(bound, fresh)]) of
                Left (NodeLookupFailed nid) -> nid `shouldBe` body
                Left other -> expectationFailure $ "Unexpected error: " ++ show other
                Right _ -> expectationFailure "Expected failure due to missing node"

    describe "EdgeTrace" $ do
        it "records nodes allocated while solving an instantiation edge" $ do
            -- TyExp s · (∀@1. a -> a) ≤ (Int -> Int)
            let a = NodeId 0
                arrow = NodeId 1
                forallNode = NodeId 2
                expNode = NodeId 3
                intNode = NodeId 4
                targetArrow = NodeId 5

                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = a, tnBound = Nothing })
                        , (1, TyArrow arrow a a)
                        , (2, TyForall forallNode arrow)
                        , (3, TyExp expNode (ExpVarId 0) forallNode)
                        , (4, TyBase intNode (BaseTy "Int"))
                        , (5, TyArrow targetArrow intNode intNode)
                        ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents =
                            IntMap.fromList
                                [ (getNodeId a, (forallNode, BindFlex))
                                , (getNodeId arrow, (forallNode, BindFlex))
                                , (getNodeId forallNode, (expNode, BindFlex))
                                , (getNodeId intNode, (targetArrow, BindFlex))
                                ]
                        }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        6
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM st0 (processInstEdge edge) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    let traces = psEdgeTraces st1
                    case IntMap.lookup 0 traces of
                        Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0"
                        Just tr -> do
                            etRoot tr `shouldBe` UF.frWith (psUnionFind st1) expNode
                            etInterior tr `shouldSatisfy` (not . IntSet.null)
                            case etBinderArgs tr of
                                [(bv, _arg)] -> do
                                    bv `shouldBe` a
                                    case IntMap.lookup (getNodeId bv) (etCopyMap tr) of
                                        Nothing -> expectationFailure "Expected binder meta in etCopyMap"
                                        Just meta ->
                                            let metaC = UF.frWith (psUnionFind st1) meta
                                            in IntSet.member (getNodeId metaC) (etInterior tr) `shouldBe` True
                                other -> expectationFailure ("Unexpected binder/arg pairs: " ++ show other)

        it "tracks binder-argument nodes across merged expansions" $ do
            -- When an expansion variable is reused across multiple instantiation edges, the
            -- *final* expansion may keep the argument nodes allocated by an earlier edge
            -- (mergeExpansions keeps the first ExpInstantiate payload).
            --
            -- Phase 1 in plans/merge_raise_merge_plan.txt expects the trace to record the
            -- exact I(r) (the expansion interior) for each edge in binding-edge mode.
            -- We should include the binder
            -- argument node even when it is reused across edges.
            let a = NodeId 0
                arrow = NodeId 1
                forallNode = NodeId 2
                expNode = NodeId 3
                intNode = NodeId 4
                target1 = NodeId 5
                boolNode = NodeId 6
                target2 = NodeId 7

                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = a, tnBound = Nothing })
                        , (1, TyArrow arrow a a)
                        , (2, TyForall forallNode arrow)
                        , (3, TyExp expNode (ExpVarId 0) forallNode)
                        , (4, TyBase intNode (BaseTy "Int"))
                        , (5, TyArrow target1 intNode intNode)
                        , (6, TyBase boolNode (BaseTy "Bool"))
                        , (7, TyArrow target2 boolNode boolNode)
                        ]

                edge0 = InstEdge (EdgeId 0) expNode target1
                edge1 = InstEdge (EdgeId 1) expNode target2
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge0, edge1]
                        , cBindParents =
                            IntMap.fromList
                                [ (getNodeId a, (forallNode, BindFlex))
                                , (getNodeId arrow, (forallNode, BindFlex))
                                , (getNodeId forallNode, (expNode, BindFlex))
                                , (getNodeId intNode, (target1, BindFlex))
                                , (getNodeId boolNode, (target2, BindFlex))
                                ]
                        }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        8
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM st0 (processInstEdge edge0 >> processInstEdge edge1) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    let traces = psEdgeTraces st1
                    case (IntMap.lookup 0 traces, IntMap.lookup 1 traces) of
                        (Just tr0, Just tr1) -> do
                            -- sanity: first edge created its binder arg
                            case etBinderArgs tr0 of
                                [(bv0, _arg0)] ->
                                    case IntMap.lookup (getNodeId bv0) (etCopyMap tr0) of
                                        Nothing -> expectationFailure "Expected binder meta in etCopyMap (edge0)"
                                        Just meta0 ->
                                            let meta0C = UF.frWith (psUnionFind st1) meta0
                                            in IntSet.member (getNodeId meta0C) (etInterior tr0) `shouldBe` True
                                other -> expectationFailure ("Unexpected binder/arg pairs (edge0): " ++ show other)
                            -- expected: second edge should also include its binder arg in the trace interior
                            case etBinderArgs tr1 of
                                [(bv1, _arg1)] ->
                                    case IntMap.lookup (getNodeId bv1) (etCopyMap tr1) of
                                        Nothing -> expectationFailure "Expected binder meta in etCopyMap (edge1)"
                                        Just meta1 ->
                                            let meta1C = UF.frWith (psUnionFind st1) meta1
                                            in IntSet.member (getNodeId meta1C) (etInterior tr1) `shouldBe` True
                                other -> expectationFailure ("Unexpected binder/arg pairs (edge1): " ++ show other)
                        other -> expectationFailure ("Missing traces: " ++ show other)

        it "binds expansion root at the same binder as the edge target (paper §3.2)" $ do
            -- Paper alignment (`papers/xmlf.txt` §3.2): "the root of the expansion is
            -- bound at the same binder as the target".
            --
            -- Setup: TyExp s · (∀@1. a -> a) ≤ (Int -> Int)
            -- where the target arrow has a binding parent.
            --
            -- After expansion, the expansion root (the forall body) should have the
            -- same binding parent as the target arrow.
            let a = NodeId 0
                arrow = NodeId 1
                forallNode = NodeId 2
                expNode = NodeId 3
                intNode = NodeId 4
                targetArrow = NodeId 5
                outerBinder = NodeId 6  -- A node that will be the binding parent

                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = a, tnBound = Nothing })
                        , (1, TyArrow arrow a a)
                        , (2, TyForall forallNode arrow)
                        , (3, TyExp expNode (ExpVarId 0) forallNode)
                        , (4, TyBase intNode (BaseTy "Int"))
                        , (5, TyArrow targetArrow intNode intNode)
                        , (6, TyForall outerBinder targetArrow)  -- Outer binder
                        ]

                -- Set up binding edges: target arrow is bound by outerBinder
                bindParents = IntMap.fromList
                    [ (getNodeId a, (forallNode, BindFlex))
                    , (getNodeId arrow, (forallNode, BindFlex))
                    , (getNodeId forallNode, (expNode, BindFlex))
                    , (getNodeId intNode, (targetArrow, BindFlex))
                    , (getNodeId targetArrow, (outerBinder, BindFlex))
                    ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents = bindParents
                    }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        7
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM st0 (processInstEdge edge) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    let traces = psEdgeTraces st1
                        c = psConstraint st1
                    case IntMap.lookup 0 traces of
                        Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0"
                        Just tr -> do
                            -- The expansion result (resNodeId) should be bound at the same
                            -- binder as the target. For ExpInstantiate, the result is a copy
                            -- of the forall body (the arrow), which should be in the copyMap.
                            -- We can find it by looking for the copied arrow node.
                            let copyMap = etCopyMap tr
                            -- The copied arrow should be in the copyMap (arrow -> copied arrow)
                            case IntMap.lookup (getNodeId arrow) copyMap of
                                Nothing -> expectationFailure $ 
                                    "Expected arrow to be copied. CopyMap: " ++ show copyMap
                                Just copiedArrow -> do
                                    let uf = psUnionFind st1
                                        copiedArrowC = UF.frWith uf copiedArrow
                                        outerBinderC = UF.frWith uf outerBinder

                                    -- The copied arrow (expansion result) should be bound
                                    -- at the same binder as the target (up to UF).
                                    case Binding.lookupBindParent c copiedArrowC of
                                        Nothing -> expectationFailure $ 
                                            "Expected expansion result " ++ show copiedArrowC ++ 
                                            " to have a binding parent. BindParents: " ++ 
                                            show (cBindParents c)
                                        Just (parentId, _flag) -> parentId `shouldBe` outerBinderC

        it "records exact I(r) on the final canonical constraint (EdgeTrace exact)" $ do
            -- Binding-edge mode: EdgeTrace.etInterior must match the paper
            -- interior computed from the final binding tree.
            let a = NodeId 0
                arrow = NodeId 1
                forallNode = NodeId 2
                expNode = NodeId 3
                intNode = NodeId 4
                targetArrow = NodeId 5
                rootArrow = NodeId 6

                nodes =
                    IntMap.fromList
                        [ (getNodeId a, TyVar { tnId = a, tnBound = Nothing })
                        , (getNodeId arrow, TyArrow arrow a a)
                        , (getNodeId forallNode, TyForall forallNode arrow)
                        , (getNodeId expNode, TyExp expNode (ExpVarId 0) forallNode)
                        , (getNodeId intNode, TyBase intNode (BaseTy "Int"))
                        , (getNodeId targetArrow, TyArrow targetArrow intNode intNode)
                        , (getNodeId rootArrow, TyArrow rootArrow expNode targetArrow)
                        ]

                bindParents =
                    IntMap.fromList
                        [ (getNodeId a, (forallNode, BindFlex))
                        , (getNodeId arrow, (forallNode, BindFlex))
                        , (getNodeId forallNode, (rootArrow, BindFlex))
                        , (getNodeId expNode, (rootArrow, BindFlex))
                        , (getNodeId intNode, (targetArrow, BindFlex))
                        , (getNodeId targetArrow, (rootArrow, BindFlex))
                        ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }

                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge]
                        , arDepGraph = undefined
                        }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure ("Presolution failed: " ++ show err)
                Right PresolutionResult{ prConstraint = c', prEdgeTraces = traces } -> do
                    tr <- case IntMap.lookup 0 traces of
                        Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0" >> fail "missing trace"
                        Just t -> pure t
                    case Binding.interiorOf c' (etRoot tr) of
                        Left err -> expectationFailure ("Binding.interiorOf failed: " ++ show err)
                        Right interior ->
                            interior `shouldBe` etInterior tr

    describe "Phase 2 — Merge/RaiseMerge emission" $ do
        it "records Merge when two instantiation metas unify" $ do
            -- TyExp s · (∀@1. a -> b -> a) ≤ (t -> t -> t)
            --
            -- Instantiation introduces fresh metas for a and b. Unifying the instantiated
            -- body against the target forces those metas to unify, which Phase 2 should
            -- record as a Merge between the corresponding source binders.
            let a = NodeId 0
                b = NodeId 1
                arrow2 = NodeId 2
                arrow1 = NodeId 3
                forallNode = NodeId 4
                expNode = NodeId 5

                t = NodeId 6
                targetArrow2 = NodeId 7
                targetArrow1 = NodeId 8

                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = a, tnBound = Nothing })
                        , (1, TyVar { tnId = b, tnBound = Nothing })
                        , (2, TyArrow arrow2 b a) -- b -> a
                        , (3, TyArrow arrow1 a arrow2) -- a -> (b -> a)
                        , (4, TyForall forallNode arrow1)
                        , (5, TyExp expNode (ExpVarId 0) forallNode)
                        , (6, TyVar { tnId = t, tnBound = Nothing })
                        , (7, TyArrow targetArrow2 t t) -- t -> t
                        , (8, TyArrow targetArrow1 t targetArrow2) -- t -> (t -> t)
                        ]

                edge = InstEdge (EdgeId 0) expNode targetArrow1
                bindParents =
                    IntMap.union
                        (IntMap.fromList
                            [ (getNodeId a, (forallNode, BindFlex))
                            , (getNodeId b, (forallNode, BindFlex))
                            ])
                        (inferBindParents nodes)
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        9
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM st0 (processInstEdge edge) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    case IntMap.lookup 0 (psEdgeWitnesses st1) of
                        Nothing -> expectationFailure "Expected EdgeWitness for EdgeId 0"
                        Just ew -> do
                            let InstanceWitness ops = ewWitness ew
                                hasMerge = any (\op -> case op of OpMerge{} -> True; _ -> False) ops
                            hasMerge `shouldBe` True

                            -- Eliminated binder-metas are recorded persistently so
                            -- elaboration can ignore them when reifying quantifiers.
                            tr <- case IntMap.lookup 0 (psEdgeTraces st1) of
                                Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0" >> fail "missing EdgeTrace"
                                Just tr' -> pure tr'

                            let eliminatedBinders =
                                    [ n | OpMerge n _ <- ops ] ++ [ n | OpRaiseMerge n _ <- ops ]
                                cmap = etCopyMap tr
                                c1 = psConstraint st1

                            eliminatedBinders `shouldSatisfy` (not . null)

                            mapM_
                                (\bv -> do
                                    meta <- case IntMap.lookup (getNodeId bv) cmap of
                                        Nothing -> expectationFailure ("Expected binder-meta in EdgeTrace.etCopyMap for " ++ show bv) >> fail "missing binder-meta"
                                        Just m -> pure m
                                    IntSet.member (getNodeId meta) (cEliminatedVars c1) `shouldBe` True
                                )
                                eliminatedBinders

        it "chooses Merge direction by ≺ (m ≺ n) rather than NodeId" $ do
            -- TyExp s · (∀@1. a -> b) ≤ (t -> t)
            --
            -- Unifying the instantiated result against the target forces the instantiation
            -- metas for `a` and `b` to unify. The witness language requires Merge(n, m)
            -- with m ≺ n, so the representative must be the leftmost binder `a`.
            let a = NodeId 10
                b = NodeId 5
                arrow = NodeId 11
                forallNode = NodeId 12
                expNode = NodeId 13
                t = NodeId 0
                targetArrow = NodeId 14

                nodes =
                    IntMap.fromList
                        [ (getNodeId t, TyVar { tnId = t, tnBound = Nothing })
                        , (getNodeId a, TyVar { tnId = a, tnBound = Nothing })
                        , (getNodeId b, TyVar { tnId = b, tnBound = Nothing })
                        , (getNodeId arrow, TyArrow arrow a b)
                        , (getNodeId forallNode, TyForall forallNode arrow)
                        , (getNodeId expNode, TyExp expNode (ExpVarId 0) forallNode)
                        , (getNodeId targetArrow, TyArrow targetArrow t t)
                        ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                bindParents =
                    IntMap.union
                        (IntMap.fromList
                            [ (getNodeId a, (forallNode, BindFlex))
                            , (getNodeId b, (forallNode, BindFlex))
                            ])
                        (inferBindParents nodes)
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        15
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

                wants op = op == OpMerge b a

            case runPresolutionM st0 (processInstEdge edge) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    case IntMap.lookup 0 (psEdgeWitnesses st1) of
                        Nothing -> expectationFailure "Expected EdgeWitness for EdgeId 0"
                        Just ew -> do
                            let InstanceWitness ops = ewWitness ew
                            ops `shouldSatisfy` any wants

        it "records RaiseMerge when a bounded binder meta escapes to an outer-scope variable" $ do
            -- TyExp s · (∀(b ⩾ x). b -> b) ≤ (y -> y)
            --
            -- Here `b` is *bounded* (non-⊥ bound `x`), but unification of the instantiated
            -- body against the target forces its instantiation meta to unify with `y`
            -- (an outer-scope variable, at an ancestor level). Phase 2 should record
            -- this as a RaiseMerge(b, y) rather than relying on Weaken(b) (which would
            -- substitute the bound `x`).
            let x = NodeId 0
                b = NodeId 1
                arrow1 = NodeId 2
                forallNode = NodeId 3
                expNode = NodeId 4
                y = NodeId 5
                targetArrow = NodeId 6
                rootArrow = NodeId 7

                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = x, tnBound = Nothing })
                        , (1, TyVar { tnId = b, tnBound = Just x })
                        , (2, TyArrow arrow1 b b)
                        , (3, TyForall forallNode arrow1)
                        , (4, TyExp expNode (ExpVarId 0) forallNode)
                        , (5, TyVar { tnId = y, tnBound = Nothing })
                        , (6, TyArrow targetArrow y y)
                        , (7, TyArrow rootArrow expNode targetArrow)
                        ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                bindParents =
                    IntMap.insert
                        (getNodeId b)
                        (forallNode, BindFlex)
                        (inferBindParents nodes)
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure ("computePresolution failed: " ++ show err)
                Right pr -> do
                    tr <- case IntMap.lookup 0 (prEdgeTraces pr) of
                        Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0" >> fail "missing EdgeTrace"
                        Just tr' -> pure tr'
                    meta <- case IntMap.lookup (getNodeId b) (etCopyMap tr) of
                        Nothing -> expectationFailure "Expected binder-meta in EdgeTrace.etCopyMap" >> fail "missing binder-meta"
                        Just m -> pure m
                    let isRaiseMerge op = case op of
                            OpRaiseMerge n m -> n == meta && m == y
                            _ -> False
                        isRaise op = case op of
                            OpRaise{} -> True
                            _ -> False
                        isMerge op = case op of
                            OpMerge{} -> True
                            _ -> False
                        ops =
                            [ op
                            | ew <- IntMap.elems (prEdgeWitnesses pr)
                            , let InstanceWitness xs = ewWitness ew
                            , op <- xs
                            ]
                    ops `shouldSatisfy` (\xs -> any isRaiseMerge xs || (any isRaise xs && any isMerge xs))
                    ops `shouldSatisfy` (not . null)

        it "does not record Raise for unbounded binder metas (graft+weaken only)" $ do
            -- TyExp s · (∀b. b -> b) ≤ (y -> y)
            --
            -- Instantiation introduces a fresh meta for `b` at the inner quantifier level.
            -- Unifying the instantiated body against the target forces that meta to unify
            -- with the outer-scope variable `y`. In the current implementation this is
            -- witnessed as a plain graft + weaken (no Raise/RaiseMerge is emitted here).
            let b = NodeId 1
                arrow1 = NodeId 2
                forallNode = NodeId 3
                expNode = NodeId 4
                y = NodeId 5
                targetArrow = NodeId 6

                nodes =
                    IntMap.fromList
                        [ (1, TyVar { tnId = b, tnBound = Nothing })
                        , (2, TyArrow arrow1 b b)
                        , (3, TyForall forallNode arrow1)
                        , (4, TyExp expNode (ExpVarId 0) forallNode)
                        , (5, TyVar { tnId = y, tnBound = Nothing })
                        , (6, TyArrow targetArrow y y)
                        ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                bindParents =
                    IntMap.insert
                        (getNodeId b)
                        (forallNode, BindFlex)
                        (inferBindParents nodes)
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        7
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

                isGraftToBinder op = case op of
                    OpGraft _ n -> n == b
                    _ -> False
                isRaise op = case op of
                    OpRaise n -> n == b
                    _ -> False
                isRaiseMerge op = case op of
                    OpRaiseMerge n m -> n == b && m == y
                    _ -> False

            case runPresolutionM st0 (processInstEdge edge) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    case IntMap.lookup 0 (psEdgeWitnesses st1) of
                        Nothing -> expectationFailure "Expected EdgeWitness for EdgeId 0"
                        Just ew -> do
                            let InstanceWitness ops = ewWitness ew
                            ops `shouldSatisfy` any isGraftToBinder
                            ops `shouldSatisfy` elem (OpWeaken b)
                            ops `shouldNotSatisfy` any isRaise
                            ops `shouldNotSatisfy` any isRaiseMerge

        it "executes OpWeaken as a binding-edge flag flip (no UF merge with its bound)" $ do
            -- Ensure that executing a base `Weaken(b)` does not substitute/merge the
            -- binder-meta with the instantiation argument (term-DAG), but does flip
            -- the binding-edge flag (flex → rigid) in binding-edge mode.
            --
            -- Note: the implementation defers actually applying Weaken until
            -- presolution finalization (after all edges are processed), so this test
            -- goes through `computePresolution` rather than `processInstEdge`.
            let b = NodeId 1
                arrow1 = NodeId 2
                forallNode = NodeId 3
                expNode = NodeId 4
                y = NodeId 5
                targetArrow = NodeId 6

                nodes =
                    IntMap.fromList
                        [ (1, TyVar { tnId = b, tnBound = Nothing })
                        , (2, TyArrow arrow1 b b)
                        , (3, TyForall forallNode arrow1)
                        , (4, TyExp expNode (ExpVarId 0) forallNode)
                        , (5, TyVar { tnId = y, tnBound = Nothing })
                        , (6, TyArrow targetArrow y y)
                        ]

                -- Binding edges model scope: the binder TyVar { tnId = is, tnBound = Nothing } flexibly bound
                -- directly to its `TyForall` node (paper Q(n)).
                bindParents =
                    IntMap.fromList
                        [ (getNodeId b, (forallNode, BindFlex))
                        , (getNodeId arrow1, (forallNode, BindFlex))
                        , (getNodeId forallNode, (expNode, BindFlex))
                        , (getNodeId y, (targetArrow, BindFlex))
                        ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure ("computePresolution failed: " ++ show err)
                Right pr -> do
                    -- Extract binder-meta and instantiation-arg from the edge trace.
                    tr <- case IntMap.lookup 0 (prEdgeTraces pr) of
                        Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0" >> fail "missing EdgeTrace"
                        Just tr' -> pure tr'

                    let cmap = etCopyMap tr
                    (bv, arg) <- case etBinderArgs tr of
                        [(bv', arg')] -> pure (bv', arg')
                        other ->
                            expectationFailure ("Expected exactly 1 binder arg pair, got: " ++ show other)
                                >> fail "unexpected etBinderArgs shape"

                    bv `shouldBe` b

                    meta <- case IntMap.lookup (getNodeId bv) cmap of
                        Nothing -> expectationFailure "Expected binder-meta in EdgeTrace.etCopyMap" >> fail "missing binder-meta"
                        Just m -> pure m

                    let c1 = prConstraint pr

                    -- 1) Flag flip: the binder-meta's binding edge becomes rigid.
                    case Binding.lookupBindParent c1 meta of
                        Nothing -> expectationFailure "Expected binder-meta to have a binding parent after expansion"
                        Just (_p, flag) -> flag `shouldBe` BindRigid

                    -- 2) No UF merge: meta is not unified with its bound/arg by OpWeaken.
                    meta `shouldNotBe` arg

        it "orders base witness ops as Graft; Merge; Weaken for bounded binders" $ do
            -- TyExp s · (∀@1. a -> b) ≤ (Int -> Int), with b ⩾ a.
            --
            -- The base witness should:
            --   • graft the instantiation arg onto the unbounded binder `a`,
            --   • merge the bounded binder `b` into `a`,
            --   • and weaken `a` last (so it cannot preempt the merge).
            let a = NodeId 0
                b = NodeId 1
                arrow = NodeId 2
                forallNode = NodeId 3
                expNode = NodeId 4
                intNode = NodeId 5
                targetArrow = NodeId 6

                nodes =
                    IntMap.fromList
                        [ (getNodeId a, TyVar { tnId = a, tnBound = Nothing })
                        , (getNodeId b, TyVar { tnId = b, tnBound = Just a })
                        , (getNodeId arrow, TyArrow arrow a b)
                        , (getNodeId forallNode, TyForall forallNode arrow)
                        , (getNodeId expNode, TyExp expNode (ExpVarId 0) forallNode)
                        , (getNodeId intNode, TyBase intNode (BaseTy "Int"))
                        , (getNodeId targetArrow, TyArrow targetArrow intNode intNode)
                        ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                bindParents =
                    IntMap.union
                        (IntMap.fromList
                            [ (getNodeId a, (forallNode, BindFlex))
                            , (getNodeId b, (forallNode, BindFlex))
                            ])
                        (inferBindParents nodes)
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        7
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

                isGraftToA op = case op of
                    OpGraft _ n -> n == a
                    _ -> False
                isMergeBA op = case op of
                    OpMerge n m -> n == b && m == a
                    _ -> False
                isWeakenA op = case op of
                    OpWeaken n -> n == a
                    _ -> False

            case runPresolutionM st0 (processInstEdge edge) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    ew <- case IntMap.lookup 0 (psEdgeWitnesses st1) of
                        Nothing -> expectationFailure "Expected EdgeWitness for EdgeId 0" >> fail "missing EdgeWitness"
                        Just ew0 -> pure ew0

                    let InstanceWitness ops = ewWitness ew
                        idx p = findIndex p ops

                    case (idx isGraftToA, idx isMergeBA, idx isWeakenA) of
                        (Just iG, Just iM, Just iW) -> do
                            iG `shouldSatisfy` (< iM)
                            iM `shouldSatisfy` (< iW)
                        other ->
                            expectationFailure ("Expected Graft(a), Merge(b,a), Weaken(a), got indices: " ++ show other ++ " ops: " ++ show ops)

    describe "Expansion witness steps" $ do
        it "preserves ExpCompose ordering with StepIntro" $ do
            let expNodeId = NodeId 0
                forallId = NodeId 1
                binderId = NodeId 2
                argId = NodeId 3
                nodes = IntMap.fromList
                    [ (0, TyExp expNodeId (ExpVarId 0) forallId)
                    , (1, TyForall forallId binderId)
                    , (2, TyVar { tnId = binderId, tnBound = Nothing })
                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 4 IntSet.empty IntMap.empty IntMap.empty IntMap.empty
                expansion =
                    ExpCompose
                        (ExpForall (ForallSpec 1 [Nothing] NE.:| []) NE.:| [ExpInstantiate [argId]])

            case runPresolutionM st0 (witnessFromExpansion expNodeId (nodes IntMap.! 0) expansion) of
                Left err -> expectationFailure ("witnessFromExpansion failed: " ++ show err)
                Right (steps, _) ->
                    steps `shouldBe`
                        [ StepIntro
                        , StepOmega (OpGraft argId binderId)
                        , StepOmega (OpWeaken binderId)
                        ]

        it "emits StepIntro per binder in ForallSpec" $ do
            let expNodeId = NodeId 0
                bodyId = NodeId 1
                nodes = IntMap.fromList
                    [ (0, TyExp expNodeId (ExpVarId 0) bodyId)
                    , (1, TyVar { tnId = bodyId, tnBound = Nothing })
                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 2 IntSet.empty IntMap.empty IntMap.empty IntMap.empty
                expansion = ExpForall (ForallSpec 2 [Nothing, Nothing] NE.:| [])

            case runPresolutionM st0 (witnessFromExpansion expNodeId (nodes IntMap.! 0) expansion) of
                Left err -> expectationFailure ("witnessFromExpansion failed: " ++ show err)
                Right (steps, _) ->
                    steps `shouldBe` [StepIntro, StepIntro]

    describe "Phase 3 — Witness normalization" $ do
        it "pushes Weaken after ops on strict descendants" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                child = NodeId 1
                arg = NodeId 10
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId child])
                ops0 = [OpWeaken root, OpGraft arg child]
            normalizeInstanceOpsFull env ops0 `shouldBe` Right [OpGraft arg child, OpWeaken root]

        it "does not move Weaken past same-binder ops without descendants" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                n = NodeId 2
                arg = NodeId 10
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                ops0 = [OpWeaken n, OpGraft arg n]
            normalizeInstanceOpsFull env ops0 `shouldBe` Right [OpWeaken n, OpGraft arg n]

        it "does not drop Graft/Weaken when a binder is eliminated by Merge" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                a = NodeId 2
                b = NodeId 3
                arg = NodeId 10
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId a, getNodeId b])
                ops0 = [OpGraft arg b, OpWeaken b, OpMerge b a]
            normalizeInstanceOpsFull env ops0 `shouldBe` Right ops0

        it "drops ops that only touch nodes outside the interior" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                interior = IntSet.fromList [getNodeId (NodeId 2)]
                env = mkNormalizeEnv c root interior
                ops0 =
                    [ OpGraft (NodeId 2) (NodeId 3)
                    , OpWeaken (NodeId 1)
                    , OpMerge (NodeId 3) (NodeId 1)
                    ]
            normalizeInstanceOpsFull env ops0 `shouldBe` Right []

        it "keeps ops under a rigid ancestor that was weakened" $ do
            let root = NodeId 0
                parent = NodeId 1
                child = NodeId 2
                arg = NodeId 10
                nodes =
                    IntMap.fromList
                        [ (getNodeId root, TyRoot root [parent, arg])
                        , (getNodeId parent, TyArrow parent child child)
                        , (getNodeId child, TyVar { tnId = child, tnBound = Nothing })
                        , (getNodeId arg, TyVar { tnId = arg, tnBound = Nothing })
                        ]
                bindParents =
                    IntMap.fromList
                        [ (getNodeId parent, (root, BindRigid))
                        , (getNodeId child, (parent, BindFlex))
                        , (getNodeId arg, (root, BindFlex))
                        ]
                c = emptyConstraint { cNodes = nodes, cBindParents = bindParents }
                interior = IntSet.fromList [getNodeId parent, getNodeId child]
                env =
                    (mkNormalizeEnv c parent interior)
                        { weakened = IntSet.fromList [getNodeId parent] }
                ops0 = [OpGraft arg child]
            normalizeInstanceOpsFull env ops0 `shouldBe` Right ops0

        it "normalizes omega segments without moving StepIntro" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                child = NodeId 2
                arg = NodeId 10
                arg2 = NodeId 11
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId child])
                steps0 =
                    [ StepOmega (OpWeaken root)
                    , StepOmega (OpGraft arg child)
                    , StepIntro
                    , StepOmega (OpGraft arg2 root)
                    ]
            normalizeInstanceStepsFull env steps0
                `shouldBe`
                    Right
                        [ StepOmega (OpGraft arg child)
                        , StepOmega (OpWeaken root)
                        , StepIntro
                        , StepOmega (OpGraft arg2 root)
                        ]

        it "preserves Graft/Weaken when a later Merge eliminates the binder during emission" $ do
            let a = NodeId 2
                b = NodeId 3
                arg = NodeId 10
                baseOps = [OpGraft arg b, OpWeaken b]
                extraOps = [OpMerge b a]
            integratePhase2Ops baseOps extraOps
                `shouldBe` [OpGraft arg b, OpMerge b a, OpWeaken b]

        it "coalesces Raise; Merge into RaiseMerge" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                n = NodeId 2
                m = NodeId 3
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
            normalizeInstanceOpsFull env [OpRaise n, OpMerge n m] `shouldBe` Right [OpRaiseMerge n m]

        it "coalesces multiple Raises followed by Merge into RaiseMerge" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                n = NodeId 2
                m = NodeId 3
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
            normalizeInstanceOpsFull env [OpRaise n, OpRaise n, OpRaise n, OpMerge n m]
                `shouldBe` Right [OpRaiseMerge n m]

        describe "RaiseMerge coalescing (interior aware)" $ do
            it "coalesces Raise; Merge when the target leaves the interior" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                coalesceRaiseMergeWithEnv env [OpRaise n, OpMerge n m]
                    `shouldBe` Right [OpRaiseMerge n m]

            it "errors when Merge leaves the interior without Raise" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                coalesceRaiseMergeWithEnv env [OpMerge n m]
                    `shouldBe` Left (MalformedRaiseMerge [OpMerge n m])

            it "keeps Raise; Merge when the target stays inside the interior" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n, getNodeId m])
                coalesceRaiseMergeWithEnv env [OpRaise n, OpMerge n m]
                    `shouldBe` Right [OpRaise n, OpMerge n m]

            it "coalesces multiple Raises before Merge" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                coalesceRaiseMergeWithEnv env [OpRaise n, OpRaise n, OpMerge n m]
                    `shouldBe` Right [OpRaiseMerge n m]

        describe "Weaken placement (interior aware)" $ do
            let root = NodeId 0
                parent = NodeId 1
                child = NodeId 2
                sibling = NodeId 3
                nodes =
                    IntMap.fromList
                        [ (getNodeId root, TyForall root parent)
                        , (getNodeId parent, TyForall parent child)
                        , (getNodeId child, TyVar { tnId = child, tnBound = Nothing })
                        , (getNodeId sibling, TyVar { tnId = sibling, tnBound = Nothing })
                        ]
                bindParents =
                    IntMap.fromList
                        [ (getNodeId parent, (root, BindFlex))
                        , (getNodeId child, (parent, BindFlex))
                        , (getNodeId sibling, (root, BindFlex))
                        ]
                c = emptyConstraint { cNodes = nodes, cBindParents = bindParents }
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId parent, getNodeId child, getNodeId sibling])

            it "moves Weaken after descendant ops" $ do
                let ops0 = [OpWeaken parent, OpGraft child child]
                reorderWeakenWithEnv env ops0
                    `shouldBe` Right [OpGraft child child, OpWeaken parent]

            it "moves Weaken when descendant is the merge RHS" $ do
                let ops0 = [OpWeaken parent, OpMerge parent child]
                reorderWeakenWithEnv env ops0
                    `shouldBe` Right [OpMerge parent child, OpWeaken parent]

            it "orders descendant Weaken before ancestor when anchors tie" $ do
                let ops0 = [OpWeaken parent, OpWeaken child]
                reorderWeakenWithEnv env ops0
                    `shouldBe` Right [OpWeaken child, OpWeaken parent]

            it "preserves unrelated op order while moving Weaken" $ do
                let ops0 = [OpWeaken parent, OpGraft sibling sibling, OpGraft child child]
                reorderWeakenWithEnv env ops0
                    `shouldBe` Right [OpGraft sibling sibling, OpGraft child child, OpWeaken parent]

        it "normalizeInstanceOpsFull produces validated witnesses when it succeeds" $ property $
            let c = mkNormalizeConstraint
                root = NodeId 0
                env = mkNormalizeEnv c root (IntSet.fromList [1, 2, 3])
                nodes = [NodeId 1, NodeId 2, NodeId 3]
                genNode = elements nodes
                genOp =
                    oneof
                        [ OpGraft <$> genNode <*> genNode
                        , OpMerge <$> genNode <*> genNode
                        , OpRaise <$> genNode
                        , OpWeaken <$> genNode
                        , OpRaiseMerge <$> genNode <*> genNode
                        ]
                genOps = listOf genOp
            in forAll genOps $ \ops ->
                case normalizeInstanceOpsFull env ops of
                    Left _ -> property True
                    Right ops' -> validateNormalizedWitness env ops' === Right ()

        it "allows ops on binders that are later eliminated (paper normalization only)" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                a = NodeId 2
                b = NodeId 3
                arg = NodeId 10
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId a, getNodeId b])
                ops0 = [OpGraft arg b, OpWeaken b, OpMerge b a]
            normalizeInstanceOpsFull env ops0 `shouldBe` Right ops0

        describe "Normalized witness validation" $ do
            it "rejects ops outside the interior (condition 1)" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    env = mkNormalizeEnv c root (IntSet.fromList [2])
                    op = OpGraft (NodeId 2) (NodeId 3)
                validateNormalizedWitness env [op]
                    `shouldBe` Left (OpOutsideInterior op)

            it "rejects Merge with wrong ≺ direction (condition 2)" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    (mLess, nGreater) = orderedPairByPrec c root
                    interior = IntSet.fromList [getNodeId mLess, getNodeId nGreater]
                    env = mkNormalizeEnv c root interior
                    bad = OpMerge mLess nGreater
                validateNormalizedWitness env [bad]
                    `shouldBe` Left (MergeDirectionInvalid mLess nGreater)

            it "rejects Raise outside the interior (condition 3)" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    env = mkNormalizeEnv c root (IntSet.fromList [2])
                    n = NodeId 3
                validateNormalizedWitness env [OpRaise n]
                    `shouldBe` Left (RaiseNotUnderRoot n root)

            it "rejects RaiseMerge when the target stays inside the interior (condition 4)" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    (mLess, nGreater) = orderedPairByPrec c root
                    interior = IntSet.fromList [getNodeId mLess, getNodeId nGreater]
                    env = mkNormalizeEnv c root interior
                    op = OpRaiseMerge nGreater mLess
                validateNormalizedWitness env [op]
                    `shouldBe` Left (RaiseMergeInsideInterior nGreater mLess)

            it "rejects ops below a Weakened binder (condition 5)" $ do
                let root = NodeId 0
                    parent = NodeId 1
                    child = NodeId 2
                    nodes =
                        IntMap.fromList
                            [ (getNodeId root, TyForall root parent)
                            , (getNodeId parent, TyForall parent child)
                            , (getNodeId child, TyVar { tnId = child, tnBound = Nothing })
                            ]
                    bindParents =
                        IntMap.fromList
                            [ (getNodeId parent, (root, BindFlex))
                            , (getNodeId child, (parent, BindFlex))
                            ]
                    c = emptyConstraint { cNodes = nodes, cBindParents = bindParents }
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId parent, getNodeId child])
                    ops = [OpWeaken parent, OpGraft child child]
                validateNormalizedWitness env ops
                    `shouldBe` Left (OpUnderRigid child)

            it "rejects ops below a Weakened binder when merge touches a descendant RHS" $ do
                let root = NodeId 0
                    parent = NodeId 1
                    child = NodeId 2
                    nodes =
                        IntMap.fromList
                            [ (getNodeId root, TyForall root parent)
                            , (getNodeId parent, TyForall parent child)
                            , (getNodeId child, TyVar { tnId = child, tnBound = Nothing })
                            ]
                    bindParents =
                        IntMap.fromList
                            [ (getNodeId parent, (root, BindFlex))
                            , (getNodeId child, (parent, BindFlex))
                            ]
                    c = emptyConstraint { cNodes = nodes, cBindParents = bindParents }
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId parent, getNodeId child])
                    ops = [OpWeaken parent, OpMerge parent child]
                validateNormalizedWitness env ops
                    `shouldBe` Left (OpUnderRigid child)

        describe "Inert-locked detection" $ do
            it "does not mark nodes with flex path to ⊥ as inert-locked" $ do
                let root = NodeId 0
                    mid = NodeId 1
                    n = NodeId 2
                    bottom = NodeId 3
                    base = NodeId 4
                    nodes =
                        IntMap.fromList
                            [ (getNodeId root, TyRoot root [mid])
                            , (getNodeId mid, TyArrow mid n base)
                            , (getNodeId n, TyForall n bottom)
                            , (getNodeId bottom, TyBottom bottom)
                            , (getNodeId base, TyBase base (BaseTy "int"))
                            ]
                    bindParents =
                        IntMap.fromList
                            [ (getNodeId mid, (root, BindRigid))
                            , (getNodeId n, (mid, BindFlex))
                            , (getNodeId bottom, (n, BindFlex))
                            , (getNodeId base, (mid, BindFlex))
                            ]
                    c = emptyConstraint { cNodes = nodes, cBindParents = bindParents }
                case Inert.inertLockedNodes c of
                    Left err -> expectationFailure ("inertLockedNodes failed: " ++ show err)
                    Right s -> IntSet.member (getNodeId n) s `shouldBe` False

            it "identifies inert-locked nodes under rigid ancestors" $ do
                let root = NodeId 0
                    mid = NodeId 1
                    n = NodeId 2
                    v = NodeId 3
                    base = NodeId 4
                    nodes =
                        IntMap.fromList
                            [ (getNodeId root, TyRoot root [mid])
                            , (getNodeId mid, TyArrow mid n base)
                            , (getNodeId n, TyArrow n v base)
                            , (getNodeId v, TyVar { tnId = v, tnBound = Nothing })
                            , (getNodeId base, TyBase base (BaseTy "int"))
                            ]
                    bindParents =
                        IntMap.fromList
                            [ (getNodeId mid, (root, BindRigid))
                            , (getNodeId n, (mid, BindFlex))
                            , (getNodeId v, (n, BindRigid))
                            , (getNodeId base, (n, BindFlex))
                            ]
                    c = emptyConstraint { cNodes = nodes, cBindParents = bindParents }
                case Inert.inertLockedNodes c of
                    Left err -> expectationFailure ("inertLockedNodes failed: " ++ show err)
                    Right s -> IntSet.member (getNodeId n) s `shouldBe` True

            it "treats polymorphic base symbols as inert anchors" $ do
                let root = NodeId 0
                    mid = NodeId 1
                    base = NodeId 2
                    nodes =
                        IntMap.fromList
                            [ (getNodeId root, TyRoot root [mid])
                            , (getNodeId mid, TyArrow mid base base)
                            , (getNodeId base, TyBase base (BaseTy "Poly"))
                            ]
                    bindParents =
                        IntMap.fromList
                            [ (getNodeId mid, (root, BindFlex))
                            , (getNodeId base, (mid, BindFlex))
                            ]
                    c =
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = bindParents
                            , cPolySyms = Set.fromList [BaseTy "Poly"]
                            }
                case Inert.inertNodes c of
                    Left err -> expectationFailure ("inertNodes failed: " ++ show err)
                    Right s -> IntSet.member (getNodeId mid) s `shouldBe` False

            it "weakens inert-locked nodes to rigid bindings" $ do
                let root = NodeId 0
                    mid = NodeId 1
                    n = NodeId 2
                    v = NodeId 3
                    base = NodeId 4
                    nodes =
                        IntMap.fromList
                            [ (getNodeId root, TyRoot root [mid])
                            , (getNodeId mid, TyArrow mid n base)
                            , (getNodeId n, TyArrow n v base)
                            , (getNodeId v, TyVar { tnId = v, tnBound = Nothing })
                            , (getNodeId base, TyBase base (BaseTy "int"))
                            ]
                    bindParents =
                        IntMap.fromList
                            [ (getNodeId mid, (root, BindRigid))
                            , (getNodeId n, (mid, BindFlex))
                            , (getNodeId v, (n, BindRigid))
                            , (getNodeId base, (n, BindFlex))
                            ]
                    c = emptyConstraint { cNodes = nodes, cBindParents = bindParents }
                case Inert.weakenInertLockedNodes c of
                    Left err -> expectationFailure ("weakenInertLockedNodes failed: " ++ show err)
                    Right c' -> do
                        Inert.inertLockedNodes c' `shouldBe` Right IntSet.empty
                        Binding.lookupBindParent c' n `shouldBe` Just (mid, BindRigid)

    describe "decideMinimalExpansion" $ do
        it "returns ExpIdentity for matching monomorphic types" $ do
            let bodyId = NodeId 0
                targetId = NodeId 1
                expNodeId = NodeId 2
                rootId = NodeId 3
                nodes =
                    IntMap.fromList
                        [ (0, TyBase bodyId (BaseTy "int"))
                        , (1, TyBase targetId (BaseTy "int"))
                        , (2, TyExp expNodeId (ExpVarId 0) bodyId)
                        , (3, TyArrow rootId expNodeId targetId)
                        ]
                edge = InstEdge (EdgeId 0) expNodeId targetId
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge]
                        , arDepGraph = undefined -- Not used by computePresolution currently
                        }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps } ->
                    case IntMap.lookup 0 exps of
                        Just ExpIdentity -> pure ()
                        Just other -> expectationFailure $ "Expected ExpIdentity, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for Edge 0"

        it "returns ExpInstantiate for Forall <= Arrow" $ do
            let varId = NodeId 0
                arrowId = NodeId 1
                forallId = NodeId 2
                targetDomId = NodeId 3
                targetCodId = NodeId 4
                targetArrowId = NodeId 5
                expNodeId = NodeId 6
                rootId = NodeId 7
                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = varId, tnBound = Nothing })
                        , (1, TyArrow arrowId varId varId)
                        , (2, TyForall forallId arrowId)
                        , (3, TyBase targetDomId (BaseTy "int"))
                        , (4, TyBase targetCodId (BaseTy "int"))
                        , (5, TyArrow targetArrowId targetDomId targetCodId)
                        , (6, TyExp expNodeId (ExpVarId 0) forallId)
                        , (7, TyArrow rootId expNodeId targetArrowId)
                        ]
                edge = InstEdge (EdgeId 0) expNodeId targetArrowId
                -- Make the forall non-vacuous under binding-edge binder enumeration:
                -- bind the TyVar { tnId = directly, tnBound = Nothing } to the forall node (flex).
                bindParents0 = inferBindParents nodes
                bindParents =
                    IntMap.insert (getNodeId varId) (forallId, BindFlex) bindParents0
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge]
                        , arDepGraph = undefined
                        }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prConstraint = c', prEdgeExpansions = exps } -> do
                    case IntMap.lookup 0 exps of
                        Just (ExpInstantiate _) -> pure ()
                        other -> expectationFailure $ "Expected ExpInstantiate, got " ++ show other

                    let nodes' = cNodes c'
                    case (IntMap.lookup 2 nodes', IntMap.lookup 5 nodes') of
                        (Just _, Just _) -> pure ()
                        _ -> expectationFailure "Nodes 2 and 5 should remain distinct"

        it "returns compose (instantiate then forall) when forall binder arity differs" $ do
            -- s · (∀ a. a) ≤ (∀ b0 b1. b0 → b1)
            -- Different binder counts mean we must instantiate the source binder(s)
            -- and then rewrap with ExpForall matching the target's binder shape.
            let srcVarId = NodeId 0
                srcForallId = NodeId 1
                tgtDomId = NodeId 2
                tgtCodId = NodeId 3
                tgtArrowId = NodeId 4
                tgtForallId = NodeId 5
                expNodeId = NodeId 6

                nodes = IntMap.fromList
                    [ (0, TyVar { tnId = srcVarId, tnBound = Nothing })
                    , (1, TyForall srcForallId srcVarId)
                    , (2, TyVar { tnId = tgtDomId, tnBound = Nothing })
                    , (3, TyVar { tnId = tgtCodId, tnBound = Nothing })
                    , (4, TyArrow tgtArrowId tgtDomId tgtCodId)
                    , (5, TyForall tgtForallId tgtArrowId)
                    , (6, TyExp expNodeId (ExpVarId 0) srcForallId)
                    ]

                edge = InstEdge (EdgeId 0) expNodeId tgtForallId

                -- Make the target forall bind both variables directly (flex) so
                -- orderedBinders sees arity 2.
                bindParents0 = inferBindParents nodes
                bindParents =
                    IntMap.insert (getNodeId tgtDomId) (tgtForallId, BindFlex) $
                        IntMap.insert (getNodeId tgtCodId) (tgtForallId, BindFlex) bindParents0

                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps } -> do
                    case IntMap.lookup 0 exps of
                        Just (ExpCompose (ExpInstantiate args NE.:| rest)) -> do
                            length args `shouldBe` 1
                            rest `shouldBe` [ExpForall (ForallSpec 2 [Nothing, Nothing] NE.:| [])]
                        Just other -> expectationFailure $ "Expected composed instantiate+forall, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for Edge 0"

        it "keeps identity when forall arity matches and requests body unification" $ do
            let srcVarId = NodeId 0
                srcForallId = NodeId 1
                tgtVarId = NodeId 2
                tgtForallId = NodeId 3
                expNodeId = NodeId 4
                rootId = NodeId 5
                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = srcVarId, tnBound = Nothing })
                        , (1, TyForall srcForallId srcVarId)
                        , (2, TyVar { tnId = tgtVarId, tnBound = Nothing })
                        , (3, TyForall tgtForallId tgtVarId)
                        , (4, TyExp expNodeId (ExpVarId 0) srcForallId)
                        , (5, TyArrow rootId expNodeId tgtForallId)
                        ]
                edge = InstEdge (EdgeId 0) expNodeId tgtForallId
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps, prConstraint = c' } -> do
                    case IntMap.lookup 0 exps of
                        Just ExpIdentity -> pure ()
                        other -> expectationFailure $ "Expected ExpIdentity, got " ++ show other

                    let nodes' = cNodes c'
                    case (IntMap.lookup (getNodeId srcVarId) nodes', IntMap.lookup (getNodeId tgtVarId) nodes') of
                        (Just _, Nothing) -> pure ()
                        (Nothing, Just _) -> pure ()
                        (Just _, Just _) ->
                            expectationFailure "Nodes 0 and 2 should have been merged but both exist in cNodes"
                        (Nothing, Nothing) -> expectationFailure "Both nodes missing?"

        it "rejects expansions that would point a binder back into its own body" $ do
            -- Edge: s · (∀a. a) ≤ ∀b. (b → s · (∀a. a))
            --
            -- Minimal expansion would keep identity and request a unification between the
            -- source binder `a` and the target body. That unification would make `a`
            -- reachable from itself (via the nested occurrence of s · (∀a. a)), so
            -- presolution must reject it via occurs-check.
            let boundVarId = NodeId 0
                srcForallId = NodeId 1
                srcExpId = NodeId 2
                tgtBinderId = NodeId 3
                tgtBodyId = NodeId 4
                tgtForallId = NodeId 5

                nodes = IntMap.fromList
                    [ (getNodeId boundVarId, TyVar { tnId = boundVarId, tnBound = Nothing })
                    , (getNodeId srcForallId, TyForall srcForallId boundVarId)
                    , (getNodeId srcExpId, TyExp srcExpId (ExpVarId 0) srcForallId)
                    , (getNodeId tgtBinderId, TyVar { tnId = tgtBinderId, tnBound = Nothing })
                    , (getNodeId tgtBodyId, TyArrow tgtBodyId tgtBinderId srcExpId)
                    , (getNodeId tgtForallId, TyForall tgtForallId tgtBodyId)
                    ]

                edge = InstEdge (EdgeId 0) srcExpId tgtForallId
                bindParents =
                    IntMap.insert
                        (getNodeId tgtBinderId)
                        (tgtForallId, BindFlex)
                        (inferBindParents nodes)
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution acyclicityRes constraint of
                Left OccursCheckPresolution{} -> return ()
                Left other -> expectationFailure $ "Unexpected error: " ++ show other
                Right _ -> expectationFailure "Expected presolution occurs-check failure"

        it "returns ExpForall for structure <= forall" $ do
            let srcDomId = NodeId 0
                srcCodId = NodeId 1
                srcArrowId = NodeId 2
                tgtDomId = NodeId 3
                tgtCodId = NodeId 4
                tgtArrowId = NodeId 5
                tgtForallId = NodeId 6
                expNodeId = NodeId 7
                rootId = NodeId 8
                nodes =
                    IntMap.fromList
                        [ (0, TyBase srcDomId (BaseTy "int"))
                        , (1, TyBase srcCodId (BaseTy "int"))
                        , (2, TyArrow srcArrowId srcDomId srcCodId)
                        , (3, TyBase tgtDomId (BaseTy "int"))
                        , (4, TyBase tgtCodId (BaseTy "int"))
                        , (5, TyArrow tgtArrowId tgtDomId tgtCodId)
                        , (6, TyForall tgtForallId tgtArrowId)
                        , (7, TyExp expNodeId (ExpVarId 0) srcArrowId)
                        , (8, TyArrow rootId expNodeId tgtForallId)
                        ]
                edge = InstEdge (EdgeId 0) expNodeId tgtForallId
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps } ->
                    case IntMap.lookup 0 exps of
                        Just (ExpForall (s NE.:| [])) -> s `shouldBe` ForallSpec 0 []
                        Just other -> expectationFailure $ "Expected ExpForall, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for Edge 0"

        it "does not require target foralls to have a binding parent" $ do
            -- Paper-faithful scope tracking uses binding edges. A forall node may be a
            -- binding root (no parent) and presolution should still succeed.
            let srcVarId = NodeId 0
                srcForallId = NodeId 1
                tgtVarId = NodeId 2
                tgtForallId = NodeId 3
                expNodeId = NodeId 4

                nodes = IntMap.fromList
                    [ (0, TyVar { tnId = srcVarId, tnBound = Nothing })
                    , (1, TyForall srcForallId srcVarId)
                    , (2, TyVar { tnId = tgtVarId, tnBound = Nothing })
                    , (3, TyForall tgtForallId tgtVarId)
                    , (4, TyExp expNodeId (ExpVarId 0) srcForallId)
                    ]

                edge = InstEdge (EdgeId 0) expNodeId tgtForallId
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution acyclicityRes constraint of
                Left other -> expectationFailure $ "Unexpected error: " ++ show other
                Right _ -> pure ()

    describe "Error Conditions" $ do
        it "reports ArityMismatch when merging ExpInstantiate with different lengths" $ do
            let st0 = PresolutionState emptyConstraint (Presolution IntMap.empty) IntMap.empty 0 IntSet.empty IntMap.empty IntMap.empty IntMap.empty
                exp1 = ExpInstantiate [NodeId 1]
                exp2 = ExpInstantiate [NodeId 1, NodeId 2]

            -- mergeExpansions is internal, but we can access it via a helper or by constructing
            -- a scenario where processInstEdge hits this case.
            -- Using runPresolutionM to call mergeExpansions directly is cleaner.

            case runPresolutionM st0 (mergeExpansions (ExpVarId 0) exp1 exp2) of
                Left (ArityMismatch ctx expected actual) -> do
                    ctx `shouldBe` "ExpInstantiate merge"
                    expected `shouldBe` 1
                    actual `shouldBe` 2
                Left err -> expectationFailure $ "Expected ArityMismatch, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "reports InstantiateOnNonForall when applying ExpInstantiate to non-forall body" $ do
            let expNodeId = NodeId 0
                bodyId = NodeId 1
                -- Body is a base type, not a forall
                nodes = IntMap.fromList
                    [ (0, TyExp expNodeId (ExpVarId 0) bodyId)
                    , (1, TyBase bodyId (BaseTy "int"))
                    ]
                constraint = emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 2 IntSet.empty IntMap.empty IntMap.empty IntMap.empty
                expansion = ExpInstantiate [NodeId 2] -- dummy arg

            case runPresolutionM st0 (applyExpansion expansion (nodes IntMap.! 0)) of
                Left (InstantiateOnNonForall nid) -> nid `shouldBe` bodyId
                Left err -> expectationFailure $ "Expected InstantiateOnNonForall, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "reports ArityMismatch when applying ExpInstantiate with wrong argument count" $ do
            let expNodeId = NodeId 0
                forallId = NodeId 1
                boundId = NodeId 2
                nodes = IntMap.fromList
                    [ (0, TyExp expNodeId (ExpVarId 0) forallId)
                    , (1, TyForall forallId boundId)
                    , (2, TyVar { tnId = boundId, tnBound = Nothing })
                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 3 IntSet.empty IntMap.empty IntMap.empty IntMap.empty
                -- Forall has 1 bound var, but we provide 2 args
                expansion = ExpInstantiate [NodeId 3, NodeId 4]

            case runPresolutionM st0 (applyExpansion expansion (nodes IntMap.! 0)) of
                Left (ArityMismatch ctx expected actual) -> do
                    ctx `shouldBe` "applyExpansion"
                    expected `shouldBe` 1
                    actual `shouldBe` 2
                Left err -> expectationFailure $ "Expected ArityMismatch, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "handles nested ExpCompose correctly in applyExpansionOverNode" $ do
            -- Create a scenario where ExpCompose is applied to a node
            -- e.g. (inst . forall) applied to a structure
            let nid = NodeId 0
                nodes = IntMap.fromList [(0, TyBase nid (BaseTy "int"))]
                constraint = emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 1 IntSet.empty IntMap.empty IntMap.empty IntMap.empty

                -- Construct an expansion: ExpCompose [ExpForall [1], ExpIdentity]
                -- This will trigger the ExpCompose branch in applyExpansionOverNode
                expansion = ExpCompose (ExpForall (ForallSpec 0 [] NE.:| []) NE.:| [ExpIdentity])

            case runPresolutionM st0 (applyExpansion expansion (nodes IntMap.! 0)) of
                Right (resId, _) -> do
                    -- Should wrap in Forall and then identity
                    -- TyBase -> TyForall(TyBase)
                    resId `shouldNotBe` nid
                Left err -> expectationFailure $ "Expansion failed: " ++ show err

        it "materializes ExpForall by rebinding binders + bounds" $ do
            let domVarId = NodeId 0
                codVarId = NodeId 1
                arrowId = NodeId 2
                expNodeId = NodeId 3
                bndId = NodeId 4

                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = domVarId, tnBound = Nothing })
                        , (1, TyVar { tnId = codVarId, tnBound = Nothing })
                        , (2, TyArrow arrowId domVarId codVarId)
                        , (3, TyExp expNodeId (ExpVarId 0) arrowId)
                        , (4, TyBase bndId (BaseTy "int"))
                        ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        5
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

                forallSpec =
                    ForallSpec
                        { fsBinderCount = 2
                        , fsBounds =
                            [ Just (BoundBinder 1)
                            , Just (BoundNode bndId)
                            ]
                        }
                expansion = ExpForall (forallSpec NE.:| [])

            case runPresolutionM st0 (applyExpansion expansion (nodes IntMap.! getNodeId expNodeId)) of
                Left err -> expectationFailure $ "Expansion failed: " ++ show err
                Right (forallId, st1) -> do
                    let c1 = psConstraint st1
                        nodes1 = cNodes c1
                        bp1 = cBindParents c1
                        boundOf nid =
                            case IntMap.lookup (getNodeId nid) nodes1 of
                                Just TyVar{ tnBound = mb } -> mb
                                _ -> Nothing

                    forallNode <- expectForall nodes1 forallId
                    tnBody forallNode `shouldBe` arrowId

                    IntMap.lookup (getNodeId arrowId) bp1 `shouldBe` Just (forallId, BindFlex)
                    IntMap.lookup (getNodeId domVarId) bp1 `shouldBe` Just (forallId, BindFlex)
                    IntMap.lookup (getNodeId codVarId) bp1 `shouldBe` Just (forallId, BindFlex)

                    boundOf domVarId `shouldBe` Just codVarId
                    boundOf codVarId `shouldBe` Just bndId

                    case Binding.orderedBinders id c1 forallId of
                        Left err -> expectationFailure $ "orderedBinders failed: " ++ show err
                        Right bs -> bs `shouldBe` [codVarId, domVarId]
        it "handles multiple edges correctly" $ do
            let varId = NodeId 0
                forallId = NodeId 1
                target1Id = NodeId 2 -- int
                target2Id = NodeId 3 -- bool
                exp1Id = NodeId 4 -- s1 . sigma
                exp2Id = NodeId 5 -- s2 . sigma
                rootEdge1 = NodeId 6
                rootEdge2 = NodeId 7
                rootId = NodeId 8
                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = varId, tnBound = Nothing })
                        , (1, TyForall forallId varId)
                        , (2, TyBase target1Id (BaseTy "int"))
                        , (3, TyBase target2Id (BaseTy "bool"))
                        , (4, TyExp exp1Id (ExpVarId 1) forallId)
                        , (5, TyExp exp2Id (ExpVarId 2) forallId)
                        , (6, TyArrow rootEdge1 exp1Id target1Id)
                        , (7, TyArrow rootEdge2 exp2Id target2Id)
                        , (8, TyArrow rootId rootEdge1 rootEdge2)
                        ]
                edge1 = InstEdge (EdgeId 0) exp1Id target1Id
                edge2 = InstEdge (EdgeId 1) exp2Id target2Id
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge1, edge2]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge1, edge2]
                        , arDepGraph = undefined
                        }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps } -> do
                    case (IntMap.lookup 0 exps, IntMap.lookup 1 exps) of
                        (Just (ExpInstantiate [n1]), Just (ExpInstantiate [n2])) ->
                            n1 `shouldNotBe` n2
                        _ -> expectationFailure "Expected two separate instantiations"

        it "merges instantiations when the same ExpVar appears in multiple edges" $ do
            let boundId = NodeId 0
                forallId = NodeId 1
                expNodeId = NodeId 2
                targetId = NodeId 3
                rootId = NodeId 4
                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = boundId, tnBound = Nothing })
                        , (1, TyForall forallId boundId)
                        , (2, TyExp expNodeId (ExpVarId 0) forallId)
                        , (3, TyBase targetId (BaseTy "int"))
                        , (4, TyArrow rootId expNodeId targetId)
                        ]
                edge1 = InstEdge (EdgeId 0) expNodeId targetId
                edge2 = InstEdge (EdgeId 1) expNodeId targetId
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge1, edge2]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge1, edge2], arDepGraph = undefined }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps, prConstraint = c' } -> do
                    case (IntMap.lookup 0 exps, IntMap.lookup 1 exps) of
                        (Just (ExpInstantiate [n1]), Just (ExpInstantiate [n2])) -> do
                            n1 `shouldBe` n2
                            IntMap.member (getNodeId n1) (cNodes c') `shouldBe` True
                        _ -> expectationFailure "Expected merged ExpInstantiate"

        it "materializes expansions and clears inst edges for strict solve" $ do
            let bound = NodeId 0
                forallId = NodeId 1
                expId = NodeId 2
                targetId = NodeId 3
                rootId = NodeId 4
                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = bound, tnBound = Nothing })
                        , (1, TyForall forallId bound)
                        , (2, TyExp expId (ExpVarId 0) forallId)
                        , (3, TyBase targetId (BaseTy "int"))
                        , (4, TyArrow rootId expId targetId)
                        ]
                edge = InstEdge (EdgeId 0) expId targetId
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

                isExp TyExp{} = True
                isExp _ = False

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prConstraint = c' } -> do
                    any isExp (IntMap.elems (cNodes c')) `shouldBe` False
                    cInstEdges c' `shouldBe` []
                    validateSolvedGraphStrict SolveResult{ srConstraint = c', srUnionFind = IntMap.empty }
                        `shouldBe` []

    describe "Phase 4 — OpRaise for interior nodes" $ do
        it "returns a non-empty OpRaise trace when harmonization raises" $ do
            let binder = NodeId 3
                n = NodeId 1
                m = NodeId 4
                rootArrow = NodeId 5

                nodes =
                    IntMap.fromList
                        [ (getNodeId n, TyVar { tnId = n, tnBound = Nothing })
                        , (getNodeId m, TyVar { tnId = m, tnBound = Nothing })
                        , (getNodeId binder, TyForall binder n)
                        , (getNodeId rootArrow, TyArrow rootArrow binder m)
                        ]

                bindParents =
                    IntMap.fromList
                        [ (getNodeId binder, (rootArrow, BindFlex))
                        , (getNodeId n, (binder, BindFlex))
                        , (getNodeId m, (rootArrow, BindFlex))
                        ]

                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        }

                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        6
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM st0 (unifyAcyclicRawWithRaiseTrace n m) of
                Left err ->
                    expectationFailure ("unifyAcyclicRawWithRaiseTrace failed: " ++ show err)
                Right (trace, st1) -> do
                    trace `shouldBe` [n]
                    let uf = psUnionFind st1
                        nC = UF.frWith uf n
                    Binding.lookupBindParent (psConstraint st1) nC `shouldBe` Just (rootArrow, BindFlex)

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

                nodes =
                    IntMap.fromList
                        [ (getNodeId a, TyVar { tnId = a, tnBound = Nothing })
                        -- b is a term-dag root (unbound) but is unioned into a's class.
                        , (getNodeId b, TyVar { tnId = b, tnBound = Nothing })
                        , (getNodeId c, TyVar { tnId = c, tnBound = Nothing })
                        , (getNodeId forallNode, TyForall forallNode a)
                        , (getNodeId rootArrow, TyArrow rootArrow forallNode c)
                        ]

                bindParents =
                    IntMap.fromList
                        [ (getNodeId forallNode, (rootArrow, BindFlex))
                        , (getNodeId a, (forallNode, BindFlex))
                        , (getNodeId c, (rootArrow, BindFlex))
                        ]

                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        }

                -- Union b into a's class (b ↦ a); b remains a binding-root node.
                uf = IntMap.fromList [(getNodeId b, a)]

                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        uf
                        6
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

                interior = IntSet.fromList [getNodeId a, getNodeId b]

            case runPresolutionM st0 (runEdgeUnifyForTest rootArrow interior a c) of
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

                nodes =
                    IntMap.fromList
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
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        7
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM st0 (processInstEdge edge) of
                Left (BindingTreeError (MissingBindParent nid)) -> nid `shouldBe` b
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

                nodes =
                    IntMap.fromList
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
                bindParents = IntMap.fromList
                    [ (getNodeId b, (forallNode, BindFlex))
                    , (getNodeId arrow1, (forallNode, BindFlex))
                    , (getNodeId forallNode, (expNode, BindFlex))
                    , (getNodeId y, (targetArrow, BindFlex))
                    ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        7
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM st0 (processInstEdge edge) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    -- Verify the edge trace contains the interior nodes
                    case IntMap.lookup 0 (psEdgeTraces st1) of
                        Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0"
                        Just tr -> do
                            -- The interior should be non-empty
                            etInterior tr `shouldSatisfy` (not . IntSet.null)
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

                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = a, tnBound = Nothing })
                        , (1, TyBase intNode (BaseTy "Int"))
                        , (2, TyArrow innerArrow a intNode)
                        , (3, TyForall forallNode innerArrow)
                        , (4, TyExp expNode (ExpVarId 0) forallNode)
                        , (5, TyVar { tnId = y, tnBound = Nothing })
                        , (6, TyArrow targetArrow y intNode)
                        ]

                -- Mark innerArrow as rigidly bound (locked)
                bindParents = IntMap.fromList
                    [ (getNodeId a, (forallNode, BindFlex))
                    , (getNodeId intNode, (innerArrow, BindFlex))
                    , (getNodeId innerArrow, (forallNode, BindRigid))  -- Rigid!
                    , (getNodeId forallNode, (expNode, BindFlex))
                    , (getNodeId y, (targetArrow, BindFlex))
                    ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        7
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

                -- Check that no OpRaise targets the rigidly bound innerArrow
                isRaiseOnRigid op = case op of
                    OpRaise n -> n == innerArrow
                    _ -> False

            case runPresolutionM st0 (processInstEdge edge) of
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

                nodes =
                    IntMap.fromList
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
                    IntMap.fromList
                        [ (getNodeId expNode, (rootArrow, BindFlex))
                        , (getNodeId forallNode, (expNode, BindFlex))
                        , (getNodeId outerArrow, (forallNode, BindFlex))
                        , (getNodeId innerArrow, (outerArrow, BindFlex))
                        , (getNodeId bv, (forallNode, BindFlex))
                        , (getNodeId targetOuterArrow, (rootArrow, BindFlex))
                        -- Bind the target's inner arrow directly to the root to force a raise of
                        -- the copied inner arrow during unification.
                        , (getNodeId targetInnerArrow, (rootArrow, BindFlex))
                        , (getNodeId y, (targetOuterArrow, BindFlex))
                        ]

                edge = InstEdge (EdgeId 0) expNode targetOuterArrow
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }

                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        10
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

                isRaiseOn nid op = case op of
                    OpRaise n -> n == nid
                    _ -> False

            case runPresolutionM st0 (processInstEdge edge) of
                Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
                Right (_, st1) -> do
                    tr <- case IntMap.lookup 0 (psEdgeTraces st1) of
                        Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0" >> fail "missing trace"
                        Just t -> pure t
                    copiedInner <- case IntMap.lookup (getNodeId innerArrow) (etCopyMap tr) of
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

                nodes =
                    IntMap.fromList
                        [ (getNodeId a, TyVar { tnId = a, tnBound = Nothing })
                        , (getNodeId b, TyVar { tnId = b, tnBound = Nothing })
                        , (getNodeId p1, TyForall p1 a)
                        , (getNodeId p2, TyForall p2 b)
                        , (getNodeId r, TyArrow r p1 p2)
                        ]

                bindParents =
                    IntMap.fromList
                        [ (getNodeId p1, (r, BindFlex))
                        , (getNodeId a, (p1, BindFlex))
                        , (getNodeId p2, (r, BindFlex))
                        , (getNodeId b, (p2, BindFlex))
                        ]

                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        }

                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        5
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

                interior = IntSet.fromList [getNodeId a]

            -- Sanity: both sides really do get raised by harmonization.
            case runPresolutionM st0 (unifyAcyclicRawWithRaiseTrace a b) of
                Left err -> expectationFailure ("unifyAcyclicRawWithRaiseTrace failed: " ++ show err)
                Right (trace, _st1) ->
                    trace `shouldBe` [a, b]

            case runPresolutionM st0 (runEdgeUnifyForTest r interior a b) of
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

                nodes =
                    IntMap.fromList
                        [ (0, TyVar { tnId = a, tnBound = Nothing })
                        , (1, TyBase intNode (BaseTy "Int"))
                        , (2, TyArrow arrow a intNode)
                        , (3, TyForall forallNode arrow)
                        , (4, TyExp expNode (ExpVarId 0) forallNode)
                        , (5, TyVar { tnId = y, tnBound = Nothing })
                        , (6, TyArrow targetArrow y intNode)
                        ]

                bindParents = IntMap.fromList
                    [ (getNodeId a, (forallNode, BindFlex))
                    , (getNodeId intNode, (arrow, BindFlex))
                    , (getNodeId arrow, (forallNode, BindFlex))
                    , (getNodeId forallNode, (expNode, BindFlex))
                    , (getNodeId y, (targetArrow, BindFlex))
                    ]

                edge = InstEdge (EdgeId 0) expNode targetArrow
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        7
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty

            case runPresolutionM st0 (processInstEdge edge) of
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

                        nodes =
                            IntMap.fromList $
                                [ (getNodeId rootArrow, TyArrow rootArrow (NodeId leftStart) (NodeId rightStart))
                                ]
                                    ++ leftForalls
                                    ++ rightForalls
                                    ++ [ (leftVarId, TyVar { tnId = NodeId leftVarId, tnBound = Nothing })
                                       , (rightVarId, TyVar { tnId = NodeId rightVarId, tnBound = Nothing })
                                       ]

                        bindParents =
                            IntMap.fromList $
                                -- bind the outermost foralls to the arrow root
                                [ (leftStart, (rootArrow, BindFlex))
                                , (rightStart, (rootArrow, BindFlex))
                                ]
                                    ++
                                    -- chain the inner foralls
                                    [ (nid, (NodeId (nid - 1), BindFlex))
                                    | nid <- [leftStart + 1 .. leftStart + leftDepth - 1]
                                    ]
                                    ++ [ (nid, (NodeId (nid - 1), BindFlex))
                                       | nid <- [rightStart + 1 .. rightStart + rightDepth - 1]
                                       ]
                                    ++
                                    -- bind leaf vars to their innermost foralls
                                    [ (leftVarId, (NodeId (leftStart + leftDepth - 1), BindFlex))
                                    , (rightVarId, (NodeId (rightStart + rightDepth - 1), BindFlex))
                                    ]

                        constraint0 =
                            emptyConstraint
                                { cNodes = nodes
                                , cBindParents = bindParents
                                }

                        st0 =
                            PresolutionState
                                constraint0
                                (Presolution IntMap.empty)
                                IntMap.empty
                                (rightVarId + 1)
                                IntSet.empty
                                IntMap.empty
                                IntMap.empty
                                IntMap.empty

                        interior = IntSet.fromList [0 .. rightVarId]

                        replayRaises :: Constraint -> [InstanceOp] -> Either BindingError Constraint
                        replayRaises c ops0 = go c ops0
                          where
                            go c' [] = Right c'
                            go c' (OpRaise nid : rest) = do
                                (c'', _mOp) <- GraphOps.applyRaiseStep nid c'
                                go c'' rest
                            go c' (_ : rest) = go c' rest

                        leftVar = NodeId leftVarId
                        rightVar = NodeId rightVarId

                    case runPresolutionM st0 (runEdgeUnifyForTest rootArrow interior leftVar rightVar) of
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
