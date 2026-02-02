module Presolution.EdgeTraceSpec (spec) where

import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Types.Graph
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionResult(..)
    , PresolutionState(..)
    , computePresolution
    , fromListInterior
    , lookupCopy
    , processInstEdge
    , runPresolutionM
    )
import MLF.Constraint.Acyclicity (AcyclicityResult(..))
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
spec = describe "EdgeTrace" $ do
    it "records nodes allocated while solving an instantiation edge" $ do
        -- TyExp s · (∀@1. a -> a) ≤ (Int -> Int)
        let a = NodeId 0
            arrow = NodeId 1
            forallNode = NodeId 2
            expNode = NodeId 3
            intNode = NodeId 4
            targetArrow = NodeId 5

            nodes = nodeMapFromList
                    [ (0, TyVar { tnId = a, tnBound = Nothing })
                    , (1, TyArrow arrow a a)
                    , (2, TyForall forallNode arrow)
                    , (3, TyExp expNode (ExpVarId 0) forallNode)
                    , (4, TyBase intNode (BaseTy "Int"))
                    , (5, TyArrow targetArrow intNode intNode)
                    ]

            edge = InstEdge (EdgeId 0) expNode targetArrow
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents =
                        bindParentsFromPairs
                            [ (a, forallNode, BindFlex)
                            , (arrow, forallNode, BindFlex)
                            , (forallNode, expNode, BindFlex)
                            , (intNode, targetArrow, BindFlex)
                            ]
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
        case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
            Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
            Right (_, st1) -> do
                let traces = psEdgeTraces st1
                case IntMap.lookup 0 traces of
                    Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0"
                    Just tr -> do
                        etRoot tr `shouldBe` UF.frWith (psUnionFind st1) expNode
                        etInterior tr `shouldSatisfy` (/= mempty)
                        case etBinderArgs tr of
                            [(bv, _arg)] -> do
                                bv `shouldBe` a
                                case lookupCopy bv (etCopyMap tr) of
                                    Nothing -> expectationFailure "Expected binder meta in etCopyMap"
                                    Just _meta -> pure ()
                            other -> expectationFailure ("Unexpected binder/arg pairs: " ++ show other)

    it "tracks binder-argument nodes across merged expansions" $ do
        -- When an expansion variable is reused across multiple instantiation edges, the
        -- *final* expansion may keep the argument nodes allocated by an earlier edge
        -- (mergeExpansions keeps the first ExpInstantiate payload).
        --
        -- Traces should still record binder metas in etCopyMap even when
        -- expansion arguments are reused across edges; etInterior itself is
        -- now the exact I(r) and may exclude merged nodes.
        let a = NodeId 0
            arrow = NodeId 1
            forallNode = NodeId 2
            expNode = NodeId 3
            intNode = NodeId 4
            target1 = NodeId 5
            boolNode = NodeId 6
            target2 = NodeId 7

            nodes = nodeMapFromList
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
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge0, edge1]
                    , cBindParents =
                        bindParentsFromPairs
                            [ (a, forallNode, BindFlex)
                            , (arrow, forallNode, BindFlex)
                            , (forallNode, expNode, BindFlex)
                            , (intNode, target1, BindFlex)
                            , (boolNode, target2, BindFlex)
                            ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    8
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (processInstEdge edge0 >> processInstEdge edge1) of
            Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
            Right (_, st1) -> do
                let traces = psEdgeTraces st1
                case (IntMap.lookup 0 traces, IntMap.lookup 1 traces) of
                    (Just tr0, Just tr1) -> do
                        -- sanity: first edge created its binder arg
                        case etBinderArgs tr0 of
                            [(bv0, _arg0)] ->
                                case lookupCopy bv0 (etCopyMap tr0) of
                                    Nothing -> expectationFailure "Expected binder meta in etCopyMap (edge0)"
                                    Just _meta0 -> pure ()
                            other -> expectationFailure ("Unexpected binder/arg pairs (edge0): " ++ show other)
                        -- expected: second edge should also include its binder arg in the trace interior
                        case etBinderArgs tr1 of
                            [(bv1, _arg1)] ->
                                case lookupCopy bv1 (etCopyMap tr1) of
                                    Nothing -> expectationFailure "Expected binder meta in etCopyMap (edge1)"
                                    Just _meta1 -> pure ()
                            other -> expectationFailure ("Unexpected binder/arg pairs (edge1): " ++ show other)
                    other -> expectationFailure ("Missing traces: " ++ show other)

    it "binds expansion root at the same binder as the edge target (paper §3.2)" $ do
        -- Paper alignment (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.2):
        -- "the root of the expansion is bound at the same binder as the target".
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

            nodes = nodeMapFromList
                    [ (0, TyVar { tnId = a, tnBound = Nothing })
                    , (1, TyArrow arrow a a)
                    , (2, TyForall forallNode arrow)
                    , (3, TyExp expNode (ExpVarId 0) forallNode)
                    , (4, TyBase intNode (BaseTy "Int"))
                    , (5, TyArrow targetArrow intNode intNode)
                    , (6, TyForall outerBinder targetArrow)  -- Outer binder
                    ]

            -- Set up binding edges: target arrow is bound by outerBinder
            bindParents =
                bindParentsFromPairs
                    [ (a, forallNode, BindFlex)
                    , (arrow, forallNode, BindFlex)
                    , (forallNode, expNode, BindFlex)
                    , (intNode, targetArrow, BindFlex)
                    , (targetArrow, outerBinder, BindFlex)
                    ]

            edge = InstEdge (EdgeId 0) expNode targetArrow
            constraint = rootedConstraint emptyConstraint
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
                        case lookupCopy arrow copyMap of
                            Nothing -> expectationFailure $
                                "Expected arrow to be copied. CopyMap: " ++ show copyMap
                            Just copiedArrow -> do
                                let uf = psUnionFind st1
                                    copiedArrowC = UF.frWith uf copiedArrow
                                    outerBinderC = UF.frWith uf outerBinder

                                -- The copied arrow (expansion result) should be bound
                                -- at the same binder as the target (up to UF).
                                case Binding.lookupBindParent c (typeRef copiedArrowC) of
                                    Nothing -> expectationFailure $
                                        "Expected expansion result " ++ show copiedArrowC ++
                                        " to have a binding parent. BindParents: " ++
                                        show (cBindParents c)
                                    Just (parentId, _flag) -> parentId `shouldBe` typeRef outerBinderC

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

            nodes = nodeMapFromList
                    [ (getNodeId a, TyVar { tnId = a, tnBound = Nothing })
                    , (getNodeId arrow, TyArrow arrow a a)
                    , (getNodeId forallNode, TyForall forallNode arrow)
                    , (getNodeId expNode, TyExp expNode (ExpVarId 0) forallNode)
                    , (getNodeId intNode, TyBase intNode (BaseTy "Int"))
                    , (getNodeId targetArrow, TyArrow targetArrow intNode intNode)
                    , (getNodeId rootArrow, TyArrow rootArrow expNode targetArrow)
                    ]

            bindParents =
                bindParentsFromPairs
                    [ (a, forallNode, BindFlex)
                    , (arrow, forallNode, BindFlex)
                    , (forallNode, rootArrow, BindFlex)
                    , (expNode, rootArrow, BindFlex)
                    , (intNode, targetArrow, BindFlex)
                    , (targetArrow, rootArrow, BindFlex)
                    ]

            edge = InstEdge (EdgeId 0) expNode targetArrow
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents = bindParents
                    }

            acyclicityRes =
                AcyclicityResult
                    { arSortedEdges = [edge]
                    , arDepGraph = undefined
                    }

        case computePresolution defaultTraceConfig acyclicityRes constraint of
            Left err -> expectationFailure ("Presolution failed: " ++ show err)
            Right PresolutionResult{ prConstraint = c', prEdgeTraces = traces } -> do
                tr <- case IntMap.lookup 0 traces of
                    Nothing -> expectationFailure "Expected EdgeTrace for EdgeId 0" >> fail "missing trace"
                    Just t -> pure t
                case Binding.interiorOf c' (typeRef (etRoot tr)) of
                    Left err -> expectationFailure ("Binding.interiorOf failed: " ++ show err)
                    Right interior -> do
                        let interiorNodes =
                                fromListInterior
                                    [ nid
                                    | key <- IntSet.toList interior
                                    , TypeRef nid <- [nodeRefFromKey key]
                                    ]
                        interiorNodes `shouldBe` etInterior tr
