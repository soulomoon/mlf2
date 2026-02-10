module Presolution.MergeEmissionSpec (spec) where

import Test.Hspec
import Data.List (findIndex)
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
    , PresolutionResult(..)
    , PresolutionState(..)
    , computePresolution
    , lookupCopy
    , processInstEdge
    , runPresolutionM
    )
import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import MLF.Constraint.Presolution.Witness (OmegaNormalizeError(..))
import qualified MLF.Binding.Tree as Binding
import SpecUtil
    ( bindParentsFromPairs
    , defaultTraceConfig
    , emptyConstraint
    , inferBindParents
    , nodeMapFromList
    , rootedConstraint
    )

spec :: Spec
spec = describe "Phase 2 — Merge/RaiseMerge emission" $ do
    it "R-MERGE-VALID-07: records Merge when two instantiation metas unify" $ do
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

            nodes = nodeMapFromList
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
                    (bindParentsFromPairs
                        [ (a, forallNode, BindFlex)
                        , (b, forallNode, BindFlex)
                        ])
                    (inferBindParents nodes)
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents = bindParents
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    9
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
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
                                meta <- case lookupCopy bv cmap of
                                    Nothing -> expectationFailure ("Expected binder-meta in EdgeTrace.etCopyMap for " ++ show bv) >> fail "missing binder-meta"
                                    Just m -> pure m
                                IntSet.member (getNodeId meta) (cEliminatedVars c1) `shouldBe` True
                            )
                            eliminatedBinders

    it "R-MERGE-NORM-09: chooses Merge direction by ≺ (m ≺ n) rather than NodeId" $ do
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

            nodes = nodeMapFromList
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
                    (bindParentsFromPairs
                        [ (a, forallNode, BindFlex)
                        , (b, forallNode, BindFlex)
                        ])
                    (inferBindParents nodes)
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents = bindParents
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    15
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            wants op = op == OpMerge b a

        case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
            Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
            Right (_, st1) -> do
                case IntMap.lookup 0 (psEdgeWitnesses st1) of
                    Nothing -> expectationFailure "Expected EdgeWitness for EdgeId 0"
                    Just ew -> do
                        let InstanceWitness ops = ewWitness ew
                        ops `shouldSatisfy` any wants

    it "R-RAISE-INVALID-11: fails normalization when escaped bounded-binder Raise is not transitively flex-bound" $ do
        -- TyExp s · (∀(b ⩾ x). b -> b) ≤ (y -> y)
        --
        -- Here `b` is bounded (non-⊥ bound `x`). During edge solving, the
        -- instantiated binder-meta can escape toward outer `y`, which can emit
        -- an `OpRaise` that is not transitively flex-bound to expansion root `r`.
        -- Under the stricter Fig. 15.3.4 translatability guard, normalization must
        -- fail fast with a witness-normalization error.
        let x = NodeId 0
            b = NodeId 1
            arrow1 = NodeId 2
            forallNode = NodeId 3
            expNode = NodeId 4
            y = NodeId 5
            targetArrow = NodeId 6
            rootArrow = NodeId 7

            nodes = nodeMapFromList
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
                    (nodeRefKey (typeRef b))
                    (typeRef forallNode, BindFlex)
                    (inferBindParents nodes)
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents = bindParents
                    }
            acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

        case computePresolution defaultTraceConfig acyclicityRes constraint of
            Left (WitnessNormalizationError (EdgeId eid) normErr) -> do
                eid `shouldBe` 0
                case normErr of
                    NotTransitivelyFlexBound (OpRaise _) _ _ -> pure ()
                    _ -> expectationFailure ("Expected NotTransitivelyFlexBound OpRaise, got: " ++ show normErr)
            Left other ->
                expectationFailure ("Expected WitnessNormalizationError, got: " ++ show other)
            Right _ ->
                expectationFailure "Expected computePresolution to fail with NotTransitivelyFlexBound"

    it "R-RAISEMERGE-VALID-13: records RaiseMerge for a live binder when a lower-≺ binder in bs was already eliminated" $ do
        -- TyExp s · (∀(a ⩾ b) (b ⩾ x). a -> b) ≤ (y -> y)
        --
        -- Base ω execution performs Merge(a, b), eliminating `a` before structural
        -- unification. Later, the aliased class {a,b} escapes to outer `y`.
        -- Phase 2 must still record RaiseMerge using the live binder `b`.
        let x = NodeId 0
            a = NodeId 1
            b = NodeId 2
            bodyArrow = NodeId 3
            forallNode = NodeId 4
            expNode = NodeId 5
            y = NodeId 6
            targetArrow = NodeId 7
            rootArrow = NodeId 8

            nodes = nodeMapFromList
                    [ (getNodeId x, TyVar { tnId = x, tnBound = Nothing })
                    , (getNodeId a, TyVar { tnId = a, tnBound = Just b })
                    , (getNodeId b, TyVar { tnId = b, tnBound = Just x })
                    , (getNodeId bodyArrow, TyArrow bodyArrow a b)
                    , (getNodeId forallNode, TyForall forallNode bodyArrow)
                    , (getNodeId expNode, TyExp expNode (ExpVarId 0) forallNode)
                    , (getNodeId y, TyVar { tnId = y, tnBound = Nothing })
                    , (getNodeId targetArrow, TyArrow targetArrow y y)
                    , (getNodeId rootArrow, TyArrow rootArrow expNode targetArrow)
                    ]

            edge = InstEdge (EdgeId 0) expNode targetArrow
            bindParents =
                IntMap.insert
                    (nodeRefKey (typeRef a))
                    (typeRef forallNode, BindFlex)
                    (IntMap.insert
                        (nodeRefKey (typeRef b))
                        (typeRef forallNode, BindFlex)
                        (inferBindParents nodes))
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents = bindParents
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    9
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
            Left err -> expectationFailure ("processInstEdge failed: " ++ show err)
            Right (_, st1) -> do
                ew <- case IntMap.lookup 0 (psEdgeWitnesses st1) of
                    Nothing -> expectationFailure "Expected EdgeWitness for EdgeId 0" >> fail "missing EdgeWitness"
                    Just ew0 -> pure ew0

                let InstanceWitness ops = ewWitness ew

                -- Base op proved `a` was eliminated before the structural escape.
                ops `shouldSatisfy` elem (OpMerge a b)
                -- Regression: RaiseMerge must still be emitted on live binder `b`.
                ops `shouldSatisfy` elem (OpRaise b)
                ops `shouldSatisfy` elem (OpMerge b y)
                ops `shouldNotSatisfy` elem (OpMerge a y)

    it "R-WEAKEN-VALID-04: does not record Raise for unbounded binder metas (graft+weaken only)" $ do
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

            nodes = nodeMapFromList
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
                    (nodeRefKey (typeRef b))
                    (typeRef forallNode, BindFlex)
                    (inferBindParents nodes)
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
            isGraftToBinder op = case op of
                OpGraft _ n -> n == b
                _ -> False
            isRaise op = case op of
                OpRaise n -> n == b
                _ -> False
            isRaiseMerge op = case op of
                OpRaiseMerge n m -> n == b && m == y
                _ -> False

        case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
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

            nodes = nodeMapFromList
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
                bindParentsFromPairs
                    [ (b, forallNode, BindFlex)
                    , (arrow1, forallNode, BindFlex)
                    , (forallNode, expNode, BindFlex)
                    , (y, targetArrow, BindFlex)
                    ]

            edge = InstEdge (EdgeId 0) expNode targetArrow
            acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    , cBindParents = bindParents
                    }

        case computePresolution defaultTraceConfig acyclicityRes constraint of
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

                meta <- case lookupCopy bv cmap of
                    Nothing -> expectationFailure "Expected binder-meta in EdgeTrace.etCopyMap" >> fail "missing binder-meta"
                    Just m -> pure m

                let c1 = prConstraint pr

                -- 1) Flag flip: the binder-meta's binding edge becomes rigid.
                case Binding.lookupBindParent c1 (typeRef meta) of
                    Nothing -> expectationFailure "Expected binder-meta to have a binding parent after expansion"
                    Just (_p, flag) -> flag `shouldBe` BindRigid

                -- 2) No UF merge: meta stays distinct from the instantiation argument.
                meta `shouldNotBe` arg

    it "R-GRAFT-VALID-01: orders base witness ops as Graft; Merge; Weaken for bounded binders" $ do
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

            nodes = nodeMapFromList
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
                    (bindParentsFromPairs
                        [ (a, forallNode, BindFlex)
                        , (b, forallNode, BindFlex)
                        ])
                    (inferBindParents nodes)
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
            isGraftToA op = case op of
                OpGraft _ n -> n == a
                _ -> False
            isMergeBA op = case op of
                OpMerge n m -> n == b && m == a
                _ -> False
            isWeakenA op = case op of
                OpWeaken n -> n == a
                _ -> False

        case runPresolutionM defaultTraceConfig st0 (processInstEdge edge) of
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
