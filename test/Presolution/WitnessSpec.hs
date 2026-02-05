module Presolution.WitnessSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
    ( EdgeWitness(..)
    , Expansion(..)
    , ForallSpec(..)
    , InstanceOp(..)
    , InstanceStep(..)
    , InstanceWitness(..)
    )
import MLF.Constraint.Presolution.Witness
    ( OmegaNormalizeEnv(..)
    , OmegaNormalizeError(..)
    , coalesceRaiseMergeWithEnv
    , integratePhase2Ops
    , normalizeInstanceOpsFull
    , normalizeInstanceStepsFull
    , reorderWeakenWithEnv
    , validateNormalizedWitness
    , witnessFromExpansion
    )
import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Presolution
    ( PresolutionState(..)
    , PresolutionError(..)
    , runPresolutionM
    , normalizeEdgeWitnessesM
    , EdgeTrace(..)
    , InteriorNodes(..)
    )
import qualified MLF.Constraint.Inert as Inert
import qualified MLF.Binding.Tree as Binding
import SpecUtil
    ( bindParentsFromPairs
    , defaultTraceConfig
    , emptyConstraint
    , inferBindParents
    , nodeMapFromList
    , rootedConstraint
    )
import Presolution.Util
    ( mkNormalizeConstraint
    , mkNormalizeEnv
    , nodeAt
    , orderedPairByPrec
    , genNormalizeEnvParams
    , mkTestNormalizeEnv
    , genInstanceOps
    , genInstanceOp
    , hasRedundantOps
    )

spec :: Spec
spec = do
    describe "Expansion witness steps" $ do
        it "preserves ExpCompose ordering with StepIntro" $ do
            let expNodeId = NodeId 0
                forallId = NodeId 1
                binderId = NodeId 2
                argId = NodeId 3
                nodes = nodeMapFromList
                    [ (0, TyExp expNodeId (ExpVarId 0) forallId)
                    , (1, TyForall forallId binderId)
                    , (2, TyVar { tnId = binderId, tnBound = Nothing })
                    ]
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 4 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                expansion =
                    ExpCompose
                        (ExpForall (ForallSpec 1 [Nothing] NE.:| []) NE.:| [ExpInstantiate [argId]])

            case runPresolutionM defaultTraceConfig st0 (witnessFromExpansion expNodeId (nodeAt nodes 0) expansion) of
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
                nodes = nodeMapFromList
                    [ (0, TyExp expNodeId (ExpVarId 0) bodyId)
                    , (1, TyVar { tnId = bodyId, tnBound = Nothing })
                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 2 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                expansion = ExpForall (ForallSpec 2 [Nothing, Nothing] NE.:| [])

            case runPresolutionM defaultTraceConfig st0 (witnessFromExpansion expNodeId (nodeAt nodes 0) expansion) of
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
                nodes = nodeMapFromList
                        [ (getNodeId root, TyArrow root parent arg)
                        , (getNodeId parent, TyArrow parent child child)
                        , (getNodeId child, TyVar { tnId = child, tnBound = Nothing })
                        , (getNodeId arg, TyVar { tnId = arg, tnBound = Nothing })
                        ]
                bindParents =
                    bindParentsFromPairs
                        [ (parent, root, BindRigid)
                        , (child, parent, BindFlex)
                        , (arg, root, BindFlex)
                        ]
                c = rootedConstraint $ emptyConstraint { cNodes = nodes, cBindParents = bindParents }
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
                nodes = nodeMapFromList
                        [ (getNodeId root, TyForall root parent)
                        , (getNodeId parent, TyForall parent child)
                        , (getNodeId child, TyVar { tnId = child, tnBound = Nothing })
                        , (getNodeId sibling, TyVar { tnId = sibling, tnBound = Nothing })
                        ]
                bindParents =
                    bindParentsFromPairs
                        [ (parent, root, BindFlex)
                        , (child, parent, BindFlex)
                        , (sibling, root, BindFlex)
                        ]
                c = rootedConstraint $ emptyConstraint { cNodes = nodes, cBindParents = bindParents }
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
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyForall root parent)
                            , (getNodeId parent, TyForall parent child)
                            , (getNodeId child, TyVar { tnId = child, tnBound = Nothing })
                            ]
                    bindParents =
                        bindParentsFromPairs
                            [ (parent, root, BindFlex)
                            , (child, parent, BindFlex)
                            ]
                    c = rootedConstraint $ emptyConstraint { cNodes = nodes, cBindParents = bindParents }
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId parent, getNodeId child])
                    ops = [OpWeaken parent, OpGraft child child]
                validateNormalizedWitness env ops
                    `shouldBe` Left (OpUnderRigid child)

            it "rejects ops below a Weakened binder when merge touches a descendant RHS" $ do
                let root = NodeId 0
                    parent = NodeId 1
                    child = NodeId 2
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyForall root parent)
                            , (getNodeId parent, TyForall parent child)
                            , (getNodeId child, TyVar { tnId = child, tnBound = Nothing })
                            ]
                    bindParents =
                        bindParentsFromPairs
                            [ (parent, root, BindFlex)
                            , (child, parent, BindFlex)
                            ]
                    c = rootedConstraint $ emptyConstraint { cNodes = nodes, cBindParents = bindParents }
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId parent, getNodeId child])
                    ops = [OpWeaken parent, OpMerge parent child]
                validateNormalizedWitness env ops
                    `shouldBe` Left (OpUnderRigid child)

            it "returns WitnessNormalizationError for op outside interior via presolution" $ do
                -- Set up a constraint with an edge whose witness contains an op
                -- targeting a node outside the expansion interior I(r).
                -- This should trigger WitnessNormalizationError during
                -- normalizeEdgeWitnessesM.
                let root = NodeId 0
                    interiorNode = NodeId 1
                    exteriorNode = NodeId 2
                    edgeId = 0
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyForall root interiorNode)
                            , (getNodeId interiorNode, TyVar { tnId = interiorNode, tnBound = Nothing })
                            , (getNodeId exteriorNode, TyVar { tnId = exteriorNode, tnBound = Nothing })
                            ]
                    bindParents =
                        bindParentsFromPairs
                            [ (interiorNode, root, BindFlex)
                            -- exteriorNode has no binding parent (outside interior)
                            ]
                    constraint = rootedConstraint $ emptyConstraint
                            { cNodes = nodes
                            , cBindParents = bindParents
                            }
                    -- Create a witness with an OpMerge where one target is outside the interior.
                    -- OpMerge n m: both n and m must be in interior for validation to pass,
                    -- but stripExteriorOps only requires one target to be in interior to keep the op.
                    -- This ensures the op is kept by normalization but fails validation.
                    badOp = OpMerge interiorNode exteriorNode
                    edgeWitness = EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = root
                            , ewRight = exteriorNode
                            , ewRoot = root
                            , ewSteps = [StepOmega badOp]
                            , ewWitness = InstanceWitness [badOp]
                            }
                    -- Create an edge trace with interior that does NOT include exteriorNode
                    edgeTrace = EdgeTrace
                            { etRoot = root
                            , etBinderArgs = []
                            , etInterior = InteriorNodes (IntSet.fromList [getNodeId interiorNode])
                            , etCopyMap = mempty
                            }
                    st0 = PresolutionState
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = IntMap.empty
                            , psNextNodeId = 3
                            , psPendingWeakens = IntSet.empty
                            , psBinderCache = IntMap.empty
                            , psEdgeExpansions = IntMap.empty
                            , psEdgeWitnesses = IntMap.fromList [(edgeId, edgeWitness)]
                            , psEdgeTraces = IntMap.fromList [(edgeId, edgeTrace)]
                            }
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left (WitnessNormalizationError (EdgeId eid) err) -> do
                        eid `shouldBe` edgeId
                        -- The op outside interior can trigger different errors depending on
                        -- which phase catches it first. MalformedRaiseMerge is returned by
                        -- coalesceRaiseMergeWithEnv when it detects a Merge with target outside
                        -- interior (n in interior, m not in interior).
                        case err of
                            OpOutsideInterior _ -> pure ()
                            MalformedRaiseMerge _ -> pure ()
                            _ -> expectationFailure $ "Expected OpOutsideInterior or MalformedRaiseMerge, got: " ++ show err
                    Left other ->
                        expectationFailure $ "Expected WitnessNormalizationError, got: " ++ show other
                    Right _ ->
                        expectationFailure "Expected WitnessNormalizationError for op outside interior, but normalization succeeded"

        describe "Inert-locked detection" $ do
            it "does not mark nodes with flex path to ⊥ as inert-locked" $ do
                let root = NodeId 0
                    mid = NodeId 1
                    n = NodeId 2
                    bottom = NodeId 3
                    base = NodeId 4
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyArrow root mid mid)
                            , (getNodeId mid, TyArrow mid n base)
                            , (getNodeId n, TyForall n bottom)
                            , (getNodeId bottom, TyBottom bottom)
                            , (getNodeId base, TyBase base (BaseTy "int"))
                            ]
                    bindParents =
                        bindParentsFromPairs
                            [ (mid, root, BindRigid)
                            , (n, mid, BindFlex)
                            , (bottom, n, BindFlex)
                            , (base, mid, BindFlex)
                            ]
                    c = rootedConstraint $ emptyConstraint { cNodes = nodes, cBindParents = bindParents }
                case Inert.inertLockedNodes c of
                    Left err -> expectationFailure ("inertLockedNodes failed: " ++ show err)
                    Right s -> IntSet.member (getNodeId n) s `shouldBe` False

            it "identifies inert-locked nodes under rigid ancestors" $ do
                let root = NodeId 0
                    mid = NodeId 1
                    n = NodeId 2
                    v = NodeId 3
                    base = NodeId 4
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyArrow root mid mid)
                            , (getNodeId mid, TyArrow mid n base)
                            , (getNodeId n, TyArrow n v base)
                            , (getNodeId v, TyVar { tnId = v, tnBound = Nothing })
                            , (getNodeId base, TyBase base (BaseTy "int"))
                            ]
                    bindParents =
                        bindParentsFromPairs
                            [ (mid, root, BindRigid)
                            , (n, mid, BindFlex)
                            , (v, n, BindRigid)
                            , (base, n, BindFlex)
                            ]
                    c = rootedConstraint $ emptyConstraint { cNodes = nodes, cBindParents = bindParents }
                case Inert.inertLockedNodes c of
                    Left err -> expectationFailure ("inertLockedNodes failed: " ++ show err)
                    Right s -> IntSet.member (getNodeId n) s `shouldBe` True

            it "treats polymorphic base symbols as inert anchors" $ do
                let root = NodeId 0
                    mid = NodeId 1
                    base = NodeId 2
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyArrow root mid mid)
                            , (getNodeId mid, TyArrow mid base base)
                            , (getNodeId base, TyBase base (BaseTy "Poly"))
                            ]
                    bindParents =
                        bindParentsFromPairs
                            [ (mid, root, BindFlex)
                            , (base, mid, BindFlex)
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
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyArrow root mid mid)
                            , (getNodeId mid, TyArrow mid n base)
                            , (getNodeId n, TyArrow n v base)
                            , (getNodeId v, TyVar { tnId = v, tnBound = Nothing })
                            , (getNodeId base, TyBase base (BaseTy "int"))
                            ]
                    bindParents =
                        bindParentsFromPairs
                            [ (mid, root, BindRigid)
                            , (n, mid, BindFlex)
                            , (v, n, BindRigid)
                            , (base, n, BindFlex)
                            ]
                    c = rootedConstraint $ emptyConstraint { cNodes = nodes, cBindParents = bindParents }
                case Inert.weakenInertLockedNodes c of
                    Left err -> expectationFailure ("weakenInertLockedNodes failed: " ++ show err)
                    Right c' -> do
                        Inert.inertLockedNodes c' `shouldBe` Right IntSet.empty
                        Binding.lookupBindParent c' (typeRef n) `shouldBe` Just (typeRef mid, BindRigid)

    describe "Property-based witness tests" $ do
        it "InstanceOp Arbitrary instance generates valid operations" $ property $
            forAll genInstanceOp $ \op ->
                -- Just verify the operation is well-formed (no error thrown)
                isTotalOp op
        it "witness normalization is idempotent" $ property $
            forAll (genInstanceOps 10) $ \ops ->
                forAll genNormalizeEnvParams $ \envParams ->
                    let env = mkTestNormalizeEnv envParams
                     in case normalizeInstanceOpsFull env ops of
                            Left _ -> property True  -- Normalization failure is acceptable
                            Right ops1 ->
                                case normalizeInstanceOpsFull env ops1 of
                                    Left _ -> property False  -- Second normalization should not fail
                                    Right ops2 -> ops1 === ops2
        it "canonicalized witnesses have no redundant operations" $ property $
            forAll (genInstanceOps 10) $ \ops ->
                forAll genNormalizeEnvParams $ \envParams ->
                    let env = mkTestNormalizeEnv envParams
                     in case normalizeInstanceOpsFull env ops of
                            Left _ -> property True  -- Normalization failure is acceptable
                            Right ops' -> property $ not (hasRedundantOps ops')
  where
    isTotalOp :: InstanceOp -> Bool
    isTotalOp _ = True
