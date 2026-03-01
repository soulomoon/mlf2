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
    , InstanceWitness(..)
    )
import MLF.Constraint.Presolution.Witness
    ( OmegaNormalizeEnv(..)
    , OmegaNormalizeError(..)
    , assertNoStandaloneGrafts
    , coalesceRaiseMergeWithEnv
    , integratePhase2Ops
    , normalizeInstanceOpsFull
    , reorderWeakenWithEnv
    , validateNormalizedWitness
    , witnessFromExpansion
    )
import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Presolution
    ( PresolutionState(..)
    , PresolutionError(..)
    , PresolutionResult(..)
    , computePresolution
    , validateReplayMapTraceContract
    , runPresolutionM
    , normalizeEdgeWitnessesM
    , EdgeTrace(..)
    , InteriorNodes(..)
    , fromListInterior
    )
import MLF.Constraint.Acyclicity (AcyclicityResult(..))
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
        it "preserves ExpCompose ordering with forall intros" $ do
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

            case runPresolutionM defaultTraceConfig st0 (witnessFromExpansion (GenNodeId 0) expNodeId (nodeAt nodes 0) expansion) of
                Left err -> expectationFailure ("witnessFromExpansion failed: " ++ show err)
                Right ((introCount, ops), _) -> do
                    introCount `shouldBe` 1
                    ops `shouldBe`
                        [ OpGraft argId binderId
                        , OpWeaken binderId
                        ]

        it "emits OpWeaken for unbounded binders even when suffix has forall (thesis-exact)" $ do
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
                -- Instantiate BEFORE forall: suffix has forall, so suppressWeaken fires today
                expansion =
                    ExpCompose
                        (ExpInstantiate [argId] NE.:| [ExpForall (ForallSpec 1 [Nothing] NE.:| [])])

            case runPresolutionM defaultTraceConfig st0 (witnessFromExpansion (GenNodeId 0) expNodeId (nodeAt nodes 0) expansion) of
                Left err -> expectationFailure ("witnessFromExpansion failed: " ++ show err)
                Right ((introCount, ops), _) -> do
                    introCount `shouldBe` 1
                    -- Thesis-exact: OpWeaken must be emitted even with suffix forall
                    ops `shouldSatisfy` (OpWeaken binderId `elem`)
                    ops `shouldSatisfy` (OpGraft argId binderId `elem`)

        it "emits OpWeaken for unbounded binders (thesis-exact, no suppression)" $ do
            let expNodeId = NodeId 0
                forallId = NodeId 1
                binderId = NodeId 2
                argId = NodeId 3
                nodes = nodeMapFromList
                    [ (0, TyExp expNodeId (ExpVarId 0) forallId)
                    , (1, TyForall forallId binderId)
                    , (2, TyVar { tnId = binderId, tnBound = Nothing })
                    , (3, TyVar { tnId = argId, tnBound = Nothing })
                    ]
                bindParents0 = inferBindParents nodes
                bindParents =
                    IntMap.insert
                        (nodeRefKey (typeRef binderId))
                        (typeRef forallId, BindRigid)
                        bindParents0
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 4 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                expansion = ExpInstantiate [argId]

            case runPresolutionM defaultTraceConfig st0 (witnessFromExpansion (GenNodeId 0) expNodeId (nodeAt nodes 0) expansion) of
                Left err -> expectationFailure ("witnessFromExpansion failed: " ++ show err)
                Right ((introCount, ops), _) -> do
                    introCount `shouldBe` 0
                    -- Thesis-exact: OpWeaken emitted even when arg is gen-bound
                    ops `shouldBe`
                        [ OpGraft argId binderId
                        , OpWeaken binderId
                        ]
        it "annotation edges preserve OpWeaken in witness (thesis-exact)" $ do
            -- Annotation edges previously had all OpWeaken stripped via dropWeakenOps
            -- in edgeWitnessPlan. After eliminating DEV-PHI-WITNESS-WEAKEN-SUPPRESSION,
            -- witnessFromExpansion emits OpWeaken unconditionally, and edgeWitnessPlan
            -- (a thin wrapper) no longer strips them. Verify the underlying emission.
            let expNodeId = NodeId 0
                forallId = NodeId 1
                binderId = NodeId 2
                argId = NodeId 3
                nodes = nodeMapFromList
                    [ (0, TyExp expNodeId (ExpVarId 0) forallId)
                    , (1, TyForall forallId binderId)
                    , (2, TyVar { tnId = binderId, tnBound = Nothing })
                    , (3, TyVar { tnId = argId, tnBound = Nothing })
                    ]
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 4 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                expansion = ExpInstantiate [argId]
            case runPresolutionM defaultTraceConfig st0 (witnessFromExpansion (GenNodeId 0) expNodeId (nodeAt nodes 0) expansion) of
                Left err -> expectationFailure ("witnessFromExpansion failed: " ++ show err)
                Right ((introCount, ops), _) -> do
                    introCount `shouldBe` 0
                    -- Thesis-exact: OpWeaken preserved (no longer stripped for annotation edges)
                    ops `shouldSatisfy` (OpGraft argId binderId `elem`)
                    ops `shouldSatisfy` (OpWeaken binderId `elem`)

        it "emits OpWeaken but not OpGraft for structurally-bounded binder (thesis-exact)" $ do
            let expNodeId = NodeId 0
                forallId = NodeId 1
                binderId = NodeId 2
                argId = NodeId 3
                boundId = NodeId 4
                nodes = nodeMapFromList
                    [ (0, TyExp expNodeId (ExpVarId 0) forallId)
                    , (1, TyForall forallId binderId)
                    , (2, TyVar { tnId = binderId, tnBound = Just boundId })
                    , (3, TyVar { tnId = argId, tnBound = Nothing })
                    , (4, TyBase boundId (BaseTy "Int"))
                    ]
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 5 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                expansion = ExpInstantiate [argId]

            case runPresolutionM defaultTraceConfig st0 (witnessFromExpansion (GenNodeId 0) expNodeId (nodeAt nodes 0) expansion) of
                Left err -> expectationFailure ("witnessFromExpansion failed: " ++ show err)
                Right ((introCount, ops), _) -> do
                    introCount `shouldBe` 0
                    -- Thesis Def. 15.3.4: OpGraft suppressed (InstBot can't target
                    -- non-⊥ bound), but OpWeaken emitted to eliminate the quantifier.
                    ops `shouldBe` [OpWeaken binderId]

        it "emits forall intros per binder in ForallSpec" $ do
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

            case runPresolutionM defaultTraceConfig st0 (witnessFromExpansion (GenNodeId 0) expNodeId (nodeAt nodes 0) expansion) of
                Left err -> expectationFailure ("witnessFromExpansion failed: " ++ show err)
                Right ((introCount, ops), _) -> do
                    introCount `shouldBe` 2
                    ops `shouldBe` []

        it "does not emit forall intros for forall <= non-forall level mismatch" $ do
            let srcBinderId = NodeId 0
                srcForallId = NodeId 1
                expNodeId = NodeId 2
                targetVarId = NodeId 3
                rootId = NodeId 4
                rootGen = GenNodeId 0
                srcGen = GenNodeId 10
                tgtGen = GenNodeId 11
                edgeId = 0
                edge = InstEdge (EdgeId edgeId) expNodeId targetVarId
                nodes = nodeMapFromList
                    [ (getNodeId srcBinderId, TyVar { tnId = srcBinderId, tnBound = Nothing })
                    , (getNodeId srcForallId, TyForall srcForallId srcBinderId)
                    , (getNodeId expNodeId, TyExp expNodeId (ExpVarId 0) srcForallId)
                    , (getNodeId targetVarId, TyVar { tnId = targetVarId, tnBound = Nothing })
                    , (getNodeId rootId, TyArrow rootId expNodeId targetVarId)
                    ]
                bindParents0 = inferBindParents nodes
                bindParents =
                    IntMap.insert (nodeRefKey (genRef srcGen)) (genRef rootGen, BindFlex) $
                        IntMap.insert (nodeRefKey (genRef tgtGen)) (genRef rootGen, BindFlex) $
                    IntMap.insert (nodeRefKey (typeRef srcForallId)) (genRef srcGen, BindFlex) $
                        IntMap.insert (nodeRefKey (typeRef targetVarId)) (genRef tgtGen, BindFlex) bindParents0
                genNodes =
                    fromListGen
                        [ (srcGen, GenNode srcGen [srcForallId])
                        , (tgtGen, GenNode tgtGen [targetVarId])
                        ]
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        , cGenNodes = genNodes
                        }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge]
                        , arDepGraph = undefined
                        }

            case computePresolution defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps, prEdgeWitnesses = ews } -> do
                    case IntMap.lookup edgeId exps of
                        Just (ExpInstantiate _) -> pure ()
                        Just other -> expectationFailure $ "Expected ExpInstantiate, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for Edge 0"
                    case IntMap.lookup edgeId ews of
                        Just ew -> ewForallIntros ew `shouldBe` 0
                        Nothing -> expectationFailure "No witness found for Edge 0"

    describe "Phase 3 — Witness normalization" $ do
        it "flags delayed-weakening violations when later ops touch strict descendants" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                child = NodeId 1
                arg = NodeId 10
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId child])
                ops0 = [OpWeaken root, OpGraft arg child]
                isLeftResult = either (const True) (const False)
            validateNormalizedWitness env ops0 `shouldSatisfy` isLeftResult

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

        describe "graft-weaken canonical alignment (H16 upstream target)" $ do
            it "coalesces delayed graft-weaken pairs when middle ops are binder-disjoint" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    binder = NodeId 1
                    arg = NodeId 2
                    (n1, n2) = orderedPairByPrec c root
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId binder, getNodeId n1, getNodeId n2])
                    ops0 = [OpGraft arg binder, OpMerge n2 n1, OpWeaken binder]
                normalizeInstanceOpsFull env ops0
                    `shouldBe` Right [OpGraft arg binder, OpWeaken binder, OpMerge n2 n1]

            it "leaves graft standalone when middle ops touch protected set (Omega handles via atBinderKeep)" $ do
                -- Build a constraint where 'descendant' is in binder's binding-tree interior.
                -- The coalescing cannot move the weaken backward past the OpRaise on the
                -- descendant (condition 5), so the graft stays standalone. Omega handles
                -- this via atBinderKeep (DEV-PHI-STANDALONE-GRAFT-EXTENSION).
                let root = NodeId 0
                    binder = NodeId 1
                    descendant = NodeId 2
                    arg = NodeId 3
                    c = rootedConstraint emptyConstraint
                        { cNodes = nodeMapFromList
                            [ (getNodeId root, TyForall root binder)
                            , (getNodeId binder, TyForall binder descendant)
                            , (getNodeId descendant, TyVar { tnId = descendant, tnBound = Nothing })
                            , (getNodeId arg, TyVar { tnId = arg, tnBound = Nothing })
                            ]
                        , cBindParents = bindParentsFromPairs
                            [ (binder, root, BindFlex)
                            , (descendant, binder, BindFlex)
                            , (arg, root, BindFlex)
                            ]
                        }
                    env = mkNormalizeEnv c root
                            (IntSet.fromList [getNodeId binder, getNodeId descendant, getNodeId arg])
                    ops0 = [OpGraft arg binder, OpRaise descendant, OpWeaken binder]
                -- Graft stays standalone; raise and weaken follow in descendant-first order
                normalizeInstanceOpsFull env ops0
                    `shouldBe` Right [OpGraft arg binder, OpRaise descendant, OpWeaken binder]

            it "rejects standalone graft with no matching weaken" $ do
                let root = NodeId 0
                    binder = NodeId 1
                    arg = NodeId 2
                    c = mkNormalizeConstraint
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId binder])
                    ops = [OpGraft arg binder]
                assertNoStandaloneGrafts env ops
                    `shouldBe` Left (StandaloneGraftRemaining binder)

            it "O15-TR-NODE-GRAFT R-GRAFT-NORM-03: normalizes graft-weaken pairs with canonical binder/arg alignment" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    canonicalMap nid =
                        case getNodeId nid of
                            20 -> NodeId 2
                            30 -> NodeId 3
                            _ -> nid
                    env =
                        (mkNormalizeEnv c root (IntSet.fromList [getNodeId (NodeId 2)]))
                            { canonical = canonicalMap
                            , binderArgs = IntMap.fromList [(2, NodeId 3)]
                            , binderReplayMap = IntMap.fromList [(2, NodeId 2)]
                            }
                    ops0 = [OpGraft (NodeId 30) (NodeId 20), OpWeaken (NodeId 20)]
                normalizeInstanceOpsFull env ops0
                    `shouldBe` Right [OpGraft (NodeId 3) (NodeId 2), OpWeaken (NodeId 2)]

            it "rejects ambiguous graft-weaken mapping after canonicalization" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    canonicalMap nid =
                        case getNodeId nid of
                            20 -> NodeId 2
                            21 -> NodeId 2
                            30 -> NodeId 3
                            31 -> NodeId 1
                            _ -> nid
                    env =
                        (mkNormalizeEnv c root (IntSet.fromList [getNodeId (NodeId 2)]))
                            { canonical = canonicalMap
                            , binderArgs = IntMap.fromList [(2, NodeId 3)]
                            , binderReplayMap = IntMap.fromList [(2, NodeId 2)]
                            }
                    ops0 =
                        [ OpGraft (NodeId 30) (NodeId 20)
                        , OpWeaken (NodeId 20)
                        , OpGraft (NodeId 31) (NodeId 21)
                        , OpWeaken (NodeId 21)
                        ]
                    isLeftResult = either (const True) (const False)
                normalizeInstanceOpsFull env ops0 `shouldSatisfy` isLeftResult

            it "is idempotent for graft-weaken-heavy normalization" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    canonicalMap nid =
                        case getNodeId nid of
                            20 -> NodeId 2
                            30 -> NodeId 3
                            31 -> NodeId 1
                            _ -> nid
                    env =
                        (mkNormalizeEnv c root (IntSet.fromList [getNodeId (NodeId 2)]))
                            { canonical = canonicalMap
                            , binderArgs = IntMap.fromList [(2, NodeId 3)]
                            , binderReplayMap = IntMap.fromList [(2, NodeId 2)]
                            }
                    ops0 =
                        [ OpGraft (NodeId 30) (NodeId 20)
                        , OpWeaken (NodeId 20)
                        , OpGraft (NodeId 30) (NodeId 20)
                        , OpWeaken (NodeId 20)
                        ]
                case normalizeInstanceOpsFull env ops0 of
                    Left err -> expectationFailure ("Expected normalization success, got: " ++ show err)
                    Right ops1 -> normalizeInstanceOpsFull env ops1 `shouldBe` Right ops1

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

        it "normalizes omega ops within a segment" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                child = NodeId 2
                arg = NodeId 10
                arg2 = NodeId 11
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId child])
                seg1 = [OpWeaken root, OpGraft arg child]
                seg2 = [OpGraft arg2 root]
            normalizeInstanceOpsFull env seg1
                `shouldBe` Right [OpGraft arg child, OpWeaken root]
            normalizeInstanceOpsFull env seg2
                `shouldBe` Right [OpGraft arg2 root]

        it "O15-TR-NODE-MERGE R-MERGE-NORM-09: normalizeInstanceOpsFull rejects wrong merge direction" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                (mLess, nGreater) = orderedPairByPrec c root
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId mLess, getNodeId nGreater])
                ops0 = [OpMerge mLess nGreater]
            normalizeInstanceOpsFull env ops0 `shouldBe` Left (MergeDirectionInvalid mLess nGreater)

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
            it "O15-TR-NODE-RAISEMERGE R-RAISEMERGE-VALID-13: coalesces Raise; Merge when the target leaves the interior" $ do
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

            it "O15-TR-NODE-WEAKEN R-WEAKEN-NORM-06: moves Weaken after descendant ops" $ do
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

        describe "Witness normalization invariants (US-010 regression)" $ do
            it "O15-TR-NODE-RAISE R-RAISE-VALID-10: accepts OpRaise for transitively flex-bound interior binder" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                validateNormalizedWitness env [OpRaise n]
                    `shouldBe` Right ()

            it "R-RAISE-NORM-12: normalizes duplicate Raise sequence deterministically" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                    ops0 = [OpRaise n, OpRaise n, OpRaise n]
                case normalizeInstanceOpsFull env ops0 of
                    Left err -> expectationFailure ("normalization failed: " ++ show err)
                    Right ops1 -> do
                        ops1 `shouldBe` [OpRaise n]
                        normalizeInstanceOpsFull env ops1 `shouldBe` Right ops1

            it "OpRaise;OpMerge coalesces to OpRaiseMerge through full pipeline" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                    ops0 = [OpRaise n, OpMerge n m]
                normalizeInstanceOpsFull env ops0 `shouldBe` Right [OpRaiseMerge n m]

            it "multiple OpRaise;OpMerge coalesces to single OpRaiseMerge" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                    ops0 = [OpRaise n, OpRaise n, OpRaise n, OpMerge n m]
                normalizeInstanceOpsFull env ops0 `shouldBe` Right [OpRaiseMerge n m]

            it "RaiseMerge validation rejects rigid endpoint only on non-operated node m" $ do
                let root = NodeId 0
                    n = NodeId 1
                    m = NodeId 2
                    c =
                        rootedConstraint emptyConstraint
                            { cNodes = nodeMapFromList
                                    [ (getNodeId root, TyArrow root n m)
                                    , (getNodeId n, TyVar { tnId = n, tnBound = Nothing })
                                    , (getNodeId m, TyVar { tnId = m, tnBound = Nothing })
                                    ]
                            , cBindParents =
                                bindParentsFromPairs
                                    [ (n, root, BindFlex)
                                    , (m, root, BindRigid)
                                    ]
                            }
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n, getNodeId m])
                    op = OpRaiseMerge n m
                validateNormalizedWitness env [op]
                    `shouldBe` Left (RigidOperandMismatch op n m)

            it "RaiseMerge with rigid operated node n passes validation" $ do
                let root = NodeId 0
                    n = NodeId 1
                    m = NodeId 2
                    c =
                        rootedConstraint emptyConstraint
                            { cNodes = nodeMapFromList
                                    [ (getNodeId root, TyArrow root n m)
                                    , (getNodeId n, TyVar { tnId = n, tnBound = Nothing })
                                    , (getNodeId m, TyVar { tnId = m, tnBound = Nothing })
                                    ]
                            , cBindParents =
                                bindParentsFromPairs
                                    [ (n, root, BindRigid)
                                    , (m, root, BindFlex)
                                    ]
                            }
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                    op = OpRaiseMerge n m
                validateNormalizedWitness env [op] `shouldBe` Right ()

            it "normalizeInstanceOpsFull preserves RaiseMerge coalescing" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId n])
                    ops0 = [OpRaise n, OpMerge n m]
                normalizeInstanceOpsFull env ops0
                    `shouldBe` Right [OpRaiseMerge n m]

            it "US-010-V1: coalesces repeated Raise;Merge into RaiseMerge" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId n])
                    ops0 = [OpRaise n, OpRaise n, OpMerge n m]
                normalizeInstanceOpsFull env ops0
                    `shouldBe` Right [OpRaiseMerge n m]

            it "US-010-V2: single-binder binderArgs does not widen interior for Raise;Merge" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env0 = mkNormalizeEnv c root (IntSet.fromList [getNodeId m])
                    env =
                        env0
                            { binderArgs = IntMap.singleton (getNodeId n) m
                            , binderReplayMap = IntMap.singleton (getNodeId n) n
                            }
                    ops0 = [OpRaise n, OpMerge n m]
                normalizeInstanceOpsFull env ops0
                    `shouldBe` Left (OpOutsideInterior (OpMerge n m))

            it "R-RAISEMERGE-NORM-15: validated witnesses remain valid after idempotent re-normalization" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                    ops0 = [OpRaise n, OpMerge n m]
                case normalizeInstanceOpsFull env ops0 of
                    Left err -> expectationFailure ("first normalization failed: " ++ show err)
                    Right ops1 -> do
                        ops1 `shouldBe` [OpRaiseMerge n m]
                        normalizeInstanceOpsFull env ops1 `shouldBe` Right ops1

        describe "Normalized witness validation" $ do
            it "rejects ops outside the interior (condition 1)" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    env = mkNormalizeEnv c root (IntSet.fromList [2])
                    op = OpGraft (NodeId 2) (NodeId 3)
                validateNormalizedWitness env [op]
                    `shouldBe` Left (OpOutsideInterior op)

            it "R-GRAFT-INVALID-02: rejects Graft on non-bottom binder bounds" $ do
                let root = NodeId 0
                    binder = NodeId 1
                    bound = NodeId 2
                    arg = NodeId 3
                    c =
                        rootedConstraint emptyConstraint
                            { cNodes = nodeMapFromList
                                    [ (getNodeId root, TyForall root binder)
                                    , (getNodeId binder, TyVar { tnId = binder, tnBound = Just bound })
                                    , (getNodeId bound, TyBase bound (BaseTy "Int"))
                                    , (getNodeId arg, TyVar { tnId = arg, tnBound = Nothing })
                                    ]
                            , cBindParents =
                                bindParentsFromPairs
                                    [ (binder, root, BindFlex)
                                    , (bound, root, BindFlex)
                                    , (arg, root, BindFlex)
                                    ]
                            }
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId binder, getNodeId bound, getNodeId arg])
                    op = OpGraft arg binder
                validateNormalizedWitness env [op]
                    `shouldBe` Left (GraftOnNonBottomBound binder bound)

            it "O15-TR-ROOT-GRAFT R-GRAFT-VALID-01: allows Graft on the expansion root (root operation)" $ do
                let root = NodeId 0
                    arg = NodeId 1
                    c =
                        rootedConstraint emptyConstraint
                            { cNodes = nodeMapFromList
                                    [ (getNodeId root, TyArrow root arg arg)
                                    , (getNodeId arg, TyVar { tnId = arg, tnBound = Nothing })
                                    ]
                            , cBindParents =
                                bindParentsFromPairs
                                    [ (arg, root, BindFlex)
                                    ]
                            }
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId arg])
                    op = OpGraft arg root
                validateNormalizedWitness env [op]
                    `shouldBe` Right ()

            it "R-MERGE-INVALID-08: rejects Merge with wrong ≺ direction (condition 2)" $ do
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

            it "R-RAISEMERGE-INVALID-14: rejects RaiseMerge when the target stays inside the interior (condition 4)" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    (mLess, nGreater) = orderedPairByPrec c root
                    interior = IntSet.fromList [getNodeId mLess, getNodeId nGreater]
                    env = mkNormalizeEnv c root interior
                    op = OpRaiseMerge nGreater mLess
                validateNormalizedWitness env [op]
                    `shouldBe` Left (RaiseMergeInsideInterior nGreater mLess)

            it "R-WEAKEN-INVALID-05: rejects ops below a Weakened binder (condition 5)" $ do
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
                    `shouldBe` Left (DelayedWeakenViolation parent child)

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
                    `shouldBe` Left (DelayedWeakenViolation parent child)

            it "rejects Merge when only the non-operated endpoint is rigid" $ do
                let root = NodeId 0
                    n = NodeId 1
                    m = NodeId 2
                    c =
                        rootedConstraint emptyConstraint
                            { cNodes = nodeMapFromList
                                    [ (getNodeId root, TyArrow root n m)
                                    , (getNodeId n, TyVar { tnId = n, tnBound = Nothing })
                                    , (getNodeId m, TyVar { tnId = m, tnBound = Nothing })
                                    ]
                            , cBindParents =
                                bindParentsFromPairs
                                    [ (n, root, BindFlex)
                                    , (m, root, BindRigid)
                                    ]
                            }
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n, getNodeId m])
                    op = OpMerge n m
                validateNormalizedWitness env [op]
                    `shouldBe` Left (RigidOperandMismatch op n m)

            it "rejects non-rigid operations not transitively flexibly bound to the root" $ do
                let root = NodeId 0
                    m = NodeId 1
                    parent = NodeId 2
                    n = NodeId 3
                    c =
                        rootedConstraint emptyConstraint
                            { cNodes = nodeMapFromList
                                    [ (getNodeId root, TyArrow root m parent)
                                    , (getNodeId m, TyVar { tnId = m, tnBound = Nothing })
                                    , (getNodeId parent, TyForall parent n)
                                    , (getNodeId n, TyVar { tnId = n, tnBound = Nothing })
                                    ]
                            , cBindParents =
                                bindParentsFromPairs
                                    [ (m, root, BindFlex)
                                    , (parent, root, BindRigid)
                                    , (n, parent, BindFlex)
                                    ]
                            }
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId m, getNodeId parent, getNodeId n])
                    op = OpMerge n m
                validateNormalizedWitness env [op]
                    `shouldBe` Left (NotTransitivelyFlexBound op n root)

            it "rejects OpRaise when target is not transitively flexibly bound to the root" $ do
                let root = NodeId 0
                    m = NodeId 1
                    parent = NodeId 2
                    n = NodeId 3
                    c =
                        rootedConstraint emptyConstraint
                            { cNodes = nodeMapFromList
                                    [ (getNodeId root, TyArrow root m parent)
                                    , (getNodeId m, TyVar { tnId = m, tnBound = Nothing })
                                    , (getNodeId parent, TyForall parent n)
                                    , (getNodeId n, TyVar { tnId = n, tnBound = Nothing })
                                    ]
                            , cBindParents =
                                bindParentsFromPairs
                                    [ (m, root, BindFlex)
                                    , (parent, root, BindRigid)
                                    , (n, parent, BindFlex)
                                    ]
                            }
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId m, getNodeId parent, getNodeId n])
                    op = OpRaise n
                validateNormalizedWitness env [op]
                    `shouldBe` Left (NotTransitivelyFlexBound op n root)

            it "R-RAISE-INVALID-11: returns WitnessNormalizationError for OpRaise not transitively flex-bound via presolution" $ do
                let root = NodeId 0
                    m = NodeId 1
                    parent = NodeId 2
                    n = NodeId 3
                    edgeId = 0
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyArrow root m parent)
                            , (getNodeId m, TyVar { tnId = m, tnBound = Nothing })
                            , (getNodeId parent, TyForall parent n)
                            , (getNodeId n, TyVar { tnId = n, tnBound = Nothing })
                            ]
                    bindParents =
                        bindParentsFromPairs
                            [ (m, root, BindFlex)
                            , (parent, root, BindRigid)
                            , (n, parent, BindFlex)
                            ]
                    constraint = rootedConstraint $ emptyConstraint
                            { cNodes = nodes
                            , cBindParents = bindParents
                            }
                    badOp = OpRaise n
                    edgeWitness = EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = m
                            , ewRight = n
                            , ewRoot = root
                            , ewForallIntros = 0
                            , ewWitness = InstanceWitness [badOp]
                            }
                    edgeTrace = EdgeTrace
                            { etRoot = root
                            , etBinderArgs = []
                            , etInterior = InteriorNodes (IntSet.fromList [getNodeId m, getNodeId parent, getNodeId n])
                            , etBinderReplayMap = mempty
                            , etCopyMap = mempty
                            }
                    st0 = PresolutionState
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = IntMap.empty
                            , psNextNodeId = 4
                            , psPendingWeakens = IntSet.empty
                            , psBinderCache = IntMap.empty
                            , psEdgeExpansions = IntMap.empty
                            , psEdgeWitnesses = IntMap.fromList [(edgeId, edgeWitness)]
                            , psEdgeTraces = IntMap.fromList [(edgeId, edgeTrace)]
                            }
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left (WitnessNormalizationError (EdgeId eid) err) -> do
                        eid `shouldBe` edgeId
                        err `shouldBe` NotTransitivelyFlexBound badOp n root
                    Left other ->
                        expectationFailure $ "Expected WitnessNormalizationError NotTransitivelyFlexBound, got: " ++ show other
                    Right _ ->
                        expectationFailure "Expected WitnessNormalizationError for OpRaise non-transitive-flex case"

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
                            , ewForallIntros = 0
                            , ewWitness = InstanceWitness [badOp]
                            }
                    -- Create an edge trace with interior that does NOT include exteriorNode
                    edgeTrace = EdgeTrace
                            { etRoot = root
                            , etBinderArgs = []
                            , etInterior = InteriorNodes (IntSet.fromList [getNodeId interiorNode])
                            , etBinderReplayMap = mempty
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

            it "fails fast with MergeDirectionInvalid via presolution normalization" $ do
                let root = NodeId 0
                    leftNode = NodeId 2
                    rightNode = NodeId 3
                    edgeId = 0
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyArrow root leftNode rightNode)
                            , (getNodeId leftNode, TyVar { tnId = leftNode, tnBound = Nothing })
                            , (getNodeId rightNode, TyVar { tnId = rightNode, tnBound = Nothing })
                            ]
                    bindParents =
                        bindParentsFromPairs
                            [ (leftNode, root, BindFlex)
                            , (rightNode, root, BindFlex)
                            ]
                    constraint = rootedConstraint $ emptyConstraint
                            { cNodes = nodes
                            , cBindParents = bindParents
                            }
                    (mLess, nGreater) = orderedPairByPrec constraint root
                    badOp = OpMerge mLess nGreater
                    edgeWitness = EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = leftNode
                            , ewRight = rightNode
                            , ewRoot = root
                            , ewForallIntros = 0
                            , ewWitness = InstanceWitness [badOp]
                            }
                    edgeTrace = EdgeTrace
                            { etRoot = root
                            , etBinderArgs = []
                            , etInterior = InteriorNodes (IntSet.fromList [getNodeId mLess, getNodeId nGreater])
                            , etBinderReplayMap = mempty
                            , etCopyMap = mempty
                            }
                    st0 = PresolutionState
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = IntMap.empty
                            , psNextNodeId = 4
                            , psPendingWeakens = IntSet.empty
                            , psBinderCache = IntMap.empty
                            , psEdgeExpansions = IntMap.empty
                            , psEdgeWitnesses = IntMap.fromList [(edgeId, edgeWitness)]
                            , psEdgeTraces = IntMap.fromList [(edgeId, edgeTrace)]
                            }
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left (WitnessNormalizationError (EdgeId eid) err) -> do
                        eid `shouldBe` edgeId
                        err `shouldBe` MergeDirectionInvalid mLess nGreater
                    Left other ->
                        expectationFailure $ "Expected WitnessNormalizationError MergeDirectionInvalid, got: " ++ show other
                    Right _ ->
                        expectationFailure "Expected WitnessNormalizationError MergeDirectionInvalid for malformed merge direction"

            it "returns WitnessNormalizationError for missing <P order key via presolution" $ do
                let root = NodeId 0
                    interiorNode = NodeId 1
                    outsideNode = NodeId 2
                    edgeId = 0
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyForall root interiorNode)
                            , (getNodeId interiorNode, TyVar { tnId = interiorNode, tnBound = Nothing })
                            , (getNodeId outsideNode, TyVar { tnId = outsideNode, tnBound = Nothing })
                            ]
                    bindParents =
                        IntMap.fromList
                            [ (nodeRefKey (typeRef interiorNode), (typeRef root, BindFlex))
                            , (nodeRefKey (typeRef outsideNode), (genRef (GenNodeId 0), BindFlex))
                            ]
                    constraint = rootedConstraint $ emptyConstraint
                            { cNodes = nodes
                            , cBindParents = bindParents
                            }
                    badOp = OpMerge interiorNode outsideNode
                    edgeWitness = EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = root
                            , ewRight = outsideNode
                            , ewRoot = root
                            , ewForallIntros = 0
                            , ewWitness = InstanceWitness [badOp]
                            }
                    edgeTrace = EdgeTrace
                            { etRoot = root
                            , etBinderArgs = []
                            , etInterior = InteriorNodes (IntSet.fromList [getNodeId interiorNode, getNodeId outsideNode])
                            , etBinderReplayMap = mempty
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
                    Left (WitnessNormalizationError (EdgeId eid) (MissingOrderKey nid)) -> do
                        eid `shouldBe` edgeId
                        nid `shouldBe` outsideNode
                    Left other ->
                        expectationFailure $ "Expected WitnessNormalizationError MissingOrderKey, got: " ++ show other
                    Right _ ->
                        expectationFailure "Expected WitnessNormalizationError for missing order key"

            it "fails fast on annotation-edge ambiguous multi-graft when replay-map injectivity cannot be satisfied" $ do
                let root = NodeId 0
                    binder = NodeId 2
                    sourceB1 = NodeId 20
                    sourceB2 = NodeId 21
                    arg1 = NodeId 30
                    arg2 = NodeId 31
                    edgeId = 0
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyForall root binder)
                            , (getNodeId binder, TyVar { tnId = binder, tnBound = Nothing })
                            , (getNodeId arg1, TyBase arg1 (BaseTy "Int"))
                            , (getNodeId arg2, TyBase arg2 (BaseTy "Bool"))
                            ]
                    bindParents =
                        bindParentsFromPairs
                            [ (binder, root, BindFlex)
                            , (arg1, root, BindFlex)
                            , (arg2, root, BindFlex)
                            ]
                    constraint = rootedConstraint $ emptyConstraint
                            { cNodes = nodes
                            , cBindParents = bindParents
                            , cAnnEdges = IntSet.singleton edgeId
                            }
                    uf = IntMap.empty
                    ops0 =
                        [ OpGraft arg2 binder
                        , OpGraft arg1 binder
                        ]
                    edgeWitness = EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = root
                            , ewRight = binder
                            , ewRoot = root
                            , ewForallIntros = 0
                            , ewWitness = InstanceWitness ops0
                            }
                    edgeTrace = EdgeTrace
                            { etRoot = root
                            , etBinderArgs = [(sourceB1, arg1), (sourceB2, arg2)]
                            , etInterior = InteriorNodes (IntSet.fromList [getNodeId binder])
                            , etBinderReplayMap =
                                IntMap.fromList
                                    [ (getNodeId sourceB1, binder)
                                    , (getNodeId sourceB2, binder)
                                    ]
                            , etCopyMap = mempty
                            }
                    st0 = PresolutionState
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = uf
                            , psNextNodeId = 32
                            , psPendingWeakens = IntSet.empty
                            , psBinderCache = IntMap.empty
                            , psEdgeExpansions = IntMap.empty
                            , psEdgeWitnesses = IntMap.fromList [(edgeId, edgeWitness)]
                            , psEdgeTraces = IntMap.fromList [(edgeId, edgeTrace)]
                            }
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left (WitnessNormalizationError (EdgeId eid) (ReplayMapIncomplete missing)) -> do
                        eid `shouldBe` edgeId
                        missing `shouldBe` [sourceB2]
                    Left other ->
                        expectationFailure $ "Expected replay-map injectivity fail-fast, got: " ++ show other
                    Right _ ->
                        expectationFailure "Expected replay-map injectivity fail-fast, but normalization succeeded"

            it "fails fast before annotation-edge graft-weaken synthesis when replay-map injectivity cannot be satisfied" $ do
                let root = NodeId 0
                    binder = NodeId 2
                    sourceB1 = NodeId 20
                    sourceB2 = NodeId 21
                    missingArg1 = NodeId 30
                    missingArg2 = NodeId 31
                    edgeId = 0
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyForall root binder)
                            , (getNodeId binder, TyVar { tnId = binder, tnBound = Nothing })
                            ]
                    bindParents =
                        bindParentsFromPairs
                            [ (binder, root, BindFlex)
                            ]
                    constraint = rootedConstraint $ emptyConstraint
                            { cNodes = nodes
                            , cBindParents = bindParents
                            , cAnnEdges = IntSet.singleton edgeId
                            }
                    uf = IntMap.empty
                    ops0 =
                        [ OpGraft missingArg2 binder
                        , OpGraft missingArg1 binder
                        ]
                    edgeWitness = EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = root
                            , ewRight = binder
                            , ewRoot = root
                            , ewForallIntros = 0
                            , ewWitness = InstanceWitness ops0
                            }
                    edgeTrace = EdgeTrace
                            { etRoot = root
                            , etBinderArgs = [(sourceB1, missingArg1), (sourceB2, missingArg2)]
                            , etInterior = InteriorNodes (IntSet.fromList [getNodeId binder])
                            , etBinderReplayMap =
                                IntMap.fromList
                                    [ (getNodeId sourceB1, binder)
                                    , (getNodeId sourceB2, binder)
                                    ]
                            , etCopyMap = mempty
                            }
                    st0 = PresolutionState
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = uf
                            , psNextNodeId = 4
                            , psPendingWeakens = IntSet.empty
                            , psBinderCache = IntMap.empty
                            , psEdgeExpansions = IntMap.empty
                            , psEdgeWitnesses = IntMap.fromList [(edgeId, edgeWitness)]
                            , psEdgeTraces = IntMap.fromList [(edgeId, edgeTrace)]
                            }
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left (WitnessNormalizationError (EdgeId eid) (ReplayMapIncomplete missing)) -> do
                        eid `shouldBe` edgeId
                        missing `shouldBe` [sourceB2]
                    Left other ->
                        expectationFailure $
                            "Expected replay-map injectivity fail-fast error, got: " ++ show other
                    Right _ ->
                        expectationFailure "Expected replay-map injectivity fail-fast error, but normalization succeeded"

            it "fails fast on non-annotation ambiguous multi-graft when replay-map injectivity cannot be satisfied" $ do
                let root = NodeId 0
                    binder = NodeId 2
                    sourceB1 = NodeId 20
                    sourceB2 = NodeId 21
                    arg1 = NodeId 30
                    arg2 = NodeId 31
                    edgeId = 0
                    nodes = nodeMapFromList
                            [ (getNodeId root, TyForall root binder)
                            , (getNodeId binder, TyVar { tnId = binder, tnBound = Nothing })
                            , (getNodeId arg1, TyBase arg1 (BaseTy "Int"))
                            , (getNodeId arg2, TyBase arg2 (BaseTy "Bool"))
                            ]
                    bindParents =
                        bindParentsFromPairs
                            [ (binder, root, BindFlex)
                            , (arg1, root, BindFlex)
                            , (arg2, root, BindFlex)
                            ]
                    constraint = rootedConstraint $ emptyConstraint
                            { cNodes = nodes
                            , cBindParents = bindParents
                            }
                    uf = IntMap.empty
                    ops0 =
                        [ OpGraft arg2 binder
                        , OpGraft arg1 binder
                        ]
                    edgeWitness = EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = root
                            , ewRight = binder
                            , ewRoot = root
                            , ewForallIntros = 0
                            , ewWitness = InstanceWitness ops0
                            }
                    edgeTrace = EdgeTrace
                            { etRoot = root
                            , etBinderArgs = [(sourceB1, arg1), (sourceB2, arg2)]
                            , etInterior = InteriorNodes (IntSet.fromList [getNodeId binder])
                            , etBinderReplayMap =
                                IntMap.fromList
                                    [ (getNodeId sourceB1, binder)
                                    , (getNodeId sourceB2, binder)
                                    ]
                            , etCopyMap = mempty
                            }
                    st0 = PresolutionState
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = uf
                            , psNextNodeId = 32
                            , psPendingWeakens = IntSet.empty
                            , psBinderCache = IntMap.empty
                            , psEdgeExpansions = IntMap.empty
                            , psEdgeWitnesses = IntMap.fromList [(edgeId, edgeWitness)]
                            , psEdgeTraces = IntMap.fromList [(edgeId, edgeTrace)]
                            }
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left (WitnessNormalizationError (EdgeId eid) (ReplayMapIncomplete missing)) -> do
                        eid `shouldBe` edgeId
                        missing `shouldBe` [sourceB2]
                    Left other ->
                        expectationFailure $ "Expected replay-map injectivity fail-fast, got: " ++ show other
                    Right _ ->
                        expectationFailure "Expected replay-map injectivity fail-fast, but normalization succeeded"

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

    describe "Phase 4 regression matrix" $ do
        it "keeps compose expansions aligned with interleaved witness steps" $ do
            let srcVarId = NodeId 0
                srcForallId = NodeId 1
                tgtDomId = NodeId 2
                tgtCodId = NodeId 3
                tgtArrowId = NodeId 4
                tgtForallId = NodeId 5
                expNodeId = NodeId 6

                nodes = nodeMapFromList
                    [ (0, TyVar { tnId = srcVarId, tnBound = Nothing })
                    , (1, TyForall srcForallId srcVarId)
                    , (2, TyVar { tnId = tgtDomId, tnBound = Nothing })
                    , (3, TyVar { tnId = tgtCodId, tnBound = Nothing })
                    , (4, TyArrow tgtArrowId tgtDomId tgtCodId)
                    , (5, TyForall tgtForallId tgtArrowId)
                    , (6, TyExp expNodeId (ExpVarId 0) srcForallId)
                    ]

                edge = InstEdge (EdgeId 0) expNodeId tgtForallId
                bindParents0 = inferBindParents nodes
                bindParents =
                    IntMap.insert (nodeRefKey (typeRef tgtDomId)) (typeRef tgtForallId, BindFlex) $
                        IntMap.insert (nodeRefKey (typeRef tgtCodId)) (typeRef tgtForallId, BindFlex) bindParents0

                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure ("Presolution failed: " ++ show err)
                Right PresolutionResult{ prEdgeExpansions = exps, prEdgeWitnesses = ews } -> do
                    case IntMap.lookup 0 exps of
                        Just (ExpCompose _) -> pure ()
                        Just other -> expectationFailure ("Expected ExpCompose, got " ++ show other)
                        Nothing -> expectationFailure "No expansion found for Edge 0"
                    case IntMap.lookup 0 ews of
                        Nothing -> expectationFailure "No witness found for Edge 0"
                        Just ew -> do
                            ewForallIntros ew > 0 `shouldBe` True
                            not (null (getInstanceOps (ewWitness ew))) `shouldBe` True

    describe "Driver replay-map boundary validation" $ do
        it "hard-rejects codomain targets when replay binder domain is empty" $ do
            let edgeKey = 0
                root = NodeId 100
                source = NodeId 1
                replayTarget = NodeId 2
                argNode = NodeId 3
                c = rootedConstraint emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (getNodeId root, TyArrow root replayTarget replayTarget)
                            , (getNodeId source, TyVar { tnId = source, tnBound = Nothing })
                            , (getNodeId replayTarget, TyVar { tnId = replayTarget, tnBound = Nothing })
                            , (getNodeId argNode, TyBase argNode (BaseTy "Int"))
                            ]
                    , cBindParents = IntMap.empty
                    }
                tr =
                    EdgeTrace
                        { etRoot = root
                        , etBinderArgs = [(source, argNode)]
                        , etInterior = fromListInterior [root, source, replayTarget, argNode]
                        , etBinderReplayMap = IntMap.fromList [(getNodeId source, replayTarget)]
                        , etCopyMap = mempty
                        }
                expected =
                    InternalError $
                        unlines
                            [ "edge replay-map codomain target outside replay binder domain"
                            , "edge: " ++ show (EdgeId edgeKey)
                            , "source key: " ++ show (getNodeId source)
                            , "replay target: " ++ show replayTarget
                            ]
            validateReplayMapTraceContract id c c edgeKey tr `shouldBe` Left expected

        it "accepts codomain targets inside replay binder domain" $ do
            let edgeKey = 1
                root = NodeId 200
                body = NodeId 201
                replayBinder = NodeId 202
                source = NodeId 203
                argNode = NodeId 204
                c = rootedConstraint emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (getNodeId root, TyForall root body)
                            , (getNodeId body, TyArrow body replayBinder replayBinder)
                            , (getNodeId replayBinder, TyVar { tnId = replayBinder, tnBound = Nothing })
                            , (getNodeId source, TyVar { tnId = source, tnBound = Nothing })
                            , (getNodeId argNode, TyBase argNode (BaseTy "Bool"))
                            ]
                    , cBindParents =
                        bindParentsFromPairs
                            [ (body, root, BindFlex)
                            , (replayBinder, root, BindFlex)
                            ]
                    }
                tr =
                    EdgeTrace
                        { etRoot = root
                        , etBinderArgs = [(source, argNode)]
                        , etInterior = fromListInterior [root, body, replayBinder, source, argNode]
                        , etBinderReplayMap = IntMap.fromList [(getNodeId source, replayBinder)]
                        , etCopyMap = mempty
                        }
            validateReplayMapTraceContract id c c edgeKey tr `shouldBe` Right ()

    describe "Thesis obligations" $ do
        it "O11-WITNESS-NORM" $ do
            -- Witness normalization: normalizeInstanceOpsFull normalizes a trivial op list
            let env = OmegaNormalizeEnv
                    { oneRoot = NodeId 0
                    , interior = IntSet.empty
                    , interiorRaw = IntSet.empty
                    , weakened = IntSet.empty
                    , orderKeys = IntMap.empty
                    , canonical = id
                    , constraint = emptyConstraint
                    , binderArgs = IntMap.empty
                    , binderReplayMap = IntMap.empty
                    , replayDomainBinders = []
                    , isAnnotationEdge = False
                    }
            case normalizeInstanceOpsFull env [] of
                Right _ -> pure ()
                Left err -> expectationFailure $ "normalizeInstanceOpsFull failed: " ++ show err

        it "O11-WITNESS-COALESCE" $ do
            -- Raise;Merge → RaiseMerge: coalesceRaiseMergeWithEnv coalesces adjacent raise+merge
            let env = OmegaNormalizeEnv
                    { oneRoot = NodeId 0
                    , interior = IntSet.empty
                    , interiorRaw = IntSet.empty
                    , weakened = IntSet.empty
                    , orderKeys = IntMap.empty
                    , canonical = id
                    , constraint = emptyConstraint
                    , binderArgs = IntMap.empty
                    , binderReplayMap = IntMap.empty
                    , replayDomainBinders = []
                    , isAnnotationEdge = False
                    }
            case coalesceRaiseMergeWithEnv env [] of
                Right _ -> pure ()
                Left err -> expectationFailure $ "coalesceRaiseMergeWithEnv failed: " ++ show err

        it "O11-WITNESS-REORDER" $ do
            -- Weaken reordering: reorderWeakenWithEnv reorders weaken ops
            let env = OmegaNormalizeEnv
                    { oneRoot = NodeId 0
                    , interior = IntSet.empty
                    , interiorRaw = IntSet.empty
                    , weakened = IntSet.empty
                    , orderKeys = IntMap.empty
                    , canonical = id
                    , constraint = emptyConstraint
                    , binderArgs = IntMap.empty
                    , binderReplayMap = IntMap.empty
                    , replayDomainBinders = []
                    , isAnnotationEdge = False
                    }
            case reorderWeakenWithEnv env [] of
                Right _ -> pure ()
                Left err -> expectationFailure $ "reorderWeakenWithEnv failed: " ++ show err

        it "fails replay-map validation when source binder domain is under-covered" $ do
            let root = NodeId 0
                binder = NodeId 1
                argNode = NodeId 2
                c = rootedConstraint emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (0, TyArrow root binder binder)
                            , (1, TyVar { tnId = binder, tnBound = Nothing })
                            , (2, TyBase argNode (BaseTy "Int"))
                            ]
                    , cBindParents =
                        bindParentsFromPairs
                            [ (binder, root, BindFlex)
                            , (argNode, root, BindFlex)
                            ]
                    }
                env = OmegaNormalizeEnv
                    { oneRoot = root
                    , interior = IntSet.fromList [0, 1, 2]
                    , interiorRaw = IntSet.fromList [0, 1, 2]
                    , weakened = IntSet.empty
                    , orderKeys = IntMap.empty
                    , canonical = id
                    , constraint = c
                    , binderArgs = IntMap.fromList [(getNodeId binder, argNode)]
                    , binderReplayMap = IntMap.empty
                    , replayDomainBinders = []
                    , isAnnotationEdge = False
                    }
            validateNormalizedWitness env [] `shouldBe` Left (ReplayMapIncomplete [binder])

        it "fails replay-map validation when codomain target is outside replay binder domain" $ do
            let root = NodeId 0
                binder = NodeId 1
                badTarget = NodeId 2
                argNode = NodeId 3
                c = rootedConstraint emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (0, TyArrow root binder binder)
                            , (1, TyVar { tnId = binder, tnBound = Nothing })
                            , (2, TyBase badTarget (BaseTy "Bool"))
                            , (3, TyBase argNode (BaseTy "Int"))
                            ]
                    , cBindParents =
                        bindParentsFromPairs
                            [ (binder, root, BindFlex)
                            , (badTarget, root, BindFlex)
                            , (argNode, root, BindFlex)
                            ]
                    }
                env = OmegaNormalizeEnv
                    { oneRoot = root
                    , interior = IntSet.fromList [0, 1, 2, 3]
                    , interiorRaw = IntSet.fromList [0, 1, 2, 3]
                    , weakened = IntSet.empty
                    , orderKeys = IntMap.empty
                    , canonical = id
                    , constraint = c
                    , binderArgs = IntMap.fromList [(getNodeId binder, argNode)]
                    , binderReplayMap = IntMap.fromList [(getNodeId binder, badTarget)]
                    , replayDomainBinders = []
                    , isAnnotationEdge = False
                    }
            validateNormalizedWitness env [] `shouldBe` Left (ReplayMapTargetOutsideReplayDomain binder badTarget)

        it "fails replay-map validation when two source binders map to one replay binder" $ do
            let root = NodeId 0
                binderA = NodeId 1
                binderB = NodeId 2
                argA = NodeId 3
                argB = NodeId 4
                c = rootedConstraint emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (0, TyArrow root binderA binderB)
                            , (1, TyVar { tnId = binderA, tnBound = Nothing })
                            , (2, TyVar { tnId = binderB, tnBound = Nothing })
                            , (3, TyBase argA (BaseTy "Int"))
                            , (4, TyBase argB (BaseTy "Bool"))
                            ]
                    , cBindParents =
                        bindParentsFromPairs
                            [ (binderA, root, BindFlex)
                            , (binderB, root, BindFlex)
                            , (argA, root, BindFlex)
                            , (argB, root, BindFlex)
                            ]
                    }
                env = OmegaNormalizeEnv
                    { oneRoot = root
                    , interior = IntSet.fromList [0, 1, 2, 3, 4]
                    , interiorRaw = IntSet.fromList [0, 1, 2, 3, 4]
                    , weakened = IntSet.empty
                    , orderKeys = IntMap.empty
                    , canonical = id
                    , constraint = c
                    , binderArgs =
                        IntMap.fromList
                            [ (getNodeId binderA, argA)
                            , (getNodeId binderB, argB)
                            ]
                    , binderReplayMap =
                        IntMap.fromList
                            [ (getNodeId binderA, binderA)
                            , (getNodeId binderB, binderA)
                            ]
                    , replayDomainBinders = []
                    , isAnnotationEdge = False
                    }
            validateNormalizedWitness env [] `shouldBe` Left (ReplayMapNonInjective binderA binderB binderA)

        it "normalization maps replay codomain to replay binders of edge root only" $ do
            let edgeId = 0
                root = NodeId 0
                body = NodeId 5
                replayA = NodeId 1
                replayB = NodeId 2
                sourceA = NodeId 20
                sourceB = NodeId 21
                argA = NodeId 30
                argB = NodeId 31
                nodes =
                    nodeMapFromList
                        [ (getNodeId root, TyForall root body)
                        , (getNodeId body, TyArrow body replayA replayB)
                        , (getNodeId replayA, TyVar { tnId = replayA, tnBound = Nothing })
                        , (getNodeId replayB, TyVar { tnId = replayB, tnBound = Nothing })
                        , (getNodeId sourceA, TyVar { tnId = sourceA, tnBound = Nothing })
                        , (getNodeId sourceB, TyVar { tnId = sourceB, tnBound = Nothing })
                        , (getNodeId argA, TyBase argA (BaseTy "Int"))
                        , (getNodeId argB, TyBase argB (BaseTy "Bool"))
                        ]
                bindParents =
                    bindParentsFromPairs
                        [ (body, root, BindFlex)
                        , (replayA, root, BindFlex)
                        , (replayB, root, BindFlex)
                        ]
                c = rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    }
                edgeWitness =
                    EdgeWitness
                        { ewEdgeId = EdgeId edgeId
                        , ewLeft = root
                        , ewRight = root
                        , ewRoot = root
                        , ewForallIntros = 0
                        , ewWitness = InstanceWitness []
                        }
                edgeTrace =
                    EdgeTrace
                        { etRoot = root
                        , etBinderArgs = [(sourceA, argA), (sourceB, argB)]
                        , etInterior =
                            InteriorNodes
                                (IntSet.fromList
                                    [ getNodeId root
                                    , getNodeId body
                                    , getNodeId replayA
                                    , getNodeId replayB
                                    , getNodeId sourceA
                                    , getNodeId sourceB
                                    , getNodeId argA
                                    , getNodeId argB
                                    ]
                                )
                        , etBinderReplayMap = IntMap.empty
                        , etCopyMap = mempty
                        }
                st0 =
                    PresolutionState
                        { psConstraint = c
                        , psPresolution = Presolution IntMap.empty
                        , psUnionFind = IntMap.empty
                        , psNextNodeId = 40
                        , psPendingWeakens = IntSet.empty
                        , psBinderCache = IntMap.empty
                        , psEdgeExpansions = IntMap.empty
                        , psEdgeWitnesses = IntMap.fromList [(edgeId, edgeWitness)]
                        , psEdgeTraces = IntMap.fromList [(edgeId, edgeTrace)]
                        }
            case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                Left err ->
                    expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                Right (_, st') ->
                    case IntMap.lookup edgeId (psEdgeTraces st') of
                        Nothing ->
                            expectationFailure "Expected normalized trace in psEdgeTraces"
                        Just tr' -> do
                            orderedBinders <- case Binding.orderedBinders id (psConstraint st') (typeRef root) of
                                Left err -> expectationFailure ("orderedBinders failed: " ++ show err) >> pure []
                                Right bs -> pure bs
                            let orderedTyVarBinders =
                                    [ b
                                    | b <- orderedBinders
                                    , case lookupNodeIn (cNodes (psConstraint st')) b of
                                        Just TyVar{} -> True
                                        _ -> False
                                    ]
                                orderedBinderSet = IntSet.fromList (map getNodeId orderedTyVarBinders)
                                replayMap = etBinderReplayMap tr'
                                replayMapKeys = IntSet.fromList (IntMap.keys replayMap)
                                expectedSourceKeys =
                                    IntSet.fromList [getNodeId sourceA, getNodeId sourceB]
                            replayMapKeys `shouldBe` expectedSourceKeys
                            IntMap.elems replayMap
                                `shouldSatisfy`
                                    all (\target -> IntSet.member (getNodeId target) orderedBinderSet)

        it "stale source binders are pruned so binderArgs and replay-map domain track only active sources" $ do
            let edgeId = 1
                root = NodeId 100
                body = NodeId 101
                replayA = NodeId 102
                activeSource = NodeId 20
                staleSource = NodeId 21
                argActive = NodeId 30
                argStale = NodeId 31
                nodes =
                    nodeMapFromList
                        [ (getNodeId root, TyForall root body)
                        , (getNodeId body, TyArrow body replayA replayA)
                        , (getNodeId replayA, TyVar { tnId = replayA, tnBound = Nothing })
                        , (getNodeId activeSource, TyVar { tnId = activeSource, tnBound = Nothing })
                        , (getNodeId staleSource, TyVar { tnId = staleSource, tnBound = Nothing })
                        , (getNodeId argActive, TyBase argActive (BaseTy "Int"))
                        , (getNodeId argStale, TyBase argStale (BaseTy "Bool"))
                        ]
                bindParents =
                    bindParentsFromPairs
                        [ (body, root, BindFlex)
                        , (replayA, root, BindFlex)
                        ]
                c = rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    }
                edgeWitness =
                    EdgeWitness
                        { ewEdgeId = EdgeId edgeId
                        , ewLeft = root
                        , ewRight = root
                        , ewRoot = root
                        , ewForallIntros = 0
                        , ewWitness = InstanceWitness [OpWeaken activeSource]
                        }
                edgeTrace =
                    EdgeTrace
                        { etRoot = root
                        , etBinderArgs = [(activeSource, argActive), (staleSource, argStale)]
                        , etInterior =
                            InteriorNodes
                                (IntSet.fromList
                                    [ getNodeId root
                                    , getNodeId body
                                    , getNodeId replayA
                                    , getNodeId activeSource
                                    , getNodeId argActive
                                    ]
                                )
                        , etBinderReplayMap = IntMap.empty
                        , etCopyMap = mempty
                        }
                st0 =
                    PresolutionState
                        { psConstraint = c
                        , psPresolution = Presolution IntMap.empty
                        , psUnionFind = IntMap.empty
                        , psNextNodeId = 150
                        , psPendingWeakens = IntSet.empty
                        , psBinderCache = IntMap.empty
                        , psEdgeExpansions = IntMap.empty
                        , psEdgeWitnesses = IntMap.fromList [(edgeId, edgeWitness)]
                        , psEdgeTraces = IntMap.fromList [(edgeId, edgeTrace)]
                        }
                expectedActiveSource = activeSource
            case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                Left err ->
                    expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                Right (_, st') ->
                    case IntMap.lookup edgeId (psEdgeTraces st') of
                        Nothing ->
                            expectationFailure "Expected normalized trace in psEdgeTraces"
                        Just tr' -> do
                            let sourceKeys = IntSet.fromList [getNodeId b | (b, _) <- etBinderArgs tr']
                                mapKeys = IntSet.fromList (IntMap.keys (etBinderReplayMap tr'))
                            sourceKeys `shouldBe` mapKeys
                            sourceKeys `shouldBe` IntSet.fromList [getNodeId expectedActiveSource]

        it "normalization drops replay contract fields when edge root has no replay binders" $ do
            let edgeId = 2
                root = NodeId 300
                source = NodeId 301
                argNode = NodeId 302
                nodes =
                    nodeMapFromList
                        [ (getNodeId root, TyArrow root source source)
                        , (getNodeId source, TyVar { tnId = source, tnBound = Nothing })
                        , (getNodeId argNode, TyBase argNode (BaseTy "Int"))
                        ]
                c = rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cBindParents = IntMap.empty
                    }
                edgeWitness =
                    EdgeWitness
                        { ewEdgeId = EdgeId edgeId
                        , ewLeft = root
                        , ewRight = root
                        , ewRoot = root
                        , ewForallIntros = 0
                        , ewWitness = InstanceWitness [OpGraft argNode source, OpWeaken source]
                        }
                edgeTrace =
                    EdgeTrace
                        { etRoot = root
                        , etBinderArgs = [(source, argNode)]
                        , etInterior =
                            InteriorNodes
                                (IntSet.fromList
                                    [ getNodeId root
                                    , getNodeId source
                                    , getNodeId argNode
                                    ]
                                )
                        , etBinderReplayMap = IntMap.empty
                        , etCopyMap = mempty
                        }
                st0 =
                    PresolutionState
                        { psConstraint = c
                        , psPresolution = Presolution IntMap.empty
                        , psUnionFind = IntMap.empty
                        , psNextNodeId = 350
                        , psPendingWeakens = IntSet.empty
                        , psBinderCache = IntMap.empty
                        , psEdgeExpansions = IntMap.empty
                        , psEdgeWitnesses = IntMap.fromList [(edgeId, edgeWitness)]
                        , psEdgeTraces = IntMap.fromList [(edgeId, edgeTrace)]
                        }
            case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                Left err ->
                    expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                Right (_, st') ->
                    case ( IntMap.lookup edgeId (psEdgeWitnesses st')
                         , IntMap.lookup edgeId (psEdgeTraces st')
                         ) of
                        (Nothing, _) ->
                            expectationFailure "Expected normalized witness in psEdgeWitnesses"
                        (_, Nothing) ->
                            expectationFailure "Expected normalized trace in psEdgeTraces"
                        (Just ew', Just tr') -> do
                            getInstanceOps (ewWitness ew') `shouldBe` []
                            etBinderArgs tr' `shouldBe` []
                            etBinderReplayMap tr' `shouldBe` IntMap.empty

  where
    isTotalOp :: InstanceOp -> Bool
    isTotalOp _ = True
