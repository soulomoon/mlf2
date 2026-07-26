{-# LANGUAGE DataKinds #-}
module Presolution.WitnessSpec (spec) where

import IdentityTestSupport
import Test.Hspec
import Test.QuickCheck
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness
    ( Expansion(..)
    , ForallSpec(..)
    , InstanceOp(..)
    , ReplayContract(..)
    , getValidatedInstanceOps
    )
import MLF.Constraint.Types.Witness.TestSupport (EdgeWitness(..), InstanceWitness(..))
import MLF.Constraint.Presolution.Witness
    ( EdgeWitnessOp(..)
    , EdgeWitnessPlan(..)
    , EdgeWitnessNonSourceOrigin(..)
    , OmegaNormalizeEnv(..)
    , OmegaNormalizeError(..)
    , assertNoStandaloneGrafts
    , coalesceRaiseMergeWithEnv
    , edgeWitnessPlanFromBinders
    , integrateEdgeWitnessOps
    , integratePhase2Ops
    , normalizeInstanceOpsFull
    , reorderWeakenWithEnv
    , validateNormalizedWitness
    , witnessFromExpansion
    )
import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Presolution
    ( PresolutionError(..)
    , PresolutionResult(..)
    , EdgeTrace(..)
    )
import MLF.Constraint.Presolution.Base
    ( rootRaiseMergeTraceAuthority
    , rootWeakenRaiseMergeTraceAuthority
    )
import MLF.Constraint.Presolution.Construction
    ( rawExpansionConstructionSemanticMetaKeys
    )
import MLF.Constraint.Presolution.TestSupport
    ( PresolutionState(..)
    , EdgeExecutionArtifacts(..)
    , psEdgeExecutionArtifacts
    , psEdgeTraces
    , psEdgeWitnesses
    , CopyMapping(..)
    , emptyExpansionResultMap
    , validateReplayMapTraceContract
    , runPresolutionM
    , certifyAppliedNonRootWeakenReplay
    , normalizeEdgeWitnessesM
    , ProvenancedInstanceOp(..)
    , ProvenancedNode(..)
    , assertNoStandaloneGraftsWithProvenance
    , normalizeInstanceOpsCoreWithProvenance
    , sourceInteriorFromList
    , sourceInteriorFromSet
    , sourceRaiseAuthorityNodesForTest
    , singletonEdgeExecutionArtifactsForTest
    , validateTerminalRootRaiseMergeForTest
    )
import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import qualified MLF.Constraint.Inert as Inert
import qualified MLF.Binding.Tree as Binding
import SpecUtil
    ( bindParentsFromPairs
    , computePresolutionRaw
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

normalizeInstanceOpsForTest
    :: OmegaNormalizeEnv p
    -> [InstanceOp]
    -> Either OmegaNormalizeError [InstanceOp]
normalizeInstanceOpsForTest env ops =
    getValidatedInstanceOps <$> normalizeInstanceOpsFull env ops

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
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 4 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                expansion =
                    ExpCompose
                        (ExpForall (ForallSpec [Nothing] NE.:| []) NE.:| [ExpInstantiate [argId]])

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
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 4 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                -- Instantiate before forall: the suffix must not suppress the
                -- thesis-required weakening.
                expansion =
                    ExpCompose
                        (ExpInstantiate [argId] NE.:| [ExpForall (ForallSpec [Nothing] NE.:| [])])

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
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 4 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
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

        it "tags ExpInstantiate graft arguments as destination and binders as source" $ do
            let binderId = NodeId 2
                argId = NodeId 69
                nodes =
                    nodeMapFromList
                        [ (2, TyVar {tnId = binderId, tnBound = Nothing})
                        , (69, TyVar {tnId = argId, tnBound = Nothing})
                        ]
                constraint =
                    rootedConstraint
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = inferBindParents nodes
                            }
                st0 =
                    PresolutionState
                        constraint
                        (Presolution IntMap.empty)
                        IntMap.empty
                        70
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
            case
                runPresolutionM
                    defaultTraceConfig
                    st0
                    (edgeWitnessPlanFromBinders [binderId] (ExpInstantiate [argId]))
                of
                    Left err -> expectationFailure ("edgeWitnessPlanFromBinders failed: " ++ show err)
                    Right (plan, _) ->
                        ewpBaseOps plan
                            `shouldBe`
                                [ DestinationSourceEdgeWitnessGraft argId binderId
                                , SourceEdgeWitnessOp (OpWeaken binderId)
                                ]

        it "rejects surplus ExpInstantiate arguments instead of truncating them" $ do
            let expNodeId = NodeId 0
                forallId = NodeId 1
                binderId = NodeId 2
                firstArgId = NodeId 3
                surplusArgId = NodeId 4
                nodes = nodeMapFromList
                    [ (0, TyExp expNodeId (ExpVarId 0) forallId)
                    , (1, TyForall forallId binderId)
                    , (2, TyVar { tnId = binderId, tnBound = Nothing })
                    , (3, TyVar { tnId = firstArgId, tnBound = Nothing })
                    , (4, TyVar { tnId = surplusArgId, tnBound = Nothing })
                    ]
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 5 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                expansion = ExpInstantiate [firstArgId, surplusArgId]

            case runPresolutionM defaultTraceConfig st0 (witnessFromExpansion (GenNodeId 0) expNodeId (nodeAt nodes 0) expansion) of
                Left (ArityMismatch mismatchContext expected actual) -> do
                    mismatchContext `shouldBe` "witnessFromExpansion/ExpInstantiate"
                    expected `shouldBe` 1
                    actual `shouldBe` 2
                Left err -> expectationFailure ("Expected exact-arity failure, got " ++ show err)
                Right _ -> expectationFailure "Expected surplus ExpInstantiate argument to be rejected"

        it "rejects ExpInstantiate arguments when the source has no binders" $ do
            let expNodeId = NodeId 0
                bodyId = NodeId 1
                argumentId = NodeId 2
                nodes = nodeMapFromList
                    [ (0, TyExp expNodeId (ExpVarId 0) bodyId)
                    , (1, TyVar { tnId = bodyId, tnBound = Nothing })
                    , (2, TyVar { tnId = argumentId, tnBound = Nothing })
                    ]
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 3 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                expansion = ExpInstantiate [argumentId]

            case runPresolutionM defaultTraceConfig st0 (witnessFromExpansion (GenNodeId 0) expNodeId (nodeAt nodes 0) expansion) of
                Left (ArityMismatch mismatchContext expected actual) -> do
                    mismatchContext `shouldBe` "witnessFromExpansion/ExpInstantiate"
                    expected `shouldBe` 0
                    actual `shouldBe` 1
                Left err -> expectationFailure ("Expected zero-binder exact-arity failure, got " ++ show err)
                Right _ -> expectationFailure "Expected zero-binder ExpInstantiate argument to be rejected"

        it "annotation edges preserve OpWeaken in witness (thesis-exact)" $ do
            -- Annotation edges previously had all OpWeaken stripped via dropWeakenOps
            -- during per-edge witness assembly. After eliminating
            -- DEV-PHI-WITNESS-WEAKEN-SUPPRESSION, witnessFromExpansion emits
            -- OpWeaken unconditionally, and the surviving witness assembly path
            -- no longer strips them. Verify the underlying emission.
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
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 4 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
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
                    , (4, TestTyBase boundId (BaseTy "Int"))
                    ]
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 5 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
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
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 2 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                expansion = ExpForall (ForallSpec [Nothing, Nothing] NE.:| [])

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

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult
                    { prEdgeExpansions = exps
                    , prEdgeWitnesses = ews
                    } -> do
                    case IntMap.lookup edgeId exps of
                        Just (ExpInstantiate _) -> pure ()
                        Just other -> expectationFailure $ "Expected ExpInstantiate, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for Edge 0"
                    case IntMap.lookup edgeId ews of
                        Just ew -> ewForallIntros ew `shouldBe` 0
                        Nothing -> expectationFailure "No witness found for Edge 0"

    describe "Phase 3 — Witness normalization" $ do
        describe "operand provenance" $ do
            let provenanceEnv root destinationInterior sourceInterior =
                    (mkNormalizeEnv mkNormalizeConstraint root destinationInterior)
                        { interiorRaw = sourceInterior }

            it "keeps source-distinct raises when their destinations coincide" $ do
                let root = NodeId 0
                    target = NodeId 2
                    env =
                        provenanceEnv
                            root
                            (IntSet.singleton (getNodeId target))
                            (IntSet.fromList [20, 21])
                    tracked source =
                        ProvenancedRaise
                            (ProvenancedNode target (IntSet.singleton source))
                    expected =
                        [ tracked 20
                        , tracked 21
                        ]
                normalizeInstanceOpsCoreWithProvenance
                    env
                    [tracked 20, tracked 21]
                    `shouldBe` Right expected

            it "drops a duplicate raise only when source provenance agrees" $ do
                let root = NodeId 0
                    target = NodeId 2
                    env =
                        provenanceEnv
                            root
                            (IntSet.singleton (getNodeId target))
                            (IntSet.singleton 20)
                    tracked =
                        ProvenancedRaise
                            (ProvenancedNode target (IntSet.singleton 20))
                normalizeInstanceOpsCoreWithProvenance env [tracked, tracked]
                    `shouldBe` Right [tracked]

            it "combines Raise and Merge operand provenance when forming RaiseMerge" $ do
                let root = NodeId 0
                    operated = NodeId 2
                    other = NodeId 99
                    env =
                        provenanceEnv
                            root
                            (IntSet.singleton (getNodeId operated))
                            (IntSet.singleton 20)
                    raiseNode =
                        ProvenancedNode operated (IntSet.singleton 20)
                    mergeNode =
                        ProvenancedNode operated (IntSet.singleton 20)
                    otherNode =
                        ProvenancedNode other (IntSet.singleton 30)
                    expected =
                        [ ProvenancedRaiseMerge
                            (ProvenancedNode operated (IntSet.singleton 20))
                            otherNode
                        ]
                normalizeInstanceOpsCoreWithProvenance
                    env
                    [ ProvenancedRaise raiseNode
                    , ProvenancedMerge mergeNode otherNode
                    ]
                    `shouldBe` Right expected

            it "does not fold a different-source Raise into RaiseMerge" $ do
                let root = NodeId 0
                    operated = NodeId 2
                    other = NodeId 99
                    env =
                        provenanceEnv
                            root
                            (IntSet.singleton (getNodeId operated))
                            (IntSet.fromList [20, 21])
                    source20 =
                        ProvenancedNode operated (IntSet.singleton 20)
                    source21 =
                        ProvenancedNode operated (IntSet.singleton 21)
                    external =
                        ProvenancedNode other (IntSet.singleton 30)
                normalizeInstanceOpsCoreWithProvenance
                    env
                    [ ProvenancedRaise source20
                    , ProvenancedRaise source21
                    , ProvenancedMerge source21 external
                    ]
                    `shouldBe`
                        Right
                            [ ProvenancedRaise source20
                            , ProvenancedRaiseMerge source21 external
                            ]

            it "keeps operand provenance attached when delayed Weaken is reordered" $ do
                let root = NodeId 0
                    child = NodeId 2
                    arg = NodeId 3
                    env =
                        provenanceEnv
                            root
                            (IntSet.fromList [getNodeId root, getNodeId child])
                            (IntSet.fromList [40, 42])
                    weaken =
                        ProvenancedWeaken
                            (ProvenancedNode root (IntSet.singleton 40))
                    graft =
                        ProvenancedGraft
                            (ProvenancedNode arg (IntSet.singleton 41))
                            (ProvenancedNode child (IntSet.singleton 42))
                normalizeInstanceOpsCoreWithProvenance env [weaken, graft]
                    `shouldBe` Right [graft, weaken]

            it "preserves a root Weaken before its same-source RaiseMerge transition" $ do
                let root = NodeId 0
                    destinationExterior = NodeId 2
                    sourceExterior = NodeId 99
                    env =
                        provenanceEnv
                            root
                            (IntSet.singleton (getNodeId root))
                            (IntSet.singleton (getNodeId root))
                    rootOperand =
                        ProvenancedNode root (IntSet.singleton (getNodeId root))
                    exteriorOperand =
                        ProvenancedNode
                            destinationExterior
                            (IntSet.singleton (getNodeId sourceExterior))
                normalizeInstanceOpsCoreWithProvenance
                    env
                    [ ProvenancedWeaken rootOperand
                    , ProvenancedRaise rootOperand
                    , ProvenancedMerge rootOperand exteriorOperand
                    ]
                    `shouldBe`
                        Right
                            [ ProvenancedWeaken rootOperand
                            , ProvenancedRaiseMerge rootOperand exteriorOperand
                            ]

            it "does not pair Graft and Weaken from different source binders" $ do
                let root = NodeId 0
                    binder = NodeId 2
                    arg = NodeId 3
                    env =
                        provenanceEnv
                            root
                            (IntSet.fromList [getNodeId root, getNodeId binder])
                            (IntSet.fromList [10, 20, 21])
                    graft =
                        ProvenancedGraft
                            (ProvenancedNode arg (IntSet.singleton 30))
                            (ProvenancedNode binder (IntSet.singleton 20))
                    middle =
                        ProvenancedRaise
                            (ProvenancedNode root (IntSet.singleton 10))
                    weaken =
                        ProvenancedWeaken
                            (ProvenancedNode binder (IntSet.singleton 21))
                normalizeInstanceOpsCoreWithProvenance env [graft, middle, weaken]
                    `shouldBe` Right [graft, middle, weaken]
                assertNoStandaloneGraftsWithProvenance env [graft, weaken]
                    `shouldBe` Left (StandaloneGraftRemaining binder)

            it "groups Graft-Weaken ambiguity by frozen source binder" $ do
                let root = NodeId 0
                    binder = NodeId 2
                    env =
                        provenanceEnv
                            root
                            (IntSet.fromList [getNodeId root, getNodeId binder])
                            (IntSet.fromList [20, 21])
                    binderFrom source =
                        ProvenancedNode binder (IntSet.singleton source)
                    graft source arg =
                        ProvenancedGraft
                            (ProvenancedNode arg (IntSet.singleton (getNodeId arg)))
                            (binderFrom source)
                    weaken source = ProvenancedWeaken (binderFrom source)
                    ops =
                        [ graft 20 (NodeId 3)
                        , weaken 20
                        , graft 21 (NodeId 1)
                        , weaken 21
                        ]
                normalizeInstanceOpsCoreWithProvenance env ops
                    `shouldBe` Right ops

            it "keeps a source-distinct RaiseMerge despite a shared destination" $ do
                let root = NodeId 0
                    target = NodeId 2
                    env =
                        provenanceEnv
                            root
                            (IntSet.singleton (getNodeId target))
                            (IntSet.fromList [50, 51])
                    operated =
                        ProvenancedNode target (IntSet.singleton 50)
                    other =
                        ProvenancedNode target (IntSet.singleton 51)
                normalizeInstanceOpsCoreWithProvenance
                    env
                    [ProvenancedRaiseMerge operated other]
                    `shouldBe` Right [ProvenancedRaiseMerge operated other]

            it "reduces a true source-identical RaiseMerge self-merge" $ do
                let root = NodeId 0
                    target = NodeId 2
                    env =
                        provenanceEnv
                            root
                            (IntSet.singleton (getNodeId target))
                            (IntSet.singleton 50)
                    operated =
                        ProvenancedNode target (IntSet.singleton 50)
                normalizeInstanceOpsCoreWithProvenance
                    env
                    [ProvenancedRaiseMerge operated operated]
                    `shouldBe` Right [ProvenancedRaise operated]

        describe "frozen-source normalization boundary" $ do
            it "normalizes against I(etResultRoot) while preserving source witness ids" $ do
                let edgeId = 40
                    sourceRoot = NodeId 10
                    sourceTarget = NodeId 11
                    resultRoot = NodeId 20
                    resultTarget = NodeId 21
                    resultOther = NodeId 22
                    nodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyForall sourceRoot sourceTarget)
                            , (getNodeId sourceTarget, TyVar sourceTarget Nothing)
                            , (getNodeId resultRoot, TyArrow resultRoot resultTarget resultOther)
                            , (getNodeId resultTarget, TyVar resultTarget Nothing)
                            , (getNodeId resultOther, TyVar resultOther Nothing)
                            ]
                    c =
                        constraintWithOwners
                            nodes
                            [ (GenNodeId 10, [sourceRoot])
                            , (GenNodeId 20, [resultRoot])
                            ]
                            [ (sourceTarget, sourceRoot, BindFlex)
                            , (resultTarget, resultRoot, BindFlex)
                            , (resultOther, resultRoot, BindFlex)
                            ]
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = resultRoot
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness = InstanceWitness [OpRaise sourceTarget]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = resultRoot
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromList [sourceRoot, sourceTarget]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap =
                                CopyMapping
                                    (IntMap.singleton (getNodeId sourceTarget) resultTarget)
                            , etReplayContract = ReplayContractNone
                            }
                    st0 =
                        mkWitnessNormState
                            c
                            IntMap.empty
                            30
                            edgeId
                            edgeWitness
                            edgeTrace
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left err ->
                        expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                    Right (_, st') ->
                        case IntMap.lookup edgeId (psEdgeWitnesses st') of
                            Nothing -> expectationFailure "missing normalized witness"
                            Just normalizedWitness ->
                                getInstanceOps (ewWitness normalizedWitness)
                                    `shouldBe` [OpRaise sourceTarget]

            it "retains source authority but rejects a projection outside the destination order domain" $ do
                let edgeId = 41
                    sourceRoot = NodeId 30
                    sourceTarget = NodeId 31
                    sourceOther = NodeId 32
                    resultRoot = NodeId 40
                    resultTarget = NodeId 41
                    resultOtherRoot = NodeId 42
                    resultOther = NodeId 43
                    nodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyArrow sourceRoot sourceTarget sourceOther)
                            , (getNodeId sourceTarget, TyVar sourceTarget Nothing)
                            , (getNodeId sourceOther, TyVar sourceOther Nothing)
                            , (getNodeId resultRoot, TyForall resultRoot resultTarget)
                            , (getNodeId resultTarget, TyVar resultTarget Nothing)
                            , (getNodeId resultOtherRoot, TyForall resultOtherRoot resultOther)
                            , (getNodeId resultOther, TyVar resultOther Nothing)
                            ]
                    c =
                        constraintWithOwners
                            nodes
                            [ (GenNodeId 30, [sourceRoot])
                            , (GenNodeId 40, [resultRoot])
                            , (GenNodeId 42, [resultOtherRoot])
                            ]
                            [ (sourceTarget, sourceRoot, BindFlex)
                            , (sourceOther, sourceRoot, BindFlex)
                            , (resultTarget, resultRoot, BindFlex)
                            , (resultOther, resultOtherRoot, BindFlex)
                            ]
                    rawOp = OpMerge sourceTarget sourceOther
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = resultRoot
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness = InstanceWitness [rawOp]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = resultRoot
                            , etBinderArgs = []
                            , etInterior =
                                sourceInteriorFromList [sourceRoot, sourceTarget, sourceOther]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap =
                                CopyMapping
                                    ( IntMap.fromList
                                        [ (getNodeId sourceTarget, resultTarget)
                                        , (getNodeId sourceOther, resultOther)
                                        ]
                                    )
                            , etReplayContract = ReplayContractNone
                            }
                    st0 =
                        mkWitnessNormState
                            c
                            IntMap.empty
                            50
                            edgeId
                            edgeWitness
                            edgeTrace
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left
                        ( WitnessNormalizationError
                            (EdgeId actualEdge)
                            (MissingOrderKey actualOther)
                          ) -> do
                            actualEdge `shouldBe` edgeId
                            actualOther `shouldBe` resultOther
                    Left err ->
                        expectationFailure ("expected destination-order rejection, got: " ++ show err)
                    Right _ ->
                        expectationFailure "expected destination-order rejection"

        describe "many-to-one copy provenance" $ do
            it "restores the uniquely operated source instead of choosing a numeric inverse" $ do
                let edgeId = 42
                    sourceRoot = NodeId 60
                    sourceA = NodeId 61
                    sourceB = NodeId 62
                    copyA = NodeId 70
                    copyB = NodeId 71
                    destination = NodeId 72
                    resultRoot = NodeId 73
                    nodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyArrow sourceRoot sourceA sourceB)
                            , (getNodeId sourceA, TyVar sourceA Nothing)
                            , (getNodeId sourceB, TyVar sourceB Nothing)
                            , (getNodeId resultRoot, TyForall resultRoot destination)
                            , (getNodeId destination, TyVar destination Nothing)
                            ]
                    c =
                        rootedConstraint emptyConstraint
                            { cNodes = nodes
                            , cBindParents =
                                bindParentsFromPairs
                                    [ (sourceA, sourceRoot, BindFlex)
                                    , (sourceB, sourceRoot, BindFlex)
                                    , (destination, resultRoot, BindFlex)
                                    ]
                            }
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = resultRoot
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness = InstanceWitness [OpRaise sourceB]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = resultRoot
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromList [sourceRoot, sourceA, sourceB]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap =
                                CopyMapping
                                    ( IntMap.fromList
                                        [ (getNodeId sourceA, copyA)
                                        , (getNodeId sourceB, copyB)
                                        ]
                                    )
                            , etReplayContract = ReplayContractNone
                            }
                    uf =
                        IntMap.fromList
                            [ (getNodeId copyA, destination)
                            , (getNodeId copyB, destination)
                            ]
                    st0 =
                        mkWitnessNormState c uf 80 edgeId edgeWitness edgeTrace
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left err ->
                        expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                    Right (_, st') ->
                        case IntMap.lookup edgeId (psEdgeWitnesses st') of
                            Nothing -> expectationFailure "missing normalized witness"
                            Just normalizedWitness -> do
                                let ops = getInstanceOps (ewWitness normalizedWitness)
                                ops `shouldBe` [OpRaise sourceB]
                                concatMap opNodeIds ops
                                    `shouldSatisfy` all (`notElem` [copyA, copyB, destination])

            it "does not let final UF aliases collapse frozen source operations" $ do
                let edgeId = 43
                    sourceRoot = NodeId 80
                    sourceA = NodeId 81
                    sourceAlias = NodeId 82
                    sharedCopy = NodeId 90
                    destination = NodeId 92
                    resultRoot = NodeId 93
                    nodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyArrow sourceRoot sourceA sourceAlias)
                            , (getNodeId sourceA, TyVar sourceA Nothing)
                            , (getNodeId sourceAlias, TyVar sourceAlias Nothing)
                            , (getNodeId resultRoot, TyForall resultRoot destination)
                            , (getNodeId destination, TyVar destination Nothing)
                            ]
                    c =
                        rootedConstraint emptyConstraint
                            { cNodes = nodes
                            , cBindParents =
                                bindParentsFromPairs
                                    [ (sourceA, sourceRoot, BindFlex)
                                    , (sourceAlias, sourceRoot, BindFlex)
                                    , (destination, resultRoot, BindFlex)
                                    ]
                            }
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = resultRoot
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness =
                                InstanceWitness
                                    [ OpRaise sourceA
                                    , OpRaise sourceAlias
                                    ]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = resultRoot
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromList [sourceRoot, sourceA, sourceAlias]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap =
                                CopyMapping
                                    ( IntMap.fromList
                                        [ (getNodeId sourceA, sharedCopy)
                                        , (getNodeId sourceAlias, sharedCopy)
                                        ]
                                    )
                            , etReplayContract = ReplayContractNone
                            }
                    uf =
                        IntMap.fromList
                            [ (getNodeId sourceAlias, sourceA)
                            , (getNodeId sharedCopy, destination)
                            ]
                    st0 =
                        mkWitnessNormState c uf 100 edgeId edgeWitness edgeTrace
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left err ->
                        expectationFailure ("canonical source aliases were treated as ambiguous: " ++ show err)
                    Right (_, st') ->
                        case IntMap.lookup edgeId (psEdgeWitnesses st') of
                            Nothing -> expectationFailure "missing normalized witness"
                            Just normalizedWitness ->
                                getInstanceOps (ewWitness normalizedWitness)
                                    `shouldBe` [OpRaise sourceA, OpRaise sourceAlias]

            it "keeps two explicit source operations after destination coalescing" $ do
                let edgeId = 43
                    sourceRoot = NodeId 80
                    sourceA = NodeId 81
                    sourceB = NodeId 82
                    copyA = NodeId 90
                    copyB = NodeId 91
                    destination = NodeId 92
                    resultRoot = NodeId 93
                    nodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyArrow sourceRoot sourceA sourceB)
                            , (getNodeId sourceA, TyVar sourceA Nothing)
                            , (getNodeId sourceB, TyVar sourceB Nothing)
                            , (getNodeId resultRoot, TyForall resultRoot destination)
                            , (getNodeId destination, TyVar destination Nothing)
                            ]
                    c =
                        rootedConstraint emptyConstraint
                            { cNodes = nodes
                            , cBindParents =
                                bindParentsFromPairs
                                    [ (sourceA, sourceRoot, BindFlex)
                                    , (sourceB, sourceRoot, BindFlex)
                                    , (destination, resultRoot, BindFlex)
                                    ]
                            }
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = resultRoot
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness =
                                InstanceWitness
                                    [ OpRaise sourceA
                                    , OpRaise sourceB
                                    ]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = resultRoot
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromList [sourceRoot, sourceA, sourceB]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap =
                                CopyMapping
                                    ( IntMap.fromList
                                        [ (getNodeId sourceA, copyA)
                                        , (getNodeId sourceB, copyB)
                                        ]
                                    )
                            , etReplayContract = ReplayContractNone
                            }
                    uf =
                        IntMap.fromList
                            [ (getNodeId copyA, destination)
                            , (getNodeId copyB, destination)
                            ]
                    st0 =
                        mkWitnessNormState c uf 100 edgeId edgeWitness edgeTrace
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left err ->
                        expectationFailure ("explicit source operations became ambiguous: " ++ show err)
                    Right (_, st') ->
                        case IntMap.lookup edgeId (psEdgeWitnesses st') of
                            Nothing -> expectationFailure "missing normalized witness"
                            Just normalizedWitness ->
                                getInstanceOps (ewWitness normalizedWitness)
                                    `shouldBe` [OpRaise sourceA, OpRaise sourceB]

            it "does not share one source Raise certificate across a coalesced destination" $ do
                let edgeId = 45
                    sourceRoot = NodeId 120
                    sourceA = NodeId 121
                    sourceB = NodeId 122
                    copyA = NodeId 130
                    copyB = NodeId 131
                    destination = NodeId 132
                    resultAncestor = NodeId 133
                    resultRoot = NodeId 134
                    nodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyArrow sourceRoot sourceA sourceB)
                            , (getNodeId sourceA, TyVar sourceA Nothing)
                            , (getNodeId sourceB, TyVar sourceB Nothing)
                            , (getNodeId resultRoot, TyForall resultRoot resultAncestor)
                            , (getNodeId resultAncestor, TyForall resultAncestor destination)
                            , (getNodeId destination, TyVar destination Nothing)
                            ]
                    c =
                        rootedConstraint emptyConstraint
                            { cNodes = nodes
                            , cBindParents =
                                bindParentsFromPairs
                                    [ (sourceA, sourceRoot, BindFlex)
                                    , (sourceB, sourceRoot, BindFlex)
                                    , (resultAncestor, resultRoot, BindRigid)
                                    , (destination, resultAncestor, BindFlex)
                                    ]
                            }
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = resultRoot
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness =
                                InstanceWitness
                                    [ OpRaise sourceA
                                    , OpRaise sourceB
                                    ]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = resultRoot
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromList [sourceRoot, sourceA, sourceB]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap =
                                CopyMapping
                                    ( IntMap.fromList
                                        [ (getNodeId sourceA, copyA)
                                        , (getNodeId sourceB, copyB)
                                        ]
                                    )
                            , etReplayContract = ReplayContractNone
                            }
                    uf =
                        IntMap.fromList
                            [ (getNodeId copyA, destination)
                            , (getNodeId copyB, destination)
                            ]
                    st0 =
                        setEdgeRaiseAuthority
                            edgeId
                            (IntSet.singleton (getNodeId sourceA))
                            (mkWitnessNormState c uf 140 edgeId edgeWitness edgeTrace)
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left
                        ( WitnessNormalizationError
                            (EdgeId actualEdge)
                            (NotTransitivelyFlexBound _ operated validationRoot)
                          ) -> do
                            actualEdge `shouldBe` edgeId
                            operated `shouldBe` destination
                            validationRoot `shouldBe` resultRoot
                    Left err ->
                        expectationFailure
                            ("expected coalesced Raise authority rejection, got: " ++ show err)
                    Right _ ->
                        expectationFailure
                            "one source certificate authorized an unrelated coalesced Raise"

            it "distinguishes exact-copy provenance from true non-operated ambiguity" $ do
                let edgeId = 44
                    sourceRoot = NodeId 100
                    operatedSource = NodeId 101
                    otherSourceA = NodeId 102
                    otherSourceB = NodeId 103
                    operatedCopy = NodeId 110
                    otherCopyA = NodeId 111
                    otherCopyB = NodeId 112
                    operatedDestination = NodeId 113
                    otherDestination = NodeId 114
                    resultRoot = NodeId 115
                    nodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyArrow sourceRoot operatedSource otherSourceA)
                            , (getNodeId operatedSource, TyVar operatedSource Nothing)
                            , (getNodeId otherSourceA, TyVar otherSourceA Nothing)
                            , (getNodeId otherSourceB, TyVar otherSourceB Nothing)
                            , (getNodeId resultRoot, TyForall resultRoot operatedDestination)
                            , (getNodeId operatedDestination, TyVar operatedDestination Nothing)
                            , (getNodeId otherDestination, TyVar otherDestination Nothing)
                            ]
                    c =
                        -- Keep the many-to-one destination outside the result
                        -- owner's I(r).  `rootedConstraint` would put every
                        -- term-DAG root under one gen node and would therefore
                        -- make `otherDestination` an interior sibling.
                        constraintWithOwners
                            nodes
                            [ (GenNodeId 100, [sourceRoot])
                            , (GenNodeId 114, [otherDestination])
                            , (GenNodeId 115, [resultRoot])
                            ]
                            [ (operatedSource, sourceRoot, BindFlex)
                            , (otherSourceA, sourceRoot, BindFlex)
                            , (otherSourceB, sourceRoot, BindFlex)
                            , (operatedDestination, resultRoot, BindFlex)
                            ]
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = resultRoot
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness =
                                InstanceWitness
                                    [ OpRaise operatedSource
                                    , OpMerge operatedSource otherCopyA
                                    ]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = resultRoot
                            , etBinderArgs = [(operatedSource, operatedCopy)]
                            , etInterior =
                                sourceInteriorFromList
                                    [ sourceRoot
                                    , operatedSource
                                    , otherSourceA
                                    , otherSourceB
                                    ]
                            , etBinderReplayMap =
                                IntMap.singleton
                                    (getNodeId operatedSource)
                                    operatedDestination
                            , etReplayDomainBinders = [operatedDestination]
                            , etCopyMap =
                                CopyMapping
                                    ( IntMap.fromList
                                        [ (getNodeId operatedSource, operatedCopy)
                                        , (getNodeId otherSourceA, otherCopyA)
                                        , (getNodeId otherSourceB, otherCopyB)
                                        ]
                                    )
                            , etReplayContract = ReplayContractStrict
                            }
                    uf =
                        IntMap.fromList
                            [ (getNodeId operatedCopy, operatedDestination)
                            , (getNodeId otherCopyA, otherDestination)
                            , (getNodeId otherCopyB, otherDestination)
                            ]
                    st0 =
                        setEdgeNonSourceOpOrigins
                            edgeId
                            (IntMap.singleton 1 SourceDestinationMergeOperation)
                            (mkWitnessNormState c uf 120 edgeId edgeWitness edgeTrace)
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left err ->
                        expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                    Right (_, st') ->
                        case IntMap.lookup edgeId (psEdgeWitnesses st') of
                            Nothing -> expectationFailure "missing normalized witness"
                            Just normalizedWitness -> do
                                let ops = getInstanceOps (ewWitness normalizedWitness)
                                ops
                                    `shouldBe` [OpRaiseMerge operatedSource otherSourceA]
                                concatMap opNodeIds ops
                                    `shouldSatisfy`
                                        all
                                            (`notElem`
                                                [ operatedCopy
                                                , otherCopyA
                                                , otherCopyB
                                                , operatedDestination
                                                , otherDestination
                                                ]
                                            )
                let ambiguousWitness =
                        edgeWitness
                            { ewWitness =
                                InstanceWitness
                                    [ OpRaise operatedSource
                                    , OpMerge operatedSource otherDestination
                                    ]
                            }
                    ambiguousState =
                        setEdgeNonSourceOpOrigins
                            edgeId
                            (IntMap.singleton 1 SourceDestinationMergeOperation)
                            ( mkWitnessNormState
                                c
                                uf
                                120
                                edgeId
                                ambiguousWitness
                                edgeTrace
                            )
                case runPresolutionM defaultTraceConfig ambiguousState normalizeEdgeWitnessesM of
                    Left
                        ( WitnessNormalizationError
                            (EdgeId actualEdge)
                            (AmbiguousOperatedSource actualDestination sources)
                          ) -> do
                            actualEdge `shouldBe` edgeId
                            actualDestination `shouldBe` otherDestination
                            sources `shouldBe` [otherSourceA, otherSourceB]
                    Left err ->
                        expectationFailure ("expected non-operated ambiguity, got: " ++ show err)
                    Right _ ->
                        expectationFailure "expected non-operated ambiguity"

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
            normalizeInstanceOpsForTest env ops0 `shouldBe` Right [OpGraft arg child, OpWeaken root]

        it "keeps the strict root Weaken before its RaiseMerge transition" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                exterior = NodeId 2
                env =
                    (mkNormalizeEnv c root (IntSet.singleton (getNodeId root)))
                        { binderArgs = IntMap.singleton (getNodeId root) (NodeId 3)
                        , binderReplayMap = IntMap.singleton (getNodeId root) exterior
                        , replayContract = ReplayContractStrict
                        , replayDomainBinders = [exterior]
                        }
                ops0 = [OpWeaken root, OpRaise root, OpMerge root exterior]
            normalizeInstanceOpsForTest env ops0
                `shouldBe` Right [OpWeaken root, OpRaiseMerge root exterior]

        it "does not move Weaken past same-binder ops without descendants" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                n = NodeId 2
                arg = NodeId 10
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                ops0 = [OpWeaken n, OpGraft arg n]
            normalizeInstanceOpsForTest env ops0 `shouldBe` Right [OpWeaken n, OpGraft arg n]

        describe "graft-weaken canonical alignment (H16 upstream target)" $ do
            it "coalesces delayed graft-weaken pairs when middle ops are binder-disjoint" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    binder = NodeId 1
                    arg = NodeId 2
                    (n1, n2) = orderedPairByPrec c root
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId binder, getNodeId n1, getNodeId n2])
                    ops0 = [OpGraft arg binder, OpMerge n2 n1, OpWeaken binder]
                normalizeInstanceOpsForTest env ops0
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
                normalizeInstanceOpsForTest env ops0
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
                            , replayContract = ReplayContractStrict
                            }
                    ops0 = [OpGraft (NodeId 30) (NodeId 20), OpWeaken (NodeId 20)]
                normalizeInstanceOpsForTest env ops0
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
                            , replayContract = ReplayContractStrict
                            }
                    ops0 =
                        [ OpGraft (NodeId 30) (NodeId 20)
                        , OpWeaken (NodeId 20)
                        , OpGraft (NodeId 31) (NodeId 21)
                        , OpWeaken (NodeId 21)
                        ]
                    isLeftResult = either (const True) (const False)
                normalizeInstanceOpsForTest env ops0 `shouldSatisfy` isLeftResult

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
                            , replayContract = ReplayContractStrict
                            }
                    ops0 =
                        [ OpGraft (NodeId 30) (NodeId 20)
                        , OpWeaken (NodeId 20)
                        , OpGraft (NodeId 30) (NodeId 20)
                        , OpWeaken (NodeId 20)
                        ]
                case normalizeInstanceOpsForTest env ops0 of
                    Left err -> expectationFailure ("Expected normalization success, got: " ++ show err)
                    Right ops1 -> normalizeInstanceOpsForTest env ops1 `shouldBe` Right ops1

        it "does not drop Graft/Weaken when a binder is eliminated by Merge" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                a = NodeId 2
                b = NodeId 3
                arg = NodeId 10
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId a, getNodeId b])
                ops0 = [OpGraft arg b, OpWeaken b, OpMerge b a]
            normalizeInstanceOpsForTest env ops0 `shouldBe` Right ops0

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
            normalizeInstanceOpsForTest env ops0 `shouldBe` Right []

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
            normalizeInstanceOpsForTest env ops0 `shouldBe` Right ops0

        it "normalizes omega ops within a segment" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                child = NodeId 2
                arg = NodeId 10
                arg2 = NodeId 11
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId child])
                seg1 = [OpWeaken root, OpGraft arg child]
                seg2 = [OpGraft arg2 root]
            normalizeInstanceOpsForTest env seg1
                `shouldBe` Right [OpGraft arg child, OpWeaken root]
            normalizeInstanceOpsForTest env seg2
                `shouldBe` Right [OpGraft arg2 root]

        it "O15-TR-NODE-MERGE R-MERGE-NORM-09: normalizeInstanceOpsFull rejects wrong merge direction" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                (mLess, nGreater) = orderedPairByPrec c root
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId mLess, getNodeId nGreater])
                ops0 = [OpMerge mLess nGreater]
            normalizeInstanceOpsForTest env ops0 `shouldBe` Left (MergeDirectionInvalid mLess nGreater)

        it "preserves Graft/Weaken when a later Merge eliminates the binder during emission" $ do
            let a = NodeId 2
                b = NodeId 3
                arg = NodeId 10
                baseOps = [OpGraft arg b, OpWeaken b]
                extraOps = [OpMerge b a]
            integratePhase2Ops baseOps extraOps
                `shouldBe` [OpGraft arg b, OpMerge b a, OpWeaken b]

        it "keeps destination origins aligned when integration reorders mixed operations" $ do
            let a = NodeId 2
                b = NodeId 3
                arg = NodeId 10
                baseOps = [OpGraft arg b, OpWeaken b]
                extraOps =
                    [ DestinationEdgeWitnessOp (OpMerge b a)
                    , DestinationEdgeWitnessOp (OpRaise b)
                    ]
            integrateEdgeWitnessOps a baseOps extraOps
                `shouldBe`
                    ( [OpGraft arg b, OpRaise b, OpMerge b a, OpWeaken b]
                    , IntMap.fromList
                        [ (1, DestinationEdgeOperation)
                        , (2, DestinationEdgeOperation)
                        ]
                    )

        it "preserves flexible terminal construction certificates through integration" $ do
            let operated = NodeId 2
                exterior = NodeId 3
                extraOps =
                    [ FlexibleTerminalSourceEdgeWitnessOp (OpRaise operated)
                    , FlexibleTerminalSourceEdgeWitnessOp (OpMerge operated exterior)
                    ]
            integrateEdgeWitnessOps operated [] extraOps
                `shouldBe`
                    ( [OpRaise operated, OpMerge operated exterior]
                    , IntMap.fromList
                        [ (0, FlexibleTerminalSourceOperation)
                        , (1, FlexibleTerminalSourceOperation)
                        ]
                    )

        it "keeps a rigid terminal root Weaken before its RaiseMerge block" $ do
            let root = NodeId 2
                exterior = NodeId 3
                baseOps = [OpWeaken root]
                extraOps =
                    [ DestinationEdgeWitnessOp (OpMerge root exterior)
                    , DestinationEdgeWitnessOp (OpRaise root)
                    ]
            integrateEdgeWitnessOps root baseOps extraOps
                `shouldBe`
                    ( [OpWeaken root, OpRaise root, OpMerge root exterior]
                    , IntMap.fromList
                        [ (1, DestinationEdgeOperation)
                        , (2, DestinationEdgeOperation)
                        ]
                    )

        it "keeps an instantiated source-root Weaken adjacent to its terminal RaiseMerge block" $ do
            let root = NodeId 2
                exterior = NodeId 3
                argument = NodeId 10
                baseOps =
                    [ OpGraft argument root
                    , OpWeaken root
                    ]
                extraOps =
                    [ SourceEdgeWitnessOp (OpMerge root exterior)
                    , SourceEdgeWitnessOp (OpRaise root)
                    ]
            integrateEdgeWitnessOps root baseOps extraOps
                `shouldBe`
                    ( [ OpGraft argument root
                      , OpWeaken root
                      , OpRaise root
                      , OpMerge root exterior
                      ]
                    , IntMap.empty
                    )

        it "keeps an execution-emitted root Weaken before its RaiseMerge block" $ do
            let root = NodeId 2
                exterior = NodeId 3
                extraOps =
                    [ SourceEdgeWitnessOp (OpMerge root exterior)
                    , SourceEdgeWitnessOp (OpRaise root)
                    , SourceEdgeWitnessOp (OpWeaken root)
                    ]
            integrateEdgeWitnessOps root [] extraOps
                `shouldBe`
                    ( [OpWeaken root, OpRaise root, OpMerge root exterior]
                    , IntMap.empty
                    )

        it "coalesces Raise; Merge into RaiseMerge" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                n = NodeId 2
                m = NodeId 3
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
            normalizeInstanceOpsForTest env [OpRaise n, OpMerge n m] `shouldBe` Right [OpRaiseMerge n m]

        it "coalesces multiple Raises followed by Merge into RaiseMerge" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                n = NodeId 2
                m = NodeId 3
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
            normalizeInstanceOpsForTest env [OpRaise n, OpRaise n, OpRaise n, OpMerge n m]
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

            it "moves a separated root Weaken immediately before its same-source terminal RaiseMerge" $ do
                let exterior = NodeId 4
                    ops0 =
                        [ OpWeaken root
                        , OpRaise child
                        , OpWeaken child
                        , OpRaiseMerge root exterior
                        ]
                reorderWeakenWithEnv env ops0
                    `shouldBe`
                        Right
                            [ OpRaise child
                            , OpWeaken child
                            , OpWeaken root
                            , OpRaiseMerge root exterior
                            ]

        it "normalizeInstanceOpsFull produces unforgeable validated witnesses when it succeeds" $ property $
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
                let normalized = normalizeInstanceOpsFull env ops
                    normalizationSuccess =
                        case normalized of
                            Left _ -> False
                            Right _ -> True
                 in checkCoverage $
                    cover 10 normalizationSuccess "normalization-success" $
                        case normalized of
                            Left _ -> property True
                            Right validatedOps ->
                                validateNormalizedWitness
                                    env
                                    (getValidatedInstanceOps validatedOps)
                                    === Right ()

        it "allows ops on binders that are later eliminated (paper normalization only)" $ do
            let c = mkNormalizeConstraint
                root = NodeId 0
                a = NodeId 2
                b = NodeId 3
                arg = NodeId 10
                env = mkNormalizeEnv c root (IntSet.fromList [getNodeId a, getNodeId b])
                ops0 = [OpGraft arg b, OpWeaken b, OpMerge b a]
            normalizeInstanceOpsForTest env ops0 `shouldBe` Right ops0

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
                case normalizeInstanceOpsForTest env ops0 of
                    Left err -> expectationFailure ("normalization failed: " ++ show err)
                    Right ops1 -> do
                        ops1 `shouldBe` [OpRaise n]
                        normalizeInstanceOpsForTest env ops1 `shouldBe` Right ops1

            it "OpRaise;OpMerge coalesces to OpRaiseMerge through full pipeline" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                    ops0 = [OpRaise n, OpMerge n m]
                normalizeInstanceOpsForTest env ops0 `shouldBe` Right [OpRaiseMerge n m]

            it "multiple OpRaise;OpMerge coalesces to single OpRaiseMerge" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                    ops0 = [OpRaise n, OpRaise n, OpRaise n, OpMerge n m]
                normalizeInstanceOpsForTest env ops0 `shouldBe` Right [OpRaiseMerge n m]

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
                normalizeInstanceOpsForTest env ops0
                    `shouldBe` Right [OpRaiseMerge n m]

            it "US-010-V1: coalesces repeated Raise;Merge into RaiseMerge" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId root, getNodeId n])
                    ops0 = [OpRaise n, OpRaise n, OpMerge n m]
                normalizeInstanceOpsForTest env ops0
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
                            , replayContract = ReplayContractStrict
                            }
                    ops0 = [OpRaise n, OpMerge n m]
                normalizeInstanceOpsForTest env ops0
                    `shouldBe` Left (OpOutsideInterior (OpMerge n m))

            it "R-RAISEMERGE-NORM-15: validated witnesses remain valid after idempotent re-normalization" $ do
                let c = mkNormalizeConstraint
                    root = NodeId 0
                    n = NodeId 2
                    m = NodeId 3
                    env = mkNormalizeEnv c root (IntSet.fromList [getNodeId n])
                    ops0 = [OpRaise n, OpMerge n m]
                case normalizeInstanceOpsForTest env ops0 of
                    Left err -> expectationFailure ("first normalization failed: " ++ show err)
                    Right ops1 -> do
                        ops1 `shouldBe` [OpRaiseMerge n m]
                        normalizeInstanceOpsForTest env ops1 `shouldBe` Right ops1

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
                                    , (getNodeId bound, TestTyBase bound (BaseTy "Int"))
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
                            , etResultRoot = root
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromSet (IntSet.fromList [getNodeId m, getNodeId parent, getNodeId n])
                            , etBinderReplayMap = mempty
                            , etReplayDomainBinders = []
                            , etCopyMap = mempty
                            , etReplayContract = ReplayContractNone
                            }
                    st0 = PresolutionStateInternal
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = IntMap.empty
                            , psNextNodeId = 4
                            , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                            , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                            , psExpansionResults = emptyExpansionResultMap
                            , psEdgeExecutionArtifacts =
                                singletonEdgeExecutionArtifactsForTest
                                    edgeId
                                    edgeWitness
                                    edgeTrace
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
                    constraint =
                        -- Destination ownership, rather than the source-domain
                        -- trace interior, is authoritative during witness
                        -- normalization.  Give the exterior node its own owner
                        -- so this fixture really places it outside I(root).
                        constraintWithOwners
                            nodes
                            [ (GenNodeId 0, [root])
                            , (GenNodeId 2, [exteriorNode])
                            ]
                            [ (interiorNode, root, BindFlex)
                            ]
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
                            , etResultRoot = root
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromSet (IntSet.fromList [getNodeId interiorNode])
                            , etBinderReplayMap = mempty
                            , etReplayDomainBinders = []
                            , etCopyMap = mempty
                            , etReplayContract = ReplayContractNone
                            }
                    st0 = PresolutionStateInternal
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = IntMap.empty
                            , psNextNodeId = 3
                            , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                            , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                            , psExpansionResults = emptyExpansionResultMap
                            , psEdgeExecutionArtifacts =
                                singletonEdgeExecutionArtifactsForTest
                                    edgeId
                                    edgeWitness
                                    edgeTrace
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
                            , etResultRoot = root
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromSet (IntSet.fromList [getNodeId mLess, getNodeId nGreater])
                            , etBinderReplayMap = mempty
                            , etReplayDomainBinders = []
                            , etCopyMap = mempty
                            , etReplayContract = ReplayContractNone
                            }
                    st0 = PresolutionStateInternal
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = IntMap.empty
                            , psNextNodeId = 4
                            , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                            , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                            , psExpansionResults = emptyExpansionResultMap
                            , psEdgeExecutionArtifacts =
                                singletonEdgeExecutionArtifactsForTest
                                    edgeId
                                    edgeWitness
                                    edgeTrace
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
                            , etResultRoot = root
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromSet (IntSet.fromList [getNodeId interiorNode, getNodeId outsideNode])
                            , etBinderReplayMap = mempty
                            , etReplayDomainBinders = []
                            , etCopyMap = mempty
                            , etReplayContract = ReplayContractNone
                            }
                    st0 = PresolutionStateInternal
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = IntMap.empty
                            , psNextNodeId = 3
                            , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                            , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                            , psExpansionResults = emptyExpansionResultMap
                            , psEdgeExecutionArtifacts =
                                singletonEdgeExecutionArtifactsForTest
                                    edgeId
                                    edgeWitness
                                    edgeTrace
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
                            , (getNodeId arg1, TestTyBase arg1 (BaseTy "Int"))
                            , (getNodeId arg2, TestTyBase arg2 (BaseTy "Bool"))
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
                            , etResultRoot = root
                            , etBinderArgs = [(sourceB1, arg1), (sourceB2, arg2)]
                            , etInterior = sourceInteriorFromSet (IntSet.fromList [getNodeId binder])
                            , etBinderReplayMap =
                                IntMap.fromList
                                    [ (getNodeId sourceB1, binder)
                                    , (getNodeId sourceB2, binder)
                                    ]
                            , etReplayDomainBinders = []
                            , etCopyMap = mempty
                            , etReplayContract = ReplayContractStrict
                            }
                    st0 = PresolutionStateInternal
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = uf
                            , psNextNodeId = 32
                            , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                            , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                            , psExpansionResults = emptyExpansionResultMap
                            , psEdgeExecutionArtifacts =
                                singletonEdgeExecutionArtifactsForTest
                                    edgeId
                                    edgeWitness
                                    edgeTrace
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
                            , etResultRoot = root
                            , etBinderArgs = [(sourceB1, missingArg1), (sourceB2, missingArg2)]
                            , etInterior = sourceInteriorFromSet (IntSet.fromList [getNodeId binder])
                            , etBinderReplayMap =
                                IntMap.fromList
                                    [ (getNodeId sourceB1, binder)
                                    , (getNodeId sourceB2, binder)
                                    ]
                            , etReplayDomainBinders = []
                            , etCopyMap = mempty
                            , etReplayContract = ReplayContractStrict
                            }
                    st0 = PresolutionStateInternal
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = uf
                            , psNextNodeId = 4
                            , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                            , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                            , psExpansionResults = emptyExpansionResultMap
                            , psEdgeExecutionArtifacts =
                                singletonEdgeExecutionArtifactsForTest
                                    edgeId
                                    edgeWitness
                                    edgeTrace
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
                            , (getNodeId arg1, TestTyBase arg1 (BaseTy "Int"))
                            , (getNodeId arg2, TestTyBase arg2 (BaseTy "Bool"))
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
                            , etResultRoot = root
                            , etBinderArgs = [(sourceB1, arg1), (sourceB2, arg2)]
                            , etInterior = sourceInteriorFromSet (IntSet.fromList [getNodeId binder])
                            , etBinderReplayMap =
                                IntMap.fromList
                                    [ (getNodeId sourceB1, binder)
                                    , (getNodeId sourceB2, binder)
                                    ]
                            , etReplayDomainBinders = []
                            , etCopyMap = mempty
                            , etReplayContract = ReplayContractStrict
                            }
                    st0 = PresolutionStateInternal
                            { psConstraint = constraint
                            , psPresolution = Presolution IntMap.empty
                            , psUnionFind = uf
                            , psNextNodeId = 32
                            , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                            , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                            , psExpansionResults = emptyExpansionResultMap
                            , psEdgeExecutionArtifacts =
                                singletonEdgeExecutionArtifactsForTest
                                    edgeId
                                    edgeWitness
                                    edgeTrace
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
                            , (getNodeId base, TestTyBase base (BaseTy "int"))
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
                            , (getNodeId base, TestTyBase base (BaseTy "int"))
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
                            , (getNodeId base, TestTyBase base (BaseTy "Poly"))
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
                            , cPolySyms = Set.fromList [testTypeIdentity "Poly"]
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
                            , (getNodeId base, TestTyBase base (BaseTy "int"))
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
        it "InstanceOp generator covers every operation constructor with non-negative node ids" $ property $
            withMaxSuccess 200 $
                forAll genInstanceOp $ \op ->
                    checkCoverage $
                        cover 15 (isOpGraft op) "OpGraft" $
                        cover 15 (isOpMerge op) "OpMerge" $
                        cover 15 (isOpRaise op) "OpRaise" $
                        cover 15 (isOpWeaken op) "OpWeaken" $
                        cover 15 (isOpRaiseMerge op) "OpRaiseMerge" $
                            counterexample
                                ("generated invalid operation: " ++ show op)
                                (all ((>= 0) . getNodeId) (opNodeIds op))
        it "witness normalization is idempotent" $ property $
            forAll (genInstanceOps 10) $ \ops ->
                forAll genNormalizeEnvParams $ \envParams ->
                    let env = mkTestNormalizeEnv envParams
                        normalized = normalizeInstanceOpsForTest env ops
                        normalizationSuccess =
                            case normalized of
                                Left _ -> False
                                Right _ -> True
                     in checkCoverage $
                        cover 10 normalizationSuccess "normalization-success" $
                            case normalized of
                                Left _ -> property True  -- Normalization failure is acceptable
                                Right ops1 ->
                                    case normalizeInstanceOpsForTest env ops1 of
                                        Left _ -> property False  -- Second normalization should not fail
                                        Right ops2 -> ops1 === ops2
        it "canonicalized witnesses have no redundant operations" $ property $
            forAll (genInstanceOps 10) $ \ops ->
                forAll genNormalizeEnvParams $ \envParams ->
                    let env = mkTestNormalizeEnv envParams
                        normalized = normalizeInstanceOpsForTest env ops
                        normalizationSuccess =
                            case normalized of
                                Left _ -> False
                                Right _ -> True
                     in checkCoverage $
                        cover 10 normalizationSuccess "normalization-success" $
                            case normalized of
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

            case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure ("Presolution failed: " ++ show err)
                Right PresolutionResult
                    { prEdgeExpansions = exps
                    , prEdgeWitnesses = ews
                    , prEdgeExpansionConstructions = constructions
                    } -> do
                    case IntMap.lookup 0 exps of
                        Just (ExpCompose _) -> pure ()
                        Just other -> expectationFailure ("Expected ExpCompose, got " ++ show other)
                        Nothing -> expectationFailure "No expansion found for Edge 0"
                    case IntMap.lookup 0 constructions of
                        Nothing ->
                            expectationFailure
                                "No construction evidence found for Edge 0"
                        Just construction ->
                            rawExpansionConstructionSemanticMetaKeys construction
                                `shouldSatisfy` (not . IntSet.null)
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
                            , (getNodeId argNode, TestTyBase argNode (BaseTy "Int"))
                            ]
                    , cBindParents = IntMap.empty
                    }
                tr =
                    EdgeTrace
                        { etRoot = root
                        , etResultRoot = root
                        , etBinderArgs = [(source, argNode)]
                        , etInterior = sourceInteriorFromList [root, source, replayTarget, argNode]
                        , etBinderReplayMap = IntMap.fromList [(getNodeId source, replayTarget)]
                        , etReplayDomainBinders = []
                        , etCopyMap = mempty
                        , etReplayContract = ReplayContractStrict
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
                            , (getNodeId argNode, TestTyBase argNode (BaseTy "Bool"))
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
                        , etResultRoot = root
                        , etBinderArgs = [(source, argNode)]
                        , etInterior = sourceInteriorFromList [root, body, replayBinder, source, argNode]
                        , etBinderReplayMap = IntMap.fromList [(getNodeId source, replayBinder)]
                        , etReplayDomainBinders = []
                        , etCopyMap = mempty
                        , etReplayContract = ReplayContractStrict
                        }
            validateReplayMapTraceContract id c c edgeKey tr `shouldBe` Right ()

        it "accepts codomain targets inside an explicit producer replay domain" $ do
            let edgeKey = 3
                root = NodeId 400
                source = NodeId 401
                replayBinder = NodeId 402
                argNode = NodeId 403
                c = rootedConstraint emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (getNodeId root, TyArrow root replayBinder replayBinder)
                            , (getNodeId source, TyVar { tnId = source, tnBound = Nothing })
                            , (getNodeId replayBinder, TyVar { tnId = replayBinder, tnBound = Nothing })
                            , (getNodeId argNode, TestTyBase argNode (BaseTy "Int"))
                            ]
                    , cBindParents = IntMap.empty
                    }
                tr =
                    EdgeTrace
                        { etRoot = root
                        , etResultRoot = root
                        , etBinderArgs = [(source, argNode)]
                        , etInterior = sourceInteriorFromList [root, source, replayBinder, argNode]
                        , etBinderReplayMap = IntMap.fromList [(getNodeId source, replayBinder)]
                        , etReplayDomainBinders = [replayBinder]
                        , etCopyMap = mempty
                        , etReplayContract = ReplayContractStrict
                        }
            validateReplayMapTraceContract id c c edgeKey tr `shouldBe` Right ()

        it "accepts an operation-time binder absent from the final graph when the producer publishes it explicitly" $ do
            let edgeKey = 4
                root = NodeId 410
                source = NodeId 411
                replayBinder = NodeId 412
                argNode = NodeId 413
                sourceConstraint = rootedConstraint emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (getNodeId root, TyArrow root replayBinder replayBinder)
                            , (getNodeId source, TyVar { tnId = source, tnBound = Nothing })
                            , (getNodeId replayBinder, TyVar { tnId = replayBinder, tnBound = Nothing })
                            , (getNodeId argNode, TestTyBase argNode (BaseTy "Int"))
                            ]
                    }
                finalConstraint = rootedConstraint emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (getNodeId root, TyArrow root argNode argNode)
                            , (getNodeId source, TyVar { tnId = source, tnBound = Nothing })
                            , (getNodeId argNode, TestTyBase argNode (BaseTy "Int"))
                            ]
                    }
                tr =
                    EdgeTrace
                        { etRoot = root
                        , etResultRoot = root
                        , etBinderArgs = [(source, argNode)]
                        , etInterior = sourceInteriorFromList [root, source, argNode]
                        , etBinderReplayMap = IntMap.singleton (getNodeId source) replayBinder
                        , etReplayDomainBinders = [replayBinder]
                        , etCopyMap = mempty
                        , etReplayContract = ReplayContractStrict
                        }
            validateReplayMapTraceContract id sourceConstraint finalConstraint edgeKey tr
                `shouldBe` Right ()

        it "hard-rejects non-injective replay-map codomain under strict contract" $ do
            let edgeKey = 11
                root = NodeId 210
                body = NodeId 211
                replayBinder = NodeId 212
                sourceA = NodeId 213
                sourceB = NodeId 214
                argA = NodeId 215
                argB = NodeId 216
                c = rootedConstraint emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (getNodeId root, TyForall root body)
                            , (getNodeId body, TyArrow body replayBinder replayBinder)
                            , (getNodeId replayBinder, TyVar { tnId = replayBinder, tnBound = Nothing })
                            , (getNodeId sourceA, TyVar { tnId = sourceA, tnBound = Nothing })
                            , (getNodeId sourceB, TyVar { tnId = sourceB, tnBound = Nothing })
                            , (getNodeId argA, TestTyBase argA (BaseTy "Int"))
                            , (getNodeId argB, TestTyBase argB (BaseTy "Bool"))
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
                        , etResultRoot = root
                        , etBinderArgs = [(sourceA, argA), (sourceB, argB)]
                        , etInterior = sourceInteriorFromList [root, body, replayBinder, sourceA, sourceB, argA, argB]
                        , etBinderReplayMap =
                            IntMap.fromList
                                [ (getNodeId sourceA, replayBinder)
                                , (getNodeId sourceB, replayBinder)
                                ]
                        , etReplayDomainBinders = []
                        , etCopyMap = mempty
                        , etReplayContract = ReplayContractStrict
                        }
                expected =
                    InternalError $
                        unlines
                            [ "edge replay-map codomain is non-injective"
                            , "edge: " ++ show (EdgeId edgeKey)
                            , "first source key: " ++ show (getNodeId sourceA)
                            , "second source key: " ++ show (getNodeId sourceB)
                            , "shared target: " ++ show replayBinder
                            ]
            validateReplayMapTraceContract id c c edgeKey tr `shouldBe` Left expected

        it "hard-rejects codomain targets that only match replay binders after canonicalization" $ do
            let edgeKey = 2
                root = NodeId 300
                body = NodeId 301
                replayBinder = NodeId 302
                replayAlias = NodeId 303
                source = NodeId 304
                argNode = NodeId 305
                c = rootedConstraint emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (getNodeId root, TyForall root body)
                            , (getNodeId body, TyArrow body replayBinder replayBinder)
                            , (getNodeId replayBinder, TyVar { tnId = replayBinder, tnBound = Nothing })
                            , (getNodeId replayAlias, TestTyBase replayAlias (BaseTy "Alias"))
                            , (getNodeId source, TyVar { tnId = source, tnBound = Nothing })
                            , (getNodeId argNode, TestTyBase argNode (BaseTy "Bool"))
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
                        , etResultRoot = root
                        , etBinderArgs = [(source, argNode)]
                        , etInterior = sourceInteriorFromList [root, body, replayBinder, replayAlias, source, argNode]
                        , etBinderReplayMap = IntMap.fromList [(getNodeId source, replayAlias)]
                        , etReplayDomainBinders = []
                        , etCopyMap = mempty
                        , etReplayContract = ReplayContractStrict
                        }
                canonical n
                    | n == replayAlias = replayBinder
                    | otherwise = n
                expected =
                    InternalError $
                        unlines
                            [ "edge replay-map codomain target outside replay binder domain"
                            , "edge: " ++ show (EdgeId edgeKey)
                            , "source key: " ++ show (getNodeId source)
                            , "replay target: " ++ show replayAlias
                            ]
            validateReplayMapTraceContract canonical c c edgeKey tr `shouldBe` Left expected

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
                    , precomputedDescendants = IntMap.empty
                    , certifiedWeakens = IntSet.empty
                    , certifiedRaises = IntSet.empty
                    , certifiedReplayBinders = IntSet.empty
                    , binderReplayMap = IntMap.empty
                    , replayContract = ReplayContractNone
                    , replayDomainBinders = []
                    , isAnnotationEdge = False
                    }
            case normalizeInstanceOpsForTest env [] of
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
                    , precomputedDescendants = IntMap.empty
                    , certifiedWeakens = IntSet.empty
                    , certifiedRaises = IntSet.empty
                    , certifiedReplayBinders = IntSet.empty
                    , binderReplayMap = IntMap.empty
                    , replayContract = ReplayContractNone
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
                    , precomputedDescendants = IntMap.empty
                    , certifiedWeakens = IntSet.empty
                    , certifiedRaises = IntSet.empty
                    , certifiedReplayBinders = IntSet.empty
                    , binderReplayMap = IntMap.empty
                    , replayContract = ReplayContractNone
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
                            , (2, TestTyBase argNode (BaseTy "Int"))
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
                    , precomputedDescendants = IntMap.empty
                    , certifiedWeakens = IntSet.empty
                    , certifiedRaises = IntSet.empty
                    , certifiedReplayBinders = IntSet.empty
                    , binderReplayMap = IntMap.empty
                    , replayContract = ReplayContractStrict
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
                            , (2, TestTyBase badTarget (BaseTy "Bool"))
                            , (3, TestTyBase argNode (BaseTy "Int"))
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
                    , precomputedDescendants = IntMap.empty
                    , certifiedWeakens = IntSet.empty
                    , certifiedRaises = IntSet.empty
                    , certifiedReplayBinders = IntSet.empty
                    , binderReplayMap = IntMap.fromList [(getNodeId binder, badTarget)]
                    , replayContract = ReplayContractStrict
                    , replayDomainBinders = []
                    , isAnnotationEdge = False
                    }
            validateNormalizedWitness env [] `shouldBe` Left (ReplayMapTargetOutsideReplayDomain binder badTarget)

        it "accepts a certified operation-time replay binder after final graph elimination" $ do
            let root = NodeId 0
                binder = NodeId 1
                replayBinder = NodeId 2
                argNode = NodeId 3
                c = rootedConstraint emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (0, TyArrow root argNode argNode)
                            , (1, TyVar { tnId = binder, tnBound = Nothing })
                            , (3, TestTyBase argNode (BaseTy "Int"))
                            ]
                    }
                env = OmegaNormalizeEnv
                    { oneRoot = root
                    , interior = IntSet.fromList [0, 1, 3]
                    , interiorRaw = IntSet.fromList [0, 1, 3]
                    , weakened = IntSet.empty
                    , orderKeys = IntMap.empty
                    , canonical = id
                    , constraint = c
                    , binderArgs = IntMap.singleton (getNodeId binder) argNode
                    , precomputedDescendants = IntMap.empty
                    , certifiedWeakens = IntSet.empty
                    , certifiedRaises = IntSet.empty
                    , certifiedReplayBinders = IntSet.singleton (getNodeId replayBinder)
                    , binderReplayMap = IntMap.singleton (getNodeId binder) replayBinder
                    , replayContract = ReplayContractStrict
                    , replayDomainBinders = [replayBinder]
                    , isAnnotationEdge = False
                    }
            validateNormalizedWitness env [] `shouldBe` Right ()

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
                            , (3, TestTyBase argA (BaseTy "Int"))
                            , (4, TestTyBase argB (BaseTy "Bool"))
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
                    , precomputedDescendants = IntMap.empty
                    , certifiedWeakens = IntSet.empty
                    , certifiedRaises = IntSet.empty
                    , certifiedReplayBinders = IntSet.empty
                    , binderReplayMap =
                        IntMap.fromList
                            [ (getNodeId binderA, binderA)
                            , (getNodeId binderB, binderA)
                            ]
                    , replayContract = ReplayContractStrict
                    , replayDomainBinders = []
                    , isAnnotationEdge = False
                    }
            validateNormalizedWitness env [] `shouldBe` Left (ReplayMapNonInjective binderA binderB binderA)

        describe "construction-certificate replay composition" $ do
            it "preserves a distinct RaiseMerge replay pair beside a certified Weaken" $ do
                let edgeId = 45
                    sourceRoot = NodeId 600
                    sourceRaise = NodeId 601
                    sourceWeaken = NodeId 602
                    sourceOther = NodeId 603
                    resultRoot = NodeId 610
                    copiedRaise = NodeId 611
                    copiedWeaken = NodeId 612
                    replayRaise = NodeId 613
                    raiseArg = NodeId 614
                    weakenArg = NodeId 615
                    nodes =
                        nodeMapFromList
                            [ (getNodeId resultRoot, TyArrow resultRoot copiedRaise copiedWeaken)
                            , (getNodeId copiedRaise, TyVar copiedRaise Nothing)
                            , (getNodeId copiedWeaken, TyVar copiedWeaken Nothing)
                            , (getNodeId replayRaise, TyVar replayRaise Nothing)
                            , (getNodeId raiseArg, TestTyBase raiseArg (BaseTy "RaiseArg"))
                            , (getNodeId weakenArg, TestTyBase weakenArg (BaseTy "WeakenArg"))
                            ]
                    constraintWithWeakenFlag weakenFlag =
                        constraintWithOwners
                            nodes
                            [ (GenNodeId 610, [resultRoot])
                            , (GenNodeId 613, [replayRaise])
                            ]
                            [ (copiedRaise, resultRoot, BindFlex)
                            , (copiedWeaken, resultRoot, weakenFlag)
                            ]
                    constraintBefore = constraintWithWeakenFlag BindFlex
                    constraintAfter = constraintWithWeakenFlag BindRigid
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = resultRoot
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness =
                                InstanceWitness
                                    [ OpRaise sourceRaise
                                    , OpMerge sourceRaise sourceOther
                                    , OpWeaken sourceWeaken
                                    ]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = resultRoot
                            , etBinderArgs =
                                [ (sourceRaise, raiseArg)
                                , (sourceWeaken, weakenArg)
                                ]
                            , etInterior =
                                sourceInteriorFromList
                                    [ sourceRoot
                                    , sourceRaise
                                    , sourceWeaken
                                    ]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap =
                                CopyMapping
                                    ( IntMap.fromList
                                        [ (getNodeId sourceRaise, copiedRaise)
                                        , (getNodeId sourceWeaken, copiedWeaken)
                                        , (getNodeId sourceOther, replayRaise)
                                        ]
                                    )
                            , etReplayContract = ReplayContractNone
                            }
                certificate <-
                    case
                        certifyAppliedNonRootWeakenReplay
                            constraintBefore
                            constraintAfter
                            id
                            sourceWeaken
                            copiedWeaken
                            resultRoot of
                        Just certified -> pure certified
                        Nothing ->
                            expectationFailure "expected a valid applied-Weaken certificate"
                                >> fail "missing certificate"
                let st0 =
                        ( mkWitnessNormState
                            constraintAfter
                            IntMap.empty
                            620
                            edgeId
                            edgeWitness
                            edgeTrace
                        )
                            { psWeakenReplayCertificates =
                                IntMap.singleton
                                    edgeId
                                    (IntMap.singleton (getNodeId sourceWeaken) certificate)
                            }
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left err ->
                        expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                    Right (_, st') ->
                        case IntMap.lookup edgeId (psEdgeTraces st') of
                            Nothing -> expectationFailure "missing normalized edge trace"
                            Just trace -> do
                                etReplayContract trace `shouldBe` ReplayContractStrict
                                etBinderArgs trace
                                    `shouldBe`
                                        [ (sourceRaise, raiseArg)
                                        , (sourceWeaken, weakenArg)
                                        ]
                                etBinderReplayMap trace
                                    `shouldBe`
                                        IntMap.fromList
                                            [ (getNodeId sourceRaise, replayRaise)
                                            , (getNodeId sourceWeaken, copiedWeaken)
                                            ]
                                etReplayDomainBinders trace
                                    `shouldBe` [replayRaise, copiedWeaken]

            it "rejects two RaiseMerge targets for one exact frozen source before map construction" $ do
                let edgeId = 46
                    sourceRoot = NodeId 630
                    sourceCanonical = NodeId 631
                    sourceAlias = NodeId 632
                    sourceOtherA = NodeId 633
                    sourceOtherB = NodeId 634
                    resultRoot = NodeId 640
                    copiedCanonical = NodeId 641
                    copiedAlias = NodeId 642
                    copiedOperated = NodeId 643
                    replayA = NodeId 644
                    replayB = NodeId 645
                    sourceArg = NodeId 646
                    nodes =
                        nodeMapFromList
                            [ (getNodeId resultRoot, TyArrow resultRoot copiedOperated copiedOperated)
                            , (getNodeId copiedOperated, TyVar copiedOperated Nothing)
                            , (getNodeId replayA, TyVar replayA Nothing)
                            , (getNodeId replayB, TyVar replayB Nothing)
                            , (getNodeId sourceArg, TestTyBase sourceArg (BaseTy "SourceArg"))
                            ]
                    c =
                        constraintWithOwners
                            nodes
                            [ (GenNodeId 640, [resultRoot])
                            , (GenNodeId 644, [replayA])
                            , (GenNodeId 645, [replayB])
                            ]
                            [ (copiedOperated, resultRoot, BindFlex)
                            ]
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = resultRoot
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness =
                                InstanceWitness
                                    [ OpRaise sourceAlias
                                    , OpMerge sourceAlias sourceOtherA
                                    , OpRaise sourceAlias
                                    , OpMerge sourceAlias sourceOtherB
                                    ]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = resultRoot
                            , etBinderArgs = [(sourceAlias, sourceArg)]
                            , etInterior =
                                sourceInteriorFromList
                                    [ sourceRoot
                                    , sourceCanonical
                                    , sourceAlias
                                    ]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap =
                                CopyMapping
                                    ( IntMap.fromList
                                        [ (getNodeId sourceCanonical, copiedCanonical)
                                        , (getNodeId sourceAlias, copiedAlias)
                                        , (getNodeId sourceOtherA, replayA)
                                        , (getNodeId sourceOtherB, replayB)
                                        ]
                                    )
                            , etReplayContract = ReplayContractNone
                            }
                    unionFind =
                        IntMap.fromList
                            [ (getNodeId sourceAlias, sourceCanonical)
                            , (getNodeId copiedCanonical, copiedOperated)
                            , (getNodeId copiedAlias, copiedOperated)
                            ]
                    st0 =
                        mkWitnessNormState
                            c
                            unionFind
                            650
                            edgeId
                            edgeWitness
                            edgeTrace
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left
                        ( WitnessNormalizationError
                            (EdgeId actualEdge)
                            (ReplayMapSourceNonFunctional actualSource actualTargets)
                          ) -> do
                            actualEdge `shouldBe` edgeId
                            actualSource `shouldBe` sourceAlias
                            actualTargets `shouldBe` [replayA, replayB]
                    Left err ->
                        expectationFailure
                            ("expected non-functional replay-source rejection, got: " ++ show err)
                    Right _ ->
                        expectationFailure "expected non-functional replay-source rejection"

            it "builds an injective zero-legacy replay map from two certificates" $ do
                let edgeId = 47
                    sourceRoot = NodeId 660
                    sourceA = NodeId 661
                    sourceB = NodeId 662
                    resultRoot = NodeId 670
                    copiedA = NodeId 671
                    copiedB = NodeId 672
                    argA = NodeId 673
                    argB = NodeId 674
                    nodes =
                        nodeMapFromList
                            [ (getNodeId resultRoot, TyArrow resultRoot copiedA copiedB)
                            , (getNodeId copiedA, TyVar copiedA Nothing)
                            , (getNodeId copiedB, TyVar copiedB Nothing)
                            , (getNodeId argA, TestTyBase argA (BaseTy "ArgA"))
                            , (getNodeId argB, TestTyBase argB (BaseTy "ArgB"))
                            ]
                    constraintWithWeakenFlag weakenFlag =
                        constraintWithOwners
                            nodes
                            [(GenNodeId 670, [resultRoot])]
                            [ (copiedA, resultRoot, weakenFlag)
                            , (copiedB, resultRoot, weakenFlag)
                            ]
                    constraintBefore = constraintWithWeakenFlag BindFlex
                    constraintAfter = constraintWithWeakenFlag BindRigid
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = resultRoot
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness = InstanceWitness [OpWeaken sourceA, OpWeaken sourceB]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = resultRoot
                            , etBinderArgs = [(sourceA, argA), (sourceB, argB)]
                            , etInterior = sourceInteriorFromList [sourceRoot, sourceA, sourceB]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap =
                                CopyMapping
                                    ( IntMap.fromList
                                        [ (getNodeId sourceA, copiedA)
                                        , (getNodeId sourceB, copiedB)
                                        ]
                                    )
                            , etReplayContract = ReplayContractNone
                            }
                    requireCertificate source copied =
                        case
                            certifyAppliedNonRootWeakenReplay
                                constraintBefore
                                constraintAfter
                                id
                                source
                                copied
                                resultRoot of
                            Just certificate -> pure certificate
                            Nothing ->
                                expectationFailure
                                    ("expected certificate for " ++ show source)
                                    >> fail "missing certificate"
                certificateA <- requireCertificate sourceA copiedA
                certificateB <- requireCertificate sourceB copiedB
                let st0 =
                        ( mkWitnessNormState
                            constraintAfter
                            IntMap.empty
                            680
                            edgeId
                            edgeWitness
                            edgeTrace
                        )
                            { psWeakenReplayCertificates =
                                IntMap.singleton
                                    edgeId
                                    ( IntMap.fromList
                                        [ (getNodeId sourceA, certificateA)
                                        , (getNodeId sourceB, certificateB)
                                        ]
                                    )
                            }
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left err ->
                        expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                    Right (_, st') ->
                        case IntMap.lookup edgeId (psEdgeTraces st') of
                            Nothing -> expectationFailure "missing normalized edge trace"
                            Just trace -> do
                                etReplayContract trace `shouldBe` ReplayContractStrict
                                etBinderArgs trace `shouldBe` [(sourceA, argA), (sourceB, argB)]
                                etBinderReplayMap trace
                                    `shouldBe`
                                        IntMap.fromList
                                            [ (getNodeId sourceA, copiedA)
                                            , (getNodeId sourceB, copiedB)
                                            ]
                                etReplayDomainBinders trace `shouldBe` [copiedA, copiedB]

        it "normalization derives strict replay lane from edge semantics and maps codomain to edge-root replay binders" $ do
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
                        , (getNodeId argA, TestTyBase argA (BaseTy "Int"))
                        , (getNodeId argB, TestTyBase argB (BaseTy "Bool"))
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
                        , etResultRoot = root
                        , etBinderArgs = [(sourceA, argA), (sourceB, argB)]
                        , etInterior =
                            sourceInteriorFromSet
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
                        , etReplayDomainBinders = []
                        , etCopyMap = mempty
                        , etReplayContract = ReplayContractStrict
                        }
                st0 =
                    PresolutionStateInternal
                        { psConstraint = c
                        , psPresolution = Presolution IntMap.empty
                        , psUnionFind = IntMap.empty
                        , psNextNodeId = 40
                        , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                        , psBinderCache = IntMap.empty
                        , psGraphVersion = 0
                        , psUnionFindVersion = 0
                        , psBindParentsVersion = 0
                        , psBindingModelCache = Nothing
                        , psEdgeLocalSnapshot = Nothing
                        , psBindingRepairCache = Nothing
                        , psBindingRepairDirty = Nothing
                        , psCachedRootGen = Nothing
                        , psExpansionResults = emptyExpansionResultMap
                        , psEdgeExecutionArtifacts =
                            singletonEdgeExecutionArtifactsForTest
                                edgeId
                                edgeWitness
                                edgeTrace
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
                            let _orderedTyVarBinders =
                                    [ b
                                    | b <- orderedBinders
                                    , case lookupNodeIn (cNodes (psConstraint st')) b of
                                        Just TyVar{} -> True
                                        _ -> False
                                    ]
                            etReplayContract tr' `shouldBe` ReplayContractStrict
                            etBinderArgs tr' `shouldBe` [(sourceA, argA), (sourceB, argB)]
                            etReplayDomainBinders tr' `shouldBe` [replayA, replayB]
                            etBinderReplayMap tr'
                                `shouldBe` IntMap.fromList
                                    [ (getNodeId sourceA, replayA)
                                    , (getNodeId sourceB, replayB)
                                    ]

        it "constructs a strict replay bridge from copied source-binder provenance after the source node is gone" $ do
            let edgeKey = 0
                sourceBinder = NodeId 2
                root = NodeId 10
                argument = NodeId 17
                replayBinder = NodeId 18
                leaf = NodeId 19
                nodes =
                    nodeMapFromList
                        [ (getNodeId root, TyArrow root replayBinder replayBinder)
                        , (getNodeId argument, TyArrow argument leaf leaf)
                        , (getNodeId replayBinder, TyVar {tnId = replayBinder, tnBound = Just argument})
                        , (getNodeId leaf, TyVar {tnId = leaf, tnBound = Nothing})
                        ]
                bindParents =
                    IntMap.fromList
                        [ (nodeRefKey (typeRef replayBinder), (genRef (GenNodeId 0), BindRigid))
                        , (nodeRefKey (typeRef argument), (typeRef root, BindFlex))
                        , (nodeRefKey (typeRef leaf), (typeRef argument, BindFlex))
                        ]
                c =
                    rootedConstraint
                        emptyConstraint
                            { cNodes = nodes
                            , cBindParents = bindParents
                            }
                edgeWitness =
                    EdgeWitness
                        { ewEdgeId = EdgeId edgeKey
                        , ewLeft = root
                        , ewRight = root
                        , ewRoot = root
                        , ewForallIntros = 0
                        , ewWitness =
                            InstanceWitness
                                [ OpGraft argument sourceBinder
                                , OpWeaken sourceBinder
                                ]
                        }
                edgeTrace =
                    EdgeTrace
                        { etRoot = root
                        , etResultRoot = root
                        , etBinderArgs = [(sourceBinder, argument)]
                        , etInterior = sourceInteriorFromList [root, sourceBinder, argument, leaf]
                        , etBinderReplayMap = IntMap.empty
                        , etReplayDomainBinders = []
                        , etCopyMap =
                            CopyMapping
                                (IntMap.singleton (getNodeId sourceBinder) replayBinder)
                        , etReplayContract = ReplayContractNone
                        }
                st0 =
                    ( PresolutionState
                        c
                        (Presolution IntMap.empty)
                        IntMap.empty
                        20
                        IntSet.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                        IntMap.empty
                    )
                        { psEdgeExecutionArtifacts =
                            singletonEdgeExecutionArtifactsForTest
                                edgeKey
                                edgeWitness
                                edgeTrace
                        }
            lookupNodeIn (cNodes c) sourceBinder `shouldBe` Nothing
            case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                Left err ->
                    expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                Right (_, st') ->
                    case IntMap.lookup edgeKey (psEdgeTraces st') of
                        Nothing ->
                            expectationFailure "Expected normalized trace in psEdgeTraces"
                        Just tr' -> do
                            etReplayContract tr' `shouldBe` ReplayContractStrict
                            etBinderArgs tr' `shouldBe` [(sourceBinder, argument)]
                            etReplayDomainBinders tr' `shouldBe` [replayBinder]
                            etBinderReplayMap tr'
                                `shouldBe` IntMap.singleton (getNodeId sourceBinder) replayBinder

        describe "copied rigid identity pruning" $ do
            let copiedRigidIdentityFixture includeWeaken =
                    let edgeKey = 41
                        root = NodeId 500
                        body = NodeId 501
                        replayBinder = NodeId 502
                        aggregate = NodeId 503
                        sourceAncestor = NodeId 504
                        sourceRaised = NodeId 505
                        sourceMuBody = NodeId 506
                        copiedRaised = NodeId 507
                        copiedMuBody = NodeId 508
                        sourceBase = NodeId 509
                        copiedBase = NodeId 510
                        argNode = NodeId 511
                        nodes =
                            nodeMapFromList
                                [ (getNodeId root, TyForall root body)
                                , (getNodeId body, TyArrow body replayBinder aggregate)
                                , (getNodeId replayBinder, TyVar { tnId = replayBinder, tnBound = Nothing })
                                , (getNodeId aggregate, TyArrow aggregate sourceAncestor copiedRaised)
                                , (getNodeId sourceAncestor, TyArrow sourceAncestor sourceRaised sourceRaised)
                                , (getNodeId sourceRaised, TyMu sourceRaised sourceMuBody)
                                , (getNodeId sourceMuBody, TyArrow sourceMuBody sourceBase sourceBase)
                                , (getNodeId copiedRaised, TyMu copiedRaised copiedMuBody)
                                , (getNodeId copiedMuBody, TyArrow copiedMuBody copiedBase copiedBase)
                                , (getNodeId sourceBase, TestTyBase sourceBase (BaseTy "Source"))
                                , (getNodeId copiedBase, TestTyBase copiedBase (BaseTy "Copy"))
                                , (getNodeId argNode, TestTyBase argNode (BaseTy "Arg"))
                                ]
                        bindParents =
                            bindParentsFromPairs
                                [ (body, root, BindFlex)
                                , (replayBinder, root, BindFlex)
                                , (aggregate, root, BindFlex)
                                , (sourceAncestor, root, BindRigid)
                                , (sourceRaised, sourceAncestor, BindFlex)
                                , (sourceMuBody, sourceRaised, BindFlex)
                                , (sourceBase, sourceMuBody, BindFlex)
                                , (copiedRaised, root, BindRigid)
                                , (copiedMuBody, copiedRaised, BindFlex)
                                , (copiedBase, copiedMuBody, BindFlex)
                                ]
                        c =
                            rootedConstraint emptyConstraint
                                { cNodes = nodes
                                , cBindParents = bindParents
                                }
                        ops =
                            OpRaise sourceRaised
                                : [OpWeaken sourceRaised | includeWeaken]
                        edgeWitness =
                            EdgeWitness
                                { ewEdgeId = EdgeId edgeKey
                                , ewLeft = root
                                , ewRight = root
                                , ewRoot = root
                                , ewForallIntros = 0
                                , ewWitness = InstanceWitness ops
                                }
                        edgeTrace =
                            EdgeTrace
                                { etRoot = root
                                , etResultRoot = root
                                , etBinderArgs = [(replayBinder, argNode)]
                                , etInterior =
                                    sourceInteriorFromList
                                        [ root
                                        , body
                                        , replayBinder
                                        , aggregate
                                        , sourceAncestor
                                        , sourceRaised
                                        , sourceMuBody
                                        , copiedRaised
                                        , copiedMuBody
                                        , sourceBase
                                        , copiedBase
                                        , argNode
                                        ]
                                , etBinderReplayMap = IntMap.empty
                                , etReplayDomainBinders = []
                                , etCopyMap =
                                    CopyMapping
                                        (IntMap.singleton (getNodeId sourceRaised) copiedRaised)
                                , etReplayContract = ReplayContractStrict
                                }
                        st0 =
                            ( PresolutionState
                                c
                                (Presolution IntMap.empty)
                                IntMap.empty
                                600
                                IntSet.empty
                                IntMap.empty
                                IntMap.empty
                                IntMap.empty
                                IntMap.empty
                                IntMap.empty
                            )
                                { psEdgeExecutionArtifacts =
                                    singletonEdgeExecutionArtifactsForTest
                                        edgeKey
                                        edgeWitness
                                        edgeTrace
                                }
                     in (edgeKey, root, sourceAncestor, sourceRaised, copiedRaised, c, st0)

            it "drops a copied directly-rigid Raise on a non-annotation edge" $ do
                let (edgeKey, _root, _sourceAncestor, _sourceRaised, _copiedRaised, c, st0) =
                        copiedRigidIdentityFixture False
                IntSet.member edgeKey (cAnnEdges c) `shouldBe` False
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left err ->
                        expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                    Right (_, st') ->
                        case IntMap.lookup edgeKey (psEdgeWitnesses st') of
                            Nothing ->
                                expectationFailure "Expected normalized witness in psEdgeWitnesses"
                            Just ew' ->
                                getInstanceOps (ewWitness ew') `shouldBe` []

            it "does not erase a copied rigid Raise made rigid by the same witness's Weaken" $ do
                let (edgeKey, root, _sourceAncestor, sourceRaised, _copiedRaised, c, st0) =
                        copiedRigidIdentityFixture True
                IntSet.member edgeKey (cAnnEdges c) `shouldBe` False
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left
                        ( WitnessNormalizationError
                            (EdgeId eid)
                            (NotTransitivelyFlexBound (OpRaise operated) target validationRoot)
                          ) -> do
                            eid `shouldBe` edgeKey
                            operated `shouldBe` sourceRaised
                            target `shouldBe` sourceRaised
                            validationRoot `shouldBe` root
                    Left err ->
                        expectationFailure
                            ("Expected retained Raise validation failure, got: " ++ show err)
                    Right _ ->
                        expectationFailure
                            "Expected same-witness Weaken to prevent rigid-identity pruning"

            it "accepts a Raise from its exact pre-mutation source authority certificate" $ do
                let (edgeKey, root, sourceAncestor, sourceRaised, _copiedRaised, _c, st0) =
                        copiedRigidIdentityFixture True
                    initialConstraint =
                        Binding.setBindParent
                            (typeRef sourceAncestor)
                            (typeRef root, BindFlex)
                            (psConstraint st0)
                    initialState = st0 { psConstraint = initialConstraint }
                    sourceInterior = sourceInteriorFromList [root, sourceRaised]
                case
                    runPresolutionM
                        defaultTraceConfig
                        initialState
                        (sourceRaiseAuthorityNodesForTest root sourceInterior)
                  of
                    Left err ->
                        expectationFailure
                            ("Expected source authority construction to succeed, got: " ++ show err)
                    Right (raiseAuthority, _) -> do
                        raiseAuthority
                            `shouldBe` IntSet.singleton (getNodeId sourceRaised)
                        let finalConstraint =
                                Binding.setBindParent
                                    (typeRef sourceRaised)
                                    (typeRef sourceAncestor, BindRigid)
                                    initialConstraint
                            stWithAuthority =
                                setEdgeRaiseAuthority
                                    edgeKey
                                    raiseAuthority
                                    initialState {psConstraint = finalConstraint}
                        IntSet.member edgeKey (cAnnEdges finalConstraint) `shouldBe` False
                        case runPresolutionM defaultTraceConfig stWithAuthority normalizeEdgeWitnessesM of
                            Left err ->
                                expectationFailure
                                    ("Expected certified source Raise to normalize, got: " ++ show err)
                            Right (_, st') ->
                                case IntMap.lookup edgeKey (psEdgeWitnesses st') of
                                    Nothing ->
                                        expectationFailure "Expected normalized witness in psEdgeWitnesses"
                                    Just ew' ->
                                        getInstanceOps (ewWitness ew')
                                            `shouldBe` [OpRaise sourceRaised]

        it "fails fast when trace source domain exceeds replay binder domain (no stale-source pruning fallback)" $ do
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
                        , (getNodeId argActive, TestTyBase argActive (BaseTy "Int"))
                        , (getNodeId argStale, TestTyBase argStale (BaseTy "Bool"))
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
                        , etResultRoot = root
                        , etBinderArgs = [(activeSource, argActive), (staleSource, argStale)]
                        , etInterior =
                            sourceInteriorFromSet
                                (IntSet.fromList
                                    [ getNodeId root
                                    , getNodeId body
                                    , getNodeId replayA
                                    , getNodeId activeSource
                                    , getNodeId argActive
                                    ]
                                )
                        , etBinderReplayMap = IntMap.empty
                        , etReplayDomainBinders = []
                        , etCopyMap = mempty
                        , etReplayContract = ReplayContractStrict
                        }
                st0 =
                    PresolutionStateInternal
                        { psConstraint = c
                        , psPresolution = Presolution IntMap.empty
                        , psUnionFind = IntMap.empty
                        , psNextNodeId = 150
                        , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                        , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                        , psExpansionResults = emptyExpansionResultMap
                        , psEdgeExecutionArtifacts =
                            singletonEdgeExecutionArtifactsForTest
                                edgeId
                                edgeWitness
                                edgeTrace
                        }
            case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                Left (WitnessNormalizationError (EdgeId eid) (ReplayMapIncomplete missing)) -> do
                    eid `shouldBe` edgeId
                    missing `shouldBe` [staleSource]
                Left err ->
                    expectationFailure ("Expected ReplayMapIncomplete, got: " ++ show err)
                Right _ ->
                    expectationFailure "Expected fail-fast replay-map source/replay domain mismatch"

        it "normalization projects no-replay wrapper ops without erasing source provenance" $ do
            let edgeId = 2
                root = NodeId 300
                source = NodeId 301
                argNode = NodeId 302
                nodes =
                    nodeMapFromList
                        [ (getNodeId root, TyArrow root source source)
                        , (getNodeId source, TyVar { tnId = source, tnBound = Nothing })
                        , (getNodeId argNode, TestTyBase argNode (BaseTy "Int"))
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
                        , etResultRoot = root
                        , etBinderArgs = [(source, argNode)]
                        , etInterior =
                            sourceInteriorFromSet
                                (IntSet.fromList
                                    [ getNodeId root
                                    , getNodeId source
                                    , getNodeId argNode
                                    ]
                                )
                        , etBinderReplayMap = IntMap.empty
                        , etReplayDomainBinders = []
                        , etCopyMap = mempty
                        , etReplayContract = ReplayContractNone
                        }
                st0 =
                    PresolutionStateInternal
                        { psConstraint = c
                        , psPresolution = Presolution IntMap.empty
                        , psUnionFind = IntMap.empty
                        , psNextNodeId = 350
                        , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                        , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                        , psExpansionResults = emptyExpansionResultMap
                        , psEdgeExecutionArtifacts =
                            singletonEdgeExecutionArtifactsForTest
                                edgeId
                                edgeWitness
                                edgeTrace
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
                            etBinderArgs tr' `shouldBe` [(source, argNode)]
                            etBinderReplayMap tr' `shouldBe` IntMap.empty

        it "normalization does not widen no-replay interiors with dead rewritten binder copies" $ do
            let edgeId = 24
                root = NodeId 360
                sourceA = NodeId 361
                sourceB = NodeId 362
                deadCopyA = NodeId 363
                deadCopyB = NodeId 364
                argA = NodeId 365
                argB = NodeId 366
                nodes =
                    nodeMapFromList
                        [ (getNodeId root, TyArrow root root root)
                        , (getNodeId argA, TestTyBase argA (BaseTy "Int"))
                        , (getNodeId argB, TestTyBase argB (BaseTy "Bool"))
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
                        , ewWitness = InstanceWitness [OpWeaken sourceA, OpWeaken sourceB]
                        }
                edgeTrace =
                    EdgeTrace
                        { etRoot = root
                        , etResultRoot = root
                        , etBinderArgs = [(sourceA, argA), (sourceB, argB)]
                        , etInterior = sourceInteriorFromList [root]
                        , etBinderReplayMap = IntMap.empty
                        , etReplayDomainBinders = []
                        , etCopyMap =
                            CopyMapping
                                (IntMap.fromList
                                    [ (getNodeId sourceA, deadCopyA)
                                    , (getNodeId sourceB, deadCopyB)
                                    ]
                                )
                        , etReplayContract = ReplayContractNone
                        }
                st0 =
                    PresolutionStateInternal
                        { psConstraint = c
                        , psPresolution = Presolution IntMap.empty
                        , psUnionFind = IntMap.empty
                        , psNextNodeId = 370
                        , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                        , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                        , psExpansionResults = emptyExpansionResultMap
                        , psEdgeExecutionArtifacts =
                            singletonEdgeExecutionArtifactsForTest
                                edgeId
                                edgeWitness
                                edgeTrace
                        }
            case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                Left err ->
                    expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                Right (_, st') ->
                    case IntMap.lookup edgeId (psEdgeWitnesses st') of
                        Nothing ->
                            expectationFailure "Expected normalized witness in psEdgeWitnesses"
                        Just ew' ->
                            getInstanceOps (ewWitness ew') `shouldBe` []

        describe "root RaiseMerge no-replay authority" $ do
            describe "terminal root RaiseMerge validation" $ do
                let sourceRoot = NodeId 690
                    interiorNode = NodeId 691
                    exterior = NodeId 692
                    flexibleTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = exterior
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromList [sourceRoot, interiorNode]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap = mempty
                            , etReplayContract = ReplayContractNone
                            }
                    rigidTrace =
                        let argument = NodeId 694
                            replayRoot = NodeId 695
                         in flexibleTrace
                                { etBinderArgs = [(sourceRoot, argument)]
                                , etBinderReplayMap =
                                    IntMap.singleton (getNodeId sourceRoot) replayRoot
                                , etReplayDomainBinders = [replayRoot]
                                , etCopyMap =
                                    CopyMapping
                                        (IntMap.singleton (getNodeId sourceRoot) replayRoot)
                                , etReplayContract = ReplayContractStrict
                                }
                    validateWith trace =
                        validateTerminalRootRaiseMergeForTest
                            (etRoot trace)
                            (\operated other ->
                                rootRaiseMergeTraceAuthority operated other trace
                            )
                            (\operated other ->
                                rootWeakenRaiseMergeTraceAuthority operated other trace
                            )

                it "accepts zero exact-root candidates, including a terminal interior RaiseMerge" $ do
                    validateWith flexibleTrace [OpRaiseMerge interiorNode exterior]
                        `shouldBe` Right ()

                it "accepts one trace-authorized naked terminal root RaiseMerge" $ do
                    validateWith flexibleTrace [OpRaiseMerge sourceRoot exterior]
                        `shouldBe` Right ()

                it "accepts root authority when strict replay belongs to an unrelated source binder" $ do
                    let argument = NodeId 694
                        replayBinder = NodeId 695
                        rootCopy = NodeId 696
                        mixedTrace =
                            flexibleTrace
                                { etBinderArgs = [(interiorNode, argument)]
                                , etBinderReplayMap =
                                    IntMap.singleton
                                        (getNodeId interiorNode)
                                        replayBinder
                                , etReplayDomainBinders = [replayBinder]
                                , etCopyMap =
                                    CopyMapping
                                        ( IntMap.fromList
                                            [ (getNodeId sourceRoot, rootCopy)
                                            , (getNodeId interiorNode, replayBinder)
                                            ]
                                        )
                                , etReplayContract = ReplayContractStrict
                                }
                    rootRaiseMergeTraceAuthority sourceRoot exterior mixedTrace
                        `shouldBe` True
                    validateWith mixedTrace [OpRaiseMerge sourceRoot exterior]
                        `shouldBe` Right ()

                it "rejects an exact-root RaiseMerge before the end of the restored witness" $ do
                    validateWith
                        flexibleTrace
                        [OpRaiseMerge sourceRoot exterior, OpWeaken interiorNode]
                        `shouldBe`
                            Left (RootRaiseMergeNotTerminal sourceRoot exterior)

                it "rejects multiple exact-root RaiseMerge transitions" $ do
                    let otherExterior = NodeId 693
                    validateWith
                        flexibleTrace
                        [ OpRaiseMerge sourceRoot otherExterior
                        , OpRaiseMerge sourceRoot exterior
                        ]
                        `shouldBe`
                            Left
                                ( MultipleRootRaiseMergeTransitions
                                    sourceRoot
                                    [ (sourceRoot, otherExterior)
                                    , (sourceRoot, exterior)
                                    ]
                                )

                it "rejects a naked terminal root RaiseMerge without flexible trace authority" $ do
                    let unauthorizedTrace =
                            flexibleTrace
                                { etReplayContract = ReplayContractStrict
                                }
                    validateWith unauthorizedTrace [OpRaiseMerge sourceRoot exterior]
                        `shouldBe`
                            Left
                                ( RootRaiseMergeTraceAuthorityMissing
                                    sourceRoot
                                    exterior
                                )

                it "accepts an adjacent root Weaken/RaiseMerge through rigid trace authority" $ do
                    rootRaiseMergeTraceAuthority sourceRoot exterior rigidTrace
                        `shouldBe` False
                    rootWeakenRaiseMergeTraceAuthority sourceRoot exterior rigidTrace
                        `shouldBe` True
                    validateWith
                        rigidTrace
                        [OpWeaken sourceRoot, OpRaiseMerge sourceRoot exterior]
                        `shouldBe` Right ()

                it "rejects an adjacent root Weaken/RaiseMerge without rigid trace authority" $ do
                    let unauthorizedTrace =
                            flexibleTrace
                                { etInterior = sourceInteriorFromList [sourceRoot, exterior]
                                }
                    validateWith
                        unauthorizedTrace
                        [OpWeaken sourceRoot, OpRaiseMerge sourceRoot exterior]
                        `shouldBe`
                            Left
                                ( RootWeakenRaiseMergeTraceAuthorityMissing
                                    sourceRoot
                                    exterior
                                )

            it "preserves exactly one source-root RaiseMerge into the exterior without replay" $ do
                let edgeId = 25
                    sourceRoot = NodeId 700
                    exterior = NodeId 710
                    nodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyVar sourceRoot Nothing)
                            , (getNodeId exterior, TyVar exterior Nothing)
                            ]
                    c =
                        constraintWithOwners
                            nodes
                            [ (GenNodeId 700, [sourceRoot])
                            , (GenNodeId 710, [exterior])
                            ]
                            []
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = exterior
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness =
                                InstanceWitness
                                    [ OpRaise sourceRoot
                                    , OpMerge sourceRoot exterior
                                    ]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = sourceRoot
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromList [sourceRoot]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap = mempty
                            , etReplayContract = ReplayContractNone
                            }
                    st0 =
                        mkWitnessNormState
                            c
                            IntMap.empty
                            720
                            edgeId
                            edgeWitness
                            edgeTrace
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left err ->
                        expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                    Right (_, st') ->
                        case IntMap.lookup edgeId (psEdgeWitnesses st') of
                            Nothing ->
                                expectationFailure "Expected normalized witness in psEdgeWitnesses"
                            Just ew' ->
                                getInstanceOps (ewWitness ew')
                                    `shouldBe` [OpRaiseMerge sourceRoot exterior]

            it "drops a source-root RaiseMerge after its edge endpoints coalesce" $ do
                let edgeId = 252
                    sourceRoot = NodeId 7020
                    exterior = NodeId 7021
                    nodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyVar sourceRoot Nothing)
                            , (getNodeId exterior, TyVar exterior Nothing)
                            ]
                    c =
                        constraintWithOwners
                            nodes
                            [ (GenNodeId 7020, [sourceRoot])
                            , (GenNodeId 7021, [exterior])
                            ]
                            []
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = exterior
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness =
                                InstanceWitness
                                    [ OpRaise sourceRoot
                                    , OpMerge sourceRoot exterior
                                    ]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = sourceRoot
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromList [sourceRoot]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap = mempty
                            , etReplayContract = ReplayContractNone
                            }
                    unionFind = IntMap.singleton (getNodeId exterior) sourceRoot
                    st0 =
                        mkWitnessNormState
                            c
                            unionFind
                            7030
                            edgeId
                            edgeWitness
                            edgeTrace
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left err ->
                        expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                    Right (_, st') ->
                        case IntMap.lookup edgeId (psEdgeWitnesses st') of
                            Nothing ->
                                expectationFailure "Expected normalized witness in psEdgeWitnesses"
                            Just ew' ->
                                getInstanceOps (ewWitness ew') `shouldBe` []

            it "does not infer replay for a source-root RaiseMerge from unrelated live binders" $ do
                let edgeId = 251
                    sourceRoot = NodeId 7000
                    body = NodeId 7001
                    replayA = NodeId 7002
                    replayB = NodeId 7003
                    exterior = NodeId 7010
                    nodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyForall sourceRoot body)
                            , (getNodeId body, TyArrow body replayA replayB)
                            , (getNodeId replayA, TyVar replayA Nothing)
                            , (getNodeId replayB, TyVar replayB Nothing)
                            , (getNodeId exterior, TyVar exterior Nothing)
                            ]
                    c =
                        constraintWithOwners
                            nodes
                            [ (GenNodeId 7000, [sourceRoot, body, replayA, replayB])
                            , (GenNodeId 7010, [exterior])
                            ]
                            [ (body, sourceRoot, BindFlex)
                            , (replayA, sourceRoot, BindFlex)
                            , (replayB, sourceRoot, BindFlex)
                            ]
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = exterior
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness =
                                InstanceWitness
                                    [ OpRaise sourceRoot
                                    , OpMerge sourceRoot exterior
                                    ]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = sourceRoot
                            , etBinderArgs = []
                            , etInterior =
                                sourceInteriorFromList
                                    [sourceRoot, body, replayA, replayB]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap = mempty
                            , etReplayContract = ReplayContractNone
                            }
                    st0 =
                        mkWitnessNormState
                            c
                            IntMap.empty
                            7020
                            edgeId
                            edgeWitness
                            edgeTrace
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left err ->
                        expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                    Right (_, st') -> do
                        case IntMap.lookup edgeId (psEdgeWitnesses st') of
                            Nothing ->
                                expectationFailure "Expected normalized witness in psEdgeWitnesses"
                            Just ew' ->
                                getInstanceOps (ewWitness ew')
                                    `shouldBe` [OpRaiseMerge sourceRoot exterior]
                        case IntMap.lookup edgeId (psEdgeTraces st') of
                            Nothing ->
                                expectationFailure "Expected normalized trace in psEdgeTraces"
                            Just trace -> do
                                etReplayContract trace `shouldBe` ReplayContractNone
                                etBinderReplayMap trace `shouldBe` IntMap.empty
                                etReplayDomainBinders trace `shouldBe` []

            it "preserves a provenance-backed interior RaiseMerge without replay" $ do
                let edgeId = 26
                    sourceRoot = NodeId 720
                    interiorNode = NodeId 721
                    exterior = NodeId 730
                    nodes =
                        nodeMapFromList
                            [ (getNodeId sourceRoot, TyArrow sourceRoot interiorNode interiorNode)
                            , (getNodeId interiorNode, TyVar interiorNode Nothing)
                            , (getNodeId exterior, TyVar exterior Nothing)
                            ]
                    c =
                        constraintWithOwners
                            nodes
                            [ (GenNodeId 720, [sourceRoot])
                            , (GenNodeId 730, [exterior])
                            ]
                            [(interiorNode, sourceRoot, BindFlex)]
                    rawOp = OpRaiseMerge interiorNode exterior
                    edgeWitness =
                        EdgeWitness
                            { ewEdgeId = EdgeId edgeId
                            , ewLeft = sourceRoot
                            , ewRight = exterior
                            , ewRoot = sourceRoot
                            , ewForallIntros = 0
                            , ewWitness = InstanceWitness [rawOp]
                            }
                    edgeTrace =
                        EdgeTrace
                            { etRoot = sourceRoot
                            , etResultRoot = sourceRoot
                            , etBinderArgs = []
                            , etInterior = sourceInteriorFromList [sourceRoot, interiorNode]
                            , etBinderReplayMap = IntMap.empty
                            , etReplayDomainBinders = []
                            , etCopyMap = mempty
                            , etReplayContract = ReplayContractNone
                            }
                    st0 =
                        mkWitnessNormState
                            c
                            IntMap.empty
                            740
                            edgeId
                            edgeWitness
                            edgeTrace
                case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                    Left err ->
                        expectationFailure
                            ("Expected source-interior RaiseMerge preservation, got: " ++ show err)
                    Right (_, st') ->
                        case IntMap.lookup edgeId (psEdgeWitnesses st') of
                            Nothing ->
                                expectationFailure "Expected normalized witness in psEdgeWitnesses"
                            Just ew' ->
                                getInstanceOps (ewWitness ew') `shouldBe` [rawOp]

        it "normalization rejects residual non-root replay-family ops when no-replay projection cannot eliminate them" $ do
            let edgeId = 22
                root = NodeId 320
                source = NodeId 321
                argNode = NodeId 322
                rogueTarget = NodeId 323
                nodes =
                    nodeMapFromList
                        [ (getNodeId root, TyArrow root source source)
                        , (getNodeId source, TyVar { tnId = source, tnBound = Nothing })
                        , (getNodeId argNode, TestTyBase argNode (BaseTy "Int"))
                        , (getNodeId rogueTarget, TyVar { tnId = rogueTarget, tnBound = Nothing })
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
                        , ewWitness = InstanceWitness [OpGraft argNode rogueTarget, OpWeaken rogueTarget]
                        }
                edgeTrace =
                    EdgeTrace
                        { etRoot = root
                        , etResultRoot = root
                        , etBinderArgs = [(source, argNode)]
                        , etInterior =
                            sourceInteriorFromSet
                                (IntSet.fromList
                                    [ getNodeId root
                                    , getNodeId source
                                    , getNodeId argNode
                                    , getNodeId rogueTarget
                                    ]
                                )
                        , etBinderReplayMap = IntMap.empty
                        , etReplayDomainBinders = []
                        , etCopyMap = mempty
                        , etReplayContract = ReplayContractNone
                        }
                st0 =
                    PresolutionStateInternal
                        { psConstraint = c
                        , psPresolution = Presolution IntMap.empty
                        , psUnionFind = IntMap.empty
                        , psNextNodeId = 350
                        , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                        , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                        , psExpansionResults = emptyExpansionResultMap
                        , psEdgeExecutionArtifacts =
                            singletonEdgeExecutionArtifactsForTest
                                edgeId
                                edgeWitness
                                edgeTrace
                        }
            case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                Left (WitnessNormalizationError (EdgeId eid) (ReplayContractNoneRequiresReplay op)) -> do
                    eid `shouldBe` edgeId
                    op `shouldBe` OpGraft argNode rogueTarget
                Left err ->
                    expectationFailure ("Expected ReplayContractNoneRequiresReplay, got: " ++ show err)
                Right _ ->
                    expectationFailure "Expected fail-fast no-replay residual non-root op rejection"

        it "normalization prunes no-replay non-root raise wrappers before Phi" $ do
            let edgeId = 23
                root = NodeId 330
                raised = NodeId 331
                nodes =
                    nodeMapFromList
                        [ (getNodeId root, TyArrow root raised raised)
                        , (getNodeId raised, TyVar { tnId = raised, tnBound = Nothing })
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
                        , ewWitness = InstanceWitness [OpRaise raised]
                        }
                edgeTrace =
                    EdgeTrace
                        { etRoot = root
                        , etResultRoot = root
                        , etBinderArgs = []
                        , etInterior = sourceInteriorFromList [root, raised]
                        , etBinderReplayMap = IntMap.empty
                        , etReplayDomainBinders = []
                        , etCopyMap = mempty
                        , etReplayContract = ReplayContractNone
                        }
                st0 =
                    PresolutionStateInternal
                        { psConstraint = c
                        , psPresolution = Presolution IntMap.empty
                        , psUnionFind = IntMap.empty
                        , psNextNodeId = 340
                        , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                        , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                        , psExpansionResults = emptyExpansionResultMap
                        , psEdgeExecutionArtifacts =
                            singletonEdgeExecutionArtifactsForTest
                                edgeId
                                edgeWitness
                                edgeTrace
                        }
            case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                Left err ->
                    expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                Right (_, st') ->
                    case IntMap.lookup edgeId (psEdgeWitnesses st') of
                        Nothing ->
                            expectationFailure "Expected normalized witness in psEdgeWitnesses"
                        Just ew' ->
                            getInstanceOps (ewWitness ew') `shouldBe` []

        it "empty replay-domain lane prunes synthesized-wrapper raise-family ops during normalization" $ do
            let edgeId = 3
                root = NodeId 400
                wrapper = NodeId 401
                owner = GenNodeId 40
                nodes =
                    nodeMapFromList
                        [ (getNodeId root, TyBottom root)
                        , (getNodeId wrapper, TyExp wrapper (ExpVarId (-1)) root)
                        ]
                c = rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cBindParents =
                        IntMap.fromList
                            [ (nodeRefKey (typeRef root), (genRef owner, BindFlex))
                            ]
                    }
                edgeWitness =
                    EdgeWitness
                        { ewEdgeId = EdgeId edgeId
                        , ewLeft = wrapper
                        , ewRight = root
                        , ewRoot = root
                        , ewForallIntros = 0
                        , ewWitness = InstanceWitness [OpRaise root]
                        }
                edgeTrace =
                    EdgeTrace
                        { etRoot = root
                        , etResultRoot = root
                        , etBinderArgs = []
                        , etInterior =
                            sourceInteriorFromSet
                                (IntSet.fromList
                                    [ getNodeId root
                                    ]
                                )
                        , etBinderReplayMap = IntMap.empty
                        , etReplayDomainBinders = []
                        , etCopyMap = mempty
                        , etReplayContract = ReplayContractNone
                        }
                st0 =
                    PresolutionStateInternal
                        { psConstraint = c
                        , psPresolution = Presolution IntMap.empty
                        , psUnionFind = IntMap.empty
                        , psNextNodeId = 450
                        , psPendingWeakens = IntSet.empty
                        , psPendingWeakenOwners = IntMap.empty
                        , psWeakenReplayCertificates = IntMap.empty
                        , psBinderCache = IntMap.empty
                            , psGraphVersion = 0
                            , psUnionFindVersion = 0
                            , psBindParentsVersion = 0
                            , psBindingModelCache = Nothing
                            , psEdgeLocalSnapshot = Nothing
                            , psBindingRepairCache = Nothing
                            , psBindingRepairDirty = Nothing
                            , psCachedRootGen = Nothing
                        , psExpansionResults = emptyExpansionResultMap
                        , psEdgeExecutionArtifacts =
                            singletonEdgeExecutionArtifactsForTest
                                edgeId
                                edgeWitness
                                edgeTrace
                        }
            case runPresolutionM defaultTraceConfig st0 normalizeEdgeWitnessesM of
                Left err ->
                    expectationFailure ("normalizeEdgeWitnessesM failed: " ++ show err)
                Right (_, st') ->
                    case IntMap.lookup edgeId (psEdgeWitnesses st') of
                        Nothing ->
                            expectationFailure "Expected normalized witness in psEdgeWitnesses"
                        Just ew' ->
                            getInstanceOps (ewWitness ew') `shouldBe` []

  where
    constraintWithOwners nodes owners childBindings =
        emptyConstraint
            { cNodes = nodes
            , cGenNodes =
                fromListGen
                    [ (owner, GenNode owner roots)
                    | (owner, roots) <- owners
                    ]
            , cBindParents =
                IntMap.union
                    (bindParentsFromPairs childBindings)
                    ( IntMap.fromList
                        [ ( nodeRefKey (typeRef root)
                          , (genRef owner, BindFlex)
                          )
                        | (owner, roots) <- owners
                        , root <- roots
                        ]
                    )
            }

    mkWitnessNormState c uf nextNode edgeId edgeWitness edgeTrace =
        PresolutionStateInternal
            { psConstraint = c
            , psPresolution = Presolution IntMap.empty
            , psUnionFind = uf
            , psNextNodeId = nextNode
            , psPendingWeakens = IntSet.empty
            , psPendingWeakenOwners = IntMap.empty
            , psWeakenReplayCertificates = IntMap.empty
            , psBinderCache = IntMap.empty
            , psGraphVersion = 0
            , psUnionFindVersion = 0
            , psBindParentsVersion = 0
            , psBindingModelCache = Nothing
            , psEdgeLocalSnapshot = Nothing
            , psBindingRepairCache = Nothing
            , psBindingRepairDirty = Nothing
            , psCachedRootGen = Nothing
            , psExpansionResults = emptyExpansionResultMap
            , psEdgeExecutionArtifacts =
                singletonEdgeExecutionArtifactsForTest
                    edgeId
                    edgeWitness
                    edgeTrace
            }

    setEdgeRaiseAuthority edgeKey authority =
        modifyEdgeExecutionArtifacts edgeKey $ \artifacts ->
            artifacts {eeaRaiseAuthorityNodes = authority}

    setEdgeNonSourceOpOrigins edgeKey origins =
        modifyEdgeExecutionArtifacts edgeKey $ \artifacts ->
            artifacts {eeaNonSourceOpOrigins = origins}

    modifyEdgeExecutionArtifacts edgeKey updateArtifacts st =
        case IntMap.lookup edgeKey (psEdgeExecutionArtifacts st) of
            Nothing ->
                error
                    ( "missing edge execution artifacts in witness fixture: "
                        ++ show (EdgeId edgeKey)
                    )
            Just artifacts ->
                st
                    { psEdgeExecutionArtifacts =
                        IntMap.insert
                            edgeKey
                            (updateArtifacts artifacts)
                            (psEdgeExecutionArtifacts st)
                    }

    opNodeIds :: InstanceOp -> [NodeId]
    opNodeIds op = case op of
        OpGraft n m -> [n, m]
        OpMerge n m -> [n, m]
        OpRaise n -> [n]
        OpWeaken n -> [n]
        OpRaiseMerge n m -> [n, m]

    isOpGraft :: InstanceOp -> Bool
    isOpGraft OpGraft{} = True
    isOpGraft _ = False

    isOpMerge :: InstanceOp -> Bool
    isOpMerge OpMerge{} = True
    isOpMerge _ = False

    isOpRaise :: InstanceOp -> Bool
    isOpRaise OpRaise{} = True
    isOpRaise _ = False

    isOpWeaken :: InstanceOp -> Bool
    isOpWeaken OpWeaken{} = True
    isOpWeaken _ = False

    isOpRaiseMerge :: InstanceOp -> Bool
    isOpRaiseMerge OpRaiseMerge{} = True
    isOpRaiseMerge _ = False
