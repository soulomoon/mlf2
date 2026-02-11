module Presolution.ExpansionSpec (spec) where

import Test.Hspec
import Control.Monad.Except (throwError)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE

import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Types.Witness
    ( BoundRef(..)
    , Expansion(..)
    , ForallSpec(..)
    )
import MLF.Constraint.Presolution
    ( PresolutionError(..)
    , PresolutionResult(..)
    , PresolutionState(..)
    , applyExpansion
    , computePresolution
    , mergeExpansions
    , runPresolutionM
    )
import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import MLF.Constraint.Solve (SolveResult(..), validateSolvedGraphStrict)
import qualified MLF.Binding.Tree as Binding
import SpecUtil
    ( defaultTraceConfig
    , emptyConstraint
    , inferBindParents
    , lookupNodeMaybe
    , nodeMapElems
    , nodeMapFromList
    , nodeMapMember
    , rootedConstraint
    )
import Presolution.Util (expectForall, nodeAt)

spec :: Spec
spec = do
    describe "decideMinimalExpansion" $ do
        it "returns ExpIdentity for matching monomorphic types" $ do
            let bodyId = NodeId 0
                targetId = NodeId 1
                expNodeId = NodeId 2
                rootId = NodeId 3
                nodes = nodeMapFromList
                        [ (0, TyBase bodyId (BaseTy "int"))
                        , (1, TyBase targetId (BaseTy "int"))
                        , (2, TyExp expNodeId (ExpVarId 0) bodyId)
                        , (3, TyArrow rootId expNodeId targetId)
                        ]
                edge = InstEdge (EdgeId 0) expNodeId targetId
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge]
                        , arDepGraph = undefined -- Not used by computePresolution currently
                        }

            case computePresolution defaultTraceConfig acyclicityRes constraint of
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
                nodes = nodeMapFromList
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
                    IntMap.insert (nodeRefKey (typeRef varId)) (typeRef forallId, BindFlex) bindParents0
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
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prConstraint = c', prEdgeExpansions = exps } -> do
                    case IntMap.lookup 0 exps of
                        Just (ExpInstantiate _) -> pure ()
                        other -> expectationFailure $ "Expected ExpInstantiate, got " ++ show other

                    let nodes' = cNodes c'
                    case (lookupNodeMaybe nodes' (NodeId 2), lookupNodeMaybe nodes' (NodeId 5)) of
                        (Just _, Just _) -> pure ()
                        _ -> expectationFailure "Nodes 2 and 5 should remain distinct"

        it "keeps ExpInstantiate for forall <= non-forall level mismatch" $ do
            let srcBinderId = NodeId 0
                srcForallId = NodeId 1
                expNodeId = NodeId 2
                targetVarId = NodeId 3
                rootId = NodeId 4
                rootGen = GenNodeId 0
                srcGen = GenNodeId 10
                tgtGen = GenNodeId 11
                nodes = nodeMapFromList
                    [ (getNodeId srcBinderId, TyVar { tnId = srcBinderId, tnBound = Nothing })
                    , (getNodeId srcForallId, TyForall srcForallId srcBinderId)
                    , (getNodeId expNodeId, TyExp expNodeId (ExpVarId 0) srcForallId)
                    , (getNodeId targetVarId, TyVar { tnId = targetVarId, tnBound = Nothing })
                    , (getNodeId rootId, TyArrow rootId expNodeId targetVarId)
                    ]
                edge = InstEdge (EdgeId 0) expNodeId targetVarId
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
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps } ->
                    case IntMap.lookup 0 exps of
                        Just (ExpInstantiate args) -> length args `shouldBe` 1
                        Just other -> expectationFailure $ "Expected ExpInstantiate, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for Edge 0"

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

                -- Make the target forall bind both variables directly (flex) so
                -- orderedBinders sees arity 2.
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
                nodes = nodeMapFromList
                        [ (0, TyVar { tnId = srcVarId, tnBound = Nothing })
                        , (1, TyForall srcForallId srcVarId)
                        , (2, TyVar { tnId = tgtVarId, tnBound = Nothing })
                        , (3, TyForall tgtForallId tgtVarId)
                        , (4, TyExp expNodeId (ExpVarId 0) srcForallId)
                        , (5, TyArrow rootId expNodeId tgtForallId)
                        ]
                edge = InstEdge (EdgeId 0) expNodeId tgtForallId
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps, prConstraint = c' } -> do
                    case IntMap.lookup 0 exps of
                        Just ExpIdentity -> pure ()
                        other -> expectationFailure $ "Expected ExpIdentity, got " ++ show other

                    let nodes' = cNodes c'
                    case (lookupNodeMaybe nodes' srcVarId, lookupNodeMaybe nodes' tgtVarId) of
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

                nodes = nodeMapFromList
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
                        (nodeRefKey (typeRef tgtBinderId))
                        (typeRef tgtForallId, BindFlex)
                        (inferBindParents nodes)
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            let isOccursCheck err = case err of
                        OccursCheckPresolution{} -> True
                        PlanError inner -> isOccursCheck inner
                        ExecError inner -> isOccursCheck inner
                        _ -> False

            case computePresolution defaultTraceConfig acyclicityRes constraint of
                Left err | isOccursCheck err -> pure ()
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
                nodes = nodeMapFromList
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
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution defaultTraceConfig acyclicityRes constraint of
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

                nodes = nodeMapFromList
                    [ (0, TyVar { tnId = srcVarId, tnBound = Nothing })
                    , (1, TyForall srcForallId srcVarId)
                    , (2, TyVar { tnId = tgtVarId, tnBound = Nothing })
                    , (3, TyForall tgtForallId tgtVarId)
                    , (4, TyExp expNodeId (ExpVarId 0) srcForallId)
                    ]

                edge = InstEdge (EdgeId 0) expNodeId tgtForallId
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution defaultTraceConfig acyclicityRes constraint of
                Left other -> expectationFailure $ "Unexpected error: " ++ show other
                Right _ -> pure ()

    describe "Error Conditions" $ do
        it "reports UnmatchableTypes when explicitly thrown" $ do
            let st0 = PresolutionState emptyConstraint (Presolution IntMap.empty) IntMap.empty 0 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                n1 = NodeId 1
                n2 = NodeId 2
                msg = "test mismatch"

            case runPresolutionM defaultTraceConfig st0 (throwError (UnmatchableTypes n1 n2 msg)) of
                Left (UnmatchableTypes n1' n2' msg') -> do
                    n1' `shouldBe` n1
                    n2' `shouldBe` n2
                    msg' `shouldBe` msg
                Left err -> expectationFailure $ "Expected UnmatchableTypes, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "reports UnresolvedExpVar when explicitly thrown" $ do
            let st0 = PresolutionState emptyConstraint (Presolution IntMap.empty) IntMap.empty 0 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                v = ExpVarId 123

            case runPresolutionM defaultTraceConfig st0 (throwError (UnresolvedExpVar v)) of
                Left (UnresolvedExpVar v') -> v' `shouldBe` v
                Left err -> expectationFailure $ "Expected UnresolvedExpVar, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "reports InternalError when explicitly thrown" $ do
            let st0 = PresolutionState emptyConstraint (Presolution IntMap.empty) IntMap.empty 0 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                msg = "test internal error"

            case runPresolutionM defaultTraceConfig st0 (throwError (InternalError msg)) of
                Left (InternalError msg') -> msg' `shouldBe` msg
                Left err -> expectationFailure $ "Expected InternalError, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "reports ArityMismatch when merging ExpInstantiate with different lengths" $ do
            let st0 = PresolutionState emptyConstraint (Presolution IntMap.empty) IntMap.empty 0 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                exp1 = ExpInstantiate [NodeId 1]
                exp2 = ExpInstantiate [NodeId 1, NodeId 2]

            -- mergeExpansions is internal, but we can access it via a helper or by constructing
            -- a scenario where processInstEdge hits this case.
            -- Using runPresolutionM to call mergeExpansions directly is cleaner.

            case runPresolutionM defaultTraceConfig st0 (mergeExpansions (ExpVarId 0) exp1 exp2) of
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
                nodes = nodeMapFromList
                    [ (0, TyExp expNodeId (ExpVarId 0) bodyId)
                    , (1, TyBase bodyId (BaseTy "int"))
                    ]
                constraint = rootedConstraint $ emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 2 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                expansion = ExpInstantiate [NodeId 2] -- dummy arg

            case runPresolutionM defaultTraceConfig st0 (applyExpansion expansion (nodeAt nodes 0)) of
                Left (InstantiateOnNonForall nid) -> nid `shouldBe` bodyId
                Left err -> expectationFailure $ "Expected InstantiateOnNonForall, got " ++ show err
                Right _ -> expectationFailure "Expected failure"

        it "reports ArityMismatch when applying ExpInstantiate with wrong argument count" $ do
            let expNodeId = NodeId 0
                forallId = NodeId 1
                boundId = NodeId 2
                nodes = nodeMapFromList
                    [ (0, TyExp expNodeId (ExpVarId 0) forallId)
                    , (1, TyForall forallId boundId)
                    , (2, TyVar { tnId = boundId, tnBound = Nothing })
                    ]
                constraint =
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 3 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
                -- Forall has 1 bound var, but we provide 2 args
                expansion = ExpInstantiate [NodeId 3, NodeId 4]

            case runPresolutionM defaultTraceConfig st0 (applyExpansion expansion (nodeAt nodes 0)) of
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
                nodes = nodeMapFromList [(0, TyBase nid (BaseTy "int"))]
                constraint = rootedConstraint $ emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 1 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

                -- Construct an expansion: ExpCompose [ExpForall [1], ExpIdentity]
                -- This will trigger the ExpCompose branch in applyExpansionOverNode
                expansion = ExpCompose (ExpForall (ForallSpec 0 [] NE.:| []) NE.:| [ExpIdentity])

            case runPresolutionM defaultTraceConfig st0 (applyExpansion expansion (nodeAt nodes 0)) of
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

                nodes = nodeMapFromList
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
                    PresolutionState constraint (Presolution IntMap.empty)
                        IntMap.empty
                        5
                        IntSet.empty
                        IntMap.empty
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

            case runPresolutionM defaultTraceConfig st0 (applyExpansion expansion (nodeAt nodes (getNodeId expNodeId))) of
                Left err -> expectationFailure $ "Expansion failed: " ++ show err
                Right (forallId, st1) -> do
                    let c1 = psConstraint st1
                        nodes1 = cNodes c1
                        bp1 = cBindParents c1
                        boundOf nid =
                            case lookupNodeMaybe nodes1 nid of
                                Just TyVar{ tnBound = mb } -> mb
                                _ -> Nothing

                    forallNode <- expectForall nodes1 forallId
                    tnBody forallNode `shouldBe` arrowId

                    IntMap.lookup (nodeRefKey (typeRef arrowId)) bp1 `shouldBe` Just (typeRef forallId, BindFlex)
                    IntMap.lookup (nodeRefKey (typeRef domVarId)) bp1 `shouldBe` Just (typeRef forallId, BindFlex)
                    IntMap.lookup (nodeRefKey (typeRef codVarId)) bp1 `shouldBe` Just (typeRef forallId, BindFlex)

                    boundOf domVarId `shouldBe` Just codVarId
                    boundOf codVarId `shouldBe` Just bndId

                    case Binding.orderedBinders id c1 (typeRef forallId) of
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
                nodes = nodeMapFromList
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
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge1, edge2]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes =
                    AcyclicityResult
                        { arSortedEdges = [edge1, edge2]
                        , arDepGraph = undefined
                        }

            case computePresolution defaultTraceConfig acyclicityRes constraint of
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
                nodes = nodeMapFromList
                        [ (0, TyVar { tnId = boundId, tnBound = Nothing })
                        , (1, TyForall forallId boundId)
                        , (2, TyExp expNodeId (ExpVarId 0) forallId)
                        , (3, TyBase targetId (BaseTy "int"))
                        , (4, TyArrow rootId expNodeId targetId)
                        ]
                edge1 = InstEdge (EdgeId 0) expNodeId targetId
                edge2 = InstEdge (EdgeId 1) expNodeId targetId
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge1, edge2]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge1, edge2], arDepGraph = undefined }

            case computePresolution defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prEdgeExpansions = exps, prConstraint = c' } -> do
                    case (IntMap.lookup 0 exps, IntMap.lookup 1 exps) of
                        (Just (ExpInstantiate [n1]), Just (ExpInstantiate [n2])) -> do
                            n1 `shouldBe` n2
                            nodeMapMember n1 (cNodes c') `shouldBe` True
                        _ -> expectationFailure "Expected merged ExpInstantiate"

        it "materializes expansions and clears inst edges for strict solve" $ do
            let bound = NodeId 0
                forallId = NodeId 1
                expId = NodeId 2
                targetId = NodeId 3
                rootId = NodeId 4
                nodes = nodeMapFromList
                        [ (0, TyVar { tnId = bound, tnBound = Nothing })
                        , (1, TyForall forallId bound)
                        , (2, TyExp expId (ExpVarId 0) forallId)
                        , (3, TyBase targetId (BaseTy "int"))
                        , (4, TyArrow rootId expId targetId)
                        ]
                edge = InstEdge (EdgeId 0) expId targetId
                constraint =
                    rootedConstraint emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = inferBindParents nodes
                        }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

                isExp TyExp{} = True
                isExp _ = False

            case computePresolution defaultTraceConfig acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prConstraint = c' } -> do
                    any isExp (nodeMapElems (cNodes c')) `shouldBe` False
                    cInstEdges c' `shouldBe` []
                    validateSolvedGraphStrict SolveResult{ srConstraint = c', srUnionFind = IntMap.empty }
                        `shouldBe` []

    describe "Phase 4 regression matrix" $ do
        it "covers identity, instantiate, forall-intro, and compose constructors" $ do
            let expansionMatrix =
                    [ ExpIdentity
                    , ExpInstantiate [NodeId 0]
                    , ExpForall (ForallSpec 1 [Nothing] NE.:| [])
                    , ExpCompose
                        ( ExpInstantiate [NodeId 1]
                            NE.:| [ExpForall (ForallSpec 1 [Nothing] NE.:| [])]
                        )
                    ]
                tag expansion = case expansion of
                    ExpIdentity -> "identity"
                    ExpInstantiate _ -> "instantiate"
                    ExpForall _ -> "forall-intro"
                    ExpCompose _ -> "compose"
            map tag expansionMatrix `shouldBe` ["identity", "instantiate", "forall-intro", "compose"]
