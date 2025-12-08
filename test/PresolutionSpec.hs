module PresolutionSpec (spec) where

import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.NonEmpty as NE

import MLF.Types
import MLF.Presolution
import MLF.Acyclicity (AcyclicityResult(..))

gNode :: Int -> GNode
gNode n = GNode (GNodeId n) Nothing [] []

emptyConstraint :: Constraint
emptyConstraint = Constraint
    { cNodes = IntMap.empty
    , cGNodes = IntMap.empty
    , cInstEdges = []
    , cUnifyEdges = []
    , cGForest = []
    }

lookupNode :: IntMap.IntMap TyNode -> NodeId -> Maybe TyNode
lookupNode nodes nid = IntMap.lookup (getNodeId nid) nodes

expectArrow :: HasCallStack => IntMap.IntMap TyNode -> NodeId -> IO TyNode
expectArrow nodes nid = case lookupNode nodes nid of
    Just a@TyArrow{} -> return a
    other -> do
        let msg = "Expected TyArrow at " ++ show nid ++ ", found " ++ show other
        expectationFailure msg >> fail msg

expectForall :: HasCallStack => IntMap.IntMap TyNode -> NodeId -> IO TyNode
expectForall nodes nid = case lookupNode nodes nid of
    Just f@TyForall{} -> return f
    other -> do
        let msg = "Expected TyForall at " ++ show nid ++ ", found " ++ show other
        expectationFailure msg >> fail msg

expectExp :: HasCallStack => IntMap.IntMap TyNode -> NodeId -> IO TyNode
expectExp nodes nid = case lookupNode nodes nid of
    Just e@TyExp{} -> return e
    other -> do
        let msg = "Expected TyExp at " ++ show nid ++ ", found " ++ show other
        expectationFailure msg >> fail msg

spec :: Spec
spec = describe "Phase 4 — Principal Presolution" $ do
    describe "instantiateScheme" $ do
        it "replaces repeated bound vars with the same fresh node" $ do
            -- forall@1. (a -> a) where a is bound at level 1
            let bound = NodeId 1
                body = NodeId 2
                fresh = NodeId 10
                nodes = IntMap.fromList
                    [ (1, TyVar bound (GNodeId 1))
                    , (2, TyArrow body bound bound)
                    , (10, TyVar fresh (GNodeId 1)) -- fresh binder image
                    ]
                constraint = emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11

            case runPresolutionM st0 (instantiateScheme body (GNodeId 1) [(bound, fresh)]) of
                Left err -> expectationFailure $ "Instantiation failed: " ++ show err
                Right (root, st1) -> do
                    arrow <- expectArrow (cNodes (psConstraint st1)) root
                    let d = tnDom arrow
                        c = tnCod arrow
                    d `shouldBe` fresh
                    c `shouldBe` fresh

        it "shares outer-scope variables below the quantifier level" $ do
            -- Body uses bound var (level 1) and outer var (level 0); outer should stay shared.
            let bound = NodeId 1
                outer = NodeId 3
                body = NodeId 2
                fresh = NodeId 10
                nodes = IntMap.fromList
                    [ (1, TyVar bound (GNodeId 1))
                    , (2, TyArrow body bound outer)
                    , (3, TyVar outer (GNodeId 0))
                    , (10, TyVar fresh (GNodeId 1))
                    ]
                constraint = emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11

            case runPresolutionM st0 (instantiateScheme body (GNodeId 1) [(bound, fresh)]) of
                Left err -> expectationFailure $ "Instantiation failed: " ++ show err
                Right (root, st1) -> do
                    arrow <- expectArrow (cNodes (psConstraint st1)) root
                    let d = tnDom arrow
                        c = tnCod arrow
                    d `shouldBe` fresh
                    c `shouldBe` outer -- shared, not copied

        it "copies shared substructure only once (cache reuse)" $ do
            -- Body: (a1 -> a1) used twice as dom/cod; copy should reuse the same new node.
            let bound = NodeId 1
                shared = NodeId 5
                body = NodeId 6
                fresh = NodeId 10
                nodes = IntMap.fromList
                    [ (1, TyVar bound (GNodeId 1))
                    , (5, TyArrow shared bound bound)    -- shared substructure
                    , (6, TyArrow body shared shared)    -- uses shared twice
                    , (10, TyVar fresh (GNodeId 1))
                    ]
                constraint = emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11

            case runPresolutionM st0 (instantiateScheme body (GNodeId 1) [(bound, fresh)]) of
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
                    [ (1, TyVar bound (GNodeId 1))
                    , (2, TyBase base (BaseTy "int"))
                    , (3, TyArrow body base base)
                    , (10, TyVar fresh (GNodeId 1))
                    ]
                constraint = emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11

            case runPresolutionM st0 (instantiateScheme body (GNodeId 1) [(bound, fresh)]) of
                Left err -> expectationFailure $ "Instantiation failed: " ++ show err
                Right (root, st1) -> do
                    arrow <- expectArrow (cNodes (psConstraint st1)) root
                    tnDom arrow `shouldBe` base
                    tnCod arrow `shouldBe` base

        it "copies nested forall inside the body" $ do
            -- Body: forall@1 b. b -> a (a is outer bound); should copy nested forall and freshen inner binders.
            let outer = NodeId 1   -- bound at level 1
                innerVar = NodeId 2
                innerBody = NodeId 3
                innerForall = NodeId 4
                topBody = NodeId 5
                freshOuter = NodeId 10
                freshInner = NodeId 11
                nodes = IntMap.fromList
                    [ (1, TyVar outer (GNodeId 1))
                    , (2, TyVar innerVar (GNodeId 2))
                    , (3, TyArrow innerBody innerVar outer)
                    , (4, TyForall innerForall (GNodeId 2) innerBody)
                    , (5, TyArrow topBody innerForall innerForall)
                    , (10, TyVar freshOuter (GNodeId 1))
                    , (11, TyVar freshInner (GNodeId 2))
                    ]
                constraint = emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 12

            case runPresolutionM st0 (instantiateScheme topBody (GNodeId 1) [(outer, freshOuter), (innerVar, freshInner)]) of
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
                    tnQuantLevel forall1 `shouldBe` GNodeId 2
                    tnQuantLevel forall2 `shouldBe` GNodeId 2
                    innerCopy `shouldBe` innerCopy2
                    tnDom innerArrow `shouldBe` freshInner
                    tnCod innerArrow `shouldBe` freshOuter

        it "copies nested expansion nodes inside the body" $ do
            -- Body: exp s (forall@1 a. a) -- expansion wrapper should be copied with its body.
            let bound = NodeId 1
                forallBody = NodeId 2
                forallNode = NodeId 3
                expNode = NodeId 4
                outerBody = NodeId 5
                fresh = NodeId 10
                nodes = IntMap.fromList
                    [ (1, TyVar bound (GNodeId 1))
                    , (2, TyArrow forallBody bound bound)
                    , (3, TyForall forallNode (GNodeId 1) forallBody)
                    , (4, TyExp expNode (ExpVarId 9) forallNode)
                    , (5, TyArrow outerBody expNode expNode)
                    , (10, TyVar fresh (GNodeId 1))
                    ]
                constraint = emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11

            case runPresolutionM st0 (instantiateScheme outerBody (GNodeId 1) [(bound, fresh)]) of
                Left err -> expectationFailure $ "Instantiation failed: " ++ show err
                Right (root, st1) -> do
                    let nodes' = cNodes (psConstraint st1)
                    arrow <- expectArrow nodes' root
                    let d = tnDom arrow
                        c = tnCod arrow
                    expNode' <- expectExp nodes' d
                    forall' <- expectForall nodes' (tnBody expNode')
                    bodyArrow <- expectArrow nodes' (tnBody forall')
                    tnExpVar expNode' `shouldBe` ExpVarId 9
                    tnQuantLevel forall' `shouldBe` GNodeId 1
                    tnDom bodyArrow `shouldBe` fresh
                    tnCod bodyArrow `shouldBe` fresh
                    d `shouldBe` c

        it "returns error when a node is missing" $ do
            -- Substitution refers to a missing node; should throw NodeLookupFailed.
            let bound = NodeId 1
                body = NodeId 99  -- missing
                fresh = NodeId 10
                nodes = IntMap.fromList
                    [ (1, TyVar bound (GNodeId 1))
                    , (10, TyVar fresh (GNodeId 1))
                    ]
                constraint = emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11

            case runPresolutionM st0 (instantiateScheme body (GNodeId 1) [(bound, fresh)]) of
                Left (NodeLookupFailed nid) -> nid `shouldBe` body
                Left other -> expectationFailure $ "Unexpected error: " ++ show other
                Right _ -> expectationFailure "Expected failure due to missing node"

    describe "decideMinimalExpansion" $ do
        it "returns ExpIdentity for matching monomorphic types" $ do
            -- s . int <= int
            -- Body is int (TyBase)
            -- Target is int (TyBase)
            let bodyId = NodeId 0
                targetId = NodeId 1
                expNodeId = NodeId 2

                nodes = IntMap.fromList
                    [ (0, TyBase bodyId (BaseTy "int"))
                    , (1, TyBase targetId (BaseTy "int"))
                    , (2, TyExp expNodeId (ExpVarId 0) bodyId)
                    ]

                -- We need to mock the state or just call decideMinimalExpansion directly if possible.
                -- But decideMinimalExpansion is in PresolutionM.
                -- We can run it using runPresolutionM helper if we expose one, or just use computePresolution.

            -- Let's use computePresolution for integration testing style
            let edge = InstEdge (EdgeId 0) expNodeId targetId
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    }
                acyclicityRes = AcyclicityResult
                    { arSortedEdges = [edge]
                    , arDepGraph = undefined -- Not used by computePresolution currently
                    }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right res -> do
                    let Presolution m = prPresolution res
                    case IntMap.lookup 0 m of
                        Just ExpIdentity -> return ()
                        Just other -> expectationFailure $ "Expected ExpIdentity, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for ExpVar 0"

        it "returns ExpInstantiate for Forall <= Arrow" $ do
            -- s . (forall a. a -> a) <= (int -> int)
            -- Body: Forall (level 1) -> Arrow (Var 0 -> Var 0)
            -- Target: Arrow (int -> int)

            let varId = NodeId 0
                arrowId = NodeId 1
                forallId = NodeId 2

                targetDomId = NodeId 3
                targetCodId = NodeId 4
                targetArrowId = NodeId 5

                expNodeId = NodeId 6

                nodes = IntMap.fromList
                    [ (0, TyVar varId (GNodeId 1)) -- Bound at level 1
                    , (1, TyArrow arrowId varId varId)
                    , (2, TyForall forallId (GNodeId 1) arrowId)

                    , (3, TyBase targetDomId (BaseTy "int"))
                    , (4, TyBase targetCodId (BaseTy "int"))
                    , (5, TyArrow targetArrowId targetDomId targetCodId)

                    , (6, TyExp expNodeId (ExpVarId 0) forallId)
                    ]

                edge = InstEdge (EdgeId 0) expNodeId targetArrowId
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge]
                    }
                acyclicityRes = AcyclicityResult
                    { arSortedEdges = [edge]
                    , arDepGraph = undefined
                    }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right res -> do
                    let Presolution m = prPresolution res
                    case IntMap.lookup 0 m of
                        Just (ExpInstantiate args) -> do
                            length args `shouldBe` 1
                            -- Verify that unifications happened?
                            -- The result constraint should have union-find updates or unify edges?
                            -- Currently computePresolution returns updated union-find in prUnionFind.
                            -- But we didn't check that.
                            return ()
                        Just other -> expectationFailure $ "Expected ExpInstantiate, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for ExpVar 0"

        it "returns compose (instantiate then forall) when forall levels differ" $ do
            -- s · (forall@1 a. a) ≤ (forall@2 b. b)
            -- Different levels mean we must instantiate the source binder (fresh
            -- at level 2) and then rewrap with ExpForall at the target level.
            -- The expected expansion is ExpCompose [ExpInstantiate [fresh],
            -- ExpForall 2].
            let srcVarId = NodeId 0
                srcForallId = NodeId 1
                tgtVarId = NodeId 2
                tgtForallId = NodeId 3
                expNodeId = NodeId 4

                nodes = IntMap.fromList
                    [ (0, TyVar srcVarId (GNodeId 1))
                    , (1, TyForall srcForallId (GNodeId 1) srcVarId)
                    , (2, TyVar tgtVarId (GNodeId 2))
                    , (3, TyForall tgtForallId (GNodeId 2) tgtVarId)
                    , (4, TyExp expNodeId (ExpVarId 0) srcForallId)
                    ]

                edge = InstEdge (EdgeId 0) expNodeId tgtForallId
                constraint = emptyConstraint { cNodes = nodes, cInstEdges = [edge] }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prPresolution = Presolution m } ->
                    case IntMap.lookup 0 m of
                        Just (ExpCompose (e NE.:| rest)) -> do
                            case e of
                                ExpInstantiate args -> length args `shouldBe` 1
                                other -> expectationFailure $ "Expected ExpInstantiate first, got " ++ show other
                            rest `shouldBe` [ExpForall (GNodeId 2 NE.:| [])]
                        Just other -> expectationFailure $ "Expected composed instantiate+forall, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for ExpVar 0"

        it "keeps identity when forall levels match and requests body unification" $ do
            -- s · (forall@1 a. a) ≤ (forall@1 b. b)
            -- Minimal expansion is identity; the only work is to unify the bodies.
            let srcVarId = NodeId 0
                srcForallId = NodeId 1
                tgtVarId = NodeId 2
                tgtForallId = NodeId 3
                expNodeId = NodeId 4

                nodes = IntMap.fromList
                    [ (0, TyVar srcVarId (GNodeId 1))
                    , (1, TyForall srcForallId (GNodeId 1) srcVarId)
                    , (2, TyVar tgtVarId (GNodeId 1))
                    , (3, TyForall tgtForallId (GNodeId 1) tgtVarId)
                    , (4, TyExp expNodeId (ExpVarId 0) srcForallId)
                    ]
                constraint = emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 5

            let srcExp = nodes IntMap.! 4
                tgtForall = nodes IntMap.! 3

            case runPresolutionM st0 (decideMinimalExpansion srcExp tgtForall) of
                Left err -> expectationFailure $ "Expansion decision failed: " ++ show err
                Right ((expn, unifs), _) -> do
                    expn `shouldBe` ExpIdentity
                    unifs `shouldBe` [(srcVarId, tgtVarId)]

        it "rejects expansions that would point a binder back into its own body" $ do
            -- Edge: s · (forall@1 a. a) ≤ forall@1 b. (s · (forall@1 a. a))
            -- The requested body unification would point the bound var `a`
            -- back into a structure containing the same expansion, which should
            -- be rejected by the presolution occurs-check guard.
            let boundVarId = NodeId 0
                srcForallId = NodeId 1
                srcExpId = NodeId 2
                tgtForallId = NodeId 3

                nodes = IntMap.fromList
                    [ (0, TyVar boundVarId (GNodeId 1))
                    , (1, TyForall srcForallId (GNodeId 1) boundVarId)
                    , (2, TyExp srcExpId (ExpVarId 0) srcForallId)
                    , (3, TyForall tgtForallId (GNodeId 1) srcExpId)
                    ]

                edge = InstEdge (EdgeId 0) srcExpId tgtForallId
                constraint = emptyConstraint { cNodes = nodes, cInstEdges = [edge] }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution acyclicityRes constraint of
                Left OccursCheckPresolution{} -> return ()
                Left other -> expectationFailure $ "Unexpected error: " ++ show other
                Right _ -> expectationFailure "Expected presolution occurs-check failure"

        it "returns ExpForall for structure <= forall" $ do
            -- s · (int -> int) ≤ (forall@3 b. int -> int)
            -- Target is a forall at level 3, source is monomorphic: we should
            -- wrap the source in ExpForall at the target level and unify bodies.
            let srcDomId = NodeId 0
                srcCodId = NodeId 1
                srcArrowId = NodeId 2
                tgtDomId = NodeId 3
                tgtCodId = NodeId 4
                tgtArrowId = NodeId 5
                tgtForallId = NodeId 6
                expNodeId = NodeId 7

                nodes = IntMap.fromList
                    [ (0, TyBase srcDomId (BaseTy "int"))
                    , (1, TyBase srcCodId (BaseTy "int"))
                    , (2, TyArrow srcArrowId srcDomId srcCodId)
                    , (3, TyBase tgtDomId (BaseTy "int"))
                    , (4, TyBase tgtCodId (BaseTy "int"))
                    , (5, TyArrow tgtArrowId tgtDomId tgtCodId)
                    , (6, TyForall tgtForallId (GNodeId 3) tgtArrowId)
                    , (7, TyExp expNodeId (ExpVarId 1) srcArrowId)
                    ]

                edge = InstEdge (EdgeId 0) expNodeId tgtForallId
                constraint = emptyConstraint { cNodes = nodes, cInstEdges = [edge] }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prPresolution = Presolution m } ->
                    case IntMap.lookup 1 m of
                        Just (ExpForall levels) -> levels `shouldBe` (GNodeId 3 NE.:| [])
                        Just other -> expectationFailure $ "Expected ExpForall, got " ++ show other
                        Nothing -> expectationFailure "No expansion found for ExpVar 1"

        it "fails when target forall level is missing" $ do
            -- s · (forall@1 a. a) ≤ (forall@2 b. b)
            -- The target level 2 is missing from cGNodes, so instantiation should
            -- fail fast with MissingGNode rather than silently allocating at an
            -- unknown level. This guards against presolution inventing levels
            -- that the acyclicity phase never created.
            let srcVarId = NodeId 0
                srcForallId = NodeId 1
                tgtVarId = NodeId 2
                tgtForallId = NodeId 3
                expNodeId = NodeId 4

                nodes = IntMap.fromList
                    [ (0, TyVar srcVarId (GNodeId 1))
                    , (1, TyForall srcForallId (GNodeId 1) srcVarId)
                    , (2, TyVar tgtVarId (GNodeId 2))
                    , (3, TyForall tgtForallId (GNodeId 2) tgtVarId)
                    , (4, TyExp expNodeId (ExpVarId 0) srcForallId)
                    ]

                edge = InstEdge (EdgeId 0) expNodeId tgtForallId
                gnodes = IntMap.fromList [ (1, gNode 1) ]
                constraint = emptyConstraint { cNodes = nodes, cInstEdges = [edge], cGNodes = gnodes, cGForest = [GNodeId 1] }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }

            case computePresolution acyclicityRes constraint of
                Left MissingGNode {} -> return ()
                Left other -> expectationFailure $ "Unexpected error: " ++ show other
                Right _ -> expectationFailure "Expected presolution failure due to missing G-node"

    describe "computePresolution" $ do
        it "handles multiple edges correctly" $ do
            -- Two *independent* instantiations of the same scheme: s1 · σ ≤ int
            -- and s2 · σ ≤ bool. Each edge should allocate its own fresh arg and
            -- not alias the other expansion’s instantiation.
            -- (If they shared the same ExpVar, unification would force int ~ bool.)

            let varId = NodeId 0
                forallId = NodeId 1

                target1Id = NodeId 2 -- int
                target2Id = NodeId 3 -- bool

                exp1Id = NodeId 4 -- s1 . sigma
                exp2Id = NodeId 5 -- s2 . sigma

                nodes = IntMap.fromList
                    [ (0, TyVar varId (GNodeId 1))
                    , (1, TyForall forallId (GNodeId 1) varId)

                    , (2, TyBase target1Id (BaseTy "int"))
                    , (3, TyBase target2Id (BaseTy "bool"))

                    , (4, TyExp exp1Id (ExpVarId 1) forallId)
                    , (5, TyExp exp2Id (ExpVarId 2) forallId)
                    ]

                edge1 = InstEdge (EdgeId 0) exp1Id target1Id
                edge2 = InstEdge (EdgeId 1) exp2Id target2Id

                constraint = emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [edge1, edge2]
                    }
                acyclicityRes = AcyclicityResult
                    { arSortedEdges = [edge1, edge2]
                    , arDepGraph = undefined
                    }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right res -> do
                    let Presolution m = prPresolution res
                    case (IntMap.lookup 1 m, IntMap.lookup 2 m) of
                        (Just (ExpInstantiate [n1]), Just (ExpInstantiate [n2])) -> do
                            n1 `shouldNotBe` n2
                        _ -> expectationFailure "Expected two separate instantiations"

        it "merges instantiations when the same ExpVar appears in multiple edges" $ do
            -- Two edges reuse the *same* expansion variable. The second edge should
            -- merge with the first and reuse the original instantiation argument.
            let boundId = NodeId 0
                forallId = NodeId 1
                expNodeId = NodeId 2
                targetId = NodeId 3

                nodes = IntMap.fromList
                    [ (0, TyVar boundId (GNodeId 1))
                    , (1, TyForall forallId (GNodeId 1) boundId)
                    , (2, TyExp expNodeId (ExpVarId 0) forallId)
                    , (3, TyBase targetId (BaseTy "int"))
                    ]

                edge1 = InstEdge (EdgeId 0) expNodeId targetId
                edge2 = InstEdge (EdgeId 1) expNodeId targetId

                constraint = emptyConstraint { cNodes = nodes, cInstEdges = [edge1, edge2] }
                acyclicityRes = AcyclicityResult { arSortedEdges = [edge1, edge2], arDepGraph = undefined }

            case computePresolution acyclicityRes constraint of
                Left err -> expectationFailure $ "Presolution failed: " ++ show err
                Right PresolutionResult{ prPresolution = Presolution m, prUnionFind = uf } -> do
                    case IntMap.lookup 0 m of
                        Just (ExpInstantiate args) -> args `shouldBe` [NodeId 4]
                        other -> expectationFailure $ "Expected merged ExpInstantiate, got " ++ show other

                    let findRoot :: IntMap.IntMap NodeId -> NodeId -> NodeId
                        findRoot ufMap nid = case IntMap.lookup (getNodeId nid) ufMap of
                            Nothing -> nid
                            Just parent -> findRoot ufMap parent

                    findRoot uf (NodeId 4) `shouldBe` findRoot uf (NodeId 5)

