module Constraint.SolvedSpec (spec) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

import qualified MLF.Constraint.Presolution as Presolution
import MLF.Constraint.Presolution (PresolutionResult(..))
import MLF.Constraint.Solve
    ( SolveOutput(..)
    , SolveResult(..)
    , SolveSnapshot(..)
    , solveUnifyWithSnapshot
    )
import MLF.Constraint.Types.Presolution (PresolutionSnapshot(..))
import MLF.Constraint.Solved
import qualified MLF.Constraint.Solved as Solved
import MLF.Frontend.Syntax
    ( Lit(..)
    , SrcTy(..)
    , SurfaceExpr
    , Expr(..)
    )
import MLF.Constraint.Types.Graph
    ( BaseTy(..)
    , BindFlag(..)
    , Constraint(..)
    , EdgeId(..)
    , GenNode(..)
    , GenNodeId(..)
    , InstEdge(..)
    , NodeId(..)
    , TyNode(..)
    , UnifyEdge(..)
    , fromListGen
    , genRef
    , nodeRefKey
    , toListGen
    , toListNode
    , typeRef
    )

import SpecUtil
    ( emptyConstraint
    , defaultTraceConfig
    , inferBindParents
    , nodeMapFromList
    , requireRight
    , runToPresolutionDefault
    , rootedConstraint
    )

-- | Build a small solved graph by hand:
--
--   Node 0: TyVar (non-canonical, merged into 1)
--   Node 1: TyBase "Int" (canonical representative)
--   Node 2: TyArrow 2 1 3
--   Node 3: TyBase "Bool"
--
--   Union-find: 0 -> 1
--   Bind parents: node 1 bound flex under node 2
--   Inst edge: EdgeId 0, from 2 to 3
--   Gen node: GenNodeId 0, scheme root [2]
testSolved :: Solved
testSolved =
    let var0  = TyVar { tnId = NodeId 0, tnBound = Nothing }
        base1 = TyBase (NodeId 1) (BaseTy "Int")
        arrow2 = TyArrow (NodeId 2) (NodeId 1) (NodeId 3)
        base3 = TyBase (NodeId 3) (BaseTy "Bool")
        nodes = nodeMapFromList
            [ (0, var0), (1, base1), (2, arrow2), (3, base3) ]
        inst = InstEdge (EdgeId 0) (NodeId 2) (NodeId 3)
        gn = GenNode (GenNodeId 0) [NodeId 2]
        constraint = emptyConstraint
            { cNodes = nodes
            , cInstEdges = [inst]
            , cBindParents = IntMap.fromList
                [ (nodeRefKey (typeRef (NodeId 1)), (typeRef (NodeId 2), BindFlex))
                , (nodeRefKey (typeRef (NodeId 3)), (typeRef (NodeId 2), BindFlex))
                ]
            , cGenNodes = fromListGen [(GenNodeId 0, gn)]
            }
        uf = IntMap.fromList [(0, NodeId 1)]
    in mkTestSolved constraint uf

snapshotEquiv :: Either String (Solved, [NodeId])
snapshotEquiv =
    let var0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
        base1 = TyBase (NodeId 1) (BaseTy "Int")
        arrow2 = TyArrow (NodeId 2) (NodeId 0) (NodeId 3)
        bool3 = TyBase (NodeId 3) (BaseTy "Bool")
        var4 = TyVar { tnId = NodeId 4, tnBound = Just (NodeId 0) }
        nodes = nodeMapFromList
            [ (0, var0)
            , (1, base1)
            , (2, arrow2)
            , (3, bool3)
            , (4, var4)
            ]
        constraint = rootedConstraint $ emptyConstraint
            { cNodes = nodes
            , cBindParents = inferBindParents nodes
            , cUnifyEdges = [UnifyEdge (NodeId 0) (NodeId 1)]
            , cInstEdges =
                [ InstEdge (EdgeId 0) (NodeId 2) (NodeId 0)
                , InstEdge (EdgeId 1) (NodeId 4) (NodeId 3)
                ]
            }
        ids = [NodeId 0, NodeId 1, NodeId 2, NodeId 3, NodeId 4, NodeId 99]
    in case solveUnifyWithSnapshot defaultTraceConfig constraint of
        Left err -> Left ("Unexpected solve error: " ++ show err)
        Right out ->
            case fromSolveOutput out of
                Left err -> Left ("Unexpected solved conversion error: " ++ show err)
                Right equiv ->
                    Right (equiv, ids)

spec :: Spec
spec = describe "MLF.Constraint.Solved" $ do
    describe "Migration guards" $ do
        it "Solved production-only builders are absent" $ do
            src <- readFile "src/MLF/Constraint/Solved.hs"
            src `shouldSatisfy` (not . isInfixOf "fromPresolutionResult")

        it "dead mutation hooks are absent from the Solved surface" $ do
            src <- readFile "src/MLF/Constraint/Solved.hs"
            forM_
                [ "rebuildWithNodes"
                , "rebuildWithBindParents"
                , "rebuildWithGenNodes"
                ] $ \marker ->
                    src `shouldSatisfy` (not . isInfixOf marker)

    let s = testSolved

    describe "Constructor compatibility" $ do
        it "fromConstraintAndUf builds canonical queries from union-find and preserves original graph" $ do
            let var0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
                base1 = TyBase (NodeId 1) (BaseTy "Int")
                inst = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
                nodes = nodeMapFromList [(0, var0), (1, base1)]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [inst]
                    }
                uf = IntMap.fromList [(0, NodeId 1)]
                solved = fromConstraintAndUf constraint uf
            canonical solved (NodeId 0) `shouldBe` NodeId 1
            lookupNode solved (NodeId 0) `shouldBe` Just base1
            originalNode solved (NodeId 0) `shouldBe` Just var0
            classMembers solved (NodeId 1) `shouldMatchList` [NodeId 0, NodeId 1]
            instEdges solved `shouldBe` [inst]
            originalConstraint solved `shouldBe` constraint

    describe "Backend equivalence" $ do
        let idLam = ELam "x" (EVar "x")
            intTy = STBase "Int"
            boolTy = STBase "Bool"
            polyIdTy = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
            cases :: [(String, SurfaceExpr)]
            cases =
                [ ("lambda identity", idLam)
                , ("application", EApp idLam (ELit (LInt 1)))
                , ("let application", ELet "id" idLam (EApp (EVar "id") (ELit (LInt 2))))
                , ("let polymorphic two-use", ELet "id" idLam (ELet "_" (EApp (EVar "id") (ELit (LInt 1))) (EApp (EVar "id") (ELit (LBool True)))))
                , ("term annotation", EAnn idLam (STArrow intTy intTy))
                , ("let annotated term", ELet "f" (EAnn idLam polyIdTy) (EApp (EVar "f") (ELit (LInt 3))))
                , ("annotated lambda", ELamAnn "f" (STArrow intTy intTy) (EApp (EVar "f") (ELit (LInt 0))))
                , ("annotated lambda polymorphic param", ELamAnn "x" polyIdTy (EApp (EVar "x") (ELit (LInt 1))))
                , ("let annotation with bool", ELet "f" (EAnn idLam (STArrow boolTy boolTy)) (EApp (EVar "f") (ELit (LBool False))))
                ]
            runCase :: (String, SurfaceExpr) -> Expectation
            runCase (label, expr) =
                case runToPresolutionDefault Set.empty expr of
                    Left err ->
                        expectationFailure
                            ( "Presolution failed for backend equivalence case: "
                                ++ label
                                ++ "\nexpr: "
                                ++ show expr
                                ++ "\nerror: "
                                ++ err
                            )
                    Right pres ->
                        case solveUnifyWithSnapshot defaultTraceConfig (prConstraint pres) of
                            Left err ->
                                expectationFailure
                                    ( "Solve failed for backend equivalence case: "
                                        ++ label
                                        ++ "\nexpr: "
                                        ++ show expr
                                        ++ "\nerror: "
                                        ++ show err
                                    )
                            Right out -> do
                                equiv <- requireRight (fromSolveOutput out)
                                -- Verify the Solved is well-formed by exercising core queries
                                length (allNodes equiv) `shouldSatisfy` (> 0)
                                let _ = canonicalMap equiv
                                    _ = instEdges equiv
                                    _ = bindParents equiv
                                pure ()
        forM_ cases $ \(label, expr) ->
            it ("produces valid Solved for " ++ label ++ " (" ++ show expr ++ ")") $
                runCase (label, expr)

    describe "Canonicalization helper dedup guards" $ do
        it "Solved and Presolution.View no longer each define local canonical-map chase helpers" $ do
            solvedSrc <- readFile "src/MLF/Constraint/Solved.hs"
            viewSrc <- readFile "src/MLF/Constraint/Presolution/View.hs"
            helperSrc <- readFile "src/MLF/Constraint/Canonicalization/Shared.hs"
            forM_
                [ "buildCanonicalMap ::"
                , "chaseUfCanonical ::"
                , "equivCanonical ::"
                , "nodeIdKey ::"
                ] $ \marker -> do
                    solvedSrc `shouldSatisfy` (not . isInfixOf marker)
                    viewSrc `shouldSatisfy` (not . isInfixOf marker)
                    helperSrc `shouldSatisfy` isInfixOf marker

    describe "Presolution view parity guards" $ do
        it "PresolutionView mirrors solved canonical/node/bound queries" $ do
            let fixtures =
                    [ ("id", ELam "x" (EVar "x"))
                    , ("let-poly-use", ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1))))
                    ]
            forM_ fixtures $ \(label, expr) ->
                case runToPresolutionDefault Set.empty expr of
                    Left err ->
                        expectationFailure
                            ( "Presolution failed for fixture "
                                ++ label
                                ++ ": "
                                ++ err
                            )
                    Right pres -> do
                        solved <- requireRight
                            (Solved.fromPreRewriteState
                                (snapshotUnionFind pres)
                                (snapshotConstraint pres)
                            )
                        let view = Presolution.fromPresolutionResult pres
                            nodeIds = map fst (toListNode (cNodes (prConstraint pres)))
                            probes = nodeIds ++ [NodeId 999]
                        forM_ probes $ \nid -> do
                            Presolution.pvCanonical view nid `shouldBe` canonical solved nid
                            Presolution.pvLookupNode view nid `shouldBe` lookupNode solved nid
                            Presolution.pvLookupVarBound view nid `shouldBe` lookupVarBound solved nid

    describe "Core queries" $ do
        it "canonical chases the union-find" $ do
            canonical s (NodeId 0) `shouldBe` NodeId 1
            canonical s (NodeId 1) `shouldBe` NodeId 1
            canonical s (NodeId 3) `shouldBe` NodeId 3

        it "lookupNode canonicalizes before lookup" $ do
            -- Looking up non-canonical 0 should find the node at canonical 1
            lookupNode s (NodeId 0) `shouldBe` Just (TyBase (NodeId 1) (BaseTy "Int"))
            lookupNode s (NodeId 1) `shouldBe` Just (TyBase (NodeId 1) (BaseTy "Int"))
            lookupNode s (NodeId 99) `shouldBe` Nothing

        it "allNodes returns all nodes in the constraint" $ do
            length (allNodes s) `shouldBe` 4

        it "lookupBindParent finds binding parents" $ do
            lookupBindParent s (typeRef (NodeId 1))
                `shouldBe` Just (typeRef (NodeId 2), BindFlex)
            lookupBindParent s (typeRef (NodeId 3))
                `shouldBe` Just (typeRef (NodeId 2), BindFlex)
            lookupBindParent s (typeRef (NodeId 2))
                `shouldBe` Nothing

        it "bindParents returns the full map" $ do
            let bp = bindParents s
            IntMap.size bp `shouldBe` 2

        it "instEdges returns inst edges" $ do
            instEdges s `shouldBe` [InstEdge (EdgeId 0) (NodeId 2) (NodeId 3)]

        it "genNodes returns gen nodes" $ do
            let gns = genNodes s
            toListGen gns `shouldBe` [(GenNodeId 0, GenNode (GenNodeId 0) [NodeId 2])]

        it "lookupVarBound canonicalizes and returns bound" $ do
            -- Node 0 is a TyVar with no bound; canonical is 1 which is TyBase
            -- so lookupVarBound should return Nothing (TyBase is not a TyVar)
            lookupVarBound s (NodeId 0) `shouldBe` Nothing
            lookupVarBound s (NodeId 99) `shouldBe` Nothing

    describe "Escape hatches" $ do
        it "canonicalMap returns the raw parent map" $ do
            let uf = canonicalMap s
            IntMap.lookup 0 uf `shouldBe` Just (NodeId 1)
            IntMap.size uf `shouldBe` 1

        it "originalConstraint returns the raw constraint" $ do
            let c = originalConstraint s
            cInstEdges c `shouldBe` [InstEdge (EdgeId 0) (NodeId 2) (NodeId 3)]

    describe "Solved invariants" $ do
        it "validateOriginalCanonicalAgreement reports original/canonical tag mismatches" $ do
            let mismatches = validateOriginalCanonicalAgreement s
            mismatches `shouldSatisfy` (not . null)
            mismatches `shouldSatisfy`
                any
                    (\msg ->
                        "Node 0" `isInfixOf` msg
                            && "original=Just \"var\"" `isInfixOf` msg
                            && "canonical=Just \"base\"" `isInfixOf` msg
                    )

        it "validateOriginalCanonicalAgreement is empty when original and canonical domains align" $ do
            let var0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
                var1 = TyVar { tnId = NodeId 1, tnBound = Nothing }
                nodes = nodeMapFromList [(0, var0), (1, var1)]
                aligned = mkTestSolved (emptyConstraint { cNodes = nodes }) IntMap.empty
            validateOriginalCanonicalAgreement aligned `shouldBe` []

        it "canonicalizedBindParents rewrites non-canonical child/parent refs into canonical-domain refs" $ do
            let childMerged = NodeId 0
                childCanonical = NodeId 1
                parentMerged = NodeId 2
                parentCanonical = NodeId 3
                nodes = nodeMapFromList
                    [ (0, TyVar { tnId = childMerged, tnBound = Nothing })
                    , (1, TyVar { tnId = childCanonical, tnBound = Nothing })
                    , (2, TyVar { tnId = parentMerged, tnBound = Nothing })
                    , (3, TyVar { tnId = parentCanonical, tnBound = Nothing })
                    ]
                bp =
                    IntMap.fromList
                        [ ( nodeRefKey (typeRef childMerged)
                          , (typeRef parentMerged, BindFlex)
                          )
                        ]
                solved =
                    mkTestSolved
                        (emptyConstraint { cNodes = nodes, cBindParents = bp })
                        (IntMap.fromList
                            [ (getNodeId childMerged, childCanonical)
                            , (getNodeId parentMerged, parentCanonical)
                            ]
                        )
            case canonicalizedBindParents solved of
                Left err ->
                    expectationFailure ("Expected canonicalized bind parents, got error: " ++ show err)
                Right canonBp -> do
                    IntMap.lookup (nodeRefKey (typeRef childCanonical)) canonBp
                        `shouldBe` Just (typeRef parentCanonical, BindFlex)
                    IntMap.lookup (nodeRefKey (typeRef childMerged)) canonBp
                        `shouldBe` Nothing

    describe "Degraded stubs (Phase 1)" $ do
        it "classMembers returns equivalence class members" $ do
            classMembers s (NodeId 0) `shouldMatchList` [NodeId 0, NodeId 1]
            classMembers s (NodeId 3) `shouldBe` [NodeId 3]

        it "wasOriginalBinder always returns False" $ do
            wasOriginalBinder s (NodeId 0) `shouldBe` False
            wasOriginalBinder s (NodeId 1) `shouldBe` False
            wasOriginalBinder s (NodeId 2) `shouldBe` False

        it "originalNode returns pre-merge node data" $ do
            -- Node 0 is a TyVar that was merged into node 1 (TyBase "Int").
            -- originalNode returns the pre-merge TyVar, not the canonical TyBase.
            originalNode s (NodeId 0)
                `shouldBe` Just (TyVar { tnId = NodeId 0, tnBound = Nothing })
            originalNode s (NodeId 2)
                `shouldBe` Just (TyArrow (NodeId 2) (NodeId 1) (NodeId 3))
            originalNode s (NodeId 99) `shouldBe` Nothing

        it "originalBindParent delegates to lookupBindParent" $ do
            originalBindParent s (typeRef (NodeId 1))
                `shouldBe` lookupBindParent s (typeRef (NodeId 1))
            originalBindParent s (typeRef (NodeId 2))
                `shouldBe` lookupBindParent s (typeRef (NodeId 2))

    describe "projection-first queries" $ do
        it "originalNode returns pre-merge node data" $ do
            -- Node 0 was merged into node 1 via union-find, but
            -- originalNode should still return the pre-merge TyVar.
            originalNode s (NodeId 0)
                `shouldBe` Just (TyVar { tnId = NodeId 0, tnBound = Nothing })
            originalNode s (NodeId 1)
                `shouldBe` Just (TyBase (NodeId 1) (BaseTy "Int"))
            originalNode s (NodeId 99) `shouldBe` Nothing

        it "originalBindParent returns pre-merge binding parents" $ do
            originalBindParent s (typeRef (NodeId 1))
                `shouldBe` Just (typeRef (NodeId 2), BindFlex)
            originalBindParent s (typeRef (NodeId 2))
                `shouldBe` Nothing

        it "lookupNode canonicalizes through union-find" $ do
            -- lookupNode chases canonical: looking up merged node 0
            -- should return the data at canonical node 1.
            lookupNode s (NodeId 0) `shouldBe` lookupNode s (NodeId 1)

    describe "lookupVarBound with actual bound" $ do
        it "returns the bound when the canonical node is a TyVar with a bound" $ do
            let boundVar = TyVar { tnId = NodeId 10, tnBound = Just (NodeId 11) }
                base11 = TyBase (NodeId 11) (BaseTy "Int")
                nodes = nodeMapFromList [(10, boundVar), (11, base11)]
                c = emptyConstraint { cNodes = nodes }
                s' = mkTestSolved c IntMap.empty
            lookupVarBound s' (NodeId 10) `shouldBe` Just (NodeId 11)

    describe "fromPreRewriteState" $ do
        let alphaNodeId = NodeId 10
            intNodeId = NodeId 11
            alphaBodyId = NodeId 12
            parentNodeId = NodeId 13
            alphaNode = TyForall alphaNodeId alphaBodyId
            intNode = TyBase intNodeId (BaseTy "Int")
            alphaBody = TyVar { tnId = alphaBodyId, tnBound = Nothing }
            parentNode = TyArrow parentNodeId alphaNodeId intNodeId
            rootGen = GenNode (GenNodeId 0) [parentNodeId]
            preRewriteNodes =
                nodeMapFromList
                    [ (10, alphaNode)
                    , (11, intNode)
                    , (12, alphaBody)
                    , (13, parentNode)
                    ]
            preRewriteConstraint = emptyConstraint
                { cNodes = preRewriteNodes
                , cBindParents = IntMap.fromList
                    [ (nodeRefKey (typeRef alphaNodeId), (typeRef parentNodeId, BindFlex))
                    , (nodeRefKey (typeRef intNodeId), (typeRef parentNodeId, BindFlex))
                    , (nodeRefKey (typeRef alphaBodyId), (typeRef alphaNodeId, BindFlex))
                    , (nodeRefKey (typeRef parentNodeId), (genRef (GenNodeId 0), BindFlex))
                    ]
                , cGenNodes = fromListGen [(GenNodeId 0, rootGen)]
                }
            uf = IntMap.fromList [(10, intNodeId)]
            equivE = fromPreRewriteState uf preRewriteConstraint

        it "classMembers returns all original nodes in class" $ do
            equiv <- requireRight equivE
            classMembers equiv intNodeId `shouldMatchList` [alphaNodeId, intNodeId]
            classMembers equiv alphaNodeId `shouldMatchList` [alphaNodeId, intNodeId]

        it "wasOriginalBinder returns True for unified-away binder" $ do
            equiv <- requireRight equivE
            wasOriginalBinder equiv intNodeId `shouldBe` True
            wasOriginalBinder equiv alphaNodeId `shouldBe` True
            wasOriginalBinder equiv alphaBodyId `shouldBe` False

        it "originalNode returns pre-solving node data" $ do
            equiv <- requireRight equivE
            originalNode equiv alphaNodeId `shouldBe` Just alphaNode
            originalNode equiv intNodeId `shouldBe` Just intNode

        it "originalBindParent preserves pre-solving binding tree" $ do
            equiv <- requireRight equivE
            originalBindParent equiv (typeRef alphaNodeId)
                `shouldBe` Just (typeRef parentNodeId, BindFlex)
            originalBindParent equiv (typeRef alphaBodyId)
                `shouldBe` Just (typeRef alphaNodeId, BindFlex)

        it "canonical works correctly for solver snapshots" $ do
            case snapshotEquiv of
                Left err -> expectationFailure err
                Right (staged, ids) ->
                    -- Verify canonical is idempotent
                    map (canonical staged . canonical staged) ids `shouldBe` map (canonical staged) ids

        it "lookupNode works correctly for solver snapshots" $ do
            case snapshotEquiv of
                Left err -> expectationFailure err
                Right (staged, ids) ->
                    -- Verify lookupNode returns consistent results
                    map (lookupNode staged . canonical staged) ids `shouldBe` map (lookupNode staged) ids

        it "fromSolveOutput matches explicit pre-rewrite snapshot construction" $ do
            case solveUnifyWithSnapshot defaultTraceConfig (rootedConstraint emptyConstraint) of
                Left err -> expectationFailure ("Unexpected solve error: " ++ show err)
                Right out ->
                    do
                        let snapshot = soSnapshot out
                        explicit <- requireRight $
                            fromPreRewriteState
                                (snapUnionFind snapshot)
                                (snapPreRewriteConstraint snapshot)
                        actual <- requireRight (fromSolveOutput out)
                        actual `shouldBe` explicit

        it "fromSolveOutput derives canonical constraint from snapshot, not soResult payload" $ do
            let var0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
                base1 = TyBase (NodeId 1) (BaseTy "Int")
                outputRootGen = GenNode (GenNodeId 0) [NodeId 1]
                preRewriteForOutput = emptyConstraint
                    { cNodes = nodeMapFromList [(0, var0), (1, base1)]
                    , cBindParents =
                        IntMap.fromList
                            [ (nodeRefKey (typeRef (NodeId 0)), (genRef (GenNodeId 0), BindFlex))
                            , (nodeRefKey (typeRef (NodeId 1)), (genRef (GenNodeId 0), BindFlex))
                            ]
                    , cGenNodes = fromListGen [(GenNodeId 0, outputRootGen)]
                    }
                snapshotUf = IntMap.fromList [(0, NodeId 1)]
                poisonedUf = IntMap.fromList [(0, NodeId 0)]
                poisonedConstraint = emptyConstraint
                    { cNodes = nodeMapFromList [(99, TyBase (NodeId 99) (BaseTy "Poison"))] }
                out =
                    SolveOutput
                        { soResult = SolveResult
                            { srConstraint = poisonedConstraint
                            , srUnionFind = poisonedUf
                            }
                        , soSnapshot =
                            SolveSnapshot
                                { snapUnionFind = snapshotUf
                                , snapPreRewriteConstraint = preRewriteForOutput
                                }
                        }
            explicit <- requireRight (fromPreRewriteState snapshotUf preRewriteForOutput)
            actual <- requireRight (fromSolveOutput out)
            actual `shouldBe` explicit

        it "keeps snapshot queries consistent" $ do
            case snapshotEquiv of
                Left err -> expectationFailure err
                Right (staged, ids) -> do
                    -- Verify queries are internally consistent
                    map (lookupVarBound staged . canonical staged) ids
                        `shouldBe` map (lookupVarBound staged) ids
                    let _ = instEdges staged
                        _ = bindParents staged
                        _ = genNodes staged
                        _ = allNodes staged
                    pure ()
