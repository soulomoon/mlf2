module Constraint.SolvedSpec (spec) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

import qualified MLF.Constraint.Presolution as Presolution
import qualified MLF.Constraint.Presolution.View as PresolutionViewBoundary
import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Presolution (PresolutionResult(..))
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Solve
    ( SolveOutput(..)
    , SolveResult(..)
    , SolveSnapshot(..)
    , solveUnifyWithSnapshot
    )
import MLF.Constraint.Types.Presolution (PresolutionSnapshot(..))
import MLF.Constraint.Solved
import qualified SolvedFacadeTestUtil as SolvedTest
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
    in SolvedTest.mkTestSolved constraint uf

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

        it "compat builders are absent from the Solved facade" $ do
            src <- readFile "src/MLF/Constraint/Solved.hs"
            forM_
                [ "fromConstraintAndUf ::"
                , "rebuildWithConstraint ::"
                , "    fromConstraintAndUf,"
                , "    rebuildWithConstraint,"
                ] $ \marker ->
                    src `shouldSatisfy` (not . isInfixOf marker)

        it "raw canonical container accessors are absent from the Solved facade" $ do
            src <- readFile "src/MLF/Constraint/Solved.hs"
            forM_
                [ "canonicalBindParents"
                , "canonicalGenNodes"
                ] $ \marker ->
                    src `shouldSatisfy` (not . isInfixOf marker)

        it "enumeration helpers are absent from the Solved facade" $ do
            src <- readFile "src/MLF/Constraint/Solved.hs"
            forM_
                [ "allNodes"
                , "instEdges"
                ] $ \marker ->
                    src `shouldSatisfy` (not . isInfixOf marker)

        it "test-only helper bundle is absent from the Solved facade" $ do
            src <- readFile "src/MLF/Constraint/Solved.hs"
            forM_
                [ "mkTestSolved"
                , "classMembers"
                , "originalNode"
                , "originalBindParent"
                , "wasOriginalBinder"
                , "validateOriginalCanonicalAgreement"
                ] $ \marker ->
                    src `shouldSatisfy` (not . isInfixOf marker)

        it "prune helper is absent from the Solved facade" $ do
            src <- readFile "src/MLF/Constraint/Solved.hs"
            src `shouldSatisfy` (not . isInfixOf "pruneBindParentsSolved")

        it "view-query wrappers are absent from the Solved facade" $ do
            src <- readFile "src/MLF/Constraint/Solved.hs"
            forM_
                [ "lookupNode"
                , "lookupBindParent"
                , "bindParents"
                ] $ \marker ->
                    src `shouldSatisfy` (not . isInfixOf marker)

        it "final reify/view helper cluster is absent from the Solved facade" $ do
            src <- readFile "src/MLF/Constraint/Solved.hs"
            forM_
                [ "lookupVarBound"
                , "genNodes"
                , "weakenedVars"
                , "isEliminatedVar"
                , "canonicalizedBindParents"
                ] $ \marker ->
                    src `shouldSatisfy` (not . isInfixOf marker)

    let s = testSolved

    describe "Constructor compatibility" $ do
        it "test mkTestSolved helper builds canonical queries from union-find and preserves original graph" $ do
            let var0 = TyVar { tnId = NodeId 0, tnBound = Nothing }
                base1 = TyBase (NodeId 1) (BaseTy "Int")
                inst = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
                nodes = nodeMapFromList [(0, var0), (1, base1)]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = [inst]
                    }
                uf = IntMap.fromList [(0, NodeId 1)]
                solved = SolvedTest.mkTestSolved constraint uf
            canonical solved (NodeId 0) `shouldBe` NodeId 1
            NodeAccess.lookupNode (originalConstraint solved) (canonical solved (NodeId 0)) `shouldBe` Just base1
            SolvedTest.originalNode solved (NodeId 0) `shouldBe` Just var0
            SolvedTest.classMembers solved (NodeId 1) `shouldMatchList` [NodeId 0, NodeId 1]
            cInstEdges (originalConstraint solved) `shouldBe` [inst]
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
                                length (NodeAccess.allNodes (originalConstraint equiv)) `shouldSatisfy` (> 0)
                                -- Force canonical map evaluation to catch crashes
                                canonicalMap equiv `seq` pure ()
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
                            (SolvedTest.solvedFromSnapshot
                                (snapshotUnionFind pres)
                                (snapshotConstraint pres)
                            )
                        let view = PresolutionViewBoundary.fromPresolutionResult pres
                            nodeIds = map fst (toListNode (cNodes (prConstraint pres)))
                            probes = nodeIds ++ [NodeId 999]
                        forM_ probes $ \nid -> do
                            Presolution.pvCanonical view nid `shouldBe` canonical solved nid
                            Presolution.pvLookupNode view nid `shouldBe`
                                NodeAccess.lookupNode (originalConstraint solved) (canonical solved nid)
                            Presolution.pvLookupVarBound view nid `shouldBe`
                                NodeAccess.lookupVarBound (originalConstraint solved) (canonical solved nid)

    describe "Core queries" $ do
        it "canonical chases the union-find" $ do
            canonical s (NodeId 0) `shouldBe` NodeId 1
            canonical s (NodeId 1) `shouldBe` NodeId 1
            canonical s (NodeId 3) `shouldBe` NodeId 3

        it "originalConstraint + canonical reproduces canonicalized lookup" $ do
            -- Looking up non-canonical 0 should find the node at canonical 1
            NodeAccess.lookupNode (originalConstraint s) (canonical s (NodeId 0)) `shouldBe` Just (TyBase (NodeId 1) (BaseTy "Int"))
            NodeAccess.lookupNode (originalConstraint s) (canonical s (NodeId 1)) `shouldBe` Just (TyBase (NodeId 1) (BaseTy "Int"))
            NodeAccess.lookupNode (originalConstraint s) (canonical s (NodeId 99)) `shouldBe` Nothing

        it "originalConstraint exposes all nodes in the constraint" $ do
            length (NodeAccess.allNodes (originalConstraint s)) `shouldBe` 4

        it "originalConstraint exposes bind-parent lookups" $ do
            NodeAccess.lookupBindParent (originalConstraint s) (typeRef (NodeId 1))
                `shouldBe` Just (typeRef (NodeId 2), BindFlex)
            NodeAccess.lookupBindParent (originalConstraint s) (typeRef (NodeId 3))
                `shouldBe` Just (typeRef (NodeId 2), BindFlex)
            NodeAccess.lookupBindParent (originalConstraint s) (typeRef (NodeId 2))
                `shouldBe` Nothing

        it "originalConstraint exposes the full bind-parent map" $ do
            let bp = cBindParents (originalConstraint s)
            IntMap.size bp `shouldBe` 2

        it "originalConstraint exposes inst edges" $ do
            cInstEdges (originalConstraint s) `shouldBe` [InstEdge (EdgeId 0) (NodeId 2) (NodeId 3)]

        it "originalConstraint exposes gen nodes" $ do
            let gns = cGenNodes (originalConstraint s)
            toListGen gns `shouldBe` [(GenNodeId 0, GenNode (GenNodeId 0) [NodeId 2])]

        it "lookupVarBound canonicalizes and returns bound" $ do
            -- Node 0 is a TyVar with no bound; canonical is 1 which is TyBase
            -- so lookupVarBound should return Nothing (TyBase is not a TyVar)
            NodeAccess.lookupVarBound (originalConstraint s) (canonical s (NodeId 0)) `shouldBe` Nothing
            NodeAccess.lookupVarBound (originalConstraint s) (canonical s (NodeId 99)) `shouldBe` Nothing

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
            let mismatches = SolvedTest.validateOriginalCanonicalAgreement s
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
                aligned = SolvedTest.mkTestSolved (emptyConstraint { cNodes = nodes }) IntMap.empty
            SolvedTest.validateOriginalCanonicalAgreement aligned `shouldBe` []

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
                    SolvedTest.mkTestSolved
                        (emptyConstraint { cNodes = nodes, cBindParents = bp })
                        (IntMap.fromList
                            [ (getNodeId childMerged, childCanonical)
                            , (getNodeId parentMerged, parentCanonical)
                            ]
                        )
            case Binding.canonicalizeBindParentsUnder (canonical solved) (canonicalConstraint solved) of
                Left err ->
                    expectationFailure ("Expected canonicalized bind parents, got error: " ++ show err)
                Right canonBp -> do
                    IntMap.lookup (nodeRefKey (typeRef childCanonical)) canonBp
                        `shouldBe` Just (typeRef parentCanonical, BindFlex)
                    IntMap.lookup (nodeRefKey (typeRef childMerged)) canonBp
                        `shouldBe` Nothing

    describe "Degraded stubs (Phase 1)" $ do
        it "classMembers returns equivalence class members" $ do
            SolvedTest.classMembers s (NodeId 0) `shouldMatchList` [NodeId 0, NodeId 1]
            SolvedTest.classMembers s (NodeId 3) `shouldBe` [NodeId 3]

        it "wasOriginalBinder always returns False" $ do
            SolvedTest.wasOriginalBinder s (NodeId 0) `shouldBe` False
            SolvedTest.wasOriginalBinder s (NodeId 1) `shouldBe` False
            SolvedTest.wasOriginalBinder s (NodeId 2) `shouldBe` False

        it "originalNode returns pre-merge node data" $ do
            -- Node 0 is a TyVar that was merged into node 1 (TyBase "Int").
            -- originalNode returns the pre-merge TyVar, not the canonical TyBase.
            SolvedTest.originalNode s (NodeId 0)
                `shouldBe` Just (TyVar { tnId = NodeId 0, tnBound = Nothing })
            SolvedTest.originalNode s (NodeId 2)
                `shouldBe` Just (TyArrow (NodeId 2) (NodeId 1) (NodeId 3))
            SolvedTest.originalNode s (NodeId 99) `shouldBe` Nothing

        it "originalBindParent delegates to lookupBindParent" $ do
            SolvedTest.originalBindParent s (typeRef (NodeId 1))
                `shouldBe` NodeAccess.lookupBindParent (originalConstraint s) (typeRef (NodeId 1))
            SolvedTest.originalBindParent s (typeRef (NodeId 2))
                `shouldBe` NodeAccess.lookupBindParent (originalConstraint s) (typeRef (NodeId 2))

    describe "projection-first queries" $ do
        it "originalNode returns pre-merge node data" $ do
            -- Node 0 was merged into node 1 via union-find, but
            -- SolvedTest.originalNode should still return the pre-merge TyVar.
            SolvedTest.originalNode s (NodeId 0)
                `shouldBe` Just (TyVar { tnId = NodeId 0, tnBound = Nothing })
            SolvedTest.originalNode s (NodeId 1)
                `shouldBe` Just (TyBase (NodeId 1) (BaseTy "Int"))
            SolvedTest.originalNode s (NodeId 99) `shouldBe` Nothing

        it "originalBindParent returns pre-merge binding parents" $ do
            SolvedTest.originalBindParent s (typeRef (NodeId 1))
                `shouldBe` Just (typeRef (NodeId 2), BindFlex)
            SolvedTest.originalBindParent s (typeRef (NodeId 2))
                `shouldBe` Nothing

        it "originalConstraint + canonical agrees on unified ids" $ do
            NodeAccess.lookupNode (originalConstraint s) (canonical s (NodeId 0))
                `shouldBe` NodeAccess.lookupNode (originalConstraint s) (canonical s (NodeId 1))

    describe "lookupVarBound with actual bound" $ do
        it "returns the bound when the canonical node is a TyVar with a bound" $ do
            let boundVar = TyVar { tnId = NodeId 10, tnBound = Just (NodeId 11) }
                base11 = TyBase (NodeId 11) (BaseTy "Int")
                nodes = nodeMapFromList [(10, boundVar), (11, base11)]
                c = emptyConstraint { cNodes = nodes }
                s' = SolvedTest.mkTestSolved c IntMap.empty
            NodeAccess.lookupVarBound (originalConstraint s') (canonical s' (NodeId 10)) `shouldBe` Just (NodeId 11)

    describe "snapshot reconstruction" $ do
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
            equivE = SolvedTest.solvedFromSnapshot uf preRewriteConstraint

        it "classMembers returns all original nodes in class" $ do
            equiv <- requireRight equivE
            SolvedTest.classMembers equiv intNodeId `shouldMatchList` [alphaNodeId, intNodeId]
            SolvedTest.classMembers equiv alphaNodeId `shouldMatchList` [alphaNodeId, intNodeId]

        it "wasOriginalBinder returns True for unified-away binder" $ do
            equiv <- requireRight equivE
            SolvedTest.wasOriginalBinder equiv intNodeId `shouldBe` True
            SolvedTest.wasOriginalBinder equiv alphaNodeId `shouldBe` True
            SolvedTest.wasOriginalBinder equiv alphaBodyId `shouldBe` False

        it "originalNode returns pre-solving node data" $ do
            equiv <- requireRight equivE
            SolvedTest.originalNode equiv alphaNodeId `shouldBe` Just alphaNode
            SolvedTest.originalNode equiv intNodeId `shouldBe` Just intNode

        it "originalBindParent preserves pre-solving binding tree" $ do
            equiv <- requireRight equivE
            SolvedTest.originalBindParent equiv (typeRef alphaNodeId)
                `shouldBe` Just (typeRef parentNodeId, BindFlex)
            SolvedTest.originalBindParent equiv (typeRef alphaBodyId)
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
                Right (staged, _ids) -> do
                    -- Existing nodes resolve through canonical form
                    NodeAccess.lookupNode (originalConstraint staged) (canonical staged (NodeId 0))
                        `shouldSatisfy` isJust
                    NodeAccess.lookupNode (originalConstraint staged) (canonical staged (NodeId 1))
                        `shouldSatisfy` isJust
                    NodeAccess.lookupNode (originalConstraint staged) (canonical staged (NodeId 2))
                        `shouldSatisfy` isJust
                    -- Non-existent node returns Nothing
                    NodeAccess.lookupNode (originalConstraint staged) (NodeId 99)
                        `shouldBe` Nothing

        it "fromSolveOutput matches explicit pre-rewrite snapshot construction" $ do
            case solveUnifyWithSnapshot defaultTraceConfig (rootedConstraint emptyConstraint) of
                Left err -> expectationFailure ("Unexpected solve error: " ++ show err)
                Right out ->
                    do
                        let snapshot = soSnapshot out
                        explicit <- requireRight $
                            SolvedTest.solvedFromSnapshot
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
            explicit <- requireRight (SolvedTest.solvedFromSnapshot snapshotUf preRewriteForOutput)
            actual <- requireRight (fromSolveOutput out)
            actual `shouldBe` explicit

        it "keeps snapshot queries consistent" $ do
            case snapshotEquiv of
                Left err -> expectationFailure err
                Right (staged, _ids) -> do
                    -- var4 (NodeId 4) has bound NodeId 0 (raw, not canonicalized)
                    NodeAccess.lookupVarBound (originalConstraint staged) (canonical staged (NodeId 4))
                        `shouldBe` Just (NodeId 0)
                    -- Non-existent node returns Nothing
                    NodeAccess.lookupVarBound (originalConstraint staged) (NodeId 99)
                        `shouldBe` Nothing
                    -- Non-variable nodes return Nothing
                    NodeAccess.lookupVarBound (originalConstraint staged) (canonical staged (NodeId 1))
                        `shouldBe` Nothing
