module Constraint.SolvedSpec (spec) where

import Control.Monad (forM_)
import Data.List (nub, sort)
import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

import MLF.Constraint.Presolution (PresolutionResult(..))
import MLF.Constraint.Solve
    ( SolveOutput(..)
    , SolveSnapshot(..)
    , solveUnifyWithSnapshot
    )
import MLF.Constraint.Solved
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
mkTestSolved :: Solved
mkTestSolved =
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
    in mkSolved constraint uf

snapshotLegacyAndEquiv :: Either String (Solved, Solved, [NodeId])
snapshotLegacyAndEquiv =
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
                    let legacy = fromSolveResult (soResult out)
                    in Right (legacy, equiv, ids)

spec :: Spec
spec = describe "MLF.Constraint.Solved" $ do
    let s = mkTestSolved

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
            collectNodeIds :: Solved -> Solved -> [NodeId]
            collectNodeIds legacy equiv =
                sort . nub $
                    map tnId (allNodes legacy ++ allNodes equiv)
                        ++ map NodeId (IntMap.keys (unionFind legacy))
                        ++ IntMap.elems (unionFind legacy)
                        ++ map NodeId (IntMap.keys (unionFind equiv))
                        ++ IntMap.elems (unionFind equiv)
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
                                let legacy = fromSolveResult (soResult out)
                                    ids = collectNodeIds legacy equiv
                                map (canonical legacy) ids `shouldBe` map (canonical equiv) ids
                                map (lookupNode legacy) ids `shouldBe` map (lookupNode equiv) ids
                                instEdges legacy `shouldBe` instEdges equiv
                                bindParents legacy `shouldBe` bindParents equiv
        forM_ cases $ \(label, expr) ->
            it ("matches legacy backend for " ++ label ++ " (" ++ show expr ++ ")") $
                runCase (label, expr)

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
        it "unionFind returns the raw parent map" $ do
            let uf = unionFind s
            IntMap.lookup 0 uf `shouldBe` Just (NodeId 1)
            IntMap.size uf `shouldBe` 1

        it "solvedConstraint returns the raw constraint" $ do
            let c = solvedConstraint s
            cInstEdges c `shouldBe` [InstEdge (EdgeId 0) (NodeId 2) (NodeId 3)]

    describe "Degraded stubs (Phase 1)" $ do
        it "classMembers returns singleton with canonical id" $ do
            classMembers s (NodeId 0) `shouldBe` [NodeId 1]
            classMembers s (NodeId 3) `shouldBe` [NodeId 3]

        it "wasOriginalBinder always returns False" $ do
            wasOriginalBinder s (NodeId 0) `shouldBe` False
            wasOriginalBinder s (NodeId 1) `shouldBe` False
            wasOriginalBinder s (NodeId 2) `shouldBe` False

        it "originalNode delegates to lookupNode" $ do
            originalNode s (NodeId 0) `shouldBe` lookupNode s (NodeId 0)
            originalNode s (NodeId 2) `shouldBe` lookupNode s (NodeId 2)
            originalNode s (NodeId 99) `shouldBe` Nothing

        it "originalBindParent delegates to lookupBindParent" $ do
            originalBindParent s (typeRef (NodeId 1))
                `shouldBe` lookupBindParent s (typeRef (NodeId 1))
            originalBindParent s (typeRef (NodeId 2))
                `shouldBe` lookupBindParent s (typeRef (NodeId 2))

    describe "lookupVarBound with actual bound" $ do
        it "returns the bound when the canonical node is a TyVar with a bound" $ do
            let boundVar = TyVar { tnId = NodeId 10, tnBound = Just (NodeId 11) }
                base11 = TyBase (NodeId 11) (BaseTy "Int")
                nodes = nodeMapFromList [(10, boundVar), (11, base11)]
                c = emptyConstraint { cNodes = nodes }
                s' = mkSolved c IntMap.empty
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

        it "canonical matches legacy backend for solver snapshots" $ do
            case snapshotLegacyAndEquiv of
                Left err -> expectationFailure err
                Right (legacy, staged, ids) ->
                    map (canonical staged) ids `shouldBe` map (canonical legacy) ids

        it "lookupNode matches legacy backend for solver snapshots" $ do
            case snapshotLegacyAndEquiv of
                Left err -> expectationFailure err
                Right (legacy, staged, ids) ->
                    map (lookupNode staged) ids `shouldBe` map (lookupNode legacy) ids

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
                        { soResult = toSolveResult (mkSolved poisonedConstraint poisonedUf)
                        , soSnapshot =
                            SolveSnapshot
                                { snapUnionFind = snapshotUf
                                , snapPreRewriteConstraint = preRewriteForOutput
                                }
                        }
            explicit <- requireRight (fromPreRewriteState snapshotUf preRewriteForOutput)
            actual <- requireRight (fromSolveOutput out)
            actual `shouldBe` explicit

        it "keeps other legacy snapshot queries in sync" $ do
            case snapshotLegacyAndEquiv of
                Left err -> expectationFailure err
                Right (legacy, staged, ids) -> do
                    map (lookupVarBound staged) ids `shouldBe` map (lookupVarBound legacy) ids
                    instEdges staged `shouldBe` instEdges legacy
                    bindParents staged `shouldBe` bindParents legacy
                    toListGen (genNodes staged) `shouldBe` toListGen (genNodes legacy)
                    allNodes staged `shouldBe` allNodes legacy
