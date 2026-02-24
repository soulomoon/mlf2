module Constraint.SolvedSpec (spec) where

import Test.Hspec
import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Solved
import MLF.Constraint.Solve (SolveResult(..))
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
    , fromListGen
    , nodeRefKey
    , toListGen
    , typeRef
    )

import SpecUtil
    ( emptyConstraint
    , nodeMapFromList
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
        sr = SolveResult { srConstraint = constraint, srUnionFind = uf }
    in fromSolveResult sr

spec :: Spec
spec = describe "MLF.Constraint.Solved" $ do
    let s = mkTestSolved

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
                sr = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }
                s' = fromSolveResult sr
            lookupVarBound s' (NodeId 10) `shouldBe` Just (NodeId 11)
