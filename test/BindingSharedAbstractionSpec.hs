module BindingSharedAbstractionSpec (spec) where

import Data.Either (isLeft)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Binding.Children (collectBoundChildrenWithFlag)
import MLF.Binding.NodeRefs (allNodeRefs, nodeRefExists)
import MLF.Binding.Path (
    bindingPathToRootWithLookup,
    firstGenAncestorFromPath,
    )
import MLF.Binding.ScopeGraph (buildTypeEdgesFrom, rootsForScope)
import MLF.Constraint.Types.Graph
import SpecUtil (emptyConstraint, nodeMapFromList)
import Test.Hspec

spec :: Spec
spec = describe "Binding shared abstractions" $ do
    it "bindingPathToRootWithLookup follows lookup chains to root" $ do
        let bindParents =
                IntMap.fromList
                    [ (nodeRefKey (TypeRef (NodeId 2)), (TypeRef (NodeId 1), BindFlex))
                    , (nodeRefKey (TypeRef (NodeId 1)), (GenRef (GenNodeId 0), BindFlex))
                    ]
        bindingPathToRootWithLookup (`IntMap.lookup` bindParents) (TypeRef (NodeId 2))
            `shouldBe` Right [TypeRef (NodeId 2), TypeRef (NodeId 1), GenRef (GenNodeId 0)]

    it "firstGenAncestorFromPath returns nearest strict gen ancestor" $ do
        let fakePath (TypeRef (NodeId 5)) =
                Right [TypeRef (NodeId 5), GenRef (GenNodeId 2), GenRef (GenNodeId 0)]
            fakePath _ = Right []
        firstGenAncestorFromPath fakePath (TypeRef (NodeId 5))
            `shouldBe` Just (GenNodeId 2)

    it "rootsForScope returns non-referenced nodes inside scope" $ do
        let edges = IntMap.fromList [(0, IntSet.fromList [1, 2]), (1, IntSet.singleton 3)]
            scope = IntSet.fromList [0, 1, 2, 3]
        rootsForScope id Just edges scope `shouldBe` IntSet.singleton 0

    it "buildTypeEdgesFrom captures structural and bound-child edges" $ do
        let nodes =
                nodeMapFromList
                    [ (0, TyArrow { tnId = NodeId 0, tnDom = NodeId 1, tnCod = NodeId 2 })
                    , (1, TyVar { tnId = NodeId 1, tnBound = Just (NodeId 2) })
                    , (2, TyBase { tnId = NodeId 2, tnBase = BaseTy "Int" })
                    , (3, TyForall { tnId = NodeId 3, tnBody = NodeId 1 })
                    ]
            expected =
                IntMap.fromList
                    [ (0, IntSet.fromList [1, 2])
                    , (1, IntSet.singleton 2)
                    , (3, IntSet.singleton 1)
                    ]
        buildTypeEdgesFrom getNodeId nodes `shouldBe` expected

    it "allNodeRefs and nodeRefExists cover type and gen refs directly" $ do
        let nodes =
                nodeMapFromList
                    [ (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                    , (2, TyBase { tnId = NodeId 2, tnBase = BaseTy "Int" })
                    ]
            genNodes =
                fromListGen
                    [ (GenNodeId 0, GenNode (GenNodeId 0) [NodeId 1])
                    , (GenNodeId 2, GenNode (GenNodeId 2) [])
                    ]
            c = emptyConstraint { cNodes = nodes, cGenNodes = genNodes }
        allNodeRefs c
            `shouldBe`
                [ TypeRef (NodeId 1)
                , TypeRef (NodeId 2)
                , GenRef (GenNodeId 0)
                , GenRef (GenNodeId 2)
                ]
        nodeRefExists c (TypeRef (NodeId 1)) `shouldBe` True
        nodeRefExists c (TypeRef (NodeId 99)) `shouldBe` False
        nodeRefExists c (GenRef (GenNodeId 2)) `shouldBe` True
        nodeRefExists c (GenRef (GenNodeId 7)) `shouldBe` False

    it "collectBoundChildrenWithFlag filters children by bind flag" $ do
        let nodes =
                nodeMapFromList
                    [ (0, TyForall { tnId = NodeId 0, tnBody = NodeId 1 })
                    , (1, TyVar { tnId = NodeId 1, tnBound = Nothing })
                    , (2, TyVar { tnId = NodeId 2, tnBound = Nothing })
                    , (3, TyVar { tnId = NodeId 3, tnBound = Nothing })
                    ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (TypeRef (NodeId 1)), (TypeRef (NodeId 0), BindFlex))
                    , (nodeRefKey (TypeRef (NodeId 2)), (TypeRef (NodeId 0), BindRigid))
                    , (nodeRefKey (TypeRef (NodeId 3)), (TypeRef (NodeId 9), BindFlex))
                    ]
            c = emptyConstraint { cNodes = nodes, cBindParents = bindParents }
            childFilter (TypeRef nid) = Just nid
            childFilter _ = Nothing
        collectBoundChildrenWithFlag childFilter (== BindFlex) c bindParents (TypeRef (NodeId 0)) "test"
            `shouldBe` Right [NodeId 1]
        collectBoundChildrenWithFlag childFilter (const True) c bindParents (TypeRef (NodeId 0)) "test"
            `shouldBe` Right [NodeId 1, NodeId 2]

    it "collectBoundChildrenWithFlag fails when a referenced child node is missing" $ do
        let nodes = nodeMapFromList [(0, TyForall { tnId = NodeId 0, tnBody = NodeId 0 })]
            bindParents =
                IntMap.singleton
                    (nodeRefKey (TypeRef (NodeId 7)))
                    (TypeRef (NodeId 0), BindFlex)
            c = emptyConstraint { cNodes = nodes, cBindParents = bindParents }
            childFilter (TypeRef nid) = Just nid
            childFilter _ = Nothing
        collectBoundChildrenWithFlag childFilter (== BindFlex) c bindParents (TypeRef (NodeId 0)) "test"
            `shouldSatisfy` isLeft
