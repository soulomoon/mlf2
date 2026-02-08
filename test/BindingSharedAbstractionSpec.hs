module BindingSharedAbstractionSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import MLF.Binding.ScopeGraph (buildTypeEdgesFrom, rootsForScope)
import MLF.Binding.Path (
    bindingPathToRootWithLookup,
    firstGenAncestorFromPath,
    )
import MLF.Constraint.Types.Graph
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
        let _keepBuildTypeEdgesFrom = buildTypeEdgesFrom
            edges = IntMap.fromList [(0, IntSet.fromList [1, 2]), (1, IntSet.singleton 3)]
            scope = IntSet.fromList [0, 1, 2, 3]
        rootsForScope id Just edges scope `shouldBe` IntSet.singleton 0
