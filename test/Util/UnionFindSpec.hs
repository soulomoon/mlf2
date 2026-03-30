module Util.UnionFindSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import MLF.Constraint.Types.Graph (NodeId (..))
import MLF.Util.UnionFind (findRootWithCompression, frWith)
import Test.Hspec

spec :: Spec
spec = describe "MLF.Util.UnionFind" $ do
  describe "frWith" $ do
    it "returns the node itself when unmapped" $
      frWith IntMap.empty (NodeId 5) `shouldBe` NodeId 5

    it "returns the node on a self-loop" $
      let uf = IntMap.singleton 1 (NodeId 1)
       in frWith uf (NodeId 1) `shouldBe` NodeId 1

    it "chases a two-step chain to the root" $
      let uf = IntMap.fromList [(1, NodeId 2), (2, NodeId 3)]
       in frWith uf (NodeId 1) `shouldBe` NodeId 3

    it "does not compress the parent map" $
      let uf = IntMap.fromList [(1, NodeId 2), (2, NodeId 3)]
          _ = frWith uf (NodeId 1)
       in IntMap.lookup 1 uf `shouldBe` Just (NodeId 2)

  describe "findRootWithCompression" $ do
    it "returns the node itself when unmapped" $
      let (root, uf') = findRootWithCompression IntMap.empty (NodeId 5)
       in do
            root `shouldBe` NodeId 5
            uf' `shouldBe` IntMap.empty

    it "returns the node on a self-loop without modifying map" $
      let uf = IntMap.singleton 1 (NodeId 1)
          (root, uf') = findRootWithCompression uf (NodeId 1)
       in do
            root `shouldBe` NodeId 1
            uf' `shouldBe` uf

    it "compresses a two-step chain" $
      let uf = IntMap.fromList [(1, NodeId 2), (2, NodeId 3)]
          (root, uf') = findRootWithCompression uf (NodeId 1)
       in do
            root `shouldBe` NodeId 3
            IntMap.lookup 1 uf' `shouldBe` Just (NodeId 3)

    it "converges after compression (second call is immediate)" $
      let uf = IntMap.fromList [(1, NodeId 2), (2, NodeId 3)]
          (_root, uf') = findRootWithCompression uf (NodeId 1)
          (root2, uf'') = findRootWithCompression uf' (NodeId 1)
       in do
            root2 `shouldBe` NodeId 3
            uf'' `shouldBe` uf'
