module Util.GraphSpec (spec) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import MLF.Util.ElabError (ElabError (..))
import MLF.Util.Graph (reachableFrom, reachableFromStop, topoSortBy)
import Test.Hspec

spec :: Spec
spec = describe "MLF.Util.Graph" $ do
  describe "topoSortBy" $ do
    it "returns empty list for empty input" $
      topoSortBy "test" compare (const []) []
        `shouldBe` Right []

    it "returns singleton for single node" $
      topoSortBy "test" compare (const []) [1]
        `shouldBe` Right [1]

    it "linearises a chain with dependencies first" $
      let depsFor 1 = [2]
          depsFor 2 = [3]
          depsFor _ = []
       in topoSortBy "test" compare depsFor [1, 2, 3]
            `shouldBe` Right [3, 2, 1]

    it "handles a diamond DAG (4 before 1)" $
      let depsFor 1 = [2, 3]
          depsFor 2 = [4]
          depsFor 3 = [4]
          depsFor _ = []
          result = topoSortBy "test" compare depsFor [1, 2, 3, 4]
       in case result of
            Right sorted -> do
              case sorted of
                (first : _) -> first `shouldBe` 4
                [] -> expectationFailure "Expected non-empty sorted list"
              case reverse sorted of
                (lst : _) -> lst `shouldBe` 1
                [] -> expectationFailure "Expected non-empty sorted list"
            Left err -> expectationFailure ("topoSortBy failed: " ++ show err)

    it "detects a cycle" $
      let depsFor 1 = [2]
          depsFor 2 = [1]
          depsFor _ = []
       in case topoSortBy "test" compare depsFor [1, 2] of
            Left (InstantiationError _) -> pure ()
            other -> expectationFailure ("Expected cycle error, got: " ++ show other)

    it "uses the comparator for tie-breaking" $
      let depsFor _ = [] :: [Int]
          forward = topoSortBy "test" compare depsFor [3, 1, 2]
          backward = topoSortBy "test" (flip compare) depsFor [3, 1, 2]
       in do
            forward `shouldBe` Right [1, 2, 3]
            backward `shouldBe` Right [3, 2, 1]

  describe "reachableFrom" $ do
    let succsMap :: IntMap.IntMap [Int]
        succsMap =
          IntMap.fromList
            [ (1, [2]),
              (2, [3]),
              (3, [])
            ]
        succs n = IntMap.findWithDefault [] n succsMap

    it "returns only the start for an isolated node" $
      reachableFrom id id (const ([] :: [Int])) (10 :: Int)
        `shouldBe` IntSet.singleton 10

    it "returns all nodes in a linear chain" $
      reachableFrom id id succs (1 :: Int)
        `shouldBe` IntSet.fromList [1, 2, 3]

    it "returns all nodes in a diamond DAG" $
      let diamondSuccs :: IntMap.IntMap [Int]
          diamondSuccs =
            IntMap.fromList
              [ (1, [2, 3]),
                (2, [4]),
                (3, [4]),
                (4, [])
              ]
          dSuccs n = IntMap.findWithDefault [] n diamondSuccs
       in reachableFrom id id dSuccs (1 :: Int)
            `shouldBe` IntSet.fromList [1, 2, 3, 4]

  describe "reachableFromStop" $ do
    let succsMap :: IntMap.IntMap [Int]
        succsMap =
          IntMap.fromList
            [ (1, [2]),
              (2, [3]),
              (3, [])
            ]
        succs n = IntMap.findWithDefault [] n succsMap

    it "behaves like reachableFrom when nothing is stopped" $
      reachableFromStop id id succs (const False) (1 :: Int)
        `shouldBe` IntSet.fromList [1, 2, 3]

    it "excludes stopped nodes and their descendants" $
      -- Stop at node 2: expansion is halted, 2 is NOT in the result set
      reachableFromStop id id succs (== 2) (1 :: Int)
        `shouldBe` IntSet.singleton 1

    it "never stops at the start node" $
      -- Even if shouldStop returns True for the start node, it is still expanded
      reachableFromStop id id succs (== 1) (1 :: Int)
        `shouldBe` IntSet.fromList [1, 2, 3]
