{-# LANGUAGE DataKinds #-}
module AcyclicitySpec (spec) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import MLF.API (Expr (..), Lit (..), SrcTy (..))
import MLF.Constraint.Acyclicity (AcyclicityResult (..), CycleError (..), buildDependencyGraph, collectReachableNodes, findCycle, isAcyclic, topologicalSort)
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Presolution
import MLF.Pipeline (ConstraintResult (..), inferConstraintGraph)
import SpecUtil (breakCyclesAndCheckAcyclicityRaw, checkAcyclicityRaw, emptyConstraint, nodeMapElems, nodeMapFromList, nodeMapSingleton)
import Test.Hspec

spec :: Spec
spec = do
  describe "Phase 3 — Acyclicity Check" $ do
    describe "Trivial cases" $ do
      it "empty constraint is acyclic" $ do
        let constraint = emptyConstraint
        checkAcyclicityRaw constraint `shouldSatisfy` isRight

      it "single InstEdge is acyclic" $ do
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing})
                ]
            edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
            constraint =
              emptyConstraint
                { cNodes = nodes,
                  cInstEdges = [edge]
                }
        checkAcyclicityRaw constraint `shouldSatisfy` isRight

      it "returns empty sorted list for empty InstEdges" $ do
        case checkAcyclicityRaw emptyConstraint of
          Right result -> arSortedEdges result `shouldBe` []
          Left _ -> expectationFailure "Expected acyclic result"

    describe "Independent edges (no dependencies)" $ do
      it "two edges on disjoint nodes are acyclic" $ do
        -- α ≤ β and γ ≤ δ share no nodes
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}),
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}),
                  (3, TyVar {tnId = NodeId 3, tnBound = Nothing})
                ]
            edges =
              [ InstEdge (EdgeId 0) (NodeId 0) (NodeId 1), -- α ≤ β
                InstEdge (EdgeId 1) (NodeId 2) (NodeId 3) -- γ ≤ δ
              ]
            constraint =
              emptyConstraint
                { cNodes = nodes,
                  cInstEdges = edges
                }
        checkAcyclicityRaw constraint `shouldSatisfy` isRight
        case checkAcyclicityRaw constraint of
          Right result -> length (arSortedEdges result) `shouldBe` 2
          Left _ -> expectationFailure "Expected acyclic"

      it "three independent edges return all in sorted order" $ do
        let nodes =
              nodeMapFromList
                [ (i, TyVar {tnId = NodeId i, tnBound = Nothing}) | i <- [0 .. 5]
                ]
            edges =
              [ InstEdge (EdgeId 0) (NodeId 0) (NodeId 1),
                InstEdge (EdgeId 1) (NodeId 2) (NodeId 3),
                InstEdge (EdgeId 2) (NodeId 4) (NodeId 5)
              ]
            constraint =
              emptyConstraint
                { cNodes = nodes,
                  cInstEdges = edges
                }
        case checkAcyclicityRaw constraint of
          Right result -> length (arSortedEdges result) `shouldBe` 3
          Left _ -> expectationFailure "Expected acyclic"

    describe "Linear chains (dependent but acyclic)" $ do
      it "chain α ≤ β, β ≤ γ is acyclic" $ do
        -- e₁: α ≤ β (right side uses β)
        -- e₂: β ≤ γ (left side uses β)
        -- e₁ depends on e₂ (e₂'s left ∩ e₁'s right = {β})
        -- Order should be: e₂ before e₁
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}) -- γ
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1) -- α ≤ β
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 2) -- β ≤ γ
            constraint =
              emptyConstraint
                { cNodes = nodes,
                  cInstEdges = [e1, e2]
                }
        checkAcyclicityRaw constraint `shouldSatisfy` isRight
        case checkAcyclicityRaw constraint of
          Right result -> do
            let sorted = arSortedEdges result
            length sorted `shouldBe` 2
            -- e₂ should come before e₁ in topological order
            -- because e₁ depends on e₂
            let ids = map instEdgeId sorted
            -- Find positions
            let posE1 = findIndex (EdgeId 0) ids
                posE2 = findIndex (EdgeId 1) ids
            -- e2 (EdgeId 1) should be before e1 (EdgeId 0)
            case (posE1, posE2) of
              (Just p1, Just p2) -> p2 `shouldSatisfy` (< p1)
              _ -> expectationFailure "Both edges should be present"
          Left _ -> expectationFailure "Expected acyclic"

      it "longer chain α ≤ β ≤ γ ≤ δ is acyclic" $ do
        let nodes =
              nodeMapFromList
                [ (i, TyVar {tnId = NodeId i, tnBound = Nothing}) | i <- [0 .. 3]
                ]
            edges =
              [ InstEdge (EdgeId 0) (NodeId 0) (NodeId 1), -- α ≤ β
                InstEdge (EdgeId 1) (NodeId 1) (NodeId 2), -- β ≤ γ
                InstEdge (EdgeId 2) (NodeId 2) (NodeId 3) -- γ ≤ δ
              ]
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = edges}
        checkAcyclicityRaw constraint `shouldSatisfy` isRight

    describe "Edges through structured types" $ do
      it "explicit TyMu annotations remain acyclic without a cyclic escape hatch" $ do
        let expr = EAnn (ELit (LInt 1)) (STMu "self" (STArrow (STVar "self") (STBase "Int")))
        case inferConstraintGraph mempty expr of
          Left err -> expectationFailure ("Expected constraint graph, got " ++ show err)
          Right result ->
            checkAcyclicityRaw (crConstraint result) `shouldSatisfy` isRight

      it "dependency through arrow domain is tracked" $ do
        -- e₁: α ≤ (β → Int)  (right side reaches β through arrow domain)
        -- e₂: β ≤ γ          (left side is β)
        --
        -- Dependency analysis:
        --   e₁.right = (β → Int), reachable nodes = {4, 1, 3} (arrow, β, Int)
        --   e₂.left = β, reachable nodes = {1} (just β)
        --   e₂.left ∩ e₁.right = {1} ≠ ∅ → e₁ depends on e₂
        --
        -- Why? Solving e₂ may modify β, which e₁'s RHS references.
        -- Therefore e₂ must be processed first.
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}), -- γ
                  (3, TyBase (NodeId 3) (BaseTy "Int")),
                  (4, TyArrow (NodeId 4) (NodeId 1) (NodeId 3)) -- β → Int
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 4) -- α ≤ (β → Int)
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 2) -- β ≤ γ
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        checkAcyclicityRaw constraint `shouldSatisfy` isRight
        -- e₂ should be processed before e₁
        case checkAcyclicityRaw constraint of
          Right result -> do
            let ids = map instEdgeId (arSortedEdges result)
            let posE1 = findIndex (EdgeId 0) ids
                posE2 = findIndex (EdgeId 1) ids
            case (posE1, posE2) of
              (Just p1, Just p2) -> p2 `shouldSatisfy` (< p1)
              _ -> expectationFailure "Both edges should be present"
          Left _ -> expectationFailure "Expected acyclic"

      it "dependency through arrow codomain is tracked" $ do
        -- e₁: α ≤ (Int → β)  (right side reaches β)
        -- e₂: β ≤ γ          (left side is β)
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}), -- γ
                  (3, TyBase (NodeId 3) (BaseTy "Int")),
                  (4, TyArrow (NodeId 4) (NodeId 3) (NodeId 1)) -- Int → β
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 4) -- α ≤ (Int → β)
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 2) -- β ≤ γ
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        checkAcyclicityRaw constraint `shouldSatisfy` isRight

      it "deep nesting in arrows is traversed" $ do
        -- α ≤ ((β → Int) → Bool)
        -- β ≤ γ
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}), -- γ
                  (3, TyBase (NodeId 3) (BaseTy "Int")),
                  (4, TyBase (NodeId 4) (BaseTy "Bool")),
                  (5, TyArrow (NodeId 5) (NodeId 1) (NodeId 3)), -- β → Int
                  (6, TyArrow (NodeId 6) (NodeId 5) (NodeId 4)) -- (β → Int) → Bool
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 6)
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 2)
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        checkAcyclicityRaw constraint `shouldSatisfy` isRight

    describe "Cycle detection" $ do
      it "detects simple 2-node cycle" $ do
        -- e₁: α ≤ β (right uses β, so e₁ depends on anything modifying β)
        -- e₂: β ≤ α (right uses α, so e₂ depends on anything modifying α)
        -- e₁ depends on e₂ (e₂.left = β, e₁.right = β → intersection)
        -- e₂ depends on e₁ (e₁.left = α, e₂.right = α → intersection)
        -- This is a cycle!
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}) -- β
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1) -- α ≤ β
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 0) -- β ≤ α
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        checkAcyclicityRaw constraint `shouldSatisfy` isLeft

      it "detects 3-node cycle" $ do
        -- e₁: α ≤ β, e₂: β ≤ γ, e₃: γ ≤ α
        -- Forms a cycle: e₁ → e₂ → e₃ → e₁
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}) -- γ
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1) -- α ≤ β
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 2) -- β ≤ γ
            e3 = InstEdge (EdgeId 2) (NodeId 2) (NodeId 0) -- γ ≤ α
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2, e3]}
        checkAcyclicityRaw constraint `shouldSatisfy` isLeft

      it "returns cycle edges in error" $ do
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing})
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 0)
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        case checkAcyclicityRaw constraint of
          Left err -> do
            ceEdgesInCycle err `shouldSatisfy` (not . null)
            ceMessage err `shouldSatisfy` (not . null)
          Right _ -> expectationFailure "Expected cycle error"

      it "cycle through structured types is detected" $ do
        -- e₁: α ≤ (β → Int)
        -- e₂: β ≤ (α → Int)
        -- Cycle because e₁.right reaches β and e₂.left is β,
        -- and e₂.right reaches α and e₁.left is α
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyBase (NodeId 2) (BaseTy "Int")),
                  (3, TyArrow (NodeId 3) (NodeId 1) (NodeId 2)), -- β → Int
                  (4, TyArrow (NodeId 4) (NodeId 0) (NodeId 2)) -- α → Int
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 3) -- α ≤ (β → Int)
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 4) -- β ≤ (α → Int)
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        checkAcyclicityRaw constraint `shouldSatisfy` isLeft

    describe "Cycle breaking" $ do
      it "rewrites a simple cycle into an acyclic graph" $ do
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing})
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 0)
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        case breakCyclesAndCheckAcyclicityRaw constraint of
          Left err -> expectationFailure ("Expected rewritten acyclic graph, got " ++ show err)
          Right (rewritten, result) -> do
            checkAcyclicityRaw rewritten `shouldSatisfy` isRight
            arSortedEdges result `shouldSatisfy` (not . null)

      it "introduces TyMu nodes when it breaks a cycle" $ do
        let intNode = TyBase (NodeId 2) (BaseTy "Int")
            nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}),
                  (2, intNode),
                  (3, TyArrow (NodeId 3) (NodeId 1) (NodeId 2)),
                  (4, TyArrow (NodeId 4) (NodeId 0) (NodeId 2))
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 3)
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 4)
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        case breakCyclesAndCheckAcyclicityRaw constraint of
          Left err -> expectationFailure ("Expected rewritten acyclic graph, got " ++ show err)
          Right (rewritten, _result) -> do
            let muNodes = [node | node@TyMu {} <- nodeMapElems (cNodes rewritten)]
            muNodes `shouldSatisfy` (not . null)

      it "leaves acyclic input unchanged" $ do
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing})
                ]
            edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [edge]}
        case breakCyclesAndCheckAcyclicityRaw constraint of
          Left err -> expectationFailure ("Expected unchanged acyclic graph, got " ++ show err)
          Right (rewritten, result) -> do
            rewritten `shouldBe` constraint
            arSortedEdges result `shouldBe` [edge]

    describe "Mixed acyclic and cyclic subgraphs" $ do
      it "detects cycle even with independent acyclic edges present" $ do
        -- Independent: γ ≤ δ (no cycle)
        -- Cycle: α ≤ β, β ≤ α
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}), -- γ
                  (3, TyVar {tnId = NodeId 3, tnBound = Nothing}) -- δ
                ]
            edges =
              [ InstEdge (EdgeId 0) (NodeId 0) (NodeId 1), -- α ≤ β (cycle)
                InstEdge (EdgeId 1) (NodeId 1) (NodeId 0), -- β ≤ α (cycle)
                InstEdge (EdgeId 2) (NodeId 2) (NodeId 3) -- γ ≤ δ (independent)
              ]
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = edges}
        checkAcyclicityRaw constraint `shouldSatisfy` isLeft

    describe "TyForall traversal" $ do
      it "dependency through forall body is tracked" $ do
        -- e₁: α ≤ ∀g.β  (right side reaches β through forall body)
        -- e₂: β ≤ γ      (left side is β)
        -- e₁ depends on e₂
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β (at inner level)
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}), -- γ
                  (3, TyForall (NodeId 3) (NodeId 1)) -- ∀g.β
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 3) -- α ≤ ∀g.β
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 2) -- β ≤ γ
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        checkAcyclicityRaw constraint `shouldSatisfy` isRight
        case checkAcyclicityRaw constraint of
          Right result -> do
            let ids = map instEdgeId (arSortedEdges result)
            let posE1 = findIndex (EdgeId 0) ids
                posE2 = findIndex (EdgeId 1) ids
            case (posE1, posE2) of
              (Just p1, Just p2) -> p2 `shouldSatisfy` (< p1)
              _ -> expectationFailure "Both edges should be present"
          Left _ -> expectationFailure "Expected acyclic"

      it "cycle through forall is detected" $ do
        -- e₁: α ≤ ∀g.β  (reaches β)
        -- e₂: β ≤ ∀g.α  (reaches α)
        -- Cycle!
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyForall (NodeId 2) (NodeId 1)), -- ∀g.β
                  (3, TyForall (NodeId 3) (NodeId 0)) -- ∀g.α
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 2) -- α ≤ ∀g.β
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 3) -- β ≤ ∀g.α
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        checkAcyclicityRaw constraint `shouldSatisfy` isLeft

    describe "TyExp (expansion node) traversal" $ do
      it "dependency through expansion body is tracked" $ do
        -- e₁: α ≤ (s · β)  (right side reaches β through exp body)
        -- e₂: β ≤ γ        (left side is β)
        -- e₁ depends on e₂
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}), -- γ
                  (3, TyExp (NodeId 3) (ExpVarId 0) (NodeId 1)) -- s · β
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 3) -- α ≤ (s · β)
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 2) -- β ≤ γ
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        checkAcyclicityRaw constraint `shouldSatisfy` isRight
        case checkAcyclicityRaw constraint of
          Right result -> do
            let ids = map instEdgeId (arSortedEdges result)
            let posE1 = findIndex (EdgeId 0) ids
                posE2 = findIndex (EdgeId 1) ids
            case (posE1, posE2) of
              (Just p1, Just p2) -> p2 `shouldSatisfy` (< p1)
              _ -> expectationFailure "Both edges should be present"
          Left _ -> expectationFailure "Expected acyclic"

      it "cycle through expansion is detected" $ do
        -- e₁: α ≤ (s · β)
        -- e₂: β ≤ (s · α)
        -- Cycle!
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyExp (NodeId 2) (ExpVarId 0) (NodeId 1)), -- s · β
                  (3, TyExp (NodeId 3) (ExpVarId 0) (NodeId 0)) -- s · α
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 2) -- α ≤ (s · β)
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 3) -- β ≤ (s · α)
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        checkAcyclicityRaw constraint `shouldSatisfy` isLeft

    describe "Diamond dependencies (DAG)" $ do
      it "diamond shape is acyclic" $ do
        -- e₁: α ≤ β
        -- e₂: α ≤ γ
        -- e₃: β ≤ δ
        -- e₄: γ ≤ δ
        -- Multiple paths to δ but no cycle
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}), -- γ
                  (3, TyVar {tnId = NodeId 3, tnBound = Nothing}) -- δ
                ]
            edges =
              [ InstEdge (EdgeId 0) (NodeId 0) (NodeId 1), -- α ≤ β
                InstEdge (EdgeId 1) (NodeId 0) (NodeId 2), -- α ≤ γ
                InstEdge (EdgeId 2) (NodeId 1) (NodeId 3), -- β ≤ δ
                InstEdge (EdgeId 3) (NodeId 2) (NodeId 3) -- γ ≤ δ
              ]
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = edges}
        checkAcyclicityRaw constraint `shouldSatisfy` isRight
        case checkAcyclicityRaw constraint of
          Right result -> length (arSortedEdges result) `shouldBe` 4
          Left _ -> expectationFailure "Expected acyclic"

      it "diamond with additional back edge creates cycle" $ do
        -- Same as above but add δ ≤ α creating cycle
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}), -- γ
                  (3, TyVar {tnId = NodeId 3, tnBound = Nothing}) -- δ
                ]
            edges =
              [ InstEdge (EdgeId 0) (NodeId 0) (NodeId 1), -- α ≤ β
                InstEdge (EdgeId 1) (NodeId 0) (NodeId 2), -- α ≤ γ
                InstEdge (EdgeId 2) (NodeId 1) (NodeId 3), -- β ≤ δ
                InstEdge (EdgeId 3) (NodeId 2) (NodeId 3), -- γ ≤ δ
                InstEdge (EdgeId 4) (NodeId 3) (NodeId 0) -- δ ≤ α (back edge!)
              ]
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = edges}
        checkAcyclicityRaw constraint `shouldSatisfy` isLeft

    describe "Shared nodes" $ do
      it "shared node on RHS creates no dependency" $ do
        -- e₁: α ≤ β
        -- e₂: γ ≤ β
        -- Both share β on right, but neither modifies the other's input
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}) -- γ
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1) -- α ≤ β
            e2 = InstEdge (EdgeId 1) (NodeId 2) (NodeId 1) -- γ ≤ β
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        checkAcyclicityRaw constraint `shouldSatisfy` isRight
        -- No dependency between them (independent)
        let g = buildDependencyGraph constraint
        -- e₁ should NOT depend on e₂ and vice versa
        case IntMap.lookup 0 (dgEdges g) of
          Just deps -> deps `shouldNotContain` [EdgeId 1]
          Nothing -> pure () -- No dependencies is fine
        case IntMap.lookup 1 (dgEdges g) of
          Just deps -> deps `shouldNotContain` [EdgeId 0]
          Nothing -> pure ()

      it "shared node on LHS creates mutual dependency (cycle)" $ do
        -- e₁: α ≤ β
        -- e₂: α ≤ γ
        -- Both have α on left. e₁.right=β, e₂.left=α → no dep
        -- e₂.right=γ, e₁.left=α → no dep
        -- Actually no cycle! They're independent.
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}), -- α
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}), -- β
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}) -- γ
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1) -- α ≤ β
            e2 = InstEdge (EdgeId 1) (NodeId 0) (NodeId 2) -- α ≤ γ
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
        checkAcyclicityRaw constraint `shouldSatisfy` isRight

    describe "Self-referential edge" $ do
      it "self-loop α ≤ α is acyclic (trivially satisfied)" $ do
        -- α ≤ α: left=α, right=α
        -- Dependency check: e.left ∩ e.right = {α} ≠ ∅
        -- But we filter out self-loops in dependency graph construction
        let nodes = nodeMapSingleton 0 (TyVar {tnId = NodeId 0, tnBound = Nothing})
            edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 0) -- α ≤ α
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [edge]}
        checkAcyclicityRaw constraint `shouldSatisfy` isRight

    describe "Edge cases" $ do
      it "missing node in IntMap is handled gracefully" $ do
        -- InstEdge references NodeId 99 which doesn't exist
        let nodes = nodeMapSingleton 0 (TyVar {tnId = NodeId 0, tnBound = Nothing})
            edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 99) -- 99 doesn't exist
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [edge]}
        -- Should still be acyclic (missing node has no children)
        checkAcyclicityRaw constraint `shouldSatisfy` isRight

      it "longer chain order is verified" $ do
        -- e₁: α ≤ β, e₂: β ≤ γ, e₃: γ ≤ δ
        -- Order should be: e₃, e₂, e₁ (dependencies first)
        let nodes =
              nodeMapFromList
                [ (i, TyVar {tnId = NodeId i, tnBound = Nothing}) | i <- [0 .. 3]
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1) -- α ≤ β
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 2) -- β ≤ γ
            e3 = InstEdge (EdgeId 2) (NodeId 2) (NodeId 3) -- γ ≤ δ
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2, e3]}
        case checkAcyclicityRaw constraint of
          Right result -> do
            let ids = map instEdgeId (arSortedEdges result)
            let pos0 = findIndex (EdgeId 0) ids
                pos1 = findIndex (EdgeId 1) ids
                pos2 = findIndex (EdgeId 2) ids
            case (pos0, pos1, pos2) of
              (Just p0, Just p1, Just p2) -> do
                -- e₃ (EdgeId 2) before e₂ (EdgeId 1) before e₁ (EdgeId 0)
                p2 `shouldSatisfy` (< p1)
                p1 `shouldSatisfy` (< p0)
              _ -> expectationFailure "All edges should be present"
          Left _ -> expectationFailure "Expected acyclic"

    describe "collectReachableNodes" $ do
      it "returns singleton for base type" $ do
        let nodes = nodeMapSingleton 0 (TyBase (NodeId 0) (BaseTy "Int"))
        collectReachableNodes nodes (NodeId 0) `shouldBe` intSetFromList [0]

      it "returns singleton for isolated variable" $ do
        let nodes = nodeMapSingleton 0 (TyVar {tnId = NodeId 0, tnBound = Nothing})
        collectReachableNodes nodes (NodeId 0) `shouldBe` intSetFromList [0]

      it "traverses arrow domain and codomain" $ do
        let nodes =
              nodeMapFromList
                [ (0, TyArrow (NodeId 0) (NodeId 1) (NodeId 2)),
                  (1, TyBase (NodeId 1) (BaseTy "Int")),
                  (2, TyBase (NodeId 2) (BaseTy "Bool"))
                ]
        collectReachableNodes nodes (NodeId 0) `shouldBe` intSetFromList [0, 1, 2]

      it "handles nested arrows" $ do
        let nodes =
              nodeMapFromList
                [ (0, TyArrow (NodeId 0) (NodeId 1) (NodeId 2)), -- outer
                  (1, TyArrow (NodeId 1) (NodeId 3) (NodeId 4)), -- inner in domain
                  (2, TyBase (NodeId 2) (BaseTy "Bool")),
                  (3, TyBase (NodeId 3) (BaseTy "Int")),
                  (4, TyVar {tnId = NodeId 4, tnBound = Nothing})
                ]
        collectReachableNodes nodes (NodeId 0) `shouldBe` intSetFromList [0, 1, 2, 3, 4]

      it "traverses forall body" $ do
        let nodes =
              nodeMapFromList
                [ (0, TyForall (NodeId 0) (NodeId 1)), -- ∀g.body
                  (1, TyArrow (NodeId 1) (NodeId 2) (NodeId 3)), -- body = Int → β
                  (2, TyBase (NodeId 2) (BaseTy "Int")),
                  (3, TyVar {tnId = NodeId 3, tnBound = Nothing}) -- β
                ]
        collectReachableNodes nodes (NodeId 0) `shouldBe` intSetFromList [0, 1, 2, 3]

      it "traverses expansion body" $ do
        let nodes =
              nodeMapFromList
                [ (0, TyExp (NodeId 0) (ExpVarId 0) (NodeId 1)), -- s · body
                  (1, TyArrow (NodeId 1) (NodeId 2) (NodeId 3)), -- body = α → β
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing}), -- α
                  (3, TyVar {tnId = NodeId 3, tnBound = Nothing}) -- β
                ]
        collectReachableNodes nodes (NodeId 0) `shouldBe` intSetFromList [0, 1, 2, 3]

      it "handles missing node gracefully" $ do
        let nodes = nodeMapSingleton 0 (TyVar {tnId = NodeId 0, tnBound = Nothing})
        -- NodeId 99 doesn't exist, should just return {99}
        collectReachableNodes nodes (NodeId 99) `shouldBe` intSetFromList [99]

    describe "Dependency graph construction" $ do
      it "builds empty graph for empty constraint" $ do
        let g = buildDependencyGraph emptyConstraint
        dgVertices g `shouldBe` []

      it "builds graph with correct vertices" $ do
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing})
                ]
            edges = [InstEdge (EdgeId 42) (NodeId 0) (NodeId 1)]
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = edges}
            g = buildDependencyGraph constraint
        dgVertices g `shouldBe` [EdgeId 42]

      it "creates dependency edge when nodes overlap" $ do
        -- e₁: α ≤ β, e₂: β ≤ γ
        -- e₁ depends on e₂ (e₂.left = β intersects e₁.right = β)
        let nodes =
              nodeMapFromList
                [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
                  (1, TyVar {tnId = NodeId 1, tnBound = Nothing}),
                  (2, TyVar {tnId = NodeId 2, tnBound = Nothing})
                ]
            e1 = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
            e2 = InstEdge (EdgeId 1) (NodeId 1) (NodeId 2)
            constraint = emptyConstraint {cNodes = nodes, cInstEdges = [e1, e2]}
            g = buildDependencyGraph constraint
        -- e₁ should depend on e₂
        case IntMap.lookup 0 (dgEdges g) of
          Just deps -> deps `shouldContain` [EdgeId 1]
          Nothing -> expectationFailure "Edge 0 should have dependencies"

    describe "isAcyclic helper" $ do
      it "returns True for empty graph" $ do
        let g = DepGraph [] IntMap.empty
        isAcyclic g `shouldBe` True

      it "returns True for single vertex" $ do
        let g = DepGraph [EdgeId 0] (IntMap.singleton 0 [])
        isAcyclic g `shouldBe` True

      it "returns False for self-loop" $ do
        let g = DepGraph [EdgeId 0] (IntMap.singleton 0 [EdgeId 0])
        isAcyclic g `shouldBe` False

    describe "findCycle helper" $ do
      it "returns Nothing for acyclic graph" $ do
        let g =
              DepGraph
                [EdgeId 0, EdgeId 1]
                (IntMap.fromList [(0, [EdgeId 1]), (1, [])])
        findCycle g `shouldBe` Nothing

      it "returns Just cycle for cyclic graph" $ do
        let g =
              DepGraph
                [EdgeId 0, EdgeId 1]
                (IntMap.fromList [(0, [EdgeId 1]), (1, [EdgeId 0])])
        findCycle g `shouldSatisfy` isJust

  acyclicityObligationsSpec

-- Helper functions

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

findIndex :: (Eq a) => a -> [a] -> Maybe Int
findIndex x xs = go 0 xs
  where
    go _ [] = Nothing
    go i (y : ys)
      | x == y = Just i
      | otherwise = go (i + 1) ys

intSetFromList :: [Int] -> IntSet.IntSet
intSetFromList = foldr IntSet.insert IntSet.empty

acyclicityObligationsSpec :: Spec
acyclicityObligationsSpec = describe "Thesis obligations (Chapter 12)" $ do
  it "O12-ACYCLIC-CHECK" $ do
    -- Acyclicity check: checkAcyclicity accepts acyclic constraints
    let nodes =
          nodeMapFromList
            [ (0, TyVar {tnId = NodeId 0, tnBound = Nothing}),
              (1, TyVar {tnId = NodeId 1, tnBound = Nothing})
            ]
        edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
        constraint = emptyConstraint {cNodes = nodes, cInstEdges = [edge]}
    checkAcyclicityRaw constraint `shouldSatisfy` isRight

  it "O12-ACYCLIC-TOPO" $ do
    -- Topological sort: topologicalSort orders edges without cycles
    let depGraph =
          DepGraph
            { dgVertices = [EdgeId 0, EdgeId 1],
              dgEdges = IntMap.fromList [(0, [EdgeId 1])]
            }
    case topologicalSort depGraph of
      Right order -> order `shouldSatisfy` (not . null)
      Left _cycle -> expectationFailure "Expected acyclic graph"
