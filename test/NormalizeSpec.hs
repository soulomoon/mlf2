module NormalizeSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import Test.Hspec

import MLF.Types
import MLF.Normalize

spec :: Spec
spec = describe "Phase 2 — Normalization" $ do
    describe "Reflexive edge removal" $ do
        it "drops reflexive instantiation edges (T ≤ T)" $ do
            let node = TyVar (NodeId 0) (GNodeId 0)
                reflexiveEdge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 0)
                constraint = emptyConstraint
                    { cNodes = IntMap.singleton 0 node
                    , cInstEdges = [reflexiveEdge]
                    }
            cInstEdges (normalize constraint) `shouldBe` []

        it "keeps non-reflexive inst edges between variables" $ do
            -- α ≤ β where both are variables: can't graft, keep the edge
            let node1 = TyVar (NodeId 0) (GNodeId 0)
                node2 = TyVar (NodeId 1) (GNodeId 0)
                edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList [(0, node1), (1, node2)]
                    , cInstEdges = [edge]
                    }
            length (cInstEdges (normalize constraint)) `shouldBe` 1

        it "handles mixed reflexive and non-reflexive inst edges" $ do
            -- α ≤ α (reflexive, drop)
            -- α ≤ β (both vars initially, but after β ≤ Int grafts, becomes α ≤ Int, which grafts)
            -- β ≤ Int (grafts: unifies β with Int)
            let nodes = IntMap.fromList
                    [ (0, TyVar (NodeId 0) (GNodeId 0))
                    , (1, TyVar (NodeId 1) (GNodeId 0))
                    , (2, TyBase (NodeId 2) (BaseTy "Int"))
                    ]
                instEdges =
                    [ InstEdge (EdgeId 0) (NodeId 0) (NodeId 0)  -- reflexive, drop
                    , InstEdge (EdgeId 1) (NodeId 0) (NodeId 1)  -- α ≤ β: after β=Int, becomes α ≤ Int, grafts
                    , InstEdge (EdgeId 2) (NodeId 1) (NodeId 2)  -- β ≤ Int: grafts to β = Int
                    ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = instEdges
                    }
                result = normalize constraint
            -- All edges are consumed:
            -- Edge 0: dropped (reflexive)
            -- Edge 2: converts β ≤ Int → β = Int (merged)
            -- Edge 1: now α ≤ Int (since β → Int in union-find), grafts to α = Int (merged)
            length (cInstEdges result) `shouldBe` 0

    describe "Fixed-point behavior" $ do
        it "reaches fixed point on empty constraint" $ do
            let result = normalize emptyConstraint
            result `shouldBe` emptyConstraint

        it "is idempotent (applying twice gives same result)" $ do
            let nodes = IntMap.fromList
                    [ (0, TyVar (NodeId 0) (GNodeId 0))
                    , (1, TyVar (NodeId 1) (GNodeId 0))
                    ]
                edges = [InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cInstEdges = edges
                    }
                once = normalize constraint
                twice = normalize once
            twice `shouldBe` once

    describe "Grafting" $ do
        it "grafts arrow structure onto variable" $ do
            -- α ≤ (Int → Bool) should graft arrow structure onto α
            let domNode = TyBase (NodeId 1) (BaseTy "Int")
                codNode = TyBase (NodeId 2) (BaseTy "Bool")
                arrowNode = TyArrow (NodeId 3) (NodeId 1) (NodeId 2)
                varNode = TyVar (NodeId 0) (GNodeId 0)
                edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 3)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList
                        [(0, varNode), (1, domNode), (2, codNode), (3, arrowNode)]
                    , cInstEdges = [edge]
                    }
                result = normalize constraint
            -- The inst edge should be consumed (grafted)
            cInstEdges result `shouldBe` []
            -- New arrow node should exist that unifies with α
            -- Check that nodes count increased (fresh arrow + 2 fresh vars)
            IntMap.size (cNodes result) `shouldSatisfy` (> 4)

        it "grafts base type onto variable" $ do
            -- α ≤ Int should unify α with Int
            let baseNode = TyBase (NodeId 1) (BaseTy "Int")
                varNode = TyVar (NodeId 0) (GNodeId 0)
                edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList [(0, varNode), (1, baseNode)]
                    , cInstEdges = [edge]
                    }
                result = normalize constraint
            -- Inst edge is converted to unify edge and merged
            cInstEdges result `shouldBe` []
            cUnifyEdges result `shouldBe` []

        it "rejects grafting when RHS arrow contains the LHS variable (occurs-check)" $ do
            -- α ≤ (α → Int) should be a type error and left for later phases
            let varNode = TyVar (NodeId 0) (GNodeId 0)
                baseNode = TyBase (NodeId 2) (BaseTy "Int")
                arrowNode = TyArrow (NodeId 1) (NodeId 0) (NodeId 2)
                edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, varNode)
                        , (1, arrowNode)
                        , (2, baseNode)
                        ]
                    , cInstEdges = [edge]
                    }
                result = normalize constraint
            -- Type error: keep the inst edge to signal unsolved occurs-check
            length (cInstEdges result) `shouldBe` 1

        it "rejects grafting when RHS Forall contains the LHS variable (occurs-check)" $ do
            -- α ≤ ∀(β). α
            let varNode = TyVar (NodeId 0) (GNodeId 0)
                forallNode = TyForall (NodeId 1) (GNodeId 0) (GNodeId 1) (NodeId 0)
                -- We need a TyArrow to trigger the check in grafting, as Forall/Exp are filtered out
                -- So: α ≤ (Int → (∀(β). α))
                intNode = TyBase (NodeId 2) (BaseTy "Int")
                arrowNode = TyArrow (NodeId 3) (NodeId 2) (NodeId 1)
                edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 3)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, varNode), (1, forallNode), (2, intNode), (3, arrowNode) ]
                    , cInstEdges = [edge]
                    }
                result = normalize constraint
            length (cInstEdges result) `shouldBe` 1

        it "rejects grafting when RHS Expansion contains the LHS variable (occurs-check)" $ do
             -- α ≤ (Int → (s · α))
             let varNode = TyVar (NodeId 0) (GNodeId 0)
                 expNode = TyExp (NodeId 1) (ExpVarId 0) (NodeId 0)
                 intNode = TyBase (NodeId 2) (BaseTy "Int")
                 arrowNode = TyArrow (NodeId 3) (NodeId 2) (NodeId 1)
                 edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 3)
                 constraint = emptyConstraint
                     { cNodes = IntMap.fromList
                         [ (0, varNode), (1, expNode), (2, intNode), (3, arrowNode) ]
                     , cInstEdges = [edge]
                     }
                 result = normalize constraint
             length (cInstEdges result) `shouldBe` 1

        it "decomposes arrow ≤ arrow into component unifications" $ do
            -- (α → β) ≤ (Int → Bool)
            let dom1 = TyVar (NodeId 0) (GNodeId 0)
                cod1 = TyVar (NodeId 1) (GNodeId 0)
                arr1 = TyArrow (NodeId 2) (NodeId 0) (NodeId 1)
                dom2 = TyBase (NodeId 3) (BaseTy "Int")
                cod2 = TyBase (NodeId 4) (BaseTy "Bool")
                arr2 = TyArrow (NodeId 5) (NodeId 3) (NodeId 4)
                edge = InstEdge (EdgeId 0) (NodeId 2) (NodeId 5)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList
                        [ (0, dom1), (1, cod1), (2, arr1)
                        , (3, dom2), (4, cod2), (5, arr2)
                        ]
                    , cInstEdges = [edge]
                    }
                result = normalize constraint
            -- Arrow ≤ Arrow becomes α = Int, β = Bool, all merged
            cInstEdges result `shouldBe` []
            cUnifyEdges result `shouldBe` []

        it "keeps Base ≤ Base (same type) as satisfied" $ do
            -- Int ≤ Int is trivially satisfied
            let node1 = TyBase (NodeId 0) (BaseTy "Int")
                node2 = TyBase (NodeId 1) (BaseTy "Int")
                edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList [(0, node1), (1, node2)]
                    , cInstEdges = [edge]
                    }
                result = normalize constraint
            -- Same base types: edge is consumed, no error
            cInstEdges result `shouldBe` []
            cUnifyEdges result `shouldBe` []

        it "keeps Base ≤ Base (different types) as type error" $ do
            -- Int ≤ Bool is a type error
            let node1 = TyBase (NodeId 0) (BaseTy "Int")
                node2 = TyBase (NodeId 1) (BaseTy "Bool")
                edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 1)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList [(0, node1), (1, node2)]
                    , cInstEdges = [edge]
                    }
                result = normalize constraint
            -- Type error: incompatible base types, edge is kept
            length (cInstEdges result) `shouldBe` 1

        it "keeps Arrow ≤ Base as type error" $ do
            -- (α → β) ≤ Int is a type error
            let dom = TyVar (NodeId 0) (GNodeId 0)
                cod = TyVar (NodeId 1) (GNodeId 0)
                arr = TyArrow (NodeId 2) (NodeId 0) (NodeId 1)
                base = TyBase (NodeId 3) (BaseTy "Int")
                edge = InstEdge (EdgeId 0) (NodeId 2) (NodeId 3)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList [(0, dom), (1, cod), (2, arr), (3, base)]
                    , cInstEdges = [edge]
                    }
                result = normalize constraint
            -- Type error: arrow vs base, edge is kept
            length (cInstEdges result) `shouldBe` 1

        it "keeps Base ≤ Arrow as type error" $ do
            -- Int ≤ (α → β) is a type error
            let base = TyBase (NodeId 0) (BaseTy "Int")
                dom = TyVar (NodeId 1) (GNodeId 0)
                cod = TyVar (NodeId 2) (GNodeId 0)
                arr = TyArrow (NodeId 3) (NodeId 1) (NodeId 2)
                edge = InstEdge (EdgeId 0) (NodeId 0) (NodeId 3)
                constraint = emptyConstraint
                    { cNodes = IntMap.fromList [(0, base), (1, dom), (2, cod), (3, arr)]
                    , cInstEdges = [edge]
                    }
                result = normalize constraint
            -- Type error: base vs arrow, edge is kept
            length (cInstEdges result) `shouldBe` 1

    describe "Merging" $ do
        -- Var = Var cases
        describe "Var = Var" $ do
            it "merges two variables via union-find" $ do
                -- α = β: both variables, merged via union-find
                let node1 = TyVar (NodeId 0) (GNodeId 0)
                    node2 = TyVar (NodeId 1) (GNodeId 0)
                    edge = UnifyEdge (NodeId 0) (NodeId 1)
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList [(0, node1), (1, node2)]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                -- Edge is consumed by union-find
                cUnifyEdges result `shouldBe` []

            it "raises variable levels to the LCA during merging (rank adjustment)" $ do
                -- Paper Raise(n) / rank adjustment: before Var/Var union, raise both vars
                -- to their lowest common binder and move `gBinds` accordingly.
                let g0 =
                        GNode
                            { gnodeId = GNodeId 0
                            , gParent = Nothing
                            , gBinds = [(NodeId 1, Nothing)]
                            , gChildren = [GNodeId 1]
                            }
                    g1 =
                        GNode
                            { gnodeId = GNodeId 1
                            , gParent = Just (GNodeId 0)
                            , gBinds = [(NodeId 0, Nothing)]
                            , gChildren = []
                            }
                    nodeInner = TyVar (NodeId 0) (GNodeId 1)
                    nodeOuter = TyVar (NodeId 1) (GNodeId 0)
                    edge = UnifyEdge (NodeId 1) (NodeId 0)
                    constraint =
                        emptyConstraint
                            { cGNodes = IntMap.fromList [(0, g0), (1, g1)]
                            , cNodes = IntMap.fromList [(0, nodeInner), (1, nodeOuter)]
                            , cUnifyEdges = [edge]
                            }
                    result = normalize constraint

                cUnifyEdges result `shouldBe` []
                IntMap.lookup 0 (cNodes result) `shouldBe` Just (TyVar (NodeId 0) (GNodeId 0))
                IntMap.lookup 1 (cNodes result) `shouldBe` Just (TyVar (NodeId 1) (GNodeId 0))
                IntMap.lookup 0 (cGNodes result)
                    `shouldBe`
                        Just
                            g0
                                { gBinds =
                                    [ (NodeId 0, Nothing)
                                    , (NodeId 1, Nothing)
                                    ]
                                }
                IntMap.lookup 1 (cGNodes result) `shouldBe` Just (g1 { gBinds = [] })

            it "handles chained variable unifications (transitivity)" $ do
                -- α = β, β = γ: should all be unified
                let node1 = TyVar (NodeId 0) (GNodeId 0)
                    node2 = TyVar (NodeId 1) (GNodeId 0)
                    node3 = TyVar (NodeId 2) (GNodeId 0)
                    edges = [UnifyEdge (NodeId 0) (NodeId 1), UnifyEdge (NodeId 1) (NodeId 2)]
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList [(0, node1), (1, node2), (2, node3)]
                        , cUnifyEdges = edges
                        }
                    result = normalize constraint
                cUnifyEdges result `shouldBe` []

        -- Var = Structure cases
        describe "Var = Structure" $ do
            it "unifies variable with arrow (var points to arrow)" $ do
                -- α = (Int → Bool)
                let varNode = TyVar (NodeId 0) (GNodeId 0)
                    domNode = TyBase (NodeId 1) (BaseTy "Int")
                    codNode = TyBase (NodeId 2) (BaseTy "Bool")
                    arrNode = TyArrow (NodeId 3) (NodeId 1) (NodeId 2)
                    edge = UnifyEdge (NodeId 0) (NodeId 3)
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList
                            [(0, varNode), (1, domNode), (2, codNode), (3, arrNode)]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                cUnifyEdges result `shouldBe` []

            it "unifies arrow with variable (same as var = arrow)" $ do
                -- (Int → Bool) = α: symmetric case
                let varNode = TyVar (NodeId 0) (GNodeId 0)
                    domNode = TyBase (NodeId 1) (BaseTy "Int")
                    codNode = TyBase (NodeId 2) (BaseTy "Bool")
                    arrNode = TyArrow (NodeId 3) (NodeId 1) (NodeId 2)
                    edge = UnifyEdge (NodeId 3) (NodeId 0)  -- Arrow on left
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList
                            [(0, varNode), (1, domNode), (2, codNode), (3, arrNode)]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                cUnifyEdges result `shouldBe` []

            it "unifies variable with base type" $ do
                -- α = Int
                let varNode = TyVar (NodeId 0) (GNodeId 0)
                    baseNode = TyBase (NodeId 1) (BaseTy "Int")
                    edge = UnifyEdge (NodeId 0) (NodeId 1)
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList [(0, varNode), (1, baseNode)]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                cUnifyEdges result `shouldBe` []

            it "unifies base type with variable (symmetric)" $ do
                -- Int = α
                let varNode = TyVar (NodeId 0) (GNodeId 0)
                    baseNode = TyBase (NodeId 1) (BaseTy "Int")
                    edge = UnifyEdge (NodeId 1) (NodeId 0)  -- Base on left
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList [(0, varNode), (1, baseNode)]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                cUnifyEdges result `shouldBe` []

        -- Arrow = Arrow cases
        describe "Arrow = Arrow" $ do
            it "merges arrows by unifying components" $ do
                -- (α → β) = (γ → δ): generates α = γ, β = δ
                let dom1 = TyVar (NodeId 0) (GNodeId 0)
                    cod1 = TyVar (NodeId 1) (GNodeId 0)
                    arr1 = TyArrow (NodeId 2) (NodeId 0) (NodeId 1)
                    dom2 = TyVar (NodeId 3) (GNodeId 0)
                    cod2 = TyVar (NodeId 4) (GNodeId 0)
                    arr2 = TyArrow (NodeId 5) (NodeId 3) (NodeId 4)
                    edge = UnifyEdge (NodeId 2) (NodeId 5)
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList
                            [ (0, dom1), (1, cod1), (2, arr1)
                            , (3, dom2), (4, cod2), (5, arr2)
                            ]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                cUnifyEdges result `shouldBe` []

            it "merges nested arrows recursively" $ do
                -- (α → (β → γ)) = (Int → (Bool → δ))
                let alpha = TyVar (NodeId 0) (GNodeId 0)
                    beta = TyVar (NodeId 1) (GNodeId 0)
                    gamma = TyVar (NodeId 2) (GNodeId 0)
                    inner1 = TyArrow (NodeId 3) (NodeId 1) (NodeId 2)
                    outer1 = TyArrow (NodeId 4) (NodeId 0) (NodeId 3)
                    intNode = TyBase (NodeId 5) (BaseTy "Int")
                    boolNode = TyBase (NodeId 6) (BaseTy "Bool")
                    delta = TyVar (NodeId 7) (GNodeId 0)
                    inner2 = TyArrow (NodeId 8) (NodeId 6) (NodeId 7)
                    outer2 = TyArrow (NodeId 9) (NodeId 5) (NodeId 8)
                    edge = UnifyEdge (NodeId 4) (NodeId 9)
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList
                            [ (0, alpha), (1, beta), (2, gamma), (3, inner1), (4, outer1)
                            , (5, intNode), (6, boolNode), (7, delta), (8, inner2), (9, outer2)
                            ]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                -- All edges resolved: α = Int, β = Bool, γ = δ
                cUnifyEdges result `shouldBe` []

        -- Base = Base cases
        describe "Base = Base" $ do
            it "succeeds when base types are identical" $ do
                -- Int = Int
                let node1 = TyBase (NodeId 0) (BaseTy "Int")
                    node2 = TyBase (NodeId 1) (BaseTy "Int")
                    edge = UnifyEdge (NodeId 0) (NodeId 1)
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList [(0, node1), (1, node2)]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                cUnifyEdges result `shouldBe` []

            it "reports error for incompatible base types" $ do
                -- Int = Bool: type error
                let node1 = TyBase (NodeId 0) (BaseTy "Int")
                    node2 = TyBase (NodeId 1) (BaseTy "Bool")
                    edge = UnifyEdge (NodeId 0) (NodeId 1)
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList [(0, node1), (1, node2)]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                length (cUnifyEdges result) `shouldBe` 1

        -- Forall = Forall cases
        describe "Forall = Forall" $ do
            it "unifies bodies when levels match" $ do
                -- ∀(α). α = ∀(β). β
                let var1 = TyVar (NodeId 0) (GNodeId 1)
                    forall1 = TyForall (NodeId 1) (GNodeId 0) (GNodeId 1) (NodeId 0)
                    var2 = TyVar (NodeId 2) (GNodeId 1)
                    forall2 = TyForall (NodeId 3) (GNodeId 0) (GNodeId 1) (NodeId 2)
                    edge = UnifyEdge (NodeId 1) (NodeId 3)
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList
                            [ (0, var1), (1, forall1), (2, var2), (3, forall2) ]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                cUnifyEdges result `shouldBe` []

            it "reports error when levels mismatch" $ do
                -- ∀(α at g1). ... = ∀(β at g2). ...
                -- TyForall constructor is: TyForall nid quantLvl ownerLvl body
                -- To mismatch levels, we need different quantLvl
                let forall1 = TyForall (NodeId 0) (GNodeId 1) (GNodeId 0) (NodeId 2)
                    forall2 = TyForall (NodeId 1) (GNodeId 2) (GNodeId 0) (NodeId 3)
                    -- Dummy body nodes
                    var1 = TyVar (NodeId 2) (GNodeId 1)
                    var2 = TyVar (NodeId 3) (GNodeId 2)
                    edge = UnifyEdge (NodeId 0) (NodeId 1)
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList
                            [ (0, forall1), (1, forall2), (2, var1), (3, var2) ]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                length (cUnifyEdges result) `shouldBe` 1

        -- Expansion = Expansion cases
        describe "Exp = Exp" $ do
            it "unifies bodies when expansion vars match" $ do
                -- s · α = s · β
                let var1 = TyVar (NodeId 0) (GNodeId 0)
                    exp1 = TyExp (NodeId 1) (ExpVarId 0) (NodeId 0)
                    var2 = TyVar (NodeId 2) (GNodeId 0)
                    exp2 = TyExp (NodeId 3) (ExpVarId 0) (NodeId 2)
                    edge = UnifyEdge (NodeId 1) (NodeId 3)
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList
                            [ (0, var1), (1, exp1), (2, var2), (3, exp2) ]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                cUnifyEdges result `shouldBe` []

            it "reports error when expansion vars mismatch" $ do
                -- s1 · ... = s2 · ...
                let exp1 = TyExp (NodeId 0) (ExpVarId 0) (NodeId 2)
                    exp2 = TyExp (NodeId 1) (ExpVarId 1) (NodeId 3)
                    var1 = TyVar (NodeId 2) (GNodeId 0)
                    var2 = TyVar (NodeId 3) (GNodeId 0)
                    edge = UnifyEdge (NodeId 0) (NodeId 1)
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList
                            [ (0, exp1), (1, exp2), (2, var1), (3, var2) ]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                length (cUnifyEdges result) `shouldBe` 1

        -- Incompatible structure cases (type errors)
        describe "Incompatible structures (type errors)" $ do
            it "reports error for Arrow = Base" $ do
                -- (α → β) = Int: type error
                let dom = TyVar (NodeId 0) (GNodeId 0)
                    cod = TyVar (NodeId 1) (GNodeId 0)
                    arr = TyArrow (NodeId 2) (NodeId 0) (NodeId 1)
                    base = TyBase (NodeId 3) (BaseTy "Int")
                    edge = UnifyEdge (NodeId 2) (NodeId 3)
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList [(0, dom), (1, cod), (2, arr), (3, base)]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                length (cUnifyEdges result) `shouldBe` 1

            it "reports error for Base = Arrow" $ do
                -- Int = (α → β): type error (symmetric)
                let dom = TyVar (NodeId 0) (GNodeId 0)
                    cod = TyVar (NodeId 1) (GNodeId 0)
                    arr = TyArrow (NodeId 2) (NodeId 0) (NodeId 1)
                    base = TyBase (NodeId 3) (BaseTy "Int")
                    edge = UnifyEdge (NodeId 3) (NodeId 2)  -- Base on left
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList [(0, dom), (1, cod), (2, arr), (3, base)]
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                length (cUnifyEdges result) `shouldBe` 1

        -- Complex / edge cases
        describe "Complex scenarios" $ do
            it "propagates through multiple edges correctly" $ do
                -- α = β, β = Int: after normalization, both point to Int
                let alpha = TyVar (NodeId 0) (GNodeId 0)
                    beta = TyVar (NodeId 1) (GNodeId 0)
                    intNode = TyBase (NodeId 2) (BaseTy "Int")
                    edges = [UnifyEdge (NodeId 0) (NodeId 1), UnifyEdge (NodeId 1) (NodeId 2)]
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList [(0, alpha), (1, beta), (2, intNode)]
                        , cUnifyEdges = edges
                        }
                    result = normalize constraint
                cUnifyEdges result `shouldBe` []

            it "handles multiple unifications of same variable" $ do
                -- α = Int, α = Int: redundant but valid
                let alpha = TyVar (NodeId 0) (GNodeId 0)
                    int1 = TyBase (NodeId 1) (BaseTy "Int")
                    int2 = TyBase (NodeId 2) (BaseTy "Int")
                    edges = [UnifyEdge (NodeId 0) (NodeId 1), UnifyEdge (NodeId 0) (NodeId 2)]
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList [(0, alpha), (1, int1), (2, int2)]
                        , cUnifyEdges = edges
                        }
                    result = normalize constraint
                cUnifyEdges result `shouldBe` []

            it "detects error in chained unifications" $ do
                -- α = Int, α = Bool: type error discovered via chain
                let alpha = TyVar (NodeId 0) (GNodeId 0)
                    intNode = TyBase (NodeId 1) (BaseTy "Int")
                    boolNode = TyBase (NodeId 2) (BaseTy "Bool")
                    edges = [UnifyEdge (NodeId 0) (NodeId 1), UnifyEdge (NodeId 0) (NodeId 2)]
                    constraint = emptyConstraint
                        { cNodes = IntMap.fromList [(0, alpha), (1, intNode), (2, boolNode)]
                        , cUnifyEdges = edges
                        }
                    result = normalize constraint
                -- After α = Int, we have α → Int. Then Int = Bool is a type error.
                length (cUnifyEdges result) `shouldBe` 1

            it "handles reflexive unification edge (T = T)" $ do
                -- α = α: trivially satisfied
                let alpha = TyVar (NodeId 0) (GNodeId 0)
                    edge = UnifyEdge (NodeId 0) (NodeId 0)
                    constraint = emptyConstraint
                        { cNodes = IntMap.singleton 0 alpha
                        , cUnifyEdges = [edge]
                        }
                    result = normalize constraint
                cUnifyEdges result `shouldBe` []

-- | An empty constraint for testing.
emptyConstraint :: Constraint
emptyConstraint = Constraint
    { cGForest = [GNodeId 0]
    , cGNodes = IntMap.singleton 0 GNode
        { gnodeId = GNodeId 0
        , gParent = Nothing
        , gBinds = []
        , gChildren = []
        }
    , cNodes = IntMap.empty
    , cInstEdges = []
    , cUnifyEdges = []
    }
