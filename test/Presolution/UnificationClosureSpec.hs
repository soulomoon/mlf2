module Presolution.UnificationClosureSpec (spec) where

import Data.Either (isLeft)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution
    ( PresolutionResult(..)
    , computePresolution
    , validateTranslatablePresolution
    )
import MLF.Constraint.Types.Graph (BaseTy(..), Constraint(..), EdgeId(..), ExpVarId(..), InstEdge(..), NodeId(..), TyNode(..), UnifyEdge(..))
import MLF.Constraint.Unify.Closure (SolveError(..), runUnifyClosureWithSeed)
import MLF.Frontend.Syntax (Expr(..), Lit(..))
import SpecUtil
    ( defaultTraceConfig
    , emptyConstraint
    , inferBindParents
    , nodeMapFromList
    , requireRight
    , rootedConstraint
    , runToPresolutionDefault
    )

spec :: Spec
spec = describe "Phase 4 thesis-exact unification closure" $ do
    it "drains pending unify edges by end of presolution" $ do
        pres <-
            requireRight
                (runToPresolutionDefault Set.empty (EApp (ELam "x" (EVar "x")) (ELit (LInt 1))))
        cUnifyEdges (prConstraint pres) `shouldBe` []

    it "re-validates translatable presolution after unification closure" $ do
        pres <-
            requireRight
                ( runToPresolutionDefault
                    Set.empty
                    (ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1))))
                )
        validateTranslatablePresolution (prConstraint pres) `shouldBe` Right ()

    it "exposes presolution UF metadata without assuming non-empty UF" $ do
        pres <- requireRight (runToPresolutionDefault Set.empty (ELam "x" (EVar "x")))
        let _uf = prUnionFind pres
        pure ()

    it "solves initial unify edges before inst-edge traversal effects are persisted" $ do
        let bodyId = NodeId 0
            targetId = NodeId 1
            expNodeId = NodeId 2
            rootId = NodeId 3
            boolId = NodeId 4
            nodes =
                nodeMapFromList
                    [ (0, TyBase bodyId (BaseTy "int"))
                    , (1, TyBase targetId (BaseTy "int"))
                    , (2, TyExp expNodeId (ExpVarId 0) bodyId)
                    , (3, TyArrow rootId expNodeId targetId)
                    , (4, TyBase boolId (BaseTy "bool"))
                    ]
            edge = InstEdge (EdgeId 0) expNodeId targetId
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        , cInstEdges = [edge]
                        , cUnifyEdges = [UnifyEdge targetId boolId]
                        }
            acyc = AcyclicityResult { arSortedEdges = [edge], arDepGraph = undefined }
        computePresolution defaultTraceConfig acyc constraint `shouldSatisfy` isLeft

    it "honors seeded UF equivalence when draining closure" $ do
        let n0 = NodeId 0
            n1 = NodeId 1
            n2 = NodeId 2
            nodes =
                nodeMapFromList
                    [ (0, TyVar { tnId = n0, tnBound = Nothing })
                    , (1, TyBase n1 (BaseTy "Int"))
                    , (2, TyBase n2 (BaseTy "Bool"))
                    ]
            c0 =
                rootedConstraint $
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = inferBindParents nodes
                        , cUnifyEdges = [UnifyEdge n0 n2]
                        }
            ufSeed = IntMap.fromList [(0, n1)]
        case runUnifyClosureWithSeed defaultTraceConfig ufSeed c0 of
            Left (BaseClash (BaseTy "Int") (BaseTy "Bool")) -> pure ()
            other ->
                expectationFailure
                    ( "Expected seeded closure to produce BaseClash Int/Bool, got: "
                        ++ show other
                    )

    it "produces TyExp-free presolution artifacts with drained edge queues" $ do
        pres <-
            requireRight
                ( runToPresolutionDefault
                    Set.empty
                    (ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1))))
                )
        let c = prConstraint pres
            tyExpNodes =
                [ tnId node
                | node@TyExp{} <- NodeAccess.allNodes c
                ]
        cUnifyEdges c `shouldBe` []
        cInstEdges c `shouldBe` []
        tyExpNodes `shouldBe` []

    it "keeps witness/trace keys aligned for retained instantiation edges" $ do
        pres <-
            requireRight
                ( runToPresolutionDefault
                    Set.empty
                    (ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id")))
                )
        let witnessKeys = IntSet.fromList (IntMap.keys (prEdgeWitnesses pres))
            traceKeys = IntSet.fromList (IntMap.keys (prEdgeTraces pres))
        witnessKeys `shouldBe` traceKeys
