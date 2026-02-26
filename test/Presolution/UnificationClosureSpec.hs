module Presolution.UnificationClosureSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import MLF.Constraint.Presolution
    ( PresolutionResult(..)
    , computePresolution
    , validateTranslatablePresolution
    )
import MLF.Constraint.Types.Graph (BaseTy(..), Constraint(..), EdgeId(..), ExpVarId(..), InstEdge(..), NodeId(..), TyNode(..), UnifyEdge(..))
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
