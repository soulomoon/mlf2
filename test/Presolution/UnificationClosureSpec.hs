module Presolution.UnificationClosureSpec (spec) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (isInfixOf)
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionResult(..)
    , computePresolution
    , toListInterior
    , validateTranslatablePresolution
    )
import MLF.Constraint.Types.Graph (BaseTy(..), BindFlag(..), Constraint(..), EdgeId(..), ExpVarId(..), InstEdge(..), NodeId(..), TyNode(..), UnifyEdge(..))
import MLF.Constraint.Types.Witness (EdgeWitness(..), InstanceOp(..), InstanceWitness(..))
import MLF.Constraint.Unify.Closure (SolveError(..), runUnifyClosureWithSeed)
import MLF.Frontend.Syntax (Expr(..), Lit(..))
import SpecUtil
    ( bindParentsFromPairs
    , defaultTraceConfig
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

    it "characterizes edge-boundary ordering by keeping OpWeaken targets inside each edge interior" $ do
        let a = NodeId 0
            arrow = NodeId 1
            forallNode = NodeId 2
            expNode = NodeId 3
            intNode = NodeId 4
            targetArrow = NodeId 5
            rootArrow = NodeId 6
            nodes =
                nodeMapFromList
                    [ (getNodeId a, TyVar { tnId = a, tnBound = Nothing })
                    , (getNodeId arrow, TyArrow arrow a a)
                    , (getNodeId forallNode, TyForall forallNode arrow)
                    , (getNodeId expNode, TyExp expNode (ExpVarId 0) forallNode)
                    , (getNodeId intNode, TyBase intNode (BaseTy "Int"))
                    , (getNodeId targetArrow, TyArrow targetArrow intNode intNode)
                    , (getNodeId rootArrow, TyArrow rootArrow expNode targetArrow)
                    ]
            bindParents =
                bindParentsFromPairs
                    [ (a, forallNode, BindFlex)
                    , (arrow, forallNode, BindFlex)
                    , (forallNode, rootArrow, BindFlex)
                    , (expNode, rootArrow, BindFlex)
                    , (intNode, targetArrow, BindFlex)
                    , (targetArrow, rootArrow, BindFlex)
                    ]
            edge = InstEdge (EdgeId 0) expNode targetArrow
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes = nodes
                        , cInstEdges = [edge]
                        , cBindParents = bindParents
                        }
            acyc =
                AcyclicityResult
                    { arSortedEdges = [edge]
                    , arDepGraph = undefined
                    }
        pres <- requireRight (computePresolution defaultTraceConfig acyc constraint)
        let weakenTargetsForWitness :: EdgeWitness -> [NodeId]
            weakenTargetsForWitness ew =
                [ n
                | op <- getInstanceOps (ewWitness ew)
                , n <- case op of
                    OpWeaken nid -> [nid]
                    _ -> []
                ]
            weakenTargetsByEdge =
                [ (eid, weakenTargetsForWitness ew)
                | (eid, ew) <- IntMap.toList (prEdgeWitnesses pres)
                ]
            nonEmptyWeakenEdges = [eid | (eid, ns) <- weakenTargetsByEdge, not (null ns)]
        nonEmptyWeakenEdges `shouldSatisfy` (not . null)
        forM_ weakenTargetsByEdge $ \(eid, weakenTargets) ->
            case IntMap.lookup eid (prEdgeTraces pres) of
                Nothing ->
                    expectationFailure ("Missing edge trace for witness key " ++ show eid)
                Just tr -> do
                    let interiorKeys =
                            IntSet.fromList
                                [ getNodeId nid
                                | nid <- toListInterior (etInterior tr)
                                ]
                    forM_ weakenTargets $ \nid ->
                        IntSet.member (getNodeId nid) interiorKeys `shouldBe` True

    it "row3 absolute thesis-exact guard: unification closure removes loop-final weaken flush fallback" $ do
        edgeSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing.hs"
        edgeSrc `shouldSatisfy` (not . isInfixOf "flushPendingWeakens\n    drainPendingUnifyClosureIfNeeded")

    it "row3 absolute thesis-exact guard: unification closure requires owner-aware scheduler API markers" $ do
        edgeSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing.hs"
        forM_
            [ "scheduleWeakensByOwnerBoundary"
            , "flushPendingWeakensAtOwnerBoundary"
            , "assertNoPendingWeakensOutsideOwnerBoundary"
            ] $ \marker ->
                edgeSrc `shouldSatisfy` isInfixOf marker

    it "row3 absolute thesis-exact guard: unification closure retires the legacy flush-all entrypoint" $ do
        edgeUnifySrc <- readFile "src/MLF/Constraint/Presolution/EdgeUnify.hs"
        forM_
            [ "flushPendingWeakens,"
            , "flushPendingWeakens :: PresolutionM ()"
            , "flushPendingWeakens = flushPendingWeakensWhere (const True)"
            ] $ \marker ->
                edgeUnifySrc `shouldSatisfy` (not . isInfixOf marker)

    it "row3 absolute thesis-exact guard: unification closure forbids flush-all-owner boundary fallback" $ do
        edgeSrc <- readFile "src/MLF/Constraint/Presolution/EdgeProcessing.hs"
        edgeSrc `shouldSatisfy` (not . isInfixOf "forM_ owners flushPendingWeakensAtOwnerBoundary")
        edgeSrc `shouldSatisfy` isInfixOf "let boundaryOwner = pendingWeakenOwnerFromMaybe mbCurrentOwner"
