{-# LANGUAGE DataKinds #-}
module Presolution.UnificationClosureSpec (spec) where

import IdentityTestSupport
import Control.Monad (forM_)
import Data.Either (isLeft)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Test.Hspec

import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import qualified MLF.Constraint.NodeAccess as NodeAccess
import MLF.Constraint.Presolution
    ( EdgeTrace(..)
    , PresolutionResult(..)
    )
import MLF.Constraint.Presolution.Base (PresolutionError(..), PresolutionUf(..))
import MLF.Constraint.Presolution.TestSupport
    ( PresolutionState(..)
    , getEdgeSourceInterior
    , runPresolutionM
    , toListInterior
    , unifyStructureForTest
    , validateTranslatablePresolution
    )
import MLF.Constraint.Types.Graph (BaseTy(..), BindFlag(..), Constraint(..), EdgeId(..), ExpVarId(..), GenNode(..), GenNodeId(..), InstEdge(..), NodeId(..), TyNode(..), UnifyEdge(..), fromListGen, genRef, nodeRefKey, typeRef)
import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Types.Witness (Expansion(..), InstanceOp(..))
import MLF.Constraint.Types.Witness.TestSupport (EdgeWitness(..), InstanceWitness(..))
import MLF.Constraint.Unify.Closure (SolveError(..), runUnifyClosureWithSeed)
import MLF.Frontend.Syntax (Expr(..), Lit(..))
import qualified MLF.Util.UnionFind as UF
import SpecUtil
    ( bindParentsFromPairs
    , computePresolutionRaw
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
        let canonical = UF.frWith (getPresolutionUf (prUnionFind pres))
            arrowChildren =
                [ (domain, codomain)
                | TyArrow {tnDom = domain, tnCod = codomain} <-
                    NodeAccess.allNodes (prConstraint pres)
                ]
        arrowChildren `shouldSatisfy` (not . null)
        forM_ arrowChildren $ \(domain, codomain) ->
            canonical domain `shouldBe` canonical codomain

    it "solves initial unify edges before inst-edge traversal effects are persisted" $ do
        let bodyId = NodeId 0
            targetId = NodeId 1
            expNodeId = NodeId 2
            rootId = NodeId 3
            boolId = NodeId 4
            nodes =
                nodeMapFromList
                    [ (0, TestTyBase bodyId (BaseTy "int"))
                    , (1, TestTyBase targetId (BaseTy "int"))
                    , (2, TyExp expNodeId (ExpVarId 0) bodyId)
                    , (3, TyArrow rootId expNodeId targetId)
                    , (4, TestTyBase boolId (BaseTy "bool"))
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
        computePresolutionRaw defaultTraceConfig acyc constraint `shouldSatisfy` isLeft

    it "honors seeded UF equivalence when draining closure" $ do
        let n0 = NodeId 0
            n1 = NodeId 1
            n2 = NodeId 2
            nodes =
                nodeMapFromList
                    [ (0, TyVar { tnId = n0, tnBound = Nothing })
                    , (1, TestTyBase n1 (BaseTy "Int"))
                    , (2, TestTyBase n2 (BaseTy "Bool"))
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

    it "resolves a one-sided nested expansion before rigid structural matching" $ do
        let sourceRecursiveBinder = NodeId 0
            sourceMu = NodeId 1
            sourceExp = NodeId 2
            leftOuter = NodeId 3
            targetRecursiveBinder = NodeId 4
            targetMu = NodeId 5
            rightOuter = NodeId 6
            rootGen = GenNodeId 0
            sourceGen = GenNodeId 1
            nodes =
                nodeMapFromList
                    [ (0, TyVar {tnId = sourceRecursiveBinder, tnBound = Nothing})
                    , (1, TyMu sourceMu sourceRecursiveBinder)
                    , (2, TyExp sourceExp (ExpVarId (-1)) sourceMu)
                    , (3, TyMu leftOuter sourceExp)
                    , (4, TyVar {tnId = targetRecursiveBinder, tnBound = Nothing})
                    , (5, TyMu targetMu targetRecursiveBinder)
                    , (6, TyMu rightOuter targetMu)
                    ]
            bindParents =
                IntMap.insert (nodeRefKey (genRef sourceGen)) (genRef rootGen, BindFlex) $
                    IntMap.insert (nodeRefKey (typeRef sourceRecursiveBinder)) (typeRef sourceMu, BindRigid) $
                        IntMap.insert (nodeRefKey (typeRef sourceMu)) (genRef sourceGen, BindFlex) $
                            IntMap.insert (nodeRefKey (typeRef sourceExp)) (genRef rootGen, BindFlex) $
                                IntMap.insert (nodeRefKey (typeRef leftOuter)) (genRef rootGen, BindFlex) $
                                    IntMap.insert (nodeRefKey (typeRef targetRecursiveBinder)) (typeRef targetMu, BindRigid) $
                                        IntMap.insert (nodeRefKey (typeRef rightOuter)) (genRef rootGen, BindRigid) $
                                            inferBindParents nodes
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes =
                        fromListGen
                            [ (rootGen, GenNode rootGen [leftOuter, rightOuter])
                            , (sourceGen, GenNode sourceGen [sourceMu])
                            ]
                    }
            initialState =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty 7 IntSet.empty IntMap.empty
                    IntMap.empty IntMap.empty IntMap.empty IntMap.empty
            assertOrientation label left right =
                case runPresolutionM defaultTraceConfig initialState (unifyStructureForTest left right) of
                    Left err -> expectationFailure (label ++ ": nested expansion failed: " ++ show err)
                    Right ((), finalState) ->
                        IntMap.lookup (-1) (getAssignments (psPresolution finalState))
                            `shouldBe` Just ExpIdentity

        assertOrientation "TyExp on left" leftOuter rightOuter
        assertOrientation "TyExp on right" rightOuter leftOuter

    it "still rejects an incompatible rigid structure after nested expansion" $ do
        let sourceBase = NodeId 0
            sourceExp = NodeId 1
            leftOuter = NodeId 2
            targetBinder = NodeId 3
            targetMu = NodeId 4
            rightOuter = NodeId 5
            rootGen = GenNodeId 0
            sourceGen = GenNodeId 1
            nodes =
                nodeMapFromList
                    [ (0, TestTyBase sourceBase (BaseTy "Int"))
                    , (1, TyExp sourceExp (ExpVarId (-1)) sourceBase)
                    , (2, TyMu leftOuter sourceExp)
                    , (3, TyVar {tnId = targetBinder, tnBound = Nothing})
                    , (4, TyMu targetMu targetBinder)
                    , (5, TyMu rightOuter targetMu)
                    ]
            bindParents =
                IntMap.insert (nodeRefKey (genRef sourceGen)) (genRef rootGen, BindFlex) $
                    IntMap.insert (nodeRefKey (typeRef sourceBase)) (genRef sourceGen, BindFlex) $
                        IntMap.insert (nodeRefKey (typeRef sourceExp)) (genRef rootGen, BindFlex) $
                            IntMap.insert (nodeRefKey (typeRef leftOuter)) (genRef rootGen, BindFlex) $
                                IntMap.insert (nodeRefKey (typeRef rightOuter)) (genRef rootGen, BindRigid) $
                                    inferBindParents nodes
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes =
                        fromListGen
                            [ (rootGen, GenNode rootGen [leftOuter, rightOuter])
                            , (sourceGen, GenNode sourceGen [sourceBase])
                            ]
                    }
            initialState =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty 6 IntSet.empty IntMap.empty
                    IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig initialState (unifyStructureForTest leftOuter rightOuter) of
            Left (UnmatchableTypes _ _ "rigid structural mismatch") -> pure ()
            Left err -> expectationFailure ("expected rigid structural mismatch, got " ++ show err)
            Right _ -> expectationFailure "expected incompatible expanded structure to remain rejected"

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
                    , (getNodeId intNode, TestTyBase intNode (BaseTy "Int"))
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
        pres <- requireRight (computePresolutionRaw defaultTraceConfig acyc constraint)
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
            witnessOpsByEdge =
                [ (eid, getInstanceOps (ewWitness ew))
                | (eid, ew) <- IntMap.toList (prEdgeWitnesses pres)
                ]
            isWeaken op =
                case op of
                    OpWeaken _ -> True
                    _ -> False
        witnessOpsByEdge
            `shouldSatisfy` any (any isWeaken . snd)
        forM_ weakenTargetsByEdge $ \(eid, weakenTargets) ->
            case IntMap.lookup eid (prEdgeTraces pres) of
                Nothing ->
                    expectationFailure ("Missing edge trace for witness key " ++ show eid)
                Just tr -> do
                    let interiorKeys =
                            IntSet.fromList
                                [ getNodeId nid
                                | nid <- toListInterior (getEdgeSourceInterior (etInterior tr))
                                ]
                    forM_ weakenTargets $ \nid ->
                        IntSet.member (getNodeId nid) interiorKeys `shouldBe` True
