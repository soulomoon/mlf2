module InertSpec (spec) where

import Test.Hspec
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import MLF.Constraint.Types.Graph
import qualified MLF.Constraint.Inert as Inert
import SpecUtil
    ( bindParentsFromPairs
    , emptyConstraint
    , nodeMapFromList
    , rootedConstraint
    )

spec :: Spec
spec = describe "Inert node classification (Ch 5)" $ do
    describe "Thesis obligations" $ do
        it "O05-INERT-NODES" $ do
            -- Def 5.2.2: inertNodes classifies nodes that do not expose polymorphism
            let root = NodeId 0
                mid = NodeId 1
                base = NodeId 2
                nodes = nodeMapFromList
                    [ (getNodeId root, TyArrow root mid mid)
                    , (getNodeId mid, TyArrow mid base base)
                    , (getNodeId base, TyBase base (BaseTy "int"))
                    ]
                bindParents =
                    bindParentsFromPairs
                        [ (mid, root, BindFlex)
                        , (base, mid, BindFlex)
                        ]
                c = rootedConstraint $ emptyConstraint { cNodes = nodes, cBindParents = bindParents }
            case Inert.inertNodes c of
                Left err -> expectationFailure ("inertNodes failed: " ++ show err)
                Right s -> s `shouldSatisfy` (not . IntSet.null)

        it "O05-INERT-LOCKED" $ do
            -- Def 15.2.2: inert-locked nodes are inert + flexibly bound + rigid ancestor
            let root = NodeId 0
                mid = NodeId 1
                n = NodeId 2
                v = NodeId 3
                base = NodeId 4
                nodes = nodeMapFromList
                    [ (getNodeId root, TyArrow root mid mid)
                    , (getNodeId mid, TyArrow mid n base)
                    , (getNodeId n, TyArrow n v base)
                    , (getNodeId v, TyVar { tnId = v, tnBound = Nothing })
                    , (getNodeId base, TyBase base (BaseTy "int"))
                    ]
                bindParents =
                    bindParentsFromPairs
                        [ (mid, root, BindRigid)
                        , (n, mid, BindFlex)
                        , (v, n, BindRigid)
                        , (base, n, BindFlex)
                        ]
                c = rootedConstraint $ emptyConstraint { cNodes = nodes, cBindParents = bindParents }
            case Inert.inertLockedNodes c of
                Left err -> expectationFailure ("inertLockedNodes failed: " ++ show err)
                Right s -> IntSet.member (getNodeId n) s `shouldBe` True

        it "O05-WEAKEN-INERT" $ do
            -- §15.2.3.2: weakenInertLockedNodes eliminates inert-locked nodes
            let root = NodeId 0
                mid = NodeId 1
                n = NodeId 2
                v = NodeId 3
                base = NodeId 4
                nodes = nodeMapFromList
                    [ (getNodeId root, TyArrow root mid mid)
                    , (getNodeId mid, TyArrow mid n base)
                    , (getNodeId n, TyArrow n v base)
                    , (getNodeId v, TyVar { tnId = v, tnBound = Nothing })
                    , (getNodeId base, TyBase base (BaseTy "int"))
                    ]
                bindParents =
                    bindParentsFromPairs
                        [ (mid, root, BindRigid)
                        , (n, mid, BindFlex)
                        , (v, n, BindRigid)
                        , (base, n, BindFlex)
                        ]
                c = rootedConstraint $ emptyConstraint { cNodes = nodes, cBindParents = bindParents }
            case Inert.weakenInertLockedNodes c of
                Left err -> expectationFailure ("weakenInertLockedNodes failed: " ++ show err)
                Right c' -> Inert.inertLockedNodes c' `shouldBe` Right IntSet.empty
