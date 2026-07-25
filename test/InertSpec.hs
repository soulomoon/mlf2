module InertSpec (spec) where

import IdentityTestSupport
import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
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
                    , (getNodeId base, TestTyBase base (BaseTy "int"))
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
                    , (getNodeId base, TestTyBase base (BaseTy "int"))
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
                    , (getNodeId base, TestTyBase base (BaseTy "int"))
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

        it "keeps a variable flexible when its lower bound exposes polymorphism" $ do
            let beta = NodeId 0
                wrapper = NodeId 1
                forallNode = NodeId 2
                arrow = NodeId 3
                binder = NodeId 4
                outerGen = GenNodeId 0
                boundGen = GenNodeId 1
                nodes = nodeMapFromList
                    [ (getNodeId beta, TyVar beta (Just wrapper))
                    , (getNodeId wrapper, TyVar wrapper (Just forallNode))
                    , (getNodeId forallNode, TyForall forallNode arrow)
                    , (getNodeId arrow, TyArrow arrow binder binder)
                    , (getNodeId binder, TyVar binder Nothing)
                    ]
                bindParents = IntMap.fromList
                    [ (nodeRefKey (typeRef beta), (genRef outerGen, BindFlex))
                    , (nodeRefKey (typeRef wrapper), (genRef boundGen, BindFlex))
                    , (nodeRefKey (typeRef forallNode), (genRef boundGen, BindFlex))
                    , (nodeRefKey (typeRef arrow), (typeRef forallNode, BindRigid))
                    , (nodeRefKey (typeRef binder), (typeRef forallNode, BindFlex))
                    ]
                genNodes = fromListGen
                    [ (outerGen, GenNode outerGen [beta])
                    , (boundGen, GenNode boundGen [wrapper, forallNode])
                    ]
                c = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cEliminatedVars = IntSet.singleton (getNodeId wrapper)
                    , cGenNodes = genNodes
                    }

            case Inert.inertNodes c of
                Left err -> expectationFailure (show err)
                Right inert ->
                    IntSet.member (getNodeId beta) inert `shouldBe` False
            case Inert.weakenInertNodes c of
                Left err -> expectationFailure (show err)
                Right weakened -> do
                    IntMap.lookup (nodeRefKey (typeRef beta)) (cBindParents weakened)
                        `shouldBe` Just (genRef outerGen, BindFlex)
                    IntMap.lookup (nodeRefKey (typeRef wrapper)) (cBindParents weakened)
                        `shouldBe` Just (genRef boundGen, BindRigid)
