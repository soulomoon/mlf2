module ScopeSpec (spec) where

import qualified Data.IntMap.Strict as IntMap
import Test.Hspec

import qualified MLF.Constraint.Presolution.View as PresolutionViewBoundary
import MLF.Constraint.Types.Graph
    ( BindFlag(..)
    , BaseTy(..)
    , BindingError(..)
    , Constraint(..)
    , GenNode(..)
    , GenNodeId(..)
    , NodeId(..)
    , TyNode(..)
    , fromListGen
    , genRef
    , getNodeId
    , nodeRefKey
    , typeRef
    )
import MLF.Elab.Run.Scope (bindingScopeRef, generalizeTargetNode, resolveCanonicalScope, schemeBodyTarget)
import qualified SolvedFacadeTestUtil as SolvedTest
import SpecUtil (emptyConstraint, nodeMapFromList)

spec :: Spec
spec = do
    describe "ga scope" $ do
        it "bindingScopeRef propagates binding tree cycle errors" $ do
            let root = NodeId 1
                cycleNode = NodeId 2
                constraint = cyclicConstraint root cycleNode
            bindingScopeRef constraint root
                `shouldSatisfy` isBindingCycleError

        it "resolveCanonicalScope propagates binding tree cycle errors" $ do
            let root = NodeId 1
                cycleNode = NodeId 2
                constraint = cyclicConstraint root cycleNode
                solved = SolvedTest.mkTestSolved constraint IntMap.empty
            resolveCanonicalScope constraint (PresolutionViewBoundary.fromSolved solved) IntMap.empty root
                `shouldSatisfy` isBindingCycleError

    describe "schemeBodyTarget" $ do
        it "keeps named non-scheme-root vars at the named node" $ do
            let genId = GenNodeId 0
                target = NodeId 1
                forallNode = NodeId 2
                body = NodeId 3
                constraint =
                    emptyConstraint
                        { cNodes = nodeMapFromList
                            [ (getNodeId target, TyVar { tnId = target, tnBound = Just forallNode })
                            , (getNodeId forallNode, TyForall { tnId = forallNode, tnBody = body })
                            , (getNodeId body, TyBase { tnId = body, tnBase = BaseTy "Int" })
                            ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef target), (genRef genId, BindFlex))
                                ]
                        , cGenNodes = fromListGen [(genId, GenNode genId [])]
                        }
            schemeBodyTarget (presolutionView constraint IntMap.empty) target
                `shouldBe` target

        it "generalizeTargetNode unwraps named non-scheme-root vars to their bound body" $ do
            let genId = GenNodeId 0
                target = NodeId 1
                forallNode = NodeId 2
                body = NodeId 3
                constraint =
                    emptyConstraint
                        { cNodes = nodeMapFromList
                            [ (getNodeId target, TyVar { tnId = target, tnBound = Just forallNode })
                            , (getNodeId forallNode, TyForall { tnId = forallNode, tnBody = body })
                            , (getNodeId body, TyBase { tnId = body, tnBase = BaseTy "Int" })
                            ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef target), (genRef genId, BindFlex))
                                ]
                        , cGenNodes = fromListGen [(genId, GenNode genId [])]
                        }
            generalizeTargetNode (presolutionView constraint IntMap.empty) target
                `shouldBe` body

        it "unwraps a scheme-root var to its bound target" $ do
            let genId = GenNodeId 0
                root = NodeId 10
                bound = NodeId 11
                constraint =
                    emptyConstraint
                        { cNodes = nodeMapFromList
                            [ (getNodeId root, TyVar { tnId = root, tnBound = Just bound })
                            , (getNodeId bound, TyVar { tnId = bound, tnBound = Nothing })
                            ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef root), (genRef genId, BindFlex))
                                ]
                        , cGenNodes = fromListGen [(genId, GenNode genId [root])]
                        }
            schemeBodyTarget (presolutionView constraint IntMap.empty) root
                `shouldBe` bound

        it "unwraps a scheme-root var bound to forall to the forall body" $ do
            let genId = GenNodeId 0
                root = NodeId 20
                forallNode = NodeId 21
                body = NodeId 22
                constraint =
                    emptyConstraint
                        { cNodes = nodeMapFromList
                            [ (getNodeId root, TyVar { tnId = root, tnBound = Just forallNode })
                            , (getNodeId forallNode, TyForall { tnId = forallNode, tnBody = body })
                            , (getNodeId body, TyBase { tnId = body, tnBase = BaseTy "Bool" })
                            ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef root), (genRef genId, BindFlex))
                                ]
                        , cGenNodes = fromListGen [(genId, GenNode genId [root])]
                        }
            schemeBodyTarget (presolutionView constraint IntMap.empty) root
                `shouldBe` body

        it "unwraps a canonical scheme-body alias to the shared bound body" $ do
            let genId = GenNodeId 0
                root = NodeId 30
                forallNode = NodeId 31
                body = NodeId 32
                aliasBound = NodeId 33
                aliasNode = NodeId 34
                constraint =
                    emptyConstraint
                        { cNodes = nodeMapFromList
                            [ (getNodeId root, TyVar { tnId = root, tnBound = Just forallNode })
                            , (getNodeId forallNode, TyForall { tnId = forallNode, tnBody = body })
                            , (getNodeId body, TyBase { tnId = body, tnBase = BaseTy "String" })
                            , (getNodeId aliasBound, TyVar { tnId = aliasBound, tnBound = Nothing })
                            , (getNodeId aliasNode, TyVar { tnId = aliasNode, tnBound = Just aliasBound })
                            ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef root), (genRef genId, BindFlex))
                                , (nodeRefKey (typeRef aliasNode), (genRef genId, BindFlex))
                                ]
                        , cGenNodes = fromListGen [(genId, GenNode genId [root])]
                        }
                uf = IntMap.singleton (getNodeId aliasBound) forallNode
            schemeBodyTarget (presolutionView constraint uf) aliasNode
                `shouldBe` body

isBindingCycleError :: Either BindingError a -> Bool
isBindingCycleError result = case result of
    Left (BindingCycleDetected _) -> True
    _ -> False

presolutionView :: Constraint -> IntMap.IntMap NodeId -> PresolutionViewBoundary.PresolutionView
presolutionView constraint uf =
    PresolutionViewBoundary.fromSolved (SolvedTest.mkTestSolved constraint uf)

cyclicConstraint :: NodeId -> NodeId -> Constraint
cyclicConstraint n1 n2 =
    let nodes = nodeMapFromList
            [ (getNodeId n1, TyVar { tnId = n1, tnBound = Nothing })
            , (getNodeId n2, TyVar { tnId = n2, tnBound = Nothing })
            ]
        bindParents = IntMap.fromList
            [ (nodeRefKey (typeRef n1), (typeRef n2, BindFlex))
            , (nodeRefKey (typeRef n2), (typeRef n1, BindFlex))
            ]
    in emptyConstraint
        { cNodes = nodes
        , cBindParents = bindParents
        }
