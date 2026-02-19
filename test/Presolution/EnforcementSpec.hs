module Presolution.EnforcementSpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec
import qualified Data.IntMap.Strict as IntMap

import MLF.Constraint.Types.Graph
import MLF.Constraint.Presolution
    ( PresolutionResult(..)
    , PresolutionError(..)
    , computePresolution
    , validateTranslatablePresolution
    )
import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import SpecUtil
    ( defaultTraceConfig
    , emptyConstraint
    , bindParentsFromPairs
    , nodeMapFromList
    , rootedConstraint
    )

spec :: Spec
spec = describe "Translatable presolution enforcement" $ do
    it "O15-TRANS-SCHEME-ROOT-RIGID O15-TRANS-ARROW-RIGID O15-TRANS-NON-INTERIOR-RIGID: rigidifies scheme roots, arrow nodes, and non-interior nodes" $ do
        let rootGen = GenNodeId 0
            schemeRoot = NodeId 0
            dom = NodeId 1
            cod = NodeId 2
            arrow = NodeId 3
            outside = NodeId 4
            nodes = nodeMapFromList
                [ (getNodeId schemeRoot, TyVar { tnId = schemeRoot, tnBound = Just arrow })
                , (getNodeId dom, TyVar { tnId = dom, tnBound = Nothing })
                , (getNodeId cod, TyVar { tnId = cod, tnBound = Nothing })
                , (getNodeId arrow, TyArrow arrow dom cod)
                , (getNodeId outside, TyVar { tnId = outside, tnBound = Nothing })
                ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindFlex))
                    , (nodeRefKey (typeRef arrow), (typeRef schemeRoot, BindFlex))
                    , (nodeRefKey (typeRef dom), (typeRef arrow, BindFlex))
                    , (nodeRefKey (typeRef cod), (typeRef arrow, BindFlex))
                    , (nodeRefKey (typeRef outside), (genRef rootGen, BindFlex))
                    ]
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
                    }
            acyclicityRes = AcyclicityResult { arSortedEdges = [], arDepGraph = undefined }

        case computePresolution defaultTraceConfig acyclicityRes constraint of
            Left err -> expectationFailure ("computePresolution failed: " ++ show err)
            Right pr -> do
                let bp = cBindParents (prConstraint pr)
                IntMap.lookup (nodeRefKey (typeRef schemeRoot)) bp `shouldBe` Just (genRef rootGen, BindRigid)
                IntMap.lookup (nodeRefKey (typeRef arrow)) bp `shouldBe` Just (typeRef schemeRoot, BindRigid)
                IntMap.lookup (nodeRefKey (typeRef outside)) bp `shouldBe` Just (genRef rootGen, BindRigid)

    it "O15-TRANS-NO-INERT-LOCKED: rejects constraints with inert-locked nodes" $ do
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
            c = rootedConstraint emptyConstraint { cNodes = nodes, cBindParents = bindParents }
        case validateTranslatablePresolution c of
            Left (NonTranslatablePresolution issues) ->
                show issues `shouldSatisfy` isInfixOf "InertLockedNodes"
            other ->
                expectationFailure ("Expected inert-locked translatability rejection, got: " ++ show other)

    it "O15-TRANS-SCHEME-ROOT-RIGID: rejects flexible scheme roots under their gen node" $ do
        let rootGen = GenNodeId 0
            schemeRoot = NodeId 0
            c =
                rootedConstraint emptyConstraint
                    { cNodes = nodeMapFromList [(0, TyVar { tnId = schemeRoot, tnBound = Nothing })]
                    , cBindParents = IntMap.fromList [(nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindFlex))]
                    , cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
                    }
        case validateTranslatablePresolution c of
            Left (NonTranslatablePresolution issues) ->
                show issues `shouldSatisfy` isInfixOf "SchemeRootNotRigid"
            other ->
                expectationFailure ("Expected scheme-root rigidity rejection, got: " ++ show other)

    it "O15-TRANS-ARROW-RIGID: rejects flexibly bound arrow nodes" $ do
        let rootGen = GenNodeId 0
            dom = NodeId 0
            cod = NodeId 1
            arr = NodeId 2
            c =
                rootedConstraint emptyConstraint
                    { cNodes =
                        nodeMapFromList
                            [ (0, TyVar { tnId = dom, tnBound = Nothing })
                            , (1, TyVar { tnId = cod, tnBound = Nothing })
                            , (2, TyArrow arr dom cod)
                            ]
                    , cBindParents =
                        IntMap.fromList
                            [ (nodeRefKey (typeRef arr), (genRef rootGen, BindFlex))
                            , (nodeRefKey (typeRef dom), (typeRef arr, BindFlex))
                            , (nodeRefKey (typeRef cod), (typeRef arr, BindFlex))
                            ]
                    , cGenNodes = fromListGen [(rootGen, GenNode rootGen [arr])]
                    }
        case validateTranslatablePresolution c of
            Left (NonTranslatablePresolution issues) ->
                show issues `shouldSatisfy` isInfixOf "ArrowNodeNotRigid"
            other ->
                expectationFailure ("Expected arrow-rigidity rejection, got: " ++ show other)

    it "O15-TRANS-NON-INTERIOR-RIGID: rejects flexible non-interior children under gen nodes" $ do
        let rootGen = GenNodeId 0
            schemeRoot = NodeId 0
            dom = NodeId 1
            cod = NodeId 2
            arrow = NodeId 3
            outside = NodeId 4
            nodes = nodeMapFromList
                [ (getNodeId schemeRoot, TyVar { tnId = schemeRoot, tnBound = Just arrow })
                , (getNodeId dom, TyVar { tnId = dom, tnBound = Nothing })
                , (getNodeId cod, TyVar { tnId = cod, tnBound = Nothing })
                , (getNodeId arrow, TyArrow arrow dom cod)
                , (getNodeId outside, TyVar { tnId = outside, tnBound = Nothing })
                ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (typeRef schemeRoot), (genRef rootGen, BindRigid))
                    , (nodeRefKey (typeRef arrow), (typeRef schemeRoot, BindRigid))
                    , (nodeRefKey (typeRef dom), (typeRef arrow, BindFlex))
                    , (nodeRefKey (typeRef cod), (typeRef arrow, BindFlex))
                    , (nodeRefKey (typeRef outside), (genRef rootGen, BindFlex))
                    ]
            c =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes = fromListGen [(rootGen, GenNode rootGen [schemeRoot])]
                    }
        case validateTranslatablePresolution c of
            Left (NonTranslatablePresolution issues) ->
                show issues `shouldSatisfy` isInfixOf "NonInteriorNodeNotRigid"
            other ->
                expectationFailure ("Expected non-interior rigidity rejection, got: " ++ show other)
