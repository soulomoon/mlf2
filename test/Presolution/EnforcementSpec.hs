{-# LANGUAGE DataKinds #-}
module Presolution.EnforcementSpec (spec) where

import IdentityTestSupport
import Data.List (isInfixOf)
import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Graph
import MLF.Constraint.Presolution
    ( PresolutionError(..)
    , prConstraint
    )
import MLF.Constraint.Presolution.TestSupport
    ( TranslatabilityIssue(..)
    , structuralInterior
    , translatableWeakenedNodes
    , validateTranslatablePresolution
    )
import MLF.Constraint.Acyclicity (AcyclicityResult(..))
import SpecUtil
    ( computePresolutionRaw
    , defaultTraceConfig
    , emptyConstraint
    , bindParentsFromPairs
    , nodeMapFromList
    , rootedConstraint
    )

spec :: Spec
spec = describe "Translatable presolution enforcement" $ do
    it "row8 translatability normalization guard: live weakened-set metadata includes all inert nodes" $ do
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
            constraint = rootedConstraint emptyConstraint { cNodes = nodes, cBindParents = bindParents }
            acyclicityRes = AcyclicityResult { arSortedEdges = [], arDepGraph = undefined }

        case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
            Left err -> expectationFailure ("computePresolution failed: " ++ show err)
            Right pr -> do
                let bp = cBindParents (prConstraint pr)
                IntMap.lookup (nodeRefKey (typeRef mid)) bp `shouldBe` Just (typeRef root, BindRigid)
                IntMap.lookup (nodeRefKey (typeRef base)) bp `shouldBe` Just (typeRef mid, BindRigid)
                translatableWeakenedNodes (prConstraint pr)
                    `shouldSatisfy` either (const False) (IntSet.member (getNodeId base))

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

        case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
            Left err -> expectationFailure ("computePresolution failed: " ++ show err)
            Right pr -> do
                let bp = cBindParents (prConstraint pr)
                IntMap.lookup (nodeRefKey (typeRef schemeRoot)) bp `shouldBe` Just (genRef rootGen, BindRigid)
                IntMap.lookup (nodeRefKey (typeRef arrow)) bp `shouldBe` Just (typeRef schemeRoot, BindRigid)
                IntMap.lookup (nodeRefKey (typeRef outside)) bp `shouldBe` Just (genRef rootGen, BindRigid)

    it "Def 9.2.16: structural interior intersects scheme reachability with the gen binding interior" $ do
        let outerGen = GenNodeId 0
            schemeGen = GenNodeId 1
            schemeRoot = NodeId 0
            owned = NodeId 1
            boundAbove = NodeId 2
            scheme = GenNode schemeGen [schemeRoot]
            nodes =
                nodeMapFromList
                    [ (getNodeId schemeRoot, TyArrow schemeRoot owned boundAbove)
                    , (getNodeId owned, TyVar { tnId = owned, tnBound = Nothing })
                    , (getNodeId boundAbove, TyVar { tnId = boundAbove, tnBound = Nothing })
                    ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (genRef schemeGen), (genRef outerGen, BindFlex))
                    , (nodeRefKey (typeRef schemeRoot), (genRef schemeGen, BindRigid))
                    , (nodeRefKey (typeRef owned), (genRef schemeGen, BindFlex))
                    , (nodeRefKey (typeRef boundAbove), (genRef outerGen, BindFlex))
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes =
                        fromListGen
                            [ (outerGen, GenNode outerGen [])
                            , (schemeGen, scheme)
                            ]
                    }
            acyclicityRes = AcyclicityResult { arSortedEdges = [], arDepGraph = undefined }

        structuralInterior constraint scheme
            `shouldBe` Right (IntSet.fromList [getNodeId schemeRoot, getNodeId owned])

        case validateTranslatablePresolution constraint of
            Left (NonTranslatablePresolution issues) ->
                issues `shouldBe` [NonInteriorNodeNotRigid outerGen boundAbove]
            other ->
                expectationFailure
                    ("Expected only the above-scope node to be non-interior, got: " ++ show other)

        case computePresolutionRaw defaultTraceConfig acyclicityRes constraint of
            Left err -> expectationFailure ("computePresolution failed: " ++ show err)
            Right pr -> do
                let bp = cBindParents (prConstraint pr)
                IntMap.lookup (nodeRefKey (typeRef owned)) bp
                    `shouldBe` Just (genRef schemeGen, BindFlex)
                IntMap.lookup (nodeRefKey (typeRef boundAbove)) bp
                    `shouldBe` Just (genRef outerGen, BindRigid)

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
                , (getNodeId base, TestTyBase base (BaseTy "int"))
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
