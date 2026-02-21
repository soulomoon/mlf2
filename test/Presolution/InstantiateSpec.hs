module Presolution.InstantiateSpec (spec) where

import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Constraint.Types.Presolution (Presolution(..))
import MLF.Constraint.Types.Graph
import MLF.Constraint.Presolution
    ( PresolutionError(..)
    , PresolutionState(..)
    , instantiateScheme
    , instantiateSchemeWithTrace
    , lookupCopy
    , runPresolutionM
    )
import SpecUtil
    ( bindParentsFromPairs
    , defaultTraceConfig
    , emptyConstraint
    , lookupNodeMaybe
    , nodeMapFromList
    , rootedConstraint
    )
import Presolution.Util (expectArrow, expectForall)

spec :: Spec
spec = describe "instantiateScheme" $ do
    it "replaces repeated bound vars with the same fresh node" $ do
        -- Scheme body (a -> a) where `a` is a bound variable to be substituted.
        let bound = NodeId 1
            body = NodeId 2
            fresh = NodeId 10
            nodes = nodeMapFromList
                [ (1, TyVar { tnId = bound, tnBound = Nothing })
                , (2, TyArrow body bound bound)
                , (10, TyVar { tnId = fresh, tnBound = Nothing }) -- fresh binder image
                ]
            constraint =
                rootedConstraint $
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParentsFromPairs
                            [ (bound, body, BindFlex) ]
                        }
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
            Left err -> expectationFailure $ "Instantiation failed: " ++ show err
            Right (root, st1) -> do
                arrow <- expectArrow (cNodes (psConstraint st1)) root
                let d = tnDom arrow
                    c = tnCod arrow
                d `shouldBe` fresh
                c `shouldBe` fresh

    it "shares outer-scope variables outside I(g)" $ do
        -- Body uses bound var and an outer var. Outer nodes are shared when they
        -- are not in the binder's interior I(g) (paper `papers/these-finale-english.txt`;
        -- see `papers/xmlf.txt` §3.2).
        let bound = NodeId 1
            outer = NodeId 3
            body = NodeId 2
            outerArrow = NodeId 4
            fresh = NodeId 10
            nodes = nodeMapFromList
                [ (1, TyVar { tnId = bound, tnBound = Nothing })
                , (2, TyArrow body bound outer)
                , (3, TyVar { tnId = outer, tnBound = Nothing })
                , (4, TyArrow outerArrow outer outer)
                , (10, TyVar { tnId = fresh, tnBound = Nothing })
                ]
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cBindParents =
                        bindParentsFromPairs
                            [ (NodeId 1, NodeId 2, BindFlex)
                            , (NodeId 3, NodeId 4, BindFlex)
                            ]
                    }
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
            Left err -> expectationFailure $ "Instantiation failed: " ++ show err
            Right (root, st1) -> do
                arrow <- expectArrow (cNodes (psConstraint st1)) root
                let d = tnDom arrow
                    c = tnCod arrow
                d `shouldBe` fresh
                c `shouldBe` outer -- shared, not copied

    it "instantiateSchemeWithTrace replaces frontier nodes with ⊥ (I(g) copy)" $ do
        -- In binding-edge mode, the paper's expansion copies nodes in I(g)
        -- and replaces frontier nodes with ⊥.
        -- Here, `outerArrow` is structurally under the ∀ body, but it is bound
        -- above the ∀ binder (so it is not in I(g)) and must be frontier-copied.
        let b = NodeId 1
            y = NodeId 2
            outerArrow = NodeId 3
            bodyArrow = NodeId 4
            forallNode = NodeId 5
            expNode = NodeId 6
            meta = NodeId 10

            nodes = nodeMapFromList
                    [ (getNodeId b, TyVar { tnId = b, tnBound = Nothing })
                    , (getNodeId y, TyVar { tnId = y, tnBound = Nothing })
                    , (getNodeId outerArrow, TyArrow outerArrow y y)
                    , (getNodeId bodyArrow, TyArrow bodyArrow outerArrow b)
                    , (getNodeId forallNode, TyForall forallNode bodyArrow)
                    , (getNodeId expNode, TyExp expNode (ExpVarId 0) forallNode)
                    , (getNodeId meta, TyVar { tnId = meta, tnBound = Nothing })
                    ]

            -- Binding edges:
            --   expNode
            --    └─ forallNode
            --        └─ bodyArrow
            --            ├─ b        (in I(g))
            --            └─ outerArrow (NOT in I(g): bound directly to expNode)
            bindParents =
                bindParentsFromPairs
                    [ (forallNode, expNode, BindFlex)
                    , (bodyArrow, forallNode, BindFlex)
                    , (b, bodyArrow, BindFlex)
                    , (outerArrow, expNode, BindFlex)
                    , (y, outerArrow, BindFlex)
                    ]

            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    11
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (instantiateSchemeWithTrace bodyArrow [(b, meta)]) of
            Left err -> expectationFailure ("instantiateSchemeWithTrace failed: " ++ show err)
            Right ((root, copyMap, _interior, frontier), st1) -> do
                arrow <- expectArrow (cNodes (psConstraint st1)) root
                let dom = tnDom arrow
                case lookupNodeMaybe (cNodes (psConstraint st1)) dom of
                    Just TyBottom{} -> pure ()
                    other ->
                        expectationFailure $
                            "Expected frontier copy to be ⊥, found " ++ show other
                tnCod arrow `shouldBe` meta
                IntSet.member (getNodeId outerArrow) frontier `shouldBe` True
                case lookupCopy outerArrow copyMap of
                    Nothing -> expectationFailure "Expected outerArrow to be frontier-copied"
                    Just dom' -> dom' `shouldBe` dom

    it "instantiateSchemeWithTrace uses I(g) even when root has no binder (no level fallback)" $ do
        -- When copying a disconnected component (e.g. an instance bound),
        -- the copied root may be a binding root. In that case, we still
        -- decide share/copy purely from binding-edge interior membership.
        --
        -- Regression: a legacy fallback would treat `y` as interior, but it
        -- is outside I(g) and should become a frontier ⊥ copy.
        let y = NodeId 1
            b = NodeId 2
            outerArrow = NodeId 3
            bodyArrow = NodeId 4

            nodes = nodeMapFromList
                    [ (getNodeId y, TyVar { tnId = y, tnBound = Nothing })
                    , (getNodeId b, TyVar { tnId = b, tnBound = Nothing })
                    , (getNodeId outerArrow, TyArrow outerArrow y y)
                    , (getNodeId bodyArrow, TyArrow bodyArrow y b)
                    ]

            -- Binding edges:
            --   b is bound to the body root (so b ∈ I(bodyArrow))
            --   y is bound to an unrelated outer arrow (so y ∉ I(bodyArrow))
            bindParents =
                bindParentsFromPairs
                    [ (b, bodyArrow, BindFlex)
                    , (y, outerArrow, BindFlex)
                    ]

            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    10
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (instantiateSchemeWithTrace bodyArrow []) of
            Left err -> expectationFailure ("instantiateSchemeWithTrace failed: " ++ show err)
            Right ((root, copyMap, _interior, frontier), st1) -> do
                arrow <- expectArrow (cNodes (psConstraint st1)) root
                let dom = tnDom arrow
                case lookupNodeMaybe (cNodes (psConstraint st1)) dom of
                    Just TyBottom{} -> pure ()
                    other ->
                        expectationFailure $
                            "Expected frontier copy to be ⊥, found " ++ show other

                case lookupCopy b copyMap of
                    Nothing -> expectationFailure "Expected b to be copied (in I(g))"
                    Just b' -> do
                        b' `shouldNotBe` b
                        tnCod arrow `shouldBe` b'

                IntSet.member (getNodeId y) frontier `shouldBe` True
                case lookupCopy y copyMap of
                    Nothing -> expectationFailure "Expected y to be frontier-copied"
                    Just dom' -> dom' `shouldBe` dom

    it "copies shared substructure only once (cache reuse)" $ do
        -- Body: (a1 -> a1) used twice as dom/cod; copy should reuse the same new node.
        let bound = NodeId 1
            shared = NodeId 5
            body = NodeId 6
            fresh = NodeId 10
            nodes = nodeMapFromList
                [ (1, TyVar { tnId = bound, tnBound = Nothing })
                , (5, TyArrow shared bound bound)    -- shared substructure
                , (6, TyArrow body shared shared)    -- uses shared twice
                , (10, TyVar { tnId = fresh, tnBound = Nothing })
                ]
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cBindParents =
                        bindParentsFromPairs
                            [ (NodeId 1, NodeId 5, BindFlex)
                            , (NodeId 5, NodeId 6, BindFlex)
                            ]
                    }
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
            Left err -> expectationFailure $ "Instantiation failed: " ++ show err
            Right (root, st1) -> do
                arrow <- expectArrow (cNodes (psConstraint st1)) root
                let d = tnDom arrow
                    c = tnCod arrow
                innerArrow <- expectArrow (cNodes (psConstraint st1)) d
                -- dom and cod of outer arrow should point to the same copied sub-node
                d `shouldBe` c
                -- inner arrow's dom/cod both use the same fresh substitution
                tnDom innerArrow `shouldBe` fresh
                tnCod innerArrow `shouldBe` fresh

    it "shares base nodes (base sharing optimization)" $ do
        -- Body uses the same base node twice; instantiate should not duplicate it.
        let base = NodeId 2
            bound = NodeId 1
            body = NodeId 3
            fresh = NodeId 10
            nodes = nodeMapFromList
                [ (1, TyVar { tnId = bound, tnBound = Nothing })
                , (2, TyBase base (BaseTy "int"))
                , (3, TyArrow body base base)
                , (10, TyVar { tnId = fresh, tnBound = Nothing })
                ]
            constraint =
                rootedConstraint $
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParentsFromPairs
                            [ (base, body, BindFlex) ]
                        }
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
            Left err -> expectationFailure $ "Instantiation failed: " ++ show err
            Right (root, st1) -> do
                arrow <- expectArrow (cNodes (psConstraint st1)) root
                tnDom arrow `shouldBe` base
                tnCod arrow `shouldBe` base

    it "copies nested forall inside the body" $ do
        -- Nested binder is copied, and substitutions apply under it.
        let outer = NodeId 1
            innerVar = NodeId 2
            innerBody = NodeId 3
            innerForall = NodeId 4
            topBody = NodeId 5
            freshOuter = NodeId 10
            freshInner = NodeId 11
            nodes = nodeMapFromList
                [ (1, TyVar { tnId = outer, tnBound = Nothing })
                , (2, TyVar { tnId = innerVar, tnBound = Nothing })
                , (3, TyArrow innerBody innerVar outer)
                , (4, TyForall innerForall innerBody)
                , (5, TyArrow topBody innerForall innerForall)
                , (10, TyVar { tnId = freshOuter, tnBound = Nothing })
                , (11, TyVar { tnId = freshInner, tnBound = Nothing })
                ]
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cBindParents =
                        bindParentsFromPairs
                            [ (NodeId 1, NodeId 3, BindFlex)
                            , (NodeId 2, NodeId 3, BindFlex)
                            , (NodeId 3, NodeId 4, BindFlex)
                            , (NodeId 4, NodeId 5, BindFlex)
                            ]
                    }
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 12 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme topBody [(outer, freshOuter), (innerVar, freshInner)]) of
            Left err -> expectationFailure $ "Instantiation failed: " ++ show err
            Right (root, st1) -> do
                let nodes' = cNodes (psConstraint st1)
                arrow <- expectArrow nodes' root
                let d = tnDom arrow
                    c = tnCod arrow
                forall1 <- expectForall nodes' d
                forall2 <- expectForall nodes' c
                let innerCopy = tnBody forall1
                    innerCopy2 = tnBody forall2
                innerArrow <- expectArrow nodes' innerCopy
                innerCopy `shouldBe` innerCopy2
                tnDom innerArrow `shouldBe` freshInner
                tnCod innerArrow `shouldBe` freshOuter

    it "copies nested expansion nodes inside the body" $ do
        -- When copying in presolution, an expansion node with identity recipe is inlined.
        let bound = NodeId 1
            forallBody = NodeId 2
            forallNode = NodeId 3
            expNode = NodeId 4
            outerBody = NodeId 5
            fresh = NodeId 10
            nodes = nodeMapFromList
                [ (1, TyVar { tnId = bound, tnBound = Nothing })
                , (2, TyArrow forallBody bound bound)
                , (3, TyForall forallNode forallBody)
                , (4, TyExp expNode (ExpVarId 9) forallNode)
                , (5, TyArrow outerBody expNode expNode)
                , (10, TyVar { tnId = fresh, tnBound = Nothing })
                ]
            constraint =
                rootedConstraint emptyConstraint
                    { cNodes = nodes
                    , cBindParents =
                        bindParentsFromPairs
                            [ (NodeId 1, NodeId 2, BindFlex)
                            , (NodeId 2, NodeId 3, BindFlex)
                            , (NodeId 3, NodeId 4, BindFlex)
                            , (NodeId 4, NodeId 5, BindFlex)
                            ]
                    }
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme outerBody [(bound, fresh)]) of
            Left err -> expectationFailure $ "Instantiation failed: " ++ show err
            Right (root, st1) -> do
                let nodes' = cNodes (psConstraint st1)
                arrow <- expectArrow nodes' root
                let d = tnDom arrow
                    c = tnCod arrow

                _ <- expectForall nodes' d
                _ <- expectForall nodes' c

                d `shouldBe` c

                d `shouldNotBe` forallNode

                let forallCopy = d
                fNode <- expectForall nodes' forallCopy

                let bodyArrowId = tnBody fNode
                bArrow <- expectArrow nodes' bodyArrowId
                tnDom bArrow `shouldBe` fresh
                tnCod bArrow `shouldBe` fresh

    it "returns error when a node is missing" $ do
        -- Substitution refers to a missing node; should throw NodeLookupFailed.
        let bound = NodeId 1
            body = NodeId 99  -- missing
            fresh = NodeId 10
            nodes = nodeMapFromList
                [ (1, TyVar { tnId = bound, tnBound = Nothing })
                , (10, TyVar { tnId = fresh, tnBound = Nothing })
                ]
            constraint = rootedConstraint $ emptyConstraint { cNodes = nodes }
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
            Left (NodeLookupFailed nid) -> nid `shouldBe` body
            Left other -> expectationFailure $ "Unexpected error: " ++ show other
            Right _ -> expectationFailure "Expected failure due to missing node"

    describe "Thesis obligations" $ do
        it "O10-COPY-SCHEME" $ do
            -- χe scheme copy: instantiateScheme copies a simple body with substitution
            let bound = NodeId 0
                body = NodeId 1
                fresh = NodeId 10
                intNode = TyBase body (BaseTy "Int")
                boundVar = TyVar { tnId = bound, tnBound = Nothing }
                freshVar = TyVar { tnId = fresh, tnBound = Nothing }
                nodes = nodeMapFromList
                    [ (0, boundVar), (1, intNode), (10, freshVar) ]
                constraint = rootedConstraint $ emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
                Right (copyRoot, _st1) -> pure ()  -- instantiateScheme succeeds
                Left err -> expectationFailure $ "instantiateScheme failed: " ++ show err

        it "O12-COPY-INST" $ do
            -- Inst-Copy rule: instantiateSchemeWithTrace copies and records trace
            let bound = NodeId 0
                body = NodeId 1
                fresh = NodeId 10
                intNode = TyBase body (BaseTy "Int")
                boundVar = TyVar { tnId = bound, tnBound = Nothing }
                freshVar = TyVar { tnId = fresh, tnBound = Nothing }
                nodes = nodeMapFromList
                    [ (0, boundVar), (1, intNode), (10, freshVar) ]
                constraint = rootedConstraint $ emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (instantiateSchemeWithTrace body [(bound, fresh)]) of
                Right ((_, _copyMap, _interior, _frontier), _st1) -> pure ()  -- instantiateSchemeWithTrace succeeds
                Left err -> expectationFailure $ "instantiateSchemeWithTrace failed: " ++ show err
