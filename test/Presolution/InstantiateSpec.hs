module Presolution.InstantiateSpec (spec) where

import IdentityTestSupport
import Test.Hspec
import Control.Monad.State (gets)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import qualified MLF.Binding.Tree as Binding
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Presolution
import MLF.Constraint.Presolution (PresolutionError(..))
import MLF.Constraint.Presolution.TestSupport
    ( PresolutionState(..)
    , copyForallBoundProjectionAtBinderForTest
    , dropVarBindForTest
    , instantiateScheme
    , instantiateSchemeAtTargetWithBoundsForTest
    , instantiateSchemeWithTrace
    , instantiationBindersForTest
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
import Presolution.Util (expectArrowNodes, expectForall, expectForallBody)

spec :: Spec
spec = describe "instantiateScheme" $ do
    it "recomputes a warmed binder cache after eliminating a binder" $ do
        let gid = GenNodeId 0
            binder = NodeId 1
            body = NodeId 2
            nodes =
                nodeMapFromList
                    [ (getNodeId binder, TyVar binder Nothing)
                    , (getNodeId body, TyArrow body binder binder)
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents =
                        IntMap.fromList
                            [ (nodeRefKey (typeRef binder), (genRef gid, BindFlex))
                            , (nodeRefKey (typeRef body), (genRef gid, BindFlex))
                            ]
                    , cGenNodes =
                        GenNodeMap $
                            IntMap.singleton
                                (getGenNodeId gid)
                                (GenNode gid [body])
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    3
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            action = do
                warm <- instantiationBindersForTest gid body
                warmCache <- gets psBinderCache
                dropVarBindForTest binder
                invalidatedCache <- gets psBinderCache
                refreshed <- instantiationBindersForTest gid body
                pure (warm, warmCache, invalidatedCache, refreshed)

        case runPresolutionM defaultTraceConfig st0 action of
            Left err -> expectationFailure ("binder cache regression failed: " ++ show err)
            Right (((warmRoot, warmBinders), warmCache, invalidatedCache, (freshRoot, freshBinders)), _) -> do
                warmRoot `shouldBe` body
                warmBinders `shouldBe` [binder]
                IntMap.lookup (getNodeId body) warmCache `shouldBe` Just [binder]
                invalidatedCache `shouldBe` IntMap.empty
                freshRoot `shouldBe` body
                freshBinders `shouldBe` []

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
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
            Left err -> expectationFailure $ "Instantiation failed: " ++ show err
            Right (root, st1) -> do
                (d, c) <- expectArrowNodes (cNodes (psConstraint st1)) root
                d `shouldBe` fresh
                c `shouldBe` fresh
                Binding.lookupBindParent (psConstraint st1) (typeRef fresh)
                    `shouldBe` Just (typeRef root, BindFlex)

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
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
            Left err -> expectationFailure $ "Instantiation failed: " ++ show err
            Right (root, st1) -> do
                (d, c) <- expectArrowNodes (cNodes (psConstraint st1)) root
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
                    IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (instantiateSchemeWithTrace bodyArrow [(b, meta)]) of
            Left err -> expectationFailure ("instantiateSchemeWithTrace failed: " ++ show err)
            Right ((root, copyMap, _interior, frontier), st1) -> do
                (dom, cod) <- expectArrowNodes (cNodes (psConstraint st1)) root
                case lookupNodeMaybe (cNodes (psConstraint st1)) dom of
                    Just TyBottom{} -> pure ()
                    other ->
                        expectationFailure $
                            "Expected frontier copy to be ⊥, found " ++ show other
                cod `shouldBe` meta
                IntSet.member (getNodeId outerArrow) frontier `shouldBe` True
                case lookupCopy outerArrow copyMap of
                    Nothing -> expectationFailure "Expected outerArrow to be frontier-copied"
                    Just dom' -> dom' `shouldBe` dom

    it "substitutes quantified binders before frontier copying" $ do
        -- A binder may appear on the frontier while copying one of a scheme's
        -- bounds.  It still belongs to the instantiation substitution: copying
        -- it as ⊥ would later add a frontier unification back to the source
        -- scheme and make one let-use specialize all other uses.
        let binder = NodeId 1
            outerArrow = NodeId 2
            bodyArrow = NodeId 3
            meta = NodeId 10
            nodes =
                nodeMapFromList
                    [ (getNodeId binder, TyVar {tnId = binder, tnBound = Nothing})
                    , (getNodeId outerArrow, TyArrow outerArrow binder binder)
                    , (getNodeId bodyArrow, TyArrow bodyArrow binder binder)
                    , (getNodeId meta, TyVar {tnId = meta, tnBound = Nothing})
                    ]
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents =
                            bindParentsFromPairs
                                [(binder, outerArrow, BindFlex)]
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
                    IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateSchemeWithTrace bodyArrow [(binder, meta)]) of
            Left err -> expectationFailure ("instantiateSchemeWithTrace failed: " ++ show err)
            Right ((root, copyMap, _interior, frontier), st1) -> do
                expectArrowNodes (cNodes (psConstraint st1)) root
                    `shouldReturn` (meta, meta)
                IntSet.member (getNodeId binder) frontier `shouldBe` False
                lookupCopy binder copyMap `shouldBe` Just meta

    it "copies a substituted binder bound once through its canonical identity" $ do
        -- A source occurrence may still carry a pre-union alias while the
        -- binder node and its lower bound live at the canonical identity.
        -- Expansion must use one canonical copy cache: the alias and binder
        -- both select the substitution meta, while the lower bound is copied
        -- in that same traversal rather than by a second expansion.
        let binderAlias = NodeId 1
            binder = NodeId 2
            lowerBound = NodeId 3
            body = NodeId 4
            meta = NodeId 10
            nodes =
                nodeMapFromList
                    [ (getNodeId binder, TyVar binder (Just lowerBound))
                    , (getNodeId lowerBound, TyVar lowerBound Nothing)
                    , (getNodeId body, TyArrow body binderAlias binderAlias)
                    , (getNodeId meta, TyVar meta Nothing)
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents =
                        IntMap.insert
                            (nodeRefKey (typeRef body))
                            (genRef (GenNodeId 0), BindFlex)
                            ( bindParentsFromPairs
                                [ (binder, body, BindFlex)
                                , (lowerBound, binder, BindFlex)
                                ]
                            )
                    , cGenNodes =
                        GenNodeMap
                            (IntMap.singleton 0 (GenNode (GenNodeId 0) [body]))
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    (IntMap.singleton (getNodeId binderAlias) binder)
                    11
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

        case runPresolutionM
            defaultTraceConfig
            st0
            (instantiateSchemeWithTrace body [(binderAlias, meta)]) of
            Left err -> expectationFailure ("canonical substituted-bound copy failed: " ++ show err)
            Right ((root, copyMap, _interior, _frontier), st1) -> do
                lookupCopy binderAlias copyMap `shouldBe` Just meta
                lookupCopy binder copyMap `shouldBe` Just meta
                expectArrowNodes (cNodes (psConstraint st1)) root
                    `shouldReturn` (meta, meta)
                lowerCopy <-
                    maybe
                        (expectationFailure "missing lower-bound copy" >> fail "missing copy")
                        pure
                        (lookupCopy lowerBound copyMap)
                lowerCopy `shouldNotBe` lowerBound
                Binding.lookupBindParent (psConstraint st1) (typeRef meta)
                    `shouldBe` Just (typeRef root, BindFlex)
                Binding.lookupBindParent (psConstraint st1) (typeRef lowerCopy)
                    `shouldBe` Just (typeRef meta, BindFlex)

    it "rejects conflicting substitutions for one canonical binder" $ do
        let binderAlias = NodeId 1
            binder = NodeId 2
            body = NodeId 3
            firstMeta = NodeId 10
            secondMeta = NodeId 11
            nodes =
                nodeMapFromList
                    [ (getNodeId binder, TyVar binder Nothing)
                    , (getNodeId body, TyArrow body binderAlias binderAlias)
                    , (getNodeId firstMeta, TyVar firstMeta Nothing)
                    , (getNodeId secondMeta, TyVar secondMeta Nothing)
                    ]
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents =
                            bindParentsFromPairs [(binder, body, BindFlex)]
                        }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    (IntMap.singleton (getNodeId binderAlias) binder)
                    12
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

        case runPresolutionM
            defaultTraceConfig
            st0
            ( instantiateSchemeWithTrace
                body
                [(binderAlias, firstMeta), (binder, secondMeta)]
            ) of
            Left (CopySubstitutionConflict source left right) -> do
                source `shouldBe` binder
                [left, right] `shouldMatchList` [firstMeta, secondMeta]
            Left err -> expectationFailure ("expected substitution conflict, got: " ++ show err)
            Right _ -> expectationFailure "expected canonical substitution conflict"

    it "rejects an unreachable substitution instead of publishing an unused meta" $ do
        -- A binder absent from the copied body is vacuous.  It must be handled
        -- by the paper's bounded-elimination operation N, not smuggled into χe
        -- as a destination sibling and then exposed through Omega provenance.
        let sourceGen = GenNodeId 0
            destinationGen = GenNodeId 1
            binder = NodeId 1
            body = NodeId 2
            target = NodeId 3
            meta = NodeId 10
            argument = NodeId 11
            nodes =
                nodeMapFromList
                    [ (getNodeId binder, TyVar binder (Just body))
                    , (getNodeId body, TestTyBase body (BaseTy "Int"))
                    , (getNodeId target, TyVar target Nothing)
                    , (getNodeId meta, TyVar meta Nothing)
                    , (getNodeId argument, TyVar argument Nothing)
                    ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (typeRef binder), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (typeRef body), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (genRef destinationGen), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (typeRef target), (genRef destinationGen, BindFlex))
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes =
                        GenNodeMap $
                            IntMap.fromList
                                [ (getGenNodeId sourceGen, GenNode sourceGen [body])
                                , (getGenNodeId destinationGen, GenNode destinationGen [target])
                                ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    12
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            action =
                instantiateSchemeAtTargetWithBoundsForTest
                    sourceGen
                    target
                    body
                    [(binder, meta)]
                    [(binder, argument)]

        case runPresolutionM defaultTraceConfig st0 action of
            Left (InternalError msg) ->
                msg `shouldContain` "does not match reachable instantiation binders"
            Left err -> expectationFailure ("expected unreachable-binder rejection, got: " ++ show err)
            Right _ -> expectationFailure "expected an unreachable substitution to be rejected"

    it "rejects a copied-parent edit that conflicts with existing ownership" $ do
        let sourceGen = GenNodeId 0
            destinationGen = GenNodeId 1
            binder = NodeId 1
            body = NodeId 2
            target = NodeId 3
            meta = NodeId 10
            argument = NodeId 11
            nodes =
                nodeMapFromList
                    [ (getNodeId binder, TyVar binder Nothing)
                    , (getNodeId body, TyArrow body binder binder)
                    , (getNodeId target, TyVar target Nothing)
                    , (getNodeId meta, TyVar meta Nothing)
                    , (getNodeId argument, TyVar argument Nothing)
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents =
                        IntMap.fromList
                            [ (nodeRefKey (typeRef body), (genRef sourceGen, BindFlex))
                            , (nodeRefKey (typeRef binder), (typeRef body, BindFlex))
                            , (nodeRefKey (genRef destinationGen), (genRef sourceGen, BindFlex))
                            , (nodeRefKey (typeRef target), (genRef destinationGen, BindFlex))
                            , (nodeRefKey (typeRef meta), (genRef sourceGen, BindFlex))
                            ]
                    , cGenNodes =
                        GenNodeMap $
                            IntMap.fromList
                                [ (getGenNodeId sourceGen, GenNode sourceGen [body])
                                , (getGenNodeId destinationGen, GenNode destinationGen [target])
                                ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    12
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            action =
                instantiateSchemeAtTargetWithBoundsForTest
                    sourceGen
                    target
                    body
                    [(binder, meta)]
                    [(binder, argument)]

        case runPresolutionM defaultTraceConfig st0 action of
            Left (CopyBindingParentConflict child existing planned) -> do
                child `shouldBe` typeRef meta
                existing `shouldBe` genRef sourceGen
                planned `shouldNotBe` existing
            Left err -> expectationFailure ("expected copied-parent conflict, got: " ++ show err)
            Right _ -> expectationFailure "expected existing meta ownership to reject the atomic projection"

    it "validates only bounds created by this atomic projection" $ do
        -- An unrelated pre-existing bound needs a Raise, but χe neither owns
        -- nor rewrites it.  Construction must leave that graph untouched instead
        -- of running a whole-constraint repair after the copy is complete.
        let sourceGen = GenNodeId 0
            destinationGen = GenNodeId 1
            binder = NodeId 1
            body = NodeId 2
            target = NodeId 3
            meta = NodeId 10
            argument = NodeId 11
            unrelated = NodeId 20
            unrelatedLower = NodeId 21
            nodes =
                nodeMapFromList
                    [ (getNodeId binder, TyVar binder Nothing)
                    , (getNodeId body, TyArrow body binder binder)
                    , (getNodeId target, TyVar target Nothing)
                    , (getNodeId meta, TyVar meta Nothing)
                    , (getNodeId argument, TyVar argument Nothing)
                    , (getNodeId unrelated, TyVar unrelated (Just unrelatedLower))
                    , (getNodeId unrelatedLower, TyVar unrelatedLower Nothing)
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents =
                        IntMap.fromList
                            [ (nodeRefKey (typeRef body), (genRef sourceGen, BindFlex))
                            , (nodeRefKey (typeRef binder), (typeRef body, BindFlex))
                            , (nodeRefKey (genRef destinationGen), (genRef sourceGen, BindFlex))
                            , (nodeRefKey (typeRef target), (genRef destinationGen, BindFlex))
                            , (nodeRefKey (typeRef unrelated), (genRef sourceGen, BindFlex))
                            , (nodeRefKey (typeRef unrelatedLower), (genRef destinationGen, BindFlex))
                            ]
                    , cGenNodes =
                        GenNodeMap $
                            IntMap.fromList
                                [ (getGenNodeId sourceGen, GenNode sourceGen [body])
                                , (getGenNodeId destinationGen, GenNode destinationGen [target])
                                ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    22
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            action =
                instantiateSchemeAtTargetWithBoundsForTest
                    sourceGen
                    target
                    body
                    [(binder, meta)]
                    [(binder, argument)]

        case runPresolutionM defaultTraceConfig st0 action of
            Left err -> expectationFailure ("local bound validation rejected unrelated state: " ++ show err)
            Right (_, st1) ->
                Binding.lookupBindParent (psConstraint st1) (typeRef unrelatedLower)
                    `shouldBe` Just (genRef destinationGen, BindFlex)

    it "rejects an enclosing vacuous binder instead of constructing a second semantic lane" $ do
        -- The outer binder does not occur below the inner forall body.  Treating
        -- its enclosing lower bound as a second χe copy would fabricate semantic
        -- provenance for a binder outside the active instantiation domain.
        let sourceGen = GenNodeId 0
            destinationGen = GenNodeId 1
            binder = NodeId 1
            boundVar = NodeId 2
            body = NodeId 3
            lowerForall = NodeId 4
            target = NodeId 5
            meta = NodeId 10
            argument = NodeId 11
            nodes =
                nodeMapFromList
                    [ (getNodeId binder, TyVar binder (Just lowerForall))
                    , (getNodeId boundVar, TyVar boundVar Nothing)
                    , (getNodeId body, TyArrow body boundVar boundVar)
                    , (getNodeId lowerForall, TyForall lowerForall body)
                    , (getNodeId target, TyVar target Nothing)
                    , (getNodeId meta, TyVar meta Nothing)
                    , (getNodeId argument, TyVar argument Nothing)
                    ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (typeRef binder), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (typeRef lowerForall), (typeRef binder, BindFlex))
                    , (nodeRefKey (typeRef body), (typeRef lowerForall, BindFlex))
                    , (nodeRefKey (typeRef boundVar), (typeRef lowerForall, BindFlex))
                    , (nodeRefKey (genRef destinationGen), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (typeRef target), (genRef destinationGen, BindFlex))
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes =
                        GenNodeMap $
                            IntMap.fromList
                                [ (getGenNodeId sourceGen, GenNode sourceGen [binder])
                                , (getGenNodeId destinationGen, GenNode destinationGen [target])
                                ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    12
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            action =
                instantiateSchemeAtTargetWithBoundsForTest
                    sourceGen
                    target
                    body
                    [(binder, meta)]
                    [(binder, argument)]

        case runPresolutionM defaultTraceConfig st0 action of
            Left (InternalError msg) ->
                msg `shouldContain` "does not match reachable instantiation binders"
            Left err -> expectationFailure ("expected enclosing-binder rejection, got: " ++ show err)
            Right _ -> expectationFailure "expected an enclosing vacuous binder to be rejected"

    it "reuses the semantic lower-bound copy reached by the body traversal" $ do
        -- The binder and its structural lower bound are siblings below the
        -- copied body.  The body traversal already copies that lower bound, so
        -- the meta must point to exactly that copy.  Only the recipe argument
        -- receives a distinct auxiliary copy.
        let sourceGen = GenNodeId 0
            destinationGen = GenNodeId 1
            binder = NodeId 1
            base = NodeId 2
            lower = NodeId 3
            body = NodeId 4
            target = NodeId 5
            meta = NodeId 10
            argument = NodeId 11
            nodes =
                nodeMapFromList
                    [ (getNodeId binder, TyVar binder (Just lower))
                    , (getNodeId base, TestTyBase base (BaseTy "Int"))
                    , (getNodeId lower, TyArrow lower base base)
                    , (getNodeId body, TyArrow body binder lower)
                    , (getNodeId target, TyVar target Nothing)
                    , (getNodeId meta, TyVar meta Nothing)
                    , (getNodeId argument, TyVar argument Nothing)
                    ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (typeRef body), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (typeRef binder), (typeRef body, BindFlex))
                    , (nodeRefKey (typeRef lower), (typeRef body, BindFlex))
                    , (nodeRefKey (typeRef base), (typeRef lower, BindFlex))
                    , (nodeRefKey (genRef destinationGen), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (typeRef target), (genRef destinationGen, BindFlex))
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes =
                        GenNodeMap $
                            IntMap.fromList
                                [ (getGenNodeId sourceGen, GenNode sourceGen [body])
                                , (getGenNodeId destinationGen, GenNode destinationGen [target])
                                ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    12
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            action =
                instantiateSchemeAtTargetWithBoundsForTest
                    sourceGen
                    target
                    body
                    [(binder, meta)]
                    [(binder, argument)]

        case runPresolutionM defaultTraceConfig st0 action of
            Left err -> expectationFailure ("shared semantic bound construction failed: " ++ show err)
            Right (((root, bodyCopyMap, _interior, _frontier), (boundCopyMap, boundInterior, boundFrontier)), st1) -> do
                semanticBound <-
                    maybe
                        (expectationFailure "missing same-traversal semantic lower bound" >> fail "missing copy")
                        pure
                        (lookupCopy lower bodyCopyMap)
                expectArrowNodes (cNodes (psConstraint st1)) root
                    `shouldReturn` (meta, semanticBound)
                lookupNode meta (cNodes (psConstraint st1))
                    `shouldBe` Just (TyVar meta (Just semanticBound))
                Binding.lookupBindParent (psConstraint st1) (typeRef meta)
                    `shouldBe` Just (typeRef root, BindFlex)
                Binding.lookupBindParent (psConstraint st1) (typeRef semanticBound)
                    `shouldBe` Just (typeRef root, BindFlex)
                argumentBound <-
                    case lookupNode argument (cNodes (psConstraint st1)) of
                        Just TyVar {tnBound = Just copiedBound} -> pure copiedBound
                        other -> expectationFailure ("recipe argument lost its auxiliary lower bound: " ++ show other) >> fail "missing bound"
                argumentBound `shouldNotBe` semanticBound
                Binding.lookupBindParent (psConstraint st1) (typeRef argumentBound)
                    `shouldBe` Just (typeRef argument, BindFlex)
                boundCopyMap `shouldBe` mempty
                boundInterior `shouldBe` IntSet.empty
                boundFrontier `shouldBe` IntSet.empty

    it "constructs distinct owner-local bounds for the copy meta and recipe argument" $ do
        -- Paper Definition 10.1.1 keeps the copied lower-bound graph inside
        -- the copied scheme root.  The ExpInstantiate recipe argument is a
        -- separate node, so it cannot reuse that meta-owned graph: doing so
        -- would require raising the bound to their common destination.
        let sourceGen = GenNodeId 0
            destinationGen = GenNodeId 1
            binder = NodeId 1
            boundVar = NodeId 2
            boundBody = NodeId 3
            lowerForall = NodeId 4
            sourceForall = NodeId 5
            target = NodeId 6
            meta = NodeId 10
            argument = NodeId 11
            nodes =
                nodeMapFromList
                    [ (getNodeId binder, TyVar binder (Just lowerForall))
                    , (getNodeId boundVar, TyVar boundVar Nothing)
                    , (getNodeId boundBody, TyArrow boundBody boundVar boundVar)
                    , (getNodeId lowerForall, TyForall lowerForall boundBody)
                    , (getNodeId sourceForall, TyForall sourceForall binder)
                    , (getNodeId target, TyVar target Nothing)
                    , (getNodeId meta, TyVar meta Nothing)
                    , (getNodeId argument, TyVar argument Nothing)
                    ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (typeRef sourceForall), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (typeRef binder), (typeRef sourceForall, BindFlex))
                    , (nodeRefKey (typeRef lowerForall), (typeRef binder, BindFlex))
                    , (nodeRefKey (typeRef boundBody), (typeRef lowerForall, BindFlex))
                    , (nodeRefKey (typeRef boundVar), (typeRef lowerForall, BindFlex))
                    , (nodeRefKey (genRef destinationGen), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (typeRef target), (genRef destinationGen, BindFlex))
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes =
                        GenNodeMap $
                            IntMap.fromList
                                [ (getGenNodeId sourceGen, GenNode sourceGen [sourceForall])
                                , (getGenNodeId destinationGen, GenNode destinationGen [target])
                                ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    12
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            action =
                instantiateSchemeAtTargetWithBoundsForTest
                    sourceGen
                    target
                    binder
                    [(binder, meta)]
                    [(binder, argument)]

        case runPresolutionM defaultTraceConfig st0 action of
            Left err -> expectationFailure ("two-lane bound construction failed: " ++ show err)
            Right (((root, copyMap, interior, _frontier), _boundsTrace), st1) -> do
                root `shouldBe` meta
                semanticBound <-
                    maybe
                        (expectationFailure "missing semantic lower-bound copy" >> fail "missing copy")
                        pure
                        (lookupCopy lowerForall copyMap)
                argumentBound <-
                    case lookupNode argument (cNodes (psConstraint st1)) of
                        Just TyVar {tnBound = Just copiedBound} -> pure copiedBound
                        other -> expectationFailure ("recipe argument lost its lower bound: " ++ show other) >> fail "missing bound"
                semanticBound `shouldNotBe` argumentBound
                IntSet.member (getNodeId argumentBound) interior `shouldBe` False
                lookupNode meta (cNodes (psConstraint st1))
                    `shouldBe` Just (TyVar meta (Just semanticBound))
                Binding.lookupBindParent (psConstraint st1) (typeRef semanticBound)
                    `shouldBe` Just (typeRef meta, BindFlex)
                Binding.lookupBindParent (psConstraint st1) (typeRef argumentBound)
                    `shouldBe` Just (typeRef argument, BindFlex)
                Binding.lookupBindParent (psConstraint st1) (typeRef argument)
                    `shouldBe` Just (genRef destinationGen, BindFlex)

    it "copies a sibling-owned unbounded recipe argument before binding the semantic meta" $ do
        let rootGen = GenNodeId 0
            sourceGen = GenNodeId 1
            destinationGen = GenNodeId 2
            binder = NodeId 1
            body = NodeId 2
            target = NodeId 3
            argumentLeaf = NodeId 4
            argument = NodeId 5
            meta = NodeId 10
            nodes =
                nodeMapFromList
                    [ (getNodeId binder, TyVar binder Nothing)
                    , (getNodeId body, TyArrow body binder binder)
                    , (getNodeId target, TyVar target Nothing)
                    , (getNodeId argumentLeaf, TestTyBase argumentLeaf (BaseTy "Int"))
                    , (getNodeId argument, TyArrow argument argumentLeaf argumentLeaf)
                    , (getNodeId meta, TyVar meta Nothing)
                    ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (genRef sourceGen), (genRef rootGen, BindFlex))
                    , (nodeRefKey (genRef destinationGen), (genRef rootGen, BindFlex))
                    , (nodeRefKey (typeRef binder), (typeRef body, BindFlex))
                    , (nodeRefKey (typeRef body), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (typeRef target), (genRef destinationGen, BindFlex))
                    , (nodeRefKey (typeRef argumentLeaf), (genRef sourceGen, BindRigid))
                    , (nodeRefKey (typeRef argument), (genRef sourceGen, BindRigid))
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes =
                        GenNodeMap $
                            IntMap.fromList
                                [ (getGenNodeId rootGen, GenNode rootGen [])
                                , (getGenNodeId sourceGen, GenNode sourceGen [body])
                                , (getGenNodeId destinationGen, GenNode destinationGen [target])
                                ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    20
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            action =
                instantiateSchemeAtTargetWithBoundsForTest
                    sourceGen
                    target
                    body
                    [(binder, meta)]
                    [(binder, argument)]

        case runPresolutionM defaultTraceConfig st0 action of
            Left err ->
                expectationFailure $
                    "sibling-owned argument projection failed: "
                        ++ show err
            Right (((semanticRoot, _copyMap, _interior, _frontier), _boundsTrace), st1) -> do
                let copiedNodes = cNodes (psConstraint st1)
                copiedArgument <-
                    case lookupNode meta copiedNodes of
                        Just TyVar{tnBound = Just lower} -> pure lower
                        other ->
                            expectationFailure
                                ("semantic meta lost its copied argument bound: " ++ show other)
                                >> fail "missing copied argument"
                copiedArgument `shouldNotBe` argument
                (copiedLeaf, copiedLeafAgain) <-
                    expectArrowNodes copiedNodes copiedArgument
                copiedLeafAgain `shouldBe` copiedLeaf
                copiedLeaf `shouldNotBe` argumentLeaf
                Binding.lookupBindParent (psConstraint st1) (typeRef meta)
                    `shouldBe` Just (typeRef semanticRoot, BindFlex)
                Binding.lookupBindParent (psConstraint st1) (typeRef copiedArgument)
                    `shouldBe` Just (genRef destinationGen, BindFlex)
                Binding.lookupBindParent (psConstraint st1) (typeRef argument)
                    `shouldBe` Just (genRef sourceGen, BindRigid)

    it "copies source-local bound frontier when relocating a forall bound projection" $ do
        let rootGen = GenNodeId 0
            sourceGen = GenNodeId 1
            destinationGen = GenNodeId 2
            lowerLeaf = NodeId 1
            siblingBound = NodeId 2
            internalVar = NodeId 3
            boundRoot = NodeId 4
            destinationBinder = NodeId 5
            nodes =
                nodeMapFromList
                    [ (getNodeId lowerLeaf, TestTyBase lowerLeaf (BaseTy "Int"))
                    , (getNodeId siblingBound, TyArrow siblingBound lowerLeaf lowerLeaf)
                    , (getNodeId internalVar, TyVar internalVar (Just siblingBound))
                    , (getNodeId boundRoot, TyArrow boundRoot internalVar internalVar)
                    , (getNodeId destinationBinder, TyVar destinationBinder Nothing)
                    ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (genRef sourceGen), (genRef rootGen, BindFlex))
                    , (nodeRefKey (genRef destinationGen), (genRef rootGen, BindFlex))
                    , (nodeRefKey (typeRef lowerLeaf), (genRef sourceGen, BindRigid))
                    , (nodeRefKey (typeRef siblingBound), (genRef sourceGen, BindRigid))
                    , (nodeRefKey (typeRef internalVar), (typeRef boundRoot, BindFlex))
                    , (nodeRefKey (typeRef boundRoot), (genRef sourceGen, BindRigid))
                    , (nodeRefKey (typeRef destinationBinder), (genRef destinationGen, BindFlex))
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes =
                        GenNodeMap $
                            IntMap.fromList
                                [ (getGenNodeId rootGen, GenNode rootGen [])
                                , (getGenNodeId sourceGen, GenNode sourceGen [boundRoot])
                                , (getGenNodeId destinationGen, GenNode destinationGen [destinationBinder])
                                ]
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
                    IntMap.empty
            action =
                copyForallBoundProjectionAtBinderForTest
                    (typeRef destinationBinder)
                    boundRoot
                    []

        case runPresolutionM defaultTraceConfig st0 action of
            Left err ->
                expectationFailure $
                    "destination bound projection failed: "
                        ++ show err
            Right (copiedRoot, st1) -> do
                let copiedNodes = cNodes (psConstraint st1)
                (copiedInternal, copiedInternalAgain) <-
                    expectArrowNodes copiedNodes copiedRoot
                copiedInternalAgain `shouldBe` copiedInternal
                copiedRoot `shouldNotBe` boundRoot
                copiedInternal `shouldNotBe` internalVar
                copiedSibling <-
                    case lookupNode copiedInternal copiedNodes of
                        Just TyVar{tnBound = Just lower} -> pure lower
                        other ->
                            expectationFailure
                                ("copied internal variable lost its lower bound: " ++ show other)
                                >> fail "missing copied lower bound"
                copiedSibling `shouldNotBe` siblingBound
                (copiedLeaf, copiedLeafAgain) <-
                    expectArrowNodes copiedNodes copiedSibling
                copiedLeafAgain `shouldBe` copiedLeaf
                copiedLeaf `shouldNotBe` lowerLeaf
                Binding.lookupBindParent (psConstraint st1) (typeRef copiedRoot)
                    `shouldBe` Just (typeRef destinationBinder, BindFlex)
                Binding.lookupBindParent (psConstraint st1) (typeRef copiedInternal)
                    `shouldBe` Just (typeRef copiedRoot, BindFlex)
                Binding.lookupBindParent (psConstraint st1) (typeRef copiedSibling)
                    `shouldBe` Just (typeRef copiedRoot, BindRigid)
                Binding.lookupBindParent (psConstraint st1) (typeRef copiedLeaf)
                    `shouldBe` Just (typeRef copiedRoot, BindRigid)
                Binding.lookupBindParent (psConstraint st1) (typeRef siblingBound)
                    `shouldBe` Just (genRef sourceGen, BindRigid)

    it "maps binder-dependent bounds within each construction lane" $ do
        -- For b >= a the semantic copy and the recipe-argument authority are
        -- parallel substitutions.  Crossing the lanes (bArg >= aMeta) would
        -- give the argument a bound owned by the semantic copy instead of by
        -- its own instantiation tuple.
        let sourceGen = GenNodeId 0
            destinationGen = GenNodeId 1
            binderA = NodeId 1
            binderB = NodeId 2
            body = NodeId 3
            target = NodeId 4
            metaA = NodeId 10
            metaB = NodeId 11
            argumentA = NodeId 12
            argumentB = NodeId 13
            nodes =
                nodeMapFromList
                    [ (getNodeId binderA, TyVar binderA Nothing)
                    , (getNodeId binderB, TyVar binderB (Just binderA))
                    , (getNodeId body, TyArrow body binderA binderB)
                    , (getNodeId target, TyVar target Nothing)
                    , (getNodeId metaA, TyVar metaA Nothing)
                    , (getNodeId metaB, TyVar metaB Nothing)
                    , (getNodeId argumentA, TyVar argumentA Nothing)
                    , (getNodeId argumentB, TyVar argumentB Nothing)
                    ]
            bindParents =
                IntMap.fromList
                    [ (nodeRefKey (typeRef body), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (typeRef binderA), (typeRef body, BindFlex))
                    , (nodeRefKey (typeRef binderB), (typeRef body, BindFlex))
                    , (nodeRefKey (genRef destinationGen), (genRef sourceGen, BindFlex))
                    , (nodeRefKey (typeRef target), (genRef destinationGen, BindFlex))
                    ]
            constraint =
                emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes =
                        GenNodeMap $
                            IntMap.fromList
                                [ (getGenNodeId sourceGen, GenNode sourceGen [body])
                                , (getGenNodeId destinationGen, GenNode destinationGen [target])
                                ]
                    }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    14
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
            action =
                instantiateSchemeAtTargetWithBoundsForTest
                    sourceGen
                    target
                    body
                    [(binderA, metaA), (binderB, metaB)]
                    [(binderA, argumentA), (binderB, argumentB)]

        case runPresolutionM defaultTraceConfig st0 action of
            Left err -> expectationFailure ("binder-dependent construction failed: " ++ show err)
            Right (_, st1) -> do
                lookupNode metaB (cNodes (psConstraint st1))
                    `shouldBe` Just (TyVar metaB (Just metaA))
                lookupNode argumentB (cNodes (psConstraint st1))
                    `shouldBe` Just (TyVar argumentB (Just argumentA))

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
                    IntMap.empty
        case runPresolutionM defaultTraceConfig st0 (instantiateSchemeWithTrace bodyArrow []) of
            Left err -> expectationFailure ("instantiateSchemeWithTrace failed: " ++ show err)
            Right ((root, copyMap, _interior, frontier), st1) -> do
                (dom, cod) <- expectArrowNodes (cNodes (psConstraint st1)) root
                case lookupNodeMaybe (cNodes (psConstraint st1)) dom of
                    Just TyBottom{} -> pure ()
                    other ->
                        expectationFailure $
                            "Expected frontier copy to be ⊥, found " ++ show other

                case lookupCopy b copyMap of
                    Nothing -> expectationFailure "Expected b to be copied (in I(g))"
                    Just b' -> do
                        b' `shouldNotBe` b
                        cod `shouldBe` b'

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
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
            Left err -> expectationFailure $ "Instantiation failed: " ++ show err
            Right (root, st1) -> do
                (d, c) <- expectArrowNodes (cNodes (psConstraint st1)) root
                -- dom and cod of outer arrow should point to the same copied sub-node
                d `shouldBe` c
                -- inner arrow's dom/cod both use the same fresh substitution
                innerArrow <- expectArrowNodes (cNodes (psConstraint st1)) d
                innerArrow `shouldBe` (fresh, fresh)

    it "installs copied variable bounds only after copied bindings are complete" $ do
        let bounded = NodeId 1
            lower = NodeId 2
            body = NodeId 3
            nodes =
                nodeMapFromList
                    [ (getNodeId bounded, TyVar bounded (Just lower))
                    , (getNodeId lower, TyVar lower Nothing)
                    , (getNodeId body, TyArrow body bounded lower)
                    ]
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents =
                            bindParentsFromPairs
                                [ (bounded, body, BindFlex)
                                , (lower, body, BindFlex)
                                ]
                        }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    4
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateSchemeWithTrace body []) of
            Left err -> expectationFailure ("bounded copy failed: " ++ show err)
            Right ((_root, copyMap, _interior, _frontier), st1) -> do
                boundedCopy <-
                    maybe
                        (expectationFailure "missing bounded variable copy" >> fail "missing copy")
                        pure
                        (lookupCopy bounded copyMap)
                lowerCopy <-
                    maybe
                        (expectationFailure "missing lower-bound copy" >> fail "missing copy")
                        pure
                        (lookupCopy lower copyMap)
                boundedCopy `shouldNotBe` bounded
                lowerCopy `shouldNotBe` lower
                lookupNode boundedCopy (cNodes (psConstraint st1))
                    `shouldBe` Just (TyVar boundedCopy (Just lowerCopy))

    it "copies an interior base lower bound with the bounded variable" $ do
        let bounded = NodeId 1
            lower = NodeId 2
            body = NodeId 3
            lowerNode = TestTyBase lower (BaseTy "Bool")
            nodes =
                nodeMapFromList
                    [ (getNodeId bounded, TyVar bounded (Just lower))
                    , (getNodeId lower, lowerNode)
                    , (getNodeId body, TyArrow body bounded bounded)
                    ]
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents =
                            bindParentsFromPairs
                                [ (lower, bounded, BindFlex)
                                , (bounded, body, BindFlex)
                                ]
                        }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    4
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateSchemeWithTrace body []) of
            Left err -> expectationFailure ("owned base-bound copy failed: " ++ show err)
            Right ((_root, copyMap, _interior, _frontier), st1) -> do
                boundedCopy <-
                    maybe
                        (expectationFailure "missing bounded variable copy" >> fail "missing copy")
                        pure
                        (lookupCopy bounded copyMap)
                lowerCopy <-
                    maybe
                        (expectationFailure "missing owned base-bound copy" >> fail "missing copy")
                        pure
                        (lookupCopy lower copyMap)
                lowerCopy `shouldNotBe` lower
                lookupNode boundedCopy (cNodes (psConstraint st1))
                    `shouldBe` Just (TyVar boundedCopy (Just lowerCopy))
                lookupNode lowerCopy (cNodes (psConstraint st1))
                    `shouldBe` Just (TestTyBase lowerCopy (BaseTy "Bool"))

    it "rejects a cyclic source bound at the copied-bound installation seam" $ do
        let left = NodeId 1
            right = NodeId 2
            body = NodeId 3
            nodes =
                nodeMapFromList
                    [ (getNodeId left, TyVar left (Just right))
                    , (getNodeId right, TyVar right (Just left))
                    , (getNodeId body, TyArrow body left right)
                    ]
            constraint =
                rootedConstraint
                    emptyConstraint
                        { cNodes = nodes
                        , cBindParents =
                            bindParentsFromPairs
                                [ (left, body, BindFlex)
                                , (right, body, BindFlex)
                                ]
                        }
            st0 =
                PresolutionState constraint (Presolution IntMap.empty)
                    IntMap.empty
                    4
                    IntSet.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty
                    IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateSchemeWithTrace body []) of
            Left OccursCheckPresolution {} -> pure ()
            Left err -> expectationFailure ("expected copied-bound occurs-check, got: " ++ show err)
            Right _ -> expectationFailure "expected cyclic copied bound to be rejected"

    it "copies an interior base node once while preserving sharing" $ do
        -- Body uses the same interior base node twice. The copy must preserve
        -- sharing, but it cannot retain a node owned by the source body.
        let base = NodeId 2
            bound = NodeId 1
            body = NodeId 3
            fresh = NodeId 10
            nodes = nodeMapFromList
                [ (1, TyVar { tnId = bound, tnBound = Nothing })
                , (2, TestTyBase base (BaseTy "int"))
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
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
            Left err -> expectationFailure $ "Instantiation failed: " ++ show err
            Right (root, st1) -> do
                (dom, cod) <- expectArrowNodes (cNodes (psConstraint st1)) root
                dom `shouldBe` cod
                dom `shouldNotBe` base
                lookupNode dom (cNodes (psConstraint st1))
                    `shouldBe` Just (TestTyBase dom (BaseTy "int"))

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
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 12 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme topBody [(outer, freshOuter), (innerVar, freshInner)]) of
            Left err -> expectationFailure $ "Instantiation failed: " ++ show err
            Right (root, st1) -> do
                let nodes' = cNodes (psConstraint st1)
                (d, c) <- expectArrowNodes nodes' root
                innerCopy <- expectForallBody nodes' d
                innerCopy2 <- expectForallBody nodes' c
                innerCopy `shouldBe` innerCopy2
                innerArrow <- expectArrowNodes nodes' innerCopy
                innerArrow `shouldBe` (freshInner, freshOuter)

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
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

        case runPresolutionM defaultTraceConfig st0 (instantiateScheme outerBody [(bound, fresh)]) of
            Left err -> expectationFailure $ "Instantiation failed: " ++ show err
            Right (root, st1) -> do
                let nodes' = cNodes (psConstraint st1)
                (d, c) <- expectArrowNodes nodes' root

                _ <- expectForall nodes' d
                _ <- expectForall nodes' c

                d `shouldBe` c

                d `shouldNotBe` forallNode

                let forallCopy = d
                bodyArrowId <- expectForallBody nodes' forallCopy
                bArrow <- expectArrowNodes nodes' bodyArrowId
                bArrow `shouldBe` (fresh, fresh)

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
            st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty

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
                intNode = TestTyBase body (BaseTy "Int")
                boundVar = TyVar { tnId = bound, tnBound = Nothing }
                freshVar = TyVar { tnId = fresh, tnBound = Nothing }
                nodes = nodeMapFromList
                    [ (0, boundVar), (1, intNode), (10, freshVar) ]
                constraint = rootedConstraint $ emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (instantiateScheme body [(bound, fresh)]) of
                Right _ -> pure ()  -- instantiateScheme succeeds
                Left err -> expectationFailure $ "instantiateScheme failed: " ++ show err

        it "O12-COPY-INST" $ do
            -- Inst-Copy rule: instantiateSchemeWithTrace copies and records trace
            let bound = NodeId 0
                body = NodeId 1
                fresh = NodeId 10
                intNode = TestTyBase body (BaseTy "Int")
                boundVar = TyVar { tnId = bound, tnBound = Nothing }
                freshVar = TyVar { tnId = fresh, tnBound = Nothing }
                nodes = nodeMapFromList
                    [ (0, boundVar), (1, intNode), (10, freshVar) ]
                constraint = rootedConstraint $ emptyConstraint { cNodes = nodes }
                st0 = PresolutionState constraint (Presolution IntMap.empty) IntMap.empty 11 IntSet.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty
            case runPresolutionM defaultTraceConfig st0 (instantiateSchemeWithTrace body [(bound, fresh)]) of
                Right ((_, _copyMap, _interior, _frontier), _st1) -> pure ()  -- instantiateSchemeWithTrace succeeds
                Left err -> expectationFailure $ "instantiateSchemeWithTrace failed: " ++ show err
