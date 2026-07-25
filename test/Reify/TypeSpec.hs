{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Reify.TypeSpec (spec) where

import IdentityTestSupport (pattern TestTyBase)
import Data.IntSet qualified as IntSet
import Data.IntMap.Strict qualified as IntMap
import Data.List (isPrefixOf)
import Data.Set qualified as Set
import MLF.Constraint.Finalize (presolutionViewFromSnapshot)
import MLF.Constraint.Finalize.TestSupport qualified as Finalize
import MLF.Constraint.Presolution.View (PresolutionView (..))
import MLF.Constraint.Solved qualified as Solved
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Constraint.Types.Graph
  ( BaseTy (..),
    BindFlag (..),
    GenNode (..),
    GenNodeId (..),
    NodeId (..),
    TyNode (..),
    cBindParents,
    cGenNodes,
    cNodes,
    cWeakenedVars,
    fromListGen,
    genRef,
    getNodeId,
    nodeRefKey,
    typeRef,
  )
import MLF.Frontend.Syntax (Expr (..), Lit (..), SurfaceExpr)
import MLF.Reify.Type
  ( ReifyRoot (..),
    freeVars,
    reifyType,
    reifyTypeWithExternalRefsNoFallbackOnConstraint,
    reifyTypeWithOuterBinderRefsNoFallbackOnConstraint,
    reifyTypeWithNamedSetRefs,
    reifyWithRefs,
    reifyWithAsRefs,
  )
import MLF.Reify.TypeOps
  ( alphaEqType,
    alphaEqTypePreservingStructuralBinders,
  )
import MLF.Types.Elab
  ( ElabType
  , Ty (..)
  , typeBinderIdentityFromNode
  , typeBinderIdentityFromUnique
  , typeBinderIdentityKey
  , typeBinderRefFromIdentity
  , typeBinderRefIdentity
  , typeBinderRefName
  , typeBinderRefsSameIdentity
  )
import MLF.Types.Identity
  ( StructuralTypeBinderRole (StructuralResultBinder, StructuralSelfBinder)
  , typeBinderIdentityFromStructural
  )
import MLF.Types.Unique (UniqueIdentity (..))
import MLF.Util.ElabError (ElabError (..))
import SpecUtil
  ( PipelineArtifacts (..),
    emptyConstraint,
    expectRight,
    nodeMapFromList,
    requireRight,
    runPipelineArtifactsDefault,
  )
import Test.Hspec

-- | Helper: run the full pipeline on a surface expression and return artifacts.
pipelineFor :: SurfaceExpr -> IO PipelineArtifacts
pipelineFor expr = requireRight (runPipelineArtifactsDefault Set.empty expr)

-- | Helper: build PresolutionView 'Raw from a Solved value.
viewFor :: Solved.Solved -> PresolutionView 'Raw
viewFor = Finalize.presolutionViewFromSolved

spec :: Spec
spec = describe "MLF.Reify.Type" $ do
  describe "identity-preserving alpha equivalence" $ do
    it "does not alpha-rename a graph mu binder into a structural owner" $ do
      let graphRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode (NodeId 991901))
              "graph-self"
          structuralRef =
            typeBinderRefFromIdentity
              ( typeBinderIdentityFromStructural
                  (UniqueIdentity 991902)
                  StructuralSelfBinder
              )
              "unit-self"
          graphTy = TMuRef graphRef (TVarRef graphRef)
          structuralTy = TMuRef structuralRef (TVarRef structuralRef)
      alphaEqType graphTy structuralTy `shouldBe` True
      alphaEqTypePreservingStructuralBinders graphTy structuralTy
        `shouldBe` False

    it "retains alpha-renaming for the same structural owner and role" $ do
      let structuralIdentity =
            typeBinderIdentityFromStructural
              (UniqueIdentity 991903)
              StructuralResultBinder
          leftRef =
            typeBinderRefFromIdentity structuralIdentity "left-result"
          rightRef =
            typeBinderRefFromIdentity structuralIdentity "right-result"
          leftTy = TForallRef leftRef Nothing (TVarRef leftRef)
          rightTy = TForallRef rightRef Nothing (TVarRef rightRef)
      alphaEqTypePreservingStructuralBinders leftTy rightTy
        `shouldBe` True

  describe "reifyType" $ do
    it "selects the literal result node, no-fallback type, or bound by root mode" $ do
      artifacts <- pipelineFor (ELit (LInt 42))
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
          refFor node@(NodeId key) =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode node)
              ("t" ++ show key)
      IntSet.member
        (getNodeId (pvCanonical view root))
        (cWeakenedVars (pvCanonicalConstraint view))
        `shouldBe` True
      expectRight (reifyType view root) $ \ty ->
        ty `shouldSatisfy` isVarType
      expectRight
        (reifyWithRefs "literal no-fallback type" view refFor (const False) RootTypeNoFallback root)
        (\ty -> ty `shouldSatisfy` isBaseType)
      expectRight
        (reifyWithRefs "literal bound" view refFor (const False) RootBound root)
        (\ty -> ty `shouldSatisfy` isBaseType)

    it "reifies identity function \\x.x to its representative type variable" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
      expectRight (reifyType view root) $ \ty ->
        ty `shouldSatisfy` isVarType

    it "attaches graph identity when reifying named variables" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
          rootC = pvCanonical view root
          key = getNodeId rootC
          alphaRef = typeBinderRefFromIdentity (typeBinderIdentityFromNode rootC) "alpha"
      expectRight (reifyTypeWithNamedSetRefs view (IntMap.singleton key alphaRef) (IntSet.singleton key) root) $ \ty ->
        case ty of
          TVarRef ref -> do
            typeBinderRefName ref `shouldBe` "alpha"
            typeBinderIdentityKey (typeBinderRefIdentity ref) `shouldBe` key
          other ->
            expectationFailure ("Expected identity-bearing TVarRef, got " ++ show other)

    it "emits an inherited source-named Bottom as a free variable" $ do
      let scopeGen = GenNodeId 20
          root = NodeId 200
          inherited = NodeId 201
          boolNode = NodeId 202
          sourceRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992004))
              "alpha"
          constraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId root, TyArrow root inherited boolNode),
                      (getNodeId inherited, TyBottom inherited),
                      (getNodeId boolNode, TestTyBase boolNode (BaseTy "Bool"))
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef root), (genRef scopeGen, BindFlex)),
                      (nodeRefKey (typeRef inherited), (typeRef root, BindRigid)),
                      (nodeRefKey (typeRef boolNode), (typeRef root, BindRigid))
                    ],
                cGenNodes = fromListGen [(scopeGen, GenNode scopeGen [root])]
              }
          sourceRefs = IntMap.singleton (getNodeId inherited) sourceRef
          externalKeys = IntSet.singleton (getNodeId inherited)
      expectRight
        (reifyTypeWithExternalRefsNoFallbackOnConstraint constraint sourceRefs externalKeys IntMap.empty root)
        $ \ty ->
          case ty of
            TArrow (TVarRef actualRef) (TBaseWithIdentity _ (BaseTy "Bool")) ->
              typeBinderRefsSameIdentity actualRef sourceRef `shouldBe` True
            other ->
              expectationFailure
                ("Expected inherited alpha -> Bool without a local forall, got " ++ show other)

    it "still quantifies a packet-local named variable" $ do
      let scopeGen = GenNodeId 21
          root = NodeId 210
          local = NodeId 211
          boolNode = NodeId 212
          localRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992005))
              "local"
          constraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId root, TyArrow root local boolNode),
                      (getNodeId local, TyVar local Nothing),
                      (getNodeId boolNode, TestTyBase boolNode (BaseTy "Bool"))
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef root), (genRef scopeGen, BindFlex)),
                      (nodeRefKey (typeRef local), (typeRef root, BindFlex)),
                      (nodeRefKey (typeRef boolNode), (typeRef root, BindRigid))
                    ],
                cGenNodes = fromListGen [(scopeGen, GenNode scopeGen [root])]
              }
          sourceRefs = IntMap.singleton (getNodeId local) localRef
      expectRight
        (reifyTypeWithExternalRefsNoFallbackOnConstraint constraint sourceRefs IntSet.empty IntMap.empty root)
        $ \ty ->
          case ty of
            TForallRef binderRef Nothing
              (TArrow (TVarRef actualRef) (TBaseWithIdentity _ (BaseTy "Bool"))) -> do
                typeBinderRefsSameIdentity binderRef localRef `shouldBe` True
                typeBinderRefsSameIdentity actualRef localRef `shouldBe` True
            other ->
              expectationFailure
                ("Expected forall local. local -> Bool, got " ++ show other)

    it "orders a binder before a sibling whose nested bound mentions it" $ do
      let scopeGen = GenNodeId 992010
          root = NodeId 992011
          dependent = NodeId 992012
          dependency = NodeId 992013
          boundRoot = NodeId 992014
          nestedArrow = NodeId 992015
          intNode = NodeId 992016
          dependentRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode dependent)
              "dependent"
          dependencyRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode dependency)
              "dependency"
          constraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId root, TyArrow root dependent dependency),
                      (getNodeId dependent, TyVar dependent (Just boundRoot)),
                      (getNodeId dependency, TyVar dependency Nothing),
                      (getNodeId boundRoot, TyArrow boundRoot nestedArrow intNode),
                      (getNodeId nestedArrow, TyArrow nestedArrow dependency intNode),
                      (getNodeId intNode, TestTyBase intNode (BaseTy "Int"))
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef root), (genRef scopeGen, BindFlex)),
                      (nodeRefKey (typeRef dependent), (typeRef root, BindFlex)),
                      (nodeRefKey (typeRef dependency), (typeRef root, BindFlex)),
                      (nodeRefKey (typeRef boundRoot), (typeRef dependent, BindRigid)),
                      (nodeRefKey (typeRef nestedArrow), (typeRef boundRoot, BindFlex)),
                      (nodeRefKey (typeRef intNode), (typeRef boundRoot, BindRigid))
                    ],
                cGenNodes = fromListGen [(scopeGen, GenNode scopeGen [root])]
              }
          subst =
            IntMap.fromList
              [ (getNodeId dependent, dependentRef),
                (getNodeId dependency, dependencyRef)
              ]
      expectRight
        ( reifyTypeWithExternalRefsNoFallbackOnConstraint
            constraint
            subst
            IntSet.empty
            IntMap.empty
            root
        )
        $ \ty ->
          case ty of
            TForallRef firstRef Nothing
              (TForallRef secondRef (Just _) _) -> do
                typeBinderRefsSameIdentity firstRef dependencyRef
                  `shouldBe` True
                typeBinderRefsSameIdentity secondRef dependentRef
                  `shouldBe` True
            other ->
              expectationFailure
                ( "Expected dependency before the sibling whose nested bound uses it, got "
                    ++ show other
                )

    it "does not reconstruct a structural forall owned by the enclosing scheme" $ do
      let scopeGen = GenNodeId 22
          forallNode = NodeId 220
          binder = NodeId 221
          body = NodeId 222
          outerRef =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromUnique (UniqueIdentity 992006))
              "outer"
          constraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId forallNode, TyForall forallNode body),
                      (getNodeId binder, TyVar binder Nothing),
                      (getNodeId body, TyArrow body binder binder)
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef forallNode), (genRef scopeGen, BindFlex)),
                      (nodeRefKey (typeRef binder), (typeRef forallNode, BindRigid)),
                      (nodeRefKey (typeRef body), (typeRef forallNode, BindRigid))
                    ],
                cGenNodes = fromListGen [(scopeGen, GenNode scopeGen [forallNode])]
              }
          subst = IntMap.singleton (getNodeId binder) outerRef
          structuralBinders = IntMap.singleton (getNodeId forallNode) [binder]
      expectRight
        ( reifyTypeWithOuterBinderRefsNoFallbackOnConstraint
            constraint
            subst
            IntSet.empty
            []
            structuralBinders
            forallNode
        )
        $ \ty ->
          case ty of
            TForallRef binderRef Nothing (TArrow (TVarRef domRef) (TVarRef codRef)) -> do
              typeBinderRefsSameIdentity binderRef outerRef `shouldBe` True
              typeBinderRefsSameIdentity domRef outerRef `shouldBe` True
              typeBinderRefsSameIdentity codRef outerRef `shouldBe` True
            other ->
              expectationFailure
                ("Expected the structural owner to construct forall outer, got " ++ show other)
      expectRight
        ( reifyTypeWithOuterBinderRefsNoFallbackOnConstraint
            constraint
            subst
            IntSet.empty
            [outerRef]
            structuralBinders
            forallNode
        )
        $ \ty ->
          case ty of
            TArrow (TVarRef domRef) (TVarRef codRef) -> do
              typeBinderRefsSameIdentity domRef outerRef `shouldBe` True
              typeBinderRefsSameIdentity codRef outerRef `shouldBe` True
            other ->
              expectationFailure
                ("Expected outer -> outer under the enclosing declaration, got " ++ show other)

    it "keeps the generalized result variable while its bound reifies to Int" $ do
      let expr =
            ELet
              "id"
              (ELam "x" (EVar "x"))
              (EApp (EVar "id") (ELit (LInt 1)))
      artifacts <- pipelineFor expr
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
          refFor node@(NodeId key) =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode node)
              ("t" ++ show key)
      -- RootType preserves a generalized flexible variable.  Asking for the
      -- lower-bound view is the explicit operation that recovers the concrete
      -- application result; conflating the two made this test assert the
      -- opposite of the reifier's type-vs-bound contract.
      expectRight (reifyType view root) $ \ty ->
        ty `shouldSatisfy` isVarType
      expectRight
        (reifyWithRefs "let-polymorphism bound" view refFor (const False) RootBound root)
        (\ty -> ty `shouldSatisfy` isBaseType)

    it "keeps an application result variable while its bound reifies to Int" $ do
      let expr = EApp (ELam "x" (EVar "x")) (ELit (LInt 1))
      artifacts <- pipelineFor expr
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
          refFor node@(NodeId key) =
            typeBinderRefFromIdentity
              (typeBinderIdentityFromNode node)
              ("t" ++ show key)
      expectRight (reifyType view root) $ \ty ->
        ty `shouldSatisfy` isVarType
      expectRight
        (reifyWithRefs "application bound" view refFor (const False) RootBound root)
        (\ty -> ty `shouldSatisfy` isBaseType)

    it "does not treat a structural forall body as the recursive binder of TyMu" $ do
      let muNode = NodeId 0
          forallBody = NodeId 1
          arrowBody = NodeId 2
          recursiveBinder = NodeId 3
          genId = GenNodeId 0
          constraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId muNode, TyMu muNode forallBody),
                      (getNodeId forallBody, TyForall forallBody arrowBody),
                      (getNodeId arrowBody, TyArrow arrowBody recursiveBinder recursiveBinder),
                      (getNodeId recursiveBinder, TyVar recursiveBinder Nothing)
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef muNode), (genRef genId, BindFlex)),
                      (nodeRefKey (typeRef forallBody), (typeRef muNode, BindFlex)),
                      (nodeRefKey (typeRef arrowBody), (typeRef forallBody, BindRigid)),
                      (nodeRefKey (typeRef recursiveBinder), (typeRef muNode, BindFlex))
                    ],
                cGenNodes = fromListGen [(genId, GenNode genId [])]
              }
          view = presolutionViewFromSnapshot constraint IntMap.empty
      expectRight (reifyType view muNode) $ \ty ->
        case ty of
          TMuRef ref (TArrow (TVarRef domRef) (TVarRef codRef)) -> do
            typeBinderIdentityKey (typeBinderRefIdentity ref)
              `shouldBe` getNodeId recursiveBinder
            typeBinderIdentityKey (typeBinderRefIdentity domRef)
              `shouldBe` getNodeId recursiveBinder
            typeBinderIdentityKey (typeBinderRefIdentity codRef)
              `shouldBe` getNodeId recursiveBinder
          other ->
            expectationFailure ("Expected mu type with the TyVar binder, got " ++ show other)

    it "uses a directly rigid TyMu child as its recursive binder" $ do
      let muNode = NodeId 0
          forallBody = NodeId 1
          arrowBody = NodeId 2
          recursiveBinder = NodeId 3
          genId = GenNodeId 0
          constraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId muNode, TyMu muNode forallBody),
                      (getNodeId forallBody, TyForall forallBody arrowBody),
                      (getNodeId arrowBody, TyArrow arrowBody recursiveBinder recursiveBinder),
                      (getNodeId recursiveBinder, TyVar recursiveBinder Nothing)
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef muNode), (genRef genId, BindFlex)),
                      (nodeRefKey (typeRef forallBody), (typeRef muNode, BindFlex)),
                      (nodeRefKey (typeRef arrowBody), (typeRef forallBody, BindRigid)),
                      (nodeRefKey (typeRef recursiveBinder), (typeRef muNode, BindRigid))
                    ],
                cGenNodes = fromListGen [(genId, GenNode genId [])]
              }
          view = presolutionViewFromSnapshot constraint IntMap.empty
      expectRight (reifyType view muNode) $ \ty ->
        case ty of
          TMuRef ref (TArrow (TVarRef domRef) (TVarRef codRef)) -> do
            typeBinderIdentityKey (typeBinderRefIdentity ref)
              `shouldBe` getNodeId recursiveBinder
            typeBinderIdentityKey (typeBinderRefIdentity domRef)
              `shouldBe` getNodeId recursiveBinder
            typeBinderIdentityKey (typeBinderRefIdentity codRef)
              `shouldBe` getNodeId recursiveBinder
          other ->
            expectationFailure ("Expected mu type with the rigid TyVar binder, got " ++ show other)

    it "uses the lower-bound identity when a direct TyMu child is an occurrence proxy" $ do
      let muNode = NodeId 0
          arrowBody = NodeId 1
          occurrenceProxy = NodeId 2
          recursiveBinder = NodeId 3
          genId = GenNodeId 0
          constraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId muNode, TyMu muNode arrowBody),
                      (getNodeId arrowBody, TyArrow arrowBody occurrenceProxy occurrenceProxy),
                      (getNodeId occurrenceProxy, TyVar occurrenceProxy (Just recursiveBinder)),
                      (getNodeId recursiveBinder, TyVar recursiveBinder Nothing)
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef muNode), (genRef genId, BindFlex)),
                      (nodeRefKey (typeRef arrowBody), (typeRef muNode, BindRigid)),
                      (nodeRefKey (typeRef occurrenceProxy), (typeRef muNode, BindRigid)),
                      (nodeRefKey (typeRef recursiveBinder), (typeRef muNode, BindFlex))
                    ],
                cGenNodes = fromListGen [(genId, GenNode genId [])]
              }
          view = presolutionViewFromSnapshot constraint IntMap.empty
      expectRight (reifyType view muNode) $ \ty ->
        case ty of
          TMuRef ref (TArrow (TVarRef domRef) (TVarRef codRef)) -> do
            typeBinderIdentityKey (typeBinderRefIdentity ref)
              `shouldBe` getNodeId recursiveBinder
            typeBinderRefsSameIdentity domRef ref `shouldBe` True
            typeBinderRefsSameIdentity codRef ref `shouldBe` True
          other ->
            expectationFailure
              ("Expected the bounded occurrence to use the mu lower-bound identity, got " ++ show other)

    it "does not treat rigid scheme siblings as recursive TyMu binders" $ do
      let muNode = NodeId 0
          forallBody = NodeId 1
          arrowBody = NodeId 2
          recursiveBinder = NodeId 3
          firstSchemeSibling = NodeId 4
          secondSchemeSibling = NodeId 5
          genId = GenNodeId 0
          constraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId muNode, TyMu muNode forallBody),
                      (getNodeId forallBody, TyForall forallBody arrowBody),
                      (getNodeId arrowBody, TyArrow arrowBody recursiveBinder recursiveBinder),
                      (getNodeId recursiveBinder, TyVar recursiveBinder Nothing),
                      (getNodeId firstSchemeSibling, TyVar firstSchemeSibling Nothing),
                      (getNodeId secondSchemeSibling, TyVar secondSchemeSibling Nothing)
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef muNode), (genRef genId, BindFlex)),
                      (nodeRefKey (typeRef forallBody), (typeRef muNode, BindFlex)),
                      (nodeRefKey (typeRef arrowBody), (typeRef forallBody, BindRigid)),
                      (nodeRefKey (typeRef recursiveBinder), (typeRef muNode, BindRigid)),
                      (nodeRefKey (typeRef firstSchemeSibling), (genRef genId, BindRigid)),
                      (nodeRefKey (typeRef secondSchemeSibling), (genRef genId, BindRigid))
                    ],
                cGenNodes = fromListGen [(genId, GenNode genId [muNode])]
              }
          view = presolutionViewFromSnapshot constraint IntMap.empty
      expectRight (reifyType view muNode) $ \ty ->
        case ty of
          TMuRef ref (TArrow (TVarRef domRef) (TVarRef codRef)) -> do
            typeBinderIdentityKey (typeBinderRefIdentity ref)
              `shouldBe` getNodeId recursiveBinder
            typeBinderIdentityKey (typeBinderRefIdentity domRef)
              `shouldBe` getNodeId recursiveBinder
            typeBinderIdentityKey (typeBinderRefIdentity codRef)
              `shouldBe` getNodeId recursiveBinder
          other ->
            expectationFailure
              ("Expected mu type with only its direct recursive binder, got " ++ show other)

    it "constructs one TyMu binder from graph aliases of one structural identity" $ do
      let muNode = NodeId 10
          bodyNode = NodeId 11
          firstAlias = NodeId 12
          secondAlias = NodeId 13
          genId = GenNodeId 1
          selfRef =
            typeBinderRefFromIdentity
              ( typeBinderIdentityFromStructural
                  (UniqueIdentity 992006)
                  StructuralSelfBinder
              )
              "self"
          constraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId muNode, TyMu muNode bodyNode),
                      (getNodeId bodyNode, TestTyBase bodyNode (BaseTy "Bool")),
                      (getNodeId firstAlias, TyVar firstAlias Nothing),
                      (getNodeId secondAlias, TyVar secondAlias Nothing)
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef muNode), (genRef genId, BindFlex)),
                      (nodeRefKey (typeRef bodyNode), (typeRef muNode, BindRigid)),
                      (nodeRefKey (typeRef firstAlias), (genRef genId, BindRigid)),
                      (nodeRefKey (typeRef secondAlias), (genRef genId, BindRigid))
                    ],
                cGenNodes = fromListGen [(genId, GenNode genId [muNode])]
              }
          sourceRefs =
            IntMap.fromList
              [ (getNodeId firstAlias, selfRef),
                (getNodeId secondAlias, selfRef)
              ]
          structuralBinders =
            IntMap.singleton
              (getNodeId muNode)
              [firstAlias, secondAlias]
      expectRight
        ( reifyTypeWithExternalRefsNoFallbackOnConstraint
            constraint
            sourceRefs
            IntSet.empty
            structuralBinders
            muNode
        )
        $ \ty ->
          case ty of
            TMuRef binderRef (TBaseWithIdentity _ (BaseTy "Bool")) ->
              typeBinderRefsSameIdentity binderRef selfRef `shouldBe` True
            other ->
              expectationFailure
                ("Expected one identity-collapsed mu binder, got " ++ show other)

  describe "Snapshot Finalization read model construction" $ do
    it "preserves the solved original constraint for identity" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          view = viewFor solved
      pvConstraint view `shouldBe` Solved.originalConstraint solved

    it "preserves the solved canonical constraint for identity" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          view = viewFor solved
      pvCanonicalConstraint view `shouldBe` Solved.canonicalConstraint solved

    it "preserves the solved original constraint for literal expression" $ do
      artifacts <- pipelineFor (ELit (LInt 0))
      let solved = paSolved artifacts
          view = viewFor solved
      pvConstraint view `shouldBe` Solved.originalConstraint solved

  describe "freeVars" $ do
    it "returns empty for a literal node" $ do
      artifacts <- pipelineFor (ELit (LInt 99))
      let solved = paSolved artifacts
          root = paRoot artifacts
          fvs = freeVars solved root IntSet.empty
      -- Literal nodes have no free vars
      fvs `shouldSatisfy` IntSet.null

    it "returns non-empty for identity function root" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          root = paRoot artifacts
          fvs = freeVars solved root IntSet.empty
      -- Identity has a binder node reachable from the reified root.
      fvs `shouldSatisfy` (not . IntSet.null)
    it "respects visited set by skipping already-seen nodes" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          root = paRoot artifacts
          -- Pre-populate visited with the root itself
          visited = IntSet.singleton (getNodeId (Solved.canonical solved root))
          fvs = freeVars solved root visited
      -- With root already visited, should return empty
      fvs `shouldBe` IntSet.empty

  describe "reifyWith" $ do
    it "reifies identity with custom var names" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
          refFor node@(NodeId i) =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode node) ("v" ++ show i)
          isNamed _ = False
      expectRight (reifyWithRefs "test" view refFor isNamed RootType root) $ \ty ->
        ty `shouldSatisfy` isPrefixedVarType "v"

    it "reifies identity with custom var refs" $ do
      artifacts <- pipelineFor (ELam "x" (EVar "x"))
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
          refFor node@(NodeId i) =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode node) ("v" ++ show i)
          isNamed _ = False
      expectRight (reifyWithRefs "test" view refFor isNamed RootType root) $ \ty ->
        case ty of
          TVarRef ref ->
            typeBinderRefName ref `shouldSatisfy` isPrefixOf "v"
          other ->
            expectationFailure ("Expected identity-bearing TVarRef, got " ++ show other)
    it "reifies literal with RootType as its bounded result variable" $ do
      artifacts <- pipelineFor (ELit (LInt 7))
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
          refFor node@(NodeId i) =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode node) ("n" ++ show i)
          isNamed _ = False
      expectRight (reifyWithRefs "test" view refFor isNamed RootType root) $ \ty ->
        ty `shouldSatisfy` isPrefixedVarType "n"

    it "reifies literal with RootBound" $ do
      artifacts <- pipelineFor (ELit (LInt 7))
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
          refFor node@(NodeId i) =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode node) ("n" ++ show i)
          isNamed _ = False
      expectRight (reifyWithRefs "test" view refFor isNamed RootBound root) $ \ty ->
        ty `shouldSatisfy` isBaseType

    it "preserves a flexible forall bound reached through a rigid alias" $ do
      let outerGen = GenNodeId 0
          rootGen = GenNodeId 1
          root = NodeId 0
          domain = NodeId 1
          result = NodeId 2
          rigidAlias = NodeId 3
          sigmaId = NodeId 4
          sigmaBody = NodeId 5
          sigmaBinder = NodeId 6
          replayResult = NodeId 7
          constraint =
            emptyConstraint
              { cNodes =
                  nodeMapFromList
                    [ (getNodeId root, TyArrow root domain replayResult),
                      (getNodeId domain, TyBottom domain),
                      (getNodeId result, TyVar result (Just rigidAlias)),
                      (getNodeId rigidAlias, TyVar rigidAlias (Just sigmaId)),
                      (getNodeId sigmaId, TyForall sigmaId sigmaBody),
                      (getNodeId sigmaBody, TyArrow sigmaBody sigmaBinder sigmaBinder),
                      (getNodeId sigmaBinder, TyVar sigmaBinder Nothing),
                      (getNodeId replayResult, TyVar replayResult Nothing)
                    ],
                cBindParents =
                  IntMap.fromList
                    [ (nodeRefKey (typeRef root), (genRef rootGen, BindFlex)),
                      (nodeRefKey (typeRef domain), (typeRef root, BindRigid)),
                      (nodeRefKey (typeRef result), (genRef outerGen, BindFlex)),
                      (nodeRefKey (typeRef rigidAlias), (genRef outerGen, BindRigid)),
                      (nodeRefKey (typeRef sigmaId), (genRef outerGen, BindFlex)),
                      (nodeRefKey (typeRef sigmaBody), (typeRef sigmaId, BindRigid)),
                      (nodeRefKey (typeRef sigmaBinder), (typeRef sigmaId, BindFlex)),
                      (nodeRefKey (typeRef replayResult), (genRef rootGen, BindFlex)),
                      (nodeRefKey (genRef rootGen), (genRef outerGen, BindFlex))
                    ],
                cGenNodes =
                  fromListGen
                    [ (outerGen, GenNode outerGen [sigmaId]),
                      (rootGen, GenNode rootGen [root])
                    ]
              }
          view = presolutionViewFromSnapshot constraint IntMap.empty
          refFor node@(NodeId i)
            | node == replayResult =
                typeBinderRefFromIdentity (typeBinderIdentityFromNode result) "result"
            | otherwise =
                typeBinderRefFromIdentity (typeBinderIdentityFromNode node) ("t" ++ show i)
          isNamed _ = False

      expectRight (reifyWithRefs "test" view refFor isNamed RootTypeNoFallback root) $ \ty ->
        case ty of
          TForallRef resultRef (Just bound) (TArrow _ (TVarRef bodyResultRef)) -> do
            typeBinderIdentityKey (typeBinderRefIdentity resultRef)
              `shouldBe` getNodeId result
            typeBinderIdentityKey (typeBinderRefIdentity bodyResultRef)
              `shouldBe` getNodeId result
            case bound of
              TForallRef sigmaRef Nothing (TArrow (TVarRef domRef) (TVarRef codRef)) -> do
                typeBinderIdentityKey (typeBinderRefIdentity sigmaRef)
                  `shouldBe` getNodeId sigmaBinder
                typeBinderIdentityKey (typeBinderRefIdentity domRef)
                  `shouldBe` getNodeId sigmaBinder
                typeBinderIdentityKey (typeBinderRefIdentity codRef)
                  `shouldBe` getNodeId sigmaBinder
              other ->
                expectationFailure ("Expected sigma-id lower bound, got " ++ show other)
          other ->
            expectationFailure ("Expected bounded flexible forall, got " ++ show other)

  describe "reifyWithAs" $ do
    it "applies conversion function after reification" $ do
      artifacts <- pipelineFor (ELit (LInt 1))
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
          refFor node@(NodeId i) =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode node) ("t" ++ show i)
          isNamed _ = False
          asString :: ElabType -> Either ElabError String
          asString ty = Right (show ty)
      expected <- requireRight (reifyWithRefs "test" view refFor isNamed RootType root)
      expectRight (reifyWithAsRefs "test" view refFor isNamed RootType asString root) $ \s ->
        s `shouldBe` show expected

    it "propagates conversion failure" $ do
      artifacts <- pipelineFor (ELit (LInt 1))
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
          refFor node@(NodeId i) =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode node) ("t" ++ show i)
          isNamed _ = False
          failConvert :: ElabType -> Either ElabError String
          failConvert _ = Left (InstantiationError "test-fail")
      case reifyWithAsRefs "test" view refFor isNamed RootType failConvert root of
        Left _ -> pure () -- expected
        Right _ -> expectationFailure "Expected conversion failure"

    it "succeeds with identity conversion" $ do
      artifacts <- pipelineFor (ELit (LInt 1))
      let solved = paSolved artifacts
          view = viewFor solved
          root = paRoot artifacts
          refFor node@(NodeId i) =
            typeBinderRefFromIdentity (typeBinderIdentityFromNode node) ("t" ++ show i)
          isNamed _ = False
      expectRight (reifyWithAsRefs "test" view refFor isNamed RootType Right root) $ \ty ->
        ty `shouldSatisfy` isVarType

-- Predicates for structural assertions on ElabType
isBaseType :: ElabType -> Bool
isBaseType (TBaseWithIdentity _ _) = True
isBaseType _ = False

isVarType :: ElabType -> Bool
isVarType (TVarRef _) = True
isVarType _ = False

isPrefixedVarType :: String -> ElabType -> Bool
isPrefixedVarType prefix ty = case ty of
  TVarRef ref -> prefix `isPrefixOf` typeBinderRefName ref
  _ -> False
