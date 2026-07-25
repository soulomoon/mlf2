{-# LANGUAGE DataKinds #-}

module ConstraintGenSpec (spec) where

import IdentityTestSupport
import Control.Monad (filterM, forM, forM_, when)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, isJust)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import MLF.Binding.Tree (boundFlexChildren, checkBindingTree, isUnderRigidBinder, lookupBindParent, nodeKind, NodeKind(..))
import MLF.Constraint.Presolution (PresolutionResult(..))
import MLF.Constraint.Solve (solveUnifyWithSnapshot)
import MLF.Constraint.Solved (fromSolveOutput, originalConstraint)
import MLF.Constraint.Types.Graph hiding (lookupNode)
import MLF.Constraint.Types.Phase (Phase(Raw))
import MLF.Frontend.ConstraintGen
    ( AnnExpr (..)
    , InstantiationSite (..)
    , InstantiationTargetTopology (..)
    , ExternalBinding (..)
    , ExternalBindingMode (..)
    , externalBindingIdentityFromDetails
    , generateConstraintsCore
    , generateConstraintsCoreWithExternalBindings
    , generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
    , generateResolvedConstraintsCore
    , generateResolvedConstraintsCoreWithExternalBindings
    )
import MLF.Elab.Run.Annotation (mapAnnNodes)
import qualified MLF.Frontend.Program.Builtins as Builtins
import MLF.API hiding (lookupNode)
import MLF.Pipeline (ConstraintError(..), ConstraintResult(..), defaultTraceConfig, inferConstraintGraph)
import MLF.Types.Identity
    ( IdDetails (..)
    , LocalIdentity (..)
    , StructuralTypeBinderRole (..)
    , TypeBinderIdentity
    , UniqueIdentity (..)
    , idDetailsSameIdentity
    , initialIdentityGenerator
    , localRefFromIdentity
    , typeBinderIdentityFromStructural
    , typeBinderIdentityFromUnique
    )
import SpecUtil
    ( expectRight
    , lookupNode
    , lookupNodeMaybe
    , nodeMapElems
    , nodeMapSize
    , requireRight
    , mkForalls
    , runToPresolutionDefault
    , unsafeNormalizeExpr
    )

inferConstraintGraphDefault :: SurfaceExpr -> Either ConstraintError (ConstraintResult 'Raw)
inferConstraintGraphDefault expr =
    inferConstraintGraph Set.empty (unsafeNormalizeExpr expr)

inferConstraintGraphWithTypeHeads :: [String] -> SurfaceExpr -> Either ConstraintError (ConstraintResult 'Raw)
inferConstraintGraphWithTypeHeads names expr =
    inferConstraintGraph
        (Set.fromList (map testTypeIdentity names))
        (unsafeNormalizeExpr expr)

localDetails :: Int -> String -> IdDetails
localDetails unique name =
    LocalId (localRefFromIdentity (GeneratedLocalId (UniqueIdentity unique)) name)

monomorphicExternalBinding :: IdDetails -> ExternalBinding
monomorphicExternalBinding details =
    ExternalBinding
        { externalBindingType = STBase "Int"
        , externalBindingMode = ExternalBindingMonomorphic
        , externalBindingIdentity = externalBindingIdentityFromDetails details
        , externalBindingTypeHeadIdentities = Map.empty
        , externalBindingTypeBinderIdentities = Map.empty
        }

expectSourceBinderIdentityNodes :: TypeBinderIdentity -> (TyNode -> Bool) -> ConstraintResult p -> Expectation
expectSourceBinderIdentityNodes expected isOwner result = do
    let identities = crSourceTypeBinderIdentities result
        nodes = cNodes (crConstraint result)
        identityNodes =
            [ node
            | (key, identity) <- IntMap.toList identities
            , identity == expected
            , Just node <- [lookupNodeMaybe nodes (NodeId key)]
            ]
    IntMap.null identities `shouldBe` False
    Set.fromList (IntMap.elems identities) `shouldBe` Set.singleton expected
    identityNodes `shouldSatisfy` any isLexicalBinder
    identityNodes `shouldSatisfy` any isOwner
    forM_ identityNodes $ \node ->
        if isLexicalBinder node || isOwner node
            then pure ()
            else expectationFailure ("Expected source identity on a lexical binder or its owner, saw " ++ show node)
  where
    isLexicalBinder TyVar {} = True
    isLexicalBinder _ = False

isForallOwner :: TyNode -> Bool
isForallOwner TyForall {} = True
isForallOwner _ = False

isMuOwner :: TyNode -> Bool
isMuOwner TyMu {} = True
isMuOwner _ = False

spec :: Spec
spec = describe "Phase 1 — Constraint generation" $ do
    describe "Source binder identities" $ do
        it "records a known semantic identity on bound forall nodes" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991850)
                expr =
                    EAnn
                        (ELam "x" (EVar "x"))
                        (STForall "a" Nothing (STArrow (STVar "a") (STVar "a")))
            expectRight
                ( generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
                    initialIdentityGenerator
                    Set.empty
                    Map.empty
                    (Map.singleton "a" identity)
                    Map.empty
                    (unsafeNormalizeExpr expr)
                )
                (expectSourceBinderIdentityNodes identity isForallOwner)

        it "records a known semantic identity on bound structural mu nodes" $ do
            let identity =
                    typeBinderIdentityFromStructural
                        (UniqueIdentity 991851)
                        StructuralSelfBinder
                expr =
                    EAnn
                        (ELam "x" (EVar "x"))
                        (STMu "self" (STArrow (STVar "self") (STVar "self")))
            expectRight
                ( generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
                    initialIdentityGenerator
                    Set.empty
                    Map.empty
                    (Map.singleton "self" identity)
                    Map.empty
                    (unsafeNormalizeExpr expr)
                )
                (expectSourceBinderIdentityNodes identity isMuOwner)

        it "records a bare annotated lambda parameter as the resolved source binder" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991852)
                expr = ELamAnn "x" (STVar "a") (EVar "x")
            result <-
                requireRight
                    ( generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
                        initialIdentityGenerator
                        Set.empty
                        Map.empty
                        (Map.singleton "a" identity)
                        Map.empty
                        (unsafeNormalizeExpr expr)
                    )
            case crAnnotated result of
                ALam _ _ paramNode _ _ _ _ ->
                    IntMap.lookup (getNodeId paramNode) (crSourceTypeBinderIdentities result)
                        `shouldBe` Just identity
                other -> expectationFailure ("Expected annotated lambda, saw " ++ show other)

        it "propagates a source binder identity to polymorphic occurrence expansions" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991853)
                annotation =
                    STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
                expr =
                    ELet
                        "id"
                        (EAnn (ELam "x" (EVar "x")) annotation)
                        (EVar "id")
            result <-
                requireRight
                    ( generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
                        initialIdentityGenerator
                        Set.empty
                        Map.empty
                        (Map.singleton "a" identity)
                        Map.empty
                        (unsafeNormalizeExpr expr)
                    )
            occurrenceNode <-
                case crAnnotated result of
                    ALet _ _ _ _ _ _ _ (ALetScope (AResolvedVar _ "id" nid) _ _) _ ->
                        pure nid
                    other ->
                        expectationFailure ("Expected a polymorphic let occurrence, saw " ++ show other)
                            >> fail "missing polymorphic occurrence"
            case lookupNodeMaybe (cNodes (crConstraint result)) occurrenceNode of
                Just TyExp {} -> pure ()
                other -> expectationFailure ("Expected a TyExp occurrence, saw " ++ show other)
            IntMap.lookup
                (getNodeId occurrenceNode)
                (crSourceTypeBinderIdentities result)
                `shouldBe` Just identity

        it "records an external scheme's own binders without hiding inherited free binder identities" $ do
            let identity = typeBinderIdentityFromUnique (UniqueIdentity 991854)
                inheritedIdentity =
                    typeBinderIdentityFromUnique (UniqueIdentity 991856)
                details = localDetails 991855 "externalId"
                externalBinding =
                    ExternalBinding
                        { externalBindingType =
                            STForall
                                "a"
                                Nothing
                                (STArrow (STVar "a") (STVar "ambient"))
                        , externalBindingMode = ExternalBindingScheme
                        , externalBindingIdentity =
                            externalBindingIdentityFromDetails details
                        , externalBindingTypeHeadIdentities = Map.empty
                        , externalBindingTypeBinderIdentities =
                            Map.singleton "a" identity
                        }
            scopedResult <-
                requireRight
                    ( generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
                        initialIdentityGenerator
                        Set.empty
                        Map.empty
                        (Map.singleton "ambient" inheritedIdentity)
                        (Map.singleton "externalId" externalBinding)
                        (unsafeNormalizeExpr (EVar "externalId"))
                    )
            let identities =
                    crSourceTypeBinderIdentities scopedResult
                nodes = cNodes (crConstraint scopedResult)
                identityNodes expected =
                    [ node
                    | (key, actual) <- IntMap.toList identities
                    , actual == expected
                    , Just node <- [lookupNodeMaybe nodes (NodeId key)]
                    ]
            Set.fromList (IntMap.elems identities)
                `shouldBe` Set.fromList [identity, inheritedIdentity]
            identityNodes identity `shouldSatisfy` any isForallOwner
            identityNodes inheritedIdentity
                `shouldSatisfy` any
                    ( \node ->
                        case node of
                            TyVar {} -> True
                            _ -> False
                    )

    describe "Literals" $ do
        it "creates a single base node for integer literals" $ do
            let expr = ELit (LInt 42)
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                nodeMapSize nodes `shouldBe` 2
                case [name | TyBase { tnBase = BaseTy name } <- nodeMapElems nodes] of
                    ["Int"] -> pure ()
                    other -> expectationFailure $ "Unexpected nodes: " ++ show other
                case lookupNodeMaybe nodes (crRoot result) of
                    Just TyVar { tnBound = Just boundId } -> do
                        bound <- lookupNode nodes boundId
                        case bound of
                            TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                            other -> expectationFailure $ "Expected Int bound, saw " ++ show other
                    other -> expectationFailure $ "Expected bounded TyVar root, saw " ++ show other

        it "records polymorphic base symbols in the constraint" $ do
            let expr = ELit (LInt 1)
                polySyms = Set.fromList [testTypeIdentity "Int"]
            expectRight (inferConstraintGraph polySyms expr) $ \result -> do
                cPolySyms (crConstraint result) `shouldBe` polySyms

        it "creates a single base node for boolean literals" $ do
            let expr = ELit (LBool True)
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                nodeMapSize nodes `shouldBe` 2
                case [name | TyBase { tnBase = BaseTy name } <- nodeMapElems nodes] of
                    ["Bool"] -> pure ()
                    other -> expectationFailure $ "Unexpected nodes: " ++ show other
                case lookupNodeMaybe nodes (crRoot result) of
                    Just TyVar { tnBound = Just boundId } -> do
                        bound <- lookupNode nodes boundId
                        case bound of
                            TyBase { tnBase = BaseTy name } -> name `shouldBe` "Bool"
                            other -> expectationFailure $ "Expected Bool bound, saw " ++ show other
                    other -> expectationFailure $ "Expected bounded TyVar root, saw " ++ show other

        it "creates a single base node for string literals" $ do
            let expr = ELit (LString "hi")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                nodeMapSize nodes `shouldBe` 2
                case [name | TyBase { tnBase = BaseTy name } <- nodeMapElems nodes] of
                    ["String"] -> pure ()
                    other -> expectationFailure $ "Unexpected nodes: " ++ show other
                case lookupNodeMaybe nodes (crRoot result) of
                    Just TyVar { tnBound = Just boundId } -> do
                        bound <- lookupNode nodes boundId
                        case bound of
                            TyBase { tnBase = BaseTy name } -> name `shouldBe` "String"
                            other -> expectationFailure $ "Expected String bound, saw " ++ show other
                    other -> expectationFailure $ "Expected bounded TyVar root, saw " ++ show other

    describe "Variables and scope" $ do
        it "reuses the let scheme node when referencing a binding" $ do
            let expr = ELet "x" (ELit (LInt 0)) (EVar "x")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                (schemeRoot, bodyAnn, resNode) <- case crAnnotated result of
                    ALet _ _ _ schemeRoot' _ _ _ bodyAnn' resNode' ->
                        pure (schemeRoot', bodyAnn', resNode')
                    other -> expectationFailure ("Expected ALet annotation, saw " ++ show other) >> fail "no schemeRoot"
                resNode `shouldBe` crRoot result
                case lookupNodeMaybe nodes resNode of
                    Just TyVar{} -> pure ()
                    other -> expectationFailure $ "Root is not the trivial scheme var: " ++ show other
                case bodyAnn of
                    ALetScope (AResolvedVar _ "x" useNode) annNode edgeId -> do
                        annNode `shouldBe` resNode
                        IntSet.member (getEdgeId edgeId) (cLetEdges constraint) `shouldBe` True
                        IntSet.member (getEdgeId edgeId) (cAnnEdges constraint) `shouldBe` False
                        case lookupNodeMaybe nodes useNode of
                            Just TyExp { tnBody = bodyId } -> bodyId `shouldBe` schemeRoot
                            other -> expectationFailure $ "Expected TyExp use of let-bound x, saw " ++ show other
                        let matchingEdges =
                                [ (instLeft edge, instRight edge)
                                | edge@(InstEdge eid _ _) <- cInstEdges constraint
                                , IntSet.member (getEdgeId eid) (cLetEdges constraint)
                                ]
                        matchingEdges `shouldBe` [(useNode, resNode)]
                        IntSet.intersection (cAnnEdges constraint) (cLetEdges constraint)
                            `shouldBe` IntSet.empty
                    other ->
                        expectationFailure $ "Expected let-scope body annotation, saw " ++ show other

        -- Shadowing should behave like lexical scope: a nested let reuses the
        -- same variable name but its reference must point at the innermost binding.
        -- In
        --   let x = 0 in let x = True in x
        -- the application should therefore resolve to the Bool expansion/node
        -- despite the outer Int binding. This ensures the environment map used by
        -- constraint generation mirrors standard scoping rules.
        it "returns the innermost binding when variables are shadowed" $ do
            let expr =
                    ELet "x" (ELit (LInt 0))
                        (ELet "x" (ELit (LBool True)) (EVar "x"))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                (innerSchemeRoot, innerBodyAnn) <- case crAnnotated result of
                    ALet _ _ _ _ _ _ _ (ALetScope innerAnn _ _) _ ->
                        case innerAnn of
                            ALet _ _ _ schemeRoot' _ _ _ bodyAnn' _ ->
                                pure (schemeRoot', bodyAnn')
                            other ->
                                expectationFailure ("Expected nested ALet annotation, saw " ++ show other) >> fail "no schemeRoot"
                    other -> expectationFailure ("Expected nested ALet annotation, saw " ++ show other) >> fail "no schemeRoot"
                case innerBodyAnn of
                    ALetScope (AResolvedVar _ "x" useNode) _ _ -> do
                        case lookupNodeMaybe nodes useNode of
                            Just TyExp { tnBody = bodyId } -> bodyId `shouldBe` innerSchemeRoot
                            other -> expectationFailure $ "Expected TyExp for inner x, saw " ++ show other
                    other ->
                        expectationFailure $ "Expected inner let body to be annotated AVar, saw " ++ show other

        it "reports unknown variables" $ do
            inferConstraintGraphDefault (EVar "free") `shouldBe` Left (UnknownVariable "free")

        it "reports unknown variables that appear inside let RHS" $ do
            let expr = ELet "x" (EVar "ghost") (ELit (LInt 0))
            inferConstraintGraphDefault expr `shouldBe` Left (UnknownVariable "ghost")

        it "resolves identity-bearing occurrences independently of display spelling and shadowing" $ do
            let outerDetails = localDetails 91001 "$runtime_outer"
                innerDetails = localDetails 91002 "$runtime_inner"
                expr =
                    EResolvedLam outerDetails "same" $
                        EResolvedLam innerDetails "same" $
                            EResolvedVar outerDetails "$stale_outer_display"
            expectRight (generateResolvedConstraintsCore Set.empty expr) $ \result ->
                case crAnnotated result of
                    ALam _ outerBinder outerNode _ (ALam _ innerBinder _ _ (AResolvedVar occurrenceDetails displayName occurrenceNode) _ _) _ _ -> do
                        outerBinder `shouldBe` outerDetails
                        innerBinder `shouldBe` innerDetails
                        occurrenceDetails `shouldBe` outerDetails
                        displayName `shouldBe` "$stale_outer_display"
                        occurrenceNode `shouldBe` outerNode
                    other ->
                        expectationFailure ("expected nested identity-bearing lambdas, saw " ++ show other)

        it "does not fall back from a resolved occurrence identity to a matching name" $ do
            let binderDetails = localDetails 91003 "same"
                missingDetails = localDetails 91004 "same"
                expr =
                    EResolvedLam binderDetails "same" (EResolvedVar missingDetails "same")
            generateResolvedConstraintsCore Set.empty expr `shouldBe` Left (UnknownVariable "same")

        it "keeps an identity-bearing let local instead of rematerializing a same-named external" $ do
            let binderDetails = localDetails 91007 "$runtime_local"
                externalDetails = localDetails 91008 "same"
                externalBinding = monomorphicExternalBinding externalDetails
                expr =
                    EResolvedLet
                        binderDetails
                        "same"
                        (ELit (LInt 1))
                        (EResolvedVar binderDetails "$stale_local_display")
            expectRight
                (generateResolvedConstraintsCoreWithExternalBindings Set.empty (Map.singleton "same" externalBinding) expr)
                $ \result ->
                    case crAnnotated result of
                        ALet _ actualBinder _ schemeRoot _ _ _ (ALetScope (AResolvedVar occurrenceDetails "$stale_local_display" occurrenceNode) _ _) _ -> do
                            actualBinder `shouldBe` binderDetails
                            occurrenceDetails `shouldBe` binderDetails
                            case lookupNodeMaybe (cNodes (crConstraint result)) occurrenceNode of
                                Just TyExp {tnBody = bodyNode} -> bodyNode `shouldBe` schemeRoot
                                other -> expectationFailure ("expected local let expansion, saw " ++ show other)
                        other -> expectationFailure ("expected identity-bearing let, saw " ++ show other)

        it "keeps a name-keyed external entry as an explicit raw adapter, then upgrades to identity" $ do
            let details = localDetails 91005 "external"
                externalBinding = monomorphicExternalBinding details
            expectRight
                (generateConstraintsCoreWithExternalBindings Set.empty (Map.singleton "external" externalBinding) (EVar "external"))
                $ \result ->
                    case crAnnotated result of
                        AResolvedVar occurrenceDetails "external" _ ->
                            occurrenceDetails `shouldBe` details
                        other -> expectationFailure ("expected raw adapter to produce an identity-bearing occurrence, saw " ++ show other)

        it "selects an external binding by resolved identity when its display spelling is stale" $ do
            let details = localDetails 91006 "$runtime_external"
                externalBinding = monomorphicExternalBinding details
                expr = EResolvedVar details "$stale_external_display"
            expectRight
                (generateResolvedConstraintsCoreWithExternalBindings Set.empty (Map.singleton "$runtime_external" externalBinding) expr)
                $ \result ->
                    case crAnnotated result of
                        AResolvedVar occurrenceDetails "$stale_external_display" _ ->
                            occurrenceDetails `shouldBe` details
                        other -> expectationFailure ("expected identity-bearing external occurrence, saw " ++ show other)

        it "materializes a monomorphic external's authoritative source type" $ do
            let details = localDetails 91009 "$runtime_external"
                externalBinding = monomorphicExternalBinding details
                expr = EResolvedVar details "$stale_external_display"
            expectRight
                (generateResolvedConstraintsCoreWithExternalBindings Set.empty (Map.singleton "$runtime_external" externalBinding) expr)
                $ \result ->
                    case crAnnotated result of
                        AResolvedVar occurrenceDetails "$stale_external_display" occurrenceNode -> do
                            occurrenceDetails `shouldBe` details
                            case lookupNodeMaybe (cNodes (crConstraint result)) occurrenceNode of
                                Just TyVar{tnBound = Just sourceNode} ->
                                    case lookupNodeMaybe (cNodes (crConstraint result)) sourceNode of
                                        Just TyBase{tnBase = BaseTy "Int"} -> pure ()
                                        other -> expectationFailure ("expected exact Int source node, saw " ++ show other)
                                other -> expectationFailure ("expected exact monomorphic wrapper, saw " ++ show other)
                        other -> expectationFailure ("expected identity-bearing external occurrence, saw " ++ show other)

    describe "Applications" $ do
        it "maps only prepared instantiation endpoints" $ do
            let edgeId = EdgeId 17
                allocatedSource = NodeId 101
                allocatedTarget = NodeId 102
                allocatedDomain = NodeId 103
                allocatedCodomain = NodeId 104
                site =
                    InstantiationSite
                        { instantiationSiteEdgeId = edgeId
                        , instantiationSiteAllocatedSource = allocatedSource
                        , instantiationSiteAllocatedTarget = allocatedTarget
                        , instantiationSiteSource = allocatedSource
                        , instantiationSiteTarget = allocatedTarget
                        , instantiationSiteTargetTopology =
                            ArrowInstantiationTarget
                                { instantiationArrowAllocatedDomain = allocatedDomain
                                , instantiationArrowAllocatedCodomain = allocatedCodomain
                                , instantiationArrowDomain = allocatedDomain
                                , instantiationArrowCodomain = allocatedCodomain
                                }
                        }
                mappedAnn =
                    mapAnnNodes
                        (\(NodeId nodeKey) -> NodeId (nodeKey + 1000))
                        (AApp (ALit (LInt 0) (NodeId 105)) (ALit (LInt 1) (NodeId 106)) site site (NodeId 107))
            case mappedAnn of
                AApp _ _ mapped _ _ -> do
                    instantiationSiteEdgeId mapped `shouldBe` edgeId
                    instantiationSiteAllocatedSource mapped `shouldBe` allocatedSource
                    instantiationSiteAllocatedTarget mapped `shouldBe` allocatedTarget
                    instantiationSiteSource mapped `shouldBe` NodeId 1101
                    instantiationSiteTarget mapped `shouldBe` NodeId 1102
                    instantiationSiteTargetTopology mapped
                        `shouldBe` ArrowInstantiationTarget
                            { instantiationArrowAllocatedDomain = allocatedDomain
                            , instantiationArrowAllocatedCodomain = allocatedCodomain
                            , instantiationArrowDomain = NodeId 1103
                            , instantiationArrowCodomain = NodeId 1104
                            }
                other -> expectationFailure ("expected mapped application, saw " ++ show other)

        it "emits instantiation edges for both function and argument" $ do
            let expr = EApp (ELam "x" (EVar "x")) (ELit (LInt 1))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    instEdges = cInstEdges constraint
                length instEdges `shouldBe` 3
                forM_ instEdges $ \edge ->
                    case lookupBindParent constraint (typeRef (instRight edge)) of
                        Just (GenRef _, _) -> pure ()
                        parent ->
                            expectationFailure
                                ( "The destination of an instantiation edge must be bound on a gen node, saw "
                                    ++ show parent
                                )
                case crAnnotated result of
                    AApp (ALam _ _ _ _ _ bodyEid _) _ funSite argSite appResult -> do
                        let funEid = instantiationSiteEdgeId funSite
                            argEid = instantiationSiteEdgeId argSite
                        funEid `shouldNotBe` argEid
                        bodyEid `shouldNotBe` funEid
                        bodyEid `shouldNotBe` argEid
                        let edgeIds = [eid | InstEdge eid _ _ <- instEdges]
                        edgeIds `shouldSatisfy` elem funEid
                        edgeIds `shouldSatisfy` elem argEid
                        edgeIds `shouldSatisfy` elem bodyEid
                        let edgeFor eid = [edge | edge@(InstEdge eid' _ _) <- instEdges, eid' == eid]
                        case (edgeFor funEid, edgeFor argEid) of
                            ([funEdge], [argEdge]) -> do
                                instantiationSiteAllocatedSource funSite `shouldBe` instLeft funEdge
                                instantiationSiteAllocatedTarget funSite `shouldBe` instRight funEdge
                                instantiationSiteSource funSite `shouldBe` instLeft funEdge
                                instantiationSiteTarget funSite `shouldBe` instRight funEdge
                                instantiationSiteAllocatedSource argSite `shouldBe` instLeft argEdge
                                instantiationSiteAllocatedTarget argSite `shouldBe` instRight argEdge
                                instantiationSiteSource argSite `shouldBe` instLeft argEdge
                                instantiationSiteTarget argSite `shouldBe` instRight argEdge
                                case instantiationSiteTargetTopology funSite of
                                    ArrowInstantiationTarget domain0 codomain0 domain codomain -> do
                                        domain0 `shouldBe` instRight argEdge
                                        domain `shouldBe` instRight argEdge
                                        codomain0 `shouldBe` appResult
                                        codomain `shouldBe` appResult
                                    topology -> expectationFailure ("Expected retained arrow topology, saw " ++ show topology)
                                instantiationSiteTargetTopology argSite `shouldBe` AtomicInstantiationTarget
                            edges -> expectationFailure ("Expected exact application edges, saw " ++ show edges)
                    other ->
                        expectationFailure $ "Expected application annotation, saw " ++ show other

    describe "Annotated Terms" $ do
        it "desugars annotated lambda parameters via let" $ do
            -- Thesis sugar (Chapter 12.3.2):
            --   λ(x : τ) a  ≜  λ(x) let x = (x : τ) in a  ≜  λ(x) let x = cτ x in a
            --
            -- So Phase 1 should see an ordinary lambda whose body is a let-binding
            -- with a coercion application as the RHS. The coercion constructs
            -- direct rigid and flexible copies of the annotated type.
            let ann = STBase "Int"
                expr = ELamAnn "x" ann (EVar "x")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case crAnnotated result of
                    ALam _ lamDetails lamParam _ bodyAnn _ _ ->
                        case bodyAnn of
                            ALet "x" mediatorDetails _ schemeRoot _ _ rhsAnn bodyAnn' _ -> do
                                mediatorDetails `shouldNotSatisfy` idDetailsSameIdentity lamDetails
                                schemeNode <- lookupNode nodes schemeRoot
                                case schemeNode of
                                    TyBase {tnBase = BaseTy name} -> name `shouldBe` "Int"
                                    other -> expectationFailure $ "Expected direct Int codomain, saw " ++ show other
                                case rhsAnn of
                                    AAnn (AResolvedVar rhsDetails "x" rhsUse) _ _ -> do
                                        rhsDetails `shouldSatisfy` idDetailsSameIdentity lamDetails
                                        rhsUse `shouldBe` lamParam
                                    other -> expectationFailure $ "Expected annotated RHS, saw " ++ show other
                                case bodyAnn' of
                                    ALetScope (AResolvedVar bodyDetails "x" useNode) _ _ -> do
                                        bodyDetails `shouldSatisfy` idDetailsSameIdentity mediatorDetails
                                        useTy <- lookupNode nodes useNode
                                        case useTy of
                                            TyExp { tnBody = bodyId } -> bodyId `shouldBe` schemeRoot
                                            other -> expectationFailure $ "Expected TyExp use of let-bound x, saw " ++ show other
                                    other -> expectationFailure $ "Expected annotated let body, saw " ++ show other
                            other -> expectationFailure $ "Expected let-body for desugared ELamAnn, saw " ++ show other
                    other -> expectationFailure $ "Expected ALam annotation, saw " ++ show other

        it "respects polymorphic term annotations in let RHS (coercion)" $ do
            -- let id = (λx. x : ∀α. α → α) in id
            -- The annotation is a term coercion, not a declared scheme.
            let ann = mkForalls [("a", Nothing)] (STArrow (STVar "a") (STVar "a"))
                expr = ELet "id" (EAnn (ELam "x" (EVar "x")) ann) (EVar "id")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                schemeRoot <- case crAnnotated result of
                    ALet _ _ _ schemeRoot' _ _ _ _ _ -> pure schemeRoot'
                    other -> expectationFailure ("Expected ALet annotation, saw " ++ show other) >> fail "no schemeRoot"
                -- Figure 8.2.3's Eq-Var case is represented directly: the
                -- flexible codomain is a copy of the source forall graph.
                annotation <- lookupNode nodes schemeRoot
                case annotation of
                    TyForall {tnBody = bodyId} -> do
                        body <- lookupNode nodes bodyId
                        case body of
                            TyArrow {tnDom = domId, tnCod = codId} ->
                                domId `shouldBe` codId
                            other -> expectationFailure $ "Expected Arrow forall body, saw " ++ show other
                    other -> expectationFailure $ "Expected explicit source forall, saw " ++ show other

        it "respects term annotations" $ do
            -- (1 : Int)
            let ann = STBase "Int"
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                rootNode <- lookupNode nodes (crRoot result)
                case rootNode of
                    TyBase {tnBase = BaseTy name} -> name `shouldBe` "Int"
                    other -> expectationFailure $ "Expected direct Int codomain, saw " ++ show other

        it "respects bounded quantification in term annotations (coercion)" $ do
            -- let f = (λx. x : ∀(a ⩾ Int). a -> a) in f
            -- The annotation is a term coercion with bounded quantification.
            let ann = mkForalls [("a", Just (STBase "Int"))] (STArrow (STVar "a") (STVar "a"))
                expr = ELet "f" (EAnn (ELam "x" (EVar "x")) ann) (EVar "f")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                schemeRoot <- case crAnnotated result of
                    ALet _ _ _ schemeRoot' _ _ _ _ _ -> pure schemeRoot'
                    other -> expectationFailure ("Expected ALet annotation, saw " ++ show other) >> fail "no schemeRoot"
                annotation <- lookupNode nodes schemeRoot
                bodyId <- case annotation of
                    TyForall {tnBody = sourceBody} -> pure sourceBody
                    other -> expectationFailure ("Expected explicit source forall, saw " ++ show other) >> fail "missing source forall"
                body <- lookupNode nodes bodyId
                domId <- case body of
                    TyArrow {tnDom = dom, tnCod = cod} -> do
                        dom `shouldBe` cod
                        pure dom
                    other -> expectationFailure ("Expected Arrow forall body, saw " ++ show other) >> fail "missing arrow"
                domNode <- lookupNode nodes domId
                boundId <- case domNode of
                    TyVar {tnBound = Just bound} -> pure bound
                    other -> expectationFailure ("Expected bounded forall binder, saw " ++ show other) >> fail "missing binder bound"
                rhs <- lookupNode nodes boundId
                case rhs of
                    TyBase {tnBase = BaseTy name} -> name `shouldBe` "Int"
                    other -> expectationFailure $ "Expected bound Int, saw " ++ show other

        it "respects instance bounds in annotated lambda parameters (coercion)" $ do
            -- λ(x : ∀(a ⩾ Int). a). x desugars to a let-binding with a coercion term.
            -- Uses of x in the body go through the coercion result type.
            let ann = STForall "a" (Just (mkSrcBound (STBase "Int"))) (STVar "a")
                expr = ELamAnn "x" ann (EVar "x")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case crAnnotated result of
                    ALam _ _ lamParam _ bodyAnn _ _ ->
                        case bodyAnn of
                            ALet "x" _ _ schemeRoot _ _ rhsAnn bodyAnn' _ -> do
                                case rhsAnn of
                                    AAnn (AResolvedVar _ "x" rhsUse) _ _ -> rhsUse `shouldBe` lamParam
                                    other -> expectationFailure $ "Expected annotated RHS, saw " ++ show other
                                case bodyAnn' of
                                    ALetScope (AResolvedVar _ "x" useNode) _ _ -> do
                                        case lookupNodeMaybe nodes useNode of
                                            Just TyExp { tnBody = bodyId } -> bodyId `shouldBe` schemeRoot
                                            other -> expectationFailure $ "Expected TyExp for polymorphic x, saw " ++ show other
                                    other -> expectationFailure $ "Expected annotated let body, saw " ++ show other
                                annotation <- lookupNode nodes schemeRoot
                                binderId <- case annotation of
                                    TyForall {tnBody = sourceBinder} -> pure sourceBinder
                                    other -> expectationFailure ("Expected explicit source forall, saw " ++ show other) >> fail "missing source forall"
                                binder <- lookupNode nodes binderId
                                boundId <- case binder of
                                    TyVar {tnBound = Just bound} -> pure bound
                                    other -> expectationFailure ("Expected bounded forall binder, saw " ++ show other) >> fail "missing binder bound"
                                rhs <- lookupNode nodes boundId
                                case rhs of
                                    TyBase {tnBase = BaseTy name} -> name `shouldBe` "Int"
                                    other -> expectationFailure $ "Expected bound Int, saw " ++ show other
                            other -> expectationFailure $ "Expected let-body for desugared ELamAnn, saw " ++ show other
                    other -> expectationFailure $ "Expected ALam annotation, saw " ++ show other

        it "internalizes normalized forall bounds using indexed StructBound alias" $ do
            let ann :: NormSrcType
                ann = STForall "a" (Just (mkNormBound (STBase "Int"))) (STVar "a")
                expr :: NormSurfaceExpr
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraph Set.empty expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                    hasIntBound =
                        [ ()
                        | TyVar { tnBound = Just boundId } <- nodeMapElems nodes
                        , Just TyBase { tnBase = BaseTy "Int" } <- [lookupNodeMaybe nodes boundId]
                        ]
                hasIntBound `shouldSatisfy` (not . null)

        it "internalizes Bottom type" $ do
            -- λ(x : ⊥). x desugars through a let-binding with scheme ⊥.
            let ann = STBottom
                expr = ELamAnn "x" ann (EVar "x")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case crAnnotated result of
                    ALam _ _ lamParam _ bodyAnn _ _ ->
                        case bodyAnn of
                            ALet "x" _ _ schemeRoot _ _ rhsAnn bodyAnn' _ -> do
                                case rhsAnn of
                                    AAnn (AResolvedVar _ "x" rhsUse) _ _ -> rhsUse `shouldBe` lamParam
                                    other -> expectationFailure $ "Expected annotated RHS, saw " ++ show other
                                case bodyAnn' of
                                    ALetScope (AResolvedVar _ "x" useNode) _ _ -> do
                                        useTy <- lookupNode nodes useNode
                                        case useTy of
                                            TyExp { tnBody = bodyId } -> bodyId `shouldBe` schemeRoot
                                            other -> expectationFailure $ "Expected TyExp use of let-bound x, saw " ++ show other
                                    other -> expectationFailure $ "Expected annotated let body, saw " ++ show other
                                schemeNode <- lookupNode nodes schemeRoot
                                case schemeNode of
                                    TyVar {tnBound = Nothing} -> pure ()
                                    other -> expectationFailure $ "Expected direct Bottom codomain, saw " ++ show other
                            other -> expectationFailure $ "Expected let-body for desugared ELamAnn, saw " ++ show other
                    other -> expectationFailure $ "Expected ALam annotation, saw " ++ show other

    describe "Annotation Edge Cases" $ do
        it "explicit forall annotation on let-bound vars uses a single TyExp" $ do
            let ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
                expr =
                    ELet "id" (ELam "x" (EVar "x"))
                        (EAnn (EVar "id") ann)
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case crAnnotated result of
                    ALet _ _ _ _ _ _ _ bodyAnn _ ->
                        case bodyAnn of
                            ALetScope (AAnn _ _ edgeId) _ letEdgeId -> do
                                IntSet.member (getEdgeId edgeId) (cAnnEdges constraint) `shouldBe` True
                                IntSet.member (getEdgeId edgeId) (cLetEdges constraint) `shouldBe` False
                                IntSet.member (getEdgeId letEdgeId) (cLetEdges constraint) `shouldBe` True
                                IntSet.member (getEdgeId letEdgeId) (cAnnEdges constraint) `shouldBe` False
                                case [edge | edge@(InstEdge eid _ _) <- cInstEdges constraint, eid == edgeId] of
                                    [InstEdge _ left _] -> do
                                        leftNode <- lookupNode nodes left
                                        case leftNode of
                                            TyExp { tnBody = bodyId } -> do
                                                bodyNode <- lookupNode nodes bodyId
                                                case bodyNode of
                                                    TyExp {} ->
                                                        expectationFailure "Expected a single TyExp between the annotation edge and scheme root"
                                                    _ -> pure ()
                                            other ->
                                                expectationFailure $ "Expected annotation edge left to be TyExp, saw " ++ show other
                                    other ->
                                        expectationFailure $ "Expected 1 annotation inst edge, saw " ++ show other
                            other ->
                                expectationFailure $ "Expected source annotation inside let-scope metadata, saw " ++ show other
                    other ->
                        expectationFailure $ "Expected ALet annotation, saw " ++ show other

        it "handles free type variables in annotations" $ do
            -- (1 : a) where 'a' is free
            -- This checks STVar with Nothing lookup result
            let ann = STVar "a"
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                case lookupNodeMaybe nodes (crRoot result) of
                    Just TyVar {} -> pure ()
                    other -> expectationFailure $ "Expected TyVar { tnId = for, tnBound = Nothing } free var, saw " ++ show other

        it "produces valid AnnExpr structure" $ do
             -- let x = 1 in x
             let expr = ELet "x" (ELit (LInt 1)) (EVar "x")
             expectRight (inferConstraintGraphDefault expr) $ \result -> do
                 let ann = crAnnotated result
                 case ann of
                     ALet name _ schemeGen _ _ rhsGen rhsAnn bodyAnn _resNode -> do
                         name `shouldBe` "x"
                         schemeGen `shouldBe` rhsGen
                         -- Basic structural check
                         case rhsAnn of
                             ALit (LInt 1) _ -> pure ()
                             _ -> expectationFailure "RHS annotation mismatch"
                         case bodyAnn of
                             ALetScope (AResolvedVar _ "x" _) _ _ -> pure ()
                             _ -> expectationFailure "Body annotation mismatch"
                     _ -> expectationFailure "Expected ALet annotation"

        it "records an annotated RHS scheme owner separately from its lexical Gamma" $ do
            let expr =
                    ELet "x"
                        (EAnn (ELit (LInt 1)) (STBase "Int"))
                        (EVar "x")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    genNodes = getGenNodeMap (cGenNodes constraint)
                case crAnnotated result of
                    ALet _ _ schemeGen schemeRoot _ rhsGen (AAnn _ annNode _) _ _ -> do
                        annNode `shouldBe` schemeRoot
                        schemeGen `shouldNotBe` rhsGen
                        lookupBindParent constraint (typeRef schemeRoot)
                            `shouldBe` Just (genRef schemeGen, BindFlex)
                        case IntMap.lookup (getGenNodeId schemeGen) genNodes of
                            Just gen -> gnSchemes gen `shouldBe` [schemeRoot]
                            Nothing -> expectationFailure "Missing annotation-owned scheme gen"
                        lookupBindParent constraint (genRef schemeGen)
                            `shouldBe` Just (genRef rhsGen, BindFlex)
                    other ->
                        expectationFailure
                            ("Expected annotated let with distinct scheme and lexical owners, saw " ++ show other)

    describe "Lambda nodes" $ do
        it "constructs the paper lambda-body edge to a fresh codomain" $ do
            let expr = ELam "x" (EVar "x")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case crAnnotated result of
                    ALam _ _ paramNode scopeRoot bodyAnn bodyEid lambdaNode -> do
                        bodyNode <- case bodyAnn of
                            AResolvedVar _ "x" nid -> pure nid
                            other -> expectationFailure ("Expected lambda-bound body occurrence, saw " ++ show other) >> fail "missing body node"
                        bodyNode `shouldBe` paramNode
                        bodyEdge <- case [edge | edge@(InstEdge eid _ _) <- cInstEdges constraint, eid == bodyEid] of
                            [edge] -> pure edge
                            other -> expectationFailure ("Expected one lambda body edge, saw " ++ show other) >> fail "missing body edge"
                        arrowNode <- case lookupNodeMaybe nodes lambdaNode of
                            Just TyVar {tnBound = Just arrowId} -> lookupNode nodes arrowId
                            other -> expectationFailure ("Expected lambda result bounded by an arrow, saw " ++ show other) >> fail "missing arrow"
                        case arrowNode of
                            TyArrow {tnDom = domainNode, tnCod = codomainNode} -> do
                                domainNode `shouldBe` paramNode
                                codomainNode `shouldNotBe` bodyNode
                                instLeft bodyEdge `shouldBe` bodyNode
                                instRight bodyEdge `shouldBe` codomainNode
                                lookupBindParent constraint (typeRef codomainNode)
                                    `shouldBe` Just (genRef scopeRoot, BindFlex)
                            other -> expectationFailure ("Expected lambda arrow, saw " ++ show other)
                        IntSet.member (getEdgeId bodyEid) (cAnnEdges constraint) `shouldBe` False
                        IntSet.member (getEdgeId bodyEid) (cLetEdges constraint) `shouldBe` False
                        [node | node@TyExp {} <- nodeMapElems nodes] `shouldBe` []
                    other -> expectationFailure ("Expected ALam annotation, saw " ++ show other)

        it "constructs the same body edge for compiler-owned exact lambdas" $ do
            let evidenceDetails =
                    EvidenceId
                        (localRefFromIdentity (GeneratedLocalId (UniqueIdentity 991852)) "$evidence")
                reference = ResolvedTermReference evidenceDetails "$evidence"
                expr = EExactLamNode reference (STBase "Int") (EVarNode reference)
            expectRight (generateResolvedConstraintsCore Set.empty expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case crAnnotated result of
                    ALam _ details paramNode scopeRoot (AResolvedVar occurrenceDetails _ bodyNode) bodyEid lambdaNode -> do
                        details `shouldBe` evidenceDetails
                        occurrenceDetails `shouldBe` evidenceDetails
                        bodyNode `shouldBe` paramNode
                        bodyEdge <- case [edge | edge@(InstEdge eid _ _) <- cInstEdges constraint, eid == bodyEid] of
                            [edge] -> pure edge
                            other -> expectationFailure ("Expected one exact-lambda body edge, saw " ++ show other) >> fail "missing exact body edge"
                        arrowNode <- case lookupNodeMaybe nodes lambdaNode of
                            Just TyVar {tnBound = Just arrowId} -> lookupNode nodes arrowId
                            other -> expectationFailure ("Expected exact lambda result bounded by an arrow, saw " ++ show other) >> fail "missing exact arrow"
                        case arrowNode of
                            TyArrow {tnDom = domainNode, tnCod = codomainNode} -> do
                                domainNode `shouldBe` paramNode
                                codomainNode `shouldNotBe` bodyNode
                                instLeft bodyEdge `shouldBe` bodyNode
                                instRight bodyEdge `shouldBe` codomainNode
                                lookupBindParent constraint (typeRef codomainNode)
                                    `shouldBe` Just (genRef scopeRoot, BindFlex)
                            other -> expectationFailure ("Expected exact lambda arrow, saw " ++ show other)
                        IntSet.member (getEdgeId bodyEid) (cAnnEdges constraint) `shouldBe` False
                        IntSet.member (getEdgeId bodyEid) (cLetEdges constraint) `shouldBe` False
                    other -> expectationFailure ("Expected exact ALam annotation, saw " ++ show other)

    describe "Applications" $ do
        -- Verify that application translation produces a single instantiation edge
        -- s τ ≤ (Int → α) where the left-hand side points to the let-generalized
        -- scheme (the TyExp node) and the right-hand side is the arrow demanded by
        -- the call site. Note [Expansion nodes] in 'MLF.Constraint.Types.Graph' explains how the
        -- solver processes these edges.
        it "emits instantiation edges for applications" $ do
            let expr =
                    ELet "f" (ELam "x" (EVar "x"))
                        (EApp (EVar "f") (ELit (LInt 1)))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    insts = cInstEdges constraint
                (lambdaBodyEid, funEid, argEid, appResult) <- case crAnnotated result of
                    ALet _ _ _ _ _ _ rhsAnn bodyAnn _ -> do
                        bodyEid <- case rhsAnn of
                            ALam _ _ _ _ _ eid _ -> pure eid
                            other -> expectationFailure ("Expected lambda let RHS, saw " ++ show other) >> fail "no lambda body edge"
                        case bodyAnn of
                            ALetScope (AApp _ _ funEid' argEid' resNode) _ _ ->
                                pure (bodyEid, funEid', argEid', resNode)
                            other -> expectationFailure ("Expected AApp in let body, saw " ++ show other) >> fail "no app"
                    other -> expectationFailure ("Expected ALet annotation, saw " ++ show other) >> fail "no let"
                let lookupEdge eid =
                        case [edge | edge@(InstEdge eid' _ _) <- insts, eid' == eid] of
                            [edge] -> pure edge
                            other -> expectationFailure ("Expected inst edge " ++ show eid ++ ", saw " ++ show other) >> fail "missing edge"
                funEdge <- lookupEdge (instantiationSiteEdgeId funEid)
                argEdge <- lookupEdge (instantiationSiteEdgeId argEid)
                lambdaBodyEdge <- lookupEdge lambdaBodyEid
                lhs <- lookupNode nodes (instLeft funEdge)
                case lhs of
                    -- The usage of 'f' creates a TyExp wrapping the RHS scheme root.
                    TyExp { tnBody = bodyId } -> do
                        body <- lookupNode nodes bodyId
                        case body of
                            TyVar { tnBound = Just arrowId } -> do
                                arrow <- lookupNode nodes arrowId
                                case arrow of
                                    TyArrow { tnDom = domId, tnCod = codId } -> do
                                        instLeft lambdaBodyEdge `shouldBe` domId
                                        instRight lambdaBodyEdge `shouldBe` codId
                                        domId `shouldNotBe` codId
                                        lookupNode nodes domId >>= (`shouldSatisfy` isVarNode)
                                        lookupNode nodes codId >>= (`shouldSatisfy` isVarNode)
                                    other -> expectationFailure $ "Expansion body is not a lambda arrow: " ++ show other
                            other -> expectationFailure $ "Expansion body is not a lambda root var: " ++ show other
                    other -> expectationFailure $ "Instantiation left-hand side is not an expansion: " ++ show other
                rhs <- lookupNode nodes (instRight funEdge)
                case rhs of
                    TyArrow { tnDom = dom, tnCod = cod } -> do
                        -- Argument instantiation edge should target the domain node.
                        instRight argEdge `shouldBe` dom
                        domNode <- lookupNode nodes (instLeft argEdge)
                        case domNode of
                            TyVar { tnBound = Just boundId } -> do
                                bound <- lookupNode nodes boundId
                                case bound of
                                    TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                                    other -> expectationFailure $ "Argument bound is not Int: " ++ show other
                            other -> expectationFailure $ "Argument node is not a bounded var: " ++ show other
                        cod `shouldBe` appResult
                    other -> expectationFailure $ "Instantiation right-hand side is not an arrow: " ++ show other

        it "connects lambda applications directly to arrow nodes" $ do
            let expr = EApp (ELam "x" (EVar "x")) (ELit (LInt 0))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case crAnnotated result of
                    AApp (ALam _ _ _ _ _ bodyEid _) _ funSite argSite _ -> do
                        let lookupEdge eid =
                                case [edge | edge@(InstEdge eid' _ _) <- cInstEdges constraint, eid' == eid] of
                                    [edge] -> pure edge
                                    other -> expectationFailure ("Expected edge " ++ show eid ++ ", saw " ++ show other) >> fail "missing edge"
                        funEdge <- lookupEdge (instantiationSiteEdgeId funSite)
                        _argEdge <- lookupEdge (instantiationSiteEdgeId argSite)
                        _bodyEdge <- lookupEdge bodyEid
                        lhs <- lookupNode nodes (instLeft funEdge)
                        case lhs of
                            TyVar { tnBound = Just boundId } -> do
                                boundNode <- lookupNode nodes boundId
                                case boundNode of
                                    TyArrow {} -> pure ()
                                    other -> expectationFailure $ "Instantiation left-hand side is not an arrow: " ++ show other
                            other -> expectationFailure $ "Instantiation left-hand side is not a lambda root var: " ++ show other
                        length (cInstEdges constraint) `shouldBe` 3
                    other -> expectationFailure $ "Expected applied lambda annotation, saw " ++ show other

        -- Even when an immediately applied lambda uses its argument multiple
        -- times (here via (\f -> let tmp = f 1 in f True) (\x -> x)), the
        -- instantiation edges should still point at the same parameter TyVar
        -- (the λ argument) and that parameter stays bound at the caller binder
        -- g₀; it is not rebound under any let-introduced binder because lambda
        -- parameters are monomorphic.
        it "reuses the same arrow for multiple immediate lambda applications" $ do
            let expr =
                    EApp
                        (ELam "f" $
                            ELet "tmp"
                                (EApp (EVar "f") (ELit (LInt 1)))
                                (EApp (EVar "f") (ELit (LBool True)))
                        )
                        (ELam "x" (EVar "x"))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    insts = cInstEdges constraint
                let isArgEdgeForDom dom edge =
                        instRight edge == dom && case lookupNodeMaybe nodes (instLeft edge) of
                            Just TyVar{ tnBound = Just boundId } ->
                                case lookupNodeMaybe nodes boundId of
                                    Just TyBase{} -> True
                                    _ -> False
                            _ -> False
                literalFunEdges <- do
                    candidateSets <-
                        mapM
                            (\edge -> do
                                rhs <- lookupNode nodes (instRight edge)
                                case rhs of
                                    TyArrow { tnDom = dom } ->
                                        pure $
                                            if any (isArgEdgeForDom dom) insts
                                                then Just edge
                                                else Nothing
                                    _ -> pure Nothing
                            )
                            insts
                    pure (catMaybes candidateSets)
                length literalFunEdges `shouldBe` 2
                let lhsIds = map instLeft literalFunEdges
                -- `f` is a lambda parameter, so its uses are monomorphic and do not
                -- allocate fresh `TyExp` nodes per occurrence.
                length (nub lhsIds) `shouldBe` 1
                case lhsIds of
                    (lhsId:_) -> do
                        lhs <- lookupNode nodes lhsId
                        case lhs of
                            TyVar{} -> pure ()
                            other ->
                                expectationFailure $ "Instantiation left-hand side is not a parameter TyVar: " ++ show other
                    [] -> expectationFailure "Expected instantiation edges"

    describe "Binding edges" $ do
        it "does not emit instantiation edges for unused let bindings" $ do
            let expr =
                    ELet "f" (ELam "x" (EVar "x"))
                        (ELit (LInt 0))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                case crAnnotated result of
                    ALet _ _ _ _ _ _ _ bodyAnn resNode ->
                        case bodyAnn of
                            ALetScope (ALit (LInt 0) litNode) annNode edgeId -> do
                                annNode `shouldBe` resNode
                                IntSet.member (getEdgeId edgeId) (cLetEdges constraint) `shouldBe` True
                                IntSet.member (getEdgeId edgeId) (cAnnEdges constraint) `shouldBe` False
                                case
                                    [ edge
                                    | edge@(InstEdge eid _ _) <- cInstEdges constraint
                                    , IntSet.member (getEdgeId eid) (cLetEdges constraint)
                                    ] of
                                    [InstEdge _ left right] -> do
                                        left `shouldBe` litNode
                                        right `shouldBe` resNode
                                    other ->
                                        expectationFailure $ "Expected 1 let-expression inst edge, saw " ++ show other
                                IntSet.intersection (cAnnEdges constraint) (cLetEdges constraint)
                                    `shouldBe` IntSet.empty
                            other ->
                                expectationFailure $ "Expected let-scope literal body, saw " ++ show other
                    other ->
                        expectationFailure $ "Expected ALet annotation, saw " ++ show other

        it "binds let RHS nodes to the let-introduced gen node" $ do
            let expr = ELet "id" (ELam "x" (EVar "x")) (ELit (LInt 0))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    bindParents = cBindParents constraint
                (schemeGen, paramId) <- case crAnnotated result of
                    ALet _ _ schemeGen _ _ _ rhsAnn _ _ ->
                        case rhsAnn of
                            ALam _ _ param _ _ _ _ -> pure (schemeGen, param)
                            other -> expectationFailure ("Expected lambda RHS, saw " ++ show other) >> fail "no param"
                    other -> expectationFailure ("Expected ALet annotation, saw " ++ show other) >> fail "no schemeGen"
                case IntMap.lookup (nodeRefKey (typeRef paramId)) bindParents of
                    Just (parent, BindFlex) -> parent `shouldBe` genRef schemeGen
                    Just (parent, flag) -> do
                        parent `shouldBe` genRef schemeGen
                        flag `shouldBe` BindFlex
                    Nothing -> expectationFailure "Missing binding parent for lambda parameter"

        it "preserves inner let binding parents" $ do
            let expr =
                    ELet "x"
                        (ELet "y" (ELit (LInt 0)) (EVar "y"))
                        (EVar "x")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    bindParents = cBindParents constraint
                (innerGen, innerRoot) <- case crAnnotated result of
                    ALet _ _ _ _ _ _ rhsAnn _ _ ->
                        case rhsAnn of
                            ALet _ _ schemeGen schemeRoot _ _ _ _ _ ->
                                pure (schemeGen, schemeRoot)
                            other ->
                                expectationFailure ("Expected inner ALet annotation, saw " ++ show other) >> fail "no inner let"
                    other -> expectationFailure ("Expected nested ALet annotation, saw " ++ show other) >> fail "no inner let"
                case IntMap.lookup (nodeRefKey (typeRef innerRoot)) bindParents of
                    Just (parent, _) -> parent `shouldBe` genRef innerGen
                    Nothing -> expectationFailure "Missing binding parent for inner let RHS"

        it "binds explicit forall variables under the direct forall owner" $ do
            let ann = STForall "a" Nothing (STBase "Int")
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    bindParents = cBindParents constraint
                annNode <- case crAnnotated result of
                    AAnn _ annNode' _ -> pure annNode'
                    other -> expectationFailure ("Expected AAnn annotation, saw " ++ show other) >> pure (error "unreachable")
                let isRootGen gid =
                        IntMap.notMember (nodeRefKey (genRef gid)) bindParents
                    schemeGens =
                        [ gnId gen
                        | gen <- IntMap.elems (getGenNodeMap (cGenNodes constraint))
                        , annNode `elem` gnSchemes gen
                        , not (isRootGen (gnId gen))
                        ]
                schemeGen <- case schemeGens of
                    [gid] -> pure gid
                    _ -> expectationFailure ("Expected single scheme gen, saw " ++ show schemeGens) >> pure (error "unreachable")
                forallNode <- lookupNode nodes annNode
                case forallNode of
                    TyForall {} -> pure ()
                    other -> expectationFailure ("Expected direct source forall root, saw " ++ show other)
                let boundChildren =
                        [ nid
                        | (childKey, (parent, _)) <- IntMap.toList bindParents
                        , parent == typeRef annNode
                        , TypeRef nid <- [nodeRefFromKey childKey]
                        ]
                binderVars <- filterM (\nid -> do
                    node <- lookupNode nodes nid
                    pure $ case node of
                        TyVar {} -> True
                        _ -> False
                    ) boundChildren
                binderVars `shouldSatisfy` (not . null)
                lookupBindParent constraint (typeRef annNode)
                    `shouldBe` Just (genRef schemeGen, BindFlex)

        it "produces a valid binding tree" $ do
            let expr =
                    ELet "id" (ELam "x" (EVar "x"))
                        (EApp (EVar "id") (ELit (LInt 1)))
            expectRight (inferConstraintGraphDefault expr) $ \result ->
                checkBindingTree (crConstraint result) `shouldBe` Right ()

        it "coercion and let scope wiring preserve single-parent invariant" $ do
            let expr =
                    ELet "id"
                        (EAnn (ELam "x" (EVar "x")) (STArrow (STBase "Int") (STBase "Int")))
                        (EVar "id")
            result <- requireRight (inferConstraintGraphDefault expr)
            checkBindingTree (crConstraint result) `shouldBe` Right ()

        it "nested STCon coercion-copy preserves binding-tree validity" $ do
            let nested =
                    STCon "Either"
                        ( STCon "List" (STBase "Int" :| [])
                        :| [STCon "Maybe" (STBase "Bool" :| [])]
                        )
                ann = STArrow nested nested
                expr =
                    ELet "f"
                        (EAnn (ELam "x" (EVar "x")) ann)
                        (EVar "f")
            result <- requireRight (inferConstraintGraphWithTypeHeads ["Either", "List", "Maybe"] expr)
            checkBindingTree (crConstraint result) `shouldBe` Right ()

        it "nested forall coercion paths preserve valid binding tree" $ do
            let ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
                expr = ELet "f" (EAnn (ELam "x" (EVar "x")) ann) (EApp (EVar "f") (ELit (LInt 1)))
            result <- requireRight (inferConstraintGraphDefault expr)
            checkBindingTree (crConstraint result) `shouldBe` Right ()

        it "keeps retained bounded-scheme wrappers out of the elimination domain" $ do
            let rhs = ELam "x" (ELam "y" (EVar "x"))
                schemeTy =
                    mkForalls
                        [ ("a", Nothing)
                        , ("b", Just (STVar "a"))
                        ]
                        (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))
                ann =
                    STForall "a" Nothing
                        (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))
                expr =
                    ELet "c" (EAnn rhs schemeTy) (EAnn (EVar "c") ann)

            pres <-
                requireRight
                    (runToPresolutionDefault Set.empty expr)
            solveOut <- requireRight (solveUnifyWithSnapshot defaultTraceConfig (prConstraint pres))
            solved <- requireRight (fromSolveOutput solveOut)
            let cSolved = originalConstraint solved
                eliminated = cEliminatedVars (prConstraint pres)
                schemeGens =
                    [ gnId gen
                    | gen <- IntMap.elems (getGenNodeMap (cGenNodes cSolved))
                    , not (null (gnSchemes gen))
                    ]
            when (null schemeGens) $
                expectationFailure "Expected at least one scheme gen node"

            qn <- fmap concat $ forM schemeGens $ \gid ->
                requireRight (boundFlexChildren cSolved (genRef gid))
            let qnIds = IntSet.fromList (map getNodeId qn)
            -- The lower-bounded nested scheme root is retained as structure by
            -- chi_e, not consumed as an active instantiation binder.  A true
            -- active-binder elimination is covered by MergeEmissionSpec.
            eliminated `shouldBe` IntSet.empty
            IntSet.intersection eliminated qnIds `shouldBe` IntSet.empty

    describe "Expansion nodes" $ do
        -- Generalized lets expose a shared scheme root. Each call site wraps that
        -- scheme in its own expansion node `s · g` (`TyExp`).
        -- For
        --   let f = λx.x in
        --     let tmp = f 1
        --     in  f True
        -- both applications of f must therefore have distinct `TyExp` nodes on the
        -- left of their function-position instantiation edges, but those `TyExp`s
        -- must wrap the same underlying scheme root.
        it "shares the same expansion node across multiple instantiations of a let-bound value" $ do
            let lam = ELam "x" (EVar "x")
                expr =
                    ELet "f" lam $
                        ELet "tmp"
                            (EApp (EVar "f") (ELit (LInt 1)))
                            (EApp (EVar "f") (ELit (LBool True)))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    insts = cInstEdges constraint
                    nodes = cNodes constraint
                    isTyExpLeft e =
                        case lookupNodeMaybe nodes (instLeft e) of
                            Just TyExp{} -> True
                            _ -> False
                    funEdges = filter isTyExpLeft insts
                -- Each application emits two instantiation edges (fun + arg). We
                -- only care about the function-position edges, which should have a
                -- TyExp node on the left.
                length funEdges `shouldBe` 2
                length (nub (map instLeft funEdges)) `shouldBe` 2

                -- But they should wrap the same underlying Forall node
                let checkBody lhsId = do
                        lhs <- lookupNode nodes lhsId
                        case lhs of
                            TyExp { tnBody = bodyId } -> pure bodyId
                            _ -> expectationFailure "Expected TyExp" >> pure (error "unreachable")

                bodyIds <- mapM checkBody (map instLeft funEdges)
                length (nub bodyIds) `shouldBe` 1

        it "allocates distinct expansion variables for independent lets" $ do
            let expr =
                    ELet "f" (ELam "x" (EVar "x")) $
                        ELet "g" (ELam "y" (EVar "y")) (EVar "g")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let genNodes = cGenNodes (crConstraint result)
                IntMap.size (getGenNodeMap genNodes) `shouldBe` 7 -- root + (scheme, let, body) for each let

        it "emits one instantiation edge per application" $ do
            let lam = ELam "x" (EVar "x")
                expr =
                    ELet "f" lam $
                        ELet "a" (EApp (EVar "f") (ELit (LInt 1))) $
                            ELet "b" (EApp (EVar "f") (ELit (LBool True)))
                                (EApp (EVar "f") (ELit (LString "ok")))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    insts = cInstEdges constraint
                    nodes = cNodes constraint
                    isTyExpLeft e =
                        case lookupNodeMaybe nodes (instLeft e) of
                            Just TyExp{} -> True
                            _ -> False
                    funEdges = filter isTyExpLeft insts
                -- Six application edges, three let-scope edges, and the
                -- lambda RHS's Figure 15.3.5 body edge.
                length insts `shouldBe` 10
                length funEdges `shouldBe` 3
                length (nub (map instLeft funEdges)) `shouldBe` 3 -- Each usage has fresh TyExp

                -- Verify same underlying source
                let checkBody lhsId = do
                        lhs <- lookupNode nodes lhsId
                        case lhs of
                            TyExp { tnBody = bodyId } -> pure bodyId
                            _ -> expectationFailure "Expected TyExp" >> pure (error "unreachable")
                bodyIds <- mapM checkBody (map instLeft funEdges)
                length (nub bodyIds) `shouldBe` 1

    describe "Higher-order structure" $ do
        it "creates application and lambda-body edges for higher-order lambdas" $ do
            let expr = ELam "x" (ELam "y" (EApp (EVar "x") (EVar "y")))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = nodeMapElems (cNodes constraint)
                    arrowNodes = [n | n@TyArrow {} <- nodes]
                length arrowNodes `shouldSatisfy` (>= 2)
                -- Two application edges plus one body edge for each lambda.
                cInstEdges constraint `shouldSatisfy` ((== 4) . length)

    describe "Coercion semantics (thesis-exact)" $ do
        -- US-004: Regression tests for thesis-exact coercion behavior
        -- These tests lock in the rigid domain / flexible codomain semantics
        -- described in papers/these-finale-english.txt §12.3.2.2, §15.3.8

        it "coercion edge destinations are restricted bounded proxies but not locked" $ do
            -- (1 : Int) - the edge destination should be a restricted proxy
            -- bounded by the direct rigid domain copy, with no rigid ancestor.
            let ann = STBase "Int"
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    insts = cInstEdges constraint
                case insts of
                    [InstEdge _ _ destinationNode] -> do
                        domainNode <-
                            case lookupNodeMaybe (cNodes constraint) destinationNode of
                                Just TyVar {tnBound = Just domain} -> pure domain
                                other -> expectationFailure ("Expected bounded annotation destination, saw " ++ show other) >> fail "missing annotation domain"
                        case lookupNodeMaybe (cNodes constraint) domainNode of
                            Just TyBase {tnBase = BaseTy "Int"} -> pure ()
                            other -> expectationFailure ("Expected direct rigid Int domain, saw " ++ show other)
                        kind <- case nodeKind constraint (typeRef destinationNode) of
                            Right k -> pure k
                            Left err -> expectationFailure (show err) >> pure NodeRoot
                        kind `shouldBe` NodeRestricted
                        underRigid <- case isUnderRigidBinder constraint (typeRef destinationNode) of
                            Right b -> pure b
                            Left err -> expectationFailure (show err) >> pure True
                        underRigid `shouldBe` False
                    other -> expectationFailure $ "Expected 1 inst edge, saw " ++ show (length other)

        it "constructs direct rigid/flexible copies behind an edge-only destination proxy" $ do
            -- (1 : Int) - the result type should be the codomain copy
            -- which is distinct from the edge-only destination and its domain
            let ann = STBase "Int"
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    insts = cInstEdges constraint
                    root = crRoot result
                case insts of
                    [InstEdge _ _ destinationNode] -> do
                        root `shouldNotBe` destinationNode
                        let nodes = cNodes constraint
                        rootNode <- lookupNode nodes root
                        domainNode <-
                            case lookupNodeMaybe nodes destinationNode of
                                Just TyVar {tnBound = Just domain} -> pure domain
                                other -> expectationFailure ("Expected bounded annotation destination, saw " ++ show other) >> fail "missing annotation domain"
                        destinationNode `shouldNotBe` domainNode
                        domain <- lookupNode nodes domainNode
                        case (rootNode, domain) of
                            (TyBase {tnBase = BaseTy codomainName}, TyBase {tnBase = BaseTy domainName}) -> do
                                codomainName `shouldBe` "Int"
                                domainName `shouldBe` "Int"
                            other -> expectationFailure $ "Expected direct Int copies, saw " ++ show other
                        nodeKind constraint (typeRef root)
                            `shouldBe` Right NodeInstantiable
                        nodeKind constraint (typeRef destinationNode)
                            `shouldBe` Right NodeRestricted
                        crAnnSourceTypes result
                            `shouldBe` IntMap.singleton (getNodeId root) ann
                        case crAnnotated result of
                            AAnn _ annNode _ -> annNode `shouldBe` root
                            other -> expectationFailure ("Expected direct source annotation result, saw " ++ show other)
                    other -> expectationFailure $ "Expected 1 inst edge, saw " ++ show (length other)

        it "compiler exact annotations construct one direct producer target without a kappa codomain" $ do
            let ann = STBase "Int"
                exactTy = RSTBase (Builtins.builtinTypeSymbol "Int")
                body = ELit (LInt 1)
            sourceResult <- requireRight (inferConstraintGraphDefault (EAnn body ann))
            exactResult <- requireRight (inferConstraintGraphDefault (EExactAnn body ann exactTy))
            let exactConstraint = crConstraint exactResult
                sourceConstraint = crConstraint sourceResult
                exactRoot = crRoot exactResult
                exactGens = IntMap.elems (getGenNodeMap (cGenNodes exactConstraint))
                sourceGens = IntMap.elems (getGenNodeMap (cGenNodes sourceConstraint))
            -- Compiler authority stays in the enclosing RHS scope.  Only the
            -- source kappa form allocates an annotation child gen.
            length exactGens `shouldBe` 1
            length sourceGens `shouldBe` 2
            exactGen <- case exactGens of
                [gen] -> pure (gnId gen)
                other -> expectationFailure ("Expected one exact RHS gen, saw " ++ show other) >> fail "missing exact gen"
            case lookupNodeMaybe (cNodes exactConstraint) exactRoot of
                Just TyBase {tnBase = BaseTy "Int"} -> pure ()
                other -> expectationFailure ("Expected direct exact Int target, saw " ++ show other)
            case cInstEdges exactConstraint of
                [InstEdge eid source target] -> do
                    target `shouldNotBe` exactRoot
                    case lookupNodeMaybe (cNodes exactConstraint) target of
                        Just TyVar {tnBound = Just targetBody} -> targetBody `shouldBe` exactRoot
                        other -> expectationFailure ("Expected bounded exact edge destination, saw " ++ show other)
                    lookupBindParent exactConstraint (typeRef source)
                        `shouldBe` Just (genRef exactGen, BindFlex)
                    lookupBindParent exactConstraint (typeRef target)
                        `shouldBe` Just (genRef exactGen, BindRigid)
                    IntSet.member (getEdgeId eid) (cAnnEdges exactConstraint)
                        `shouldBe` False
                other -> expectationFailure ("Expected one exact authority edge, saw " ++ show other)
            [node | node@TyForall {} <- nodeMapElems (cNodes exactConstraint)] `shouldBe` []
            crAnnSourceTypes exactResult
                `shouldBe` IntMap.singleton (getNodeId exactRoot) ann
            nodeMapSize (cNodes exactConstraint)
                `shouldSatisfy` (< nodeMapSize (cNodes sourceConstraint))
            case lookupNodeMaybe (cNodes sourceConstraint) (crRoot sourceResult) of
                Just TyBase {tnBase = BaseTy "Int"} -> pure ()
                other -> expectationFailure ("Expected direct source kappa codomain, saw " ++ show other)
            case cInstEdges sourceConstraint of
                [InstEdge eid _ _] ->
                    IntSet.member (getEdgeId eid) (cAnnEdges sourceConstraint)
                        `shouldBe` True
                other -> expectationFailure ("Expected one source annotation edge, saw " ++ show other)
            case crAnnotated exactResult of
                AExactAnn {} -> pure ()
                other -> expectationFailure ("Expected compiler annotation authority, saw " ++ show other)
            case crAnnotated sourceResult of
                AExactAnn {} -> expectationFailure "Source EAnn acquired compiler annotation authority"
                AAnn {} -> pure ()
                other -> expectationFailure ("Expected source annotation, saw " ++ show other)

        it "compiler exact root variables use a rigid proxy without stealing the shared identity binder" $ do
            let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991852)
                binderRef = resolvedTypeBinderRefFromIdentity binderIdentity "a"
                ann = STVar "a"
                exactTy = RSTVar binderRef
                body = ELit (LInt 1)
            exactResult <-
                requireRight
                    ( generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
                        initialIdentityGenerator
                        Set.empty
                        Map.empty
                        (Map.singleton "a" binderIdentity)
                        Map.empty
                        (unsafeNormalizeExpr (EExactAnn body ann exactTy))
                    )
            let constraint = crConstraint exactResult
                nodes = cNodes constraint
                resultProxy = crRoot exactResult
                gens = IntMap.elems (getGenNodeMap (cGenNodes constraint))
            rootGen <- case gens of
                [gen] -> do
                    gnSchemes gen `shouldBe` [resultProxy]
                    pure (gnId gen)
                other -> expectationFailure ("Expected one exact RHS gen, saw " ++ show other) >> fail "missing exact gen"
            shared <- case lookupNodeMaybe nodes resultProxy of
                Just TyVar {tnBound = Just identityNode} -> pure identityNode
                other -> expectationFailure ("Expected rigid exact result proxy, saw " ++ show other) >> fail "missing exact result proxy"
            shared `shouldNotBe` resultProxy
            lookupBindParent constraint (typeRef resultProxy)
                `shouldBe` Just (genRef rootGen, BindRigid)
            lookupBindParent constraint (typeRef shared)
                `shouldBe` Just (genRef rootGen, BindRigid)
            IntMap.lookup (getNodeId shared) (crSourceTypeBinderIdentities exactResult)
                `shouldBe` Just binderIdentity
            IntMap.toList (crSourceTypeBinderIdentities exactResult)
                `shouldBe` [(getNodeId shared, binderIdentity)]
            case cInstEdges constraint of
                [InstEdge eid _ target] -> do
                    target `shouldNotBe` resultProxy
                    case lookupNodeMaybe nodes target of
                        Just TyVar {tnBound = Just targetBody} -> targetBody `shouldBe` resultProxy
                        other -> expectationFailure ("Expected edge-only exact proxy, saw " ++ show other)
                    lookupBindParent constraint (typeRef target)
                        `shouldBe` Just (genRef rootGen, BindRigid)
                    IntSet.member (getEdgeId eid) (cAnnEdges constraint) `shouldBe` False
                other -> expectationFailure ("Expected one exact variable edge, saw " ++ show other)
            case crAnnotated exactResult of
                AExactAnn _ _ annNode _ -> annNode `shouldBe` resultProxy
                other -> expectationFailure ("Expected exact variable authority, saw " ++ show other)

        it "keeps a nested exact root variable's shared identity at the definition owner" $ do
            let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991853)
                binderRef = resolvedTypeBinderRefFromIdentity binderIdentity "a"
                ann = STVar "a"
                exactTy = RSTVar binderRef
                expr =
                    ELet "exact"
                        (EExactAnn (ELit (LInt 1)) ann exactTy)
                        (EVar "exact")
            exactResult <-
                requireRight
                    ( generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
                        initialIdentityGenerator
                        Set.empty
                        Map.empty
                        (Map.singleton "a" binderIdentity)
                        Map.empty
                        (unsafeNormalizeExpr expr)
                    )
            let constraint = crConstraint exactResult
                nodes = cNodes constraint
                bindParents = cBindParents constraint
                genNodes = getGenNodeMap (cGenNodes constraint)
            (schemeGen, proxy) <-
                case crAnnotated exactResult of
                    ALet _ _ schemeGen0 schemeRoot _ rhsGen (AExactAnn _ _ annNode _) _ _ -> do
                        rhsGen `shouldBe` schemeGen0
                        annNode `shouldBe` schemeRoot
                        pure (schemeGen0, schemeRoot)
                    other -> expectationFailure ("Expected nested exact let authority, saw " ++ show other) >> fail "missing nested exact let"
            shared <-
                case lookupNodeMaybe nodes proxy of
                    Just TyVar {tnBound = Just identityNode} -> pure identityNode
                    other -> expectationFailure ("Expected nested exact proxy, saw " ++ show other) >> fail "missing nested exact proxy"
            rootGen <-
                case
                    [ gnId gen
                    | gen <- IntMap.elems genNodes
                    , IntMap.notMember (nodeRefKey (genRef (gnId gen))) bindParents
                    ] of
                    [rootGen0] -> pure rootGen0
                    roots -> expectationFailure ("Expected one definition root gen, saw " ++ show roots) >> fail "missing definition root gen"
            rootGen `shouldNotBe` schemeGen
            case IntMap.lookup (getGenNodeId schemeGen) genNodes of
                Just gen -> gnSchemes gen `shouldBe` [proxy]
                Nothing -> expectationFailure "Missing nested exact scheme gen"
            case lookupBindParent constraint (typeRef proxy) of
                Just (parent, _) -> parent `shouldBe` genRef schemeGen
                Nothing -> expectationFailure "Missing nested exact proxy owner"
            lookupBindParent constraint (typeRef shared)
                `shouldBe` Just (genRef rootGen, BindRigid)
            IntMap.toList (crSourceTypeBinderIdentities exactResult)
                `shouldBe` [(getNodeId shared, binderIdentity)]
            checkBindingTree constraint `shouldBe` Right ()

        it "compiler exact annotations preserve a bounded-forall producer identity exactly once" $ do
            let binderIdentity = typeBinderIdentityFromUnique (UniqueIdentity 991851)
                binderRef = resolvedTypeBinderRefFromIdentity binderIdentity "a"
                ann =
                    STForall
                        "a"
                        (Just (mkSrcBound (STBase "Int")))
                        (STArrow (STVar "a") (STVar "a"))
                exactTy =
                    RSTForall
                        binderRef
                        (Just (ResolvedSrcBound (RSTBase (Builtins.builtinTypeSymbol "Int"))))
                        (RSTArrow (RSTVar binderRef) (RSTVar binderRef))
                body = ELam "x" (EVar "x")
            exactResult <-
                requireRight
                    ( generateConstraintsWithExternalBindingsAndTypeIdentitiesFromSupply
                        initialIdentityGenerator
                        Set.empty
                        Map.empty
                        (Map.singleton "a" binderIdentity)
                        Map.empty
                        (unsafeNormalizeExpr (EExactAnn body ann exactTy))
                    )
            let nodes = cNodes (crConstraint exactResult)
                root = crRoot exactResult
                forallNodes = [node | node@TyForall {} <- nodeMapElems nodes]
                gens = IntMap.elems (getGenNodeMap (cGenNodes (crConstraint exactResult)))
            rootGen <- case gens of
                [gen] -> pure (gnId gen)
                other -> expectationFailure ("Expected one exact forall RHS gen, saw " ++ show other) >> fail "missing exact forall gen"
            length forallNodes `shouldBe` 1
            bodyNode <- case lookupNodeMaybe nodes root of
                Just TyForall {tnBody = bodyId} -> pure bodyId
                other -> expectationFailure ("Expected one exact forall target, saw " ++ show other) >> fail "missing exact forall"
            arrowNode <- lookupNode nodes bodyNode
            binderNode <- case arrowNode of
                TyArrow {tnDom = dom, tnCod = cod} -> do
                    dom `shouldBe` cod
                    pure dom
                other -> expectationFailure ("Expected exact forall arrow body, saw " ++ show other) >> fail "missing exact arrow"
            case lookupNodeMaybe nodes binderNode of
                Just TyVar {tnBound = Just boundId} ->
                    case lookupNodeMaybe nodes boundId of
                        Just TyBase {tnBase = BaseTy "Int"} -> pure ()
                        other -> expectationFailure ("Expected exact Int lower bound, saw " ++ show other)
                other -> expectationFailure ("Expected bounded exact binder, saw " ++ show other)
            IntMap.lookup (getNodeId binderNode) (crSourceTypeBinderIdentities exactResult)
                `shouldBe` Just binderIdentity
            (lambdaBodyEid, exactEid) <-
                case crAnnotated exactResult of
                    AExactAnn (ALam _ _ _ _ _ bodyEid _) _ _ annotationEid ->
                        pure (bodyEid, annotationEid)
                    other -> expectationFailure ("Expected exact annotation around lambda, saw " ++ show other) >> fail "missing exact edges"
            let lookupEdge eid =
                    case [edge | edge@(InstEdge eid' _ _) <- cInstEdges (crConstraint exactResult), eid' == eid] of
                        [edge] -> pure edge
                        other -> expectationFailure ("Expected edge " ++ show eid ++ ", saw " ++ show other) >> fail "missing exact edge"
            lambdaBodyEdge <- lookupEdge lambdaBodyEid
            exactEdge <- lookupEdge exactEid
            instRight exactEdge `shouldNotBe` root
            case lookupNodeMaybe nodes (instRight exactEdge) of
                Just TyVar {tnBound = Just targetBody} -> targetBody `shouldBe` root
                other -> expectationFailure ("Expected bounded exact edge destination, saw " ++ show other)
            lookupBindParent (crConstraint exactResult) (typeRef (instLeft exactEdge))
                `shouldBe` Just (genRef rootGen, BindFlex)
            lookupBindParent (crConstraint exactResult) (typeRef (instRight exactEdge))
                `shouldBe` Just (genRef rootGen, BindRigid)
            lookupBindParent (crConstraint exactResult) (typeRef (instRight lambdaBodyEdge))
                `shouldBe` Just (genRef rootGen, BindFlex)

        it "existential type variables are shared between domain and codomain" $ do
            -- (1 : a) where 'a' is free - the free var should be shared
            -- between domain and codomain copies
            let ann = STVar "a"
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    insts = cInstEdges constraint
                    root = crRoot result
                case insts of
                    [InstEdge _ _ destinationNode] -> do
                        -- The direct domain and codomain still share the free
                        -- existential, while the edge owns a distinct target.
                        destinationNode `shouldNotBe` root
                        case lookupNodeMaybe (cNodes constraint) destinationNode of
                            Just TyVar {tnBound = Just domainNode} -> domainNode `shouldBe` root
                            other -> expectationFailure ("Expected bounded annotation destination, saw " ++ show other)
                    other -> expectationFailure $ "Expected 1 inst edge, saw " ++ show (length other)

    describe "Constructor types (STCon)" $ do
        it "internalizes STCon annotations into TestTyCon nodes" $ do
            -- (1 : List Int) - should create a TestTyCon node with head "List" and one arg
            let ann = STCon "List" (STBase "Int" :| [])
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphWithTypeHeads ["List"] expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                    tyConNodes = [n | n@TyCon{} <- nodeMapElems nodes]
                length tyConNodes `shouldBe` 2  -- domain + codomain copies
                case tyConNodes of
                    (TyCon { tnCon = BaseTy name, tnArgs = args }:_) -> do
                        name `shouldBe` "List"
                        length args `shouldBe` 1
                    _ -> expectationFailure "Expected TestTyCon nodes"

        it "internalizes nested STCon annotations" $ do
            -- (1 : Either Int Bool) - should create a TestTyCon node with two args
            let ann = STCon "Either" (STBase "Int" :| [STBase "Bool"])
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphWithTypeHeads ["Either"] expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                    tyConNodes = [n | n@TyCon{} <- nodeMapElems nodes]
                length tyConNodes `shouldBe` 2  -- domain + codomain copies
                case tyConNodes of
                    (TyCon { tnCon = BaseTy name, tnArgs = args }:_) -> do
                        name `shouldBe` "Either"
                        length args `shouldBe` 2
                    _ -> expectationFailure "Expected TestTyCon nodes"

        it "TestTyCon args are correctly structured" $ do
            -- (1 : List Int) - the arg should be an Int base type
            let ann = STCon "List" (STBase "Int" :| [])
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphWithTypeHeads ["List"] expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                    tyConNodes = [n | n@TyCon{} <- nodeMapElems nodes]
                case tyConNodes of
                    (TyCon { tnArgs = (argId :| _) }:_) -> do
                        argNode <- lookupNode nodes argId
                        case argNode of
                            TyBase {tnBase = BaseTy name} -> name `shouldBe` "Int"
                            other -> expectationFailure $ "Expected direct Int argument copy, saw " ++ show other
                    _ -> expectationFailure "Expected TestTyCon nodes"

    describe "Typed coercion error regressions" $ do
        it "bare ECoerceConst rejects with typed UnexpectedBareCoercionConst (not InternalConstraintError string)" $ do
            case generateConstraintsCore Set.empty (ECoerceConst (STBase "Int")) of
                Left UnexpectedBareCoercionConst -> pure ()
                Left (InternalConstraintError msg) ->
                    expectationFailure $ "expected typed bare-coercion error, got internal string path: " ++ msg
                other ->
                    expectationFailure $ "expected typed bare-coercion error, got: " ++ show other

        it "STCon coercion-copy failures surface as typed errors" $ do
            let ann =
                    STArrow
                        (STCon "List" (STBase "Int" :| []))
                        (STCon "List" (STBase "Int" :| [STBase "Bool"]))
                expr = EAnn (ELit (LInt 1)) ann
            case inferConstraintGraphWithTypeHeads ["List"] expr of
                Left (TypeConstructorArityMismatch _ _ _) -> pure ()
                Left (InternalConstraintError msg) ->
                    expectationFailure $ "unexpected internal path: " ++ msg
                other ->
                    expectationFailure $ "unexpected result: " ++ show other

    describe "Type constructor arity validation" $ do
        it "throws TypeConstructorArityMismatch on conflicting arities" $ do
            -- Use List with arity 1, then with arity 2
            let ann1 = STCon "List" (STBase "Int" :| [])
                ann2 = STCon "List" (STBase "Int" :| [STBase "Bool"])
                -- Create a type that uses List with different arities
                ann = STArrow ann1 ann2
                expr = EAnn (ELit (LInt 1)) ann
            case inferConstraintGraphWithTypeHeads ["List"] expr of
                Left (TypeConstructorArityMismatch (BaseTy name) expected actual) -> do
                    name `shouldBe` "List"
                    expected `shouldBe` 1
                    actual `shouldBe` 2
                Left other -> expectationFailure $ "Expected TypeConstructorArityMismatch, saw " ++ show other
                Right _ -> expectationFailure "Expected arity mismatch error"

        it "STBase registers arity 0" $ do
            -- Use Int as base (arity 0), then as constructor (arity 1)
            let ann = STArrow (STBase "Int") (STCon "Int" (STBase "Bool" :| []))
                expr = EAnn (ELit (LInt 1)) ann
            case inferConstraintGraphDefault expr of
                Left (TypeConstructorArityMismatch (BaseTy name) expected actual) -> do
                    name `shouldBe` "Int"
                    expected `shouldBe` 0
                    actual `shouldBe` 1
                Left other -> expectationFailure $ "Expected TypeConstructorArityMismatch, saw " ++ show other
                Right _ -> expectationFailure "Expected arity mismatch error"

    describe "Forall-bound well-formedness (normalized-only)" $ do
        it "alias self-bound ∀(a ⩾ a) caught by normalization as SelfBoundVariable" $ do
            -- ∀(a ⩾ a). a - alias self-bound is rejected at normalization,
            -- before reaching constraint generation.
            let ann = STForall "a" (Just (mkSrcBound (STVar "a"))) (STVar "a")
                expr = EAnn (ELit (LInt 1)) ann
            case normalizeExpr expr of
                Left (SelfBoundVariable name _) -> name `shouldBe` "a"
                Left other -> expectationFailure $ "Expected SelfBoundVariable, saw " ++ show other
                Right _ -> expectationFailure "Expected SelfBoundVariable error"

        it "structural self-reference ∀(a ⩾ List a) caught by ForallBoundMentionsBinder" $ do
            -- ∀(a ⩾ List a). a - the binder 'a' occurs nested in a structural bound.
            -- Normalization passes this through (structural bound, not alias), so
            -- constraint generation catches it via ForallBoundMentionsBinder.
            let ann = STForall "a" (Just (mkSrcBound (STCon "List" (STVar "a" :| [])))) (STVar "a")
                expr = EAnn (ELit (LInt 1)) ann
            case inferConstraintGraphWithTypeHeads ["List"] expr of
                Left (ForallBoundMentionsBinder name) -> name `shouldBe` "a"
                Left other -> expectationFailure $ "Expected ForallBoundMentionsBinder, saw " ++ show other
                Right _ -> expectationFailure "Expected ForallBoundMentionsBinder error"

        it "allows binder in body but not in bound" $ do
            -- ∀(a ⩾ Int). a - valid: 'a' is in body but not in bound
            let ann = STForall "a" (Just (mkSrcBound (STBase "Int"))) (STVar "a")
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                -- The forall annotation should produce bind parents
                length (cBindParents (crConstraint result)) `shouldSatisfy` (> 0)
                -- Root node should exist in the graph
                lookupNodeMaybe nodes (crRoot result) `shouldSatisfy` isJust

        it "alias bound ∀(b ⩾ a) inlined by normalization before constraint gen" $ do
            -- ∀(b ⩾ a). b → a is an alias bound: normalization inlines b := a,
            -- producing a → a. Constraint generation never sees the alias bound.
            let ann = STForall "b" (Just (mkSrcBound (STVar "a"))) (STArrow (STVar "b") (STVar "a"))
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                -- After alias inlining, the annotation type is a → a (an arrow
                -- with shared domain/codomain variable). Verify an arrow exists.
                let arrowNodes = [n | n@TyArrow{} <- nodeMapElems nodes]
                length arrowNodes `shouldSatisfy` (> 0)

    describe "Recursive surface annotation boundary" $ do
        it "internalizes top-level recursive annotations into acyclic TyMu nodes" $ do
            let ann = STMu "self" (STArrow (STVar "self") (STBase "Int"))
                expr :: NormSurfaceExpr
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraph Set.empty expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                    muNodes = [node | node@TyMu{} <- nodeMapElems nodes]
                length muNodes `shouldBe` 2
                case muNodes of
                    [TyMu{ tnBody = body1 }, TyMu{ tnBody = body2 }] -> do
                        body1 `shouldNotBe` body2
                        lookupNode nodes body1 >>= (`shouldSatisfy` isArrow)
                        lookupNode nodes body2 >>= (`shouldSatisfy` isArrow)
                    other ->
                        expectationFailure ("Expected two TyMu nodes, saw " ++ show other)

        it "internalizes recursive annotations nested inside normalized forall bounds" $ do
            let recursiveBound = STMu "self" (STVar "self")
                ann :: NormSrcType
                ann = STForall "a" (Just (mkNormBound recursiveBound)) (STVar "a")
                expr :: NormSurfaceExpr
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraph Set.empty expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                    muNodes = [node | node@TyMu{} <- nodeMapElems nodes]
                length muNodes `shouldBe` 2
                forM_ muNodes $ \node ->
                    case node of
                        TyMu{ tnBody = body } ->
                            lookupNode nodes body >>= (`shouldSatisfy` isVarNode)
                        other ->
                            expectationFailure ("Expected TyMu node, saw " ++ show other)

        -- US-003 Regression: ELet x (EAnn e σ) should NOT introduce explicit-scheme
        -- instantiation edge structure. With coercion-only semantics, an annotated
        -- let is just a normal let whose RHS happens to be a coercion term.
        it "ELet with EAnn RHS does not create explicit-scheme instantiation structure" $ do
            -- let id = ((\x. x) : ∀a. a -> a) in id
            -- This should create the same constraint structure as:
            --   let id = \x. x in id
            -- Because the annotation is just a term coercion, not a declared scheme.
            let ann = mkForalls [("a", Nothing)] (STArrow (STVar "a") (STVar "a"))
                expr = ELet "id" (EAnn (ELam "x" (EVar "x")) ann) (EVar "id")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint

                -- Get the scheme root from the let annotation
                -- ALet: name, schemeGenId, schemeRootId, expVar, scopeRoot, rhs, body, nid
                schemeRoot <- case crAnnotated result of
                    ALet _ _ _ schemeRoot' _ _ _ _ _ -> pure schemeRoot'
                    other -> expectationFailure ("Expected ALet annotation, saw " ++ show other) >> fail "no schemeRoot"

                -- The RHS remains an ordinary term coercion. Figure 8.2.3's
                -- Eq-Var case represents its flexible codomain directly as a
                -- copy of the source annotation's forall graph.
                annotation <- lookupNode nodes schemeRoot
                bodyId <- case annotation of
                    TyForall {tnBody = sourceBody} -> pure sourceBody
                    other -> expectationFailure ("Expected source annotation forall, saw " ++ show other) >> fail "missing source forall"
                body <- lookupNode nodes bodyId
                case body of
                    TyArrow {tnDom = domId, tnCod = codId} ->
                        domId `shouldBe` codId
                    other -> expectationFailure $ "Expected identity Arrow body, saw " ++ show other

                -- The annotated RHS itself owns the annotation edge; there is
                -- no separate declared-scheme path at the let boundary.
                case crAnnotated result of
                    ALet _ _ _ _ _ _ (AAnn _ rhsNode rhsEdge) _ _ -> do
                        rhsNode `shouldBe` schemeRoot
                        IntSet.member (getEdgeId rhsEdge) (cAnnEdges constraint) `shouldBe` True
                    other -> expectationFailure $ "Expected term-coercion RHS, saw " ++ show other

                -- The use of 'id' in the body should have a TyExp node (normal let-polymorphism)
                -- not a direct link to a TyForall scheme
                case crAnnotated result of
                    ALet _ _ _ _ _ _ _ bodyAnn _ ->
                        case bodyAnn of
                            ALetScope (AResolvedVar _ "id" useNode) _ _ -> do
                                useTy <- lookupNode nodes useNode
                                case useTy of
                                    TyExp {} -> pure () -- Normal let-polymorphic use
                                    other -> expectationFailure $
                                        "Expected TyExp for let-bound use, saw " ++ show other
                            other -> expectationFailure $
                                "Expected annotated var in body, saw " ++ show other
                    other -> expectationFailure $
                        "Expected ALet annotation, saw " ++ show other

    describe "Thesis obligations" $ do
        it "O09-CGEN-ROOT" $ do
            -- Root constraint: inferConstraintGraph builds a rooted constraint for a simple expr
            let expr = ELit (LInt 42)
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                nodeMapSize (cNodes constraint) `shouldSatisfy` (> 0)

        it "O09-CGEN-EXPR" $ do
            -- Expr constraint: generateConstraintsCore handles lambda expressions
            let expr = ELam "x" (EVar "x")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                nodeMapSize (cNodes constraint) `shouldSatisfy` (> 1)

isArrow :: TyNode -> Bool
isArrow TyArrow{} = True
isArrow _ = False

isVarNode :: TyNode -> Bool
isVarNode TyVar{} = True
isVarNode _ = False
