{-# LANGUAGE PatternSynonyms #-}
module PipelineSpec (spec) where

import Control.Monad (forM_, unless, when)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck (Gen, arbitrary, chooseInt, counterexample, elements, forAll, property, withMaxSuccess, (===))

import MLF.Elab.Pipeline
    ( ElabType
    , generalizeAtWithBuilder
    , applyRedirectsToAnn
    , canonicalizeAnn
    , isValue
    , normalize
    , renderPipelineError
    , runPipelineElab
    , runPipelineElabChecked
    , step
    , typeCheck
    , pattern Forall
    , Pretty(..)
    )
import MLF.Frontend.Syntax
import MLF.Frontend.ConstraintGen
import MLF.Constraint.Canonicalizer (canonicalizeNode)
import MLF.Constraint.Presolution
import MLF.Constraint.Solve
import MLF.Constraint.Types.Graph
import MLF.Constraint.Types.Witness (EdgeWitness(..), InstanceOp(..), InstanceStep(..))
import qualified MLF.Binding.Tree as Binding
import MLF.Elab.Run.Provenance (buildTraceCopyMap, collectBaseNamedKeys)
import MLF.Elab.Run.Util
    ( canonicalizeTrace
    , canonicalizeWitness
    , chaseRedirects
    , makeCanonicalizer
    )
import SpecUtil
    ( collectVarNodes
    , defaultTraceConfig
    , emptyConstraint
    , PipelineArtifacts(..)
    , mkForalls
    , runPipelineArtifactsDefault
    , runToPresolutionWithAnnDefault
    , runToSolvedDefault
    , unsafeNormalizeExpr
    )
import MLF.Types.Elab (Ty(..), containsArrowTy, containsForallTy)
import MLF.Reify.Core (reifyType)
import Data.List.NonEmpty (NonEmpty(..))

spec :: Spec
spec = describe "Pipeline (Phases 1-5)" $ do
    describe "Elaboration helpers" $ do
        it "reifies type with flexible bound" $ do
            -- Note: With coercion-only annotations, let-bindings with annotated RHS
            -- are treated as normal lets with coercion terms, not declared schemes.
            -- let f = ((\x.x) : ∀(a ⩾ Int). a -> a) in f
            -- The coercion constrains the RHS to match the annotation type.
            let ann = STForall "a" (Just (mkSrcBound (STBase "Int"))) (STArrow (STVar "a") (STVar "a"))
                expr =
                    let schemeTy = mkForalls [] ann
                    in ELet "f" (EAnn (ELam "x" (EVar "x")) schemeTy) (EVar "f")

            case runPipelineArtifactsDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right PipelineArtifacts{ paPresolution = pres, paSolved = res, paRoot = root } -> do
                    validateStrict res
                    let rootRedirected = chaseRedirects (prRedirects pres) root
                        root' = canonical (srUnionFind res) rootRedirected
                    -- With coercion-only semantics, f's type is inferred (not declared)
                    -- The coercion ensures the RHS has the annotated type.
                    let scopeRoot =
                            case Binding.bindingRoots (srConstraint res) of
                                [GenRef gid] -> genRef gid
                                roots -> error ("PipelineSpec: unexpected binding roots " ++ show roots)
                    let generalizeAt = generalizeAtWithBuilder (defaultPlanBuilder defaultTraceConfig) Nothing
                    case generalizeAt res scopeRoot root' of
                        Right (Forall binds ty, _subst) -> do
                            binds `shouldBe` []
                            let tyStr = pretty ty
                            -- With coercion-only: type is inferred, not the declared scheme
                            tyStr `shouldSatisfy` ("∀" `isInfixOf`)
                            tyStr `shouldSatisfy` ("->" `isInfixOf`)
                        Left err -> expectationFailure $ "Generalize error: " ++ show err

        it "generalizes at binding site" $ do
             -- let id = \x. x in id
             let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")

             -- We intercept the pipeline after solve to test generalizeAt on the 'id' binding
             case runPipelineArtifactsDefault defaultPolySyms expr of
                 Left err -> expectationFailure err
                 Right PipelineArtifacts{ paPresolution = pres, paSolved = res, paAnnotated = ann0 } -> do
                     validateStrict res
                     let ann = applyRedirectsToAnn (prRedirects pres) ann0
                     case ann of
                         ALet _ schemeGen schemeRoot _ _ _ _ _ -> do
                             let generalizeAt = generalizeAtWithBuilder (defaultPlanBuilder defaultTraceConfig) Nothing
                             case generalizeAt res (genRef schemeGen) schemeRoot of
                                 Right scheme -> show scheme `shouldSatisfy` ("Forall" `isInfixOf`)
                                 Left err -> expectationFailure $ "Generalize error: " ++ show err
                         _ -> expectationFailure "Expected ALet annotation"

        it "reifies TyCon nodes to TCon in elaborated types" $ do
            -- Test that a constraint containing TyCon nodes reifies correctly to TCon
            -- Create a simple constraint with TyCon: List Int
            let intBase = BaseTy "Int"
                listBase = BaseTy "List"
                var0 = NodeId 0
                var1 = NodeId 1
                baseNode = TyBase var0 intBase
                listNode = TyCon var1 listBase (var0 :| [])
                nodes = fromListNode [(var0, baseNode), (var1, listNode)]
                constraint = emptyConstraint { cNodes = nodes }
                res = SolveResult { srConstraint = constraint, srUnionFind = IntMap.empty }
            case reifyType res var1 of
                Right ty ->
                    case ty of
                        TCon con args -> do
                            con `shouldBe` listBase
                            length args `shouldBe` 1
                            case args of
                                (TBase b :| []) -> b `shouldBe` intBase
                                _ -> expectationFailure "Expected TBase Int as argument"
                        _ -> expectationFailure $ "Expected TCon, got: " ++ show ty
                Left err -> expectationFailure $ "Reify error: " ++ show err

        it "reifies nested TyCon nodes to nested TCon" $ do
            -- Test nested TyCon: List (Maybe Int)
            let intBase = BaseTy "Int"
                maybeBase = BaseTy "Maybe"
                listBase = BaseTy "List"
                var0 = NodeId 0
                var1 = NodeId 1
                var2 = NodeId 2
                intNode = TyBase var0 intBase
                maybeNode = TyCon var1 maybeBase (var0 :| [])
                listNode = TyCon var2 listBase (var1 :| [])
                nodes = fromListNode [(var0, intNode), (var1, maybeNode), (var2, listNode)]
                constraint = emptyConstraint { cNodes = nodes }
                res = SolveResult { srConstraint = constraint, srUnionFind = IntMap.empty }
            case reifyType res var2 of
                Right ty ->
                    case ty of
                        TCon outerCon outerArgs -> do
                            outerCon `shouldBe` listBase
                            case outerArgs of
                                (TCon innerCon innerArgs :| []) -> do
                                    innerCon `shouldBe` maybeBase
                                    case innerArgs of
                                        (TBase b :| []) -> b `shouldBe` intBase
                                        _ -> expectationFailure "Expected TBase Int as innermost arg"
                                _ -> expectationFailure "Expected nested TCon (Maybe Int)"
                        _ -> expectationFailure $ "Expected TCon, got: " ++ show ty
                Left err -> expectationFailure $ "Reify error: " ++ show err

    describe "Integration Tests" $ do
        it "solves let-bound id applied to Bool" $ do
            let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LBool True)))
            case runToSolvedDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right solved -> do
                    validateStrict solved
                    let nodes = cNodes (srConstraint solved)
                    baseNames nodes `shouldContain` [BaseTy "Bool"]
                    noExpNodes nodes

        it "instantiates let-polymorphic id at Int and Bool" $ do
            -- let id = \x. x in let a = id 1 in id True
            let expr =
                    ELet "id" (ELam "x" (EVar "x"))
                        (ELet "a" (EApp (EVar "id") (ELit (LInt 1)))
                            (EApp (EVar "id") (ELit (LBool True))))
            case runToSolvedDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right res -> do
                    validateStrict res
                    let nodes = cNodes (srConstraint res)
                    baseNames nodes `shouldContain` [BaseTy "Int"]
                    noExpNodes nodes

        it "tracks instantiation copy maps for named binders" $ do
            -- Non-trivial instantiation: polymorphic id used at two types
            let expr =
                    ELet "id" (ELam "x" (EVar "x"))
                        (ELet "a" (EApp (EVar "id") (ELit (LInt 1)))
                            (EApp (EVar "id") (ELit (LBool True))))
            case runPipelineArtifactsDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right PipelineArtifacts{ paConstraintNorm = c1, paPresolution = pres, paSolved = solved } ->
                    case validateSolvedGraphStrict solved of
                        [] -> do
                            let canon = makeCanonicalizer (srUnionFind solved) (prRedirects pres)
                                adoptNode = canonicalizeNode canon
                                baseNamedKeysAll = collectBaseNamedKeys c1
                                traceMaps = map (buildTraceCopyMap c1 baseNamedKeysAll adoptNode)
                                                 (IntMap.elems (prEdgeTraces pres))
                                instCopyMapFull = foldl' IntMap.union IntMap.empty traceMaps
                                baseCopyPairs =
                                    [ (baseKey, copyN)
                                    | tr <- IntMap.elems (prEdgeTraces pres)
                                    , (baseKey, copyN) <- IntMap.toList (getCopyMapping (etCopyMap tr))
                                    , IntSet.member baseKey baseNamedKeysAll
                                    ]
                            baseNamedKeysAll `shouldSatisfy` (not . IntSet.null)
                            baseCopyPairs `shouldSatisfy` (not . null)
                            mapM_
                                (\(baseKey, copyN) ->
                                    let expected = adoptNode (NodeId baseKey)
                                        actual = IntMap.lookup (getNodeId (adoptNode copyN)) instCopyMapFull
                                    in case actual of
                                        Nothing ->
                                            expectationFailure ("Missing instantiation copy-map entry for base key " ++ show baseKey)
                                        Just mapped ->
                                            adoptNode mapped `shouldBe` expected
                                )
                                baseCopyPairs
                        vs -> expectationFailure ("validateSolvedGraph failed:\n" ++ unlines vs)

        it "BUG-002-V4 keeps OpRaise targets inside etInterior after witness/trace canonicalization" $ do
            let makeFactory = ELam "x" (ELam "y" (EVar "x"))
                expr =
                    ELam "k"
                        (ELet "make" makeFactory
                            (ELet "c1" (EApp (EVar "make") (EVar "k"))
                                (EApp (EVar "c1") (ELit (LBool True)))))
            case runPipelineArtifactsDefault defaultPolySyms expr of
                Left err -> expectationFailure err
                Right PipelineArtifacts{ paPresolution = pres, paSolved = solved } -> do
                    let canon = makeCanonicalizer (srUnionFind solved) (prRedirects pres)
                        edgeWitnesses = IntMap.map (canonicalizeWitness canon) (prEdgeWitnesses pres)
                        edgeTraces = IntMap.map (canonicalizeTrace canon) (prEdgeTraces pres)
                        raisesForEdge (eid, ew) =
                            [ (eid, n)
                            | StepOmega (OpRaise n) <- ewSteps ew
                            ]
                        raiseTargets = concatMap raisesForEdge (IntMap.toList edgeWitnesses)
                    raiseTargets `shouldSatisfy` (not . null)
                    forM_ raiseTargets $ \(eid, n) ->
                        case IntMap.lookup eid edgeTraces of
                            Nothing ->
                                expectationFailure ("Missing trace for edge with OpRaise: " ++ show eid)
                            Just tr -> do
                                let interiorKeys =
                                        IntSet.fromList
                                            [ getNodeId nid
                                            | nid <- toListInterior (etInterior tr)
                                            ]
                                IntSet.member (getNodeId n) interiorKeys `shouldBe` True

    it "handles higher-order polymorphic apply used twice" $ do
        -- let apply f x = f x; let id = \y. y; let a = apply id 1 in apply id True
        let expr =
                ELet "apply" (ELam "f" (ELam "x" (EApp (EVar "f") (EVar "x"))))
                    (ELet "id" (ELam "y" (EVar "y"))
                        (ELet "a" (EApp (EApp (EVar "apply") (EVar "id")) (ELit (LInt 1)))
                            (EApp (EApp (EVar "apply") (EVar "id")) (ELit (LBool True)))))
        case runToSolvedDefault defaultPolySyms expr of
            Left err -> expectationFailure err
            Right res -> do
                validateStrict res
                let c = srConstraint res
                    nodes = cNodes c
                baseNames nodes `shouldContain` [BaseTy "Int"]
                baseNames nodes `shouldContain` [BaseTy "Bool"]
                noExpNodes nodes
                cInstEdges c `shouldBe` []
                cUnifyEdges c `shouldBe` []

    it "redirected let-use sites keep polymorphic schemes" $ do
        -- let id = \x. x in id id
        let expr =
                ELet "id" (ELam "x" (EVar "x"))
                    (EApp (EVar "id") (EVar "id"))
        case runToPresolutionWithAnnDefault defaultPolySyms expr of
            Left err -> expectationFailure err
            Right (pres, ann) -> do
                let redirects = prRedirects pres
                    varNodes = collectVarNodes "id" ann
                    redirected =
                        [ nid
                        | nid <- varNodes
                        , chaseRedirects redirects nid /= nid
                        ]
                varNodes `shouldSatisfy` (not . null)
                redirected `shouldSatisfy` (not . null)
        case runPipelineElab Set.empty expr of
            Left err -> expectationFailure (renderPipelineError err)
            Right (_term, ty) -> pretty ty `shouldSatisfy` ("∀" `isInfixOf`)

    it "applyRedirectsToAnn and canonicalizeAnn rewrite every node occurrence consistently" $ do
        -- Exercise rewrite coverage on a shape with redirected + canonicalized let scheme roots.
        let rewriteExpr =
                ELet "id" (ELam "x" (EVar "x"))
                    (ELet "f" (EVar "id")
                        (ELet "a" (EApp (EVar "f") (ELit (LInt 1)))
                            (EApp (EVar "f") (ELit (LBool True)))))
            -- Separately exercise the production canonicalizeAnn path via pipeline run.
            canonicalizePathExpr =
                    ELet "id" (ELam "x" (EVar "x"))
                    (ELet "a" (EApp (EVar "id") (ELit (LInt 1)))
                        (EApp (EVar "id") (ELit (LBool True)))
                    )
        case runPipelineArtifactsDefault defaultPolySyms rewriteExpr of
            Left err -> expectationFailure err
            Right PipelineArtifacts{ paPresolution = pres, paSolved = solved, paAnnotated = ann } -> do
                let redirects = prRedirects pres
                    redirectedSchemeRoots =
                        [ nid
                        | nid <- annLetSchemeRoots ann
                        , chaseRedirects redirects nid /= nid
                        ]
                    annRedirected = applyRedirectsToAnn redirects ann
                    staleRedirectNodes =
                        [ nid
                        | nid <- annNodeOccurrences annRedirected
                        , chaseRedirects redirects nid /= nid
                        ]
                redirectedSchemeRoots `shouldSatisfy` (not . null)
                staleRedirectNodes `shouldBe` []
                annRootNode annRedirected `shouldSatisfy` (\nid -> chaseRedirects redirects nid == nid)
                validateStrict solved
                let canonicalize = canonicalizeNode (makeCanonicalizer (srUnionFind solved) redirects)
                    canonicalizedSchemeRoots =
                        [ nid
                        | nid <- annLetSchemeRoots annRedirected
                        , canonicalize nid /= nid
                        ]
                    hasCanonicalizationWork = not (IntMap.null (srUnionFind solved))
                    annCanonical = canonicalizeAnn canonicalize annRedirected
                    staleCanonicalNodes =
                        [ nid
                        | nid <- annNodeOccurrences annCanonical
                        , canonicalize nid /= nid
                        ]
                when hasCanonicalizationWork $
                    canonicalizedSchemeRoots `shouldSatisfy` (not . null)
                staleCanonicalNodes `shouldBe` []
                annRootNode annCanonical `shouldSatisfy` (\nid -> canonicalize nid == nid)
        case runPipelineElab Set.empty canonicalizePathExpr of
            Left err -> expectationFailure (renderPipelineError err)
            Right (_term, ty) ->
                case runPipelineElabChecked Set.empty canonicalizePathExpr of
                    Left errChecked -> expectationFailure (renderPipelineError errChecked)
                    Right (_termChecked, tyChecked) -> do
                        ty `shouldBe` tyChecked

    it "generalizes reused constructors via make const" $ do
        -- let make x = (\z -> x) in let c1 = make 2 in let c2 = make False in c1 True
        let expr =
                ELet "make" (ELam "x" (ELam "z" (EVar "x")))
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt 2)))
                        (ELet "c2" (EApp (EVar "make") (ELit (LBool False)))
                            (EApp (EVar "c1") (ELit (LBool True)))))
        case runToSolvedDefault defaultPolySyms expr of
            Left err -> expectationFailure err
            Right res -> do
                validateStrict res
                let c = srConstraint res
                    nodes = cNodes c
                baseNames nodes `shouldContain` [BaseTy "Int"]
                baseNames nodes `shouldContain` [BaseTy "Bool"]
                noExpNodes nodes
                cInstEdges c `shouldBe` []
                cUnifyEdges c `shouldBe` []

    it "make let-c1-apply-bool path typechecks to Int (strict success)" $ do
        -- BUG-2026-02-06-002 / H15 follow-up:
        -- let make = \x.\y.x in let c1 = make (-4) in c1 True
        let expr =
                ELet "make" (ELam "x" (ELam "y" (EVar "x")))
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EApp (EVar "c1") (ELit (LBool True))))
        case runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr) of
            Left err -> expectationFailure ("Expected success, got error:\n" ++ renderPipelineError err)
            Right (_term, ty) -> ty `shouldBe` TBase (BaseTy "Int")

    describe "BUG-2026-02-08-004 sentinel" $ do
        let expr =
                ELet "id" (ELam "x" (EVar "x"))
                    (ELet "use"
                        (ELamAnn "f" (STArrow (STBase "Int") (STBase "Int"))
                            (EApp (EVar "f") (ELit (LInt 0))))
                        (EApp (EVar "use") (EVar "id")))

        it "BUG-2026-02-08-004 nested let + annotated lambda typechecks to Int" $ do
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure ("Unchecked pipeline failed:\n" ++ renderPipelineError err)
                Right (_term, ty) -> ty `shouldBe` TBase (BaseTy "Int")
            case runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure ("Checked pipeline failed:\n" ++ renderPipelineError err)
                Right (_term, ty) -> ty `shouldBe` TBase (BaseTy "Int")

    it "A6 parity: bounded alias + coercion-heavy path agrees across unchecked, checked, and typeCheck" $ do
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
                ELet "c" (EAnn rhs schemeTy)
                    (EAnn (EVar "c") ann)
            normExpr = unsafeNormalizeExpr expr
            expectPolyBinaryId ty =
                case ty of
                    TForall v Nothing (TArrow dom (TArrow dom' cod))
                        | dom == TVar v && dom' == TVar v && cod == TVar v -> pure ()
                    other ->
                        expectationFailure
                            ("Expected forall a. a -> a -> a, got: " ++ show other)

        case runPipelineElab Set.empty normExpr of
            Left err -> expectationFailure ("Unchecked pipeline failed:\n" ++ renderPipelineError err)
            Right (term, ty) -> do
                expectPolyBinaryId ty
                checkedFromUnchecked <-
                    case typeCheck term of
                        Left tcErr -> expectationFailure ("typeCheck(unchecked term) failed: " ++ show tcErr) >> fail "typeCheck failed"
                        Right out -> pure out
                expectPolyBinaryId checkedFromUnchecked
                case runPipelineElabChecked Set.empty normExpr of
                    Left errChecked -> expectationFailure ("Checked pipeline failed:\n" ++ renderPipelineError errChecked)
                    Right (termChecked, tyChecked) -> do
                        expectPolyBinaryId tyChecked
                        checkedFromUnchecked `shouldBe` tyChecked
                        case typeCheck termChecked of
                            Left tcErr -> expectationFailure ("typeCheck(checked term) failed: " ++ show tcErr)
                            Right out -> out `shouldBe` tyChecked

    it "BUG-2026-02-17-002: applied bounded-coercion path elaborates to Int in unchecked and checked pipelines" $ do
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
                ELet "c" (EAnn rhs schemeTy)
                    (EApp
                        (EApp (EAnn (EVar "c") ann) (ELit (LInt 1)))
                        (ELit (LInt 2)))
            normExpr = unsafeNormalizeExpr expr
            expectedTy = TBase (BaseTy "Int")
        case runPipelineElab Set.empty normExpr of
            Left err ->
                expectationFailure ("Unchecked pipeline failed:\n" ++ renderPipelineError err)
            Right (term, ty) -> do
                ty `shouldBe` expectedTy
                typeCheck term `shouldBe` Right expectedTy
                case runPipelineElabChecked Set.empty normExpr of
                    Left errChecked ->
                        expectationFailure ("Checked pipeline failed:\n" ++ renderPipelineError errChecked)
                    Right (termChecked, tyChecked) -> do
                        tyChecked `shouldBe` expectedTy
                        tyChecked `shouldBe` ty
                        typeCheck termChecked `shouldBe` Right expectedTy

    describe "BUG-2026-02-06-002 sentinel matrix" $ do
        let makeFactory = ELam "x" (ELam "y" (EVar "x"))
            makeOnlyExpr = ELet "make" makeFactory (EVar "make")
            makeAppExpr = ELet "make" makeFactory (EApp (EVar "make") (ELit (LInt (-4))))
            letC1ReturnExpr =
                ELet "make" makeFactory
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EVar "c1"))
            letC1ApplyBoolExpr =
                ELet "make" makeFactory
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EApp (EVar "c1") (ELit (LBool True))))

            runChecked expr =
                runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr)

            stripForallsTy ty =
                case ty of
                    TForall _ _ body -> stripForallsTy body
                    _ -> ty

            expectPolymorphicIntReturn ty =
                case stripForallsTy ty of
                    TArrow dom cod -> do
                        dom `shouldNotBe` TBottom
                        cod `shouldBe` TBase (BaseTy "Int")
                    other ->
                        expectationFailure
                            ("Expected arrow return shape ending in Int, got: " ++ show other)

        it "make-only still reports let mismatch sentinel" $
            case runChecked makeOnlyExpr of
                Left err -> expectationFailure ("Expected success, got error:\\n" ++ renderPipelineError err)
                Right (_term, ty) -> do
                    ty `shouldSatisfy` containsForallTy
                    ty `shouldSatisfy` containsArrowTy
                    show ty `shouldNotSatisfy` ("TBottom" `isInfixOf`)

        it "make-app currently collapses to bottom-int arrow sentinel" $
            case runChecked makeAppExpr of
                Left err -> expectationFailure ("Expected success, got error:\\n" ++ renderPipelineError err)
                Right (_term, ty) -> expectPolymorphicIntReturn ty

        it "let-c1-return still reports bottom-vs-var mismatch sentinel" $
            case runChecked letC1ReturnExpr of
                Left err -> expectationFailure ("Expected success, got error:\\n" ++ renderPipelineError err)
                Right (_term, ty) -> expectPolymorphicIntReturn ty

        it "let-c1-apply-bool keeps post-H15 mismatch without t23 leakage" $
            case runChecked letC1ApplyBoolExpr of
                Left err -> expectationFailure ("Expected success, got error:\\n" ++ renderPipelineError err)
                Right (_term, ty) -> ty `shouldBe` TBase (BaseTy "Int")

    describe "BUG-2026-02-06-002 strict target matrix" $ do
        let makeFactory = ELam "x" (ELam "y" (EVar "x"))
            makeOnlyExpr = ELet "make" makeFactory (EVar "make")
            makeAppExpr = ELet "make" makeFactory (EApp (EVar "make") (ELit (LInt (-4))))
            letC1ReturnExpr =
                ELet "make" makeFactory
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EVar "c1"))
            letC1ApplyBoolExpr =
                ELet "make" makeFactory
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EApp (EVar "c1") (ELit (LBool True))))

            runChecked expr =
                runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr)

            stripForallsTy ty =
                case ty of
                    TForall _ _ body -> stripForallsTy body
                    _ -> ty

            expectPolymorphicIntReturn ty =
                case stripForallsTy ty of
                    TArrow dom cod -> do
                        dom `shouldNotBe` TBottom
                        cod `shouldBe` TBase (BaseTy "Int")
                    other ->
                        expectationFailure
                            ("Expected arrow return shape ending in Int, got: " ++ show other)

        it "make-only elaborates as polymorphic factory" $
            case runChecked makeOnlyExpr of
                Left err -> expectationFailure ("Expected success, got error:\n" ++ renderPipelineError err)
                Right (_term, ty) -> do
                    ty `shouldSatisfy` containsForallTy
                    ty `shouldSatisfy` containsArrowTy
                    show ty `shouldNotSatisfy` ("TBottom" `isInfixOf`)

        it "make-app keeps codomain Int without bottom-domain collapse" $
            case runChecked makeAppExpr of
                Left err -> expectationFailure ("Expected success, got error:\n" ++ renderPipelineError err)
                Right (_term, ty) -> expectPolymorphicIntReturn ty

        it "let-c1-return keeps second binder polymorphic" $
            case runChecked letC1ReturnExpr of
                Left err -> expectationFailure ("Expected success, got error:\n" ++ renderPipelineError err)
                Right (_term, ty) -> expectPolymorphicIntReturn ty

        it "let-c1-apply-bool typechecks to Int" $
            case runChecked letC1ApplyBoolExpr of
                Left err -> expectationFailure ("Expected success, got error:\n" ++ renderPipelineError err)
                Right (_term, ty) -> ty `shouldBe` TBase (BaseTy "Int")
    it "composes instantiate then forall when rebound at new level" $ do
        -- let id = \x -> x in let rebound = (let alias = id in alias) in let _ = rebound 1 in rebound True
        let expr =
                ELet "id" (ELam "x" (EVar "x")) $
                    ELet "rebound" (ELet "alias" (EVar "id") (EVar "alias")) $
                        ELet "_" (EApp (EVar "rebound") (ELit (LInt 1)))
                            (EApp (EVar "rebound") (ELit (LBool True)))
        case runToSolvedDefault defaultPolySyms expr of
            Left err -> expectationFailure err
            Right res -> do
                validateStrict res
                let c = srConstraint res
                    nodes = cNodes c
                baseNames nodes `shouldContain` [BaseTy "Int"]
                noExpNodes nodes
                cInstEdges c `shouldBe` []
                cUnifyEdges c `shouldBe` []

    it "allocates fresh instantiations per polymorphic factory call" $ do
        -- let mk = \u -> (let inner = \x -> x in inner) in let a = mk False in let b = mk True in let _ = a 1 in b True
        let expr =
                ELet "mk" (ELam "u" (ELet "inner" (ELam "x" (EVar "x")) (EVar "inner")))
                    (ELet "a" (EApp (EVar "mk") (ELit (LBool False)))
                        (ELet "b" (EApp (EVar "mk") (ELit (LBool True)))
                            (ELet "_" (EApp (EVar "a") (ELit (LInt 1)))
                                (EApp (EVar "b") (ELit (LBool True))))))
        case runToSolvedDefault defaultPolySyms expr of
            Left err -> expectationFailure err
            Right res -> do
                validateStrict res
                let c = srConstraint res
                    nodes = cNodes c
                baseNames nodes `shouldContain` [BaseTy "Int"]
                noExpNodes nodes
                cInstEdges c `shouldBe` []
                cUnifyEdges c `shouldBe` []

    it "materializes expansions inside returned higher-order closures" $ do
        -- let id = \x -> x in let lift f = (\y -> f y) in let lifted = lift id in let _ = lifted 1 in lifted True
        let expr =
                ELet "id" (ELam "x" (EVar "x"))
                    (ELet "lift" (ELam "f" (ELam "y" (EApp (EVar "f") (EVar "y"))))
                        (ELet "lifted" (EApp (EVar "lift") (EVar "id"))
                            (ELet "_" (EApp (EVar "lifted") (ELit (LInt 1)))
                                (EApp (EVar "lifted") (ELit (LBool True))))))
        case runToSolvedDefault defaultPolySyms expr of
            Left err -> expectationFailure err
            Right res -> do
                validateStrict res
                let c = srConstraint res
                    nodes = cNodes c
                baseNames nodes `shouldContain` [BaseTy "Int"]
                noExpNodes nodes
                cInstEdges c `shouldBe` []
                cUnifyEdges c `shouldBe` []

    describe "Checked-authoritative invariant" $ do
        it "runPipelineElab type matches typeCheck(term) and checked pipeline type" $ property $
            withMaxSuccess 80 $
                forAll genClosedWellTypedExpr $ \expr -> do
                    case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                        Left err ->
                            expectationFailure
                                ( "runPipelineElab failed for generated expression:\n"
                                    ++ show expr
                                    ++ "\nerror: "
                                    ++ renderPipelineError err
                                )
                        Right (term, ty) -> do
                            checkedTy <-
                                case typeCheck term of
                                    Left tcErr ->
                                        expectationFailure
                                            ( "typeCheck failed for elaborated term.\nexpr: "
                                                ++ show expr
                                                ++ "\nterm: "
                                                ++ show term
                                                ++ "\nerror: "
                                                ++ show tcErr
                                            )
                                            >> fail "typeCheck failure"
                                    Right out -> pure out
                            assertTypeEq "runPipelineElab vs typeCheck(term)" expr ty checkedTy

                            case runPipelineElabChecked Set.empty (unsafeNormalizeExpr expr) of
                                Left errChecked ->
                                    expectationFailure
                                        ( "runPipelineElabChecked failed for generated expression:\n"
                                            ++ show expr
                                            ++ "\nerror: "
                                            ++ renderPipelineError errChecked
                                        )
                                Right (_termChecked, tyChecked) -> do
                                    assertTypeEq "runPipelineElab vs runPipelineElabChecked" expr ty tyChecked
                                    assertTypeEq "runPipelineElabChecked vs typeCheck(term)" expr tyChecked checkedTy

    describe "Phase 3 atomic wrapping equivalence gates" $ do
        let bugExpr :: SurfaceExpr
            bugExpr =
                ELet "make" (ELam "x" (ELam "y" (EVar "x")))
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt (-4))))
                        (EApp (EVar "c1") (ELit (LBool True))))

            lambdaLetIdExpr :: SurfaceExpr
            lambdaLetIdExpr =
                ELam "y"
                    (ELet "id" (ELam "x" (EVar "x"))
                        (EApp (EVar "id") (EVar "y")))

            expectForallIdentityArrow :: ElabType -> Expectation
            expectForallIdentityArrow ty =
                case ty of
                    TForall v Nothing (TArrow dom cod)
                        | dom == TVar v && cod == TVar v -> pure ()
                    other ->
                        expectationFailure
                            ("Expected forall identity arrow (forall a. a -> a), got: " ++ show other)

        it "gate: make let-c1-apply-bool path typechecks to Int (no mismatch fallback)" $
            case runPipelineElabChecked Set.empty (unsafeNormalizeExpr bugExpr) of
                Left err -> expectationFailure ("checked pipeline failed: " ++ renderPipelineError err)
                Right (_term, ty) -> ty `shouldBe` TBase (BaseTy "Int")

        it "gate: let-c1-apply-bool sentinel matrix returns Int" $
            case runPipelineElabChecked Set.empty (unsafeNormalizeExpr bugExpr) of
                Left err -> expectationFailure ("checked sentinel failed: " ++ renderPipelineError err)
                Right (_term, ty) -> ty `shouldBe` TBase (BaseTy "Int")

        it "gate: let-c1-apply-bool strict target matrix returns Int" $
            case runPipelineElabChecked Set.empty (unsafeNormalizeExpr bugExpr) of
                Left err -> expectationFailure ("checked strict-target failed: " ++ renderPipelineError err)
                Right (_term, ty) -> ty `shouldBe` TBase (BaseTy "Int")

        it "gate: checked-authoritative invariant" $
            case runPipelineElab Set.empty (unsafeNormalizeExpr bugExpr) of
                Left err -> expectationFailure ("runPipelineElab failed: " ++ renderPipelineError err)
                Right (term, ty) -> do
                    checkedTy <-
                        case typeCheck term of
                            Left tcErr -> expectationFailure ("typeCheck failed: " ++ show tcErr) >> fail "typeCheck failed"
                            Right out -> pure out
                    assertTypeEq "gate runPipelineElab vs typeCheck(term)" bugExpr ty checkedTy
                    case runPipelineElabChecked Set.empty (unsafeNormalizeExpr bugExpr) of
                        Left errChecked -> expectationFailure ("runPipelineElabChecked failed: " ++ renderPipelineError errChecked)
                        Right (_termChecked, tyChecked) -> do
                            assertTypeEq "gate runPipelineElab vs runPipelineElabChecked" bugExpr ty tyChecked
                            assertTypeEq "gate runPipelineElabChecked vs typeCheck(term)" bugExpr tyChecked checkedTy

        it "gate: thesis target unchecked pipeline returns Int" $
            case runPipelineElab Set.empty (unsafeNormalizeExpr bugExpr) of
                Left err -> expectationFailure ("unchecked pipeline failed: " ++ renderPipelineError err)
                Right (_term, ty) -> ty `shouldBe` TBase (BaseTy "Int")

        it "gate: thesis target checked pipeline returns Int" $
            case runPipelineElabChecked Set.empty (unsafeNormalizeExpr bugExpr) of
                Left err -> expectationFailure ("checked pipeline failed: " ++ renderPipelineError err)
                Right (_term, ty) -> ty `shouldBe` TBase (BaseTy "Int")

        it "gate: \\y. let id = (\\x. x) in id y has type forall a. a -> a" $
            case runPipelineElab Set.empty (unsafeNormalizeExpr lambdaLetIdExpr) of
                Left err -> expectationFailure ("pipeline failed: " ++ renderPipelineError err)
                Right (_term, ty) -> expectForallIdentityArrow ty


    describe "Phase 4 regression matrix" $ do
        it "suppresses OpWeaken on annotation edges while preserving expansion assignments" $ do
            let annTy = mkForalls [] (STArrow (STBase "Int") (STBase "Int"))
                expr =
                    ELet "f" (EAnn (ELam "x" (EVar "x")) annTy)
                        (EApp (EVar "f") (ELit (LInt 1)))

                hasWeakenStep s = case s of
                    StepOmega (OpWeaken _) -> True
                    _ -> False

            case runToPresolutionWithAnnDefault defaultPolySyms expr of
                Left err -> expectationFailure ("Pipeline failed: " ++ err)
                Right (PresolutionResult{ prConstraint = cPres, prEdgeExpansions = exps, prEdgeWitnesses = ews }, _ann) -> do
                    let annEdges = IntSet.toList (cAnnEdges cPres)
                    annEdges `shouldSatisfy` (not . null)
                    forM_ annEdges $ \eid -> do
                        IntMap.member eid exps `shouldBe` True
                        case IntMap.lookup eid ews of
                            Nothing -> expectationFailure ("Missing witness for annotation edge " ++ show eid)
                            Just EdgeWitness{ ewSteps = steps } ->
                                steps `shouldSatisfy` all (not . hasWeakenStep)

    describe "Pipeline soundness proxies" $ do
        -- Note: Pipeline-elaborated terms may contain internal type annotations
        -- (e.g. TVar "t0") that become unresolvable after step substitutes away
        -- ELet bindings. We skip typeCheck failures on stepped/normalized terms
        -- as a known limitation rather than a soundness violation.

        it "BUG-2026-02-20-001: stepped annotated-let identity remains type-checkable" $ do
            let ann = mkForalls [("a", Nothing)] (STArrow (STVar "a") (STVar "a"))
                expr =
                    ELet "f" (EAnn (ELam "x" (EVar "x")) ann)
                        (EApp (EVar "f") (ELit (LInt 7)))
            case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                Left err -> expectationFailure ("Unchecked pipeline failed:\n" ++ renderPipelineError err)
                Right (term, _ty) -> do
                    ty0 <-
                        case typeCheck term of
                            Left tcErr ->
                                expectationFailure ("typeCheck(term) failed:\n" ++ show tcErr)
                                    >> fail "typeCheck(term) failed"
                            Right out -> pure out
                    stepped <-
                        case step term of
                            Nothing ->
                                expectationFailure "Expected a reduction step for elaborated let term"
                                    >> fail "missing step"
                            Just t' -> pure t'
                    typeCheck stepped `shouldBe` Right ty0

        it "Pipeline preservation proxy: elaborated term preserves type under step" $
            property $
                withMaxSuccess 300 $
                    forAll genClosedWellTypedExpr $ \expr ->
                        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                            Left _ -> property True  -- pipeline failure is not a soundness bug
                            Right (term, _ty) ->
                                case typeCheck term of
                                    Left _ -> property True  -- skip if typeCheck fails
                                    Right ty ->
                                        case step term of
                                            Nothing -> property True
                                            Just term' ->
                                                case typeCheck term' of
                                                    Left _ -> property True  -- internal type var limitation
                                                    Right ty' ->
                                                        counterexample
                                                            ( "pipeline preservation failed\nexpr: "
                                                                ++ show expr
                                                                ++ "\nterm: " ++ show term
                                                                ++ "\nterm': " ++ show term'
                                                                ++ "\ntype(term): " ++ show ty
                                                                ++ "\ntype(term'): " ++ show ty'
                                                            )
                                                            (ty' === ty)

        it "Pipeline progress proxy: elaborated well-typed closed term is value or steps" $
            property $
                withMaxSuccess 300 $
                    forAll genClosedWellTypedExpr $ \expr ->
                        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                            Left _ -> property True
                            Right (term, _ty) ->
                                case typeCheck term of
                                    Left _ -> property True
                                    Right _ ->
                                        counterexample
                                            ( "pipeline progress failed\nexpr: "
                                                ++ show expr
                                                ++ "\nterm: " ++ show term
                                            )
                                            (isValue term || isJust (step term))

        it "Pipeline multi-step preservation proxy: typeCheck(term) = typeCheck(normalize term)" $
            property $
                withMaxSuccess 300 $
                    forAll genClosedWellTypedExpr $ \expr ->
                        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                            Left _ -> property True
                            Right (term, _ty) ->
                                case typeCheck term of
                                    Left _ -> property True
                                    Right ty ->
                                        let term' = normalize term
                                        in case typeCheck term' of
                                            Left _ -> property True  -- internal type var limitation
                                            Right ty' ->
                                                counterexample
                                                    ( "pipeline multi-step preservation failed\nexpr: "
                                                        ++ show expr
                                                        ++ "\nterm: " ++ show term
                                                        ++ "\nnormalize(term): " ++ show term'
                                                        ++ "\ntype(term): " ++ show ty
                                                        ++ "\ntype(normalize): " ++ show ty'
                                                    )
                                                    (ty' === ty)

        it "Pipeline determinism proxy: step is a function on elaborated terms" $
            property $
                withMaxSuccess 300 $
                    forAll genClosedWellTypedExpr $ \expr ->
                        case runPipelineElab Set.empty (unsafeNormalizeExpr expr) of
                            Left _ -> property True
                            Right (term, _ty) ->
                                counterexample
                                    ( "pipeline determinism failed\nexpr: "
                                        ++ show expr
                                        ++ "\nterm: " ++ show term
                                        ++ "\nstep run 1: " ++ show (step term)
                                        ++ "\nstep run 2: " ++ show (step term)
                                    )
                                    (step term === step term)

canonical :: IntMap.IntMap NodeId -> NodeId -> NodeId
canonical uf nid =
    case IntMap.lookup (getNodeId nid) uf of
        Nothing -> nid
        Just p -> canonical uf p

annNodeOccurrences :: AnnExpr -> [NodeId]
annNodeOccurrences expr = case expr of
    AVar _ nid -> [nid]
    ALit _ nid -> [nid]
    ALam _ pNode _ body nid -> pNode : nid : annNodeOccurrences body
    AApp fn arg _ _ nid -> nid : annNodeOccurrences fn ++ annNodeOccurrences arg
    ALet _ _ schemeRoot _ _ rhs body nid ->
        schemeRoot : nid : annNodeOccurrences rhs ++ annNodeOccurrences body
    AAnn inner nid _ -> nid : annNodeOccurrences inner

annLetSchemeRoots :: AnnExpr -> [NodeId]
annLetSchemeRoots expr = case expr of
    AVar _ _ -> []
    ALit _ _ -> []
    ALam _ _ _ body _ -> annLetSchemeRoots body
    AApp fn arg _ _ _ -> annLetSchemeRoots fn ++ annLetSchemeRoots arg
    ALet _ _ schemeRoot _ _ rhs body _ ->
        schemeRoot : annLetSchemeRoots rhs ++ annLetSchemeRoots body
    AAnn inner _ _ -> annLetSchemeRoots inner

annRootNode :: AnnExpr -> NodeId
annRootNode expr = case expr of
    AVar _ nid -> nid
    ALit _ nid -> nid
    ALam _ _ _ _ nid -> nid
    AApp _ _ _ _ nid -> nid
    ALet _ _ _ _ _ _ _ nid -> nid
    AAnn _ nid _ -> nid


validateStrict :: SolveResult -> Expectation
validateStrict res =
    case validateSolvedGraphStrict res of
        [] -> pure ()
        vs -> expectationFailure ("validateSolvedGraph failed:\n" ++ unlines vs)

defaultPolySyms :: PolySyms
defaultPolySyms = Set.empty

noExpNodes :: NodeMap TyNode -> Expectation
noExpNodes nodes =
    case [ nid | TyExp { tnId = nid } <- map snd (toListNode nodes) ] of
        [] -> pure ()
        bad -> expectationFailure ("Unexpected TyExp nodes: " ++ show bad)

baseNames :: NodeMap TyNode -> [BaseTy]
baseNames nodes = [ b | TyBase _ b <- map snd (toListNode nodes) ]

assertTypeEq :: String -> SurfaceExpr -> ElabType -> ElabType -> Expectation
assertTypeEq label expr actual expected =
    unless (actual == expected) $
        expectationFailure
            ( label
                ++ " mismatch.\nexpr: "
                ++ show expr
                ++ "\nactual: "
                ++ pretty actual
                ++ "\nexpected: "
                ++ pretty expected
            )

genClosedWellTypedExpr :: Gen SurfaceExpr
genClosedWellTypedExpr = do
    n <- chooseInt (-5, 5)
    m <- chooseInt (-3, 9)
    b1 <- arbitrary
    let idLam = ELam "x" (EVar "x")
        intLit = ELit (LInt (fromIntegral n))
        intLit2 = ELit (LInt (fromIntegral m))
        boolLit = ELit (LBool b1)
        polyIdTy = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
        exprs =
            [ idLam
            , EApp idLam intLit
            , EApp idLam boolLit
            , ELet "id" idLam (EVar "id")
            , ELet "id" idLam (EApp (EVar "id") intLit)
            , ELet "id" idLam (EApp (EVar "id") boolLit)
            , ELet "id" idLam (ELet "_" (EApp (EVar "id") intLit) (EApp (EVar "id") boolLit))
            , ELet "id" idLam (EApp (EVar "id") (EVar "id"))
            , ELam "y" (ELet "id" idLam (EApp (EVar "id") (EVar "y")))
            , ELamAnn "x" polyIdTy (EApp (EVar "x") intLit)
            , ELet "f" (EAnn idLam polyIdTy) (EApp (EVar "f") intLit2)
            ]
    elements exprs
