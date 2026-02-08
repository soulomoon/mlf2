{-# LANGUAGE PatternSynonyms #-}
module PipelineSpec (spec) where

import Control.Monad (unless)
import Data.List (isInfixOf)
import Data.Bifunctor (first)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck (Gen, arbitrary, chooseInt, elements, forAll, property, withMaxSuccess)

import MLF.Elab.Pipeline
    ( ElabType
    , generalizeAtWithBuilder
    , applyRedirectsToAnn
    , renderPipelineError
    , runPipelineElab
    , runPipelineElabChecked
    , typeCheck
    , pattern Forall
    , Pretty(..)
    )
import MLF.Frontend.Syntax
import MLF.Frontend.ConstraintGen
import MLF.Frontend.Normalize (normalizeExpr)
import MLF.Constraint.Normalize
import MLF.Constraint.Acyclicity
import MLF.Constraint.Canonicalizer (canonicalizeNode)
import MLF.Constraint.Presolution
import MLF.Constraint.Solve
import MLF.Constraint.Types.Graph
import qualified MLF.Binding.Tree as Binding
import MLF.Elab.Run.Provenance (buildTraceCopyMap, collectBaseNamedKeys)
import MLF.Elab.Run.Util (makeCanonicalizer)
import SpecUtil (collectVarNodes, defaultTraceConfig, mkForalls)
import MLF.Types.Elab (Ty(..))
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
            let ann = STForall "a" (Just (STBase "Int")) (STArrow (STVar "a") (STVar "a"))
                expr =
                    let schemeTy = mkForalls [] ann
                    in ELet "f" (EAnn (ELam "x" (EVar "x")) schemeTy) (EVar "f")

            case runPipeline expr of
                Right (res, root) -> do
                    -- With coercion-only semantics, f's type is inferred (not declared)
                    -- The coercion ensures the RHS has the annotated type.
                    let scopeRoot =
                            case Binding.bindingRoots (srConstraint res) of
                                [GenRef gid] -> genRef gid
                                roots -> error ("PipelineSpec: unexpected binding roots " ++ show roots)
                    let generalizeAt = generalizeAtWithBuilder (defaultPlanBuilder defaultTraceConfig) Nothing
                    case generalizeAt res scopeRoot root of
                        Right (Forall binds ty, _subst) -> do
                            binds `shouldBe` []
                            let tyStr = pretty ty
                            -- With coercion-only: type is inferred, not the declared scheme
                            tyStr `shouldSatisfy` ("∀" `isInfixOf`)
                            tyStr `shouldSatisfy` ("->" `isInfixOf`)
                        Left err -> expectationFailure $ "Generalize error: " ++ show err
                Left err -> expectationFailure err

        it "generalizes at binding site" $ do
             -- let id = \x. x in id
             let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")

             -- We intercept the pipeline after solve to test generalizeAt on the 'id' binding
             case runPipelineWithInternals expr of
                 Right (res, ann) -> do
                     case ann of
                         ALet _ schemeGen schemeRoot _ _ _ _ _ -> do
                             let generalizeAt = generalizeAtWithBuilder (defaultPlanBuilder defaultTraceConfig) Nothing
                             case generalizeAt res (genRef schemeGen) schemeRoot of
                                 Right scheme -> show scheme `shouldSatisfy` ("Forall" `isInfixOf`)
                                 Left err -> expectationFailure $ "Generalize error: " ++ show err
                         _ -> expectationFailure "Expected ALet annotation"
                 Left err -> expectationFailure err

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
            case runPipeline expr of
                Left err -> expectationFailure err
                Right (res, _root) -> do
                    validateStrict res
                    let nodes = cNodes (srConstraint res)
                    baseNames nodes `shouldContain` [BaseTy "Bool"]
                    noExpNodes nodes

        it "instantiates let-polymorphic id at Int and Bool" $ do
            -- let id = \x. x in let a = id 1 in id True
            let expr =
                    ELet "id" (ELam "x" (EVar "x"))
                        (ELet "a" (EApp (EVar "id") (ELit (LInt 1)))
                            (EApp (EVar "id") (ELit (LBool True))))
            case runPipeline expr of
                Left err -> expectationFailure err
                Right (res, _root) -> do
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
            let pipelineParts = do
                    ConstraintResult{ crConstraint = c0 } <- first show (generateConstraints defaultPolySyms (unsafeNormalize expr))
                    let c1 = normalize c0
                    acyc <- first show (checkAcyclicity c1)
                    pres <- first show (computePresolution defaultTraceConfig acyc c1)
                    solved <- first show (solveUnify defaultTraceConfig (prConstraint pres))
                    pure (c1, pres, solved)
            case pipelineParts of
                Left err -> expectationFailure err
                Right (c1, pres, solved) ->
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

    it "handles higher-order polymorphic apply used twice" $ do
        -- let apply f x = f x; let id = \y. y; let a = apply id 1 in apply id True
        let expr =
                ELet "apply" (ELam "f" (ELam "x" (EApp (EVar "f") (EVar "x"))))
                    (ELet "id" (ELam "y" (EVar "y"))
                        (ELet "a" (EApp (EApp (EVar "apply") (EVar "id")) (ELit (LInt 1)))
                            (EApp (EApp (EVar "apply") (EVar "id")) (ELit (LBool True)))))
        case runPipeline expr of
            Left err -> expectationFailure err
            Right (res, _root) -> do
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
        case runPipelineWithPresolution expr of
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

    it "generalizes reused constructors via make const" $ do
        -- let make x = (\z -> x) in let c1 = make 2 in let c2 = make False in c1 True
        let expr =
                ELet "make" (ELam "x" (ELam "z" (EVar "x")))
                    (ELet "c1" (EApp (EVar "make") (ELit (LInt 2)))
                        (ELet "c2" (EApp (EVar "make") (ELit (LBool False)))
                            (EApp (EVar "c1") (ELit (LBool True)))))
        case runPipeline expr of
            Left err -> expectationFailure err
            Right (res, _root) -> do
                validateStrict res
                let c = srConstraint res
                    nodes = cNodes c
                baseNames nodes `shouldContain` [BaseTy "Int"]
                baseNames nodes `shouldContain` [BaseTy "Bool"]
                noExpNodes nodes
                cInstEdges c `shouldBe` []
                cUnifyEdges c `shouldBe` []

    it "composes instantiate then forall when rebound at new level" $ do
        -- let id = \x -> x in let rebound = (let alias = id in alias) in let _ = rebound 1 in rebound True
        let expr =
                ELet "id" (ELam "x" (EVar "x")) $
                    ELet "rebound" (ELet "alias" (EVar "id") (EVar "alias")) $
                        ELet "_" (EApp (EVar "rebound") (ELit (LInt 1)))
                            (EApp (EVar "rebound") (ELit (LBool True)))
        case runPipeline expr of
            Left err -> expectationFailure err
            Right (res, _root) -> do
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
        case runPipeline expr of
            Left err -> expectationFailure err
            Right (res, _root) -> do
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
        case runPipeline expr of
            Left err -> expectationFailure err
            Right (res, _root) -> do
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
                    case runPipelineElab Set.empty (unsafeNormalize expr) of
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

                            case runPipelineElabChecked Set.empty (unsafeNormalize expr) of
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

-- | Normalize a surface expression, failing on normalization error.
unsafeNormalize :: SurfaceExpr -> NormSurfaceExpr
unsafeNormalize expr =
    case normalizeExpr expr of
        Left err -> error ("normalizeExpr failed in test: " ++ show err)
        Right normExpr -> normExpr

-- Helpers
runPipelineWithInternals :: SurfaceExpr -> Either String (SolveResult, AnnExpr)
runPipelineWithInternals expr = do
    ConstraintResult{ crConstraint = c0, crRoot = _root, crAnnotated = ann } <-
        first show (generateConstraints defaultPolySyms (unsafeNormalize expr))
    let c1 = normalize c0
    acyc <- first show (checkAcyclicity c1)
    pres <- first show (computePresolution defaultTraceConfig acyc c1)
    res <- first show (solveUnify defaultTraceConfig (prConstraint pres))
    case validateSolvedGraphStrict res of
        [] -> do
            let ann' = applyRedirectsToAnn (prRedirects pres) ann
            Right (res, ann')
        vs -> Left ("validateSolvedGraph failed:\n" ++ unlines vs)

runPipelineWithPresolution :: SurfaceExpr -> Either String (PresolutionResult, AnnExpr)
runPipelineWithPresolution expr = do
    ConstraintResult{ crConstraint = c0, crAnnotated = ann } <-
        first show (generateConstraints defaultPolySyms (unsafeNormalize expr))
    let c1 = normalize c0
    acyc <- first show (checkAcyclicity c1)
    pres <- first show (computePresolution defaultTraceConfig acyc c1)
    pure (pres, ann)

runPipeline :: SurfaceExpr -> Either String (SolveResult, NodeId)
runPipeline expr = do
    ConstraintResult{ crConstraint = c0, crRoot = root, crAnnotated = _ann } <-
        first show (generateConstraints defaultPolySyms (unsafeNormalize expr))
    let c1 = normalize c0
    acyc <- first show (checkAcyclicity c1)
    PresolutionResult{ prConstraint = c4, prRedirects = redirects } <- first show (computePresolution defaultTraceConfig acyc c1)
    res <- first show (solveUnify defaultTraceConfig c4)
    -- Apply redirects (from Presolution) then canonicalize (from Solve)
    let rootRedirected = chaseRedirects redirects root
        root' = canonical (srUnionFind res) rootRedirected
    case validateSolvedGraphStrict res of
        [] -> Right (res, root')
        vs -> Left ("validateSolvedGraph failed:\n" ++ unlines vs)

canonical :: IntMap.IntMap NodeId -> NodeId -> NodeId
canonical uf nid =
    case IntMap.lookup (getNodeId nid) uf of
        Nothing -> nid
        Just p -> canonical uf p

chaseRedirects :: IntMap.IntMap NodeId -> NodeId -> NodeId
chaseRedirects redirects nid = case IntMap.lookup (getNodeId nid) redirects of
    Just n' -> if n' == nid then nid else chaseRedirects redirects n'
    Nothing -> nid


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

-- | Empty constraint for testing reification
emptyConstraint :: Constraint
emptyConstraint = Constraint
    { cNodes = fromListNode []
    , cInstEdges = []
    , cUnifyEdges = []
    , cBindParents = IntMap.empty
    , cPolySyms = Set.empty
    , cEliminatedVars = IntSet.empty
    , cWeakenedVars = IntSet.empty
    , cAnnEdges = IntSet.empty
    , cLetEdges = IntSet.empty
    , cGenNodes = fromListGen []
    }
