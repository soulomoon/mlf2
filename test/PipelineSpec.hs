{-# LANGUAGE PatternSynonyms #-}
module PipelineSpec (spec) where

import Data.List (isInfixOf)
import Data.Bifunctor (first)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Test.Hspec

import MLF.Elab.Pipeline
    ( generalizeAtWithBuilder
    , applyRedirectsToAnn
    , renderPipelineError
    , runPipelineElab
    , pattern Forall
    , Pretty(..)
    )
import MLF.Frontend.Syntax
import MLF.Frontend.ConstraintGen
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

spec :: Spec
spec = describe "Pipeline (Phases 1-5)" $ do
    describe "Elaboration helpers" $ do
        it "reifies type with flexible bound" $ do
            -- let f : ∀(a ⩾ Int). a -> a = \x. x
            let ann = STForall "a" (Just (STBase "Int")) (STArrow (STVar "a") (STVar "a"))
                expr =
                    let schemeTy = mkForalls [] ann
                    in ELet "f" (EAnn (ELam "x" (EVar "x")) schemeTy) (EVar "f")

            case runPipeline expr of
                Right (res, root) -> do
                    -- The root should be f's type: ∀(a ⩾ Int). a -> a
                    let scopeRoot =
                            case Binding.bindingRoots (srConstraint res) of
                                [GenRef gid] -> genRef gid
                                roots -> error ("PipelineSpec: unexpected binding roots " ++ show roots)
                    let generalizeAt = generalizeAtWithBuilder (defaultPlanBuilder defaultTraceConfig) Nothing
                    case generalizeAt res scopeRoot root of
                        Right (Forall binds ty, _subst) -> do
                            binds `shouldBe` []
                            let tyStr = pretty ty
                            tyStr `shouldSatisfy` ("∀(" `isInfixOf`)
                            tyStr `shouldSatisfy` ("⩾ Int" `isInfixOf`)
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
                    ConstraintResult{ crConstraint = c0 } <- first show (generateConstraints defaultPolySyms expr)
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
        -- US-003: thesis-exact Φ now rejects OpRaise outside I(r) instead of skipping
        pendingWith "US-003: thesis-exact Φ rejects OpRaise outside interior; witness generation needs update"
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
        -- US-003: thesis-exact Φ now rejects OpRaise outside I(r) instead of skipping
        pendingWith "US-003: thesis-exact Φ rejects OpRaise outside interior; witness generation needs update"
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

-- Helpers
runPipelineWithInternals :: Expr -> Either String (SolveResult, AnnExpr)
runPipelineWithInternals expr = do
    ConstraintResult{ crConstraint = c0, crRoot = _root, crAnnotated = ann } <-
        first show (generateConstraints defaultPolySyms expr)
    let c1 = normalize c0
    acyc <- first show (checkAcyclicity c1)
    pres <- first show (computePresolution defaultTraceConfig acyc c1)
    res <- first show (solveUnify defaultTraceConfig (prConstraint pres))
    case validateSolvedGraphStrict res of
        [] -> do
            let ann' = applyRedirectsToAnn (prRedirects pres) ann
            Right (res, ann')
        vs -> Left ("validateSolvedGraph failed:\n" ++ unlines vs)

runPipelineWithPresolution :: Expr -> Either String (PresolutionResult, AnnExpr)
runPipelineWithPresolution expr = do
    ConstraintResult{ crConstraint = c0, crAnnotated = ann } <-
        first show (generateConstraints defaultPolySyms expr)
    let c1 = normalize c0
    acyc <- first show (checkAcyclicity c1)
    pres <- first show (computePresolution defaultTraceConfig acyc c1)
    pure (pres, ann)

runPipeline :: Expr -> Either String (SolveResult, NodeId)
runPipeline expr = do
    ConstraintResult{ crConstraint = c0, crRoot = root, crAnnotated = _ann } <-
        first show (generateConstraints defaultPolySyms expr)
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
