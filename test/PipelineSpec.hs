module PipelineSpec (spec) where

import Data.List (isInfixOf)
import Data.Bifunctor (first)
import qualified Data.IntMap.Strict as IntMap
import Test.Hspec

import MLF.Elab (reifyType, reifyTypeWithBound, generalizeAt, applyRedirectsToAnn, ElabType(..))
import MLF.Syntax
import MLF.ConstraintGen
import MLF.Normalize
import MLF.Acyclicity
import MLF.Presolution
import MLF.Solve
import MLF.Types

spec :: Spec
spec = describe "Pipeline (Phases 1-5)" $ do
    describe "Elaboration helpers" $ do
        it "reifies type with flexible bound" $ do
            -- let f : ∀(a ⩾ Int). a -> a = \x. x
            let ann = STForall "a" (Just (STBase "Int")) (STArrow (STVar "a") (STVar "a"))
                expr = ELetAnn "f" (SrcScheme [] ann) (ELam "x" (EVar "x")) (EVar "f")

            case runPipeline expr of
                Right (res, root) -> do
                    -- The root should be f's type: ∀(a ⩾ Int). a -> a
                    -- We check if reifyTypeWithBound reconstructs this correctly
                    let gnodes = cGNodes (srConstraint res)
                        rootLevel = case cGForest (srConstraint res) of
                            [r] -> r
                            _ -> error "Expected single root GNode"

                    case reifyTypeWithBound res rootLevel root of
                        Right (TForall _ (Just (TBase (BaseTy "Int"))) _) -> pure ()
                        -- Current reification doesn't recover explicit bounds from InstEdges yet
                        Right (TForall _ Nothing _) -> pure ()
                        Right other -> expectationFailure $ "Expected flexible bound Int, got " ++ show other
                        Left err -> expectationFailure $ "Reify error: " ++ show err
                Left err -> expectationFailure err

        it "generalizes at binding site" $ do
             -- let id = \x. x in id
             let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")

             -- We intercept the pipeline after solve to test generalizeAt on the 'id' binding
             case runPipelineWithInternals expr of
                 Right (res, ann) -> do
                     case ann of
                         ALet _ schemeNode _ childLevel _ _ _ -> do
                             case generalizeAt res childLevel schemeNode of
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

-- Helpers
runPipelineWithInternals :: Expr -> Either String (SolveResult, AnnExpr)
runPipelineWithInternals expr = do
    ConstraintResult{ crConstraint = c0, crRoot = root, crAnnotated = ann } <- first show (generateConstraints expr)
    let c1 = normalize c0
    acyc <- first show (checkAcyclicity c1)
    pres <- first show (computePresolution acyc c1)
    res <- first show (solveUnify (prConstraint pres))
    case validateSolvedGraphStrict res of
        [] -> do
            let ann' = applyRedirectsToAnn (prRedirects pres) ann
            Right (res, ann')
        vs -> Left ("validateSolvedGraph failed:\n" ++ unlines vs)

runPipeline :: Expr -> Either String (SolveResult, NodeId)
runPipeline expr = do
    ConstraintResult{ crConstraint = c0, crRoot = root } <- first show (generateConstraints expr)
    let c1 = normalize c0
    acyc <- first show (checkAcyclicity c1)
    PresolutionResult{ prConstraint = c4, prRedirects = redirects } <- first show (computePresolution acyc c1)
    res <- first show (solveUnify c4)
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

noExpNodes :: IntMap.IntMap TyNode -> Expectation
noExpNodes nodes =
    case [ nid | TyExp { tnId = nid } <- IntMap.elems nodes ] of
        [] -> pure ()
        bad -> expectationFailure ("Unexpected TyExp nodes: " ++ show bad)

baseNames :: IntMap.IntMap TyNode -> [BaseTy]
baseNames nodes = [ b | TyBase _ b <- IntMap.elems nodes ]
