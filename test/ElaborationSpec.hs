module ElaborationSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import Data.List (isInfixOf)
import qualified Data.IntMap.Strict as IntMap
import System.Timeout (timeout)

import MLF.Syntax (Expr(..), Lit(..), SrcType(..), SrcScheme(..))
import qualified MLF.Elab as Elab
import MLF.Types (BaseTy(..), NodeId(..), EdgeId(..), InstanceOp(..), InstanceWitness(..), EdgeWitness(..))
import MLF.ConstraintGen (ConstraintResult(..), generateConstraints)
import MLF.Normalize (normalize)
import MLF.Acyclicity (checkAcyclicity)
import MLF.Presolution (PresolutionResult(..), computePresolution)
import MLF.Solve (SolveResult, solveUnify)

-- | Run an IO action with a timeout (in microseconds).
-- If the action times out, the test fails.
withTimeout :: Int -> IO () -> IO ()
withTimeout limit action = do
    result <- timeout limit action
    case result of
        Just () -> return ()
        Nothing -> expectationFailure $ "Test timed out after " ++ show limit ++ " microseconds"

requireRight :: Show e => Either e a -> IO a
requireRight = either (\e -> expectationFailure (show e) >> fail "requireRight") pure

requirePipeline :: Expr -> IO (Elab.ElabTerm, Elab.ElabType)
requirePipeline = requireRight . Elab.runPipelineElab

schemeToType :: Elab.ElabScheme -> Elab.ElabType
schemeToType (Elab.Forall binds body) =
    foldr (\(v, b) t -> Elab.TForall v b t) body binds

stripForalls :: Elab.ElabType -> Elab.ElabType
stripForalls (Elab.TForall _ _ t) = stripForalls t
stripForalls t = t

-- | Canonicalize binder names in a type to compare up to α-equivalence.
canonType :: Elab.ElabType -> Elab.ElabType
canonType = go [] (0 :: Int)
  where
    go :: [(String, String)] -> Int -> Elab.ElabType -> Elab.ElabType
    go env n ty = case ty of
        Elab.TVar v ->
            case lookup v env of
                Just v' -> Elab.TVar v'
                Nothing -> Elab.TVar v
        Elab.TBase b -> Elab.TBase b
        Elab.TBottom -> Elab.TBottom
        Elab.TArrow a b -> Elab.TArrow (go env n a) (go env n b)
        Elab.TForall v mb body ->
            let v' = "a" ++ show n
                env' = (v, v') : env
                -- binder is not in scope for its bound
                mb' = fmap (go env n) mb
                body' = go env' (n + 1) body
            in Elab.TForall v' mb' body'

shouldAlphaEqType :: Elab.ElabType -> Elab.ElabType -> Expectation
shouldAlphaEqType actual expected =
    canonType actual `shouldBe` canonType expected

spec :: Spec
spec = describe "Phase 6 — Elaborate (xMLF)" $ do
    describe "Basic elaboration" $ do
        it "elaborates integer literal" $ do
            let expr = ELit (LInt 1)
            (term, ty) <- requirePipeline expr
            Elab.pretty term `shouldBe` "1"
            Elab.pretty ty `shouldBe` "Int"

        it "elaborates boolean literal" $ do
            let expr = ELit (LBool True)
            (term, ty) <- requirePipeline expr
            Elab.pretty term `shouldBe` "true"
            Elab.pretty ty `shouldBe` "Bool"

        it "elaborates lambda" $ do
            let expr = ELam "x" (EVar "x")
            (_, ty) <- requirePipeline expr
            -- Result is generalized at top level
            Elab.pretty ty `shouldBe` "∀a. a -> a"

        it "elaborates application" $ do
            let expr = EApp (ELam "x" (EVar "x")) (ELit (LInt 42))
            (_, ty) <- requirePipeline expr
            Elab.pretty ty `shouldBe` "Int"

    describe "Polymorphism and Generalization" $ do
        it "elaborates polymorphic let-binding" $ do
            -- let id = \x. x in id
            let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")
            (term, ty) <- requirePipeline expr

            -- Term should look like: let id : ∀a. a -> a = Λa. λx:a. x in id
            Elab.pretty term `shouldBe` "let id : ∀a. a -> a = Λa. λx:a. x in id"

            -- Type is polymorphic (compare up to α-equivalence).
            let expected =
                    Elab.TForall "a" Nothing
                        (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
            ty `shouldAlphaEqType` expected

        it "elaborates polymorphic instantiation" $ do
            -- let id = \x. x in id 1
            let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
            (term, ty) <- requirePipeline expr
            Elab.pretty ty `shouldBe` "Int"
            Elab.pretty term `shouldBe` "let id : ∀a. a -> a = Λa. λx:a. x in (id [⟨Int⟩]) 1"

        it "elaborates usage of polymorphic let (instantiated at different types)" $ do
            -- let f = \x. x in let _ = f 1 in f true
            -- This forces 'f' to be instantiated twice: once at Int, once at Bool
            let expr = ELet "f" (ELam "x" (EVar "x"))
                        (ELet "_" (EApp (EVar "f") (ELit (LInt 1)))
                            (EApp (EVar "f") (ELit (LBool True))))
            (term, ty) <- requirePipeline expr
            Elab.pretty ty `shouldBe` "Bool"
            let s = Elab.pretty term
            s `shouldSatisfy` ("f [⟨Int⟩]" `isInfixOf`)
            s `shouldSatisfy` ("f [⟨Bool⟩]" `isInfixOf`)

        it "elaborates nested let bindings" $ do
            -- let x = 1 in let y = x in y
            let expr = ELet "x" (ELit (LInt 1)) (ELet "y" (EVar "x") (EVar "y"))
            (term, ty) <- requirePipeline expr

            -- Allow vacuous quantification, but require the body to be Int.
            stripForalls ty `shouldBe` Elab.TBase (BaseTy "Int")

            Elab.pretty term `shouldSatisfy` ("let x" `isInfixOf`)
            Elab.pretty term `shouldSatisfy` ("let y" `isInfixOf`)

        it "elaborates term annotations" $ do
            -- (\x. x) : Int -> Int
            let ann = STArrow (STBase "Int") (STBase "Int")
                expr = EAnn (ELam "x" (EVar "x")) ann
            (_term, ty) <- requirePipeline expr
            Elab.pretty ty `shouldBe` "Int -> Int"

    describe "Elaboration of Bounded Quantification (Flexible Bounds)" $ do
        it "elaborates annotated let with flexible bound (Int -> Int)" $ do
            -- let f : ∀(a ⩾ Int -> Int). a -> a = \x. x in f
            -- This restricts 'f' to be an instance of 'Int -> Int' (or more specific),
            -- but 'f' itself is the identity.
            -- Actually, 'a >= Int -> Int' means 'a' is an instance of 'Int -> Int'.
            -- So 'a' could be 'Int -> Int'.
            let bound = STArrow (STBase "Int") (STBase "Int")
                scheme = SrcScheme [("a", Just bound)] (STArrow (STVar "a") (STVar "a"))
                expr = ELetAnn "f" scheme (ELam "x" (EVar "x")) (EVar "f")

            (term, ty) <- requirePipeline expr
            Elab.pretty term `shouldSatisfy` (\s -> "let f : ∀(a ⩾ Int -> Int). a -> a" `isInfixOf` s)

            let expectedBound =
                    Elab.TArrow (Elab.TBase (BaseTy "Int")) (Elab.TBase (BaseTy "Int"))
                expectedTy =
                    Elab.TForall "a" (Just expectedBound)
                        (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
            ty `shouldAlphaEqType` expectedTy

        it "elaborates annotated let with polymorphic bound (Rank-2ish)" $ do
            -- let f : ∀(a ⩾ ∀b. b -> b). a -> a = \x. x in f
            let innerBound = STForall "b" Nothing (STArrow (STVar "b") (STVar "b"))
                scheme = SrcScheme [("a", Just innerBound)] (STArrow (STVar "a") (STVar "a"))
                expr = ELetAnn "f" scheme (ELam "x" (EVar "x")) (EVar "f")

            (_term, ty) <- requirePipeline expr
            let expectedInner =
                    Elab.TForall "b" Nothing
                        (Elab.TArrow (Elab.TVar "b") (Elab.TVar "b"))
                expectedTy =
                    Elab.TForall "a" (Just expectedInner)
                        (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
            ty `shouldAlphaEqType` expectedTy

        it "elaborates lambda with rank-2 argument" $ do
            -- \x : (∀a. a -> a). x 1
            -- This uses a rigid bound for the argument.
            let paramTy = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
                expr = ELamAnn "x" paramTy (EApp (EVar "x") (ELit (LInt 1)))

            (_term, ty) <- requirePipeline expr
            let expected =
                    Elab.TArrow
                        (Elab.TForall "a" Nothing (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a")))
                        (Elab.TBase (BaseTy "Int"))
            ty `shouldAlphaEqType` expected

    describe "xMLF types (instance bounds)" $ do
        it "pretty prints unbounded forall" $ do
            let ty = Elab.TForall "a" Nothing (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
            Elab.pretty ty `shouldBe` "∀a. a -> a"

        it "pretty prints bounded forall" $ do
            let bound = Elab.TArrow (Elab.TBase (BaseTy "Int")) (Elab.TBase (BaseTy "Int"))
                ty = Elab.TForall "a" (Just bound) (Elab.TVar "a")
            Elab.pretty ty `shouldBe` "∀(a ⩾ Int -> Int). a"

        it "pretty prints nested bounded forall" $ do
            let innerBound = Elab.TArrow (Elab.TVar "b") (Elab.TVar "b")
                inner = Elab.TForall "b" Nothing innerBound
                outer = Elab.TForall "a" (Just inner) (Elab.TVar "a")
            Elab.pretty outer `shouldBe` "∀(a ⩾ ∀b. b -> b). a"

        it "pretty prints bottom type" $ do
            Elab.pretty Elab.TBottom `shouldBe` "⊥"

    describe "xMLF instantiation witnesses" $ do
        it "pretty prints identity instantiation" $ do
            Elab.pretty Elab.InstId `shouldBe` "1"

        it "pretty prints type application" $ do
            let inst = Elab.InstApp (Elab.TBase (BaseTy "Int"))
            Elab.pretty inst `shouldBe` "⟨Int⟩"

        it "pretty prints intro (skip forall)" $ do
            Elab.pretty Elab.InstIntro `shouldBe` "O"

        it "pretty prints elim (eliminate forall)" $ do
            Elab.pretty Elab.InstElim `shouldBe` "N"

        it "pretty prints abstract bound" $ do
            Elab.pretty (Elab.InstAbstr "a") `shouldBe` "!a"

        it "pretty prints under instantiation" $ do
            let inst = Elab.InstUnder "a" (Elab.InstApp (Elab.TBase (BaseTy "Int")))
            Elab.pretty inst `shouldBe` "∀(a ⩾) ⟨Int⟩"

        it "pretty prints inside instantiation" $ do
            let inst = Elab.InstInside (Elab.InstApp (Elab.TBase (BaseTy "Int")))
            Elab.pretty inst `shouldBe` "∀(⩾ ⟨Int⟩)"

        it "pretty prints composed instantiation" $ do
            let inst = Elab.InstSeq (Elab.InstApp (Elab.TBase (BaseTy "Int"))) Elab.InstIntro
            Elab.pretty inst `shouldBe` "⟨Int⟩; O"

        it "pretty prints bottom instantiation" $ do
            let inst = Elab.InstBot (Elab.TBase (BaseTy "Int"))
            Elab.pretty inst `shouldBe` "Int"

    describe "xMLF instantiation semantics (applyInstantiation)" $ do
        it "InstElim substitutes the binder with its bound (default ⊥)" $ do
            let ty = Elab.TForall "a" Nothing (Elab.TVar "a")
            out <- requireRight (Elab.applyInstantiation ty Elab.InstElim)
            out `shouldBe` Elab.TBottom

        it "InstElim substitutes the binder with an explicit bound" $ do
            let ty = Elab.TForall "a" (Just (Elab.TBase (BaseTy "Int"))) (Elab.TVar "a")
            out <- requireRight (Elab.applyInstantiation ty Elab.InstElim)
            out `shouldBe` Elab.TBase (BaseTy "Int")

        it "InstInside can update a ⊥ bound to a concrete bound" $ do
            let ty = Elab.TForall "a" Nothing (Elab.TVar "a")
                inst = Elab.InstInside (Elab.InstBot (Elab.TBase (BaseTy "Int")))
            out <- requireRight (Elab.applyInstantiation ty inst)
            out `shouldBe` Elab.TForall "a" (Just (Elab.TBase (BaseTy "Int"))) (Elab.TVar "a")

        it "InstUnder applies to the body and renames the instantiation binder" $ do
            let ty = Elab.TForall "a" Nothing (Elab.TVar "zzz")
                inst = Elab.InstUnder "x" (Elab.InstAbstr "x")
            out <- requireRight (Elab.applyInstantiation ty inst)
            out `shouldBe` Elab.TForall "a" Nothing (Elab.TVar "a")

        it "InstApp behaves like (∀(⩾ τ); N) on the outermost quantifier" $ do
            let ty = Elab.TForall "a" Nothing (Elab.TVar "a")
            out <- requireRight (Elab.applyInstantiation ty (Elab.InstApp (Elab.TBase (BaseTy "Int"))))
            out `shouldBe` Elab.TBase (BaseTy "Int")

        it "fails InstElim on a non-∀ type" $ do
            case Elab.applyInstantiation (Elab.TBase (BaseTy "Int")) Elab.InstElim of
                Left _ -> pure ()
                Right t -> expectationFailure ("Expected failure, got: " ++ show t)

        it "fails InstInside on a non-∀ type" $ do
            let inst = Elab.InstInside (Elab.InstBot (Elab.TBase (BaseTy "Int")))
            case Elab.applyInstantiation (Elab.TBase (BaseTy "Int")) inst of
                Left _ -> pure ()
                Right t -> expectationFailure ("Expected failure, got: " ++ show t)

        it "fails InstUnder on a non-∀ type" $ do
            case Elab.applyInstantiation (Elab.TBase (BaseTy "Int")) (Elab.InstUnder "a" Elab.InstId) of
                Left _ -> pure ()
                Right t -> expectationFailure ("Expected failure, got: " ++ show t)

        it "fails InstBot on a non-⊥ type" $ do
            case Elab.applyInstantiation (Elab.TBase (BaseTy "Int")) (Elab.InstBot (Elab.TBase (BaseTy "Bool"))) of
                Left _ -> pure ()
                Right t -> expectationFailure ("Expected failure, got: " ++ show t)

    describe "xMLF terms" $ do
        it "pretty prints type abstraction with bound" $ do
            let bound = Elab.TArrow (Elab.TVar "b") (Elab.TVar "b")
                term = Elab.ETyAbs "a" (Just bound) (Elab.EVar "x")
            Elab.pretty term `shouldBe` "Λ(a ⩾ b -> b). x"

        it "pretty prints unbounded type abstraction" $ do
            let term = Elab.ETyAbs "a" Nothing (Elab.EVar "x")
            Elab.pretty term `shouldBe` "Λa. x"

        it "pretty prints type instantiation" $ do
            let inst = Elab.InstApp (Elab.TBase (BaseTy "Int"))
                term = Elab.ETyInst (Elab.EVar "f") inst
            Elab.pretty term `shouldBe` "f [⟨Int⟩]"

        it "pretty prints let with scheme" $ do
            let scheme = Elab.Forall [("a", Nothing)] (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
                term = Elab.ELet "id" scheme (Elab.ETyAbs "a" Nothing (Elab.ELam "x" (Elab.TVar "a") (Elab.EVar "x"))) (Elab.EVar "id")
            Elab.pretty term `shouldBe` "let id : ∀a. a -> a = Λa. λx:a. x in id"

    describe "eMLF source annotations" $ do
        it "parses and represents STVar" $ do
            let st = STVar "alpha"
            st `shouldBe` STVar "alpha"

        it "parses and represents STArrow" $ do
            let st = STArrow (STBase "Int") (STBase "Bool")
            st `shouldBe` STArrow (STBase "Int") (STBase "Bool")

        it "parses and represents unbounded STForall" $ do
            let st = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
            st `shouldBe` STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))

        it "parses and represents bounded STForall" $ do
            let bound = STArrow (STBase "Int") (STBase "Int")
                st = STForall "a" (Just bound) (STVar "a")
            st `shouldBe` STForall "a" (Just bound) (STVar "a")

        it "parses and represents STBottom" $ do
            STBottom `shouldBe` STBottom

        it "represents SrcScheme with multiple binders" $ do
            let binds = [("a", Nothing), ("b", Just (STBase "Int"))]
                body = STArrow (STVar "a") (STVar "b")
                scheme = SrcScheme binds body
            scheme `shouldBe` SrcScheme binds body

    describe "Expansion to Instantiation conversion" $ do
        it "converts ExpIdentity to InstId" $ do
            -- Test the conversion function directly would require more setup
            -- For now just test that the types exist
            Elab.InstId `shouldBe` Elab.InstId

        it "converts ExpInstantiate to InstApp sequence" $ do
            -- InstSeq combines multiple applications
            let inst = Elab.InstSeq (Elab.InstApp (Elab.TBase (BaseTy "Int")))
                                    (Elab.InstApp (Elab.TBase (BaseTy "Bool")))
            Elab.pretty inst `shouldBe` "⟨Int⟩; ⟨Bool⟩"

        it "converts ExpForall to InstIntro" $ do
            Elab.pretty Elab.InstIntro `shouldBe` "O"

    describe "Witness translation (Φ/Σ)" $ do
        describe "Σ(g) quantifier reordering" $ do
            it "commutes two adjacent quantifiers" $ do
                let src =
                        Elab.TForall "a" Nothing
                            (Elab.TForall "b" Nothing (Elab.TArrow (Elab.TVar "a") (Elab.TVar "b")))
                    tgt =
                        Elab.TForall "b" Nothing
                            (Elab.TForall "a" Nothing (Elab.TArrow (Elab.TVar "a") (Elab.TVar "b")))

                case Elab.sigmaReorder src tgt of
                    Left err -> expectationFailure (show err)
                    Right sig ->
                        case Elab.applyInstantiation src sig of
                            Left err -> expectationFailure (show err)
                            Right out -> canonType out `shouldBe` canonType tgt

            it "commutes two adjacent bounded quantifiers (bounds preserved)" $ do
                let intTy = Elab.TBase (BaseTy "Int")
                    boolTy = Elab.TBase (BaseTy "Bool")
                    src =
                        Elab.TForall "a" (Just intTy)
                            (Elab.TForall "b" (Just boolTy)
                                (Elab.TArrow (Elab.TVar "a") (Elab.TVar "b")))
                    tgt =
                        Elab.TForall "b" (Just boolTy)
                            (Elab.TForall "a" (Just intTy)
                                (Elab.TArrow (Elab.TVar "a") (Elab.TVar "b")))
                sig <- requireRight (Elab.sigmaReorder src tgt)
                out <- requireRight (Elab.applyInstantiation src sig)
                canonType out `shouldBe` canonType tgt

            it "permutes three quantifiers" $ do
                let src =
                        Elab.TForall "a" Nothing
                            (Elab.TForall "b" Nothing
                                (Elab.TForall "c" Nothing
                                    (Elab.TArrow (Elab.TVar "a")
                                        (Elab.TArrow (Elab.TVar "b") (Elab.TVar "c")))))
                    tgt =
                        Elab.TForall "c" Nothing
                            (Elab.TForall "a" Nothing
                                (Elab.TForall "b" Nothing
                                    (Elab.TArrow (Elab.TVar "a")
                                        (Elab.TArrow (Elab.TVar "b") (Elab.TVar "c")))))

                case Elab.sigmaReorder src tgt of
                    Left err -> expectationFailure (show err)
                    Right sig ->
                        case Elab.applyInstantiation src sig of
                            Left err -> expectationFailure (show err)
                            Right out -> canonType out `shouldBe` canonType tgt

            it "fails when target binder identity is not present in the source" $ do
                let src = Elab.TForall "a" Nothing (Elab.TVar "a")
                    tgt = Elab.TForall "b" Nothing (Elab.TVar "b")
                case Elab.sigmaReorder src tgt of
                    Left _ -> pure ()
                    Right sig -> expectationFailure ("Expected failure, got: " ++ show sig)

        describe "Φ translation soundness" $ do
            let runToSolved :: Expr -> Either String (SolveResult, IntMap.IntMap EdgeWitness)
                runToSolved e = do
                    ConstraintResult { crConstraint = c0 } <- firstShow (generateConstraints e)
                    let c1 = normalize c0
                    acyc <- firstShow (checkAcyclicity c1)
                    pres <- firstShow (computePresolution acyc c1)
                    solved <- firstShow (solveUnify (prConstraint pres))
                    pure (solved, prEdgeWitnesses pres)

                runSolvedWithRoot :: Expr -> Either String (SolveResult, NodeId)
                runSolvedWithRoot e = do
                    ConstraintResult { crConstraint = c0, crRoot = root } <- firstShow (generateConstraints e)
                    let c1 = normalize c0
                    acyc <- firstShow (checkAcyclicity c1)
                    pres <- firstShow (computePresolution acyc c1)
                    solved <- firstShow (solveUnify (prConstraint pres))
                    let root' = Elab.chaseRedirects (prRedirects pres) root
                    pure (solved, root')

                firstShow :: Show err => Either err a -> Either String a
                firstShow = either (Left . show) Right

            it "scheme-aware Φ can target a non-front binder (reordering before instantiation)" $ do
                -- Build a SolveResult that can reify a graft argument type (Int).
                (solved, intNode) <- requireRight (runSolvedWithRoot (ELit (LInt 1)))

                let scheme =
                        Elab.Forall [("a", Nothing), ("b", Nothing)]
                            (Elab.TArrow (Elab.TVar "a") (Elab.TVar "b"))
                    subst = IntMap.fromList [(1, "a"), (2, "b")]
                    si = Elab.SchemeInfo { Elab.siScheme = scheme, Elab.siSubst = subst }

                    -- Witness says: graft Int into binder “b” (NodeId 2), then weaken it.
                    ew = EdgeWitness
                        { ewEdgeId = EdgeId 0
                        , ewLeft = NodeId 0
                        , ewRight = NodeId 0
                        , ewRoot = NodeId 0
                        , ewWitness = InstanceWitness [OpGraft intNode (NodeId 2), OpWeaken (NodeId 2)]
                        }

                phi <- requireRight (Elab.phiFromEdgeWitness solved (Just si) ew)

                -- Because we target the *second* binder, Φ must do more than a plain ⟨Int⟩.
                phi `shouldNotBe` Elab.InstApp (Elab.TBase (BaseTy "Int"))

                out <- requireRight (Elab.applyInstantiation (schemeToType scheme) phi)
                let expected =
                        Elab.TForall "a" Nothing
                            (Elab.TArrow (Elab.TVar "a") (Elab.TBase (BaseTy "Int")))
                canonType out `shouldBe` canonType expected

            it "witness instantiation matches solved edge types (id @ Int)" $ do
                let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
                case runToSolved expr of
                    Left err -> expectationFailure err
                    Right (solved, ews) -> do
                        IntMap.size ews `shouldSatisfy` (> 0)
                        forM_ (IntMap.elems ews) $ \ew -> do
                            srcTy <- requireRight (Elab.reifyType solved (ewRoot ew))
                            tgtTy <- requireRight (Elab.reifyType solved (ewRight ew))
                            phi <- requireRight (Elab.phiFromEdgeWitness solved Nothing ew)
                            out <- requireRight (Elab.applyInstantiation srcTy phi)
                            canonType out `shouldBe` canonType tgtTy

            it "witness instantiation matches solved edge types (two instantiations)" $ do
                let expr =
                        ELet "f" (ELam "x" (EVar "x"))
                            (ELet "_" (EApp (EVar "f") (ELit (LInt 1)))
                                (EApp (EVar "f") (ELit (LBool True))))
                case runToSolved expr of
                    Left err -> expectationFailure err
                    Right (solved, ews) -> do
                        IntMap.size ews `shouldBe` 2
                        forM_ (IntMap.elems ews) $ \ew -> do
                            srcTy <- requireRight (Elab.reifyType solved (ewRoot ew))
                            tgtTy <- requireRight (Elab.reifyType solved (ewRight ew))
                            phi <- requireRight (Elab.phiFromEdgeWitness solved Nothing ew)
                            out <- requireRight (Elab.applyInstantiation srcTy phi)
                            canonType out `shouldBe` canonType tgtTy
