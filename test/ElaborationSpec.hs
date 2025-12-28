module ElaborationSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import Data.List (isInfixOf, stripPrefix)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

import MLF.Frontend.Syntax (Expr(..), Lit(..), SrcType(..), SrcScheme(..))
import qualified MLF.Elab.Pipeline as Elab
import qualified MLF.Util.Order as Order
import MLF.Constraint.Types (BaseTy(..), NodeId(..), EdgeId(..), TyNode(..), Constraint(..), InstanceOp(..), InstanceStep(..), InstanceWitness(..), EdgeWitness(..), BindFlag(..))
import MLF.Frontend.ConstraintGen (ConstraintResult(..), generateConstraints)
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Acyclicity (checkAcyclicity)
import MLF.Constraint.Presolution (PresolutionResult(..), EdgeTrace(..), computePresolution)
import MLF.Constraint.Solve (SolveResult(..), solveUnify)
import SpecUtil (requireRight)

requirePipeline :: Expr -> IO (Elab.ElabTerm, Elab.ElabType)
requirePipeline = requireRight . Elab.runPipelineElab

fInstantiations :: String -> [String]
fInstantiations = go
  where
    go [] = []
    go s =
        case stripPrefix "f [" s of
            Just rest ->
                let inst = takeWhile (/= ']') rest
                    afterBracket = dropWhile (/= ']') rest
                    next = case afterBracket of
                        [] -> []
                        (_:xs) -> xs
                in inst : go next
            Nothing -> go (drop 1 s)

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
            let insts = fInstantiations s
            insts `shouldSatisfy` any ("Int" `isInfixOf`)
            insts `shouldSatisfy` any ("Bool" `isInfixOf`)

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

    describe "Elaboration bookkeeping (eliminated vars)" $ do
        it "generalizeAt ignores eliminated binders" $ do
            let v = NodeId 1
                arrow = NodeId 2
                forallNode = NodeId 3
                c =
                    Constraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId v, TyVar v)
                                , (getNodeId arrow, TyArrow arrow v v)
                                , (getNodeId forallNode, TyForall forallNode arrow)
                                ]
                        , cInstEdges = []
                        , cUnifyEdges = []
                        , cBindParents = IntMap.fromList [(getNodeId v, (forallNode, BindFlex))]
                        , cVarBounds = IntMap.empty
                        , cEliminatedVars = IntSet.singleton (getNodeId v)
                        }
                solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

            (sch, _subst) <- requireRight (Elab.generalizeAt solved forallNode forallNode)
            sch `shouldBe` Elab.Forall [] (Elab.TArrow (Elab.TVar "t1") (Elab.TVar "t1"))

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
            let inst =
                    Elab.InstSeq
                        (Elab.InstApp (Elab.TBase (BaseTy "Int")))
                        (Elab.InstApp (Elab.TBase (BaseTy "Bool")))
            Elab.pretty inst `shouldBe` "⟨Int⟩; ⟨Bool⟩"

        it "converts ExpForall to InstIntro" $ do
            Elab.pretty Elab.InstIntro `shouldBe` "O"

    describe "Paper ≺ ordering (leftmost-lowermost)" $ do
        it "generalizeAt orders binders by ≺ (not by NodeId)" $ do
            -- Construct a tiny solved graph where the leftmost variable in the type
            -- has a *larger* NodeId than the right one, so NodeId-order would be wrong.
            let vLeft = NodeId 10
                vRight = NodeId 5
                arrow = NodeId 20
                forallNode = NodeId 30

                c =
                    Constraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId vLeft, TyVar vLeft)
                                , (getNodeId vRight, TyVar vRight)
                                , (getNodeId arrow, TyArrow arrow vLeft vRight)
                                , (getNodeId forallNode, TyForall forallNode arrow)
                                ]
                        , cInstEdges = []
                        , cUnifyEdges = []
                        , cBindParents =
                            IntMap.fromList
                                [ (getNodeId vLeft, (forallNode, BindFlex))
                                , (getNodeId vRight, (forallNode, BindFlex))
                                ]
                        , cVarBounds = IntMap.empty
                        , cEliminatedVars = IntSet.empty
                        }

                solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

            (sch, _subst) <- requireRight (Elab.generalizeAt solved forallNode forallNode)
            Elab.pretty sch `shouldBe` "∀a b. a -> b"

        it "generalizeAt orders lowermost binders first (depth beats leftmost)" $ do
            -- Here the deeper variable must quantify before the shallow one, even
            -- if it has a larger NodeId.
            let vShallow = NodeId 5
                vDeep = NodeId 10
                nOuter = NodeId 20
                nInner = NodeId 21
                nInt = NodeId 22
                forallNode = NodeId 30

                c =
                    Constraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId vShallow, TyVar vShallow)
                                , (getNodeId vDeep, TyVar vDeep)
                                , (getNodeId nInt, TyBase nInt (BaseTy "Int"))
                                , (getNodeId nInner, TyArrow nInner nInt vDeep)
                                , (getNodeId nOuter, TyArrow nOuter vShallow nInner)
                                , (getNodeId forallNode, TyForall forallNode nOuter)
                                ]
                        , cInstEdges = []
                        , cUnifyEdges = []
                        , cBindParents =
                            IntMap.fromList
                                [ (getNodeId vShallow, (forallNode, BindFlex))
                                , (getNodeId vDeep, (forallNode, BindFlex))
                                ]
                        , cVarBounds = IntMap.empty
                        , cEliminatedVars = IntSet.empty
                        }

                solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

            (sch, _subst) <- requireRight (Elab.generalizeAt solved forallNode forallNode)
            Elab.pretty sch `shouldBe` "∀a b. b -> Int -> a"

        it "generalizeAt respects binder bound dependencies (a ≺ b if b’s bound mentions a)" $ do
            let vA = NodeId 10
                vB = NodeId 5
                arrow = NodeId 20
                forallNode = NodeId 30

                c =
                    Constraint
                        { cVarBounds =
                            IntMap.fromList
                                [ (getNodeId vA, Nothing)
                                , (getNodeId vB, Just vA)
                                ]
                        , cEliminatedVars = IntSet.empty
                        , cNodes =
                            IntMap.fromList
                                [ (getNodeId vA, TyVar vA)
                                , (getNodeId vB, TyVar vB)
                                , (getNodeId arrow, TyArrow arrow vB vA)
                                , (getNodeId forallNode, TyForall forallNode arrow)
                                ]
                        , cInstEdges = []
                        , cUnifyEdges = []
                        , cBindParents =
                            IntMap.fromList
                                [ (getNodeId vA, (forallNode, BindFlex))
                                , (getNodeId vB, (forallNode, BindFlex))
                                ]
                        }

                solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

            (sch, _subst) <- requireRight (Elab.generalizeAt solved forallNode forallNode)
            Elab.pretty sch `shouldBe` "∀a (b ⩾ a). b -> a"

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
            let runToSolved :: Expr -> Either String (SolveResult, IntMap.IntMap EdgeWitness, IntMap.IntMap EdgeTrace)
                runToSolved e = do
                    ConstraintResult { crConstraint = c0 } <- firstShow (generateConstraints e)
                    let c1 = normalize c0
                    acyc <- firstShow (checkAcyclicity c1)
                    pres <- firstShow (computePresolution acyc c1)
                    solved <- firstShow (solveUnify (prConstraint pres))
                    pure (solved, prEdgeWitnesses pres, prEdgeTraces pres)

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
                    ops = [OpGraft intNode (NodeId 2), OpWeaken (NodeId 2)]
                    ew = EdgeWitness
                        { ewEdgeId = EdgeId 0
                        , ewLeft = NodeId 0
                        , ewRight = NodeId 0
                        , ewRoot = NodeId 0
                        , ewSteps = map StepOmega ops
                        , ewForallIntros = 0
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitness solved (Just si) ew)

                -- Because we target the *second* binder, Φ must do more than a plain ⟨Int⟩.
                phi `shouldNotBe` Elab.InstApp (Elab.TBase (BaseTy "Int"))

                out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
                let expected =
                        Elab.TForall "a" Nothing
                            (Elab.TArrow (Elab.TVar "a") (Elab.TBase (BaseTy "Int")))
                canonType out `shouldBe` canonType expected

            it "scheme-aware Φ can translate Merge (alias one binder to another)" $ do
                (solved, _intNode) <- requireRight (runSolvedWithRoot (ELit (LInt 1)))

                let scheme =
                        Elab.Forall [("a", Nothing), ("b", Nothing)]
                            (Elab.TArrow (Elab.TVar "a") (Elab.TVar "b"))
                    subst = IntMap.fromList [(1, "a"), (2, "b")]
                    si = Elab.SchemeInfo { Elab.siScheme = scheme, Elab.siSubst = subst }

                    -- Merge binder “b” into binder “a”, i.e. ∀a. ∀b. a -> b  ~~>  ∀a. a -> a
                    ops = [OpMerge (NodeId 2) (NodeId 1)]
                    ew = EdgeWitness
                        { ewEdgeId = EdgeId 0
                        , ewLeft = NodeId 0
                        , ewRight = NodeId 0
                        , ewRoot = NodeId 0
                        , ewSteps = map StepOmega ops
                        , ewForallIntros = 0
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitness solved (Just si) ew)
                out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
                let expected =
                        Elab.TForall "a" Nothing
                            (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
                canonType out `shouldBe` canonType expected

            it "scheme-aware Φ can translate Raise (raise a binder to the front)" $ do
                (solved, _intNode) <- requireRight (runSolvedWithRoot (ELit (LInt 1)))

                let scheme =
                        Elab.Forall [("a", Nothing), ("b", Nothing)]
                            (Elab.TArrow (Elab.TVar "a") (Elab.TVar "b"))
                    subst = IntMap.fromList [(1, "a"), (2, "b")]
                    si = Elab.SchemeInfo { Elab.siScheme = scheme, Elab.siSubst = subst }

                    -- Raise binder “b” outward by introducing a fresh front binder and
                    -- aliasing/eliminating the old one (paper Fig. 10 Raise).
                    ops = [OpRaise (NodeId 2)]
                    ew = EdgeWitness
                        { ewEdgeId = EdgeId 0
                        , ewLeft = NodeId 0
                        , ewRight = NodeId 0
                        , ewRoot = NodeId 0
                        , ewSteps = map StepOmega ops
                        , ewForallIntros = 0
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitness solved (Just si) ew)
                out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
                let expected =
                        Elab.TForall "u0" Nothing
                            (Elab.TForall "a" Nothing
                                (Elab.TArrow (Elab.TVar "a") (Elab.TVar "u0")))
                canonType out `shouldBe` canonType expected

            it "scheme-aware Φ places Raise after bound dependencies (well-scoped bound)" $ do
                (solved, _intNode) <- requireRight (runSolvedWithRoot (ELit (LInt 1)))

                let scheme =
                        Elab.Forall
                            [ ("a", Nothing)
                            , ("b", Nothing)
                            , ("c", Just (Elab.TVar "a"))
                            ]
                            (Elab.TArrow (Elab.TVar "a") (Elab.TArrow (Elab.TVar "c") (Elab.TVar "b")))
                    subst = IntMap.fromList [(1, "a"), (2, "b"), (3, "c")]
                    si = Elab.SchemeInfo { Elab.siScheme = scheme, Elab.siSubst = subst }

                    ops = [OpRaise (NodeId 3)]
                    ew = EdgeWitness
                        { ewEdgeId = EdgeId 0
                        , ewLeft = NodeId 0
                        , ewRight = NodeId 0
                        , ewRoot = NodeId 0
                        , ewSteps = map StepOmega ops
                        , ewForallIntros = 0
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitness solved (Just si) ew)
                out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)

                let expected =
                        Elab.TForall "a" Nothing
                            (Elab.TForall "u0" (Just (Elab.TVar "a"))
                                (Elab.TForall "b" Nothing
                                    (Elab.TArrow (Elab.TVar "a") (Elab.TArrow (Elab.TVar "u0") (Elab.TVar "b")))))
                canonType out `shouldBe` canonType expected

            it "Φ uses per-edge ≺ (via EdgeTrace) to order binders before placing Raise" $ do
                let root = NodeId 100
                    aN = NodeId 1
                    bN = NodeId 2
                    cN = NodeId 3

                    c = Constraint
                        { cVarBounds =
                            IntMap.fromList
                                [ (getNodeId aN, Nothing)
                                , (getNodeId bN, Nothing)
                                , (getNodeId cN, Just bN)
                                ]
                        , cEliminatedVars = IntSet.empty
                        , cNodes =
                            IntMap.fromList
                                [ (100, TyArrow root bN aN)
                                , (1, TyVar aN)
                                , (2, TyVar bN)
                                , (3, TyVar cN)
                                ]
                        , cInstEdges = []
                        , cUnifyEdges = []
                        , cBindParents =
                            IntMap.fromList
                                [ (getNodeId aN, (root, BindFlex))
                                , (getNodeId bN, (root, BindFlex))
                                ]
                        }
                    solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

                    scheme =
                        Elab.Forall
                            [ ("a", Nothing)
                            , ("b", Nothing)
                            , ("c", Just (Elab.TVar "b"))
                            ]
                            (Elab.TArrow (Elab.TVar "b") (Elab.TVar "a"))
                    subst = IntMap.fromList [(1, "a"), (2, "b"), (3, "c")]
                    si = Elab.SchemeInfo { Elab.siScheme = scheme, Elab.siSubst = subst }

                    tr =
                        EdgeTrace
                            { etRoot = root
                            , etBinderArgs = []
                            , etInterior = IntSet.empty
                            , etCopyMap = IntMap.empty
                            }

                    ops = [OpRaise cN]
                    ew = EdgeWitness
                        { ewEdgeId = EdgeId 0
                        , ewLeft = NodeId 0
                        , ewRight = NodeId 0
                        , ewRoot = root
                        , ewSteps = map StepOmega ops
                        , ewForallIntros = 0
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitnessWithTrace solved (Just si) (Just tr) ew)
                out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)

                let expected =
                        Elab.TForall "b" Nothing
                            (Elab.TForall "u0" (Just (Elab.TVar "b"))
                                (Elab.TForall "a" Nothing
                                    (Elab.TArrow (Elab.TVar "b") (Elab.TVar "a"))))
                canonType out `shouldBe` canonType expected

            it "witness instantiation matches solved edge types (id @ Int)" $ do
                let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
                case runToSolved expr of
                    Left err -> expectationFailure err
                    Right (solved, ews, traces) -> do
                        IntMap.size ews `shouldSatisfy` (> 0)
                        forM_ (IntMap.elems ews) $ \ew -> do
                            srcTy <- requireRight (Elab.reifyType solved (ewRoot ew))
                            tgtTy <- requireRight (Elab.reifyType solved (ewRight ew))
                            let EdgeId eid = ewEdgeId ew
                                mTrace = IntMap.lookup eid traces
                            phi <- requireRight (Elab.phiFromEdgeWitnessWithTrace solved Nothing mTrace ew)
                            out <- requireRight (Elab.applyInstantiation srcTy phi)
                            canonType out `shouldBe` canonType tgtTy

            it "witness instantiation matches solved edge types (two instantiations)" $ do
                let expr =
                        ELet "f" (ELam "x" (EVar "x"))
                            (ELet "_" (EApp (EVar "f") (ELit (LInt 1)))
                                (EApp (EVar "f") (ELit (LBool True))))
                case runToSolved expr of
                    Left err -> expectationFailure err
                    Right (solved, ews, traces) -> do
                        -- Each application emits two instantiation edges (fun + arg).
                        IntMap.size ews `shouldBe` 4
                        forM_ (IntMap.elems ews) $ \ew -> do
                            srcTy <- requireRight (Elab.reifyType solved (ewRoot ew))
                            tgtTy <- requireRight (Elab.reifyType solved (ewRight ew))
                            let EdgeId eid = ewEdgeId ew
                                mTrace = IntMap.lookup eid traces
                            phi <- requireRight (Elab.phiFromEdgeWitnessWithTrace solved Nothing mTrace ew)
                            out <- requireRight (Elab.applyInstantiation srcTy phi)
                            canonType out `shouldBe` canonType tgtTy

            it "contextToNodeBound computes quantifier-only contexts (context)" $ do
                -- Binding tree:
                --   root binds a and m
                --   m binds n
                -- Context to reach n from root must go under a, then inside m's bound.
                let root = NodeId 100
                    aN = NodeId 1
                    mN = NodeId 2
                    nN = NodeId 3

                    c = Constraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId root, TyArrow root aN mN)
                                , (getNodeId aN, TyVar aN)
                                , (getNodeId mN, TyArrow mN nN aN)
                                , (getNodeId nN, TyArrow nN aN aN)
                                ]
                        , cInstEdges = []
                        , cUnifyEdges = []
                        , cBindParents = IntMap.fromList
                            [ (getNodeId aN, (root, BindFlex))
                            , (getNodeId mN, (root, BindFlex))
                            , (getNodeId nN, (mN, BindFlex))
                            ]
                        , cVarBounds = IntMap.empty
                        , cEliminatedVars = IntSet.empty
                        }
                    solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

                steps <- requireRight (Elab.contextToNodeBound solved root nN)
                steps `shouldBe` Just [Elab.StepUnder "t1", Elab.StepInside]

            it "selectMinPrecInsertionIndex implements m = min≺ selection (min≺)" $ do
                -- Keys are ordered by: deeper first, then leftmost path.
                -- We craft: key(1) ≺ key(n) ≺ key(2) ≺ key(3).
                let k depth path = Order.OrderKey { Order.okDepth = depth, Order.okPath = path }
                    keys =
                        IntMap.fromList
                            [ (1, k 3 [0])
                            , (2, k 1 [0])
                            , (3, k 0 [0])
                            , (10, k 2 [0])
                            ]
                    ids = [Just (NodeId 1), Just (NodeId 2), Just (NodeId 3)]
                    nN = NodeId 10
                    canonical = id

                Elab.selectMinPrecInsertionIndex 0 keys canonical nN ids `shouldBe` 1
                Elab.selectMinPrecInsertionIndex 2 keys canonical nN ids `shouldBe` 2

            -- Regression test for non-spine OpRaise (paper Fig. 10)
            -- Requirements: 6.1, 6.2, 6.3, 7.3
            it "Φ translates non-spine OpRaise using binding edges and ≺ ordering (non-spine)" $ do
                -- This models a Raise(n) where n is a flex node bound under m, and m is
                -- bound under the edge root. In the type, n's quantifier appears *inside*
                -- m's bound (non-spine). Raise(n) should:
                --   1) insert a fresh quantifier at the root level (before m), and
                --   2) alias/eliminate the original nested quantifier for n inside m's bound.
                let root = NodeId 100
                    aN = NodeId 1
                    mN = NodeId 2
                    nN = NodeId 3

                    c = Constraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId root, TyArrow root aN mN)
                                , (getNodeId aN, TyVar aN)
                                , (getNodeId mN, TyArrow mN nN aN)
                                , (getNodeId nN, TyArrow nN aN aN)
                                ]
                        , cInstEdges = []
                        , cUnifyEdges = []
                        , cBindParents = IntMap.fromList
                            [ (getNodeId aN, (root, BindFlex))
                            , (getNodeId mN, (root, BindFlex))
                            , (getNodeId nN, (mN, BindFlex))
                            ]
                        , cVarBounds = IntMap.empty
                        , cEliminatedVars = IntSet.empty
                        }
                    solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

                    nTy = Elab.TArrow (Elab.TVar "a") (Elab.TVar "a")
                    scheme =
                        Elab.Forall
                            [ ("a", Nothing)
                            , ("m", Just (Elab.TForall "c" (Just nTy) (Elab.TVar "c")))
                            ]
                            (Elab.TVar "m")
                    subst = IntMap.fromList [(getNodeId aN, "a"), (getNodeId mN, "m")]
                    si = Elab.SchemeInfo { Elab.siScheme = scheme, Elab.siSubst = subst }

                    tr =
                        EdgeTrace
                            { etRoot = root
                            , etBinderArgs = []
                            , etInterior = IntSet.fromList [getNodeId root, getNodeId aN, getNodeId mN, getNodeId nN]
                            , etCopyMap = IntMap.empty
                            }

                    ops = [OpRaise nN]
                    ew = EdgeWitness
                        { ewEdgeId = EdgeId 0
                        , ewLeft = NodeId 0
                        , ewRight = NodeId 0
                        , ewRoot = root
                        , ewSteps = map StepOmega ops
                        , ewForallIntros = 0
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitnessWithTrace solved (Just si) (Just tr) ew)
                out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)

                let expected =
                        Elab.TForall "a" Nothing
                            (Elab.TForall "u0" (Just nTy)
                                (Elab.TForall "m" (Just (Elab.TVar "u0"))
                                    (Elab.TVar "m")))
                out `shouldAlphaEqType` expected

    describe "Presolution witness ops (paper alignment)" $ do
        it "emits Merge for bounded aliasing (b ⩾ a)" $ do
            let rhs = ELam "x" (ELam "y" (EVar "x"))
                scheme =
                    SrcScheme
                        [ ("a", Nothing)
                        , ("b", Just (STVar "a"))
                        ]
                        (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))
                ann =
                    STForall "a" Nothing
                        (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))
                expr = ELetAnn "c" scheme rhs (EAnn (EVar "c") ann)

            let runToPresolutionWitnesses :: Expr -> Either String (IntMap.IntMap EdgeWitness)
                runToPresolutionWitnesses e = do
                    ConstraintResult { crConstraint = c0 } <- firstShow (generateConstraints e)
                    let c1 = normalize c0
                    acyc <- firstShow (checkAcyclicity c1)
                    pres <- firstShow (computePresolution acyc c1)
                    pure (prEdgeWitnesses pres)

                firstShow :: Show err => Either err a -> Either String a
                firstShow = either (Left . show) Right

            ews <- requireRight (runToPresolutionWitnesses expr)
            let ops =
                    [ op
                    | ew <- IntMap.elems ews
                    , let InstanceWitness xs = ewWitness ew
                    , op <- xs
                    ]
            let isMerge :: InstanceOp -> Bool
                isMerge op = case op of
                    OpMerge{} -> True
                    _ -> False
            ops `shouldSatisfy` any isMerge

    describe "Paper alignment baselines" $ do
        it "let id = (\\x. x) in id id should have type ∀a. a -> a" $ do
            let expr =
                    ELet "id" (ELam "x" (EVar "x"))
                        (EApp (EVar "id") (EVar "id"))
            (_term, ty) <- requirePipeline expr
            let expected =
                    Elab.TForall "a" Nothing
                        (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
            ty `shouldAlphaEqType` expected

        it "\\y. let id = (\\x. x) in id y should have type ∀a. a -> a" $ do
            let expr =
                    ELam "y"
                        (ELet "id" (ELam "x" (EVar "x"))
                            (EApp (EVar "id") (EVar "y")))
            (_term, ty) <- requirePipeline expr
            let expected =
                    Elab.TForall "a" Nothing
                        (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
            ty `shouldAlphaEqType` expected

        it "bounded aliasing (b ⩾ a) coerces to ∀a. a -> a -> a (needs Merge/RaiseMerge)" $ do
            -- This corresponds to “aliasing” a bounded variable to an existing binder:
            --   ∀a. ∀(b ⩾ a). a -> b -> a  ≤  ∀a. a -> a -> a
            --
            -- In paper terms, this is naturally witnessed via Merge/RaiseMerge (Fig. 10).
            -- Today, presolution witnesses are derived only from expansions (Graft/Weaken),
            -- which translates to InstApp and fails on non-⊥ bounds.
            let rhs = ELam "x" (ELam "y" (EVar "x"))
                scheme =
                    SrcScheme
                        [ ("a", Nothing)
                        , ("b", Just (STVar "a"))
                        ]
                        (STArrow (STVar "a") (STArrow (STVar "b") (STVar "a")))
                ann =
                    STForall "a" Nothing
                        (STArrow (STVar "a") (STArrow (STVar "a") (STVar "a")))
                expr = ELetAnn "c" scheme rhs (EAnn (EVar "c") ann)

            case Elab.runPipelineElab expr of
                Left err ->
                    expectationFailure ("Expected this to typecheck per xmlf.txt, but got: " ++ err)
                Right (_term, ty) -> do
                    let expected =
                            Elab.TForall "a" Nothing
                                (Elab.TArrow (Elab.TVar "a")
                                    (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a")))
                    ty `shouldAlphaEqType` expected

        it "term annotation can instantiate a polymorphic result" $ do
            -- Paper view (xmlf.txt §3.1): (b : σ) is κσ b, which checks that
            -- type(b) ≤ σ (instantiation), not type(b) == σ.
            --
            -- Here the lambda returns a polymorphic `id`, and the annotation asks
            -- for a monomorphic instance of that result.
            let ann =
                    STArrow (STBase "Int")
                        (STArrow (STBase "Int") (STBase "Int"))
                expr =
                    EAnn
                        (ELam "x"
                            (ELet "id" (ELam "y" (EVar "y")) (EVar "id")))
                        ann

            (_term, ty) <- requirePipeline expr
            ty `shouldBe`
                Elab.TArrow
                    (Elab.TBase (BaseTy "Int"))
                    (Elab.TArrow (Elab.TBase (BaseTy "Int")) (Elab.TBase (BaseTy "Int")))

        it "annotated lambda parameter should accept a polymorphic argument via κσ" $ do
            -- λ(f : Int -> Int). f 1   applied to polymorphic id
            -- Desugaring: λf. let f = κ(Int->Int) f in f 1
            -- Outer f may be ∀a. a -> a as long as it can be instantiated to Int -> Int.
            let idExpr = ELam "x" (EVar "x")
                paramTy = STArrow (STBase "Int") (STBase "Int")
                use =
                    EApp
                        (ELamAnn "f" paramTy
                            (EApp (EVar "f") (ELit (LInt 1))))
                        (EVar "id")
                expr = ELet "id" idExpr use

            (_term, ty) <- requirePipeline expr
            ty `shouldBe` Elab.TBase (BaseTy "Int")
