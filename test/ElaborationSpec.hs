module ElaborationSpec (spec) where

import Test.Hspec
import Control.Monad (forM_, when)
import Data.List (isInfixOf, stripPrefix)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

import MLF.Frontend.Syntax (Expr(..), Lit(..), SrcType(..), SrcScheme(..))
import qualified MLF.Elab.Pipeline as Elab
import qualified MLF.Util.Order as Order
import MLF.Constraint.Types (BaseTy(..), BindingError(..), NodeId(..), EdgeId(..), GenNode(..), GenNodeId(..), NodeRef(..), TyNode(..), Constraint(..), InstanceOp(..), InstanceStep(..), InstanceWitness(..), EdgeWitness(..), BindFlag(..), getNodeId, genRef, nodeRefKey, typeRef)
import qualified MLF.Binding.Tree as Binding
import MLF.Frontend.ConstraintGen (ConstraintError, ConstraintResult(..), generateConstraints)
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Acyclicity (checkAcyclicity)
import MLF.Constraint.Presolution (PresolutionResult(..), EdgeTrace(..), computePresolution, defaultPlanBuilder)
import MLF.Constraint.Solve (SolveResult(..), solveUnify)
import qualified MLF.Constraint.Solve as Solve (frWith)
import SpecUtil (bindParentsFromPairs, collectVarNodes, emptyConstraint, requireRight, rootedConstraint)

generalizeAtWith = Elab.generalizeAtWithBuilder defaultPlanBuilder

generalizeAt = generalizeAtWith True False Nothing

requirePipeline :: Expr -> IO (Elab.ElabTerm, Elab.ElabType)
requirePipeline = requireRight . Elab.runPipelineElab Set.empty

generateConstraintsDefault :: Expr -> Either ConstraintError ConstraintResult
generateConstraintsDefault = generateConstraints Set.empty

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

stripBoundWrapper :: Elab.ElabType -> Elab.ElabType
stripBoundWrapper (Elab.TForall v (Just bound) (Elab.TVar v'))
    | v == v' = stripBoundWrapper bound
stripBoundWrapper t = t

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
            Elab.prettyDisplay term `shouldBe` "1"
            Elab.prettyDisplay ty `shouldBe` "Int"

        it "elaborates boolean literal" $ do
            let expr = ELit (LBool True)
            (term, ty) <- requirePipeline expr
            Elab.prettyDisplay term `shouldBe` "true"
            Elab.prettyDisplay ty `shouldBe` "Bool"

        it "elaborates lambda" $ do
            let expr = ELam "x" (EVar "x")
            (_, ty) <- requirePipeline expr
            -- Result is generalized at top level
            Elab.prettyDisplay ty `shouldBe` "∀a. a -> a"

        it "elaborates application" $ do
            let expr = EApp (ELam "x" (EVar "x")) (ELit (LInt 42))
            (_, ty) <- requirePipeline expr
            Elab.prettyDisplay ty `shouldBe` "Int"

    describe "Polymorphism and Generalization" $ do
        it "elaborates polymorphic let-binding" $ do
            -- let id = \x. x in id
            let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")
            (term, ty) <- requirePipeline expr

            -- Term should look like: let id : ∀a. a -> a = Λa. λx:a. x in id
            Elab.prettyDisplay term `shouldBe` "let id : ∀a. a -> a = Λa. λx:a. x in id"

            -- Type is polymorphic (compare up to α-equivalence).
            let expected =
                    Elab.TForall "a" Nothing (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
            ty `shouldAlphaEqType` expected

        it "elaborates monomorphic let without extra instantiation" $ do
            -- let x = 1 in x
            let expr = ELet "x" (ELit (LInt 1)) (EVar "x")
            (term, ty) <- requirePipeline expr
            Elab.prettyDisplay term `shouldBe` "let x : Int = 1 in x"
            Elab.prettyDisplay ty `shouldBe` "Int"

        it "elaborates polymorphic instantiation" $ do
            -- let id = \x. x in id 1
            let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
            (term, ty) <- requirePipeline expr
            Elab.prettyDisplay ty `shouldBe` "Int"
            Elab.prettyDisplay term `shouldBe` "let id : ∀a. a -> a = Λa. λx:a. x in (id [⟨Int⟩]) 1"

        it "generalizeAt quantifies vars bound under the scope root" $ do
            let rootGen = GenNodeId 0
                arrow = NodeId 1
                var = NodeId 2
                root = NodeId 3
                nodes = IntMap.fromList
                    [ (getNodeId arrow, TyArrow { tnId = arrow, tnDom = var, tnCod = var })
                    , (getNodeId var, TyVar { tnId = var, tnBound = Nothing })
                    , (getNodeId root, TyVar { tnId = root, tnBound = Just arrow })
                    ]
                bindParents = IntMap.fromList
                    [ (nodeRefKey (typeRef root), (genRef rootGen, BindFlex))
                    , (nodeRefKey (typeRef var), (genRef rootGen, BindFlex))
                    , (nodeRefKey (typeRef arrow), (typeRef root, BindFlex))
                    ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes = IntMap.singleton (getGenNodeId rootGen) (GenNode rootGen [root])
                    }
                solved = SolveResult
                    { srConstraint = constraint
                    , srUnionFind = IntMap.empty
                    }

            (Elab.Forall binds ty, _subst) <- requireRight (generalizeAt solved (genRef rootGen) root)
            case binds of
                [("a", Nothing), ("b", Just boundTy)] -> do
                    boundTy `shouldAlphaEqType` (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
                other ->
                    expectationFailure $ "Expected two binders, got " ++ show other
            ty `shouldBe` Elab.TVar "b"

        it "generalizeAt uses direct gen-node binders (Q(g))" $ do
            let rootGen = GenNodeId 0
                root = NodeId 10
                body = NodeId 11
                direct = NodeId 12
                interior = NodeId 13
                nodes = IntMap.fromList
                    [ (getNodeId root, TyForall { tnId = root, tnBody = body })
                    , (getNodeId body, TyArrow { tnId = body, tnDom = direct, tnCod = interior })
                    , (getNodeId direct, TyVar { tnId = direct, tnBound = Nothing })
                    , (getNodeId interior, TyVar { tnId = interior, tnBound = Nothing })
                    ]
                bindParents = IntMap.fromList
                    [ (nodeRefKey (typeRef root), (genRef rootGen, BindFlex))
                    , (nodeRefKey (typeRef body), (typeRef root, BindFlex))
                    , (nodeRefKey (typeRef direct), (genRef rootGen, BindFlex))
                    , (nodeRefKey (typeRef interior), (typeRef root, BindFlex))
                    ]
                constraint = emptyConstraint
                    { cNodes = nodes
                    , cBindParents = bindParents
                    , cGenNodes = IntMap.singleton (getGenNodeId rootGen) (GenNode rootGen [root])
                    }
                solved = SolveResult
                    { srConstraint = constraint
                    , srUnionFind = IntMap.empty
                    }

            (Elab.Forall binds ty, _subst) <- requireRight (generalizeAt solved (genRef rootGen) root)
            binds `shouldBe` [("a", Nothing)]
            ty `shouldBe`
                Elab.TForall "t13" Nothing
                    (Elab.TArrow (Elab.TVar "a") (Elab.TVar "t13"))

        it "elaborates dual instantiation in application" $ do
            -- let id = \x. x in id id
            let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
            (term, _ty) <- requirePipeline expr
            case term of
                Elab.ELet _ _ _ body ->
                    case body of
                        Elab.EApp fun arg ->
                            case (fun, arg) of
                                (Elab.ETyInst (Elab.EVar "id") instF, Elab.ETyInst (Elab.EVar "id") instA) -> do
                                    instF `shouldNotBe` Elab.InstId
                                    instA `shouldNotBe` Elab.InstId
                                _ ->
                                    expectationFailure $
                                        "Expected instantiation on both sides, saw " ++ show body
                        other ->
                            expectationFailure $ "Expected application body, saw " ++ show other
                other ->
                    expectationFailure $ "Expected let-binding result, saw " ++ show other

        it "elaborates usage of polymorphic let (instantiated at different types)" $ do
            -- let f = \x. x in let _ = f 1 in f true
            -- This forces 'f' to be instantiated twice: once at Int, once at Bool
            let expr = ELet "f" (ELam "x" (EVar "x"))
                        (ELet "_" (EApp (EVar "f") (ELit (LInt 1)))
                            (EApp (EVar "f") (ELit (LBool True))))
            (term, ty) <- requirePipeline expr
            Elab.pretty ty `shouldBe` "∀(a ⩾ Bool). a"
            let s = Elab.pretty term
            let insts = fInstantiations s
            insts `shouldSatisfy` any ("Int" `isInfixOf`)
            insts `shouldSatisfy` any ("Bool" `isInfixOf`)

        it "elaborates nested let bindings" $ do
            -- let x = 1 in let y = x in y
            let expr = ELet "x" (ELit (LInt 1)) (ELet "y" (EVar "x") (EVar "y"))
            (term, ty) <- requirePipeline expr
            Elab.prettyDisplay ty `shouldBe` "Int"

            Elab.prettyDisplay term `shouldSatisfy` ("let x" `isInfixOf`)
            Elab.prettyDisplay term `shouldSatisfy` ("let y" `isInfixOf`)

        it "top-level generalization ignores binders outside the type" $ do
            let expr =
                    ELet "unused" (ELam "x" (EVar "x"))
                        (ELam "y" (EVar "y"))
            (_term, ty) <- requirePipeline expr
            let expected =
                    Elab.TForall "a" Nothing (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
            ty `shouldAlphaEqType` expected

        it "elaborates term annotations" $ do
            -- (\x. x) : Int -> Int
            let ann = STArrow (STBase "Int") (STBase "Int")
                expr = EAnn (ELam "x" (EVar "x")) ann
            (_term, ty) <- requirePipeline expr
            case ty of
                Elab.TForall v (Just (Elab.TBase (BaseTy "Int"))) body ->
                    case body of
                        Elab.TArrow (Elab.TVar _) (Elab.TVar v') | v == v' -> pure ()
                        _ ->
                            expectationFailure ("Expected arrow with binder codomain, got " ++ show ty)
                _ ->
                    expectationFailure ("Expected bounded forall, got " ++ show ty)

    describe "Binding tree coverage" $ do
        let firstShow :: Show err => Either err a -> Either String a
            firstShow = either (Left . show) Right

            runSolvedWithScope :: Expr -> Either String (SolveResult, NodeRef, NodeId)
            runSolvedWithScope e = do
                ConstraintResult { crConstraint = c0, crRoot = root } <- firstShow (generateConstraintsDefault e)
                let c1 = normalize c0
                acyc <- firstShow (checkAcyclicity c1)
                pres <- firstShow (computePresolution acyc c1)
                solved <- firstShow (solveUnify (prConstraint pres))
                let root' = Elab.chaseRedirects (prRedirects pres) root
                scopeRoot <- case Binding.bindingRoots (srConstraint solved) of
                    [rootRef] -> Right rootRef
                    roots -> Left ("Expected single binding root, got " ++ show roots)
                pure (solved, scopeRoot, root')

            bindingPathToRootUnder
                :: (NodeId -> NodeId)
                -> Constraint
                -> NodeRef
                -> Either BindingError [NodeRef]
            bindingPathToRootUnder canonical constraint start0 =
                let startC = case start0 of
                        TypeRef nid -> typeRef (canonical nid)
                        GenRef gid -> GenRef gid
                    go visited path ref = do
                        let key = nodeRefKey ref
                        if IntSet.member key visited
                            then Left (BindingCycleDetected (reverse path))
                            else do
                                mbParent <- Binding.lookupBindParentUnder canonical constraint ref
                                case mbParent of
                                    Nothing -> Right (reverse path)
                                    Just (parent, _flag) ->
                                        go (IntSet.insert key visited) (parent : path) parent
                in go IntSet.empty [startC] startC

            freeVarsUnder :: SolveResult -> NodeId -> Either BindingError IntSet.IntSet
            freeVarsUnder res nid0 =
                let constraint = srConstraint res
                    nodes = cNodes constraint
                    canonical = Solve.frWith (srUnionFind res)
                    go bound visited nid =
                        let key = getNodeId nid
                        in if IntSet.member key visited
                            then Right IntSet.empty
                            else case IntMap.lookup key nodes of
                                Nothing ->
                                    Left (InvalidBindingTree ("freeVarsUnder: missing node " ++ show nid))
                                Just TyVar{} ->
                                    if IntSet.member key bound
                                        then Right IntSet.empty
                                        else Right (IntSet.singleton key)
                                Just TyBase{} -> Right IntSet.empty
                                Just TyBottom{} -> Right IntSet.empty
                                Just TyArrow{ tnDom = d, tnCod = c } -> do
                                    let visited' = IntSet.insert key visited
                                    fv1 <- go bound visited' (canonical d)
                                    fv2 <- go bound visited' (canonical c)
                                    pure (fv1 `IntSet.union` fv2)
                                Just TyForall{ tnId = fId, tnBody = b } -> do
                                    let visited' = IntSet.insert key visited
                                    binders <- Binding.boundFlexChildrenUnder canonical constraint (typeRef (canonical fId))
                                    let bound' =
                                            bound `IntSet.union`
                                            IntSet.fromList (map (getNodeId . canonical) binders)
                                    go bound' visited' (canonical b)
                                Just TyExp{ tnBody = b } -> do
                                    let visited' = IntSet.insert key visited
                                    go bound visited' (canonical b)
                in go IntSet.empty IntSet.empty (canonical nid0)

            assertBindingCoverage :: Expr -> IO ()
            assertBindingCoverage expr = do
                (solved, scopeRoot, typeRoot) <- requireRight (runSolvedWithScope expr)
                freeVars <- requireRight (freeVarsUnder solved typeRoot)
                freeVars `shouldSatisfy` (not . IntSet.null)
                let canonical = Solve.frWith (srUnionFind solved)
                    constraint = srConstraint solved
                    scopeRootC = case scopeRoot of
                        TypeRef nid -> typeRef (canonical nid)
                        GenRef gid -> GenRef gid
                forM_ (IntSet.toList freeVars) $ \vid -> do
                    let v = typeRef (NodeId vid)
                    path <- requireRight (bindingPathToRootUnder canonical constraint v)
                    let hasRoot = scopeRootC `elem` path
                    when (not hasRoot) $
                        expectationFailure $
                            "Free var missing binding path to scope root: "
                                ++ show v
                                ++ " path "
                                ++ show path

        it "covers free vars for top-level lambda" $ do
            let expr = ELam "x" (EVar "x")
            assertBindingCoverage expr

        it "covers free vars for let-polymorphic instantiation (f 1)" $ do
            -- let f = \x. \y. x in f 1  ==>  a -> Int
            -- The free var in the result (a) comes from instantiation copying.
            let expr =
                    ELet "f" (ELam "x" (ELam "y" (EVar "x")))
                        (EApp (EVar "f") (ELit (LInt 1)))
            assertBindingCoverage expr

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
            let termStr = Elab.prettyDisplay term
            termStr `shouldSatisfy` ("let f : (Int -> Int) -> Int -> Int" `isInfixOf`)
            termStr `shouldSatisfy` ("λx:" `isInfixOf`)

            Elab.prettyDisplay ty `shouldBe` "(Int -> Int) -> Int -> Int"

        it "elaborates annotated let with polymorphic bound (Rank-2ish)" $ do
            -- let f : ∀(a ⩾ ∀b. b -> b). a -> a = \x. x in f
            let innerBound = STForall "b" Nothing (STArrow (STVar "b") (STVar "b"))
                scheme = SrcScheme [("a", Just innerBound)] (STArrow (STVar "a") (STVar "a"))
                expr = ELetAnn "f" scheme (ELam "x" (EVar "x")) (EVar "f")

            (_term, ty) <- requirePipeline expr
            let expected =
                    Elab.TForall "a"
                        (Just (Elab.TForall "b" Nothing (Elab.TArrow (Elab.TVar "b") (Elab.TVar "b"))))
                        (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
            ty `shouldAlphaEqType` expected

        it "elaborates lambda with rank-2 argument" $ do
            -- \x : (∀a. a -> a). x 1
            -- The annotation is preserved as a rank-2 argument type.
            let paramTy = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
                expr = ELamAnn "x" paramTy (EApp (EVar "x") (ELit (LInt 1)))

            (_term, ty) <- requirePipeline expr
            let expected =
                    Elab.TForall "a"
                        (Just (Elab.TBase (BaseTy "Int")))
                        (Elab.TArrow
                            (Elab.TForall "b" Nothing (Elab.TArrow (Elab.TVar "b") (Elab.TVar "b")))
                            (Elab.TVar "a"))
            ty `shouldAlphaEqType` expected

    describe "Elaboration bookkeeping (eliminated vars)" $ do
        it "generalizeAt inlines eliminated binders to bottom" $ do
            let v = NodeId 1
                arrow = NodeId 2
                forallNode = NodeId 3
                c =
                    rootedConstraint emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId v, TyVar { tnId = v, tnBound = Nothing })
                                , (getNodeId arrow, TyArrow arrow v v)
                                , (getNodeId forallNode, TyForall forallNode arrow)
                                ]
                        , cBindParents =
                            bindParentsFromPairs
                                [ (arrow, forallNode, BindFlex)
                                , (v, forallNode, BindFlex)
                                ]
                        , cEliminatedVars = IntSet.singleton (getNodeId v)
                        }

            solved <- requireRight (solveUnify c)
            (sch, _subst) <- requireRight (generalizeAt solved (typeRef forallNode) forallNode)
            sch `shouldBe`
                Elab.Forall [] (Elab.TArrow Elab.TBottom Elab.TBottom)

        it "generalizeAt inlines eliminated binders with bounds" $ do
            let v = NodeId 1
                b = NodeId 2
                arrow = NodeId 3
                forallNode = NodeId 4
                c =
                    rootedConstraint emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId v, TyVar { tnId = v, tnBound = Just b })
                                , (getNodeId b, TyVar { tnId = b, tnBound = Nothing })
                                , (getNodeId arrow, TyArrow arrow v b)
                                , (getNodeId forallNode, TyForall forallNode arrow)
                                ]
                        , cBindParents =
                            bindParentsFromPairs
                                [ (arrow, forallNode, BindFlex)
                                , (v, forallNode, BindFlex)
                                , (b, forallNode, BindFlex)
                                ]
                        , cEliminatedVars = IntSet.singleton (getNodeId v)
                        }

            solved <- requireRight (solveUnify c)
            (sch, _subst) <- requireRight (generalizeAt solved (typeRef forallNode) forallNode)
            Elab.prettyDisplay sch `shouldBe` "∀a. a -> a"

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
            let rootGen = GenNodeId 0
                vLeft = NodeId 10
                vRight = NodeId 5
                arrow = NodeId 20
                forallNode = NodeId 30

                c =
                    rootedConstraint emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId vLeft, TyVar { tnId = vLeft, tnBound = Nothing })
                                , (getNodeId vRight, TyVar { tnId = vRight, tnBound = Nothing })
                                , (getNodeId arrow, TyArrow arrow vLeft vRight)
                                , (getNodeId forallNode, TyForall forallNode arrow)
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef vLeft), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef vRight), (genRef rootGen, BindFlex))
                                ]
                        }

                solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

            (sch, _subst) <- requireRight (generalizeAt solved (genRef rootGen) forallNode)
            Elab.pretty sch `shouldBe` "∀a b. a -> b"

        it "generalizeAt orders binders by <P when paths diverge (leftmost beats depth)" $ do
            -- The leftmost binder should quantify first even if it is shallower.
            let rootGen = GenNodeId 0
                vShallow = NodeId 5
                vDeep = NodeId 10
                nOuter = NodeId 20
                nInner = NodeId 21
                nInt = NodeId 22
                forallNode = NodeId 30

                c =
                    rootedConstraint emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId vShallow, TyVar { tnId = vShallow, tnBound = Nothing })
                                , (getNodeId vDeep, TyVar { tnId = vDeep, tnBound = Nothing })
                                , (getNodeId nInt, TyBase nInt (BaseTy "Int"))
                                , (getNodeId nInner, TyArrow nInner nInt vDeep)
                                , (getNodeId nOuter, TyArrow nOuter vShallow nInner)
                                , (getNodeId forallNode, TyForall forallNode nOuter)
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef vShallow), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef vDeep), (genRef rootGen, BindFlex))
                                ]
                        }

                solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

            (sch, _subst) <- requireRight (generalizeAt solved (genRef rootGen) forallNode)
            Elab.pretty sch `shouldBe` "∀a b. a -> Int -> b"

        it "generalizeAt respects binder bound dependencies (a ≺ b if b’s bound mentions a)" $ do
            let rootGen = GenNodeId 0
                vA = NodeId 10
                vB = NodeId 5
                arrow = NodeId 20
                forallNode = NodeId 30

                c =
                    rootedConstraint emptyConstraint
                        { cEliminatedVars = IntSet.empty
                        , cNodes =
                            IntMap.fromList
                                [ (getNodeId vA, TyVar { tnId = vA, tnBound = Nothing })
                                , (getNodeId vB, TyVar { tnId = vB, tnBound = Just vA })
                                , (getNodeId arrow, TyArrow arrow vB vA)
                                , (getNodeId forallNode, TyForall forallNode arrow)
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef vA), (genRef rootGen, BindFlex))
                                , (nodeRefKey (typeRef vB), (genRef rootGen, BindFlex))
                                ]
                        }

                solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

            (sch, _subst) <- requireRight (generalizeAt solved (genRef rootGen) forallNode)
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
                    ConstraintResult { crConstraint = c0 } <- firstShow (generateConstraintsDefault e)
                    let c1 = normalize c0
                    acyc <- firstShow (checkAcyclicity c1)
                    pres <- firstShow (computePresolution acyc c1)
                    solved <- firstShow (solveUnify (prConstraint pres))
                    pure (solved, prEdgeWitnesses pres, prEdgeTraces pres)

                runSolvedWithRoot :: Expr -> Either String (SolveResult, NodeId)
                runSolvedWithRoot e = do
                    ConstraintResult { crConstraint = c0, crRoot = root } <- firstShow (generateConstraintsDefault e)
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

                let binderA = NodeId 101
                    binderB = NodeId 102
                    scheme =
                        Elab.Forall [("a", Nothing), ("b", Nothing)]
                            (Elab.TArrow (Elab.TVar "a") (Elab.TVar "b"))
                    subst =
                        IntMap.fromList
                            [ (getNodeId binderA, "a")
                            , (getNodeId binderB, "b")
                            ]
                    si = Elab.SchemeInfo { Elab.siScheme = scheme, Elab.siSubst = subst }

                    -- Witness says: graft Int into binder “b”, then weaken it.
                    ops = [OpGraft intNode binderB, OpWeaken binderB]
                    ew = EdgeWitness
                        { ewEdgeId = EdgeId 0
                        , ewLeft = NodeId 0
                        , ewRight = NodeId 0
                        , ewRoot = NodeId 0
                        , ewSteps = map StepOmega ops
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitness generalizeAtWith solved (Just si) ew)

                -- Because we target the *second* binder, Φ must do more than a plain ⟨Int⟩.
                phi `shouldNotBe` Elab.InstApp (Elab.TBase (BaseTy "Int"))

                out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
                argTy <- requireRight (Elab.reifyBoundWithNames solved IntMap.empty intNode)
                let expected =
                        Elab.TForall "a" Nothing
                            (Elab.TArrow (Elab.TVar "a") argTy)
                canonType out `shouldBe` canonType expected

            it "interleaves StepIntro with Omega ops in Φ translation" $ do
                let root = NodeId 0
                    binder = NodeId 1
                    nodes =
                        IntMap.fromList
                            [ (getNodeId root, TyForall root binder)
                            , (getNodeId binder, TyVar { tnId = binder, tnBound = Nothing })
                            ]
                    bindParents =
                        IntMap.fromList
                            [ (nodeRefKey (typeRef binder), (typeRef root, BindFlex)) ]
                    constraint =
                        rootedConstraint emptyConstraint
                            { cNodes = nodes
                            , cBindParents = bindParents
                            }
                    solved = SolveResult { srConstraint = constraint, srUnionFind = IntMap.empty }

                    scheme = Elab.Forall [("a", Nothing)] (Elab.TVar "a")
                    subst = IntMap.fromList [(getNodeId binder, "a")]
                    si = Elab.SchemeInfo { Elab.siScheme = scheme, Elab.siSubst = subst }

                    ops = [OpWeaken binder]
                    steps = [StepIntro, StepOmega (OpWeaken binder)]
                    ew = EdgeWitness
                        { ewEdgeId = EdgeId 0
                        , ewLeft = root
                        , ewRight = root
                        , ewRoot = root
                        , ewSteps = steps
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitness generalizeAtWith solved (Just si) ew)
                Elab.pretty phi `shouldBe` "O; ∀(u0 ⩾) N"

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
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitness generalizeAtWith solved (Just si) ew)
                out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
                let expected =
                        Elab.TForall "a" Nothing
                            (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
                canonType out `shouldBe` canonType expected

            it "scheme-aware Φ can translate Raise (raise a binder to the front)" $ do
                let scheme =
                        Elab.Forall [("a", Nothing), ("b", Nothing)]
                            (Elab.TArrow (Elab.TVar "a") (Elab.TVar "b"))
                    subst = IntMap.fromList [(1, "a"), (2, "b")]
                    si = Elab.SchemeInfo { Elab.siScheme = scheme, Elab.siSubst = subst }
                    root = NodeId 100
                    aN = NodeId 1
                    bN = NodeId 2
                    c =
                        rootedConstraint emptyConstraint
                            { cEliminatedVars = IntSet.empty
                            , cNodes =
                                IntMap.fromList
                                    [ (getNodeId root, TyArrow root aN bN)
                                    , (getNodeId aN, TyVar { tnId = aN, tnBound = Nothing })
                                    , (getNodeId bN, TyVar { tnId = bN, tnBound = Nothing })
                                    ]
                            , cBindParents =
                                IntMap.fromList
                                    [ (nodeRefKey (typeRef aN), (genRef (GenNodeId 0), BindFlex))
                                    , (nodeRefKey (typeRef bN), (genRef (GenNodeId 0), BindFlex))
                                    ]
                            }
                    solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

                    -- Raise binder “b” outward by introducing a fresh front binder and
                    -- aliasing/eliminating the old one (paper Fig. 10 Raise).
                    ops = [OpRaise (NodeId 2)]
                    ew = EdgeWitness
                        { ewEdgeId = EdgeId 0
                        , ewLeft = root
                        , ewRight = root
                        , ewRoot = root
                        , ewSteps = map StepOmega ops
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitness generalizeAtWith solved (Just si) ew)
                out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)
                let expected =
                        Elab.TForall "u0" Nothing
                            (Elab.TForall "a" Nothing
                                (Elab.TArrow (Elab.TVar "a") (Elab.TVar "u0")))
                canonType out `shouldBe` canonType expected

            it "scheme-aware Φ places Raise after bound dependencies (well-scoped bound)" $ do
                let scheme =
                        Elab.Forall
                            [ ("a", Nothing)
                            , ("b", Nothing)
                            , ("c", Just (Elab.TVar "a"))
                            ]
                            (Elab.TArrow (Elab.TVar "a") (Elab.TArrow (Elab.TVar "c") (Elab.TVar "b")))
                    subst = IntMap.fromList [(1, "a"), (2, "b"), (3, "c")]
                    si = Elab.SchemeInfo { Elab.siScheme = scheme, Elab.siSubst = subst }
                    root = NodeId 100
                    aN = NodeId 1
                    bN = NodeId 2
                    cN = NodeId 3
                    inner = NodeId 101
                    c =
                        rootedConstraint emptyConstraint
                            { cEliminatedVars = IntSet.empty
                            , cNodes =
                                IntMap.fromList
                                    [ (getNodeId root, TyArrow root aN inner)
                                    , (getNodeId inner, TyArrow inner cN bN)
                                    , (getNodeId aN, TyVar { tnId = aN, tnBound = Nothing })
                                    , (getNodeId bN, TyVar { tnId = bN, tnBound = Nothing })
                                    , (getNodeId cN, TyVar { tnId = cN, tnBound = Just aN })
                                    ]
                            , cBindParents =
                                IntMap.fromList
                                    [ (nodeRefKey (typeRef aN), (genRef (GenNodeId 0), BindFlex))
                                    , (nodeRefKey (typeRef bN), (genRef (GenNodeId 0), BindFlex))
                                    , (nodeRefKey (typeRef cN), (genRef (GenNodeId 0), BindFlex))
                                    , (nodeRefKey (typeRef inner), (typeRef root, BindFlex))
                                    ]
                            }
                    solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

                    ops = [OpRaise (NodeId 3)]
                    ew = EdgeWitness
                        { ewEdgeId = EdgeId 0
                        , ewLeft = root
                        , ewRight = root
                        , ewRoot = root
                        , ewSteps = map StepOmega ops
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitness generalizeAtWith solved (Just si) ew)
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
                    inner = NodeId 101

                    c = rootedConstraint emptyConstraint
                        { cEliminatedVars = IntSet.empty
                        , cNodes =
                            IntMap.fromList
                                [ (100, TyArrow root bN inner)
                                , (getNodeId inner, TyArrow inner cN aN)
                                , (1, TyVar { tnId = aN, tnBound = Nothing })
                                , (2, TyVar { tnId = bN, tnBound = Nothing })
                                , (3, TyVar { tnId = cN, tnBound = Just bN })
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef aN), (genRef (GenNodeId 0), BindFlex))
                                , (nodeRefKey (typeRef bN), (genRef (GenNodeId 0), BindFlex))
                                , (nodeRefKey (typeRef cN), (genRef (GenNodeId 0), BindFlex))
                                , (nodeRefKey (typeRef inner), (typeRef root, BindFlex))
                                ]
                        }
                    solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

                    scheme =
                        Elab.Forall
                            [ ("a", Nothing)
                            , ("b", Nothing)
                            , ("c", Just (Elab.TVar "b"))
                            ]
                            (Elab.TArrow (Elab.TVar "b") (Elab.TArrow (Elab.TVar "c") (Elab.TVar "a")))
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
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitnessWithTrace generalizeAtWith solved Nothing (Just si) (Just tr) ew)
                out <- requireRight (Elab.applyInstantiation (Elab.schemeToType scheme) phi)

                let expected =
                        Elab.TForall "b" Nothing
                            (Elab.TForall "u0" (Just (Elab.TVar "b"))
                                (Elab.TForall "a" Nothing
                                    (Elab.TArrow (Elab.TVar "b") (Elab.TArrow (Elab.TVar "u0") (Elab.TVar "a")))))
                canonType out `shouldBe` canonType expected

            it "witness instantiation matches solved edge types (id @ Int)" $ do
                let expr = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 1)))
                case runToSolved expr of
                    Left err -> expectationFailure err
                    Right (solved, ews, traces) -> do
                        IntMap.size ews `shouldSatisfy` (> 0)
                        forM_ (IntMap.elems ews) $ \ew -> do
                            let EdgeId eid = ewEdgeId ew
                                mTrace = IntMap.lookup eid traces
                                canonical = Solve.frWith (srUnionFind solved)
                            let scopeRootFor nid = do
                                    path <- Binding.bindingPathToRoot (srConstraint solved) (typeRef (canonical nid))
                                    case drop 1 path of
                                        [] -> Right (typeRef (canonical nid))
                                        rest ->
                                            case [gid | GenRef gid <- rest] of
                                                (gid:_) -> Right (genRef gid)
                                                [] -> Right (typeRef (canonical nid))
                            srcScope <- requireRight (scopeRootFor (ewRoot ew))
                            tgtScope <- requireRight (scopeRootFor (ewRight ew))
                            (srcSch, _) <- requireRight (generalizeAt solved srcScope (ewRoot ew))
                            (tgtSch, _) <- requireRight (generalizeAt solved tgtScope (ewRight ew))
                            let srcTy = Elab.schemeToType srcSch
                                tgtTy = Elab.schemeToType tgtSch
                            phi <- requireRight (Elab.phiFromEdgeWitnessWithTrace generalizeAtWith solved Nothing Nothing mTrace ew)
                            out <- requireRight (Elab.applyInstantiation srcTy phi)
                            canonType (stripBoundWrapper out) `shouldBe` canonType (stripBoundWrapper tgtTy)

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
                            let EdgeId eid = ewEdgeId ew
                                mTrace = IntMap.lookup eid traces
                                canonical = Solve.frWith (srUnionFind solved)
                            let scopeRootFor nid = do
                                    path <- Binding.bindingPathToRoot (srConstraint solved) (typeRef (canonical nid))
                                    case drop 1 path of
                                        [] -> Right (typeRef (canonical nid))
                                        rest ->
                                            case [gid | GenRef gid <- rest] of
                                                (gid:_) -> Right (genRef gid)
                                                [] -> Right (typeRef (canonical nid))
                            srcScope <- requireRight (scopeRootFor (ewRoot ew))
                            tgtScope <- requireRight (scopeRootFor (ewRight ew))
                            (srcSch, _) <- requireRight (generalizeAt solved srcScope (ewRoot ew))
                            (tgtSch, _) <- requireRight (generalizeAt solved tgtScope (ewRight ew))
                            let srcTy = Elab.schemeToType srcSch
                                tgtTy = Elab.schemeToType tgtSch
                            phi <- requireRight (Elab.phiFromEdgeWitnessWithTrace generalizeAtWith solved Nothing Nothing mTrace ew)
                            out <- requireRight (Elab.applyInstantiation srcTy phi)
                            canonType (stripBoundWrapper out) `shouldBe` canonType (stripBoundWrapper tgtTy)

            it "contextToNodeBound computes inside-bound contexts (context)" $ do
                -- root binds a and b; b's bound contains binder c.
                -- Context to reach c must go under a, then inside b's bound.
                let root = NodeId 100
                    body = NodeId 101
                    aN = NodeId 1
                    bN = NodeId 2
                    cN = NodeId 3

                    c = rootedConstraint emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId root, TyForall root body)
                                , (getNodeId body, TyArrow body aN bN)
                                , (getNodeId aN, TyVar { tnId = aN, tnBound = Nothing })
                                , (getNodeId bN, TyVar { tnId = bN, tnBound = Just cN })
                                , (getNodeId cN, TyVar { tnId = cN, tnBound = Nothing })
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef aN), (typeRef root, BindFlex))
                                , (nodeRefKey (typeRef bN), (typeRef root, BindFlex))
                                , (nodeRefKey (typeRef cN), (typeRef bN, BindFlex))
                                , (nodeRefKey (typeRef body), (typeRef root, BindFlex))
                                ]
                        }
                    solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

                steps <- requireRight (Elab.contextToNodeBound solved root cN)
                steps `shouldBe` Just [Elab.StepUnder "t1", Elab.StepInside]

            it "contextToNodeBound computes under-quantifier contexts (context)" $ do
                -- Same graph as above: binder b is after a at the root.
                let root = NodeId 100
                    body = NodeId 101
                    aN = NodeId 1
                    bN = NodeId 2
                    cN = NodeId 3

                    c = rootedConstraint emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId root, TyForall root body)
                                , (getNodeId body, TyArrow body aN bN)
                                , (getNodeId aN, TyVar { tnId = aN, tnBound = Nothing })
                                , (getNodeId bN, TyVar { tnId = bN, tnBound = Just cN })
                                , (getNodeId cN, TyVar { tnId = cN, tnBound = Nothing })
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef aN), (typeRef root, BindFlex))
                                , (nodeRefKey (typeRef bN), (typeRef root, BindFlex))
                                , (nodeRefKey (typeRef cN), (typeRef bN, BindFlex))
                                , (nodeRefKey (typeRef body), (typeRef root, BindFlex))
                                ]
                        }
                    solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

                steps <- requireRight (Elab.contextToNodeBound solved root bN)
                steps `shouldBe` Just [Elab.StepUnder "t1"]

            it "contextToNodeBound handles shared bound subgraphs (context dag)" $ do
                -- The bound of b is the same node that also appears in the body.
                -- The context should still be computed without treating sharing as a cycle.
                let root = NodeId 100
                    body = NodeId 101
                    bN = NodeId 2
                    shared = NodeId 200
                    xN = NodeId 3

                    c = rootedConstraint emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId root, TyForall root body)
                                , (getNodeId body, TyArrow body bN shared)
                                , (getNodeId bN, TyVar { tnId = bN, tnBound = Just shared })
                                , (getNodeId shared, TyArrow shared xN xN)
                                , (getNodeId xN, TyVar { tnId = xN, tnBound = Nothing })
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef bN), (typeRef root, BindFlex))
                                , (nodeRefKey (typeRef body), (typeRef root, BindRigid))
                                , (nodeRefKey (typeRef shared), (typeRef bN, BindFlex))
                                , (nodeRefKey (typeRef xN), (typeRef shared, BindFlex))
                                ]
                        }
                    solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

                steps <- requireRight (Elab.contextToNodeBound solved root xN)
                steps `shouldBe` Just [Elab.StepInside]

            it "contextToNodeBound ignores non-variable binder bounds (context non-var)" $ do
                -- Binder b is an arrow node; non-variable bounds are ignored.
                let root = NodeId 100
                    body = NodeId 101
                    bN = NodeId 2
                    domN = NodeId 3
                    codN = NodeId 4

                    c = rootedConstraint emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId root, TyForall root body)
                                , (getNodeId body, TyArrow body bN bN)
                                , (getNodeId bN, TyArrow bN domN codN)
                                , (getNodeId domN, TyVar { tnId = domN, tnBound = Nothing })
                                , (getNodeId codN, TyVar { tnId = codN, tnBound = Nothing })
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef body), (typeRef root, BindFlex))
                                , (nodeRefKey (typeRef bN), (typeRef root, BindFlex))
                                , (nodeRefKey (typeRef domN), (typeRef bN, BindFlex))
                                , (nodeRefKey (typeRef codN), (typeRef bN, BindFlex))
                                ]
                        }
                    solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

                steps <- requireRight (Elab.contextToNodeBound solved root domN)
                steps `shouldBe` Nothing

            it "rejects fallback-dependent binders (gen fallback invariant)" $ do
                let rootGen = GenNodeId 0
                    root = NodeId 100
                    aN = NodeId 1
                    nodes = IntMap.fromList
                        [ (getNodeId root, TyForall root aN)
                        , (getNodeId aN, TyVar { tnId = aN, tnBound = Nothing })
                        ]
                    bindParents = IntMap.fromList
                        [ (nodeRefKey (typeRef root), (genRef rootGen, BindFlex))
                        , (nodeRefKey (typeRef aN), (genRef rootGen, BindFlex))
                        ]
                    constraint = emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        , cGenNodes = IntMap.singleton (getGenNodeId rootGen) (GenNode rootGen [root])
                        }

                case Binding.checkNoGenFallback constraint of
                    Left GenFallbackRequired{ fallbackBinder, fallbackGen, fallbackBinders } -> do
                        fallbackBinder `shouldBe` root
                        fallbackGen `shouldBe` rootGen
                        fallbackBinders `shouldBe` [aN]
                    Left err ->
                        expectationFailure ("Expected GenFallbackRequired, got " ++ show err)
                    Right () ->
                        expectationFailure "Expected GenFallbackRequired, got success"

            it "rejects schemes that reach named nodes outside their gen scope" $ do
                let rootGen = GenNodeId 0
                    innerGen = GenNodeId 1
                    root = NodeId 100
                    vN = NodeId 1
                    nodes = IntMap.fromList
                        [ (getNodeId root, TyForall root vN)
                        , (getNodeId vN, TyVar { tnId = vN, tnBound = Nothing })
                        ]
                    bindParents = IntMap.fromList
                        [ (nodeRefKey (typeRef root), (genRef rootGen, BindFlex))
                        , (nodeRefKey (typeRef vN), (genRef innerGen, BindFlex))
                        , (nodeRefKey (genRef innerGen), (genRef rootGen, BindFlex))
                        ]
                    constraint = emptyConstraint
                        { cNodes = nodes
                        , cBindParents = bindParents
                        , cGenNodes = IntMap.fromList
                            [ (getGenNodeId rootGen, GenNode rootGen [root])
                            , (getGenNodeId innerGen, GenNode innerGen [])
                            ]
                        }

                case Binding.checkSchemeClosure constraint of
                    Left GenSchemeFreeVars{ schemeRoot, schemeGen, freeNodes } -> do
                        schemeRoot `shouldBe` root
                        schemeGen `shouldBe` rootGen
                        freeNodes `shouldBe` [vN]
                    Left err ->
                        expectationFailure ("Expected GenSchemeFreeVars, got " ++ show err)
                    Right () ->
                        expectationFailure "Expected GenSchemeFreeVars, got success"

            it "selectMinPrecInsertionIndex implements m = min≺ selection (min≺)" $ do
                -- Keys are ordered by <P (lexicographic with empty path greatest).
                -- We craft: key(1) ≺ key(n) ≺ key(2) ≺ key(3).
                let k path = Order.OrderKey { Order.okDepth = length path, Order.okPath = path }
                    keys =
                        IntMap.fromList
                            [ (1, k [0])
                            , (2, k [2])
                            , (3, k [3])
                            , (10, k [1])
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

                    c = rootedConstraint emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId root, TyArrow root aN mN)
                                , (getNodeId aN, TyVar { tnId = aN, tnBound = Nothing })
                                , (getNodeId mN, TyArrow mN nN aN)
                                , (getNodeId nN, TyArrow nN aN aN)
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef aN), (genRef (GenNodeId 0), BindFlex))
                                , (nodeRefKey (typeRef mN), (typeRef root, BindFlex))
                                , (nodeRefKey (typeRef nN), (typeRef mN, BindFlex))
                                ]
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
                        , ewWitness = InstanceWitness ops
                        }

                phi <- requireRight (Elab.phiFromEdgeWitnessWithTrace generalizeAtWith solved Nothing (Just si) (Just tr) ew)
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
                    ConstraintResult { crConstraint = c0 } <- firstShow (generateConstraintsDefault e)
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

        it "let-use sites are redirected for polymorphic instantiation" $ do
            let expr =
                    ELet "id" (ELam "x" (EVar "x"))
                        (EApp (EVar "id") (EVar "id"))
            let firstShow :: Show err => Either err a -> Either String a
                firstShow = either (Left . show) Right
                runToPresolution = do
                    ConstraintResult { crConstraint = c0, crAnnotated = ann } <- firstShow (generateConstraintsDefault expr)
                    let c1 = normalize c0
                    acyc <- firstShow (checkAcyclicity c1)
                    pres <- firstShow (computePresolution acyc c1)
                    pure (prRedirects pres, ann)

            case runToPresolution of
                Left err -> expectationFailure err
                Right (redirects, ann) -> do
                    let varNodes = collectVarNodes "id" ann
                        redirected =
                            [ nid
                            | nid <- varNodes
                            , Elab.chaseRedirects redirects nid /= nid
                            ]
                    varNodes `shouldSatisfy` (not . null)
                    redirected `shouldSatisfy` (not . null)

        it "generalizeAt inlines rigid vars via bounds at top-level" $ do
            let rootGen = GenNodeId 0
                arrow = NodeId 1
                rigidVar = NodeId 2
                flexVar = NodeId 3
                c =
                    rootedConstraint emptyConstraint
                        { cNodes =
                            IntMap.fromList
                                [ (getNodeId arrow, TyArrow arrow rigidVar rigidVar)
                                , (getNodeId rigidVar, TyVar { tnId = rigidVar, tnBound = Just flexVar })
                                , (getNodeId flexVar, TyVar { tnId = flexVar, tnBound = Nothing })
                                ]
                        , cBindParents =
                            IntMap.fromList
                                [ (nodeRefKey (typeRef arrow), (genRef rootGen, BindRigid))
                                , (nodeRefKey (typeRef rigidVar), (typeRef arrow, BindRigid))
                                , (nodeRefKey (typeRef flexVar), (genRef rootGen, BindFlex))
                                ]
                        }
                solved = SolveResult { srConstraint = c, srUnionFind = IntMap.empty }

            (sch, _subst) <- requireRight (generalizeAt solved (genRef rootGen) arrow)
            Elab.prettyDisplay sch `shouldBe` "∀a. a -> a"

        it "\\y. let id = (\\x. x) in id y should have type ∀a. a -> a" $ do
            let expr =
                    ELam "y"
                        (ELet "id" (ELam "x" (EVar "x"))
                            (EApp (EVar "id") (EVar "y")))
            (_term, ty) <- requirePipeline expr
            let expected =
                    Elab.TForall "a" Nothing
                        (Elab.TForall "b" (Just (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a")))
                            (Elab.TVar "b"))
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

            case Elab.runPipelineElab Set.empty expr of
                Left err ->
                    expectationFailure ("Expected this to typecheck per these-finale-english.txt (see xmlf.txt), but got: " ++ err)
                Right (_term, ty) -> do
                    let expected =
                            Elab.TForall "a" Nothing
                                (Elab.TArrow (Elab.TVar "a") (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a")))
                    ty `shouldAlphaEqType` expected

        it "term annotation can instantiate a polymorphic result" $ do
            -- Paper view (`papers/these-finale-english.txt`; see `papers/xmlf.txt` §3.1):
            -- (b : σ) is κσ b, which checks that
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
            let expected =
                    Elab.TForall "a" (Just (Elab.TBase (BaseTy "Int")))
                        (Elab.TForall "b"
                            (Just (Elab.TArrow (Elab.TVar "a") (Elab.TArrow (Elab.TBase (BaseTy "Int")) (Elab.TBase (BaseTy "Int")))))
                            (Elab.TVar "b"))
            ty `shouldAlphaEqType` expected

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

    describe "Explicit forall annotation edge cases" $ do
        it "explicit forall annotation round-trips on let-bound variables" $ do
            let ann = STForall "a" Nothing (STArrow (STVar "a") (STVar "a"))
                expr =
                    ELet "id" (ELam "x" (EVar "x"))
                        (EAnn (EVar "id") ann)

            (_term, ty) <- requirePipeline expr
            let expected =
                    Elab.TForall "a" Nothing
                        (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
            ty `shouldAlphaEqType` expected

        it "explicit forall annotation preserves foralls in bounds" $ do
            let ann =
                    STForall "a"
                        (Just (STForall "b" Nothing (STArrow (STVar "b") (STVar "b"))))
                        (STArrow (STVar "a") (STVar "a"))
                expr = EAnn (ELam "x" (EVar "x")) ann

            (_term, ty) <- requirePipeline expr
            let expected =
                    Elab.TForall "a"
                        (Just (Elab.TForall "b" Nothing (Elab.TArrow (Elab.TVar "b") (Elab.TVar "b"))))
                        (Elab.TArrow (Elab.TVar "a") (Elab.TVar "a"))
            ty `shouldAlphaEqType` expected
