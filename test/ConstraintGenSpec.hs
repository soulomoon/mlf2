module ConstraintGenSpec (spec) where

import Control.Monad (filterM)
import Data.List (nub)
import Data.Maybe (catMaybes)
import qualified Data.IntMap.Strict as IntMap
import Test.Hspec

import MLF.Binding (checkBindingTree)
import MLF.ConstraintGen (AnnExpr (..))
import MyLib

spec :: Spec
spec = describe "Phase 1 — Constraint generation" $ do
    describe "Literals" $ do
        it "creates a single base node for integer literals" $ do
            let expr = ELit (LInt 42)
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                IntMap.size nodes `shouldBe` 1
                case IntMap.elems nodes of
                    [TyBase { tnBase = BaseTy name }] -> name `shouldBe` "Int"
                    other -> expectationFailure $ "Unexpected nodes: " ++ show other
                case IntMap.lookup (getNodeId (crRoot result)) nodes of
                    Just TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                    _ -> expectationFailure "Root does not point to literal node"

        it "creates a single base node for boolean literals" $ do
            let expr = ELit (LBool True)
            expectRight (inferConstraintGraph expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                IntMap.size nodes `shouldBe` 1
                case IntMap.elems nodes of
                    [TyBase { tnBase = BaseTy name }] -> name `shouldBe` "Bool"
                    other -> expectationFailure $ "Unexpected nodes: " ++ show other

        it "creates a single base node for string literals" $ do
            let expr = ELit (LString "hi")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                IntMap.size nodes `shouldBe` 1
                case IntMap.elems nodes of
                    [TyBase { tnBase = BaseTy name }] -> name `shouldBe` "String"
                    other -> expectationFailure $ "Unexpected nodes: " ++ show other

    describe "Variables and scope" $ do
        it "reuses the let scheme node when referencing a binding" $ do
            let expr = ELet "x" (ELit (LInt 0)) (EVar "x")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case IntMap.lookup (getNodeId (crRoot result)) nodes of
                    Just TyExp { tnBody = bodyId } -> do
                        -- With the fix, the body of the EVar's TyExp is the Forall node from the binding
                        body <- lookupNode nodes bodyId
                        case body of
                            TyForall { tnBody = innerBody } -> do
                                inner <- lookupNode nodes innerBody
                                case inner of
                                    TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                                    other -> expectationFailure $ "Expected Int body inside Forall, saw " ++ show other
                            other -> expectationFailure $ "Expected Forall body, saw " ++ show other
                    other -> expectationFailure $ "Root is not the let scheme: " ++ show other

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
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case IntMap.lookup (getNodeId (crRoot result)) nodes of
                    Just TyExp { tnBody = bodyId } -> do
                        -- Expect Forall -> Bool
                        body <- lookupNode nodes bodyId
                        case body of
                            TyForall { tnBody = innerBody } -> do
                                inner <- lookupNode nodes innerBody
                                case inner of
                                    TyBase { tnBase = BaseTy name } -> name `shouldBe` "Bool"
                                    other -> expectationFailure $ "Expected Bool body inside Forall, saw " ++ show other
                            other -> expectationFailure $ "Expected Forall body, saw " ++ show other
                    other -> expectationFailure $ "Root is not an expansion node: " ++ show other

        it "reports unknown variables" $ do
            inferConstraintGraph (EVar "free") `shouldBe` Left (UnknownVariable "free")

        it "reports unknown variables that appear inside let RHS" $ do
            let expr = ELet "x" (EVar "ghost") (ELit (LInt 0))
            inferConstraintGraph expr `shouldBe` Left (UnknownVariable "ghost")

    describe "Annotated Terms" $ do
        it "desugars lambda parameter annotations via κσ coercions" $ do
            -- λ(x:Int). x  ≜  λ(x). let x = (x : Int) in x
            let ann = STBase "Int"
                expr = ELamAnn "x" ann (EVar "x")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    instEdges = cInstEdges constraint
                case IntMap.lookup (getNodeId (crRoot result)) nodes of
                    Just TyArrow { tnDom = domId } -> do
                        domNode <- lookupNode nodes domId
                        case domNode of
                            -- The lambda parameter itself remains monomorphic (fresh var),
                            -- and the annotation is enforced by a coerced let-binding.
                            TyVar{} -> do
                                case instEdges of
                                    [InstEdge _ leftId rightId] -> do
                                        rightNode <- lookupNode nodes rightId
                                        leftId `shouldBe` domId
                                        case rightNode of
                                            TyBase{ tnBase = BaseTy name } -> name `shouldBe` "Int"
                                            other -> expectationFailure $ "Expected Int annotation node, saw " ++ show other
                                    other -> expectationFailure $ "Expected exactly 1 inst edge, saw " ++ show other
                            other -> expectationFailure $ "Expected TyVar parameter, saw " ++ show other
                    other -> expectationFailure $ "Root is not an arrow: " ++ show other

        it "respects polymorphic let annotations" $ do
            -- let id : ∀α. α → α = λx. x in id
            let scheme = SrcScheme [("a", Nothing)] (STArrow (STVar "a") (STVar "a"))
                expr = ELetAnn "id" scheme (ELam "x" (EVar "x")) (EVar "id")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                -- The result should be the body (id), which refers to the expansion of the scheme
                case IntMap.lookup (getNodeId (crRoot result)) nodes of
                    Just TyExp { tnBody = bodyId } -> do
                        body <- lookupNode nodes bodyId
                        -- Expect Forall (scheme) -> Arrow. No extra let-gen layer.
                        case body of
                            TyForall { tnBody = arrowBody } -> do -- Scheme layer
                                arrow <- lookupNode nodes arrowBody
                                case arrow of
                                    TyArrow { tnDom = domId, tnCod = codId } -> do
                                        domId `shouldBe` codId
                                    other -> expectationFailure $ "Expected Arrow body, saw " ++ show other
                            other -> expectationFailure $ "Expected Scheme Forall, saw " ++ show other
                    other -> expectationFailure $ "Root is not an expansion: " ++ show other

        it "respects term annotations" $ do
            -- (1 : Int)
            let ann = STBase "Int"
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraph expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                case IntMap.lookup (getNodeId (crRoot result)) nodes of
                    Just TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                    other -> expectationFailure $ "Expected Int node, saw " ++ show other

        it "respects bounded quantification in schemes" $ do
            -- let f : ∀(a ⩾ Int). a -> a = ...
            -- This checks internalizeBinders with a bound
            let scheme = SrcScheme [("a", Just (STBase "Int"))] (STArrow (STVar "a") (STVar "a"))
                expr = ELetAnn "f" scheme (ELam "x" (EVar "x")) (EVar "f")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                -- We verify that the 'a' variable has an instantiation edge to 'Int'
                case IntMap.lookup (getNodeId (crRoot result)) nodes of
                    Just TyExp { tnBody = bodyId } -> do
                        body <- lookupNode nodes bodyId
                        -- Expect Forall (scheme) -> Arrow. No extra let-gen layer.
                        case body of
                            TyForall { tnBody = arrowBody } -> do
                                arrow <- lookupNode nodes arrowBody
                                case arrow of
                                    TyArrow { tnDom = domId } -> do
                                        -- domId is 'a'. Its bound is recorded in `Constraint.cVarBounds`.
                                        domNode <- lookupNode nodes domId
                                        case domNode of
                                            TyVar{} -> do
                                                case IntMap.lookup (getNodeId domId) (cVarBounds constraint) of
                                                    Just (Just boundId) -> do
                                                        rhs <- lookupNode nodes boundId
                                                        case rhs of
                                                            TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                                                            other -> expectationFailure $ "Expected bound Int, saw " ++ show other
                                                    Just Nothing ->
                                                        expectationFailure "Expected bound for variable, saw Nothing"
                                                    Nothing ->
                                                        expectationFailure "Expected bound entry for variable, saw missing key"
                                            other ->
                                                expectationFailure $ "Expected TyVar for domain, saw " ++ show other
                                    other -> expectationFailure $ "Expected Arrow body, saw " ++ show other
                            other -> expectationFailure $ "Expected Scheme Forall, saw " ++ show other
                    other -> expectationFailure $ "Root is not an expansion: " ++ show other

        it "respects instance bounds in Forall types" $ do
            -- λ(x : ∀(a ⩾ Int). a). x  desugars through a coercion in the body,
            -- which exercises internalizeSrcType STForall with a bound.
            let ann = STForall "a" (Just (STBase "Int")) (STVar "a")
                expr = ELamAnn "x" ann (EVar "x")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    instEdges = cInstEdges constraint
                case instEdges of
                    [InstEdge _ _ annId] -> do
                        annNode <- lookupNode nodes annId
                        case annNode of
                            TyForall { tnBody = bodyId } -> do
                                -- bodyId is 'a'. Its bound is recorded in `Constraint.cVarBounds`.
                                case IntMap.lookup (getNodeId bodyId) (cVarBounds constraint) of
                                    Just (Just boundId) -> do
                                        rhs <- lookupNode nodes boundId
                                        case rhs of
                                            TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                                            other -> expectationFailure $ "Expected bound Int, saw " ++ show other
                                    Just Nothing ->
                                        expectationFailure "Expected bound for variable, saw Nothing"
                                    Nothing ->
                                        expectationFailure "Expected bound entry for variable, saw missing key"
                            other -> expectationFailure $ "Expected TyForall annotation node, saw " ++ show other
                    other -> expectationFailure $ "Expected exactly 1 inst edge, saw " ++ show other

        it "internalizes Bottom type" $ do
            -- λ(x : ⊥). x desugars through a coercion in the body, which
            -- exercises internalizeSrcType STBottom.
            let ann = STBottom
                expr = ELamAnn "x" ann (EVar "x")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    instEdges = cInstEdges constraint
                case instEdges of
                    [InstEdge _ _ annId] -> do
                        annNode <- lookupNode nodes annId
                        case annNode of
                            -- Bottom is internalized as a fresh TyVar.
                            TyVar {} -> pure ()
                            other -> expectationFailure $ "Expected TyVar for Bottom, saw " ++ show other
                    other -> expectationFailure $ "Expected exactly 1 inst edge, saw " ++ show other

    describe "Annotation Edge Cases" $ do
        it "handles free type variables in annotations" $ do
            -- (1 : a) where 'a' is free
            -- This checks STVar with Nothing lookup result
            let ann = STVar "a"
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraph expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                case IntMap.lookup (getNodeId (crRoot result)) nodes of
                    Just TyVar {} -> pure ()
                    other -> expectationFailure $ "Expected TyVar for free var, saw " ++ show other

        it "produces valid AnnExpr structure" $ do
             -- let x = 1 in x
             let expr = ELet "x" (ELit (LInt 1)) (EVar "x")
             expectRight (inferConstraintGraph expr) $ \result -> do
                 let ann = crAnnotated result
                 case ann of
                     ALet name schemeNode _ scopeRoot rhsAnn bodyAnn _resNode -> do
                         name `shouldBe` "x"
                         schemeNode `shouldBe` scopeRoot
                         -- Basic structural check
                         case rhsAnn of
                             ALit (LInt 1) _ -> pure ()
                             _ -> expectationFailure "RHS annotation mismatch"
                         case bodyAnn of
                             AVar "x" _ -> pure ()
                             _ -> expectationFailure "Body annotation mismatch"
                     _ -> expectationFailure "Expected ALet annotation"

    describe "Lambda nodes" $ do
        -- A λx.x term yields a single parameter node whose identifier appears in
        -- both the domain and codomain of the arrow node, reflecting the shared
        -- variable in the graphic type.
        it "builds a shared parameter node when translating lambdas" $ do
            let expr = ELam "x" (EVar "x")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let nodes = IntMap.elems (cNodes (crConstraint result))
                    arrowNodes = [n | n@TyArrow {} <- nodes]
                    varNodes = [n | n@TyVar {} <- nodes]
                    expNodes = [n | n@TyExp {} <- nodes] -- EVar usage adds a fresh TyExp

                -- We should have 1 Arrow, 1 Param Var, and 1 Exp node (for the usage of x)
                case (arrowNodes, varNodes, expNodes) of
                    ([TyArrow { tnDom = dom, tnCod = cod }], [TyVar { tnId = varId }], [TyExp { tnId = expId, tnBody = bodyId }]) -> do
                        dom `shouldBe` varId
                        -- The codomain is the expansion of the parameter variable
                        -- But wait, ELam "x" (EVar "x")
                        -- buildExpr ELam: allocVar (param). bind x -> param. buildExpr body.
                        -- buildExpr EVar "x": lookup param. allocExpNode param. return expNode.
                        -- So body result is expNode.
                        -- Arrow is param -> expNode.
                        -- So tnDom = varId. tnCod = expId.
                        -- tnBody of expNode = varId.
                        expId `shouldBe` cod
                        bodyId `shouldBe` varId
                    _ ->
                        expectationFailure $ "Unexpected lambda nodes: " ++ show (length arrowNodes, length varNodes, length expNodes)

    describe "Applications" $ do
        -- Verify that application translation produces a single instantiation edge
        -- s τ ≤ (Int → α) where the left-hand side points to the let-generalized
        -- scheme (the TyExp node) and the right-hand side is the arrow demanded by
        -- the call site. Note [Expansion nodes] in 'MLF.Types' explains how the
        -- solver processes these edges.
        it "emits instantiation edges for applications" $ do
            let expr =
                    ELet "f" (ELam "x" (EVar "x"))
                        (EApp (EVar "f") (ELit (LInt 1)))
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    insts = cInstEdges constraint
                case insts of
                    [edge] -> do
                        lhs <- lookupNode nodes (instLeft edge)
                        case lhs of
                            -- The usage of 'f' creates a TyExp wrapping the bound Forall
                            TyExp { tnBody = bodyId } -> do
                                body <- lookupNode nodes bodyId
                                case body of
                                    TyForall { tnBody = innerBody } -> do
                                        inner <- lookupNode nodes innerBody
                                        case inner of
                                            TyArrow { tnDom = domId, tnCod = codId } -> do
                                                -- domId/codId are the bound variable.
                                                -- But codId is actually the Expansion of that var (from EVar "x")
                                                -- So domId = var. codId = TyExp(var).
                                                domNode <- lookupNode nodes domId
                                                codNode <- lookupNode nodes codId
                                                case (domNode, codNode) of
                                                    (TyVar { tnId = domVar }, TyExp { tnBody = expBody }) ->
                                                        domVar `shouldBe` expBody
                                                    other -> expectationFailure $ "Lambda arrow points to unexpected nodes: " ++ show other
                                            other -> expectationFailure $ "Forall body is not a lambda arrow: " ++ show other
                                    other -> expectationFailure $ "Expansion body is not a Forall: " ++ show other
                            other -> expectationFailure $ "Instantiation left-hand side is not an expansion: " ++ show other
                        rhs <- lookupNode nodes (instRight edge)
                        case rhs of
                            TyArrow { tnDom = dom, tnCod = cod } -> do
                                domNode <- lookupNode nodes dom
                                case domNode of
                                    TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                                    other -> expectationFailure $ "Argument node is not the Int literal: " ++ show other
                                cod `shouldBe` crRoot result
                            other -> expectationFailure $ "Instantiation right-hand side is not an arrow: " ++ show other
                    other -> expectationFailure $ "Expected a single instantiation edge, saw " ++ show (length other)

        it "connects lambda applications directly to arrow nodes" $ do
            let expr = EApp (ELam "x" (EVar "x")) (ELit (LInt 0))
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case cInstEdges constraint of
                    [edge] -> do
                        lhs <- lookupNode nodes (instLeft edge)
                        case lhs of
                            TyArrow {} -> pure ()
                            other -> expectationFailure $ "Instantiation left-hand side is not an arrow: " ++ show other
                    other -> expectationFailure $ "Expected single instantiation edge, saw " ++ show (length other)

        -- Even when an immediately applied lambda uses its argument multiple
        -- times (here via (\f -> let tmp = f 1 in f True) (\x -> x)), the
        -- instantiation edges should still point at the same parameter TyVar
        -- (the λ argument) and that parameter stays bound at the caller level
        -- g₀. No child G-node should appear because nothing was let-generalized.
        it "reuses the same arrow for multiple immediate lambda applications" $ do
            let expr =
                    EApp
                        (ELam "f" $
                            ELet "tmp"
                                (EApp (EVar "f") (ELit (LInt 1)))
                                (EApp (EVar "f") (ELit (LBool True)))
                        )
                        (ELam "x" (EVar "x"))
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    insts = cInstEdges constraint
                literalInsts <- do
                    candidateSets <-
                        mapM
                            (\edge -> do
                                rhs <- lookupNode nodes (instRight edge)
                                case rhs of
                                    TyArrow { tnDom = dom } -> do
                                        domNode <- lookupNode nodes dom
                                        case domNode of
                                            TyBase {} -> pure (Just edge)
                                            _ -> pure Nothing
                                    _ -> pure Nothing
                            )
                            insts
                    pure (catMaybes candidateSets)
                length literalInsts `shouldBe` 2
                let lhsIds = map instLeft literalInsts
                -- Now that we wrap usages in fresh TyExp nodes, the LHS IDs will be different!
                -- They will be distinct TyExp nodes wrapping the same underlying parameter variable.
                length (nub lhsIds) `shouldBe` 2

                -- Verify they wrap the same parameter
                let checkParam lhsId = do
                        lhs <- lookupNode nodes lhsId
                        case lhs of
                            TyExp { tnBody = bodyId } -> pure bodyId
                            _ -> expectationFailure "Expected TyExp" >> pure (error "unreachable")

                paramIds <- mapM checkParam lhsIds
                length (nub paramIds) `shouldBe` 1

                paramVar <- case paramIds of
                    (paramId:_) -> lookupNode nodes paramId
                    [] -> expectationFailure "Expected parameter ids" >> pure (error "unreachable")
                case paramVar of
                    TyVar {} -> pure ()
                    other -> expectationFailure $ "Instantiation left-hand side is not a parameter TyVar: " ++ show other

    describe "Binding edges" $ do
        it "does not emit instantiation edges for unused let bindings" $ do
            let expr =
                    ELet "f" (ELam "x" (EVar "x"))
                        (ELit (LInt 0))
            expectRight (inferConstraintGraph expr) $ \result -> do
                cInstEdges (crConstraint result) `shouldBe` []

        it "binds let RHS nodes to the let-introduced TyForall" $ do
            let expr = ELet "id" (ELam "x" (EVar "x")) (ELit (LInt 0))
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    bindParents = cBindParents constraint
                forallNode <- case [n | n@TyForall {} <- IntMap.elems nodes] of
                    [node] -> pure node
                    other -> expectationFailure ("Expected single Forall node, saw " ++ show other) >> pure (error "unreachable")
                bodyNode <- lookupNode nodes (tnBody forallNode)
                paramId <- case bodyNode of
                    TyArrow { tnDom = dom } -> pure dom
                    other -> expectationFailure ("Forall body is not a lambda: " ++ show other) >> pure (error "unreachable")
                case IntMap.lookup (getNodeId paramId) bindParents of
                    Just (parent, BindFlex) -> parent `shouldBe` tnId forallNode
                    Just (parent, flag) -> do
                        parent `shouldBe` tnId forallNode
                        flag `shouldBe` BindFlex
                    Nothing -> expectationFailure "Missing binding parent for lambda parameter"

        it "preserves inner let binding parents" $ do
            let expr =
                    ELet "x"
                        (ELet "y" (ELit (LInt 0)) (EVar "y"))
                        (EVar "x")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    bindParents = cBindParents constraint
                foralls <- case [n | n@TyForall {} <- IntMap.elems nodes] of
                    [f1, f2] -> pure (f1, f2)
                    other -> expectationFailure ("Expected two Forall nodes, saw " ++ show other) >> pure (error "unreachable")
                let (innerForall, _outerForall) = case foralls of
                        (f1, f2) ->
                            case IntMap.lookup (getNodeId (tnBody f1)) nodes of
                                Just TyBase {} -> (f1, f2)
                                _ -> (f2, f1)
                case IntMap.lookup (getNodeId (tnBody innerForall)) bindParents of
                    Just (parent, _) -> parent `shouldBe` tnId innerForall
                    Nothing -> expectationFailure "Missing binding parent for inner let RHS"

        it "binds explicit forall variables to their TyForall" $ do
            let ann = STForall "a" Nothing (STBase "Int")
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    bindParents = cBindParents constraint
                forallNode <- case [n | n@TyForall {} <- IntMap.elems nodes] of
                    [node] -> pure node
                    other -> expectationFailure ("Expected single Forall node, saw " ++ show other) >> pure (error "unreachable")
                let boundChildren =
                        [ NodeId childId
                        | (childId, (parent, _)) <- IntMap.toList bindParents
                        , parent == tnId forallNode
                        ]
                binderVars <- filterM (\nid -> do
                    node <- lookupNode nodes nid
                    pure $ case node of
                        TyVar {} -> True
                        _ -> False
                    ) boundChildren
                binderVars `shouldSatisfy` (not . null)

        it "produces a valid binding tree" $ do
            let expr =
                    ELet "id" (ELam "x" (EVar "x"))
                        (EApp (EVar "id") (ELit (LInt 1)))
            expectRight (inferConstraintGraph expr) $ \result ->
                checkBindingTree (crConstraint result) `shouldBe` Right ()

    describe "Expansion nodes" $ do
        -- Generalized lets expose a single expansion node s · τ that every call
        -- site reuses. For
        --   let f = λx.x in
        --     let tmp = f 1
        --     in  f True
        -- both applications of f must point at the same 'TyExp' node on the left
        -- of their instantiation edges, proving that Phase 1 does not duplicate
        -- schemes per use. See Note [Expansion nodes] for how s · τ is later
        -- instantiated.
        it "shares the same expansion node across multiple instantiations of a let-bound value" $ do
            let lam = ELam "x" (EVar "x")
                expr =
                    ELet "f" lam $
                        ELet "tmp"
                            (EApp (EVar "f") (ELit (LInt 1)))
                            (EApp (EVar "f") (ELit (LBool True)))
            expectRight (inferConstraintGraph expr) $ \result -> do
                let insts = cInstEdges (crConstraint result)
                -- Should contain 2 edges for the applications of 'f'
                -- Plus 2 edges for the usages of 'x' inside the lambda? No, inside 'lam' x is used once.
                -- Let's trace 'f' usages.
                -- EVar "f" -> allocExpNode -> TyExp1(Forall(lam))
                -- EVar "f" -> allocExpNode -> TyExp2(Forall(lam))
                -- So inst edges will have different LHS.
                length insts `shouldBe` 2
                length (nub (map instLeft insts)) `shouldBe` 2

                -- But they should wrap the same underlying Forall node
                let nodes = cNodes (crConstraint result)
                let checkBody lhsId = do
                        lhs <- lookupNode nodes lhsId
                        case lhs of
                            TyExp { tnBody = bodyId } -> pure bodyId
                            _ -> expectationFailure "Expected TyExp" >> pure (error "unreachable")

                bodyIds <- mapM checkBody (map instLeft insts)
                length (nub bodyIds) `shouldBe` 1

        it "allocates distinct expansion variables for independent lets" $ do
            let expr =
                    ELet "f" (ELam "x" (EVar "x")) $
                        ELet "g" (ELam "y" (EVar "y")) (EVar "g")
            expectRight (inferConstraintGraph expr) $ \result -> do
                -- We look for TyForall nodes now, as they represent the let bindings
                let forallNodes = [n | n@TyForall {} <- IntMap.elems (cNodes (crConstraint result))]
                -- One for 'f', one for 'g'.
                -- Wait, ELet "f" ... -> allocForall.
                -- ELet "g" ... -> allocForall.
                -- EVar "g" -> allocExpNode -> TyExp(Forall_g).
                -- Also inside f and g bodies there are usages of x and y (EVar "x", EVar "y") which create TyExps.
                -- So we check for unique Foralls.
                -- But wait, inside f: ELam "x" (EVar "x"). EVar "x" -> TyExp(x).
                -- inside g: ELam "y" (EVar "y"). EVar "y" -> TyExp(y).
                -- So there are TyExps for x and y.
                -- And TyExp for usage of g.
                -- We specifically want to check the Let bindings.
                length forallNodes `shouldBe` 2

        it "emits one instantiation edge per application" $ do
            let lam = ELam "x" (EVar "x")
                expr =
                    ELet "f" lam $
                        ELet "a" (EApp (EVar "f") (ELit (LInt 1))) $
                            ELet "b" (EApp (EVar "f") (ELit (LBool True)))
                                (EApp (EVar "f") (ELit (LString "ok")))
            expectRight (inferConstraintGraph expr) $ \result -> do
                let insts = cInstEdges (crConstraint result)
                length insts `shouldBe` 3
                length (nub (map instLeft insts)) `shouldBe` 3 -- Each usage has fresh TyExp

                -- Verify same underlying source
                let nodes = cNodes (crConstraint result)
                let checkBody lhsId = do
                        lhs <- lookupNode nodes lhsId
                        case lhs of
                            TyExp { tnBody = bodyId } -> pure bodyId
                            _ -> expectationFailure "Expected TyExp" >> pure (error "unreachable")
                bodyIds <- mapM checkBody (map instLeft insts)
                length (nub bodyIds) `shouldBe` 1

    describe "Higher-order structure" $ do
        it "creates nested arrow nodes and one instantiation for higher-order lambdas" $ do
            let expr = ELam "x" (ELam "y" (EApp (EVar "x") (EVar "y")))
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = IntMap.elems (cNodes constraint)
                    arrowNodes = [n | n@TyArrow {} <- nodes]
                length arrowNodes `shouldSatisfy` (>= 2)
                cInstEdges constraint `shouldSatisfy` ((== 1) . length)

expectRight :: (Show e) => Either e a -> (a -> Expectation) -> Expectation
expectRight value k =
    case value of
        Left err -> expectationFailure $ "Expected success, but got: " ++ show err
        Right result -> k result

lookupNode :: IntMap.IntMap TyNode -> NodeId -> IO TyNode
lookupNode table nid =
    case IntMap.lookup (getNodeId nid) table of
        Just node -> pure node
        Nothing -> do
            expectationFailure $ "Missing node: " ++ show nid
            pure (error "unreachable: missing TyNode")
