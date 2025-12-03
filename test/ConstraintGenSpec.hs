module ConstraintGenSpec (spec) where

import Data.List (nub)
import Data.Maybe (catMaybes)
import qualified Data.IntMap.Strict as IntMap
import Test.Hspec

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
                        body <- lookupNode nodes bodyId
                        case body of
                            TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                            other -> expectationFailure $ "Expected Int body, saw " ++ show other
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
                    Just TyExp { tnBody = bodyId } ->
                        case IntMap.lookup (getNodeId bodyId) nodes of
                            Just TyBase { tnBase = BaseTy name } -> name `shouldBe` "Bool"
                            other -> expectationFailure $ "Expected Bool body, saw " ++ show other
                    other -> expectationFailure $ "Root is not an expansion node: " ++ show other

        it "reports unknown variables" $ do
            inferConstraintGraph (EVar "free") `shouldBe` Left (UnknownVariable "free")

        it "reports unknown variables that appear inside let RHS" $ do
            let expr = ELet "x" (EVar "ghost") (ELit (LInt 0))
            inferConstraintGraph expr `shouldBe` Left (UnknownVariable "ghost")

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
                case (arrowNodes, varNodes) of
                    ([TyArrow { tnDom = dom, tnCod = cod }], [TyVar { tnId = varId }]) -> do
                        dom `shouldBe` varId
                        cod `shouldBe` varId
                    _ ->
                        expectationFailure $ "Unexpected lambda nodes: " ++ show (length arrowNodes, length varNodes)

        -- Each lambda parameter should be registered with the G-node that
        -- represents its scope so Phase 2+ know which variables can be
        -- generalized. For λx.x the parameter lives at the root level g₀, so we
        -- ensure that the TyVar id for x appears inside gBinds(g₀).
        it "records lambda parameters at the surrounding binding level" $ do
            let expr = ELam "x" (EVar "x")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                arrow <- case IntMap.lookup (getNodeId (crRoot result)) nodes of
                    Just node@TyArrow {} -> pure node
                    other -> expectationFailure ("Root is not a lambda arrow: " ++ show other) >> pure (error "unreachable")
                rootLevel <- case cGForest constraint of
                    [gid] -> pure gid
                    other -> expectationFailure ("Unexpected forest: " ++ show other) >> pure (error "unreachable")
                rootNode <- lookupGNode (cGNodes constraint) rootLevel
                (tnDom arrow `elem` gBinds rootNode) `shouldBe` True

        it "records lambda parameters inside child binding levels" $ do
            let expr = ELet "f" (ELam "x" (EVar "x")) (ELit (LInt 0))
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    gnodes = cGNodes constraint
                -- Locate the TyExp introduced for the let RHS by scanning nodes.
                expNode <- case [n | n@TyExp {} <- IntMap.elems nodes] of
                    [node] -> pure node
                    other -> expectationFailure ("Expected single expansion node, saw " ++ show other) >> pure (error "unreachable")
                bodyNode <- lookupNode nodes (tnBody expNode)
                paramId <- case bodyNode of
                    TyArrow { tnDom = dom } -> pure dom
                    other -> expectationFailure ("Expansion body is not a lambda: " ++ show other) >> pure (error "unreachable")
                rootId <- case cGForest constraint of
                    [gid] -> pure gid
                    other -> expectationFailure ("Unexpected forest: " ++ show other) >> pure (error "unreachable")
                rootNode <- lookupGNode gnodes rootId
                childId <- case gChildren rootNode of
                    [cid] -> pure cid
                    other -> expectationFailure ("Expected single child, saw " ++ show other) >> pure (error "unreachable")
                childNode <- lookupGNode gnodes childId
                paramId `elem` gBinds childNode `shouldBe` True

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
                            TyExp { tnBody = bodyId } -> do
                                body <- lookupNode nodes bodyId
                                case body of
                                    TyArrow { tnDom = domId, tnCod = codId } -> do
                                        domNode <- lookupNode nodes domId
                                        codNode <- lookupNode nodes codId
                                        case (domNode, codNode) of
                                            (TyVar { tnId = domVar }, TyVar { tnId = codVar }) ->
                                                domVar `shouldBe` codVar
                                            other -> expectationFailure $ "Lambda arrow points to unexpected nodes: " ++ show other
                                    other -> expectationFailure $ "Expansion body is not a lambda arrow: " ++ show other
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
                length (nub lhsIds) `shouldBe` 1
                paramVar <- case lhsIds of
                    (lhsId:_) -> lookupNode nodes lhsId
                    [] -> expectationFailure "Missing instantiation left-hand side" >> pure (error "unreachable")
                case paramVar of
                    TyVar { tnLevel = level } -> do
                        rootLevel <- case cGForest constraint of
                            [gid] -> pure gid
                            other -> expectationFailure ("Unexpected forest: " ++ show other) >> pure (error "unreachable")
                        level `shouldBe` rootLevel
                    other -> expectationFailure $ "Instantiation left-hand side is not a parameter TyVar: " ++ show other

        -- Applying an un-generalized lambda must leave the result at the caller
        -- level (g₀ here) instead of allocating a fresh child G-node, otherwise
        -- the returned TyVar would be eligible for quantification it should not
        -- receive. This ensures application results inherit the surrounding
        -- generalization level. Type tree view for the identity call:
        --   TyVar ρ (level g₀)
        --   └─ TyArrow λx.x (level g₀)
        --      ├─ dom: TyBase "Int"
        --      └─ cod: TyVar ρ
        it "keeps application results at the current generalization level" $ do
            let expr = EApp (ELam "x" (EVar "x")) (ELit (LInt 0))
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case IntMap.lookup (getNodeId (crRoot result)) nodes of
                    Just TyVar { tnLevel = level } -> do
                        rootLevel <- case cGForest constraint of
                            [gid] -> pure gid
                            other -> expectationFailure ("Unexpected forest: " ++ show other) >> pure (error "unreachable")
                        level `shouldBe` rootLevel
                    other -> expectationFailure $ "Application result is not a TyVar: " ++ show other

    describe "Let-generalization and G-nodes" $ do
        it "does not emit instantiation edges for unused let bindings" $ do
            let expr =
                    ELet "f" (ELam "x" (EVar "x"))
                        (ELit (LInt 0))
            expectRight (inferConstraintGraph expr) $ \result -> do
                cInstEdges (crConstraint result) `shouldBe` []

        -- Let-bound RHS terms allocate a fresh G-node child under the current
        -- generalization level so that any TyVar nodes introduced in the binding
        -- can later be quantified. In the graphic constraint this manifests as the
        -- root level g₀ gaining a child g₁ such that gParent(g₁) = g₀ and gChildren(g₀)
        -- contains g₁. This test inspects the forest and G-node table to confirm
        -- that exact lattice fragment g₀ → g₁ exists.
        it "creates a child generalization node for let-bindings" $ do
            let expr = ELet "id" (ELam "x" (EVar "x")) (EVar "id")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    forest = cGForest constraint
                    gnodes = cGNodes constraint
                case forest of
                    [rootId] -> do
                        case IntMap.lookup (getGNodeId rootId) gnodes of
                            Nothing -> expectationFailure "Missing root G-node"
                            Just rootNode ->
                                case gChildren rootNode of
                                    (childId:_) ->
                                        case IntMap.lookup (getGNodeId childId) gnodes of
                                            Nothing -> expectationFailure "Missing child G-node"
                                            Just childNode -> gParent childNode `shouldBe` Just rootId
                                    [] -> expectationFailure "Root G-node has no children"
                    other -> expectationFailure $ "Unexpected root forest shape: " ++ show other

        it "stores RHS lambda parameters inside the child generalization node" $ do
            let expr = ELet "id" (ELam "x" (EVar "x")) (ELit (LInt 0))
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                    gnodes = cGNodes constraint
                rootId <- case cGForest constraint of
                    [gid] -> pure gid
                    other -> expectationFailure ("Unexpected forest: " ++ show other) >> pure (error "unreachable")
                rootNode <- lookupGNode gnodes rootId
                childId <- case gChildren rootNode of
                    [cid] -> pure cid
                    other -> expectationFailure ("Expected single child, saw " ++ show other) >> pure (error "unreachable")
                childNode <- lookupGNode gnodes childId
                let childVars = [tnId n | n@TyVar { tnLevel = level } <- IntMap.elems nodes, level == childId]
                childVars `shouldSatisfy` (not . null)
                all (`elem` gBinds childNode) childVars `shouldBe` True

        -- Nested lets should form a linear chain g₀ → g₁ → g₂ so that each inner
        -- binding knows its parent level. For
        --   let x = (let y = 0 in y) in x
        -- we expect the outer binding to create g₁ under g₀ and the inner binding
        -- to create g₂ under g₁. If we annotate the AST with levels we get:
        --       Program root (g₀)
        --       └─ Let x (g₁)
        --          ├─ bind: Let y (g₂)
        --          │        ├─ bind: 0
        --          │        └─ body: y
        --          └─ body: x
        -- A linear parent list therefore fully captures the structure. This check
        -- ensures the stored 'gParent' links match that nesting.
        it "links nested lets through parent G-nodes" $ do
            let expr =
                    ELet "x"
                        (ELet "y" (ELit (LInt 0)) (EVar "y"))
                        (EVar "x")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    gnodes = cGNodes constraint
                IntMap.size gnodes `shouldBe` 3 -- root + inner + nested
                case cGForest constraint of
                    [rootId] -> do
                        rootNode <- lookupGNode gnodes rootId
                        case gChildren rootNode of
                            [childId] -> do
                                childNode <- lookupGNode gnodes childId
                                gParent childNode `shouldBe` Just rootId
                                case gChildren childNode of
                                    [grandChildId] -> do
                                        grandChild <- lookupGNode gnodes grandChildId
                                        gParent grandChild `shouldBe` Just childId
                                    other -> expectationFailure $ "Expected single grandchild, saw " ++ show other
                            other -> expectationFailure $ "Expected single child, saw " ++ show other
                    other -> expectationFailure $ "Unexpected root forest shape: " ++ show other

        it "creates sibling generalization nodes for sequential lets" $ do
            let expr =
                    ELet "x" (ELit (LInt 0)) $
                        ELet "y" (ELit (LBool True)) (ELit (LInt 1))
            expectRight (inferConstraintGraph expr) $ \result -> do
                let constraint = crConstraint result
                    gnodes = cGNodes constraint
                case cGForest constraint of
                    [rootId] -> do
                        rootNode <- lookupGNode gnodes rootId
                        let children = gChildren rootNode
                        length children `shouldBe` 2
                        length (nub children) `shouldBe` 2
                        all (\cid -> maybe False ((== Just rootId) . gParent) (IntMap.lookup (getGNodeId cid) gnodes)) children
                            `shouldBe` True
                    other -> expectationFailure $ "Unexpected root forest shape: " ++ show other

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
                length insts `shouldBe` 2
                length (nub (map instLeft insts)) `shouldBe` 1

        it "allocates distinct expansion variables for independent lets" $ do
            let expr =
                    ELet "f" (ELam "x" (EVar "x")) $
                        ELet "g" (ELam "y" (EVar "y")) (EVar "g")
            expectRight (inferConstraintGraph expr) $ \result -> do
                let expNodes = [n | n@TyExp {} <- IntMap.elems (cNodes (crConstraint result))]
                length expNodes `shouldBe` 2
                length (nub (map tnExpVar expNodes)) `shouldBe` 2

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
                length (nub (map instLeft insts)) `shouldBe` 1

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

lookupGNode :: IntMap.IntMap GNode -> GNodeId -> IO GNode
lookupGNode table gid =
    case IntMap.lookup (getGNodeId gid) table of
        Just node -> pure node
        Nothing -> do
            expectationFailure $ "Missing G-node: " ++ show gid
            pure (error "unreachable: missing G-node")
