module ConstraintGenSpec (spec) where

import Control.Monad (filterM, forM, when)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Test.Hspec

import MLF.Binding.Tree (boundFlexChildren, checkBindingTree, isUnderRigidBinder, nodeKind, NodeKind(..))
import MLF.Constraint.Acyclicity (checkAcyclicity)
import MLF.Constraint.Normalize (normalize)
import MLF.Constraint.Presolution (PresolutionResult(..), computePresolution)
import MLF.Constraint.Solve (SolveResult(..), solveUnify)
import MLF.Frontend.ConstraintGen (AnnExpr (..))
import MyLib hiding (normalize, lookupNode)
import SpecUtil
    ( expectRight
    , firstShowE
    , lookupNode
    , lookupNodeMaybe
    , nodeMapElems
    , nodeMapSize
    , requireRight
    , mkForalls
    )

inferConstraintGraphDefault :: SurfaceExpr -> Either ConstraintError ConstraintResult
inferConstraintGraphDefault expr =
    case normalizeExpr expr of
        Left err -> error ("normalizeExpr failed in test: " ++ show err)
        Right normExpr -> inferConstraintGraph Set.empty normExpr

spec :: Spec
spec = describe "Phase 1 — Constraint generation" $ do
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
                polySyms = Set.fromList [BaseTy "Int"]
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
                    ALet _ _ schemeRoot' _ _ _ bodyAnn' resNode' ->
                        pure (schemeRoot', bodyAnn', resNode')
                    other -> expectationFailure ("Expected ALet annotation, saw " ++ show other) >> fail "no schemeRoot"
                resNode `shouldBe` crRoot result
                case lookupNodeMaybe nodes resNode of
                    Just TyVar{} -> pure ()
                    other -> expectationFailure $ "Root is not the trivial scheme var: " ++ show other
                case bodyAnn of
                    AAnn (AVar "x" useNode) annNode edgeId -> do
                        annNode `shouldBe` resNode
                        case lookupNodeMaybe nodes useNode of
                            Just TyExp { tnBody = bodyId } -> bodyId `shouldBe` schemeRoot
                            other -> expectationFailure $ "Expected TyExp use of let-bound x, saw " ++ show other
                        let matchingEdges =
                                [ (instLeft edge, instRight edge)
                                | edge@(InstEdge eid _ _) <- cInstEdges constraint
                                , eid == edgeId
                                ]
                        matchingEdges `shouldBe` [(useNode, resNode)]
                    other ->
                        expectationFailure $ "Expected let body annotation to be AAnn, saw " ++ show other

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
                    ALet _ _ _ _ _ _ (AAnn innerAnn _ _) _ ->
                        case innerAnn of
                            ALet _ _ schemeRoot' _ _ _ bodyAnn' _ ->
                                pure (schemeRoot', bodyAnn')
                            other ->
                                expectationFailure ("Expected nested ALet annotation, saw " ++ show other) >> fail "no schemeRoot"
                    other -> expectationFailure ("Expected nested ALet annotation, saw " ++ show other) >> fail "no schemeRoot"
                case innerBodyAnn of
                    AAnn (AVar "x" useNode) _ _ -> do
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

    describe "Applications" $ do
        it "emits instantiation edges for both function and argument" $ do
            let expr = EApp (ELam "x" (EVar "x")) (ELit (LInt 1))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    instEdges = cInstEdges constraint
                length instEdges `shouldBe` 2
                case crAnnotated result of
                    AApp _ _ funEid argEid _ -> do
                        funEid `shouldNotBe` argEid
                        let edgeIds = [eid | InstEdge eid _ _ <- instEdges]
                        edgeIds `shouldSatisfy` elem funEid
                        edgeIds `shouldSatisfy` elem argEid
                    other ->
                        expectationFailure $ "Expected application annotation, saw " ++ show other

    describe "Annotated Terms" $ do
        it "desugars annotated lambda parameters via let" $ do
            -- Thesis sugar (Chapter 12.3.2):
            --   λ(x : τ) a  ≜  λ(x) let x = (x : τ) in a  ≜  λ(x) let x = cτ x in a
            --
            -- So Phase 1 should see an ordinary lambda whose body is a let-binding
            -- with a coercion application as the RHS. The coercion creates a type
            -- variable with the annotated type as its bound.
            let ann = STBase "Int"
                expr = ELamAnn "x" ann (EVar "x")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case crAnnotated result of
                    ALam _ lamParam _ bodyAnn _ ->
                        case bodyAnn of
                            ALet "x" _ schemeRoot _ _ rhsAnn bodyAnn' _ -> do
                                schemeNode <- lookupNode nodes schemeRoot
                                -- With coercion-based desugaring, the scheme root is a type
                                -- variable (the coercion's codomain), not the base type directly
                                case schemeNode of
                                    TyVar { tnBound = Just boundId } -> do
                                        boundNode <- lookupNode nodes boundId
                                        case boundNode of
                                            TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                                            other -> expectationFailure $ "Expected Int bound, saw " ++ show other
                                    other -> expectationFailure $ "Expected TyVar with bound, saw " ++ show other
                                case rhsAnn of
                                    AAnn (AVar "x" rhsUse) _ _ -> rhsUse `shouldBe` lamParam
                                    other -> expectationFailure $ "Expected annotated RHS, saw " ++ show other
                                case bodyAnn' of
                                    AAnn (AVar "x" useNode) _ _ -> do
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
                    ALet _ _ schemeRoot' _ _ _ _ _ -> pure schemeRoot'
                    other -> expectationFailure ("Expected ALet annotation, saw " ++ show other) >> fail "no schemeRoot"
                body <- lookupNode nodes schemeRoot
                -- Expect a scheme root Arrow with shared dom/cod.
                case body of
                    TyArrow { tnDom = domId, tnCod = codId } -> do
                        domId `shouldBe` codId
                    other -> expectationFailure $ "Expected Arrow scheme root, saw " ++ show other

        it "respects term annotations" $ do
            -- (1 : Int)
            let ann = STBase "Int"
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                case lookupNodeMaybe nodes (crRoot result) of
                    Just TyVar { tnBound = Just bnd } -> do
                        bndNode <- lookupNode nodes bnd
                        case bndNode of
                            TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                            other -> expectationFailure $ "Expected Int bound, saw " ++ show other
                    other -> expectationFailure $ "Expected Int node, saw " ++ show other

        it "respects bounded quantification in term annotations (coercion)" $ do
            -- let f = (λx. x : ∀(a ⩾ Int). a -> a) in f
            -- The annotation is a term coercion with bounded quantification.
            let ann = mkForalls [("a", Just (STBase "Int"))] (STArrow (STVar "a") (STVar "a"))
                expr = ELet "f" (EAnn (ELam "x" (EVar "x")) ann) (EVar "f")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                schemeRoot <- case crAnnotated result of
                    ALet _ _ schemeRoot' _ _ _ _ _ -> pure schemeRoot'
                    other -> expectationFailure ("Expected ALet annotation, saw " ++ show other) >> fail "no schemeRoot"
                body <- lookupNode nodes schemeRoot
                -- Expect a scheme root Arrow with bounded dom.
                case body of
                    TyArrow { tnDom = domId } -> do
                        -- domId is 'a'. Its bound is recorded on the TyVar node.
                        domNode <- lookupNode nodes domId
                        case domNode of
                            TyVar{ tnBound = mb } -> do
                                case mb of
                                    Just boundId -> do
                                        rhs <- lookupNode nodes boundId
                                        case rhs of
                                            TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                                            other -> expectationFailure $ "Expected bound Int, saw " ++ show other
                                    Nothing ->
                                        expectationFailure "Expected bound for variable, saw Nothing"
                            other ->
                                expectationFailure $ "Expected TyVar { tnId = for, tnBound = Nothing } domain, saw " ++ show other
                    other -> expectationFailure $ "Expected Arrow scheme root, saw " ++ show other

        it "respects instance bounds in annotated lambda parameters (coercion)" $ do
            -- λ(x : ∀(a ⩾ Int). a). x desugars to a let-binding with a coercion term.
            -- Uses of x in the body go through the coercion result type.
            let ann = STForall "a" (Just (mkSrcBound (STBase "Int"))) (STVar "a")
                expr = ELamAnn "x" ann (EVar "x")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = cNodes constraint
                case crAnnotated result of
                    ALam _ lamParam _ bodyAnn _ ->
                        case bodyAnn of
                            ALet "x" _ schemeRoot _ _ rhsAnn bodyAnn' _ -> do
                                case rhsAnn of
                                    AAnn (AVar "x" rhsUse) _ _ -> rhsUse `shouldBe` lamParam
                                    other -> expectationFailure $ "Expected annotated RHS, saw " ++ show other
                                case bodyAnn' of
                                    AAnn (AVar "x" useNode) _ _ -> do
                                        case lookupNodeMaybe nodes useNode of
                                            Just TyExp { tnBody = bodyId } -> bodyId `shouldBe` schemeRoot
                                            other -> expectationFailure $ "Expected TyExp for polymorphic x, saw " ++ show other
                                    other -> expectationFailure $ "Expected annotated let body, saw " ++ show other
                                annVar <- lookupNode nodes schemeRoot
                                case annVar of
                                    TyVar { tnBound = Just boundId } -> do
                                        rhs <- lookupNode nodes boundId
                                        case rhs of
                                            TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                                            other -> expectationFailure $ "Expected bound Int, saw " ++ show other
                                    TyVar { tnBound = Nothing } ->
                                        expectationFailure "Expected bound for variable, saw Nothing"
                                    other -> expectationFailure $ "Expected TyVar scheme root node, saw " ++ show other
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
                    ALam _ lamParam _ bodyAnn _ ->
                        case bodyAnn of
                            ALet "x" _ schemeRoot _ _ rhsAnn bodyAnn' _ -> do
                                case rhsAnn of
                                    AAnn (AVar "x" rhsUse) _ _ -> rhsUse `shouldBe` lamParam
                                    other -> expectationFailure $ "Expected annotated RHS, saw " ++ show other
                                case bodyAnn' of
                                    AAnn (AVar "x" useNode) _ _ -> do
                                        useTy <- lookupNode nodes useNode
                                        case useTy of
                                            TyExp { tnBody = bodyId } -> bodyId `shouldBe` schemeRoot
                                            other -> expectationFailure $ "Expected TyExp use of let-bound x, saw " ++ show other
                                    other -> expectationFailure $ "Expected annotated let body, saw " ++ show other
                                annNode' <- lookupNode nodes schemeRoot
                                case annNode' of
                                    -- Bottom is internalized as a fresh TyVar.
                                    TyVar {} -> pure ()
                                    other -> expectationFailure $ "Expected TyVar scheme root for Bottom, saw " ++ show other
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
                    ALet _ _ _ _ _ _ bodyAnn _ ->
                        case bodyAnn of
                            AAnn innerAnn _ _ ->
                                case innerAnn of
                                    AAnn _ _ edgeId ->
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
                                        expectationFailure $ "Expected inner annotation to be AAnn, saw " ++ show other
                            other ->
                                expectationFailure $ "Expected let body annotation to be AAnn, saw " ++ show other
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
                     ALet name schemeGen _ _ rhsGen rhsAnn bodyAnn _resNode -> do
                         name `shouldBe` "x"
                         schemeGen `shouldBe` rhsGen
                         -- Basic structural check
                         case rhsAnn of
                             ALit (LInt 1) _ -> pure ()
                             _ -> expectationFailure "RHS annotation mismatch"
                         case bodyAnn of
                             AAnn (AVar "x" _) _ _ -> pure ()
                             _ -> expectationFailure "Body annotation mismatch"
                     _ -> expectationFailure "Expected ALet annotation"

    describe "Lambda nodes" $ do
        -- A λx.x term yields a single parameter node whose identifier appears in
        -- both the domain and codomain of the arrow node, reflecting the shared
        -- variable in the graphic type.
        it "builds a shared parameter node when translating lambdas" $ do
            let expr = ELam "x" (EVar "x")
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let nodes = nodeMapElems (cNodes (crConstraint result))
                    arrowNodes = [n | n@TyArrow {} <- nodes]
                    varNodes = [n | n@TyVar {} <- nodes]
                    expNodes = [n | n@TyExp {} <- nodes]

                -- Monomorphic bindings (lambda parameters) do not introduce `TyExp`.
                case (arrowNodes, expNodes) of
                    ([TyArrow { tnId = arrowId, tnDom = dom, tnCod = cod }], []) -> do
                        dom `shouldBe` cod
                        let hasParam =
                                any
                                    (\n -> case n of
                                        TyVar{ tnId = nid } -> nid == dom
                                        _ -> False)
                                    varNodes
                            hasRoot =
                                any
                                    (\n -> case n of
                                        TyVar{ tnBound = Just bnd } -> bnd == arrowId
                                        _ -> False)
                                    varNodes
                        if hasParam && hasRoot
                            then pure ()
                            else expectationFailure $ "Unexpected lambda nodes: " ++ show (length arrowNodes, length varNodes, length expNodes)
                    _ ->
                        expectationFailure $ "Unexpected lambda nodes: " ++ show (length arrowNodes, length varNodes, length expNodes)

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
                (funEid, argEid, appResult) <- case crAnnotated result of
                    ALet _ _ _ _ _ _ bodyAnn _ ->
                        case bodyAnn of
                            AAnn (AApp _ _ funEid' argEid' resNode) _ _ ->
                                pure (funEid', argEid', resNode)
                            other -> expectationFailure ("Expected AApp in let body, saw " ++ show other) >> fail "no app"
                    other -> expectationFailure ("Expected ALet annotation, saw " ++ show other) >> fail "no let"
                let lookupEdge eid =
                        case [edge | edge@(InstEdge eid' _ _) <- insts, eid' == eid] of
                            [edge] -> pure edge
                            other -> expectationFailure ("Expected inst edge " ++ show eid ++ ", saw " ++ show other) >> fail "missing edge"
                funEdge <- lookupEdge funEid
                argEdge <- lookupEdge argEid
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
                                        domNode <- lookupNode nodes domId
                                        codNode <- lookupNode nodes codId
                                        case (domNode, codNode) of
                                            (TyVar { tnId = domVar }, TyVar { tnId = codVar }) ->
                                                domVar `shouldBe` codVar
                                            other ->
                                                expectationFailure $ "Lambda arrow points to unexpected nodes: " ++ show other
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
                case cInstEdges constraint of
                    [edge0, edge1] -> do
                        let (funEdge, _argEdge) =
                                case lookupNodeMaybe nodes (instLeft edge0) of
                                    Just TyVar{ tnBound = Just boundId } ->
                                        case lookupNodeMaybe nodes boundId of
                                            Just TyArrow{} -> (edge0, edge1)
                                            _ -> (edge1, edge0)
                                    _ -> (edge1, edge0)
                        lhs <- lookupNode nodes (instLeft funEdge)
                        case lhs of
                            TyVar { tnBound = Just boundId } -> do
                                boundNode <- lookupNode nodes boundId
                                case boundNode of
                                    TyArrow {} -> pure ()
                                    other -> expectationFailure $ "Instantiation left-hand side is not an arrow: " ++ show other
                            other -> expectationFailure $ "Instantiation left-hand side is not a lambda root var: " ++ show other
                    other -> expectationFailure $ "Expected two instantiation edges, saw " ++ show (length other)

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
                    ALet _ _ _ _ _ _ bodyAnn resNode ->
                        case bodyAnn of
                            AAnn (ALit (LInt 0) litNode) annNode edgeId -> do
                                annNode `shouldBe` resNode
                                case [edge | edge@(InstEdge eid _ _) <- cInstEdges constraint, eid == edgeId] of
                                    [InstEdge _ left right] -> do
                                        left `shouldBe` litNode
                                        right `shouldBe` annNode
                                    other ->
                                        expectationFailure $ "Expected 1 let-expression inst edge, saw " ++ show other
                            other ->
                                expectationFailure $ "Expected let body annotation to be AAnn, saw " ++ show other
                    other ->
                        expectationFailure $ "Expected ALet annotation, saw " ++ show other

        it "binds let RHS nodes to the let-introduced gen node" $ do
            let expr = ELet "id" (ELam "x" (EVar "x")) (ELit (LInt 0))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    bindParents = cBindParents constraint
                (schemeGen, paramId) <- case crAnnotated result of
                    ALet _ schemeGen _ _ _ rhsAnn _ _ ->
                        case rhsAnn of
                            ALam _ param _ _ _ -> pure (schemeGen, param)
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
                    ALet _ _ _ _ _ rhsAnn _ _ ->
                        case rhsAnn of
                            ALet _ schemeGen schemeRoot _ _ _ _ _ ->
                                pure (schemeGen, schemeRoot)
                            other ->
                                expectationFailure ("Expected inner ALet annotation, saw " ++ show other) >> fail "no inner let"
                    other -> expectationFailure ("Expected nested ALet annotation, saw " ++ show other) >> fail "no inner let"
                case IntMap.lookup (nodeRefKey (typeRef innerRoot)) bindParents of
                    Just (parent, _) -> parent `shouldBe` genRef innerGen
                    Nothing -> expectationFailure "Missing binding parent for inner let RHS"

        it "binds explicit forall variables under a gen node" $ do
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
                let boundChildren =
                        [ nid
                        | (childKey, (parent, _)) <- IntMap.toList bindParents
                        , parent == genRef schemeGen
                        , TypeRef nid <- [nodeRefFromKey childKey]
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
            expectRight (inferConstraintGraphDefault expr) $ \result ->
                checkBindingTree (crConstraint result) `shouldBe` Right ()

        it "elimination rewrite removes eliminated binders from Q(n)" $ do
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

                runToPresolution :: SurfaceExpr -> Either String PresolutionResult
                runToPresolution e = do
                    ConstraintResult { crConstraint = c0 } <- firstShowE (inferConstraintGraphDefault e)
                    let c1 = normalize c0
                    acyc <- firstShowE (checkAcyclicity c1)
                    firstShowE (computePresolution defaultTraceConfig acyc c1)

            pres <- requireRight (runToPresolution expr)
            let eliminated = cEliminatedVars (prConstraint pres)

            solved <- requireRight (solveUnify defaultTraceConfig (prConstraint pres))
            let cSolved = srConstraint solved
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
                length insts `shouldBe` 9
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
        it "creates nested arrow nodes and one instantiation for higher-order lambdas" $ do
            let expr = ELam "x" (ELam "y" (EApp (EVar "x") (EVar "y")))
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    nodes = nodeMapElems (cNodes constraint)
                    arrowNodes = [n | n@TyArrow {} <- nodes]
                length arrowNodes `shouldSatisfy` (>= 2)
                cInstEdges constraint `shouldSatisfy` ((== 2) . length)

    describe "Coercion semantics (thesis-exact)" $ do
        -- US-004: Regression tests for thesis-exact coercion behavior
        -- These tests lock in the rigid domain / flexible codomain semantics
        -- described in papers/these-finale-english.txt §12.3.2.2, §15.3.8

        it "coercion domain nodes are restricted but not locked" $ do
            -- (1 : Int) - the domain copy should be restricted (rigid binding edge)
            -- but not locked (no rigid ancestor)
            let ann = STBase "Int"
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    insts = cInstEdges constraint
                -- Find the annotation edge (the one with domain as right side)
                case insts of
                    [InstEdge _ _ domainNode] -> do
                        -- Domain node should be restricted (rigid binding edge)
                        -- but not locked (no rigid ancestor)
                        kind <- case nodeKind constraint (typeRef domainNode) of
                            Right k -> pure k
                            Left err -> expectationFailure (show err) >> pure NodeRoot
                        kind `shouldBe` NodeRestricted
                        -- "Locked" means flex edge but rigid ancestor; ensure there is
                        -- no strict rigid ancestor on the binding path.
                        underRigid <- case isUnderRigidBinder constraint (typeRef domainNode) of
                            Right b -> pure b
                            Left err -> expectationFailure (show err) >> pure True
                        underRigid `shouldBe` False
                    other -> expectationFailure $ "Expected 1 inst edge, saw " ++ show (length other)

        it "EAnn returns the codomain copy (not domain)" $ do
            -- (1 : Int) - the result type should be the codomain copy
            -- which is distinct from the domain node on the inst edge
            let ann = STBase "Int"
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let constraint = crConstraint result
                    insts = cInstEdges constraint
                    root = crRoot result
                case insts of
                    [InstEdge _ _ domainNode] -> do
                        -- The root (annotation result) should NOT be the domain node
                        -- It should be the codomain copy
                        root `shouldNotBe` domainNode
                        -- Both should be TyVar nodes bound to Int
                        let nodes = cNodes constraint
                        rootNode <- lookupNode nodes root
                        domNode <- lookupNode nodes domainNode
                        case (rootNode, domNode) of
                            (TyVar { tnBound = Just rootBnd }, TyVar { tnBound = Just domBnd }) -> do
                                rootBndNode <- lookupNode nodes rootBnd
                                domBndNode <- lookupNode nodes domBnd
                                case (rootBndNode, domBndNode) of
                                    (TyBase { tnBase = BaseTy n1 }, TyBase { tnBase = BaseTy n2 }) -> do
                                        n1 `shouldBe` "Int"
                                        n2 `shouldBe` "Int"
                                    other -> expectationFailure $ "Expected Int bases, saw " ++ show other
                            other -> expectationFailure $ "Expected TyVar nodes, saw " ++ show other
                    other -> expectationFailure $ "Expected 1 inst edge, saw " ++ show (length other)

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
                    [InstEdge _ _ domainNode] -> do
                        -- For a free type variable, domain and codomain should
                        -- share the same node (existential sharing)
                        root `shouldBe` domainNode
                    other -> expectationFailure $ "Expected 1 inst edge, saw " ++ show (length other)

    describe "Constructor types (STCon)" $ do
        it "internalizes STCon annotations into TyCon nodes" $ do
            -- (1 : List Int) - should create a TyCon node with head "List" and one arg
            let ann = STCon "List" (STBase "Int" :| [])
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                    tyConNodes = [n | n@TyCon{} <- nodeMapElems nodes]
                length tyConNodes `shouldBe` 2  -- domain + codomain copies
                case tyConNodes of
                    (TyCon { tnCon = BaseTy name, tnArgs = args }:_) -> do
                        name `shouldBe` "List"
                        length args `shouldBe` 1
                    _ -> expectationFailure "Expected TyCon nodes"

        it "internalizes nested STCon annotations" $ do
            -- (1 : Either Int Bool) - should create a TyCon node with two args
            let ann = STCon "Either" (STBase "Int" :| [STBase "Bool"])
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                    tyConNodes = [n | n@TyCon{} <- nodeMapElems nodes]
                length tyConNodes `shouldBe` 2  -- domain + codomain copies
                case tyConNodes of
                    (TyCon { tnCon = BaseTy name, tnArgs = args }:_) -> do
                        name `shouldBe` "Either"
                        length args `shouldBe` 2
                    _ -> expectationFailure "Expected TyCon nodes"

        it "TyCon args are correctly structured" $ do
            -- (1 : List Int) - the arg should be an Int base type
            let ann = STCon "List" (STBase "Int" :| [])
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \result -> do
                let nodes = cNodes (crConstraint result)
                    tyConNodes = [n | n@TyCon{} <- nodeMapElems nodes]
                case tyConNodes of
                    (TyCon { tnArgs = (argId :| _) }:_) -> do
                        argNode <- lookupNode nodes argId
                        case argNode of
                            TyVar { tnBound = Just boundId } -> do
                                boundNode <- lookupNode nodes boundId
                                case boundNode of
                                    TyBase { tnBase = BaseTy name } -> name `shouldBe` "Int"
                                    other -> expectationFailure $ "Expected Int base, saw " ++ show other
                            other -> expectationFailure $ "Expected TyVar arg, saw " ++ show other
                    _ -> expectationFailure "Expected TyCon nodes"

    describe "Type constructor arity validation" $ do
        it "throws TypeConstructorArityMismatch on conflicting arities" $ do
            -- Use List with arity 1, then with arity 2
            let ann1 = STCon "List" (STBase "Int" :| [])
                ann2 = STCon "List" (STBase "Int" :| [STBase "Bool"])
                -- Create a type that uses List with different arities
                ann = STArrow ann1 ann2
                expr = EAnn (ELit (LInt 1)) ann
            case inferConstraintGraphDefault expr of
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
            case inferConstraintGraphDefault expr of
                Left (ForallBoundMentionsBinder name) -> name `shouldBe` "a"
                Left other -> expectationFailure $ "Expected ForallBoundMentionsBinder, saw " ++ show other
                Right _ -> expectationFailure "Expected ForallBoundMentionsBinder error"

        it "allows binder in body but not in bound" $ do
            -- ∀(a ⩾ Int). a - valid: 'a' is in body but not in bound
            let ann = STForall "a" (Just (mkSrcBound (STBase "Int"))) (STVar "a")
                expr = EAnn (ELit (LInt 1)) ann
            expectRight (inferConstraintGraphDefault expr) $ \_ -> pure ()

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
                    ALet _ _ schemeRoot' _ _ _ _ _ -> pure schemeRoot'
                    other -> expectationFailure ("Expected ALet annotation, saw " ++ show other) >> fail "no schemeRoot"

                -- The scheme root should be a TyArrow (the lambda's type), not a
                -- TyForall (which would indicate declared-scheme behavior)
                schemeNode <- lookupNode nodes schemeRoot
                case schemeNode of
                    TyArrow { tnDom = domId, tnCod = codId } -> do
                        -- The identity function has shared domain/codomain
                        domId `shouldBe` codId
                    other ->
                        expectationFailure $
                            "Expected TyArrow scheme root (inferred type), saw " ++ show other ++
                            ". This may indicate declared-scheme semantics are still present."

                -- There should be no TyForall nodes in the constraint (no declared scheme)
                let forallNodes = [n | n@TyForall{} <- nodeMapElems nodes]
                forallNodes `shouldBe` []

                -- The use of 'id' in the body should have a TyExp node (normal let-polymorphism)
                -- not a direct link to a TyForall scheme
                case crAnnotated result of
                    ALet _ _ _ _ _ _ bodyAnn _ ->
                        case bodyAnn of
                            AAnn (AVar "id" useNode) _ _ -> do
                                useTy <- lookupNode nodes useNode
                                case useTy of
                                    TyExp {} -> pure () -- Normal let-polymorphic use
                                    other -> expectationFailure $
                                        "Expected TyExp for let-bound use, saw " ++ show other
                            other -> expectationFailure $
                                "Expected annotated var in body, saw " ++ show other
                    other -> expectationFailure $
                        "Expected ALet annotation, saw " ++ show other
