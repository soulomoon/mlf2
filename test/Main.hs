module Main (main) where

import Data.List (nub)
import qualified Data.IntMap.Strict as IntMap
import Test.Hspec

import MyLib

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Phase 1 — Constraint generation" $ do
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

    -- Verify that application translation produces a single instantiation edge
    -- s τ ≤ (Int → α) where s is the expansion variable introduced for the
    -- let-bound value and τ ≡ (β → β) is the λ-bound body. The left-hand side is
    -- therefore the generalizable scheme and the right-hand side is the arrow
    -- demanded by the call site.
    -- In the constraint graph, an expansion node encodes the recipe for a
    -- let-generalized binding: it remembers the graphic type τ along with the
    -- meta-variable s that later receives a presolution (e.g. instantiate, add
    -- ∀-levels). Instantiation edges consume these nodes by forcing s ⋅ τ to
    -- match the shape required at each usage site.
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
