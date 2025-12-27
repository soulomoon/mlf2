module MLF.Frontend.ConstraintGen.Translate (
    buildRootExpr
) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (get)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import MLF.Constraint.Types
import MLF.Frontend.Syntax
import MLF.Frontend.Desugar (desugarCoercions)
import MLF.Frontend.ConstraintGen.Emit
import qualified MLF.Frontend.ConstraintGen.Scope as Scope
import MLF.Frontend.ConstraintGen.State (BuildState(..), ConstraintM)
import MLF.Frontend.ConstraintGen.Types

buildRootExpr :: NodeId -> Expr -> ConstraintM (NodeId, AnnExpr)
buildRootExpr scopeRoot expr = do
    (rootNode, annRoot) <- buildExpr Map.empty scopeRoot expr
    topFrame <- Scope.peekScope
    Scope.rebindScopeNodes rootNode rootNode topFrame
    pure (rootNode, annRoot)

buildExpr :: Env -> NodeId -> Expr -> ConstraintM (NodeId, AnnExpr)
buildExpr env scopeRoot expr = case expr of
  EVarRaw name -> do
    nid <- lookupVar env name
    pure (nid, AVar name nid)
  EVar name -> do
    nid <- lookupVar env name
    st <- get
    case IntMap.lookup (intFromNode nid) (bsNodes st) of
        -- Wrap polymorphic bindings (let-bound schemes) in a fresh expansion node
        -- to allow per-occurrence instantiation.
        Just TyForall{} -> do
            (expNode, _) <- allocExpNode nid
            pure (expNode, AVar name expNode)
        -- Monomorphic bindings (e.g. lambda parameters) do not need expansion.
        _ ->
            pure (nid, AVar name nid)
  ELit lit -> do
    nid <- allocBase (baseFor lit)
    -- Literal is a root (no binding parent)
    pure (nid, ALit lit nid)
  -- See Note [Lambda Translation]
  ELam param body -> do
    -- Allocate children first, then create the arrow
    argNode <- allocVar
    let env' = Map.insert param (Binding argNode) env
    (bodyNode, bodyAnn) <- buildExpr env' scopeRoot body
    -- allocArrow sets binding parents for dom/cod automatically
    arrowNode <- allocArrow argNode bodyNode
    pure (arrowNode, ALam param argNode scopeRoot bodyAnn arrowNode)

  -- ELamAnn is desugared to ELam + let-bound κσ coercion (see `MLF.Frontend.Desugar`).
  ELamAnn{} ->
    buildExpr env scopeRoot (desugarCoercions expr)

  -- See Note [Application and Instantiation Edges]
  EApp fun arg -> do
    (funNode, funAnn) <- buildExpr env scopeRoot fun
    (argNode, argAnn) <- buildExpr env scopeRoot arg
    domNode <- allocVar
    resultNode <- allocVar
    -- allocArrow sets binding parents for dom/cod automatically
    arrowNode <- allocArrow domNode resultNode
    funEid <- addInstEdge funNode arrowNode
    argEid <- addInstEdge argNode domNode
    -- The result node is what we return, but the arrow is the structural root
    pure (resultNode, AApp funAnn argAnn funEid argEid resultNode)

  -- See Note [Let Bindings and Expansion Variables]
  ELet name rhs body -> do
    Scope.pushScope
    (rhsNode, rhsAnn) <- buildExpr env scopeRoot rhs
    rhsScope <- Scope.popScope

    -- Insert TyForall to mark generalization over the RHS scope
    forallNode <- allocForall rhsNode
    Scope.rebindScopeNodes forallNode rhsNode rhsScope

    -- Bind the Forall node directly (no expansion node at definition)
    -- Expansion happens at use sites via EVar
    let env' = Map.insert name (Binding forallNode) env
    (bodyNode, bodyAnn) <- buildExpr env' scopeRoot body
    -- We pass forallNode as schemeNode to ALet, and 0 as expVar (unused)
    -- This is a slight hack on ALet structure, but consistent with the graph change.
    pure (bodyNode, ALet name forallNode (ExpVarId 0) forallNode rhsAnn bodyAnn bodyNode)

  -- See Note [Annotated Let]
  ELetAnn name (SrcScheme bindings bodyType) rhs body -> do
    -- Build the explicit scheme in its own scope.
    Scope.pushScope
    (tyEnv, quantVars) <- internalizeBinders bindings
    schemeBodyNode <- internalizeSrcType tyEnv bodyType
    schemeScope <- Scope.popScope
    schemeNode <- case quantVars of
        [] -> pure schemeBodyNode
        _ -> do
            node <- allocForall schemeBodyNode
            mapM_ (\varNode -> setBindParentIfMissing varNode node BindFlex) quantVars
            pure node
    Scope.rebindScopeNodes schemeNode schemeBodyNode schemeScope

    -- Build the RHS in a fresh scope and bind it under a let-introduced forall.
    Scope.pushScope
    (rhsNode, rhsAnn) <- buildExpr env scopeRoot rhs
    rhsScope <- Scope.popScope

    rhsScopeNode <- allocForall rhsNode
    Scope.rebindScopeNodes rhsScopeNode rhsNode rhsScope

    -- Emit instantiation: inferred RHS scheme must be an instance of the annotated scheme.
    rhsEdge <- addInstEdge rhsScopeNode schemeNode
    let rhsAnn' = AAnn rhsAnn schemeNode rhsEdge

    -- Bind the scheme node directly
    let env' = Map.insert name (Binding schemeNode) env
    (bodyNode, bodyAnn) <- buildExpr env' scopeRoot body
    -- Pass schemeNode as scheme, 0 as dummy expVar
    pure (bodyNode, ALet name schemeNode (ExpVarId 0) rhsScopeNode rhsAnn' bodyAnn bodyNode)

  -- Term Annotation
  EAnn annotatedExpr srcType -> do
    (exprNode, exprAnn) <- buildExpr env scopeRoot annotatedExpr
    annNode <- internalizeSrcType Map.empty srcType
    eid <- addInstEdge exprNode annNode
    pure (annNode, AAnn exprAnn annNode eid)

{- Note [Annotated Lambda parameters via κσ]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`papers/xmlf.txt` (§3.1) presents annotated lambda parameters as syntactic sugar:

  λ(x : σ) b  ≜  λ(x) let x = κσ x in b

We follow that structure by desugaring `ELamAnn` before Phase 1 (see
`MLF.Frontend.Desugar.desugarCoercions`). Constraint generation therefore only needs the
core lambda translation (allocate a fresh parameter type variable), while the
inserted `let` + `EAnn` nodes enforce the annotation.
-}

{- Note [Annotated Let]
~~~~~~~~~~~~~~~~~~~~~~~
An annotated let `let x : σ = e₁ in e₂` allows the user to specify the type
scheme for a let-bound variable. We:

  1. Internalize the annotation scheme σ as a graph node (often a `TyForall`).
  2. Translate the RHS e₁ in a fresh scope and bind it under a fresh `TyForall`
     anchor gᵣₕₛ.
  3. Emit an instantiation edge gᵣₕₛ ≤ σ to enforce the annotation.
  4. Bind x to σ in the environment and translate the body e₂.

As with unannotated lets, use sites wrap σ in `TyExp` so presolution can record
per-occurrence witnesses Φ(e).

This allows explicit type annotations while maintaining the MLF constraint
solving approach.
-}

{- Note [Lambda Translation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Lambda abstraction `λx. e` is translated as follows:

  1. Allocate a fresh type variable α for the parameter at the CURRENT binding node
  2. Extend the environment to bind x to α
  3. Recursively translate the body e to get type τ
  4. Return a fresh arrow node (α → τ)

The parameter is bound at the current binding node (the surrounding
generalization site), NOT under a fresh child binder. This means lambda
parameters are monomorphic — they cannot be generalized. This is the key
difference from let-bindings.

From the paper's pseudocode (§1):
  "lambda λx.e: create nodes for argument and body; tie argument var node
   to scope, produce arrow node with succ = [argnode, bodyNode]"

Example:
  λx. λy. x y

  Generates (under binding node g₀):
    - α : TyVar bound under g₀ (for x)
    - β : TyVar bound under g₀ (for y)
    - γ : TyVar bound under g₀ (result of application)
    - (β → γ) : TyArrow
    - InstEdge: α ≤ (β → γ)
    - (α → (β → γ)) : TyArrow (final type)

Note that all variables are under the same binding node — no generalization happens
inside a lambda body unless there's a nested let.
-}

{- Note [Application and Instantiation Edges]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Function application `e₁ e₂` is the source of instantiation edges (≤).
This is where MLF's polymorphism machinery connects to the constraint graph.

Translation:
  1. Recursively translate e₁ to get node n₁ (the function)
  2. Recursively translate e₂ to get node n₂ (the argument)
  3. Allocate a fresh domain variable d and result variable r
  4. Create an arrow node (d → r)
  5. Emit instantiation edge: n₁ ≤ (d → r) (instantiate the function)
  6. Emit instantiation edge: n₂ ≤ d       (instantiate the argument)
  7. Return r as the application's type

Paper reference: `papers/xmlf.txt` Figure 7: both the function and the argument
subexpression have their own instantiation witness (Φ(e₁), Φ(e₂)).

Why an instantiation edge, not unification?
  If e₁ has a polymorphic type (wrapped in TyExp from a let-binding), we
  don't want to immediately unify it with (n₂ → r). The instantiation edge
  says "n₁ must be AT LEAST as polymorphic as (n₂ → r)" — the presolution
  phase will decide HOW to instantiate the polymorphism.

Example: `let id = λx.x in id 42`
  - id has type: s · (α → α)  where s is an expansion variable
  - The application emits:
      s · (α → α) ≤ (d → β)
      Int         ≤ d
  - Phase 4 will decide s := inst, grafting Int onto α
  - This generates unification: α = Int, β = Int

The instantiation edge is the key mechanism that delays the instantiation
decision until we have enough information (from all use sites) to choose
the minimal expansion.

Paper reference: ICFP 2008, §1 (constraint generation), §5 (presolution)
-}

{- Note [Let Bindings and Expansion Variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Let-bindings `let x = e₁ in e₂` introduce generalization and are the source
of polymorphism in MLF. The translation differs from lambda in crucial ways.

In this repo, the paper’s binding nodes `g` are represented explicitly using a
`TyForall` anchor plus binding edges (`Constraint.cBindParents`).

Repo translation:
  1. Translate e₁ while recording the nodes allocated for it (a fresh scope frame)
  2. Insert a `TyForall` anchor g whose body is the RHS type node τ
  3. Rebind all RHS-scope nodes under g via `Constraint.cBindParents`
  4. Bind x to g in the environment (no `TyExp` at the definition site)
  5. At each *use site* of x, wrap g in a fresh expansion node `s · g` (`TyExp`)

From the paper's pseudocode (§1):
  "let x = e1 in e2: create new binding node g (child of current g);
   n1 = gen(e1) in environment bound to g;
   create ExpVar s for this let binding and represent scheme as s n1;
   bind occurrences of x in e2 to the s n1 scheme;
   n2 = gen(e2) in environment extended with that binding;
   return n2"

Why a child binder?
  Variables created while translating e₁ are rebound under the let-introduced
  `TyForall` anchor g. This marks them as “inside” the let-RHS scope in the
  binding tree and therefore candidates for generalization/elaboration.

Why an expansion node at each use site?
  Each occurrence gets its own expansion variable s and therefore its own
  instantiation edge `s · g ≤ …`. Phase 4 computes a minimal expansion recipe
  for that edge (Identity / Instantiate / Forall-intro / composition) and
  records a per-edge witness Φ(e) for elaboration.

Example: `let f = λx.x in (f 1, f True)`
  - let introduces a shared `TyForall` anchor g for the RHS
  - each use site wraps it in a fresh `TyExp`:
      s₁ · g ≤ (Int → β₁)
      s₂ · g ≤ (Bool → β₂)

Paper references:
  - ICFP 2008, §1 for the translation
  - ICFP 2008, §3 for expansion variables and solved forms
  - ICFP 2008, §5 for computing minimal expansions
-}

lookupVar :: Env -> VarName -> ConstraintM NodeId
lookupVar env name = case Map.lookup name env of
    Just binding -> pure (bindingNode binding)
    Nothing -> throwError (UnknownVariable name)

-- | Type variable environment for internalizing source types.
type TyEnv = Map VarName NodeId

-- | Internalize a source type annotation into a constraint graph.
-- This creates nodes for the type structure and connects them appropriately.
--
-- The tyEnv maps type variable names to their allocated NodeIds (for quantified vars).
internalizeSrcType :: TyEnv -> SrcType -> ConstraintM NodeId
internalizeSrcType tyEnv srcType = case srcType of
    STVar name -> case Map.lookup name tyEnv of
        Just nid -> pure nid
        Nothing -> do
            -- Free type variable: allocate a fresh variable
            nid <- allocVar
            pure nid

    STArrow dom cod -> do
        domNode <- internalizeSrcType tyEnv dom
        codNode <- internalizeSrcType tyEnv cod
        allocArrow domNode codNode

    STBase name -> allocBase (BaseTy name)

    STForall var mBound body -> do
        Scope.pushScope
        -- Allocate a type variable for the bound variable
        varNode <- allocVar
        -- Extend the environment with this binding
        let tyEnv' = Map.insert var varNode tyEnv
        -- Process the bound if present (for instance bounds)
        mbBoundNode <- case mBound of
            Nothing -> pure Nothing
            Just bound -> do
                boundNode <- internalizeSrcType tyEnv' bound
                pure (Just boundNode)

        -- Record the bound on the variable
        setVarBound varNode mbBoundNode

        -- Internalize the body with the extended environment
        bodyNode <- internalizeSrcType tyEnv' body
        scopeFrame <- Scope.popScope
        -- Create the forall node
        forallNode <- allocForall bodyNode
        setBindParentIfMissing varNode forallNode BindFlex
        Scope.rebindScopeNodes forallNode bodyNode scopeFrame
        pure forallNode

    STBottom -> do
        -- Bottom is the minimal type, represented as a fresh variable
        -- that can be instantiated to anything
        allocVar

-- | Internalize a list of binders from a source scheme.
-- Returns an environment mapping variable names to their nodes,
-- and the list of allocated nodes.
internalizeBinders :: [(String, Maybe SrcType)] -> ConstraintM (TyEnv, [NodeId])
internalizeBinders bindings = go Map.empty [] bindings
  where
    go tyEnv acc [] = pure (tyEnv, reverse acc)
    go tyEnv acc ((name, mBound):rest) = do
        -- Allocate a type variable for this binding
        varNode <- allocVar
        let tyEnv' = Map.insert name varNode tyEnv
        -- Process the bound if present
        mbBoundNode <- case mBound of
            Nothing -> pure Nothing
            Just bound -> do
                boundNode <- internalizeSrcType tyEnv' bound
                pure (Just boundNode)

        -- Set the bound
        setVarBound varNode mbBoundNode

        go tyEnv' (varNode:acc) rest

baseFor :: Lit -> BaseTy
baseFor lit = BaseTy $ case lit of
    LInt _ -> "Int"
    LBool _ -> "Bool"
    LString _ -> "String"
