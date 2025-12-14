module MLF.ConstraintGen (
    ConstraintError (..),
    ConstraintResult (..),
  AnnExpr (..),
    generateConstraints
) where

import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.State.Strict (StateT, gets, modify', runStateT)
import Control.Monad (foldM)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import MLF.Syntax
import MLF.Types

{- Note [Phase 1: Constraint Generation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module implements Phase 1 of the MLF type inference algorithm: translating
a source expression into a graphic constraint. This is the "compositional
translation" described in Rémy & Yakobowski (ICFP 2008) §1.

The translation is syntax-directed and produces:
  1. A DAG of type nodes (TyVar, TyArrow, TyBase, TyExp)
  2. A forest of G-nodes representing generalization levels
  3. Instantiation edges (≤) at application sites
  4. The root NodeId representing the expression's type

Key invariants maintained:
  - Every TyVar records its binding level via tnVarLevel :: GNodeId
  - G-nodes form a proper tree (child levels point to parent)
  - Lambda parameters are bound at the CURRENT level (monomorphic)
  - Let bindings create a CHILD level and wrap RHS in TyExp

The constraint graph is the input to subsequent phases:
  - Phase 2 normalizes via grafting/merging
  - Phase 3 checks acyclicity of instantiation dependencies
  - Phase 4 computes the principal presolution
  - Phase 5 solves remaining unification
  - Phase 6 elaborates to xMLF

Paper reference: ICFP 2008, §1 "From ML to constraints"
-}

{- Note [Lambda vs Let Polymorphism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MLF distinguishes between lambda-bound and let-bound variables in how they
handle polymorphism. This follows standard ML-style let-polymorphism.

Lambda-bound variables (monomorphic by default):
  In `λf. (f 1, f True)`, the parameter `f` gets a plain type variable `α`.
  Each use of `f` must have the SAME type, so this fails: we can't unify
  `α → Int` with `α → Bool`.

Let-bound variables (polymorphic via expansion nodes):
  In `let f = λx. x in (f 1, f True)`, the binding `f` is wrapped in an
  expansion node `s · (α → α)`. Each USE of `f` can instantiate differently,
  so `f 1 : Int` and `f True : Bool` both work.

The classic example that illustrates this difference:

    (λf. (f 1, f True)) (λx. x)     -- FAILS in ML and MLF (without annotation)
    let f = λx. x in (f 1, f True)  -- WORKS in ML and MLF

Why can't MLF infer polymorphism for lambda parameters?

To type the lambda version, you need HIGHER-RANK polymorphism:
  (λf. ...) : (∀α. α → α) → (Int, Bool)

MLF CAN express this type, but cannot INFER it without help because:
  1. The argument type (∀α. α → α) is not determined by the lambda body alone
  2. Multiple valid types exist (the type is not principal without annotation)
  3. Inference would require "guessing" where to place ∀ quantifiers

This is why our implementation:
  - ELam: allocates a plain TyVar for the parameter (monomorphic)
  - ELet: wraps the RHS in a TyExp expansion node (polymorphic)

Future extensions could support explicit type annotations like:
  ELamAnnot :: VarName -> Type -> Expr -> Expr
  -- λ(f : ∀α. α → α). (f 1, f True)

This would allow the user to request higher-rank types where needed.

Paper references:
  - ICFP 2008, §1 describes the constraint language and type syntax
  - ICFP 2008, §3 defines solved forms and expansion variables (s · τ)
  - Le Botlan & Rémy (2003) "MLF: Raising ML to the Power of System F"
    discusses the design choice of annotation-free let-polymorphism
-}

-- | Errors that can surface during constraint generation.
data ConstraintError
    = UnknownVariable VarName
    deriving (Eq, Show)

-- | Successful constraint generation returns the full constraint graph and the
-- root 'NodeId' that represents the program's type.
data ConstraintResult = ConstraintResult
  { crConstraint :: Constraint
  , crRoot :: NodeId
  , crAnnotated :: AnnExpr
  }
    deriving (Eq, Show)

-- | Expression annotated with the NodeIds allocated during constraint generation.
-- The NodeIds are stable and match the ones found in the constraint graph, so
-- later phases (e.g., elaboration) can recover binder types.
data AnnExpr
    = AVar VarName NodeId
    | ALit Lit NodeId
    | ALam VarName NodeId GNodeId AnnExpr NodeId
      -- ^ param name, param node, param level, body, result node
    | AApp AnnExpr AnnExpr EdgeId NodeId
      -- ^ fun, arg, inst edge id, result node
    | ALet VarName NodeId ExpVarId GNodeId AnnExpr AnnExpr NodeId
      -- ^ binder name, scheme node (TyExp), expansion var, child level of RHS, rhs, body, result node
    | AAnn AnnExpr NodeId EdgeId
      -- ^ expression, annotation node
    deriving (Eq, Show)

data Binding = Binding
  { bindingNode :: NodeId
  }

type Env = Map VarName Binding

data BuildState = BuildState
    { bsNextNode :: !Int          -- ^ Next available NodeId
    , bsNextGNode :: !Int         -- ^ Next available GNodeId
    , bsNextExpVar :: !Int        -- ^ Next available ExpVarId
    , bsNextEdge :: !Int          -- ^ Next available EdgeId
    , bsNodes :: !(IntMap TyNode) -- ^ Map of all allocated type nodes
    , bsGNodes :: !(IntMap GNode) -- ^ Map of all generalization nodes
    , bsForest :: ![GNodeId]      -- ^ Roots of the G-node forest
    , bsInstEdges :: ![InstEdge]  -- ^ Instantiation edges (accumulated in reverse)
    , bsUnifyEdges :: ![UnifyEdge] -- ^ Unification edges (accumulated in reverse)
    , bsRootLevel :: !GNodeId     -- ^ The root generalization level
    }

type ConstraintM = StateT BuildState (Except ConstraintError)

runConstraintM :: ConstraintM a -> BuildState -> Either ConstraintError (a, BuildState)
runConstraintM action st = runExcept (runStateT action st)

generateConstraints :: Expr -> Either ConstraintError ConstraintResult
generateConstraints expr = do
    let initialState = mkInitialState
        rootLevel = bsRootLevel initialState
    ((rootNode, annRoot), finalState) <-
        runConstraintM (buildExpr Map.empty rootLevel expr) initialState
    let constraint = buildConstraint finalState
    pure ConstraintResult
        { crConstraint = constraint
        , crRoot = rootNode
        , crAnnotated = annRoot
        }

mkInitialState :: BuildState
mkInitialState = BuildState
    { bsNextNode = 0
    , bsNextGNode = 1
    , bsNextExpVar = 0
    , bsNextEdge = 0
    , bsNodes = IntMap.empty
    , bsGNodes = IntMap.singleton (intFromG rootLevel) rootNode
    , bsForest = [rootLevel]
    , bsInstEdges = []
    , bsUnifyEdges = []
    , bsRootLevel = rootLevel
    }
  where
    rootLevel = GNodeId 0
    rootNode = GNode
        { gnodeId = rootLevel
        , gParent = Nothing
        , gBinds = []
        , gChildren = []
        }

buildConstraint :: BuildState -> Constraint
buildConstraint st = Constraint
    { cGForest = bsForest st
    , cGNodes = bsGNodes st
    , cNodes = bsNodes st
    , cInstEdges = reverse (bsInstEdges st)
    , cUnifyEdges = reverse (bsUnifyEdges st)
    }

buildExpr :: Env -> GNodeId -> Expr -> ConstraintM (NodeId, AnnExpr)
buildExpr env level expr = case expr of
  EVar name -> do
    nid <- lookupVar env name
    -- Wrap in fresh expansion node to allow instantiation at this specific use site
    (expNode, _) <- allocExpNode nid
    pure (expNode, AVar name expNode)
  ELit lit -> do
    nid <- allocBase (baseFor lit)
    pure (nid, ALit lit nid)
  -- See Note [Lambda Translation]
  ELam param body -> do
    argNode <- allocVar level
    let env' = Map.insert param (Binding argNode) env
    (bodyNode, bodyAnn) <- buildExpr env' level body
    arrowNode <- allocArrow argNode bodyNode
    pure (arrowNode, ALam param argNode level bodyAnn arrowNode)

  -- See Note [Annotated Lambda]
  ELamAnn param srcType body -> do
    -- Translate the annotation to a constraint graph node
    argNode <- internalizeSrcType Map.empty level srcType
    let env' = Map.insert param (Binding argNode) env
    (bodyNode, bodyAnn) <- buildExpr env' level body
    arrowNode <- allocArrow argNode bodyNode
    pure (arrowNode, ALam param argNode level bodyAnn arrowNode)

  -- See Note [Application and Instantiation Edges]
  EApp fun arg -> do
    (funNode, funAnn) <- buildExpr env level fun
    (argNode, argAnn) <- buildExpr env level arg
    resultNode <- allocVar level
    arrowNode <- allocArrow argNode resultNode
    eid <- addInstEdge funNode arrowNode
    pure (resultNode, AApp funAnn argAnn eid resultNode)

  -- See Note [Let Bindings and Expansion Variables]
  ELet name rhs body -> do
    childLevel <- newChildLevel level
    (rhsNode, rhsAnn) <- buildExpr env childLevel rhs

    -- Insert TyForall to mark generalization at childLevel
    forallNode <- allocForall level childLevel rhsNode

    -- Bind the Forall node directly (no expansion node at definition)
    -- Expansion happens at use sites via EVar
    let env' = Map.insert name (Binding forallNode) env
    (bodyNode, bodyAnn) <- buildExpr env' level body
    -- We pass forallNode as schemeNode to ALet, and 0 as expVar (unused)
    -- This is a slight hack on ALet structure, but consistent with the graph change.
    pure (bodyNode, ALet name forallNode (ExpVarId 0) childLevel rhsAnn bodyAnn bodyNode)

  -- See Note [Annotated Let]
  ELetAnn name (SrcScheme bindings bodyType) rhs body -> do
    childLevel <- newChildLevel level
    -- Create a type variable environment for the quantified variables
    (tyEnv, quantVars) <- internalizeBinders childLevel bindings
    -- Internalize the scheme body type
    schemeBodyNode <- internalizeSrcType tyEnv childLevel bodyType
    -- Translate the RHS and unify with the annotated type
    (rhsNode, rhsAnn) <- buildExpr env childLevel rhs
    -- Emit instantiation: inferred RHS type must be capable of generating the annotated type
    -- (rhsNode <= schemeBodyNode)
    rhsEdge <- addInstEdge rhsNode schemeBodyNode
    let rhsAnn' = AAnn rhsAnn schemeBodyNode rhsEdge

    -- Insert TyForall to mark generalization at childLevel
    -- For ELetAnn, we first wrap the scheme binders (explicit polymorphism).
    schemeNode <- foldM (\body _var -> allocForall level childLevel body) schemeBodyNode (reverse quantVars)

    -- We do NOT wrap in an extra let-generalization TyForall if the scheme is already explicit.
    -- This avoids "double quantification" (forall a. forall t. T).
    -- If quantVars was empty, schemeNode is just the body.
    -- If we want to support implicit generalization ON TOP of explicit schemes, we would wrap.
    -- But usually annotations are exhaustive.
    -- However, standard ELet *always* has a TyForall anchor.
    -- To satisfy tests that expect a single Forall for "let id : forall a...", we rely on schemeNode.
    -- If schemeNode is NOT a Forall (monomorphic annotation), we use it as is.
    -- Presolution handles Monomorphic <= Type by Identity expansion, which is correct.

    -- Bind the scheme node directly
    let env' = Map.insert name (Binding schemeNode) env
    (bodyNode, bodyAnn) <- buildExpr env' level body
    -- Pass schemeNode as scheme, 0 as dummy expVar
    pure (bodyNode, ALet name schemeNode (ExpVarId 0) childLevel rhsAnn' bodyAnn bodyNode)

  -- Term Annotation
  EAnn expr srcType -> do
    (exprNode, exprAnn) <- buildExpr env level expr
    annNode <- internalizeSrcType Map.empty level srcType
    eid <- addInstEdge exprNode annNode
    pure (annNode, AAnn exprAnn annNode eid)

{- Note [Annotated Lambda]
~~~~~~~~~~~~~~~~~~~~~~~~~~
An annotated lambda `λ(x : τ). e` is similar to a regular lambda, but instead
of allocating a fresh type variable for the parameter, we translate the type
annotation `τ` into a constraint graph node using `internalizeSrcType`.

This allows the user to request higher-rank polymorphism. For example:
  λ(f : ∀α. α → α). (f 1, f True)

The annotation creates the polymorphic type in the constraint, and the body
can then use `f` at multiple instantiations.

Paper reference: eMLF annotations in Rémy & Yakobowski's work.
-}

{- Note [Annotated Let]
~~~~~~~~~~~~~~~~~~~~~~~
An annotated let `let x : σ = e₁ in e₂` allows the user to specify the type
scheme for a let-bound variable. We:
  1. Create a child level for the RHS (as with regular let)
  2. Internalize the binders from the scheme (creating type variables)
  3. Internalize the body type of the scheme
  4. Translate the RHS and emit a unification constraint
  5. Wrap in an expansion node as usual

This allows explicit type annotations while maintaining the MLF constraint
solving approach.
-}

{- Note [Lambda Translation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Lambda abstraction `λx. e` is translated as follows:

  1. Allocate a fresh type variable α for the parameter at the CURRENT level
  2. Extend the environment to bind x to α
  3. Recursively translate the body e to get type τ
  4. Return a fresh arrow node (α → τ)

The parameter is bound at the current G-node level, NOT a child level. This
means lambda parameters are monomorphic — they cannot be generalized. This
is the key difference from let-bindings.

From the paper's pseudocode (§1):
  "lambda λx.e: create nodes for argument and body; tie argument var node
   to scope, produce arrow node with succ = [argnode, bodyNode]"

Example:
  λx. λy. x y

  Generates (at level g₀):
    - α : TyVar at g₀ (for x)
    - β : TyVar at g₀ (for y)
    - γ : TyVar at g₀ (result of application)
    - (β → γ) : TyArrow
    - InstEdge: α ≤ (β → γ)
    - (α → (β → γ)) : TyArrow (final type)

Note that all variables are at the same level — no generalization happens
inside a lambda body unless there's a nested let.
-}

{- Note [Application and Instantiation Edges]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Function application `e₁ e₂` is the source of instantiation edges (≤).
This is where MLF's polymorphism machinery connects to the constraint graph.

Translation:
  1. Recursively translate e₁ to get node n₁ (the function)
  2. Recursively translate e₂ to get node n₂ (the argument)
  3. Allocate a fresh result variable r at the current level
  4. Create an arrow node (n₂ → r)
  5. Emit instantiation edge: n₁ ≤ (n₂ → r)
  6. Return r as the application's type

From the paper's pseudocode (§1):
  "application e1 e2: n1 = gen(e1); n2 = gen(e2); res = fresh variable node r;
   add inst-edge: (n1, Arrow(n2, r)); return r"

Why an instantiation edge, not unification?
  If e₁ has a polymorphic type (wrapped in TyExp from a let-binding), we
  don't want to immediately unify it with (n₂ → r). The instantiation edge
  says "n₁ must be AT LEAST as polymorphic as (n₂ → r)" — the presolution
  phase will decide HOW to instantiate the polymorphism.

Example: `let id = λx.x in id 42`
  - id has type: s · (α → α)  where s is an expansion variable
  - The application emits: s · (α → α) ≤ (Int → β)
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
of polymorphism in MLF. The translation differs from lambda in crucial ways:

  1. Create a CHILD G-node level for the RHS
  2. Translate e₁ at the child level → get node τ
  3. Wrap τ in an expansion node: s · τ (where s is fresh)
  4. Bind x to the expansion node (s · τ) in the environment
  5. Translate e₂ at the ORIGINAL level (not the child)

From the paper's pseudocode (§1):
  "let x = e1 in e2: create new G-node g (child of current G);
   n1 = gen(e1) in environment bound to g;
   create ExpVar s for this let binding and represent scheme as s n1;
   bind occurrences of x in e2 to the s n1 scheme;
   n2 = gen(e2) in environment extended with that binding;
   return n2"

Why a child G-node?
  Variables created while translating e₁ are bound at the child level. This
  marks them as candidates for generalization. When Phase 4 processes the
  instantiation edges, it can see which variables are "inside" the let-RHS
  scope and choose to quantify them.

Why an expansion node?
  The expansion variable s is a placeholder for the "expansion recipe" that
  Phase 4 will compute. Options include:
    - s := Identity (no instantiation needed)
    - s := Inst (instantiate all quantifiers)
    - s := ∀(levels) (add explicit quantification)

  Different use sites of x may need different instantiations. Each use
  references the same s · τ node, but contributes its own instantiation
  edge that Phase 4 uses to compute the minimal expansion.

Example: `let f = λx.x in (f 1, f True)`
  - Child G-node g₁ created
  - λx.x translated at g₁: α → α (where α is at g₁)
  - Expansion node: s · (α → α)
  - Use sites emit:
      s · (α → α) ≤ (Int → β₁)
      s · (α → α) ≤ (Bool → β₂)
  - Phase 4 processes these and can instantiate α differently each time

This is how let-polymorphism works: the expansion variable s allows the
solver to defer the generalization decision until all constraints are known.

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
internalizeSrcType :: TyEnv -> GNodeId -> SrcType -> ConstraintM NodeId
internalizeSrcType tyEnv level srcType = case srcType of
    STVar name -> case Map.lookup name tyEnv of
        Just nid -> pure nid
        Nothing -> do
            -- Free type variable: allocate a fresh variable
            nid <- allocVar level
            pure nid

    STArrow dom cod -> do
        domNode <- internalizeSrcType tyEnv level dom
        codNode <- internalizeSrcType tyEnv level cod
        allocArrow domNode codNode

    STBase name -> allocBase (BaseTy name)

    STForall var mBound body -> do
        -- Create a child level for the quantifier
        childLevel <- newChildLevel level
        -- Allocate a type variable for the bound variable
        varNode <- allocVar childLevel
        -- Extend the environment with this binding
        let tyEnv' = Map.insert var varNode tyEnv
        -- Process the bound if present (for instance bounds)
        mbBoundNode <- case mBound of
            Nothing -> pure Nothing
            Just bound -> do
                -- The bound type at the same level as the quantifier body
                boundNode <- internalizeSrcType tyEnv' childLevel bound
                pure (Just boundNode)

        -- Record the bound on the variable
        setVarBound childLevel varNode mbBoundNode

        -- Internalize the body with the extended environment
        bodyNode <- internalizeSrcType tyEnv' childLevel body
        -- Create the forall node
        allocForall level childLevel bodyNode

    STBottom -> do
        -- Bottom is the minimal type, represented as a fresh variable
        -- that can be instantiated to anything
        allocVar level

-- | Allocate a TyForall node.
allocForall :: GNodeId -> GNodeId -> NodeId -> ConstraintM NodeId
allocForall ownerLevel quantLevel bodyNode = do
    nid <- freshNodeId
    insertNode TyForall
        { tnId = nid
        , tnOwnerLevel = ownerLevel
        , tnQuantLevel = quantLevel
        , tnBody = bodyNode
        }
    pure nid

-- | Internalize a list of binders from a source scheme.
-- Returns an environment mapping variable names to their nodes,
-- and the list of allocated nodes.
internalizeBinders :: GNodeId -> [(String, Maybe SrcType)] -> ConstraintM (TyEnv, [NodeId])
internalizeBinders level bindings = go Map.empty [] bindings
  where
    go tyEnv acc [] = pure (tyEnv, reverse acc)
    go tyEnv acc ((name, mBound):rest) = do
        -- Allocate a type variable for this binding
        varNode <- allocVar level
        let tyEnv' = Map.insert name varNode tyEnv
        -- Process the bound if present
        mbBoundNode <- case mBound of
            Nothing -> pure Nothing
            Just bound -> do
                boundNode <- internalizeSrcType tyEnv' level bound
                pure (Just boundNode)

        -- Set the bound
        setVarBound level varNode mbBoundNode

        go tyEnv' (varNode:acc) rest

-- | Add a unification edge (T₁ = T₂).
addUnifyEdge :: NodeId -> NodeId -> ConstraintM ()
addUnifyEdge left right = do
    let edge = UnifyEdge left right
    modify' $ \st -> st { bsUnifyEdges = edge : bsUnifyEdges st }

baseFor :: Lit -> BaseTy
baseFor lit = BaseTy $ case lit of
    LInt _ -> "Int"
    LBool _ -> "Bool"
    LString _ -> "String"

allocVar :: GNodeId -> ConstraintM NodeId
allocVar level = do
    nid <- freshNodeId
    let node = TyVar
            { tnId = nid
            , tnVarLevel = level
            }
    insertNode node
    attachVar level nid
    pure nid

allocBase :: BaseTy -> ConstraintM NodeId
allocBase base = do
    nid <- freshNodeId
    insertNode TyBase
        { tnId = nid
        , tnBase = base
        }
    pure nid

allocArrow :: NodeId -> NodeId -> ConstraintM NodeId
allocArrow domNode codNode = do
    nid <- freshNodeId
    insertNode TyArrow
        { tnId = nid
        , tnDom = domNode
        , tnCod = codNode
        }
    pure nid

allocExpNode :: NodeId -> ConstraintM (NodeId, ExpVarId)
allocExpNode bodyNode = do
  expVar <- freshExpVarId
  nid <- freshNodeId
  insertNode TyExp
    { tnId = nid
    , tnExpVar = expVar
    , tnBody = bodyNode
    }
  pure (nid, expVar)

newChildLevel :: GNodeId -> ConstraintM GNodeId
newChildLevel parent = do
    gid <- freshGNodeId
    let node = GNode
            { gnodeId = gid
            , gParent = Just parent
            , gBinds = []
            , gChildren = []
            }
    modify' $ \st ->
        st { bsGNodes = IntMap.insert (intFromG gid) node (bsGNodes st)
           , bsForest = bsForest st
           }
    modify' $ \st ->
        st { bsGNodes = IntMap.adjust (\gn -> gn { gChildren = gid : gChildren gn }) (intFromG parent) (bsGNodes st) }
    pure gid

attachVar :: GNodeId -> NodeId -> ConstraintM ()
attachVar level nodeId = modify' $ \st ->
    st { bsGNodes = IntMap.adjust (\gn -> gn { gBinds = (nodeId, Nothing) : gBinds gn }) (intFromG level) (bsGNodes st) }

setVarBound :: GNodeId -> NodeId -> Maybe NodeId -> ConstraintM ()
setVarBound level varNode bound = modify' $ \st ->
    st { bsGNodes = IntMap.adjust (\gn -> gn { gBinds = updateBinds (gBinds gn) }) (intFromG level) (bsGNodes st) }
  where
    updateBinds [] = []
    updateBinds ((v, _) : xs) | v == varNode = (v, bound) : xs
    updateBinds (x : xs) = x : updateBinds xs

addInstEdge :: NodeId -> NodeId -> ConstraintM EdgeId
addInstEdge left right = do
  eid <- freshEdgeId
  let edgeId = EdgeId eid
      edge = InstEdge edgeId left right
  modify' $ \st -> st { bsInstEdges = edge : bsInstEdges st }
  pure edgeId

freshNodeId :: ConstraintM NodeId
freshNodeId = do
    next <- gets bsNextNode
    modify' $ \st -> st { bsNextNode = next + 1 }
    pure (NodeId next)

freshGNodeId :: ConstraintM GNodeId
freshGNodeId = do
    next <- gets bsNextGNode
    modify' $ \st -> st { bsNextGNode = next + 1 }
    pure (GNodeId next)

freshExpVarId :: ConstraintM ExpVarId
freshExpVarId = do
    next <- gets bsNextExpVar
    modify' $ \st -> st { bsNextExpVar = next + 1 }
    pure (ExpVarId next)

freshEdgeId :: ConstraintM Int
freshEdgeId = do
    next <- gets bsNextEdge
    modify' $ \st -> st { bsNextEdge = next + 1 }
    pure next

insertNode :: TyNode -> ConstraintM ()
insertNode node = modify' $ \st ->
    st { bsNodes = IntMap.insert (intFromNode (tnId node)) node (bsNodes st) }

intFromNode :: NodeId -> Int
intFromNode (NodeId x) = x

intFromG :: GNodeId -> Int
intFromG (GNodeId x) = x
