module MLF.Frontend.ConstraintGen.Translate (
    buildRootExpr
) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (gets)
import Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)

import MLF.Constraint.Types
import MLF.Frontend.Syntax
import MLF.Frontend.ConstraintGen.Emit
import qualified MLF.Frontend.ConstraintGen.Scope as Scope
import MLF.Frontend.ConstraintGen.State (BuildState(..), ConstraintM)
import MLF.Frontend.ConstraintGen.Types

buildRootExpr :: Expr -> ConstraintM (GenNodeId, NodeId, AnnExpr)
buildRootExpr expr = do
    rootGen <- allocGenNode []
    (rootNode, annRoot) <- buildExpr Map.empty rootGen expr
    topFrame <- Scope.peekScope
    Scope.rebindScopeNodes (genRef rootGen) rootNode topFrame
    setBindParentIfMissing (typeRef rootNode) (genRef rootGen) BindFlex
    setGenNodeSchemes rootGen [rootNode]
    pure (rootGen, rootNode, annRoot)

buildExpr :: Env -> GenNodeId -> Expr -> ConstraintM (NodeId, AnnExpr)
buildExpr env scopeRoot expr = do
    (rootNode, ann) <- buildExprRaw env scopeRoot expr
    setBindParentIfMissing (typeRef rootNode) (genRef scopeRoot) BindFlex
    pure (rootNode, ann)

buildExprRaw :: Env -> GenNodeId -> Expr -> ConstraintM (NodeId, AnnExpr)
buildExprRaw env scopeRoot expr =
    case viewCoerce expr of
        Just (annTy, annotatedExpr) -> buildCoerce env scopeRoot annTy annotatedExpr
        Nothing -> case expr of
            EVar name -> do
                binding <- lookupVar env name
                let nid = bindingNode binding
                case bindingGen binding of
                    -- Polymorphic bindings (let-bound schemes) get a fresh expansion node.
                    Just _ -> do
                        (expNode, _) <- allocExpNode nid
                        pure (expNode, AVar name expNode)
                    -- Monomorphic bindings (e.g. lambda parameters) do not need expansion.
                    Nothing ->
                        pure (nid, AVar name nid)
            ELit lit -> do
                baseNode <- allocBase (baseFor lit)
                varNode <- allocVar
                setVarBound varNode (Just baseNode)
                pure (varNode, ALit lit varNode)
            -- See Note [Lambda Translation]
            ELam param body -> do
                -- Allocate children first, then create the arrow
                argNode <- allocVar
                let env' = Map.insert param (Binding argNode Nothing) env
                (bodyNode, bodyAnn) <- buildExpr env' scopeRoot body
                -- allocArrow sets binding parents for dom/cod automatically
                arrowNode <- allocArrow argNode bodyNode
                -- Lambda parameters are bound at the current binding node (not under the arrow).
                setBindParentOverride (typeRef argNode) (genRef scopeRoot) BindFlex
                rootVar <- allocVar
                setVarBound rootVar (Just arrowNode)
                pure (rootVar, ALam param argNode scopeRoot bodyAnn rootVar)

            -- Annotated lambda parameters preserve the annotation as the parameter type,
            -- enabling rank-2 argument types for explicit foralls.
            ELamAnn param annTy body -> do
                annNode <- internalizeSrcTypeBound scopeRoot Map.empty annTy
                schemeGenUsed <- lookupSchemeGenForRoot annNode
                let bindingGenUsed =
                        case (annTy, schemeGenUsed) of
                            (STForall{}, Nothing) -> Just scopeRoot
                            _ -> schemeGenUsed
                let env' = Map.insert param (Binding annNode bindingGenUsed) env
                (bodyNode, bodyAnn) <- buildExpr env' scopeRoot body
                arrowNode <- allocArrow annNode bodyNode
                -- Annotated parameters still bind at the current binding node.
                case annTy of
                    STForall{} -> pure ()
                    _ ->
                        case schemeGenUsed of
                            Nothing -> setBindParentOverride (typeRef annNode) (genRef scopeRoot) BindFlex
                            Just _ -> pure ()
                rootVar <- allocVar
                setVarBound rootVar (Just arrowNode)
                pure (rootVar, ALam param annNode scopeRoot bodyAnn rootVar)

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
                case funAnn of
                    ALam _ paramNode _ _ _ -> do
                        nodes <- gets bsNodes
                        case IntMap.lookup (getNodeId paramNode) nodes of
                            Just TyVar{ tnBound = Nothing } -> pure ()
                            _ -> setVarBound domNode (Just paramNode)
                    _ -> pure ()
                -- The result node is what we return, but the arrow is the structural root
                pure (resultNode, AApp funAnn argAnn funEid argEid resultNode)

            -- See Note [Let Bindings and Expansion Variables]
            ELet name rhs body ->
                let buildUnder gen subExpr = do
                        Scope.pushScope
                        (node, ann) <- buildExpr env gen subExpr
                        scope <- Scope.popScope
                        pure (node, ann, scope)
                    buildLet schemeGenId schemeGenUsed schemeRootNode rhsGen rhsAnn = do
                        let env' = Map.insert name (Binding schemeRootNode (Just schemeGenUsed)) env

                        -- Alternative let scoping (Fig. 15.2.6, rightmost constraint):
                        -- introduce a gen node for the let expression and a trivial scheme root.
                        letGen <- allocGenNode []
                        setBindParentIfMissing (genRef letGen) (genRef scopeRoot) BindFlex
                        setBindParentOverride (genRef schemeGenId) (genRef letGen) BindFlex
                        if rhsGen /= schemeGenId
                            then setBindParentOverride (genRef rhsGen) (genRef letGen) BindFlex
                            else pure ()
                        bodyGen <- allocGenNode []
                        setBindParentIfMissing (genRef bodyGen) (genRef letGen) BindFlex

                        trivialRoot <- allocVar
                        setBindParentIfMissing (typeRef trivialRoot) (genRef letGen) BindFlex
                        setGenNodeSchemes letGen [trivialRoot]

                        Scope.pushScope
                        (bodyNode, bodyAnn0) <- buildExpr env' bodyGen body
                        bodyScope <- Scope.popScope
                        Scope.rebindScopeNodes (genRef bodyGen) bodyNode bodyScope

                        letEdge <- addInstEdge bodyNode trivialRoot
                        recordLetEdge letEdge
                        let bodyAnn = AAnn bodyAnn0 trivialRoot letEdge

                        -- Pass schemeNode as scheme, 0 as dummy expVar
                        pure (trivialRoot, ALet name schemeGenUsed schemeRootNode (ExpVarId 0) rhsGen rhsAnn bodyAnn trivialRoot)
                    buildAnnotated annTy rhsExpr = do
                        let (bindings, bodyType) = splitForalls annTy
                        -- Build the explicit scheme in its own scope.
                        Scope.pushScope
                        schemeGenId <- allocGenNode []
                        (tyEnv, quantVars) <- internalizeBinders schemeGenId bindings
                        schemeBodyNode <- internalizeSrcTypeBound schemeGenId tyEnv bodyType
                        schemeScope <- Scope.popScope
                        let schemeRootNode = schemeBodyNode
                        schemeGenUsed <- fmap (maybe schemeGenId id) (lookupSchemeGenForRoot schemeRootNode)
                        -- schemeGenId already allocated above
                        Scope.rebindScopeNodes (typeRef schemeRootNode) schemeBodyNode schemeScope
                        setBindParentIfMissing (typeRef schemeRootNode) (genRef schemeGenUsed) BindFlex
                        -- Ensure explicit scheme binders are bound under the scheme gen node (named nodes).
                        mapM_ (\varNode -> setBindParentOverride (typeRef varNode) (genRef schemeGenUsed) BindFlex) quantVars
                        setGenNodeSchemes schemeGenUsed [schemeRootNode]

                        -- Build the RHS in a fresh scope and bind it under a let-introduced forall.
                        rhsGen <- allocGenNode []
                        (rhsNode, rhsAnn, rhsScope) <- buildUnder rhsGen rhsExpr
                        Scope.rebindScopeNodes (genRef rhsGen) rhsNode rhsScope

                        -- Emit instantiation: the annotated scheme must instantiate to the RHS.
                        (annExpNode, _) <- allocExpNode schemeRootNode
                        setBindParentIfMissing (typeRef annExpNode) (genRef schemeGenId) BindFlex
                        rhsEdge <- addInstEdge annExpNode rhsNode
                        let rhsAnn' = AAnn rhsAnn annExpNode rhsEdge

                        pure (schemeGenId, schemeGenUsed, schemeRootNode, rhsGen, rhsAnn')
                    buildInferred rhsExpr = do
                        schemeGenId <- allocGenNode []
                        (rhsNode, rhsAnn, rhsScope) <- buildUnder schemeGenId rhsExpr
                        schemeGenUsed <- fmap (maybe schemeGenId id) (lookupSchemeGenForRoot rhsNode)
                        setGenNodeSchemes schemeGenUsed [rhsNode]
                        Scope.rebindScopeNodes (genRef schemeGenUsed) rhsNode rhsScope
                        setBindParentOverride (typeRef rhsNode) (genRef schemeGenUsed) BindFlex
                        pure (schemeGenId, schemeGenUsed, rhsNode, schemeGenId, rhsAnn)
                in do
                    (schemeGenId, schemeGenUsed, schemeRootNode, rhsGen, rhsAnn) <-
                        case viewCoerce rhs of
                            -- Desugared annotated let: let x = (e : σ) in b
                            -- (note: ELetAnn was removed; we handle the annotation here)
                            Just (annTy, rhsExpr) -> buildAnnotated annTy rhsExpr
                            Nothing -> buildInferred rhs
                    buildLet schemeGenId schemeGenUsed schemeRootNode rhsGen rhsAnn

            -- Term Annotation (surface form; desugars to ECoerce)
            EAnn annotatedExpr srcType ->
                buildExprRaw env scopeRoot (mkCoerce srcType annotatedExpr)
            _ ->
                throwError (InternalConstraintError "buildExprRaw: unexpected Expr constructor")

buildCoerce :: Env -> GenNodeId -> SrcType -> Expr -> ConstraintM (NodeId, AnnExpr)
buildCoerce env scopeRoot annTy annotatedExpr = do
    (exprNode, exprAnn) <- buildExpr env scopeRoot annotatedExpr
    annGen <- allocGenNode []
    setBindParentIfMissing (genRef annGen) (genRef scopeRoot) BindFlex
    Scope.pushScope
    (domainNode, codomainNode) <- internalizeCoercionType annGen annTy
    annScope <- Scope.popScope
    Scope.rebindScopeNodes (genRef annGen) codomainNode annScope
    edgeBody <- case annotatedExpr of
        EVar{} -> do
            nodes <- gets bsNodes
            case IntMap.lookup (getNodeId exprNode) nodes of
                Just TyExp{ tnBody = body } -> pure body
                _ -> pure exprNode
        _ -> pure exprNode
    (edgeLeft, _) <- allocExpNode edgeBody
    setBindParentOverride (typeRef edgeLeft) (genRef annGen) BindFlex
    eid <- addInstEdge edgeLeft domainNode
    pure (codomainNode, AAnn exprAnn codomainNode eid)

{- Note [Coercion domain/codomain semantics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The thesis' coercion κσ builds a rigid domain and flexible codomain. We
construct two copies (with shared existentials) and return the *codomain* node
as the annotation result, matching the thesis semantics (§12.3.2.2, §15.3.8).

The instantiation edge connects the expression to the *domain* node, ensuring
the expression is constrained to match the annotation type. The codomain is
returned as the result type, allowing the annotation to be used in contexts
that expect the annotated type.

We mark the domain copy as *restricted* by binding its coercion-local nodes
with rigid edges under gen nodes (shared existentials stay flexible, and no
rigid ancestor is introduced). This pushes toward
the thesis’ “rigid domain” intent while staying presolution-safe: the nodes are
not instantiable, but they are not locked under a rigid ancestor.

Both copies remain wrapped (like `internalizeSrcType`). The separate codomain
copy is retained for future work once presolution/elaboration can consume it
directly without breaking the binding tree invariants.
-}

{- Note [Annotated Lambda parameters via κσ]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`papers/these-finale-english.txt` (see `papers/xmlf.txt` §3.1) presents annotated lambda
parameters as syntactic sugar:

  λ(x : σ) b  ≜  λ(x) let x = κσ x in b

We preserve `ELamAnn` in the surface AST and translate it directly so the
parameter type is the annotation scheme itself. This keeps the κσ intuition
while ensuring explicit-forall annotations produce rank-2 argument types:

  λ(x : ∀a. a -> a). b  :  (∀a. a -> a) -> ...

Inside the body, `x` is bound to the annotated scheme and uses the usual
expansion machinery when the scheme is polymorphic.
-}

{- Note [Annotated Let via EAnn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The surface grammar does not include a dedicated `let` annotation form.
Instead, users write:

  let x = (e : σ) in b

When `ELet` sees an RHS of the form `EAnn`, we treat the annotation as the
declared scheme for the binding (the old `ELetAnn` behavior):

  1. Split the leading `∀` binders of σ into explicit scheme binders.
  2. Internalize the scheme under a fresh gen node (named binders).
  3. Translate the RHS `e` under a fresh gen node.
  4. Emit an instantiation edge from an expansion of the scheme to the RHS.
  5. Bind `x` to the scheme node when translating the body.

This keeps the surface language thesis-faithful while preserving the
annotation semantics expected by later phases.
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
    - α : TyVar { tnId = bound, tnBound = Nothing } under g₀ (for x)
    - β : TyVar { tnId = bound, tnBound = Nothing } under g₀ (for y)
    - γ : TyVar { tnId = bound, tnBound = Nothing } under g₀ (result of application)
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

Paper reference: `papers/these-finale-english.txt` (see `papers/xmlf.txt` Figure 7):
both the function and the argument subexpression have their own instantiation
witness (Φ(e₁), Φ(e₂)).

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

lookupVar :: Env -> VarName -> ConstraintM Binding
lookupVar env name = case Map.lookup name env of
    Just binding -> pure binding
    Nothing -> throwError (UnknownVariable name)

lookupSchemeGenForRoot :: NodeId -> ConstraintM (Maybe GenNodeId)
lookupSchemeGenForRoot root = do
    owner <- lookupSchemeOwnerForRoot root
    case owner of
        Just _ -> pure owner
        Nothing -> do
            bindParents <- gets bsBindParents
            let go ref =
                    case IntMap.lookup (nodeRefKey ref) bindParents of
                        Nothing -> Nothing
                        Just (parent, _) ->
                            case parent of
                                GenRef gid -> Just gid
                                TypeRef parentN -> go (typeRef parentN)
            pure (go (typeRef root))

lookupSchemeOwnerForRoot :: NodeId -> ConstraintM (Maybe GenNodeId)
lookupSchemeOwnerForRoot root = do
    genNodes <- gets bsGenNodes
    pure $ listToMaybe
        [ gnId gen
        | gen <- IntMap.elems (getGenNodeMap genNodes)
        , root `elem` gnSchemes gen
        ]

-- | Type variable environment for internalizing source types.
type TyEnv = Map VarName NodeId
type SharedEnv = Map VarName NodeId

-- | Split a nested forall type into explicit scheme binders and a body.
-- This peels only the leading `STForall`s, leaving any inner foralls intact.
splitForalls :: SrcType -> ([(String, Maybe SrcType)], SrcType)
splitForalls = go []
  where
    go acc (STForall name mb body) = go ((name, mb):acc) body
    go acc body = (reverse acc, body)

-- | Internalize a coercion type κ as a rigid domain and flexible codomain,
-- sharing existential (free) variables across both copies.
internalizeCoercionType :: GenNodeId -> SrcType -> ConstraintM (NodeId, NodeId)
internalizeCoercionType coerceGen ty = do
    (domainNode, shared1) <-
        internalizeCoercionCopy BindRigid True coerceGen coerceGen Map.empty Map.empty ty
    (codomainNode, _shared2) <-
        internalizeCoercionCopy BindFlex True coerceGen coerceGen Map.empty shared1 ty
    pure (domainNode, codomainNode)

-- | Internalize a coercion copy with a given binding flag (rigid/flex),
-- optional wrapping, and shared existentials.
internalizeCoercionCopy
    :: BindFlag
    -> Bool
    -> GenNodeId
    -> GenNodeId
    -> TyEnv
    -> SharedEnv
    -> SrcType
    -> ConstraintM (NodeId, SharedEnv)
internalizeCoercionCopy bindFlag wrap coerceGen currentGen tyEnv shared srcType =
    case srcType of
        -- Domain copies use BindRigid to mark coercion-local nodes as restricted.
        -- To avoid locked descendants, rebind children auto-bound under structural
        -- nodes back to the current gen when rigid.
        STVar name ->
            case Map.lookup name tyEnv of
                Just nid -> pure (nid, shared)
                Nothing ->
                    case Map.lookup name shared of
                        Just nid -> pure (nid, shared)
                        Nothing -> do
                            nid <- allocVar
                            setBindParentOverride (typeRef nid) (genRef coerceGen) BindFlex
                            pure (nid, Map.insert name nid shared)
    
        STArrow dom cod -> do
            (domNode, shared1) <-
                internalizeCoercionCopy bindFlag wrap coerceGen currentGen tyEnv shared dom
            (codNode, shared2) <-
                internalizeCoercionCopy bindFlag wrap coerceGen currentGen tyEnv shared1 cod
            arrowNode <- allocArrow domNode codNode
            case bindFlag of
                BindRigid -> do
                    rebindIfParent domNode (typeRef arrowNode) (genRef currentGen) bindFlag
                    rebindIfParent codNode (typeRef arrowNode) (genRef currentGen) bindFlag
                BindFlex -> pure ()
            if wrap
                then do
                    varNode <- allocVar
                    setVarBound varNode (Just arrowNode)
                    case bindFlag of
                        BindRigid ->
                            rebindIfParent arrowNode (typeRef varNode) (genRef currentGen) bindFlag
                        BindFlex -> pure ()
                    setBindParentOverride (typeRef varNode) (genRef currentGen) bindFlag
                    pure (varNode, shared2)
                else pure (arrowNode, shared2)
    
        STBase name -> do
            baseNode <- allocBase (BaseTy name)
            if wrap
                then do
                    varNode <- allocVar
                    setVarBound varNode (Just baseNode)
                    case bindFlag of
                        BindRigid ->
                            rebindIfParent baseNode (typeRef varNode) (genRef currentGen) bindFlag
                        BindFlex -> pure ()
                    setBindParentOverride (typeRef varNode) (genRef currentGen) bindFlag
                    pure (varNode, shared)
                else pure (baseNode, shared)
    
        STForall var mBound body ->
            case mBound of
                Just (STVar alias) | alias /= var -> do
                    (aliasNode, shared1) <-
                        case Map.lookup alias tyEnv of
                            Just nid -> pure (nid, shared)
                            Nothing ->
                                case Map.lookup alias shared of
                                    Just nid -> pure (nid, shared)
                                    Nothing -> do
                                        nid <- allocVar
                                        setBindParentOverride (typeRef nid) (genRef coerceGen) BindFlex
                                        pure (nid, Map.insert alias nid shared)
                    let tyEnv' = Map.insert var aliasNode tyEnv
                    internalizeCoercionCopy bindFlag wrap coerceGen currentGen tyEnv' shared1 body
                Just (STVar _) ->
                    internalizeCoercionCopy bindFlag wrap coerceGen currentGen tyEnv shared (STForall var Nothing body)
                _ -> do
                    Scope.pushScope
                    schemeGenId <- allocGenNode []
                    setBindParentIfMissing (genRef schemeGenId) (genRef currentGen) BindFlex
                    varNode <- allocVar
                    let tyEnv' = Map.insert var varNode tyEnv
                    (mbBoundNode, shared1) <- case mBound of
                        Nothing -> pure (Nothing, shared)
                        Just bound -> do
                            Scope.pushScope
                            (boundNode, shared2) <-
                                internalizeCoercionCopy bindFlag False coerceGen schemeGenId tyEnv' shared bound
                            boundScope <- Scope.popScope
                            mbBoundOwner <- lookupSchemeOwnerForRoot boundNode
                            case mbBoundOwner of
                                Just gid -> do
                                    Scope.rebindScopeNodes (genRef gid) boundNode boundScope
                                    setGenNodeSchemes gid [boundNode]
                                Nothing -> Scope.rebindScopeNodes (typeRef varNode) boundNode boundScope
                            let sharedNodes = Map.elems shared2
                            case mbBoundOwner of
                                Nothing ->
                                    if boundNode `elem` sharedNodes
                                        then pure ()
                                        else
                                            case bindFlag of
                                                BindRigid ->
                                                    setBindParentOverride
                                                        (typeRef boundNode)
                                                        (genRef schemeGenId)
                                                        bindFlag
                                                BindFlex ->
                                                    setBindParentOverride
                                                        (typeRef boundNode)
                                                        (typeRef varNode)
                                                        bindFlag
                                Just gid ->
                                    if boundNode `elem` sharedNodes
                                        then pure ()
                                        else setBindParentOverride (typeRef boundNode) (genRef gid) bindFlag
                            pure (Just boundNode, shared2)
    
                    setVarBound varNode mbBoundNode
    
                    (bodyNode, shared2) <-
                        internalizeCoercionCopy bindFlag False coerceGen schemeGenId tyEnv' shared1 body
                    scopeFrame <- Scope.popScope
                    Scope.rebindScopeNodes (genRef schemeGenId) bodyNode scopeFrame
                    setBindParentOverride (typeRef varNode) (genRef schemeGenId) bindFlag
                    let sharedNodes = Map.elems shared2
                    if bodyNode `elem` sharedNodes
                        then pure ()
                        else setBindParentOverride (typeRef bodyNode) (genRef schemeGenId) bindFlag
                    setGenNodeSchemes schemeGenId [bodyNode]
                    pure (bodyNode, shared2)
    
        STBottom -> do
            varNode <- allocVar
            pure (varNode, shared)

rebindIfParent :: NodeId -> NodeRef -> NodeRef -> BindFlag -> ConstraintM ()
rebindIfParent child expectedParent newParent flag = do
    bindParents <- gets bsBindParents
    case IntMap.lookup (nodeRefKey (typeRef child)) bindParents of
        Just (parent, _)
            | parent == expectedParent ->
                setBindParentOverride (typeRef child) newParent flag
        _ -> pure ()

-- | Internalize a source type annotation into a constraint graph.
-- This creates nodes for the type structure and connects them appropriately.
--
-- The tyEnv maps type variable names to their allocated NodeIds (for quantified vars).
internalizeSrcTypeBound :: GenNodeId -> TyEnv -> SrcType -> ConstraintM NodeId
internalizeSrcTypeBound = internalizeSrcTypeWith False

internalizeSrcTypeWith :: Bool -> GenNodeId -> TyEnv -> SrcType -> ConstraintM NodeId
internalizeSrcTypeWith wrap currentGen tyEnv srcType = case srcType of
    STVar name -> case Map.lookup name tyEnv of
        Just nid -> pure nid
        Nothing -> do
            -- Free type variable: allocate a fresh variable
            nid <- allocVar
            pure nid

    STArrow dom cod -> do
        domNode <- internalizeSrcTypeWith wrap currentGen tyEnv dom
        codNode <- internalizeSrcTypeWith wrap currentGen tyEnv cod
        arrowNode <- allocArrow domNode codNode
        if wrap
            then do
                varNode <- allocVar
                setVarBound varNode (Just arrowNode)
                pure varNode
            else pure arrowNode

    STBase name -> do
        baseNode <- allocBase (BaseTy name)
        if wrap
            then do
                varNode <- allocVar
                setVarBound varNode (Just baseNode)
                pure varNode
            else pure baseNode

    STForall var mBound body ->
        case mBound of
            Just (STVar alias) | alias /= var -> do
                aliasNode <- case Map.lookup alias tyEnv of
                    Just nid -> pure nid
                    Nothing -> allocVar
                let tyEnv' = Map.insert var aliasNode tyEnv
                internalizeSrcTypeWith False currentGen tyEnv' body
            Just (STVar _) ->
                internalizeSrcTypeWith wrap currentGen tyEnv (STForall var Nothing body)
            _ -> do
                Scope.pushScope
                schemeGenId <- allocGenNode []
                setBindParentIfMissing (genRef schemeGenId) (genRef currentGen) BindFlex
                -- Allocate a type variable for the bound variable
                varNode <- allocVar
                -- Extend the environment with this binding
                let tyEnv' = Map.insert var varNode tyEnv
                -- Process the bound if present (for instance bounds)
                mbBoundNode <- case mBound of
                    Nothing -> pure Nothing
                    Just bound -> do
                        Scope.pushScope
                        boundNode <- internalizeSrcTypeBound schemeGenId tyEnv' bound
                        boundScope <- Scope.popScope
                        mbBoundOwner <- lookupSchemeOwnerForRoot boundNode
                        case mbBoundOwner of
                            Just gid -> do
                                Scope.rebindScopeNodes (genRef gid) boundNode boundScope
                                setGenNodeSchemes gid [boundNode]
                            Nothing -> Scope.rebindScopeNodes (typeRef varNode) boundNode boundScope
                        case mbBoundOwner of
                            Nothing -> setBindParentOverride (typeRef boundNode) (typeRef varNode) BindFlex
                            Just gid -> setBindParentOverride (typeRef boundNode) (genRef gid) BindFlex
                        pure (Just boundNode)

                -- Record the bound on the variable
                setVarBound varNode mbBoundNode

                -- Internalize the body with the extended environment
                bodyNode <- internalizeSrcTypeWith False schemeGenId tyEnv' body
                scopeFrame <- Scope.popScope
                -- Represent explicit forall via a fresh gen node (named binders).
                Scope.rebindScopeNodes (genRef schemeGenId) bodyNode scopeFrame
                setBindParentOverride (typeRef varNode) (genRef schemeGenId) BindFlex
                setBindParentOverride (typeRef bodyNode) (genRef schemeGenId) BindFlex
                setGenNodeSchemes schemeGenId [bodyNode]
                pure bodyNode

    STBottom -> do
        -- Bottom is the minimal type, represented as a fresh variable
        -- that can be instantiated to anything
        allocVar

-- | Internalize a list of binders from a source scheme.
-- Returns an environment mapping variable names to their nodes,
-- and the list of allocated nodes.
internalizeBinders :: GenNodeId -> [(String, Maybe SrcType)] -> ConstraintM (TyEnv, [NodeId])
internalizeBinders currentGen bindings = go Map.empty [] bindings
  where
    go tyEnv acc [] = pure (tyEnv, reverse acc)
    go tyEnv acc ((name, mBound):rest) =
        case mBound of
            Just (STVar alias) | alias /= name -> do
                aliasNode <- case Map.lookup alias tyEnv of
                    Just nid -> pure nid
                    Nothing -> allocVar
                let tyEnv' = Map.insert name aliasNode tyEnv
                go tyEnv' acc rest
            _ -> do
                -- Allocate a type variable for this binding
                varNode <- allocVar
                let tyEnv' = Map.insert name varNode tyEnv
                let mBound' = case mBound of
                        Just (STVar v) | v == name -> Nothing
                        _ -> mBound
                -- Process the bound if present
                mbBoundNode <- case mBound' of
                    Nothing -> pure Nothing
                    Just bound -> do
                        Scope.pushScope
                        boundNode <- internalizeSrcTypeBound currentGen tyEnv' bound
                        boundScope <- Scope.popScope
                        mbBoundOwner <- lookupSchemeOwnerForRoot boundNode
                        case mbBoundOwner of
                            Just gid -> do
                                Scope.rebindScopeNodes (genRef gid) boundNode boundScope
                                setGenNodeSchemes gid [boundNode]
                            Nothing -> Scope.rebindScopeNodes (typeRef varNode) boundNode boundScope
                        case mbBoundOwner of
                            Nothing -> setBindParentOverride (typeRef boundNode) (typeRef varNode) BindFlex
                            Just gid -> setBindParentOverride (typeRef boundNode) (genRef gid) BindFlex
                        pure (Just boundNode)

                -- Set the bound
                setVarBound varNode mbBoundNode

                go tyEnv' (varNode:acc) rest

baseFor :: Lit -> BaseTy
baseFor lit = BaseTy $ case lit of
    LInt _ -> "Int"
    LBool _ -> "Bool"
    LString _ -> "String"
