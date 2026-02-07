{-# LANGUAGE GADTs #-}
module MLF.Frontend.ConstraintGen.Translate (
    buildRootExpr
) where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (gets, modify')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set

import MLF.Constraint.Types
import MLF.Frontend.Syntax
import MLF.Frontend.ConstraintGen.Emit
import qualified MLF.Frontend.ConstraintGen.Scope as Scope
import MLF.Frontend.ConstraintGen.State (BuildState(..), ConstraintM)
import MLF.Frontend.ConstraintGen.Types

buildRootExpr :: NormCoreExpr -> ConstraintM (GenNodeId, NodeId, AnnExpr)
buildRootExpr expr = do
    rootGen <- allocGenNode []
    (rootNode, annRoot) <- buildExpr Map.empty rootGen expr
    topFrame <- Scope.peekScope
    Scope.rebindScopeNodes (genRef rootGen) rootNode topFrame
    setBindParentIfMissing (typeRef rootNode) (genRef rootGen) BindFlex
    setGenNodeSchemes rootGen [rootNode]
    pure (rootGen, rootNode, annRoot)

buildExpr :: Env -> GenNodeId -> NormCoreExpr -> ConstraintM (NodeId, AnnExpr)
buildExpr env scopeRoot expr = do
    (rootNode, ann) <- buildExprRaw env scopeRoot expr
    setBindParentIfMissing (typeRef rootNode) (genRef scopeRoot) BindFlex
    pure (rootNode, ann)

buildExprRaw :: Env -> GenNodeId -> NormCoreExpr -> ConstraintM (NodeId, AnnExpr)
buildExprRaw env scopeRoot expr =
    case expr of
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
            -- Allocate children first, then create the arrow.
            argNode <- allocVar
            let env' = Map.insert param (Binding argNode Nothing) env
            (bodyNode, bodyAnn) <- buildExpr env' scopeRoot body
            let codNode = bodyNode
                bodyAnn' = bodyAnn
            -- allocArrow sets binding parents for dom/cod automatically
            arrowNode <- allocArrow argNode codNode
            -- Lambda parameters are bound at the current binding node (not under the arrow).
            setBindParentOverride (typeRef argNode) (genRef scopeRoot) BindFlex
            rootVar <- allocVar
            setVarBound rootVar (Just arrowNode)
            pure (rootVar, ALam param argNode scopeRoot bodyAnn' rootVar)

        -- Term annotation sugar: (a : τ) ≜ cτ a. See Note [Coercion domain/codomain semantics].
        EApp (ECoerceConst annTy) annotatedExpr ->
            buildCoerce env scopeRoot annTy annotatedExpr

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
                buildLet schemeGenId schemeRootNode rhsGen rhsAnn = do
                    let schemeGenUsed = schemeGenId
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
                buildInferred rhsExpr = do
                    schemeGenId <- allocGenNode []
                    (rhsNode, rhsAnn, rhsScope) <- buildUnder schemeGenId rhsExpr
                    schemeGenUsed <- fmap (maybe schemeGenId id) (lookupSchemeGenForRoot rhsNode)
                    setGenNodeSchemes schemeGenUsed [rhsNode]
                    Scope.rebindScopeNodes (genRef schemeGenUsed) rhsNode rhsScope
                    setBindParentOverride (typeRef rhsNode) (genRef schemeGenUsed) BindFlex
                    pure (schemeGenId, rhsNode, schemeGenId, rhsAnn)
            in do
                (schemeGenId, schemeRootNode, rhsGen, rhsAnn) <- buildInferred rhs
                buildLet schemeGenId schemeRootNode rhsGen rhsAnn

        -- We only expect coercion constants to appear in an application position,
        -- i.e. as the result of desugaring @(a : τ)@ to @cτ a@.
        ECoerceConst{} ->
            throwError (InternalConstraintError "buildExprRaw: unexpected bare ECoerceConst")

-- | Translate a coercion application @cτ a@ (surface form @(a : τ)@).
--
-- We treat coercions as a special form rather than a regular function
-- application so that Phase 1 produces exactly one instantiation edge for the
-- annotation site, and Phase 6 can elaborate it as an xMLF instantiation.
buildCoerce :: Env -> GenNodeId -> NormSrcType -> NormCoreExpr -> ConstraintM (NodeId, AnnExpr)
buildCoerce env scopeRoot annTy annotatedExpr = do
    (exprNode, exprAnn) <- buildExpr env scopeRoot annotatedExpr
    annGen <- allocGenNode []
    setBindParentIfMissing (genRef annGen) (genRef scopeRoot) BindFlex
    Scope.pushScope
    (domainNode, codomainNode) <- internalizeCoercionType annGen annTy
    annScope <- Scope.popScope
    Scope.rebindScopeNodes (genRef annGen) codomainNode annScope

    -- If we're annotating a variable occurrence, prefer instantiating from the
    -- underlying scheme root rather than stacking expansions (see tests).
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

Both copies remain wrapped. The codomain is returned as the annotation result,
while the domain stays the instantiation target.
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

{- Note [Alternative let scoping (Figure 15.2.6)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The thesis distinguishes two typing constraints for `let x = a in b`
(papers/these-finale-english.txt §15.2.6, Figure 15.2.6):

  1) The "basic" / leftmost constraint does *not* introduce a fresh gen node for
     the whole let expression. Instead, it piggybacks on the gen node introduced
     for `b`.

     This is the nicer constraint for *type inference* (smaller/simpler), but it
     has an unusual scope interaction: the scope of the let expression (hence of
     `b`) is visible from `a`. The thesis explicitly calls out that this severely
     complicates translation into xMLF.

  2) The "alternative" / rightmost constraint introduces an extra gen node for
     the let expression and a trivial type scheme at the root, plus a single
     additional instantiation edge from the body to that trivial scheme.

We follow the thesis' *translation-friendly* choice (2):

  - `letGen` is the gen node for the whole let expression.
  - `trivialRoot` is the (bottom) type node used as the trivial scheme root.
  - We translate the body under `bodyGen`, then add `letEdge : bodyNode ≤ trivialRoot`.

This makes the binding/scope structure well-behaved for translation/elaboration.
In the principal presolution, the added instantiation edge corresponds to the
identity computation (thesis §15.2.6.1).

Implementation detail: `letEdge` is recorded in `cLetEdges` (via `recordLetEdge`)
so presolution/elaboration can drop its witness/expansion (`dropTrivialSchemeEdges`)
and avoid generating spurious instantiation computations for this internal edge.
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

-- | Internalize a coercion type κ as a rigid domain and flexible codomain,
-- sharing existential (free) variables across both copies.
internalizeCoercionType :: GenNodeId -> NormSrcType -> ConstraintM (NodeId, NodeId)
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
    -> NormSrcType
    -> ConstraintM (NodeId, SharedEnv)
internalizeCoercionCopy bindFlag wrap coerceGen currentGen tyEnv shared srcType =
    case srcType of
        -- Domain copies use BindRigid to mark coercion-local nodes as restricted.
        -- To avoid locked descendants, rebind children auto-bound under structural
        -- nodes back to the current gen when rigid.
        NSTVar name ->
            case Map.lookup name tyEnv of
                Just nid -> pure (nid, shared)
                Nothing ->
                    case Map.lookup name shared of
                        Just nid -> pure (nid, shared)
                        Nothing -> do
                            nid <- allocVar
                            setBindParentOverride (typeRef nid) (genRef coerceGen) BindFlex
                            pure (nid, Map.insert name nid shared)

        NSTArrow dom cod -> do
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

        NSTBase name -> do
            registerTyConArity (BaseTy name) 0
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

        NSTCon name args -> do
            let arity = NE.length args
            registerTyConArity (BaseTy name) arity
            (argNodes, sharedFinal) <- foldM
                (\(accNodes, accShared) argTy -> do
                    (argNode, newShared) <-
                        internalizeCoercionCopy bindFlag wrap coerceGen currentGen tyEnv accShared argTy
                    pure (accNodes ++ [argNode], newShared))
                ([], shared)
                (NE.toList args)
            conNode <- case argNodes of
                (h:t) -> allocCon (BaseTy name) (h :| t)
                [] -> error "NSTCon must have at least one argument"
            case bindFlag of
                BindRigid -> do
                    mapM_ (\argNode -> rebindIfParent argNode (typeRef conNode) (genRef currentGen) bindFlag) argNodes
                BindFlex -> pure ()
            if wrap
                then do
                    varNode <- allocVar
                    setVarBound varNode (Just conNode)
                    case bindFlag of
                        BindRigid ->
                            rebindIfParent conNode (typeRef varNode) (genRef currentGen) bindFlag
                        BindFlex -> pure ()
                    setBindParentOverride (typeRef varNode) (genRef currentGen) bindFlag
                    pure (varNode, sharedFinal)
                else pure (conNode, sharedFinal)

        NSTForall var mBound body -> do
            -- Note: Alias bounds (∀(b ⩾ a). body where the bound is a bare
            -- variable) are unreachable here — normalization inlines them via
            -- capture-avoiding substitution before constraint generation.
            -- StructBound has no variable constructor, so mBound :: Maybe StructBound
            -- can only be Nothing or a structural bound (arrow, base, con, forall, bottom).
            --
            -- Well-formedness check: binder must not occur in its own structural bound.
            -- This catches cases like ∀(a ⩾ List a). a where the binder appears
            -- nested inside a structural bound.
            case mBound of
                Just bound
                    | Set.member var (structBoundFreeVars bound) ->
                        throwError (ForallBoundMentionsBinder var)
                _ -> pure ()
            Scope.pushScope
            schemeGenId <- allocGenNode []
            setBindParentIfMissing (genRef schemeGenId) (genRef currentGen) BindFlex
            varNode <- allocVar
            let tyEnv' = Map.insert var varNode tyEnv
            (mbBoundNode, shared1) <- case mBound of
                Nothing -> pure (Nothing, shared)
                Just bound -> do
                    let boundAsNorm = structBoundToNormSrcType bound
                    Scope.pushScope
                    (boundNode, shared2) <-
                        internalizeCoercionCopy bindFlag False coerceGen schemeGenId tyEnv' shared boundAsNorm
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

        NSTBottom -> do
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

baseFor :: Lit -> BaseTy
baseFor lit = BaseTy $ case lit of
    LInt _ -> "Int"
    LBool _ -> "Bool"
    LString _ -> "String"

-- | Register the arity of a type constructor. If the constructor has already
-- been seen with a different arity, throw TypeConstructorArityMismatch.
registerTyConArity :: BaseTy -> Int -> ConstraintM ()
registerTyConArity con arity = do
    arityMap <- gets bsTyConArity
    case Map.lookup con arityMap of
        Just existingArity
            | existingArity /= arity ->
                throwError (TypeConstructorArityMismatch con existingArity arity)
        _ -> modify' $ \st ->
            st { bsTyConArity = Map.insert con arity (bsTyConArity st) }

-- | Convert a 'StructBound' back to 'NormSrcType' for recursive internalization.
structBoundToNormSrcType :: StructBound -> NormSrcType
structBoundToNormSrcType sb = case sb of
    SBArrow dom cod -> NSTArrow dom cod
    SBBase name -> NSTBase name
    SBCon name args -> NSTCon name args
    SBForall v mb body -> NSTForall v mb body
    SBBottom -> NSTBottom

-- | Check if a type variable name occurs free in a 'StructBound'.
structBoundFreeVars :: StructBound -> Set.Set String
structBoundFreeVars = go Set.empty
  where
    go bound sb = case sb of
        SBArrow dom cod -> normFreeVars bound dom <> normFreeVars bound cod
        SBBase _ -> Set.empty
        SBCon _ args -> foldMap (normFreeVars bound) args
        SBForall v mb body ->
            let bound' = Set.insert v bound
            in maybe Set.empty (go bound) mb <> normFreeVars bound' body
        SBBottom -> Set.empty

    normFreeVars bound ty = case ty of
        NSTVar v -> if Set.member v bound then Set.empty else Set.singleton v
        NSTArrow dom cod -> normFreeVars bound dom <> normFreeVars bound cod
        NSTBase _ -> Set.empty
        NSTCon _ args -> foldMap (normFreeVars bound) args
        NSTForall v mb body ->
            let bound' = Set.insert v bound
            in maybe Set.empty (go bound) mb <> normFreeVars bound' body
        NSTBottom -> Set.empty
