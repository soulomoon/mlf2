{-# LANGUAGE GADTs #-}

module MLF.Frontend.ConstraintGen.Translate
  ( buildRootExprWithExternalBindings,
    buildModuleRootExprsKeyedWithExternalBindings,
  )
where

import Control.Monad (foldM, forM)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (gets, modify')
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import MLF.Constraint.Types.Graph
import MLF.Frontend.ConstraintGen.Emit
import qualified MLF.Frontend.ConstraintGen.Scope as Scope
import MLF.Frontend.ConstraintGen.State (BuildState (..), ConstraintM, ScopeFrame, resolveTypeHeadIdentity, withModuleRootOwner)
import MLF.Frontend.ConstraintGen.Types
import MLF.Frontend.Syntax
import MLF.Types.Identity
  ( IdDetails,
    TypeBinderIdentity,
    idDetailsIdentityKey,
    lookupTypeBinderIdentityAlias,
    typeBinderIdentityFromNode
  )

buildRootExprWithExternalBindings :: ExternalBindings -> ResolvedNormCoreExpr -> ConstraintM (GenNodeId, Env, NodeId, AnnExpr)
buildRootExprWithExternalBindings extBindings expr = do
  (rootGen, initialBindings) <- buildInitialExternalBindings extBindings
  withTypeBinderRoot rootGen $ do
    referencedBindings <- materializeReferencedExternalBindings expr initialBindings
    (rootNode, annRoot) <- buildRootExprFromInitialEnv rootGen referencedBindings expr
    pure (rootGen, referencedBindings, rootNode, annRoot)

buildModuleRootExprsKeyedWithExternalBindings :: (Ord key) => Map key (Map String TypeBinderIdentity) -> ExternalBindings -> [(key, VarName, ResolvedNormCoreExpr)] -> ConstraintM (GenNodeId, Env, Map key (ModuleRootId, NodeId, AnnExpr, Map String TypeBinderIdentity))
buildModuleRootExprsKeyedWithExternalBindings rootTypeBinderIdentities extBindings keyedExprs = do
  moduleGen <- allocGenNode []
  sharedTypeBinderIdentities <- gets bsTypeBinderIdentities
  builtRoots <-
    forM (zip [0 ..] keyedExprs) $ \(rootIndex, (key, _name, expr)) -> do
      let referencedRefs = freeCoreBindingReferences expr
      let rootId = ModuleRootId rootIndex
      let typeBinderIdentities =
            Map.findWithDefault Map.empty key rootTypeBinderIdentities
              `Map.union` sharedTypeBinderIdentities
      (referencedBindings, rootNode, annRoot, sourceTypeBinderAliases) <-
        withTypeBinderIdentities typeBinderIdentities $
          withModuleRootOwner rootId $ do
            rootGen <- allocChildGenUnder moduleGen
            withTypeBinderRoot rootGen $ do
              initialBindings <- buildInitialEnvForReferences rootGen referencedRefs extBindings
              withRootLocalExternalBindingCache $ do
                referencedBindings <- materializeExternalBindingReferences referencedRefs initialBindings
                (rootNode, annRoot) <- buildModuleRootExprFromInitialEnv rootGen referencedBindings expr
                sourceTypeBinderAliases <- gets bsTypeBinderIdentities
                pure (referencedBindings, rootNode, annRoot, sourceTypeBinderAliases)
      pure (key, rootId, referencedBindings, rootNode, annRoot, sourceTypeBinderAliases)
  let rootMap =
        Map.fromList
          [ (key, (rootId, rootNode, annRoot, sourceTypeBinderAliases))
          | (key, rootId, _, rootNode, annRoot, sourceTypeBinderAliases) <- builtRoots
          ]
      mergedEnv =
        foldl'
          (Map.unionWith preferMaterializedBinding)
          Map.empty
          [referencedBindings | (_, _, referencedBindings, _, _, _) <- builtRoots]
  pure (moduleGen, mergedEnv, rootMap)
  where
    preferMaterializedBinding old new =
      case (old, new) of
        (Binding {}, _) -> old
        (_, Binding {}) -> new
        _ -> old

withRootLocalExternalBindingCache :: ConstraintM a -> ConstraintM a
withRootLocalExternalBindingCache action = do
  oldCache <- gets bsExternalBindingCache
  modify' $ \st -> st {bsExternalBindingCache = Map.empty}
  out <- action
  modify' $ \st -> st {bsExternalBindingCache = oldCache}
  pure out

withTypeBinderIdentities :: Map String TypeBinderIdentity -> ConstraintM a -> ConstraintM a
withTypeBinderIdentities identities action = do
  oldIdentities <- gets bsTypeBinderIdentities
  modify' $ \st -> st {bsTypeBinderIdentities = identities}
  out <- action
  modify' $ \st -> st {bsTypeBinderIdentities = oldIdentities}
  pure out

withTypeBinderRoot :: GenNodeId -> ConstraintM a -> ConstraintM a
withTypeBinderRoot root action = do
  oldRoot <- gets bsTypeBinderRoot
  modify' $ \st -> st {bsTypeBinderRoot = Just root}
  out <- action
  modify' $ \st -> st {bsTypeBinderRoot = oldRoot}
  pure out

buildModuleRootExprFromInitialEnv :: GenNodeId -> Env -> ResolvedNormCoreExpr -> ConstraintM (NodeId, AnnExpr)
buildModuleRootExprFromInitialEnv rootGen initialBindings expr = do
  ((rootNode, annRoot), frame) <-
    withScopedBuild (buildExpr initialBindings rootGen expr)
  rebindScopeRoot (genRef rootGen) rootNode frame
  setBindParentIfMissing (typeRef rootNode) (genRef rootGen) BindFlex
  setGenNodeSchemes rootGen [rootNode]
  pure (rootNode, annRoot)

buildInitialExternalBindings :: ExternalBindings -> ConstraintM (GenNodeId, Env)
buildInitialExternalBindings extBindings = do
  rootGen <- allocGenNode []
  initialBindings <- buildInitialEnv rootGen extBindings
  pure (rootGen, initialBindings)

buildRootExprFromInitialEnv :: GenNodeId -> Env -> ResolvedNormCoreExpr -> ConstraintM (NodeId, AnnExpr)
buildRootExprFromInitialEnv rootGen initialBindings expr = do
  (rootNode, annRoot) <- buildExpr initialBindings rootGen expr
  topFrame <- Scope.peekScope
  rebindScopeRoot (genRef rootGen) rootNode topFrame
  setBindParentIfMissing (typeRef rootNode) (genRef rootGen) BindFlex
  setGenNodeSchemes rootGen [rootNode]
  pure (rootNode, annRoot)

-- | Build an initial 'Env' from an 'ExternalEnv' without allocating every
-- external scheme graph.  Referenced free variables are materialized before
-- translating the expression, preserving the eager binding shape for used
-- entries while avoiding the unused external graph.
buildInitialEnv :: GenNodeId -> ExternalBindings -> ConstraintM Env
buildInitialEnv rootGen extBindings =
  pure $
    Map.fromList
      [ ( externalBindingKey externalBinding,
          LazyExternalBinding
            { bindingExternalRoot = rootGen,
              bindingExternal = externalBinding
            }
        )
      | externalBinding <- Map.elems extBindings
      ]

buildInitialEnvForReferences :: GenNodeId -> [BindingKey] -> ExternalBindings -> ConstraintM Env
buildInitialEnvForReferences rootGen references extBindings = do
  initialEnv <- buildInitialEnv rootGen extBindings
  pure (Map.restrictKeys initialEnv (Set.fromList references))

externalBindingKey :: ExternalBinding -> BindingKey
externalBindingKey externalBinding =
  ResolvedBindingKey
    (idDetailsIdentityKey (externalBindingDetails (externalBindingIdentity externalBinding)))

-- | Create a let-bound polymorphic 'Binding' for an external variable.
-- Allocates a child gen node under the root, internalizes the source
-- type as a flexible copy, and returns a binding with the scheme root
-- and gen node so that variable references get expansion nodes.
buildExternalBinding :: GenNodeId -> ExternalBinding -> ConstraintM Binding
buildExternalBinding rootGen externalBinding@ExternalBinding {externalBindingType = srcTy, externalBindingMode = mode} = do
  inheritedTypeBinderIdentities <- gets bsTypeBinderIdentities
  withTypeBinderIdentities
    ( externalBindingTypeBinderIdentities externalBinding
        `Map.union` inheritedTypeBinderIdentities
    )
    ( case mode of
        ExternalBindingScheme -> do
          extSchemeGen <- allocChildGenUnder rootGen
          ((extSchemeRoot, _shared), scopeFrame) <-
            withScopedBuild
              (internalizeCoercionCopy BindFlex True extSchemeGen extSchemeGen Map.empty Map.empty srcTy)
          rebindScopeRoot (genRef extSchemeGen) extSchemeRoot scopeFrame
          setBindParentIfMissing (typeRef extSchemeRoot) (genRef extSchemeGen) BindFlex
          setGenNodeSchemes extSchemeGen [extSchemeRoot]
          pure Binding {bindingNode = extSchemeRoot, bindingGen = Just extSchemeGen, bindingIdentity = externalDetails}
        ExternalBindingMonomorphic -> do
          -- A monomorphic external is exact, not unknown.  Deferred constructors
          -- use this mode so their specialized source type participates in
          -- inference without acquiring a let-polymorphic expansion layer.
          -- Discarding the type here forces later applications to reconstruct it
          -- from an unconstrained variable and can emit Raise across a source
          -- binder owned by another root.
          (monoRoot, _) <-
            internalizeCoercionCopy
              BindFlex
              True
              rootGen
              rootGen
              Map.empty
              Map.empty
              srcTy
          setBindParentIfMissing (typeRef monoRoot) (genRef rootGen) BindFlex
          pure Binding {bindingNode = monoRoot, bindingGen = Nothing, bindingIdentity = externalDetails}
    )
  where
    externalDetails = externalBindingDetails (externalBindingIdentity externalBinding)

buildExpr :: Env -> GenNodeId -> ResolvedNormCoreExpr -> ConstraintM (NodeId, AnnExpr)
buildExpr env scopeRoot expr = do
  (rootNode, ann) <- buildExprRaw env scopeRoot expr
  setBindParentIfMissing (typeRef rootNode) (genRef scopeRoot) BindFlex
  pure (rootNode, ann)

withScopedBuild :: ConstraintM a -> ConstraintM (a, ScopeFrame)
withScopedBuild action = do
  Scope.pushScope
  out <- action
  frame <- Scope.popScope
  pure (out, frame)

attachUnder :: NodeRef -> NodeRef -> BindFlag -> ConstraintM ()
attachUnder child parent flag =
  setBindParentOverride child parent flag

rebindScopeRoot :: NodeRef -> NodeId -> ScopeFrame -> ConstraintM ()
rebindScopeRoot binder root frame =
  Scope.rebindScopeNodes binder root frame

withScopedRebind :: NodeRef -> (a -> NodeId) -> ConstraintM a -> ConstraintM a
withScopedRebind binder rootOf action = do
  (out, frame) <- withScopedBuild action
  rebindScopeRoot binder (rootOf out) frame
  pure out

allocChildGenUnder :: GenNodeId -> ConstraintM GenNodeId
allocChildGenUnder parent = do
  child <- allocGenNode []
  setBindParentIfMissing (genRef child) (genRef parent) BindFlex
  pure child

withWrappedNode ::
  BindFlag ->
  Bool ->
  GenNodeId ->
  NodeId ->
  SharedEnv ->
  ConstraintM (NodeId, SharedEnv)
withWrappedNode bindFlag wrap currentGen innerNode shared =
  if wrap
    then do
      varNode <- allocVar
      setVarBound varNode (Just innerNode)
      whenRigid bindFlag $
        rebindIfParent innerNode (typeRef varNode) (genRef currentGen) bindFlag
      setBindParentOverride (typeRef varNode) (genRef currentGen) bindFlag
      pure (varNode, shared)
    else pure (innerNode, shared)

whenRigid :: BindFlag -> ConstraintM () -> ConstraintM ()
whenRigid bindFlag action =
  case bindFlag of
    BindRigid -> action
    BindFlex -> pure ()

rebindStructuralChildren ::
  BindFlag ->
  NodeId ->
  GenNodeId ->
  SharedEnv ->
  [NodeId] ->
  ConstraintM ()
rebindStructuralChildren bindFlag parent currentGen shared children =
  case bindFlag of
    BindRigid ->
      mapM_
        (\child -> rebindIfParent child (typeRef parent) (genRef currentGen) BindRigid)
        children
    BindFlex ->
      mapM_
        (\child -> unlessShared shared child (attachUnder (typeRef child) (typeRef parent) BindFlex))
        children

unlessShared :: SharedEnv -> NodeId -> ConstraintM () -> ConstraintM ()
unlessShared shared node action =
  if node `elem` Map.elems shared
    then pure ()
    else action

setExpBody :: NodeId -> NodeId -> ConstraintM ()
setExpBody expNode bodyNode = do
  modify' $ \st ->
    let nodes0 = bsNodes st
        key = getNodeId expNode
     in case IntMap.lookup key nodes0 of
          Just te@TyExp {} ->
            st {bsNodes = IntMap.insert key te {tnBody = bodyNode} nodes0}
          _ -> st
  setBindParentIfMissing (typeRef bodyNode) (typeRef expNode) BindFlex

unwrapSchemeRoot :: NodeId -> ConstraintM NodeId
unwrapSchemeRoot start = do
  nodes <- gets bsNodes
  let go visited nid
        | IntSet.member (getNodeId nid) visited = nid
        | otherwise =
            case IntMap.lookup (getNodeId nid) nodes of
              Just TyVar {tnBound = Just bnd} ->
                go (IntSet.insert (getNodeId nid) visited) bnd
              _ -> nid
  pure (go IntSet.empty start)

rhsMentionsBinder :: BindingKey -> ResolvedNormCoreExpr -> Bool
rhsMentionsBinder needle expr =
  case expr of
    EVarNode reference -> bindingKeyForTermReference reference == needle
    ELit _ -> False
    ELamNode reference body
      | bindingKeyForTermReference reference == needle -> False
      | otherwise -> rhsMentionsBinder needle body
    EApp fun arg ->
      rhsMentionsBinder needle fun || rhsMentionsBinder needle arg
    ELetNode reference rhs body
      | bindingKeyForTermReference reference == needle -> False
      | otherwise ->
          rhsMentionsBinder needle rhs || rhsMentionsBinder needle body
    EExactLamNode reference _ body
      | bindingKeyForTermReference reference == needle -> False
      | otherwise -> rhsMentionsBinder needle body
    ECoerceConst {} -> False
    EExactCoerceConst {} -> False

freeCoreBindingReferences :: ResolvedNormCoreExpr -> [BindingKey]
freeCoreBindingReferences = Set.toAscList . go Set.empty
  where
    go :: Set.Set BindingKey -> ResolvedNormCoreExpr -> Set.Set BindingKey
    go bound expr =
      case expr of
        EVarNode reference ->
          freeReference bound (bindingKeyForTermReference reference)
        ELit _ -> Set.empty
        ELamNode reference body ->
          go (Set.insert (bindingKeyForTermReference reference) bound) body
        EApp fun arg ->
          go bound fun <> go bound arg
        ELetNode reference rhs body ->
          let key = bindingKeyForTermReference reference
              bound' = Set.insert key bound
           in go bound' rhs <> go bound' body
        EExactLamNode reference _ body ->
          go (Set.insert (bindingKeyForTermReference reference) bound) body
        ECoerceConst {} -> Set.empty
        EExactCoerceConst {} -> Set.empty

    freeReference bound key
      | Set.member key bound = Set.empty
      | otherwise = Set.singleton key

materializeReferencedExternalBindings :: ResolvedNormCoreExpr -> Env -> ConstraintM Env
materializeReferencedExternalBindings expr env0 =
  materializeExternalBindingReferences (freeCoreBindingReferences expr) env0

materializeExternalBindingReferences :: [BindingKey] -> Env -> ConstraintM Env
materializeExternalBindingReferences references env0 =
  foldM materializeOne env0 references
  where
    materializeOne acc reference =
      case Map.lookup reference acc of
        Just lazy@LazyExternalBinding{} -> do
          binding <-
            materializeBinding lazy
          pure (Map.insert reference binding acc)
        _ -> pure acc

buildExprRaw :: Env -> GenNodeId -> ResolvedNormCoreExpr -> ConstraintM (NodeId, AnnExpr)
buildExprRaw env scopeRoot expr =
  case expr of
    EVarNode reference ->
      buildVar env (bindingKeyForTermReference reference) (resolvedTermReferenceDetails reference) (termReferenceName reference)
    ELit lit -> do
      let base = baseFor lit
      identity <- resolveTypeHeadIdentity (getBaseName base)
      baseNode <- allocBase identity base
      varNode <- allocVar
      setVarBound varNode (Just baseNode)
      pure (varNode, ALit lit varNode)
    -- See Note [Lambda Translation]
    ELamNode reference body ->
      buildLambda
        env
        scopeRoot
        (bindingKeyForTermReference reference)
        (resolvedTermReferenceDetails reference)
        (termReferenceName reference)
        body

    EExactLamNode reference paramTy body ->
      buildExactLambda
        env
        scopeRoot
        (bindingKeyForTermReference reference)
        (resolvedTermReferenceDetails reference)
        (termReferenceName reference)
        paramTy
        body

    -- Term annotation sugar: (a : τ) ≜ cτ a. See Note [Coercion domain/codomain semantics].
    EApp (ECoerceConst annTy) annotatedExpr ->
      buildCoerce env scopeRoot annTy annotatedExpr
    -- Compiler-generated checks carry an exact producer type. They are kept
    -- distinct from source κσ so Phase 6 can consume that authority directly.
    EApp (EExactCoerceConst annTy exactTy) annotatedExpr ->
      buildExactCoerce env scopeRoot annTy exactTy annotatedExpr
    -- See Note [Application and Instantiation Edges]
    EApp fun arg -> do
      (funNode, funAnn) <- buildExpr env scopeRoot fun
      (argNode, argAnn) <- buildExpr env scopeRoot arg
      domNode <- allocVar
      resultNode <- allocVar
      -- allocArrow sets binding parents for dom/cod automatically
      arrowNode <- allocArrow domNode resultNode
      -- Instantiation-edge destinations are existential constraint nodes.  The
      -- thesis requires each destination to be bound on a gen node
      -- (Definition 9.2.1, condition 10), so keep the fresh application
      -- variables at the application scope rather than under the arrow's
      -- structural root.  This also makes that scope the expansion destination
      -- by construction during propagation (Definition 10.3.2).
      setBindParentOverride (typeRef domNode) (genRef scopeRoot) BindFlex
      setBindParentOverride (typeRef resultNode) (genRef scopeRoot) BindFlex
      funEid <- addInstEdge funNode arrowNode
      argEid <- addInstEdge argNode domNode
      case funAnn of
        ALam _ _ paramNode _ _ _ _ -> do
          nodes <- gets bsNodes
          case IntMap.lookup (getNodeId paramNode) nodes of
            Just TyVar {tnBound = Nothing} -> pure ()
            _ -> setVarBound domNode (Just paramNode)
        _ -> pure ()
      -- The result node is what we return, but the arrow is the structural root
      let funSite = mkArrowInstantiationSite funEid funNode arrowNode domNode resultNode
          argSite = mkInstantiationSite argEid argNode domNode
      pure (resultNode, AApp funAnn argAnn funSite argSite resultNode)

    -- See Note [Let Bindings and Expansion Variables]
    ELetNode reference rhs body ->
      buildLet
        env
        scopeRoot
        (bindingKeyForTermReference reference)
        (resolvedTermReferenceDetails reference)
        (termReferenceName reference)
        rhs
        body

    -- We only expect coercion constants to appear in an application position,
    -- i.e. as the result of desugaring @(a : τ)@ to @cτ a@.
    ECoerceConst {} ->
      throwError UnexpectedBareCoercionConst
    EExactCoerceConst {} ->
      throwError UnexpectedBareCoercionConst

buildVar :: Env -> BindingKey -> IdDetails -> VarName -> ConstraintM (NodeId, AnnExpr)
buildVar env bindingKey details name = do
  binding <- lookupVar env bindingKey name >>= materializeBinding
  case binding of
    Binding nid mGen _bindingDetails ->
      case mGen of
        -- Polymorphic bindings (let-bound schemes) get a fresh expansion node.
        Just _ -> do
          (expNode, _) <- allocExpNode nid
          propagateSourceTypeBinderIdentity nid expNode
          pure (expNode, AResolvedVar details name expNode)
        -- Monomorphic bindings (e.g. lambda parameters) do not need expansion.
        Nothing ->
          pure (nid, AResolvedVar details name nid)
    LazyExternalBinding {} ->
      throwError (InternalConstraintError ("unmaterialized external binding for " ++ name))
buildLambda :: Env -> GenNodeId -> BindingKey -> IdDetails -> VarName -> ResolvedNormCoreExpr -> ConstraintM (NodeId, AnnExpr)
buildLambda env scopeRoot bindingKey binderDetails param body = do
  -- Allocate the source body first, then construct its paper-prescribed
  -- instantiation boundary to a fresh arrow codomain.
  argNode <- allocVar
  recordAnnotatedLambdaParameterSourceIdentity bindingKey argNode body
  let env' = Map.insert bindingKey (Binding argNode Nothing binderDetails) env
  (bodyNode, bodyAnn) <- buildExpr env' scopeRoot body
  codomainNode <- allocVar
  -- allocArrow sets binding parents for dom/cod automatically.
  arrowNode <- allocArrow argNode codomainNode
  -- Lambda parameters and instantiation destinations are bound at the current
  -- binding node (not under the arrow).
  setBindParentOverride (typeRef argNode) (genRef scopeRoot) BindFlex
  setBindParentOverride (typeRef codomainNode) (genRef scopeRoot) BindFlex
  bodyEid <- addInstEdge bodyNode codomainNode
  rootVar <- allocVar
  setVarBound rootVar (Just arrowNode)
  pure (rootVar, ALam param binderDetails argNode scopeRoot bodyAnn bodyEid rootVar)

-- | Source @lambda (x : a) body@ is translated, as in Chapter 12.3.2, to
-- @lambda x. let x' = kappa_a x in body@.  Var-Abs makes the outer parameter
-- monomorphic, so when the annotation is one already-resolved source binder,
-- that parameter node denotes the same binder by construction.  Publish the
-- identity before presolution; otherwise the annotation copy retains the
-- source identity while the parameter is later reified with a graph-local one.
recordAnnotatedLambdaParameterSourceIdentity :: BindingKey -> NodeId -> ResolvedNormCoreExpr -> ConstraintM ()
recordAnnotatedLambdaParameterSourceIdentity bindingKey argNode body =
  case body of
    ELetNode _ (EApp (ECoerceConst (STVar alias)) (EVarNode occurrenceReference)) _
      | bindingKeyForTermReference occurrenceReference == bindingKey -> do
          identities <- gets bsTypeBinderIdentities
          case lookupTypeBinderIdentityAlias identities alias of
            Just identity -> recordSourceTypeBinderIdentity argNode identity
            Nothing -> pure ()
    _ -> pure ()

-- | Build a compiler-owned lambda whose parameter type is authoritative.
-- Unlike a source 'ELamAnn', this does not create κσ's flexible codomain: the
-- evidence parameter is already typed by class metadata and must remain the
-- exact argument of the runtime function.
buildExactLambda :: Env -> GenNodeId -> BindingKey -> IdDetails -> VarName -> NormSrcType -> ResolvedNormCoreExpr -> ConstraintM (NodeId, AnnExpr)
buildExactLambda env scopeRoot bindingKey binderDetails param paramTy body = do
  (argNode, _) <-
    internalizeCoercionCopy
      BindFlex
      True
      scopeRoot
      scopeRoot
      Map.empty
      Map.empty
      paramTy
  modify' $ \st ->
    st
      { bsAnnSourceTypes =
          IntMap.insert (getNodeId argNode) paramTy (bsAnnSourceTypes st)
      }
  let env' = Map.insert bindingKey (Binding argNode Nothing binderDetails) env
  (bodyNode, bodyAnn) <- buildExpr env' scopeRoot body
  codomainNode <- allocVar
  arrowNode <- allocArrow argNode codomainNode
  setBindParentOverride (typeRef argNode) (genRef scopeRoot) BindFlex
  setBindParentOverride (typeRef codomainNode) (genRef scopeRoot) BindFlex
  bodyEid <- addInstEdge bodyNode codomainNode
  rootVar <- allocVar
  setVarBound rootVar (Just arrowNode)
  pure (rootVar, ALam param binderDetails argNode scopeRoot bodyAnn bodyEid rootVar)

buildLet :: Env -> GenNodeId -> BindingKey -> IdDetails -> VarName -> ResolvedNormCoreExpr -> ResolvedNormCoreExpr -> ConstraintM (NodeId, AnnExpr)
buildLet env scopeRoot bindingKey binderDetails name rhs body = do
  (ownedSchemeGen, schemeGenUsed, schemeRootNode, rhsGen, rhsAnn) <-
    if shouldBuildRecursive
      then buildRecursive rhs
      else do
        alias <- buildPolymorphicAlias rhs
        case alias of
          Just builtAlias -> pure builtAlias
          Nothing -> buildInferred rhs
  buildBody ownedSchemeGen schemeGenUsed schemeRootNode rhsGen rhsAnn
  where
    buildUnder env0 gen subExpr = do
      ((node, ann), scope) <- withScopedBuild (buildExpr env0 gen subExpr)
      pure (node, ann, scope)

    shouldBuildRecursive =
      Map.notMember bindingKey env && rhsMentionsBinder bindingKey rhs

    buildBody ownedSchemeGen schemeGenUsed schemeRootNode rhsGen rhsAnn = do
      let env' = Map.insert bindingKey (Binding schemeRootNode (Just schemeGenUsed) binderDetails) env

      -- Alternative let scoping (Fig. 15.2.6, rightmost constraint):
      -- introduce a gen node for the let expression and a trivial scheme root.
      letGen <- allocChildGenUnder scopeRoot
      case ownedSchemeGen of
        Just schemeGenId -> do
          attachUnder (genRef schemeGenId) (genRef letGen) BindFlex
          if rhsGen /= schemeGenId
            then attachUnder (genRef rhsGen) (genRef letGen) BindFlex
            else pure ()
        Nothing -> pure ()
      bodyGen <- allocChildGenUnder letGen

      trivialRoot <- allocVar
      setBindParentIfMissing (typeRef trivialRoot) (genRef letGen) BindFlex
      setGenNodeSchemes letGen [trivialRoot]

      (bodyNode, bodyAnn0) <-
        withScopedRebind (genRef bodyGen) fst (buildExpr env' bodyGen body)

      letEdge <- addInstEdge bodyNode trivialRoot
      recordLetEdge letEdge

      -- This constraint-only identity edge has no xMLF term.  Keeping it out
      -- of 'AAnn' ensures every 'AAnn' denotes a source coercion with
      -- authoritative replay artifacts. 'ALetScope' retains the result-root
      -- metadata needed by generalization without pretending to be a coercion.
      let bodyAnn = ALetScope bodyAnn0 trivialRoot letEdge
      pure (trivialRoot, ALet name binderDetails schemeGenUsed schemeRootNode (ExpVarId 0) rhsGen rhsAnn bodyAnn trivialRoot)

    -- Var-Let (§12.4.1) makes a direct let-bound variable occurrence an
    -- indirection.  Reuse the source scheme rather than wrapping it in another
    -- expansion and then trying to reconstruct that alias after presolution.
    buildPolymorphicAlias rhsExpr =
      case rhsExpr of
        EVarNode reference -> do
          sourceBinding <-
            lookupVar
              env
              (bindingKeyForTermReference reference)
              (termReferenceName reference)
              >>= materializeBinding
          case sourceBinding of
            Binding sourceRoot (Just sourceGen) _ ->
              pure
                ( Just
                    ( Nothing,
                      sourceGen,
                      sourceRoot,
                      sourceGen,
                      AResolvedVar
                        (resolvedTermReferenceDetails reference)
                        (termReferenceName reference)
                        sourceRoot
                    )
                )
            _ -> pure Nothing
        _ -> pure Nothing

    buildInferred rhsExpr = do
      schemeGenId <- allocGenNode []
      (rhsNode, rhsAnn, rhsScope) <- buildUnder env schemeGenId rhsExpr
      schemeOwner <- fmap (maybe schemeGenId id) (lookupSchemeGenForRoot rhsNode)
      rebindScopeRoot (genRef schemeOwner) rhsNode rhsScope
      setGenNodeSchemes schemeOwner [rhsNode]
      attachUnder (typeRef rhsNode) (genRef schemeOwner) BindFlex
      -- A source annotation already constructs its result under the
      -- annotation-owned gen.  Keep the enclosing RHS gen as the lexical
      -- construction scope, but publish the scheme owner that actually owns
      -- the root.  Conflating these two authorities makes elaboration collect
      -- an outer Gamma obligation and then try to place it at the inner
      -- annotation gen.
      pure (Just schemeGenId, schemeOwner, rhsNode, schemeGenId, rhsAnn)

    buildRecursive rhsExpr = do
      schemeGenId <- allocGenNode []
      recursiveBody <- allocVar
      (recursiveAssumption, _) <- allocExpNode recursiveBody
      setBindParentIfMissing (typeRef recursiveAssumption) (genRef schemeGenId) BindFlex
      setBindParentIfMissing (typeRef recursiveBody) (typeRef recursiveAssumption) BindFlex
      let env' = Map.insert bindingKey (Binding recursiveAssumption (Just schemeGenId) binderDetails) env
      (rhsNode, rhsAnn, rhsScope) <- buildUnder env' schemeGenId rhsExpr
      schemeRootNode <- unwrapSchemeRoot rhsNode
      setExpBody recursiveAssumption schemeRootNode
      setGenNodeSchemes schemeGenId [schemeRootNode]
      rebindScopeRoot (genRef schemeGenId) schemeRootNode rhsScope
      attachUnder (typeRef schemeRootNode) (genRef schemeGenId) BindFlex
      _ <- addInstEdge schemeRootNode recursiveAssumption
      pure (Just schemeGenId, schemeGenId, schemeRootNode, schemeGenId, rhsAnn)

-- | Allocate the existential destination owned by an annotation edge.
--
-- Definition 9.2.1 condition 10 requires every instantiation-edge destination
-- to be bound on a gen node.  Keep that allocation identity separate from the
-- direct structural annotation copy: normalization may merge structural
-- equality, but it must not consume the edge's destination provenance.
allocRigidEdgeDestination :: GenNodeId -> NodeId -> ConstraintM NodeId
allocRigidEdgeDestination owner target = do
  destination <- allocVar
  setVarBound destination (Just target)
  attachUnder (typeRef destination) (genRef owner) BindRigid
  pure destination

-- | Translate a coercion application @cτ a@ (surface form @(a : τ)@).
--
-- We treat coercions as a special form rather than a regular function
-- application so that Phase 1 produces exactly one instantiation edge for the
-- annotation site, and Phase 6 can elaborate it as an xMLF instantiation.
buildCoerce :: Env -> GenNodeId -> NormSrcType -> ResolvedNormCoreExpr -> ConstraintM (NodeId, AnnExpr)
buildCoerce env scopeRoot annTy annotatedExpr = do
  (exprNode, exprAnn) <- buildExpr env scopeRoot annotatedExpr
  annGen <- allocChildGenUnder scopeRoot
  copies <-
    withScopedRebind
      (genRef annGen)
      coercionFlexibleCodomain
      (internalizeCoercionType annGen annTy)
  let domainNode = coercionRigidDomain copies
      codomainNode = coercionFlexibleCodomain copies
  -- The scoped rebind discovers every disconnected annotation component as a
  -- provisional scheme root.  Only the flexible codomain is exported by the
  -- coercion; recording that fact now prevents a later enclosing-let rebind
  -- from mistaking nested TyForall nodes for sibling gen schemes and detaching
  -- their binders.
  setGenNodeSchemes annGen [codomainNode]

  -- Every source annotation owns an explicit expansion node.  The annotation
  -- edge therefore produces its authoritative expansion, witness, and replay
  -- trace by construction instead of asking Phase 6 to recover one from the
  -- annotated expression's shape.  A polymorphic variable occurrence already
  -- has an occurrence expansion; use its scheme body so the annotation still
  -- owns exactly one expansion layer.
  nodes <- gets bsNodes
  let edgeBody =
        case IntMap.lookup (getNodeId exprNode) nodes of
          Just TyExp {tnBody = body} -> body
          _ -> exprNode
  (edgeLeft, _) <- allocExpNode edgeBody
  attachUnder (typeRef edgeLeft) (genRef annGen) BindFlex
  edgeDestination <- allocRigidEdgeDestination annGen domainNode
  eid <- addInstEdge edgeLeft edgeDestination
  -- Preserve the original source annotation type so elaboration can recover
  -- types that presolution strips (e.g. TForall inside a μ body).
  modify' $ \st ->
    st {bsAnnSourceTypes = IntMap.insert (getNodeId codomainNode) annTy (bsAnnSourceTypes st)}
  pure (codomainNode, AAnn exprAnn codomainNode eid)

-- | Translate a compiler-owned exact annotation from its authoritative
-- producer type.  Unlike source kappa-sigma, this owns one direct rigid target:
-- there is no flexible codomain copy and no synthetic
-- @forall beta >= sigma. beta@ result wrapper to generalize later.
buildExactCoerce :: Env -> GenNodeId -> NormSrcType -> ResolvedSrcType -> ResolvedNormCoreExpr -> ConstraintM (NodeId, AnnExpr)
buildExactCoerce env scopeRoot annTy exactTy annotatedExpr = do
  (exprNode, exprAnn) <- buildExpr env scopeRoot annotatedExpr
  -- Compiler authority is an equality check in the current RHS scope, not the
  -- source-language kappa-sigma coercion form.  Giving it a child gen would put
  -- the rigid target in a sibling scope of the producer.  Installing the
  -- resulting lower-bound frontier would then require a target-side Raise that
  -- cannot belong to the producer edge's source-domain witness.
  --
  -- Internalize the target directly at the enclosing scope instead.  A bare
  -- source type variable is a shared semantic binder: keep the owner installed
  -- by 'internalizeCoercionCopy' rather than stealing it as an annotation root.
  (targetBody, shared) <-
    internalizeCoercionCopy
      BindRigid
      False
      scopeRoot
      scopeRoot
      Map.empty
      Map.empty
      annTy
  targetNode <-
    if targetBody `elem` Map.elems shared
      then do
        -- A free/root STVar denotes a definition-owned semantic binder.  The
        -- exact site gets its own rigid target bounded by that identity node;
        -- returning or reparenting the shared node would make an occurrence
        -- binder double as this expression's scheme root.
        proxy <- allocVar
        setVarBound proxy (Just targetBody)
        attachUnder (typeRef proxy) (genRef scopeRoot) BindRigid
        pure proxy
      else do
        attachUnder (typeRef targetBody) (genRef scopeRoot) BindRigid
        pure targetBody

  -- The edge checks the producer against the authoritative target.  Keep the
  -- annotation-owned expansion layer so presolution and witness ownership stay
  -- explicit, but do not manufacture another type result around the target.
  nodes <- gets bsNodes
  let edgeBody =
        case IntMap.lookup (getNodeId exprNode) nodes of
          Just TyExp {tnBody = body} -> body
          _ -> exprNode
  (edgeLeft, _) <- allocExpNode edgeBody
  edgeDestination <- allocRigidEdgeDestination scopeRoot targetNode
  eid <- addInstEdge edgeLeft edgeDestination
  modify' $ \st ->
    st
      { bsAnnSourceTypes =
          IntMap.insert (getNodeId targetNode) annTy (bsAnnSourceTypes st),
        bsExactProducerTypes =
          IntMap.insert (getEdgeId eid) exactTy (bsExactProducerTypes st)
      }
  pure (targetNode, AExactAnn exprAnn exactTy targetNode eid)

{- Note [Coercion domain/codomain semantics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The thesis' coercion κσ builds a rigid domain and flexible codomain. We
construct two direct graphic copies (with shared existentials) and return the
*codomain* node as the annotation result, matching the thesis semantics
(§12.3.2.2, §15.3.8).  In particular, construction must not encode a copy
as @forall beta >= sigma. beta@: Figure 8.2.3's Eq-Var case represents that
syntactic type directly by the graph for @sigma@.

The instantiation edge connects the expression to a fresh existential bounded
by the *domain* node.  The existential owns the edge-destination gen path,
while its lower bound preserves the direct rigid domain constraint.  The
codomain is returned as the result type, allowing the annotation to be used in
contexts that expect the annotated type.

We mark the domain copy as *restricted* by binding its coercion-local nodes
with rigid edges under gen nodes (shared existentials stay flexible, and no
rigid ancestor is introduced). This pushes toward
the thesis’ “rigid domain” intent while staying presolution-safe: the nodes are
not instantiable, but they are not locked under a rigid ancestor.

The codomain is returned as the annotation result, while the domain stays the
lower bound of the edge-only instantiation target.
-}

{- Note [Lambda Translation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Lambda abstraction `λx. e` is translated as follows:

  1. Allocate a fresh type variable α for the parameter at the CURRENT binding node
  2. Extend the environment to bind x to α
  3. Recursively translate the body e to get type τ
  4. Allocate a fresh codomain variable β at the current binding node
  5. Add the instantiation edge τ ≤ β
  6. Return a fresh arrow node (α → β)

The parameter is bound at the current binding node (the surrounding
generalization site), NOT under a fresh child binder. This means lambda
parameters are monomorphic — they cannot be generalized. This is the key
difference from let-bindings.

Figure 15.3.5 elaborates the body as @(T(e))[φR(e);T(edge)]@.  Carrying
that edge on 'ALam' makes the computation construction authority for Phase 6;
the elaborator does not have to retrofit the lambda's codomain afterward.

Example:
  λx. λy. x y

  Generates (under binding node g₀):
    - α : TyVar { tnId = bound, tnBound = Nothing } under g₀ (for x)
    - β : TyVar { tnId = bound, tnBound = Nothing } under g₀ (for y)
    - γ : TyVar { tnId = bound, tnBound = Nothing } under g₀ (result of application)
    - (β → γ) : TyArrow
    - InstEdge: α ≤ (β → γ)
    - fresh codomains δ and ε for the inner and outer lambdas
    - InstEdge: γ ≤ δ (inner lambda body)
    - (β → δ) : TyArrow
    - InstEdge: (β → δ) ≤ ε (outer lambda body)
    - (α → ε) : TyArrow (final type)

Var-Abs still keeps parameters monomorphic.  Body generalization is a distinct
subterm boundary and is consumed before the body edge during elaboration.

See Note [Constraint simplification: Var-Abs (Ch 12.4.1)].
-}

{- Note [Constraint simplification: Var-Abs (Ch 12.4.1)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The thesis (§9.4, Figure 9.4.1) generates a gen node for each lambda-bound
variable, then removes it via the Var-Abs simplification rule (Lemma 12.4.1)
because lambda parameters can only be typed monomorphically — the gen node is
always degenerate.

We apply Var-Abs on-the-fly during constraint generation (§12.4.3: "they can
be performed on-the-fly during the generation of typing constraints. From an
algorithmic standpoint, the second approach is actually simpler.") by never
creating the gen node in the first place: the parameter is bound directly at
the current scope with a monomorphic `Binding` carrying its generated local
identity, and variable references skip expansion (the `Nothing` branch of
`bindingGen` in `buildExprRaw`).

See also Note [Minimal Expansion Decision] in Presolution/Expansion.hs for the
degenerate-forall case (case 2: "If there are no bound vars (degenerate ∀),
reuse the body and just unify"), which handles the residual Var-Abs scenario
during presolution.
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
`TyForall` anchor plus binding edges (`Constraint p.cBindParents`).

Repo translation:
  1. Translate e₁ while recording the nodes allocated for it (a fresh scope frame)
  2. Insert a `TyForall` anchor g whose body is the RHS type node τ
  3. Rebind all RHS-scope nodes under g via `Constraint p.cBindParents`
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
so presolution can drop its witness/expansion (`dropTrivialSchemeEdges`). It is
represented by `ALetScope`, which has no xMLF term; `AAnn` is reserved for
source coercions.
-}

lookupVar :: Env -> BindingKey -> VarName -> ConstraintM Binding
lookupVar env key name = case Map.lookup key env of
  Just binding -> pure binding
  Nothing -> throwError (UnknownVariable name)

materializeBinding :: Binding -> ConstraintM Binding
materializeBinding binding@Binding{} =
  pure binding
materializeBinding LazyExternalBinding {bindingExternalRoot = rootGen, bindingExternal = externalBinding} = do
  cache <- gets bsExternalBindingCache
  let cacheKey = externalBindingKey externalBinding
  case Map.lookup cacheKey cache of
    Just binding -> pure binding
    Nothing -> do
      binding <- buildExternalBinding rootGen externalBinding
      registerLazyExternalRootScope binding
      modify' $ \st ->
        st
          { bsExternalBindingCache =
              Map.insert cacheKey binding (bsExternalBindingCache st)
          }
      pure binding

registerLazyExternalRootScope :: Binding -> ConstraintM ()
registerLazyExternalRootScope Binding {bindingNode = nid, bindingGen = mbGen} =
  case mbGen of
    Just gid -> Scope.registerRootScopeNode (genRef gid)
    Nothing -> Scope.registerRootScopeNode (typeRef nid)
registerLazyExternalRootScope LazyExternalBinding {} =
  pure ()

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
  pure $
    listToMaybe
      [ gnId gen
        | gen <- IntMap.elems (getGenNodeMap genNodes),
          root `elem` gnSchemes gen
      ]

-- | Type variable environment for internalizing source types.
type TyEnv = Map VarName NodeId

type SharedEnv = Map VarName NodeId

-- | The two graphic copies owned by a source annotation.
--
-- Naming the roles prevents the annotation result and its edge-only domain
-- constraint from being selected positionally.  In particular, callers can
-- only export 'coercionFlexibleCodomain'; the rigid domain remains authority
-- for the annotation edge.
data CoercionTypeCopies = CoercionTypeCopies
  { coercionRigidDomain :: NodeId,
    coercionFlexibleCodomain :: NodeId
  }

-- | Internalize a coercion type κ as a rigid domain and flexible codomain,
-- sharing existential (free) variables across both copies.
internalizeCoercionType :: GenNodeId -> NormSrcType -> ConstraintM CoercionTypeCopies
internalizeCoercionType coerceGen ty = do
  (domainNode, shared1) <-
    internalizeCoercionCopy BindRigid False coerceGen coerceGen Map.empty Map.empty ty
  (codomainNode, _shared2) <-
    internalizeCoercionCopy BindFlex False coerceGen coerceGen Map.empty shared1 ty
  setBindParentOverride (typeRef domainNode) (genRef coerceGen) BindRigid
  setBindParentOverride (typeRef codomainNode) (genRef coerceGen) BindFlex
  pure
    CoercionTypeCopies
      { coercionRigidDomain = domainNode,
        coercionFlexibleCodomain = codomainNode
      }

-- | Internalize constructor arguments left-to-right while threading
-- coercion-copy sharing.
internalizeConArgs ::
  BindFlag ->
  Bool ->
  GenNodeId ->
  GenNodeId ->
  TyEnv ->
  SharedEnv ->
  NonEmpty NormSrcType ->
  ConstraintM (NonEmpty NodeId, SharedEnv)
internalizeConArgs bindFlag wrap coerceGen currentGen tyEnv shared (argTy :| rest) = do
  (argNode, shared1) <-
    internalizeCoercionCopy bindFlag wrap coerceGen currentGen tyEnv shared argTy
  case rest of
    [] -> pure (argNode :| [], shared1)
    nextArg : remainingArgs -> do
      (nextNodes, sharedFinal) <-
        internalizeConArgs bindFlag wrap coerceGen currentGen tyEnv shared1 (nextArg :| remainingArgs)
      pure (argNode :| NE.toList nextNodes, sharedFinal)

-- | Internalize a coercion copy with a given binding flag (rigid/flex),
-- optional wrapping, and shared existentials.
internalizeCoercionCopy ::
  BindFlag ->
  Bool ->
  GenNodeId ->
  GenNodeId ->
  TyEnv ->
  SharedEnv ->
  NormSrcType ->
  ConstraintM (NodeId, SharedEnv)
internalizeCoercionCopy bindFlag wrap coerceGen currentGen tyEnv shared srcType =
  case srcType of
    -- Domain copies use BindRigid to mark coercion-local nodes as restricted.
    -- To avoid locked descendants, rebind children auto-bound under structural
    -- nodes back to the current gen when rigid.
    STVar name ->
      internalizeTypeVariable name shared
    STArrow dom cod -> do
      (domNode, shared1) <-
        internalizeCoercionCopy bindFlag wrap coerceGen currentGen tyEnv shared dom
      (codNode, shared2) <-
        internalizeCoercionCopy bindFlag wrap coerceGen currentGen tyEnv shared1 cod
      arrowNode <- allocArrow domNode codNode
      rebindStructuralChildren bindFlag arrowNode currentGen shared2 [domNode, codNode]
      withWrappedNode bindFlag wrap currentGen arrowNode shared2
    STBase name -> do
      registerTyConArity (BaseTy name) 0
      identity <- resolveTypeHeadIdentity name
      baseNode <- allocBase identity (BaseTy name)
      withWrappedNode bindFlag wrap currentGen baseNode shared
    STCon name args -> do
      let arity = NE.length args
      registerTyConArity (BaseTy name) arity
      (argNodes, sharedFinal) <-
        internalizeConArgs bindFlag wrap coerceGen currentGen tyEnv shared args
      identity <- resolveTypeHeadIdentity name
      conNode <- allocCon identity (BaseTy name) argNodes
      rebindStructuralChildren bindFlag conNode currentGen sharedFinal (NE.toList argNodes)
      withWrappedNode bindFlag wrap currentGen conNode sharedFinal
    STVarApp name args -> do
      (headNode, shared1) <- internalizeTypeVariable name shared
      (argNodes, sharedFinal) <-
        internalizeConArgs bindFlag wrap coerceGen currentGen tyEnv shared1 args
      varAppNode <- allocVarApp headNode argNodes
      rebindStructuralChildren bindFlag varAppNode currentGen sharedFinal (headNode : NE.toList argNodes)
      withWrappedNode bindFlag wrap currentGen varAppNode sharedFinal
    STTyLam {} ->
      throwError (InternalConstraintError "residual type lambda reached constraint generation")
    STTyApp {} ->
      throwError (InternalConstraintError "residual type application reached constraint generation")
    STForall var mBound body -> do
      -- Note: Alias bounds (∀(b ⩾ a). body where the bound is a bare
      -- variable) are unreachable here — normalization inlines them via
      -- capture-avoiding substitution before constraint generation.
      -- `mBound` is wrapped as `Maybe (SrcBound 'NormN)`; unwrapping with
      -- `unNormBound` yields a `StructBound` whose root cannot be a variable.
      --
      -- Well-formedness check: binder must not occur in its own structural bound.
      -- This catches cases like ∀(a ⩾ List a). a where the binder appears
      -- nested inside a structural bound.
      case mBound of
        Just bound
          | Set.member var (structBoundFreeVars (unNormBound bound)) ->
              throwError (ForallBoundMentionsBinder var)
        _ -> pure ()
      ((varNode, bodyNode, shared2), scopeFrame) <- withScopedBuild $ do
        varNode <- allocVar
        recordKnownSourceTypeBinderIdentity var varNode
        let tyEnv' = Map.insert var varNode tyEnv
        (mbBoundNode, shared1) <- case mBound of
          Nothing -> pure (Nothing, shared)
          Just bound -> do
            let boundAsNorm = structBoundToNormSrcType (unNormBound bound)
            (boundNode, shared2) <-
              internalizeCoercionCopy bindFlag False coerceGen currentGen tyEnv' shared boundAsNorm
            pure (Just boundNode, shared2)

        setVarBound varNode mbBoundNode

        (bodyNode, shared2) <-
          internalizeCoercionCopy bindFlag False coerceGen currentGen tyEnv' shared1 body
        pure (varNode, bodyNode, shared2)

      forallNode <- allocForall bodyNode
      recordKnownSourceTypeBinderIdentity var forallNode
      rebindScopeRoot (typeRef forallNode) bodyNode scopeFrame
      attachUnder (typeRef varNode) (typeRef forallNode) bindFlag
      unlessShared shared2 bodyNode $
        attachUnder (typeRef bodyNode) (typeRef forallNode) bindFlag
      withWrappedNode bindFlag wrap currentGen forallNode shared2
    STMu v body -> do
      ((varNode, bodyNode, shared1), scopeFrame) <- withScopedBuild $ do
        varNode <- allocVar
        recordKnownSourceTypeBinderIdentity v varNode
        let tyEnv' = Map.insert v varNode tyEnv
        (bodyNode, shared1) <-
          internalizeCoercionCopy bindFlag False coerceGen currentGen tyEnv' shared body
        pure (varNode, bodyNode, shared1)
      muNode <- allocMu bodyNode
      recordKnownSourceTypeBinderIdentity v muNode
      rebindScopeRoot (typeRef muNode) bodyNode scopeFrame
      attachUnder (typeRef varNode) (typeRef muNode) bindFlag
      withWrappedNode bindFlag wrap currentGen muNode shared1
    STBottom -> do
      varNode <- allocVar
      pure (varNode, shared)
  where
    internalizeTypeVariable name shared0 =
      case Map.lookup name tyEnv of
        Just nid -> pure (nid, shared0)
        Nothing -> do
          mbSourceNode <- sourceTypeBinderNode name
          case mbSourceNode of
            Just nid -> pure (nid, Map.insert name nid shared0)
            Nothing ->
              case Map.lookup name shared0 of
                Just nid -> pure (nid, shared0)
                Nothing -> do
                  nid <- allocVar
                  setBindParentOverride (typeRef nid) (genRef coerceGen) BindFlex
                  -- A lower-level pipeline entrypoint may not have a preceding
                  -- source-program resolver.  The newly allocated graph node
                  -- is then the semantic identity of this free annotation
                  -- binder.  Publish that identity under its source alias and
                  -- in the node sidecar immediately so every later occurrence
                  -- and annotation elaboration reuses it.
                  let identity = typeBinderIdentityFromNode nid
                  mbRoot <- gets bsTypeBinderRoot
                  modify' $ \st ->
                    st
                      { bsTypeBinderIdentities =
                          Map.insert name identity (bsTypeBinderIdentities st)
                      , bsTypeBinderNodes =
                          maybe
                            (bsTypeBinderNodes st)
                            ( \root ->
                                Map.insert
                                  (root, identity)
                                  nid
                                  (bsTypeBinderNodes st)
                            )
                            mbRoot
                      , bsTypeBinderNodeIdentities =
                          IntMap.insert
                            (getNodeId nid)
                            identity
                            (bsTypeBinderNodeIdentities st)
                      }
                  pure (nid, Map.insert name nid shared0)

    sourceTypeBinderNode name = do
      identities <- gets bsTypeBinderIdentities
      mbRoot <- gets bsTypeBinderRoot
      case (lookupTypeBinderIdentityAlias identities name, mbRoot) of
        (Just identity, Just root) -> do
          nodes <- gets bsTypeBinderNodes
          case Map.lookup (root, identity) nodes of
            Just nid -> do
              recordSourceTypeBinderIdentity nid identity
              pure (Just nid)
            Nothing -> do
              nid <- allocVar
              setBindParentOverride (typeRef nid) (genRef root) BindRigid
              modify' $ \st ->
                st
                  { bsTypeBinderNodes = Map.insert (root, identity) nid (bsTypeBinderNodes st),
                    bsTypeBinderNodeIdentities =
                      IntMap.insert (getNodeId nid) identity (bsTypeBinderNodeIdentities st)
                  }
              pure (Just nid)
        _ -> pure Nothing

    -- Bound source binders are lexical nodes, so they must not enter
    -- 'bsTypeBinderNodes', which shares free binders by definition root.  Both
    -- the lexical variable and its forall/mu owner need the semantic identity
    -- sidecar: instantiation provenance may retain either representation for a
    -- copied binder.  Without it reification falls back to a graph-local
    -- 'NodeId' identity for an already-resolved source binder.
    recordKnownSourceTypeBinderIdentity :: String -> NodeId -> ConstraintM ()
    recordKnownSourceTypeBinderIdentity name nid = do
      identities <- gets bsTypeBinderIdentities
      case lookupTypeBinderIdentityAlias identities name of
        Just identity -> recordSourceTypeBinderIdentity nid identity
        Nothing -> pure ()

-- | An expansion occurrence is another graph representative of its source
-- scheme.  Publish that source identity when it exists so presolution can
-- route either representative to the same semantic binder.
propagateSourceTypeBinderIdentity :: NodeId -> NodeId -> ConstraintM ()
propagateSourceTypeBinderIdentity sourceNode targetNode = do
  identities <- gets bsTypeBinderNodeIdentities
  case IntMap.lookup (getNodeId sourceNode) identities of
    Just identity -> recordSourceTypeBinderIdentity targetNode identity
    Nothing -> pure ()

recordSourceTypeBinderIdentity :: NodeId -> TypeBinderIdentity -> ConstraintM ()
recordSourceTypeBinderIdentity nid identity = do
  identities <- gets bsTypeBinderNodeIdentities
  case IntMap.lookup (getNodeId nid) identities of
    Just existing
      | existing /= identity ->
          throwError
            ( InternalConstraintError
                ( "conflicting source type binder identities for "
                    ++ show nid
                    ++ ": "
                    ++ show existing
                    ++ " and "
                    ++ show identity
                )
            )
    _ ->
      modify' $ \st ->
        st
          { bsTypeBinderNodeIdentities =
              IntMap.insert (getNodeId nid) identity (bsTypeBinderNodeIdentities st)
          }

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
  LChar _ -> "Char"
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
      st {bsTyConArity = Map.insert con arity (bsTyConArity st)}

-- | Convert a structural bound into normalized type shape for recursive internalization.
structBoundToNormSrcType :: StructBound -> NormSrcType
structBoundToNormSrcType sb = case sb of
  STArrow dom cod -> STArrow dom cod
  STBase name -> STBase name
  STCon name args -> STCon name args
  STVarApp name args -> STVarApp name args
  STTyLam name body -> STTyLam name body
  STTyApp fun arg -> STTyApp fun arg
  STForall v mb body -> STForall v mb body
  STMu v body -> STMu v body
  STBottom -> STBottom

-- | Check if a type variable name occurs free in a 'StructBound'.
structBoundFreeVars :: StructBound -> Set.Set String
structBoundFreeVars = go Set.empty
  where
    go bound sb = case sb of
      STArrow dom cod -> normFreeVars bound dom <> normFreeVars bound cod
      STBase _ -> Set.empty
      STCon _ args -> foldMap (normFreeVars bound) args
      STVarApp name args ->
        let headVars =
              if Set.member name bound
                then Set.empty
                else Set.singleton name
         in headVars <> foldMap (normFreeVars bound) args
      STTyLam v body ->
        normFreeVars (Set.insert v bound) body
      STTyApp fun arg ->
        normFreeVars bound fun <> normFreeVars bound arg
      STForall v mb body ->
        let bound' = Set.insert v bound
         in maybe Set.empty (go bound . unNormBound) mb <> normFreeVars bound' body
      STMu v body ->
        normFreeVars (Set.insert v bound) body
      STBottom -> Set.empty

    normFreeVars bound ty = case ty of
      STVar v -> if Set.member v bound then Set.empty else Set.singleton v
      STArrow dom cod -> normFreeVars bound dom <> normFreeVars bound cod
      STBase _ -> Set.empty
      STCon _ args -> foldMap (normFreeVars bound) args
      STVarApp name args ->
        let headVars =
              if Set.member name bound
                then Set.empty
                else Set.singleton name
         in headVars <> foldMap (normFreeVars bound) args
      STTyLam v body ->
        normFreeVars (Set.insert v bound) body
      STTyApp fun arg ->
        normFreeVars bound fun <> normFreeVars bound arg
      STForall v mb body ->
        let bound' = Set.insert v bound
         in maybe Set.empty (go bound . unNormBound) mb <> normFreeVars bound' body
      STMu v body ->
        normFreeVars (Set.insert v bound) body
      STBottom -> Set.empty
